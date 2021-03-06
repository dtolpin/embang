(ns embang.trap
  "CPS transformation of Anglican program"
  (:require embang.runtime) ; for primitive procedure names
  (:use embang.ast
        embang.state))

;;;;; Trampoline-ready Anglican program

;;;; Architecture outline
;;
;; This module defines the transformation of an Anglican program
;; into a function in continuation-passing style. The function
;; itself is the initial continuation.
;;
;; Any continuation receives a value and a state, and returns
;; either a thunk (an argumentless function) or a checkpoint ---
;; an object corresponding to either `sample', `observe', or to
;; returning the final result.
;;
;; Functions are assumed to be in the CPS form. Exceptions
;; either come from certain namespaces or explicitly defined.

;;;; Public interface

;;; Checkpoints
;;
;; Checkpoints corresponding to `sample' and `observe' include a
;; continuation. This continuation must be called to continue
;; the execution after the inference algorithm processed the
;; checkpoint. The `result' checkpoint does not have a
;; continuation. 

(defrecord observe [id dist value cont state])
(defrecord sample [id dist cont state])
(defrecord result [state])

;;; CPS Escapes

;; Value and state access

(defn value-cont "returns value" [v _] v)
(defn state-cont "returns state" [_ s] s)

;; Retrieval of the final result

(defn result-cont [v s] (->result s))

;;;; Transformations

;;; Managing primitive procedures

(def ^:dynamic *primitive-namespaces*
  "functions in these namespaces are primitive"
  '#{clojure.core
     embang.runtime
     embang.state})

(defmacro adding-primitive-namespaces
  "includes namespaces into the set of primitive namespaces"
  [names & body]
  `(binding [*primitive-namespaces*
             (reduce conj *primitive-procedures* (flatten ~names))]
     ~@body))

(def ^:dynamic *primitive-procedures*
  "primitive procedures, do not exist in CPS form"
  (let [;; higher-order procedures cannot be primitive
        exclude '#{loop
                   map reduce
                   filter keep keep-indexed remove
                   repeatedly
                   every? not-any? some
                   every-pred some-fn
                   comp juxt partial
                   sort sort-by
                   sorted-map-by sorted-set-by}
        ;; all functions in clojure.core and embang.runtime,
        ;; except excluded ones, are primitive
        runtime-namespaces '[clojure.core embang.runtime]]
    (set (keep (fn [[k v]]
                 (when (and (not (exclude k))
                            (fn? (var-get v)))
                   k))
               (mapcat ns-publics runtime-namespaces)))))

(defmacro adding-primitive-procedures
  "includes names into the set of primitive procedures"
  [names & body]
  `(binding [*primitive-procedures*
             (reduce conj *primitive-procedures* (flatten ~names))]
     ~@body))

(defmacro shading-primitive-procedures
  "excludes names from the set of primitive procedures"
  [names & body]
  `(binding [*primitive-procedures*
             (reduce disj *primitive-procedures* (flatten ~names))]
     ~@body))

;;; Expression predicates

(defn declaration?
  "true if the form is a declaration"
  [form]
  (and (seq? form) (= 'declare (first form))))

(defn primitive-procedure?
  "true if the procedure is primitive,
  that is, does not have a CPS form"
  [procedure]
  (and 
    (symbol? procedure)
    (or 
      ;; Accept either unqualified names for every procedure,
      (*primitive-procedures* procedure)
      ;; or qualified names from primitive namespaces.
      (and 
        (namespace procedure)
        (*primitive-namespaces* (symbol (namespace procedure)))
        (fn? (deref (resolve procedure)))))))

(defn primitive-operator?
  "true if the experssion is converted by clojure 
  to a primitive procedure in operator position"
  [procedure]
  (or (primitive-procedure? procedure)
      (keyword? procedure)))

(defn fn-form?
  "true when the argument is a fn form"
  [expr]
  (and (seq? expr) ('#{fn fn*} (first expr))))

(defn mem-form?
  "true when the argument is a mem form"
  [expr]
  (and (seq? expr) (= (first expr) 'mem)))

;;; Simple expressions
;;
;; Simple expressions are passed to continuations unmodified.

(defn simple?
  "true if expr has no continuation"
  [expr]
  (cond
    (or (vector? expr) (map? expr) (set? expr))
    (every? simple? expr)

    (and (seq? expr) (seq expr))
    (case (first expr)
      quote true
      (if and or) (every? simple? (rest expr))
      case (if (even? (count expr))
             ;; No default clause.
             (every? simple? (take-nth 2 (rest expr)))
             ;; Default clause is a single expression.
             (and (every? simple? (take-nth 2 (rest expr)))
                  (simple? (last expr))))
      (fn mem
       let loop recur
       when cond do 
       predict observe sample
       store retrieve
       apply) false
      ;; application
      (and (primitive-operator? (first expr))
           (every? simple? (rest expr))))

    (not (primitive-procedure?  expr)) true))

;;; Opaque expresions
;;
;; Some expressions are opaque, they are transformed
;; into CPS and then passed to continuations as arguments.

(defn opaque?
  "true when the argument is an expression
  which is passed to continuation (rather
  than accepts continuation) in its CPS form"
  [expr]
  (or (simple? expr)
      (primitive-procedure? expr)
      (fn-form? expr)
      (mem-form? expr)))
 
;; Simple expressions, primitive procedure wrappers,
;; as well as `fn' and `mem' forms are opaque.

(declare primitive-procedure-cps fn-cps mem-cps)
(defn opaque-cps
  "return CPS form of an opaque expression"
  [expr] {:pre [(opaque? expr)]}
  (cond
   (simple? expr) expr
   (primitive-procedure? expr) (primitive-procedure-cps expr)
   (fn-form? expr) (fn-cps (rest expr))
   (mem-form? expr) (mem-cps (rest expr))))

;;; Recursively called transformers

(declare cps-of-expression
         cps-of-do
         cps-of-application)

;;; Generating unique symbols

(def ^:dynamic *gensym* 
  "customized gensym for code generation,
  bound to 'symbol' in tests"
  gensym)

;;; Managing continuations

;; A continuation can be any symbolic expression. Sometimes
;; it has to be bound to a symbol to avoid variable capture
;; or exponential code expansion.

(defmacro ^:private defn-naming-cont
  "defines a transformer while binding 
  the continuation to a name"
  [cps-of & args]
  (let [[docstring [parms & body]]
        (if (string? (first args)) 
          [(first args) (rest args)]
          [(format "CPS transformation macro '%s'" cps-of) args])]
    (let [parms* (*gensym* "P")]
      `(defn ~(with-meta cps-of {:doc docstring})
         ;; Parameters can be structured. The sequence of
         ;; all arguments is bound to ~parms*.
         [& [~@parms :as ~parms*]]
         (if (symbol? ~(last parms))
           (do ~@body)
           (let [~'cont* (*gensym* "C")]
             `(~~''let [~~'cont* ~~(last parms)]
                ;; If the continuation is not a symbol, ~parms* is
                ;; used to re-pass arguments to the function along
                ;; with the named continuation.
                ~(apply ~cps-of
                        (concat (butlast ~parms*) [~'cont*])))))))))

;; When a continuation is called, it is wrapped in a thunk
;; --- an argumentless function. This collapses the stack
;; and ensures that recursion of any depth does not cause
;; stack overflow. Thunks should be used in conjunction with
;; Clojure `trampoline'.

(defn continue
  "returns a trampolined call to continuation"
  [cont value state]
  (with-meta
    `(~'fn [] (~cont ~value ~state))
    {::continuation true}))

;;; Literal data structures --- vectors, maps and sets.

;; Literal vector is a shorthand notation for (vector ... ).
(defn cps-of-vector
  "transforms literal vector to CPS"
  [expr cont]
  (cps-of-expression `(~'vector ~@expr) cont))

;; Literal map is a shorthand notation for (hash-map ...).
(defn cps-of-hash-map
  "transforms literal map to CPS"
  [expr cont]
  (cps-of-expression `(~'hash-map ~@(apply concat (seq expr))) cont))

;; Literal set is a shorthand notation for (set (list ...)).
(defn cps-of-set
  "transforms literal set to CPS"
  [expr cont]
  (cps-of-expression `(~'set (~'list ~@expr)) cont))

;; Opaque expressions are passed to continuations as arguments.
(defn cps-of-opaque
  "transforms opaque expression to CPS"
  [expr cont]
  (continue cont (opaque-cps expr) '$state))

;; Continuation is the first, rather than the last, parameter of a
;; function to support functions with variable arguments.

;;; Closures

(defn fn-cps
  "transforms function definition to CPS form"
  [args]
  (let [[name parms & body] (fn-args args)
        cont (*gensym* "C")]
    (shading-primitive-procedures parms
      `(~'fn ~(or name (*gensym* "fn"))
         [~cont ~'$state ~@parms]
         ~(cps-of-do body cont)))))

(defn mem-cps
  "transforms mem to CPS"
  [[arg & _ :as args]] {:pre [(= (count args) 1)]}
  (let [cont (*gensym* "C")
        id (*gensym* "M")
        value (*gensym* "V")
        mparms (*gensym* "P")

        ;; If the argument of mem is a named lambda,
        ;; move the name to outer function.
        [name expr] (if (and (fn-form? arg)
                             (symbol? (second arg)))
                      [(second arg) `(~'fn ~@(nnext arg))]
                      [nil arg])]

    `(~'let [~id (~'gensym "M")]
       (~'fn ~(or name (*gensym* "mem"))
         [~cont ~'$state & ~mparms]
         (~'if (in-mem? ~'$state ~id ~mparms)
           ;; continue with stored value
           ~(continue cont `(get-mem ~'$state ~id ~mparms) '$state)
           ;; apply the function to the arguments with
           ;; continuation that intercepts the value
           ;; and updates the state
           ~(cps-of-expression
              `(~'apply ~expr ~mparms)
              `(~'fn ~(*gensym* "set-mem") [~value ~'$state]
                 ~(continue cont value
                            `(set-mem ~'$state
                                      ~id ~mparms ~value)))))))))

(defn-naming-cont cps-of-let
  "transforms let to CPS"
  ;; The continuation is named to avoid variable capture.
  [[bindings & body] cont]
  (if (seq bindings)
    (let [[name value & bindings] bindings]
      (shading-primitive-procedures [name]
        (let [rst (cps-of-let `(~bindings ~@body) cont)]
          (if (opaque? value)
            `(~'let [~name ~(opaque-cps value)]
               ~rst)
            (cps-of-expression
              value
              `(~'fn ~(*gensym* "var") [~name ~'$state]
                 ~rst))))))
    (cps-of-do body cont)))

;; `loop' is translated into an application of recursive
;; function, due to the trampolining of all calls, there
;; is no need for loop/recur.

(defn cps-of-loop
  "transforms loop"
  [[bindings & body] cont]
  (cps-of-expression `((~'fn ~'loop [~@(take-nth 2 bindings)]
                         ~@body)
                       ~@(take-nth 2 (rest bindings)))
                     cont))

(defn cps-of-recur
  "transforms recur"
  [args cont]
  (cps-of-application `(~'loop ~@args) cont))

;;; Flow control.

(defn-naming-cont cps-of-if
  "transforms if to CPS"
  ;; The continuation is named to avoid exponential
  ;; expansion of code.
  [args cont]
  (let [[cnd thn els & rst] (if-args args)]
    (if (opaque? cnd)
      `(~'if ~(opaque-cps cnd)
         ~(cps-of-expression thn cont)
         ~(cps-of-expression els cont))
      (cps-of-expression
        cnd
        (let [cnd (*gensym* "I")]
          `(~'fn ~(*gensym* "if") [~cnd ~'$state]
             ;; Call `cps-of-expression' to detect simple `if'.
             ~(cps-of-expression `(~'if ~cnd ~thn ~els) cont)))))))

;; `when' is translated into `if'.

(defn cps-of-when
  "transforms when to CPS"
  [args cont]
  (cps-of-if [(first args) `(~'do ~@(rest args))] cont))

;; `cond' is translated into nested `if's.

(defn cps-of-cond
  "transforms cond to CPS"
  [clauses cont]
  (if (seq clauses)
    (let [[cnd thn & clauses] clauses]
      (cps-of-if [cnd thn `(~'cond ~@clauses)] cont))
    (cps-of-expression nil cont)))

(defn-naming-cont cps-of-case
  "transforms case to CPS"
  ;; The continuation is named to avoid exponential
  ;; expansion of code.
  [[key & clauses] cont]
  (if (opaque? key)
    `(~'case ~(opaque-cps key)
       ~@(apply
           concat
           (loop [clauses clauses
                  cps-clauses []]
             (if (seq clauses)
               (if (rest clauses)
                 ;; Normal clause.
                 (let [[tag expr & clauses] clauses]
                   (recur
                     clauses
                     (conj cps-clauses 
                           [tag (cps-of-expression expr cont)])))
                 ;; Default clause.
                 (let [[expr] clauses]
                   (conj cps-clauses
                         [(cps-of-expression expr cont)])))
               cps-clauses))))
    (cps-of-expression
      key
      (let [key (*gensym* "K")]
        `(~'fn ~(*gensym* "case") [~key ~'$state]
           ;; Call `cps-of-expression' to detect simple `case'.
           ~(cps-of-expression `(~'case ~key ~@clauses) cont))))))

(defn-naming-cont cps-of-and
  "transforms and to CPS"
  ;; The continuation is named to avoid exponential
  ;; expansion of code.
  [args cont]
  (if (seq args)
    (let [[cnd & args] args]
      (if (seq args)
        (cps-of-expression
          cnd
          (let [cnd (*gensym* "I")]
            `(~'fn ~(*gensym* "and") [~cnd ~'$state]
               ~(cps-of-if [cnd `(~'and ~@args) cnd] cont))))
        (cps-of-expression cnd cont)))
    (cps-of-expression true cont)))

(defn-naming-cont cps-of-or
  "transforms or to CPS"
  ;; The continuation is named to avoid exponential
  ;; expansion of code.
  [args cont]
  (if (seq args)
    (let [[cnd & args] args]
      (if (seq args)
        (cps-of-expression
          cnd
          (let [cnd (*gensym* "I")]
            `(~'fn ~(*gensym* "or") [~cnd ~'$state]
               ~(cps-of-if [cnd cnd `(~'or ~@args)] cont))))
        (cps-of-expression cnd cont)))
    (cps-of-expression false cont)))

;; Declarations may be specified anywhere in a form sequence.
;; A declaration has the syntax:
;;   (declare keyword arg ...)
;; Normally, `keyword' identifies the declared property of
;; `arg ...'.

(defn cps-of-do
  "transforms do to CPS"
  [exprs cont]
  (let [[expr & exprs] exprs]
    (if (declaration? expr)
      (let [[_ kwd & args] expr]
        (case kwd
          :ns-primitive (adding-primitive-namespaces args
                          (cps-of-do exprs cont))
          :primitive (adding-primitive-procedures args
                       (cps-of-do exprs cont))
          (assert false (format "Unknown declaration %s" expr))))
      (cps-of-expression
        expr
        (if (seq exprs)
          `(~'fn ~(*gensym* "do") [~'_ ~'$state]
             ~(cps-of-do exprs cont))
          cont)))))

;;; Applications and applicative forms

(defn make-of-args
  "builds lexical bindings for all compound args
  and then calls 'make' to build expression
  out of the args; used by predict, observe, sample, application"
  ([args make] (make-of-args args false make))
  ([args first-is-rator make]
     (let [substs (map (fn [arg is-rator]
                         (cond
                          (and is-rator
                               (primitive-operator? arg)) [nil arg]
                          (opaque? arg) [nil (opaque-cps arg)]
                          :else [arg (*gensym* "A")]))
                       args
                       (cons first-is-rator (repeat false)))]
       (letfn [(make-of-slist [slist]
                 (if (seq slist)
                   (let [[[arg subst] & slist] slist]
                     (if arg ;; is a compound expression
                       (cps-of-expression
                         arg
                         `(~'fn ~(*gensym* "arg") [~subst ~'$state]
                            ~(make-of-slist slist)))
                       (make-of-slist slist)))
                   (make (map second substs))))]

         (make-of-slist substs)))))

;;; Probabilistic forms

;; If `predict' has two arguments, then the first argument
;; is used as the predict label. Otherwise the label is
;; the symbolic representation of `predict''s argument.

(defn cps-of-predict
  "transforms predict to CPS,
  predict appends predicted expression
  and its value to (:predicts $state)"
  [args cont]
  (make-of-args args
                (fn [args*]
                  (let [[label value]
                        (if (= (count args*) 2)
                          args*
                          `['~(first args) ~@args*])]
                    (continue cont nil 
                              `(add-predict ~'$state
                                            ~label ~value))))))

;; `observe' and `sample' may accept an optional argument at
;; the first position. If the argument is specified, then
;; the value is the identifier of the checkpoint; otherwise,
;; the identifier is a statically generated symbol.

(defn cps-of-observe
  "transforms observe to CPS,
  observe updates the weight by adding
  the result of observe (log-probability)
  to the log-weight"
  [args cont]
  (make-of-args args
                (fn [args*]
                  (let [[id dist value]
                        (if (= (count args*) 3)
                          args*
                          `['~(*gensym* "O") ~@args*])]
                    `(->observe ~id ~dist ~value ~cont ~'$state)))))

(defn cps-of-sample
  "transforms sample to CPS;
  on sample the program is interrupted
  and the control is transferred to the inference algorithm"
  [args cont]
  (make-of-args args
                (fn [args*]
                  (let [[id dist value]
                        (if (= (count args*) 2)
                          args*
                          `['~(*gensym* "S") ~@args*])]
                    `(->sample ~id ~dist ~cont ~'$state)))))

;;; State access

(defn cps-of-store
  "transforms store to CPS;
  the continuation receives the stored value"
  [args cont]
  (make-of-args args
                (fn [args]
                  (continue cont (last args)
                            `(store ~'$state ~@args)))))

(defn cps-of-retrieve
  "transforms retrieve to CPS"
  [args cont]
  (make-of-args args
                (fn [args]
                  (continue cont `(retrieve ~'$state ~@args)
                            '$state))))

;;; Function applications

(defn cps-of-apply
  "transforms apply to CPS;
  сalls of procedures are trampolined
  --- wrapped into a parameterless closure"
  [args cont]
  (make-of-args args :first-is-rator
                (fn [acall]
                  (let [[rator & rands] acall]
                    (if (primitive-operator? rator)
                      (continue cont 
                                (with-meta
                                  `(apply ~@acall) {::primitive true})
                                '$state)
                      `(apply ~rator
                              ~cont ~'$state ~@rands))))))

(defn cps-of-application
  "transforms application to CPS;
  applications of procedures are trampolined
  --- wrapped into a parameterless closure"
  [exprs cont]
  (make-of-args exprs :first-is-rator
                (fn [call]
                  (let [[rator & rands] call]
                    (if (primitive-operator? rator)
                      (continue cont 
                                (with-meta call {:primitive true})
                                '$state)
                      `(~rator ~cont ~'$state ~@rands))))))

;;; Primitive procedures in value postition

(defn primitive-procedure-cps
  "wraps primitive procedure as a CPS form"
  [expr]
  (let [cont (*gensym* "C")
        parms (*gensym* "P")]
    `(~'fn ~(*gensym* (name expr)) [~cont ~'$state & ~parms]
       ~(continue cont `(~'apply ~expr ~parms) '$state))))

;;; Transformation dispatch
    
(defn cps-of-expression
  "dispatches CPS transformation by expression type"
  [expr cont]
  (cond
    (opaque?  expr)            (cps-of-opaque expr cont)
    (vector? expr)             (cps-of-vector expr cont)
    (map? expr)                (cps-of-hash-map expr cont)
    (set? expr)                (cps-of-set expr cont)
    (seq?  expr) (let [[kwd & args] expr]
                   (case kwd
                     let       (cps-of-let args cont)
                     loop      (cps-of-loop args cont)
                     recur     (cps-of-recur args cont)
                     if        (cps-of-if args cont)
                     when      (cps-of-when args cont)
                     cond      (cps-of-cond args cont)
                     case      (cps-of-case args cont)
                     and       (cps-of-and args cont)
                     or        (cps-of-or args cont)
                     do        (cps-of-do args cont)
                     predict   (cps-of-predict args cont)
                     observe   (cps-of-observe args cont)
                     sample    (cps-of-sample args cont)
                     store     (cps-of-store args cont)
                     retrieve  (cps-of-retrieve args cont)
                     ;; Intercept clojure.core/apply to handle
                     ;; the code generated by quasiquote and
                     ;; unquote.
                     (apply clojure.core/apply)
                               (cps-of-apply args cont)
                     ;; application
                     (cps-of-application expr cont)))
    :else (assert false (format "Cannot transform %s to CPS" expr))))
