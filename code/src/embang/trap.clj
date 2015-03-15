(ns embang.trap
  (:require embang.runtime)
  (:use embang.state))

;;;; Trampoline-ready Anglican program

;; The input to this ransformations is an Anglican program in
;; clojure syntax (embang.xlat). The output is a Clojure function
;; that returns either the next step as a structure incroprorating
;; continuations and parameters, or the state containing a vector of
;; predicted values and the sample weight.
;;
;; Steps are delimited by random choices.  Between the steps, the
;; inference decisions can be made.
;;
;; The state is threaded through computation and consists of
;;   - the running sample weight
;;   - the list of predicted values.

(declare ^:dynamic *primitive-procedures*)

(defmacro shading-primitive-procedures
  "excludes names from the set of primitive procedures"
  [names & body]
  `(binding [*primitive-procedures*
             (reduce disj *primitive-procedures* (flatten ~names))]
     ~@body))

;;; Continuation management

;; Value and state access

(defn value-cont "returns value" [v _] v)
(defn state-cont "returns state" [_ s] s)

;; Interrupt points

(defrecord observe [id dist value cont state])
(defrecord sample [id dist cont state])
(defrecord result [state])

;; Retrieval of final result

(defn result-cont [v s] (->result s))

;; When a continuation is called, it is trampolined,
;; that is, wrapped in a thunk. This collapses the stack
;; and ensures that recursion of any depth does not cause
;; stack overflow.

(defn continue
  "returns a trampolined call to continuation"
  [cont value state]
  `(~'fn [] (~cont ~value ~state)))

;;; Expression predicates

(defn primitive-procedure?
  "true if the procedure is primitive,
  that is, does not have a CPS form"
  [procedure]
  (*primitive-procedures* procedure))

(defn fn-form?
  "true when the argument is a fn form"
  [expr]
  (and (seq? expr) (= (first expr) 'fn)))

(defn mem-form?
  "true when the argument is a mem form"
  [expr]
  (and (seq? expr) (= (first expr) 'mem)))

;;; Simple expressions

;; Simple expressions are passed to continuations
;; unmodified.

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
      (and (primitive-procedure? (first expr))
           (every? simple? (rest expr))))

    (not (primitive-procedure?  expr)) true))

;;; Opaque expresions

;; Some expressions are opaque, they are transformed
;; into CPS and then passed to continuations as arguments.

(defn opaque?
  "true when the argument is an expression
  which is passed to continuation (rather
  than accepts continuation) in its CPS form"
  [expr]
  (or (simple? expr)
      (primitive-procedure? expr)
      (fn-form? expr)))
 
;; Simple expressions, primitive procedure wrappers
;; and fn forms are opaque.

(declare primitive-procedure-cps fn-cps mem-cps)
(defn opaque-cps
  "return CPS form of an opaque expression"
  [expr] {:pre [(opaque? expr)]}
  (cond
   (simple? expr) expr
   (primitive-procedure? expr) (primitive-procedure-cps expr)
   (fn-form? expr) (fn-cps (rest expr))
   (mem-form? expr) (mem-cps (rest expr))))

;;; General CPS transformation rules

(declare cps-of-expression
         cps-of-application)

(def ^:dynamic *gensym* 
  "customized gensym for code generation,
  bound to `symbol' in tests"
  gensym)

(defn ^:private cps-of-elist
  [exprs cont]
  (let [[fst & rst] exprs]
    (cps-of-expression
      fst
      (if (seq rst)
        `(~'fn ~(*gensym* "elist") [~'_ ~'$state]
           ~(cps-of-elist rst cont))
        cont))))

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
  (if (vector? (first args))
    (fn-cps `[nil ~@args])
    (let [[name parms & body] args
          cont (*gensym* "C")]
      (shading-primitive-procedures parms
        `(~'fn ~(or name (*gensym* "fn"))
           [~cont ~'$state ~@parms]
           ~(cps-of-elist body cont))))))

(defn mem-cps
  "transforms mem to CPS"
  [[arg & _ :as args] cont] {:pre [(= (count args) 1)]}
  (let [mcont (*gensym* "C")
        id (*gensym* "M")
        value (*gensym* "V")
        mparms (*gensym* "P")

        ;; If the argument of mem is a named lambda,
        ;; move the name to outer function.
        [name expr] (if (and (seq? arg)
                             (= (first arg) 'fn)
                             (symbol? (second arg)))
                      [(second arg) `(~'fn ~@(nnext arg))]
                      [nil arg])]

    `(~'let [~id (~'gensym "M")]
       (~'fn ~(or name (*gensym* "mem"))
         [~mcont ~'$state & ~mparms]
         (~'if (in-mem? ~'$state ~id ~mparms)
           ;; continue with stored value
           (~mcont (get-mem ~'$state ~id ~mparms) ~'$state)
           ;; apply the function to the arguments with
           ;; continuation that intercepts the value
           ;; and updates the state
           ~(cps-of-expression
              `(~'apply ~expr ~mparms)
              `(~'fn ~(*gensym* "set-mem") [~value ~'$state]
                 (~mcont ~value
                         (set-mem ~'$state
                                  ~id ~mparms ~value)))))))))

(defn cps-of-let
  "transforms let to CPS;
  body of let is trampolined
  --- wrapped in a parameterless closure"
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
    (cps-of-elist body cont)))

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

(defmacro ^:private defn-with-named-cont
  "binds the continuation to a name to make the code
  slightly easier to reason about"
  [cps-of & args]
  (let [[docstring [parms & body]]
        (if (string? (first args)) 
          [(first args) (rest args)]
          [(format "CPS transformation macro '%s'" cps-of) args])]
    (let [cont (last parms)]
      `(defn ~(with-meta cps-of {:doc docstring})
         ~parms
         (if (symbol? ~cont)
           (do ~@body)
           (let [~'named-cont (*gensym* "C")]
             `(~~''let [~~'named-cont ~~cont]
                ~(~cps-of ~@(butlast parms) ~'named-cont))))))))

(defn-with-named-cont
  cps-of-if
  "transforms if to CPS"
  [args cont]
  (let [[cnd thn els & rst] args]
    (assert (empty? rst)
            (format "Invalid number of args (%d) passed to if"
                    (count args)))
    (if (opaque? cnd)
      `(~'if ~(opaque-cps cnd)
         ~(cps-of-expression thn cont)
         ~(cps-of-expression els cont))
      (cps-of-expression
        cnd
        (let [cnd (*gensym* "I")]
          `(~'fn ~(*gensym* "if") [~cnd ~'$state]
             (~'if ~cnd
               ~(cps-of-expression thn cont)
               ~(cps-of-expression els cont))))))))

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

(defn-with-named-cont
  cps-of-case
  "transforms case to CPS"
  [args cont]
  (let [[key & clauses] args]
    (if (opaque? key)
      `(~'case ~(opaque-cps key)
         ~@(mapcat (fn [clause]
                     (if (= (count clause) 2)
                       (let [[tag expr] clause]
                         [tag (cps-of-expression expr cont)])
                       ;; The last clause is the default clause.
                       (let [[expr] clause]
                         [(cps-of-expression expr cont)])))
                   ;; This magic call to `partition' breaks clauses
                   ;; into two-element tuples, with the last tuple
                   ;; containing a single element if the number of
                   ;; clauses is odd (default clause is specified).
                   (partition 2 2 nil clauses)))
      (cps-of-expression
        key
        (let [key (*gensym* "K")]
          `(~'fn ~(*gensym* "case") [~key ~'$state]
             ~(cps-of-expression
                `(~'case ~key ~@clauses) cont)))))))

(defn-with-named-cont
  cps-of-and
  "transforms and to CPS"
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

(defn-with-named-cont
  cps-of-or
  "transforms or to CPS"
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

(defn cps-of-do
  "transforms do to CPS"
  [exprs cont]
  (cps-of-elist exprs cont))

;;;; Special forms and applications

(defn ^:private make-of-args
  "builds lexical bindings for all compound args
  and then calls `make' to build expression
  out of the args; used by predict, observe, sample, application"
  ([args make] (make-of-args args false make))
  ([args first-is-rator make]
     (let [substs (map (fn [arg is-rator]
                         (cond
                          (and is-rator
                               (primitive-procedure? arg)) [nil arg]
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

;; `observe' and `select' may accept an optional argument at
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
                    (if (primitive-procedure? rator)
                      (continue cont `(apply ~@acall) '$state)
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
                    (if (primitive-procedure? rator)
                      (continue cont call '$state)
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
    (vector? expr)             (cps-of-vector expr cont)
    (map? expr)                (cps-of-hash-map expr cont)
    (set? expr)                (cps-of-set expr cont)
    (opaque?  expr)            (cps-of-opaque expr cont)
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
                     apply     (cps-of-apply args cont)
                     ;; application
                               (cps-of-application expr cont)))
    :else (assert false (str "Cannot transform " expr " to CPS"))))

(def ^:dynamic *primitive-procedures*
  "primitive procedures, do not exist in CPS form"
  (let [;; higher-order procedures cannot be primitive
        exclude '#{loop
                   map reduce
                   filter keep keep-indexed remove
                   repeatedly
                   every? not-any? some
                   every-pred some-fn
                   comp juxt partial}
        ;; runtime namespaces
        runtime-namespaces '[clojure.core embang.runtime]]
    (set (keep (fn [[k v]]
                 (when (and (not (exclude k))
                            (fn? (var-get v)))
                   k))
               (mapcat ns-publics runtime-namespaces)))))
