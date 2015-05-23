(ns embang.emit
  "Top-level forms for Anglican programs"

  ;; Essential CPS transformation calls
  (:use [embang.trap :only [*gensym*
                            shading-primitive-procedures
                            cps-of-expression result-cont 
                            fn-cps mem-cps primitive-procedure-cps]])

  ;; Pre- and post-processing
  (:require [embang.pre :as pre]
            [embang.post :as post]))

;;;; Top-level forms for Anglican programs

;;; Code manipulation

;; Higher-order functions cannot be re-used from Clojure,
;; they have to be re-implemented in CPS. Clojure has
;; many higher-order functions in the clojure.core
;; namespace, some of which are rather esoteric and not
;; quite appropriate for a small language like Anglican.
;; Still, some higher-order functions are used ubiquitously 
;; in functional programs, and should be available by
;; default.

(defn ^:private overriding-higher-order-functions
  "binds names of essential higher-order functions
  to their CPS implementations"
  [& body]
  `(~'let [~@(mapcat (fn [fun]
                       [fun (symbol "embang.emit" (str "$" fun))])
                     '[map reduce
                       filter some
                       repeatedly
                       comp partial])]
     ~@body))

;; The main program (or query) is defined using the `query'
;; macro and can also be immediately bound to a symbol
;; using the `defquery' macro.

(defmacro query
  "macro for embedding m! programs"
  [& args]
  (let [[value source]
        (if (or (symbol? (first args)) (vector? (first args)))
          [(first args) (rest args)]
          ['$value args])]
    (overriding-higher-order-functions
      (shading-primitive-procedures (if (vector? value) value [value])
        `(~'fn ~(*gensym* "query") [~value ~'$state]
           ~(-> `(~'do ~@source)
                pre/process
                (cps-of-expression result-cont)
                post/process))))))

(defmacro defquery
  "binds variable to m! program"
  [name & args]
  (let [[docstring source]
        (if (string? (first args))
          [(first args) (rest args)]
          [(format "m! program '%s'" name) args])]
    `(def ~(with-meta name {:doc docstring})
       (query ~@source))))

;;; Auxiliary macros

;; When a function is defined outside an Anglican program, it
;; must be in CPS form. fm and defm are like fn and defn but
;; automatically transform functions into CPS.

(defmacro fm
  "converts function to CPS,
  useful for defining functions outside of defanglican"
  [& args]
  (overriding-higher-order-functions
    (-> `(fn ~@args)
        pre/process
        rest fn-cps
        post/process)))

(defmacro defm
  "binds variable to function in CPS form"
  [name & args]
  (let [[docstring source]
        (if (string? (first args))
          [(first args) (rest args)]
          [(format "CPS function '%s'" name) args])
        arglist (first source)]
    `(def ~(with-meta name
             {:doc docstring
              :arglists `'([~'$cont ~'$state
                            ~@(first source)])})
       (fm ~name ~@source))))

;; In m!, memoized computations, which can be impure, are
;; created by `mem'. Inside a CPS expression, `mem' is
;; intrepreted as a special form. In a Clojure context, the macro
;; `mem' replicates the functionality of `mem' in m!.

(defmacro mem
  "creates a memoized computation in CPS form"
  [& args]
  (overriding-higher-order-functions
    (-> `(mem ~@args)
        pre/process
        rest mem-cps
        post/process)))

;; Any non-CPS procedures can be used in the code, but must be
;; either declared primitive inside the code or wrapped and
;; re-bound. `with-primitive-procedures' is deprecated in favour
;; of `(declare :primitive ...)', but supported for compatibility.

(defmacro ^:deprecated
  with-primitive-procedures
  "binds primitive procedure names to their CPS versions;
  if procedure name is qualified, it becomes unqualified in
  the scope of the macro"
  [procedures & body]
  `(let [~@(mapcat (fn [proc] 
                     [(symbol (name proc))
                      (primitive-procedure-cps proc)])
                   procedures)]
     ~@body))

;;;; Program prologue

;; Common anglican code that must be available to all
;; programs goes here. 

;;; CPS versions of higher-order functions

;; Higher-order functions must be re-implemented in CPS,
;; such that both the functions themselves and the
;; functional arguments follow the CPS calling convention.
;; Essential higher-order functions are implemented here and
;; rebound in macros defining anglican code by calling
;; `overriding-higher-order-functions'.

(declare $map $reduce
         $filter $some
         $repeatedly
         $comp $partial)

(defm ^:private $map1 
  "map on a single sequence"
  [fun lst]
  (if (empty? lst) nil
      (cons (fun (first lst))
            ($map1 fun (rest lst)))))

(defm ^:private $nils? 
  "true if the list contains nil"
  [lst]
  (and (seq lst)
       (or (nil? (first lst))
           ($nils? (rest lst)))))

(defm $map 
  "map in CPS"
  [fun & lsts]
  (let [tuple ($map1 first lsts)]
    (if ($nils? tuple) nil
        (let [lsts ($map1 rest lsts)]
          (cons (apply fun tuple)
                (apply $map fun lsts))))))

(defm ^:private $reduce1
  "reduce with explicit init in CPS"
  [fun init lst]
  (if (empty? lst) init
      ($reduce1 fun
                (fun init (first lst))
                (rest lst))))

(defm $reduce
  "reduce in CPS"
  [fun & args]
  (let [init (if (seq (rest args))
               (first args) 
               (first (first args)))
        lst (if (seq (rest args))
              (second args)
              (rest (first args)))]
    ($reduce1 fun init lst)))

(defm $filter
  "filter in CPS"
  [fun lst]
  (cond
   (empty? lst) lst
   (fun (first lst)) (cons (first lst) ($filter fun (rest lst)))
   :else ($filter fun (rest lst))))

(defm $some
  "some in CPS"
  [fun lst]
  (and (seq lst)
    (or (fun (first lst))
        ($some fun (rest lst)))))

;; `repeatedly' is useful for sampling from multivariates
;; using `transform-sample'
(defm $repeatedly
  "repeatedly in CPS"
  [n thunk]
  (if (zero? n) nil
    (cons (thunk) ($repeatedly (- n 1) thunk))))

(defm $comp
  "comp in CPS"
  [& funs]
  (if funs
    (let [funs (reverse funs)]
      (fn [& args]
        ($reduce (fn [res fun] (fun res))
                 (apply (first funs) args) (rest funs))))
    identity))

(defm $partial
  "partial in CPS"
  [fun & bound]
  (fn [& free] (apply fun (concat bound free))))
