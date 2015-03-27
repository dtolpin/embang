(ns embang.ast
  "Abstract syntax tree")

;;;; Abstract syntax tree 

;; Destructuring and predicates for Anglican expressions
;; provided in this module for use in code transformations.

;;; Predicates

;; Literal expressions evalute to themselves.

(defn literal? 
  "returns true when the form evalutes to itself"
  [form]
  (cond
    (seq? form) (empty? form)
    (vector? form) (every? literal? form)
    (map? form) (every? (fn [[key val]] 
                          (and (literal? key) (literal? val)))
                        form)
    (set? form) (every? literal? form)
    :else (not (symbol? form))))

;;; Destructuring

(defn fn-args
  "destructures arguments of `fn';
  returns [name args & body]"
  [args]
  (if (vector? (first args))
    ;; The name is optional.
    `[nil ~@args]
    args))

(defn if-args
  "destructures arguments of `if';
  returns [condition true-clause false-clause]"
  [[cnd thn els & rst :as args]]
  (assert (empty? rst)
          (format "Invalid number of args (%d) passed to if"
                  (count args)))
  [cnd thn els])
