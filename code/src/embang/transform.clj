(ns embang.transform
  (:require [clojure.algo.monads
             :refer [with-monad domonad m-result m-map state-m]])
  (:use [embang state trap]))

(declare traverse-form)

;;; Public interface

(defn traverse
  "traverses Anglican source code, as produced
  by the CPS compiler (embang.trap).
  Parameters:
   - code     the code
   - tx       the transformation to apply."
  [form tx]
  ((traverse-form form) tx))

;;; Transformed expression predicates

;; A regular function application takes the continuation
;; and the state as two initial parameters. If the application
;; is itself a continuation, or if the applied function is
;; primitive, the argument list has a different structure:
;; a continuation always takes two arguments --- the value
;; and the state; a primitive function does not take either
;; a continuation or a state.
;;
;; Continuations and primitive function applications (either
;; direct or through 'apply') are marked in meta data by
;; corresponding flags.

(defn continuation?
  "returns true if the form is a continuation"
  [form]
  (:embang.trap/continuation (meta form)))

(defn primitive?
  "returns true if the form is a primitive call or apply"
  [form]
  (:embang.trap/primitive (meta form)))

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

;; Functions for traversing anglican programs and applying
;; source-to-source transformations.

(defn transform-dispatch
  "dispatch function for transform,
  named for ease of debugging"
  [form tag tx]
  (if (:tags tx)
    (if (or (= (:tags tx) :all) ((:tags tx) tag))
      (:type tx)
      :default)
    [tag (:type tx)]))

(defmulti transform
  "transformer called by traverse,
  dispatches on [tag (:type tx)] or on (:type tx).
  Parameters:
  - form     the form to transform;
  - tag      the form tag;
  - tx       the transformation state,
             (:type tx) defines the transformation type;
             optionally, (:tags tx) is the set of tags
             to which the transformation is applied, in
             which case the dispatch is on the
             transformation type only. If (:tags tx) is :all,
             the transformation is applied to all tags.
  - return   accepts the transformed form and the
             updated transformation state."
  transform-dispatch)

;; The default transformation is to return the unmodified form
;; and the unchanged state.

(defmethod transform :default [form _ tx] [form tx])

;; At many occasions the transformer applies transformations
;; through monadic computations to parts of an expression
;; and then calls transform on the re-built expression.
;; Since transform is (almost) monadic itself, this leads
;; to a common pattern expressed in the `dotransform' macro.

(defmacro dotransform
  "macro wrapper over monadic steps,
  if the steps are supplied and are note empty,
  the final transform is performed on the result
  of the last step"
  ([form tag] `#(transform ~form ~tag %))
  ([steps form tag]
     (if (> (count steps) 0)
       `(domonad state-m [~@steps
                          txform# #(transform ~form ~tag %)]
                 txform#)
       `(dotransform form tag))))

(defn traverse-literal
  "traverses literal"
  [literal]
  (dotransform literal :literal))

(defn traverse-vector
  "traverses vector"
  [vector*]
  (dotransform
    [txvector (m-map traverse-form vector*)]
    (into [] txvector) :vector))

(defn traverse-map
  "traverses map"
  [map*]
  (dotransform 
    [txmap (m-map (fn [[key val]]
                    (with-monad state-m
                      (domonad
                        [txkey (traverse-form key)
                         txval (traverse-form val)]
                        [txkey txval])))
                  map*)]
    (into {} txmap) :map))

(defn traverse-set
  "traverse-set"
  [set*]
  (dotransform 
    [txset (m-map traverse-form set*)]
    (into {} txset) :set))

(defn traverse-quote
  "traverses quote"
  [[quote* literal]]
  (dotransform
    [txliteral (traverse-literal literal)]
    `(~quote* ~txliteral) :quote))

(defn traverse-parameters
  "traverses parameter list"
  [parameters]
  (dotransform parameters :parameters))

(defn traverse-variable
  "traverses variable"
  [variable]
  (dotransform variable :variable))

(defn traverse-value
  "traverses variable value"
  [value]
  (dotransform 
    [txvalue (traverse-form value)]
    txvalue :value))

(defn traverse-binding
  "traverses binding"
  [[variable value]]
  (dotransform
    [txvariable (traverse-variable variable)
     txvalue (traverse-value value)]
    [txvariable txvalue] :binding))

(defn traverse-fn
  "traverses fn"
  [[fn* parameters & expressions]]
  (dotransform
    [txparameters (traverse-parameters parameters)
     txexpressions (m-map traverse-form expressions)]
    `(~fn* ~txparameters ~@txexpressions) :fn))

(defn traverse-let
  "traverses let"
  [[let* bindings & expressions]]
  (dotransform
    [txbindings (m-map traverse-binding 
                       (partition 2 bindings))
     txexpressions (m-map traverse-form expressions)]
    `(~let* [~@(apply concat txbindings)] ~@txexpressions) :let))

(defn traverse-if
  "traverses if"
  [[if* cond then else]]
  (dotransform
    [txcond (traverse-form cond)
     txthen (traverse-form then)
     txelse (traverse-form else)]
    `(~if* ~txcond ~txthen ~txelse) :if))

(defn traverse-case-clause
  "traverses case clause"
  [clause]
  (if (= (count clause) 2)
    (dotransform
      [txtag (traverse-literal (first clause))
       txexpr (traverse-form (second clause))]
      [txtag txexpr] :clause)
    (dotransform
      [txexpr (traverse-form (first clause))]
      [txexpr] :default-clause)))

(defn traverse-case
  "traverses case"
  [[case* & clauses]]
  (dotransform
    [txclauses (m-map traverse-case-clause
                      (partition 2 2 nil clauses))]
    `(~case* ~@(apply concat txclauses)) :case))

(defn traverse-do
  "traverses do"
  [[do* & expressions]]
  (dotransform
    [txexpressions (m-map traverse-form expressions)]
    `(~do* ~@txexpressions) :do))

(defn traverse-observe
  "traverses observe"
  [form]
  (dotransform form :observe))

(defn traverse-sample
  "traverses sample"
  [form]
  (dotransform form :sample))

(defn traverse-result
  "traverses result"
  [form]
  (dotransform form :result))

(defn traverse-add-predict
  "traverses add-predict"
  [form]
  (dotransform form :add-predict))

(defn traverse-get-mem
  "traverses get-mem"
  [form]
  (dotransform form :get-mem))

(defn traverse-set-mem
  "traverses set-mem"
  [form]
  (dotransform form :set-mem))

(defn traverse-store
  "traverses store"
  [form]
  (dotransform form :store))

(defn traverse-retrieve
  "traverses retrieve"
  [form]
  (dotransform form :retrieve))

(defn traverse-apply
  "traverses apply"
  [[apply* & args]]
  (dotransform
    [txargs (m-map traverse-form args)]
    `(~apply* ~@txargs) :apply))

(defn traverse-application
  "traverses application"
  [application]
  (dotransform
    [txapplication (m-map traverse-form application)]
    txapplication :application))

 (defn traverse-reference
   "traverses variable reference"
   [reference]
   (dotransform reference :reference))

(defn traverse-form
  "traverses form"
  [form]
  ((cond
     (literal? form) traverse-literal
     (vector? form) traverse-vector
     (map? form) traverse-map
     (set? form) traverse-set

     (and (seq? form) (seq form))
     (case (first form)
       ;; Clojure forms in CPS-transformed Anglican.
       quote                    traverse-quote
       fn                       traverse-fn
       let                      traverse-let
       if                       traverse-if
       case                     traverse-case
       do                       traverse-do

       ;; Checkpoints.
       embang.trap/->observe    traverse-observe
       embang.trap/->sample     traverse-sample
       embang.trap/->result     traverse-result

       ;; State operations.
       embang.state/add-predict traverse-add-predict
       embang.state/get-mem     traverse-get-mem
       embang.state/set-mem     traverse-set-mem
       embang.state/store       traverse-store
       embang.state/retrieve    traverse-retrieve

       ;; Function applications.
       clojure.core/apply       traverse-apply
       traverse-application)

     ;; A symbol is a reference.
     (symbol? form) traverse-reference
     ;; No other options.
     :else (throw (AssertionError.
                    (format "cannot traverse %s" form))))
   form))
