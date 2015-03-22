(ns anglican.transform
  (:require [clojure.algo.monads
             :refer [with-monad domonad m-result m-map state-m]])
  (:use [embang state trap]))

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

(declare traverse-form)

(defn traverse
  "traverses Anglican code, as produced
  by the CPS compiler (embang.trap).
  Parameters:
   - code     the code
   - tx       the transformation to apply."
  [form tx]
  ((traverse-form code) tx))

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

(defn traverse-vector
  "traverses vector"
  [form]
  (if (every? literal? form)
    (dotransform form :literal)
    (dotransform
      [txvector (m-map traverse-form form)]
      (into [] txvector) :vector)))

(defn traverse-map
  "traverses map"
  [form]
  (if (every? literal? form)
    (dotransform form :literal)
    (dotransform 
      [txmap (m-map (fn [[k v]]
                      (dotransform
                        [k (traverse-form k)
                         v (traverse-form v)]
                        [k v] :map-entry))
                    form)]
      (into {} txmap) :map)))

(defn traverse-set
  "traverse-set"
  [form]
  (if (every? literal? form)
    (dotransform form :literal)
    (dotransform 
      [txset (m-map traverse-form form)]
      (into {} txset) :set)))

(defn traverse-literal
  "traverses literal"
  [form]
  (dotransform form :literal))

(defn traverse-quote
  "traverses quote"
  [[quote literal]]
  (dotransform
    [txliteral (traverse-literal literal)]
    (list quote txliteral) :quote))

(declare traverse-form)

(defn traverse-variable
   "traverses variable definition"
   [form]
   (dotransform form :variable))

(defn traverse-parameter
   "traverses parameter"
   [form]
   (dotransform form :parameter))

(defn traverse-parameter-list
  "traverses parameter list"
  [form]
  (cond
    (a-pair? form)
    (with-monad state-m
      (domonad
        [p (traverse-parameter (first form))
         ps (traverse-parameter-list (next form))]
        (cons p ps)))

    (a-nil? form)
    (with-monad state-m
      (m-result '()))

    :else
    (dotransform form :variadic)))

(defn traverse-parameters
    "traverse parameters"
    [form]
    (if (a-list? form) (traverse-parameter-list form)
        (dotransform form :variadic)))

(defn traverse-binding
  "traverses parameter"
  [[variable expression]]
  (dotransform
    [txvariable (traverse-variable variable)
     txexpression (traverse-form expression)]
    (list txvariable txexpression) :binding))

 (defn traverse-reference
   "traverses variable reference"
   [form]
   (dotransform form :reference))

(defn traverse-application
  "traverses application"
  [form]
  (dotransform
    [txform (m-map traverse-form form)]
    txform :application))

(defn traverse-fn
  "traverses lambda form"
  [[lambda parameters & expressions]]
  (dotransform
    [txparameters (traverse-parameters parameters)
     txexpressions (m-map traverse-form expressions)]
    (list* lambda txparameters txexpressions) :lambda))

(defn traverse-let
  "traverses let form"
  [[let bindings & expressions]]
  (dotransform
    [txbindings (m-map traverse-binding bindings)
     txexpressions (m-map traverse-form expressions)]
    (list* let txbindings txexpressions) :let))

(defn traverse-if
  "traverses if form"
  [[if cond then else]]
  (if (some? else)
    (dotransform
      [txcond (traverse-form cond)
       txthen (traverse-form then)
       txelse (traverse-form else)]
      (list if txcond txthen txelse) :if)
    (dotransform
      [txcond (traverse-form cond)
       txthen (traverse-form then)]
      (list if txcond txthen) :if)))

(defn traverse-do
  "traverses begin form"
  [[begin & expressions]]
  (dotransform
    [txexpressions (m-map traverse-form expressions)]
    (list* begin txexpressions) :do))

(defn traverse-observe
  "traverses observe form, the distribution fires the
  `:distribution' rather than `:application' event"
  [[observe distribution observation]]
  (dotransform
    [txdistribution (dotransform
                      [txexpressions (m-map traverse-form distribution)]
                      txexpressions :distribution)
     txobservation (traverse-form observation)]
    (list observe txdistribution txobservation) :observe))

(defn traverse-form
  "traverses form"
  [form]
  ((cond
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

     ;; The empty list is a compound literal.
     (and (seq? form) (empty? form)) traverse-literal
     ;; A symbol is a reference.
     (symbol? form) traverse-reference
     ;; Otherwise, this is an atomic literal.
     :else traverse-literal)
   form))
