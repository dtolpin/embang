(ns embang.post
  "Post-processing of CPS-transformed code")

;;; Public interface.

(defn process
  "post-processes CPS-transformed code"
  [code]
  code)

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
