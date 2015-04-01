(ns dp-church
  (:use [embang emit runtime])
  (:use dpmem)                       ; DPmem via stick breaking
  (:require [anglib.crp :as crp]))   ; DPmem via crp

;; An example from 
;;   http://www.robots.ox.ac.uk/~fwood/anglican/examples/dp_mixture_model/index.html
;; slightly modified.

(defanglican dp-church
  ;; sample-stick-index is a procedure that samples an index from
  ;; a potentially infinite dimensional discrete distribution 
  ;; lazily constructed by a stick breaking rule
  [assume sample-stick-index (lambda (breaking-rule index)
                               (if (sample (flip (breaking-rule index)))
                                 index
                                 (sample-stick-index breaking-rule (+ index 1))))]

  ;; sethuraman-stick-picking-procedure returns a procedure that picks
  ;; a stick each time its called from the set of sticks lazily constructed
  ;; via the closed-over one-parameter stick breaking rule
  [assume make-sethuraman-stick-picking-procedure (lambda (concentration)
                                                    (begin (define V
                                                             (mem (lambda (x)
                                                                    (sample (beta 1.0 concentration)))))
                                                           (lambda () (sample-stick-index V 1))))] 

  ;; DPmem is a procedure that takes two arguments -- the concentration
  ;; to a Dirichlet process and a base sampling procedure
  ;; DPmem returns a procedure 
  [assume DPmem (lambda (concentration base)
                  (begin (define get-value-from-cache-or-sample 
                           (mem (lambda (args stick-index) 
                                  (apply base args))))
                         (define get-stick-picking-procedure-from-cache 
                           (mem (lambda (args) 
                                  (make-sethuraman-stick-picking-procedure concentration))))
                         (lambda varargs
                           ;; when the returned function is called, the first thing it does is get
                           ;; the cached stick breaking procedure for the passed in arguments
                           ;; and _calls_ it to get an index
                           (begin (define index ((get-stick-picking-procedure-from-cache varargs)))
                                  ;; if, for the given set of arguments and just sampled index
                                  ;; a return value has already been computed, get it from the cache
                                  ;; and return it, otherwise sample a new value
                                  (get-value-from-cache-or-sample varargs index)))))]

  [assume H (lambda () (begin (define v (/ 1.0 (sample (gamma 1 10))))
                              (list (sample (normal 0 (sqrt (* 10 v)))) (sqrt v))))] 

  [assume gaussian-mixture-model-parameters (DPmem 1.72 H)]

  ;; instead of observe-csv
  [assume _ (reduce (lambda (_ o)
                      (observe (apply normal (gaussian-mixture-model-parameters))
                               o))
                    nil '(10 11 12 -100 -150 -200 0.001 0.01 0.005 0))]

  ;; sample* is just like sample, but does not create a checkpoint. 
  [predict (sample* (apply normal (gaussian-mixture-model-parameters)))])

;; The same example, but without re-definition of DPmem. DPmem is defined in a
;; separate module, dpmem, and imported in the namespace declaration.

(defanglican stick-breaking
  [assume H (lambda () (begin (define v (/ 1.0 (sample (gamma 1 10))))
                              (list (sample (normal 0 (sqrt (* 10 v)))) (sqrt v))))] 

  [assume gaussian-mixture-model-parameters (DPmem 1.72 H)]

  [assume _ (reduce (lambda (_ o)
                      (observe (apply normal (gaussian-mixture-model-parameters))
                               o))
                    nil '(10 11 12 -100 -150 -200 0.001 0.01 0.005 0))]

  [predict (sample (apply normal (gaussian-mixture-model-parameters)))])

;; with DPmem implemented using CRP.

(defanglican crp
  [assume H (lambda () (begin (define v (/ 1.0 (sample (gamma 1 10))))
                              (list (sample (normal 0 (sqrt (* 10 v)))) (sqrt v))))] 

  [assume gaussian-mixture-model-parameters (crp/DPmem 1.72 H)]

  [assume _ (reduce (lambda (_ o)
                      (observe (apply normal (gaussian-mixture-model-parameters))
                               o))
                    nil '(10 11 12 -100 -150 -200 0.001 0.01 0.005 0))]

  [predict (sample (apply normal (gaussian-mixture-model-parameters)))])
