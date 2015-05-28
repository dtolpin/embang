(ns dp-church (:use [embang emit runtime]) (:use dpmem) (:require crp))

(defquery
 dp-church
 (let
  [sample-stick-index
   (fn
    sample-stick-index
    [breaking-rule index]
    (if
     (sample (flip (breaking-rule index)))
     index
     (sample-stick-index breaking-rule (+ index 1))))]
  (let
   [make-sethuraman-stick-picking-procedure
    (fn
     make-sethuraman-stick-picking-procedure
     [concentration]
     (do
      (let
       [V (mem (fn V [x] (sample (beta 1.0 concentration))))]
       (fn [] (sample-stick-index V 1)))))]
   (let
    [DPmem
     (fn
      DPmem
      [concentration base]
      (do
       (let
        [get-value-from-cache-or-sample
         (mem
          (fn
           get-value-from-cache-or-sample
           [args stick-index]
           (apply base args)))]
        (let
         [get-stick-picking-procedure-from-cache
          (mem
           (fn
            get-stick-picking-procedure-from-cache
            [args]
            (make-sethuraman-stick-picking-procedure concentration)))]
         (fn
          [& varargs]
          (do
           (let
            [index ((get-stick-picking-procedure-from-cache varargs))]
            (get-value-from-cache-or-sample varargs index))))))))]
    (let
     [H
      (fn
       H
       []
       (do
        (let
         [v (/ 1.0 (sample (gamma 1 10)))]
         (list (sample (normal 0 (sqrt (* 10 v)))) (sqrt v)))))]
     (let
      [gaussian-mixture-model-parameters (DPmem 1.72 H)]
      (let
       [_
        (reduce
         (fn
          [_ o]
          (observe
           (apply normal (gaussian-mixture-model-parameters))
           o))
         nil
         '(10 11 12 -100 -150 -200 0.001 0.01 0.005 0))]
       (predict
        '(sample (apply normal (gaussian-mixture-model-parameters)))
        (sample
         (apply normal (gaussian-mixture-model-parameters)))))))))))

(defquery
 stick-breaking
 (let
  [H
   (fn
    H
    []
    (do
     (let
      [v (/ 1.0 (sample (gamma 1 10)))]
      (list (sample (normal 0 (sqrt (* 10 v)))) (sqrt v)))))]
  (let
   [gaussian-mixture-model-parameters (DPmem 1.72 H)]
   (let
    [_
     (reduce
      (fn
       [_ o]
       (observe (apply normal (gaussian-mixture-model-parameters)) o))
      nil
      '(10 11 12 -100 -150 -200 0.001 0.01 0.005 0))]
    (predict
     '(sample (apply normal (gaussian-mixture-model-parameters)))
     (sample (apply normal (gaussian-mixture-model-parameters))))))))

(defquery
 crp
 (let
  [H
   (fn
    H
    []
    (do
     (let
      [v (/ 1.0 (sample (gamma 1 10)))]
      (list (sample (normal 0 (sqrt (* 10 v)))) (sqrt v)))))]
  (let
   [gaussian-mixture-model-parameters (crp/DPmem 1.72 H)]
   (let
    [_
     (reduce
      (fn
       [_ o]
       (observe (apply normal (gaussian-mixture-model-parameters)) o))
      nil
      '(10 11 12 -100 -150 -200 0.001 0.01 0.005 0))]
    (predict
     '(sample (apply normal (gaussian-mixture-model-parameters)))
     (sample (apply normal (gaussian-mixture-model-parameters))))))))

