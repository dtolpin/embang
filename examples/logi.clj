;; Automatically converted from old syntax;
;; comments were lost during conversion.

(ns logi (:use [embang emit runtime]))

(defquery
 logi
 "logistic regression"
 (let
  [m (sample (normal 0 1))]
  (let
   [sigma (sqrt (sample (gamma 1 1)))]
   (let
    [m (sample (normal 0 sigma))]
    (let
     [b (sample (normal 0 sigma))]
     (let
      [y (fn y [x] (+ (* m x) b))]
      (let
       [z (fn z [x] (/ 1 (+ 1 (exp (* -1 (y x))))))]
       (observe (flip (z -10)) false)
       (observe (flip (z -5)) false)
       (observe (flip (z 2)) true)
       (observe (flip (z 6)) true)
       (observe (flip (z 10)) true)
       (predict '(sample (flip (z 8))) (sample (flip (z 8)))))))))))

