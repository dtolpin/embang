;; Automatically converted from old syntax;
;; comments were lost during conversion.

(ns gp (:use [embang emit runtime]))

(def
 data
 [[0.0 0.5] [1.0 0.4] [2.0 0.2] [3.0 -0.05] [4.0 -0.2] [5.0 0.1]])

(defquery
 gp
 (let
  [belief (normal 0 1)]
  (let
   [positive-belief (gamma 1 1)]
   (let
    [a (sample belief)]
    (let
     [b (sample belief)]
     (let
      [c (sample belief)]
      (let
       [d (sample positive-belief)]
       (let
        [e (sample positive-belief)]
        (let
         [m (fn m [x] (+ c (* x (+ b (* x a)))))]
         (let
          [k
           (fn
            k
            [x y]
            (let [dx (- x y)] (* d (exp (- (/ (* dx dx) 2.0 e))))))]
          (let
           [gp
            (reduce
             (fn
              [gp point]
              (let
               [d (produce gp)]
               (observe (d (first point)) (second point))
               (absorb gp point)))
             (GP m k)
             data)]
           (predict 'a a)
           (predict 'b b)
           (predict 'c c))))))))))))

(defquery
 noisy
 (let
  [belief (normal 0 1)]
  (let
   [positive-belief (gamma 2 2)]
   (let
    [a (sample belief)]
    (let
     [b (sample belief)]
     (let
      [c (sample belief)]
      (let
       [d (sample positive-belief)]
       (let
        [f (sample positive-belief)]
        (let
         [g (sample positive-belief)]
         (let
          [m (fn m [x] (+ c (* x (+ b (* x a)))))]
          (let
           [k
            (fn
             k
             [x y]
             (let
              [dx (- x y)]
              (+
               (* d (exp (- (/ (* dx dx) 2.0 f))))
               (if (= dx 0.0) g 0.0))))]
           (let
            [gp
             (reduce
              (fn
               [gp point]
               (let
                [d (produce gp)]
                (observe (d (first point)) (second point))
                (absorb gp point)))
              (GP m k)
              data)]
            (predict 'a a)
            (predict 'b b)
            (predict 'c c)))))))))))))

