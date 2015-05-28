;; Automatically converted from old syntax;
;; comments were lost during conversion.

(ns neural-net (use [embang runtime emit]))

(defm activate-sigmoid [v] (- (/ 2 (+ 1 (exp (* -10 (+ v -0.5))))) 1))

(defm activate-threshold [v] (if (<= v 0.5) -1 1))

(declare make-neural-net)

(defquery neural-net (make-neural-net activate-sigmoid))

(defquery threshold (make-neural-net activate-threshold))

(defm
 make-neural-net
 [activate]
 (let
  [Input (list (list -1 -1) (list -1 1) (list 1 -1) (list 1 1))]
  (let
   [Output (list 0 1 1 0)]
   (let
    [I (fn I [i l j] (nth (nth Input i) j))]
    (let
     [O (fn O [i] (nth Output i))]
     (let
      [L-MAX 3]
      (let
       [N-MAX
        (mem
         (fn
          N-MAX
          [l]
          (- (cond (= l 0) 2 (= l 1) 4 (= l 2) 2 (= l 3) 1) 1)))]
       (let
        [w (mem (fn w [l n j] (sample (normal 0 0.5))))]
        (let
         [get-input (fn get-input [f i l n j] (* (f i l j) (w l n j)))]
         (let
          [sum-inputs
           (fn
            sum-inputs
            [f i l n j]
            (if
             (= j 0)
             (get-input f i l n j)
             (+ (sum-inputs f i l n (- j 1)) (get-input f i l n j))))]
          (let
           [N
            (mem
             (fn
              N
              [i l n]
              (activate
               (if
                (= l 1)
                (sum-inputs I i (- l 1) n (N-MAX 0))
                (sum-inputs N i (- l 1) n (N-MAX (- l 1)))))))]
           (let
            [p-O
             (mem
              (fn p-O [i] (if (>= (N i L-MAX (N-MAX L-MAX)) 0) 1 0)))]
            (observe (normal (p-O 0) 0.1) (O 0))
            (observe (normal (p-O 1) 0.1) (O 1))
            (observe (normal (p-O 2) 0.1) (O 2))
            (observe (normal (p-O 3) 0.1) (O 3))
            (predict
             '(list (I 0 0 0) (I 0 0 1) (p-O 0))
             (list (I 0 0 0) (I 0 0 1) (p-O 0)))
            (predict
             '(list (I 1 0 0) (I 1 0 1) (p-O 1))
             (list (I 1 0 0) (I 1 0 1) (p-O 1)))
            (predict
             '(list (I 2 0 0) (I 2 0 1) (p-O 2))
             (list (I 2 0 0) (I 2 0 1) (p-O 2)))
            (predict
             '(list (I 3 0 0) (I 3 0 1) (p-O 3))
             (list (I 3 0 0) (I 3 0 1) (p-O 3)))))))))))))))

