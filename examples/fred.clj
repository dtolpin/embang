;; Automatically converted from old syntax;
;; comments were lost during conversion.

(ns fred (use (embang runtime emit)))

(defm is-fred? [person] (= person 0))

(defm
 is-guilty-fred?
 [guilty]
 (let
  [sex
   (mem
    (fn
     sex
     [person]
     (if
      (is-fred? person)
      'male
      (sample (categorical '((male 0.49) (female 0.51)))))))]
  (let
   [size
    (mem
     (fn
      size
      [person]
      (if
       (is-fred? person)
       'large
       (let
        [sex (sex person)]
        (sample
         (cond
          (= sex 'male)
          (categorical '((small 0.2) (medium 0.3) (large 0.5)))
          (= sex 'female)
          (categorical '((small 0.4) (medium 0.4) (large 0.2)))))))))]
   (let
    [hair
     (mem
      (fn
       hair
       [person]
       (if
        (is-fred? person)
        'purple
        (sample
         (categorical '((black 0.7) (blond 0.28) (purple 0.02)))))))]
    (observe
     (categorical '((small 0.1) (medium 0.2) (large 0.7)))
     (size guilty))
    (observe
     (categorical '((black 0.15) (purple 0.8) (blond 0.05)))
     (hair guilty))
    (predict '(is-fred? guilty) (is-fred? guilty))))))

(defquery
 fred
 (let
  [guilty (sample (uniform-discrete 0 100))]
  (is-guilty-fred? guilty)))

(defquery
 counting
 (let
  [guilty (sample (discrete '(0.01 0.99)))]
  (is-guilty-fred? guilty)))

