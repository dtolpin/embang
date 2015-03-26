(ns anglib.crp
  "crp and DPmem implementation for easy porting
  of legacy Anglican code"
  (:use [embang emit runtime]))

;;; Wrappers to ease porting code from original Anglican.

;; Stateful Chinese restaurant process, draws an index.

(defm crp [alpha]
  (let [name (gensym "crp")]
    (fn []
      (let [p (or (retrieve name) (CRP alpha))
            s (sample (produce p))]
        (store name (absorb p s))
        s))))

;; DPmem, memoizes calls to h softly.
;; h may get an arbitrary number of arguments.

(defm DPmem [alpha h]
  (let [C (crp alpha)
        f (mem (fn [s args] (apply h args)))]
    (fn [& args]
      (f (C) args))))
