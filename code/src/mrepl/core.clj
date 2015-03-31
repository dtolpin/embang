(ns mrepl.core
  "Inference and result processing wrappers for use with Gorilla REPL"
  (:require [embang
             [inference :refer [infer warmup]]
             [core :refer [load-algorithm]]
             [state :as state]]))

;; Inference query

(defn doquery
  "performs inference query;
  returns lazy sequence of states"
  [algorithm query value & options]
  (do
    ;; Use the auto-loading machinery in embang.core to load
    ;; the inference algorithm on demand.
    (load-algorithm algorithm)
    (let [options* (apply hash-map options)]
      (try
        ;; Optionally, warm up the query by pre-evaluating
        ;; the determenistic prefix.
        (let [[query value] (if (:warmup options* true)
                                [(warmup query value) nil]
                                [query value])]
          ;; Finally, call the inference to create
          ;; a lazy sequence of states.
          (apply infer algorithm query value options))
        (catch Exception e
          (when (:debug options*)
            (.printStackTrace e *out*))
          (throw e))))))

;; State readers

(def get-predicts 
  "retrieves predicts;
  accepts a state, returns a hash map of predicts"
  (comp (partial into {}) state/get-predicts))

(def get-log-weight 
  "retrieves the log weight;
  accepts a state, returns the log weight"
  state/get-log-weight)
