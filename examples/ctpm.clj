(ns ctpm
  (require embang.state)
  (use [embang runtime emit]
       ctp-data))

;;; Canadian Traveller Problem

;; The graph is attributed by two probabilities for each
;; edge --- that the edge is open, and that the traveller
;; [dis]likes the edge.
;;
;; The objective is to learn a policy of traversal order
;; that maximizes the probability of arriving alive. 
;;
;; The stochastic policy is represented by vector of
;; probabilities of selecting each edge.

;;; Default values for parameters. 

;; Parameters can be passed via the initial value as
;;   (p-open cost).

(def P-OPEN "probability that the edge is open" 0.5)
(def COST "multiplier for edge costs" 1)
(def INSTANCE "problem instance" 20)
(def NITER "number of iterations" 100)

(def-cps-fn travel [graph s t p-open cost policy]
  ;; All edges are open or blocked with the same probability.
  ;; The results are conditioned on this random choice, hence
  ;; the choice is hidden (*) from the inference algorithm,
  ;; and re-considered at each invocation of travel.
  (let [open? (mem (fn [u v] 
                     (let [is-open (sample* (flip p-open))]
                       (if is-open
                         ;; Keep counts of open and closed
                         ;; explored edges for predicting.
                         (store ::nopen (inc (or (retrieve ::nopen) 0)))
                         (store ::nblocked (inc (or (retrieve ::nblocked) 0))))
                       is-open)))

        ;; Used to compute the walk distance.
        edge-weight (fn [u v]
                      (some (fn [child]
                               (if (= (first child) v)
                                 (second child)))
                             (nth graph u)))

        ;; Returns true when t is reachable from u.
        ;; Updates the distance to the goal as a side effect,
        ;; via observing edges.
        dfs (fn dfs [u t]
              (if (= u t)
                (list true 0.)
                ((fn loop [policy passed]
                   ;; On every step of the loop, filter visited
                   ;; edges from the policy.
                   (let [policy
                         (filter
                           (fn [choice]
                             (not (contains?
                                    (retrieve ::visited)
                                    (sort (list u (first choice))))))
                           policy)]

                     (if (empty? policy)
                       (list false passed)
                       (let [dist (categorical policy)
                             ;; We implement here stochastic policy
                             ;; and do not want to learn the best
                             ;; path, but rather to win on average.
                             ;; Again, sampling is hidden from MH.
                             v (sample* dist)]
                         ;; Search for the goal in the subtree.
                         (store ::visited
                                (conj (retrieve ::visited)
                                      (sort (list u v))))
                         (let [res (dfs v t)
                               passed (+ passed
                                         (edge-weight u v)
                                         (second res))]
                           (if (first res)
                             ;; Goal found in the subtree.
                             (list true passed)
                             ;; Continue the search in another
                             ;; subtree.
                             (loop  
                               policy
                               ;; Add the weight of the edge
                               ;; through which we return.
                               (+ passed (edge-weight v u)))))))))

                 ;; Initialize policy for the node by transition
                 ;; weights for all open edges.
                 (filter
                   (fn [choice]
                     (open? u (first choice)))
                   (policy u))

                 ;; Start with zero passed distance.
                 0.)))]

    (store ::visited (set ()))
    (dfs s t)))

(defquery ctpm "expected path cost" parameters

  (let [parameters (if (seq parameters)
                     parameters
                     (list parameters))
        p-open (or (first parameters) P-OPEN)
        cost (or (second parameters) COST)
        instance (get ctp-data
                      (or (second (rest parameters)) INSTANCE))
        niter (or (second (rest (rest parameters))) NITER)]

    ;; Fix policy for all iterations.
    (let [graph (get instance :graph)
          ;; Policy is conditioned on the parent node p and
          ;; current node u.
          policy (mem (fn [u]
                        (let [children (map first (nth graph u))]
                          (map list
                               children
                               ;; This is what we want to learn,
                               ;; expose it to MH.
                               (sample (dirichlet
                                         (repeat (count children) 1.)))))))]

      ((fn loop [n sum]
         (if (= n niter)
           ;; Found niter connected items.
           (do
             ;;; Debugging predicts.

             ;; Policy at start node.
             (let [s (get instance :s)]
               ((fn loop [s-trans]
                  (if (seq s-trans)
                    (do
                      (predict (list 'T s (first (first s-trans)))
                               (second (first s-trans)))
                      (loop (rest s-trans)))))
                (policy s)))

             ;;; The average distance should decrease with convergence.
             (let [distance (/ sum n)]
               ;; Observe how we liked the journey.
               (observe (flip (exp (- (* cost distance)))) true)
               (predict 'distance distance)))


           ;; Continue to next iterations.
           (let [res (travel (get instance :graph)
                             (get instance :s) (get instance :t)
                             p-open cost
                             policy)]
             (if (first res) 
               ;; Connected instance.
               (loop (inc n) (+ sum (second res)))
               ;; Disconnected instance.
               (loop n sum)))))
       0 0.))))