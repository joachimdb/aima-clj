(ns aima-clj.search.algorithms.tree.simple
  (require [aima-clj.search.core :refer :all]
           [aima-clj.utilities.queue :refer :all]
           [aima-clj.search.algorithms.node :refer :all]))

(defn general
  "General tree search algorithm"
  ([problem q]
   (general problem q -1))
  ([problem q depth-limit]   
   (loop [q (insert q (make-initial-node problem))]
     (let [[node q] (remove-next q)]
       (println (:action node) "->" (:state node))
       (cond (or (nil? node)
                 (goal-state? problem (:state node)))
             node
             (and (pos? depth-limit)
                  (>= (:depth node) depth-limit))
             :cut-off
             :else
             (recur (insert-nodes q (successors problem node))))))))

(defn breadth-first
  "Search the shallowest nodes in the search tree first. [p 74]"
  ([problem]
   (breadth-first problem -1))
  ([problem depth-limit]
   (general problem (fifo) depth-limit)))

(defn depth-first
  "Search the deepest nodes in the search tree first. [p 78]"
  ([problem]
   (breadth-first problem -1))
  ([problem depth-limit]
   (general problem (lifo) depth-limit)))

(defn iterative-deepening
  "Do a series of depth-limited searches, increasing depth each time. [p 79]"
  [problem]
  (loop [depth 0]
    (let [solution (depth-first problem depth)]
      (if (= solution :cut-off)
        solution
        (recur (inc depth))))))

(defn best-first
  "Search the nodes with the best evaluation first. [p 93]"
  [problem eval-fn]
  (general problem (priority eval-fn)))

(defn greedy [problem]
  "Best-first search using H (heuristic distance to goal). [p 93]"
  (best-first problem :h-cost))

(defn a*
  "Best-first search using estimated total cost, or (F = G + H). [p 97]"
  [problem]
  (best-first problem :f-cost))

(defn uniform-cost
  "Best-first search using the node's depth as its cost.  Discussion on [p 75]"
  [problem]
  (best-first problem :g-cost))



