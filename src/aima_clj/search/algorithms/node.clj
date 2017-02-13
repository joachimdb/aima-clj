(ns aima-clj.search.algorithms.node
  (:require [aima-clj.search.core :refer :all]
            [aima-clj.utilities.queue :refer [insert remove-next]]))

(defrecord Node
    [
     state               ; a state in the domain
     action              ; the domain action leading to state
     parent              ; the parent node of this node (or nil)
     depth               ; depth of node in tree (root = 0)
     gcost              ; path cost from root to node
     hcost              ; estimated distance from state to goal
     fcost              ; g-cost + h-cost
     ]
  )

(defn make-initial-node
  "Make the initial node for a problem"
  [problem]
  (->Node (initial-state problem) nil nil 0 0 0 0))

(defn- make-successor-node
  "Make a successor node from the current node and the next action"
  [problem node action]
  (let [{:keys [state gcost hcost fcost depth]} node
        r (result problem state action)
        g (+ gcost 
             (cost problem state action))
        h (h-cost problem state)]
    (->Node r action node (inc depth) g h (max fcost (+ g h)))))

(defn successors
  "The successor nodes of a given node for a problem"
  [problem node]
  (map (partial make-successor-node problem node)
       (actions problem (:state node))))

(defn insert-nodes
  "Insert multiple nodes into a fringe"
  [q nodes]
  (reduce insert q nodes))

(defn path-to-top
  "Return a sequence of the nodes along the path from a node to the initial node."
  ([node]
   (when-not (nil? node)
     (cons node (lazy-seq (path-to-top (:parent node)))))))

