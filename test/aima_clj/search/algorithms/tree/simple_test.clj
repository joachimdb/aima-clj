(ns aima-clj.search.algorithms.tree.simple-test
  (:require  [clojure.test :refer :all]
             [aima-clj.search.core :refer :all]
             [aima-clj.search.algorithms.tree.simple :refer :all]))
;; (remove-ns 'aima-clj.search.algorithms.simple-test)

;;; Canibal problem definition

;; The problem is to move M missionaries and C cannibals from one side
;; of a river to another, using B boats that holds at most two people each,
;; in such a way that the cannibals never outnumber the missionaries in
;; any one place.  See [p 68].

(defn initial-cannibal-state [m c b]
  {:m1 m :c1 c :b1 b
   :m2 0 :c2 0 :b2 0})

(def all-actions '[[+1 0 +1] [0 +1 +1] [+2 0 +1] [0 +2 +1] [+1 +1 +1]
                   [-1 0 -1] [0 -1 -1] [-2 0 -1] [0 -2 -1] [-1 -1 -1]])

(defn take-the-boat
  "Move a certain number of missionaries, cannibals, and boats (if possible)."
  [{:keys [m1 c1 b1 m2 c2 b2] :as state} [delta-m delta-c delta-b]]
  (when (or (and (= delta-b +1) (> b1 0))
            (and (= delta-b -1) (> b2 0)))    
    (let [new {:m1 (- m1 delta-m) :c1 (- c1 delta-c) :b1 (- b1 delta-b)
               :m2 (+ m2 delta-m) :c2 (+ c2 delta-c) :b2 (+ b2 delta-b)}]
      (when (and (>= (:m1 new) 0) (>= (:m2 new) 0)
                 (>= (:c1 new) 0) (>= (:c2 new) 0))
        new))))

(defn cannibals-can-eat?
  "The cannibals feast if they outnumber the missionaries on either side."
  [state]
  (or (> (:c1 state) (:m1 state) 0)
      (> (:c2 state) (:m2 state) 0)))

(defn valid-action?
  [state action]
  (if-let [next (take-the-boat state action)]
    (not (cannibals-can-eat? state))
    false))

(defrecord CannibalProblem [m c b]
  Problem
  (initial-state [this]
    (initial-cannibal-state m c b))
  (actions [this state]
    (filter (partial valid-action? state) all-actions))
  (result [this state action]
    (take-the-boat state action))
  (goal-state? [this state]
    (= 0 (:m1 state) (:c1 state)))
  (cost [this state action] 1)
  (h-cost [_ _] 0))

;;; End of Canibal problem definition

(deftest simple-search-test
  (testing "breadth first on cannibal problem"
    (let [p (CannibalProblem. 0 3 1)]
      (is (= (breadth-first p 3) :cut-off))
      (is (= (:state (breadth-first p 5))
             {:m1 0, :c1 0, :b1 0, :m2 0, :c2 3, :b2 1}))))
  (testing "depth first on cannibal problem"
    (let [p (CannibalProblem. 0 3 1)]
      (is (= (depth-first p 3) :cut-off))
      (is (= (depth-first p 5) :cut-off))))  
  (testing "greedy search on cannibal problem"
    (let [p (CannibalProblem. 0 3 1)]
      (is (= (greedy p 3) :cut-off))
      (is (= (greedy p 5) :cut-off))))
  (testing "a* search on cannibal problem"
    (let [p (CannibalProblem. 0 3 1)]
      (is (= (a* p 2) :cut-off))
      (is (= (:state (a* p 4)) {:m1 0, :c1 0, :b1 0, :m2 0, :c2 3, :b2 1}))))
  (testing "uniform-cost search on cannibal problem"
    (let [p (CannibalProblem. 0 3 1)]
      (is (= (uniform-cost p 2) :cut-off))
      (is (= (:state (uniform-cost p 4)) {:m1 0, :c1 0, :b1 0, :m2 0, :c2 3, :b2 1}))))
  )



