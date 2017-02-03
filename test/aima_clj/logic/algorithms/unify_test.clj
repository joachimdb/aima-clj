(ns aima-clj.logic.algorithms.unify-test
  (:require [clojure.test :refer :all]
            [aima-clj.logic.algorithms.unify :refer :all]))

(deftest unify-test
  (testing "unify"
    (is (= (unify 1 1) {}))
    (is (= (unify 1 2) +fail+))
    (is (= (let [v (new-variable)]
             (unify v v))
           {}))
    (let [v1 (new-variable)
          v2 (new-variable)]
      (is (= (unify v1 v2)
             {v1 v2})))
    (let [v (new-variable)]
      (is (= (unify v 2)
             {v 2})))
    (let [v (new-variable)]
      (is (= (unify 2 v)
             {v 2})))
    (let [v (new-variable)]
      (is (= (unify [2] v)
             {v [2]})))
    (let [v1 (new-variable)
          v2 (new-variable)
          v3 (new-variable)]
      (is (= (unify [v1 [3 5 v2]] [2 [v3 5 6]])
             {v1 2, v2 6, v3 3})))
    (let [v (new-variable)]
      (is (= (unify [v v]
                    [2 2])
             {v 2})))
    (let [v (new-variable)]
      (is (= (unify [v v]
                    [1 2])
             +fail+)))
    (let [v (new-variable)]
      (is (= (unify [v 1]
                    [1 v])
             {v 1})))
    (let [v (new-variable)]
      (is (= (unify [v 2]
                    [1 v])
             +fail+)))
    
    (let [v (new-variable)]
      (is (= (unify [v [1 2]]
                    [1 [v v]])
             +fail+)))
    (let [v (new-variable)]
      (is (= (unify [v [2 2]]
                    [1 [v v]])
             +fail+)))

    (let [v (new-variable)]
      (is (= (unify [v [2 1]]
                    [1 [v v]])
             +fail+)))

    (is (= (unify {:foo "bar"} [:foo "bar"])
           +fail+))
    (is (= (unify {:foo "bar"} [[:foo "bar"]])
           {}))

    (let [v (new-variable)]
      (is (= (unify {:foo "bar"} v)
             {v {:foo "bar"}})))    
    (let [v (new-variable)]
      (is (= (unify v {:foo "bar"})
             {v {:foo "bar"}})))
    (let [v (new-variable)]
      (is (= (unify [1 2] v)
             {v [1 2]})))
    (let [v (new-variable)]
      (is (= (unify v [1 2])
             {v [1 2]})))
    (let [v (new-variable)]
      (is (= (unify {:foo "bar"} [v])
             {v [:foo "bar"]})))

    (let [v1 (new-variable)
          v2 (new-variable)]
      (is (= (unify {:foo "bar"} [[v1 v2]])
             {v1 :foo, v2 "bar"})))
    
    (let [v (new-variable)]
      (is (= (unify {:foo v} {:foo "bar"})
             {v "bar"})))

    (is (= (unify "hi there!" "hi there!")
           {}))
    (is (= (unify "foo" "hi there!")
           +fail+))))


(comment

  (unify 1 1)
  (unify 1 2)
  

  )
