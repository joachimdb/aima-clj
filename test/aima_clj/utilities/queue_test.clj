(ns aima-clj.utilities.queue-test
  (:require  [clojure.test :refer :all]
             [aima-clj.utilities.queue :refer :all]))
;; (remove-ns 'aima-clj.queue-test)


(deftest priority-queue-test
  (testing "priority queue"
    (is (= (priority-queue) (second (remove-next (priority-queue)))))
    (let [q (insert (priority-queue) 5 2 9 3)]
      (is (= 2 (first (remove-next q))))
      (is (= [2 3] (first (remove-next q 2))))      
      (is (= 5 (first (remove-next (second (remove-next q 2))))))
      (is (= [2 3 5 9] (first (remove-next q 4))))
      (is (= [2 3 5 9 nil] (first (remove-next q 43)))))))

(deftest fifo-queue-test
  (testing "fifo queue"
    (is (= (fifo-queue) (second (remove-next (fifo-queue)))))
    (let [q (insert (fifo-queue) 5 2 9 3)]
      (is (= 5 (first (remove-next q))))
      (is (= [5 2] (first (remove-next q 2))))      
      (is (= 9 (first (remove-next (second (remove-next q 2))))))
      (is (= [5 2 9 3] (first (remove-next q 4))))
      (is (= [5 2 9 3 nil] (first (remove-next q 43)))))))

(deftest lifo-queue-test
  (testing "lifo queue"
    (is (= (lifo-queue) (second (remove-next (lifo-queue)))))
    (let [q (insert (lifo-queue) 5 2 9 3)]
      (is (= 3 (first (remove-next q))))
      (is (= [3 9] (first (remove-next q 2))))      
      (is (= 2 (first (remove-next (second (remove-next q 2))))))
      (is (= [3 9 2 5] (first (remove-next q 4))))
      (is (= [3 9 2 5 nil] (first (remove-next q 43)))))))
