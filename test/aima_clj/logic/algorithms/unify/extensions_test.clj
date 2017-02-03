(ns aima-clj.logic.algorithms.unify.extensions-test
  (:require [clojure.test :refer :all]
            [aima-clj.logic.algorithms.unify :refer :all]
            [aima-clj.logic.algorithms.unify.extensions :refer :all]
            [clj-time.coerce :refer [from-string]])
  (import [org.joda.time DateTimeZone DateTime]
          [java.util.regex Pattern]))

(deftest unify-datetime-extension
  (testing "unify datetimes"
    (let [dt (DateTime. 2013 02 12 4 30 0 (DateTimeZone/forOffsetHours -2))
          dt2 (clj-time.coerce/from-string "2013-02-12T04:30:00.000-02:00")]
      (is (= (unify dt dt2 {}) {})))))


(deftest unify-pattern-extension
  (testing "unify regex"
    (is (= +fail+ (unify "ba?" "bar"))) 
    (is (= {} (unify #"ba." "bar")))
    (is (= {} (unify "bar" #"ba.")))
    (is (= {} (unify #"b.rdo" "bardo")))
    (let [bs (unify #"b((.)r)(d.)" "bardo")]
      (is (= ["bardo" "ar" "a" "do"]
             (vals (sort-by #(:group (key %)) bs)))))))



