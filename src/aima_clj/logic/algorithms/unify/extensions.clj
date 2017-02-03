(ns aima-clj.logic.algorithms.unify.extensions
  (require [aima-clj.logic.algorithms.unify :refer :all]
           [clj-time.core :as t])
  (import [org.joda.time DateTime]
          [java.util.regex Pattern]))

(defmethod unify* [DateTime DateTime] [x y bindings]
  (if (t/equal? x y)
    bindings
    +fail+))

;; FIXME: not safe when the same pattern occurs more than once
(defn unify-pattern [p s bindings]
  (if-let [m (re-matches p s)]
    (if (string? m)
      bindings
      (merge bindings
             (zipmap (map #(assoc (new-variable)
                                  :pattern p
                                  :group %)
                          (range))
                     m)))
    +fail+))
(defmethod unify* [Pattern String] [p s bindings]
  (unify-pattern p s bindings))
(defmethod unify* [String Pattern] [s p bindings]
  (unify-pattern p s bindings))


