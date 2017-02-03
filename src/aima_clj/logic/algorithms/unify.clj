(ns aima-clj.logic.algorithms.unify)
;; (remove-ns 'aima-clj.logic.algorithms.unify)

(defonce ^{:doc "Indicates unification failure"} +fail+ nil)
(definline fail? [x]
  `(= +fail+ ~x))

(defrecord Variable [id])
(definline variable? [x]
  `(instance? Variable ~x))

(defonce ^{:private true} +new-variable-counter+ (atom 0))

(defn new-variable
  ([]
   (Variable. (swap! +new-variable-counter+ inc)))
  ([id]
   (Variable. id)))

(definline lookup [var bindings]
  `(get ~bindings ~var))

(defn- occurs-in?
  "Does var occur anywhere inside x?"
  [var x bindings]
  (cond (= var x) true
        (and (variable? x) (lookup x bindings))
        (recur var (lookup x bindings) bindings)        
        (and (coll? x) (not-empty x))
        (or (occurs-in? var (first x) bindings)
            (occurs-in? var (rest x) bindings))
        :else false))

(defn subst-bindings
  "Substitute the value of variables in bindings into x,
  taking recursively bound variables into account."
  [bindings x]
  (cond (fail? bindings) +fail+
        (empty? bindings) x
        (and (variable? x) (lookup x bindings))
        (recur bindings (lookup x bindings))
        (vector? x) (mapv (partial subst-bindings bindings) x)
        (map? x) (zipmap (map (partial subst-bindings bindings) (keys x))
                        (map (partial subst-bindings bindings) (vals x)))
        (and (coll? x) (not-empty x)) (into (empty x) (map subst-bindings (seq x)))
        :else x))

(defmulti unify* (fn [x y bindings] [(class x) (class y)]))

(defmethod unify* :default
  [x y bindings]
  (let [recover (or (get-method unify* [:default (class y)]))]
    ;; Prevent infinite loop:
    (if (and recover (not (= (get-method unify* :default) recover)))
      (do
        (.addMethod ^clojure.lang.MultiFn unify* [(class x) (class y)] recover)
        (recover x y bindings))
      (let [recover (get-method unify* [(type x) :default])]
        (if (and recover (not (= (get-method unify* :default) recover)))
          (do
            (.addMethod ^clojure.lang.MultiFn unify* [(class x) (class y)] recover)
            (recover x y bindings))
          (if (= x y)
            bindings
            +fail+))))))

(defn- unify-var [var x bindings]
  (cond (lookup var bindings)
        (unify* (lookup var bindings) x bindings)
        (= var x)
        bindings
        (occurs-in? var x bindings)
        +fail+
        :else (assoc bindings var x)))

(defmethod unify* [Variable Variable]
  [v1 v2 bindings]
  (if (lookup v2 bindings)
    (unify* v1 (lookup v2 bindings) bindings)
    (if (= v1 v2)
      bindings
      (assoc bindings v1 v2))))
(defmethod unify* [Variable :default]
  [var x bindings]
  (unify-var var x bindings))
(defmethod unify* [:default Variable]
  [x var bindings]
  (unify-var var x bindings))

(derive java.util.Collection ::collection)
(derive java.util.Map ::collection)
(defmethod unify* [::collection ::collection]
  [col1 col2 bindings]
  (if (fail? bindings)
    +fail+
    (if (empty? col1)
      (if (empty? col2)
        bindings
        +fail+)
      (recur (rest col1) (rest col2)
             (unify* (first col1) (first col2) bindings)))))
;; needed because a Variable is a java.util.Collection
(defmethod unify* [Variable ::collection]
  [var x bindings]
  (unify-var var x bindings))
;; needed because a Variable is a java.util.Map
(defmethod unify* [::collection Variable]
  [x var bindings]
  (unify-var var x bindings))



(defn unify
  "See if x and y match with given bindings.  If they do,
  return a binding list that would make them equal [p 303]."
  ([x y]
   (unify x y {}))
  ([x y bindings]
   (unify* x y bindings)))


(defn unifier
 "Return something that unifies with both x and y (or fail)."
  [x y]
  (subst-bindings (unify x y) x))




