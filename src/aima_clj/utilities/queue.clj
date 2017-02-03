(ns aima-clj.utilities.queue)

(defprotocol QueueP
  "A queue for storing unexplored nodes"
  (insert* [this item] "Insert one or more items into the queue. Returns an updated queue.")
  (remove-next* [this] "Remove one or more items from the queue. Returns a vector containing the new item and the updated queue"))

(extend-type clojure.lang.IPersistentList
  QueueP
  (insert* [this node] (conj this node))
  (remove-next* [this] [(first this) (rest this)]))

(extend-type clojure.lang.PersistentQueue
  QueueP
  (insert* [this node] (conj this node))
  (remove-next* [this] [(peek this) (pop this)]))

(defrecord PriorityQueue [items key]
  QueueP
  (insert* [this item] (update-in this [:items (key item)] conj item))
  (remove-next* [this]
    (if-let [[k l] (first items)]
      (let [f (first l)
            r (rest l)]
        (if (empty? r)
          [f (update-in this [:items] dissoc k)]
          [f (assoc-in this [:items k] r)]))
      [nil this])))

;;; Public API

(defn insert
  ([q] q)
  ([q item]
   (insert* q item))
  ([q item & more]
   (reduce insert* (insert* q item) more)))

(defn remove-next
  ([q] (remove-next* q))
  ([q n] (loop [q q
                items (transient [])]
           (if (= n (count items))
             [(persistent! items) q]
             (let [[nxt q] (remove-next* q)]
               (if (nil? nxt)
                 [(persistent! (conj! items nil)) q]
                 (recur q (conj! items nxt))))))))

(defn lifo-queue
  "Make a lifo queue"
  []
  ())

(defn fifo-queue
  "Make a fifo queue"
  []
  clojure.lang.PersistentQueue/EMPTY)

(defn priority-queue
  ([]
   (priority-queue identity))
  ([k]
   (PriorityQueue. (sorted-map) k)))

