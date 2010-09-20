(ns fogus.me.keepsake)

(defprotocol CacheProtocol
  (lookup  [cache e])
  (has?    [cache e] )
  (hit     [cache e])
  (miss    [cache e ret]))

(deftype BasicCache [cache]
  CacheProtocol
  (lookup [_ item]
    (get cache item))
  (has? [_ item]
    (contains? cache item))
  (hit [this item] this)
  (miss [_ item result]
    (BasicCache. (assoc cache item result)))
  Object
  (toString [_] (str cache)))

(defprotocol MemoizationProtocol
  (through [cache e]))

(deftype PluggableMemoization [f cache]
  CacheProtocol
  (hit  [this item] this)
  (miss [_ item result]
    (PluggableMemoization. f (miss cache item result)))
  (lookup [_ item]
    (lookup cache item))
  MemoizationProtocol
  (through [this item]
    (if (has? cache item)
      (hit this item)
      (miss this item (delay (apply f item)))))
  Object
  (toString [_] (str cache)))

(defn memo
  ([f] (memo #(PluggableMemoization. % (BasicCache. {})) f))
  ([cache-factory f]
     (let [cache (atom (cache-factory f))]
       (with-meta
        (fn [& args] 
          (let [cs (swap! cache through args)]
            @(lookup cs args)))
        {:cache cache}))))

(comment
  (def slowly (fn [x] (Thread/sleep 3000) x))
  (def sometimes-slowly (memo slowly))

  (time [(sometimes-slowly 108) (sometimes-slowly 108)])
  ; "Elapsed time: 3001.611 msecs"
  ;=> [108 108]

  (time [(sometimes-slowly 108) (sometimes-slowly 108)])
  ; "Elapsed time: 0.049 msecs"
  ;=> [108 108]

  @(:cache (meta sometimes-slowly))
)
