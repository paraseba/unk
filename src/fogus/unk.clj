(ns fogus.unk
  "unk is a memoization library offering functionality above Clojure's core `memoize`
   function in the following ways:

   - Pluggable memoization
   - Manipulable memoization cache

   ## Pluggable memoization

   unk allows for different back-end cache implmentations to be used as appropriate without
   changing the memoization modus operandi.

   ## Manipulable memoization

   Because unk allows you to access a function's memoization store, you do interesting things like
   clear it, modify it, and save it for later.
  "
  {:author "fogus"})

(defprotocol CacheProtocol
  "This is the protocol describing the basic cache capability."
  (lookup  [cache e]
   "Retrieve the value associated with `e` if it exists")
  (has?    [cache e]
   "Checks if the cache contains a value associtaed with `e`")
  (hit     [cache e]
   "Is meant to be called if the cache is determined to contain a value
   associated with `e`")
  (miss    [cache e ret]
   "Is meant to be called if the cache is determined to **not** contain a
   value associated with `e`"))

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

(defn- basic-cache
  [& kvs]
  (BasicCache. (apply hash-map kvs)))

(defn- through [cache f item]
  (if (has? cache item)
    (hit cache item)
    (miss cache item (delay (apply f item)))))

(deftype PluggableMemoization [f cache]
  CacheProtocol
  (has? [_ item] (has? cache item))
  (hit  [this item] this)
  (miss [_ item result]
    (PluggableMemoization. f (miss cache item result)))
  (lookup [_ item]
    (lookup cache item))
  Object
  (toString [_] (str cache)))

;; # Public API

(defn memo
  ([f] (memo #(PluggableMemoization. % (basic-cache)) f))
  ([cache-factory f]
     (let [cache (atom (cache-factory f))]
       (with-meta
        (fn [& args] 
          (let [cs (swap! cache through f args)]
            @(lookup cs args)))
        {:cache cache}))))

(comment
  (def cache (basic-cache))
  (lookup (miss cache '(servo) :robot) '(servo))
  
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
