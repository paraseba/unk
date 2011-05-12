;; unk.clj -- A pluggable, manipulable memoization library for Clojure

;; by Michael Fogus - <http://fogus.me/fun/trammel>
;; Feb. 2011

; Copyright (c) Michael Fogus, 2011. All rights reserved.  The use
; and distribution terms for this software are covered by the Eclipse
; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file COPYING the root of this
; distribution.  By using this software in any fashion, you are
; agreeing to be bound by the terms of this license.  You must not
; remove this notice, or any other, from this software.
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
   value associated with `e`")
  (clear   [cache]
   "Is used to signal that the cache should be cleared of all elements.
   The contract is that said cache should return an empty instance of its
   own type."))

(deftype BasicCache [cache]
  CacheProtocol
  (lookup [_ item]
    (get cache item))
  (has? [_ item]
    (contains? cache item))
  (hit [this item] this)
  (miss [_ item result]
    (BasicCache. (assoc cache item result)))
  (clear [_]
    (BasicCache. {}))
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
  (clear [_]
    (PluggableMemoization. f (clear cache)))
  Object
  (toString [_] (str cache)))

;; # Public API

(defn memo
  "Used as a more flexible alternative to Clojure's core `memoization`
   function.  Memoized functions built using `memo` will respond to
   the core unk manipulable memoization utilities.  As a nice bonus,
   you can use `memo` in place of `memoize` without any additional
   changes.

   You can access the memoization cache directly via the `:unk` key
   on the memoized function's metadata.  However, it is advised to
   use the unk primitives instead as implementation details may
   change over time."
  ([f] (memo #(PluggableMemoization. % (basic-cache)) f))
  ([cache-factory f]
     (let [cache (atom (cache-factory f))]
       (with-meta
        (fn [& args] 
          (let [cs (swap! cache through f args)]
            @(lookup cs args)))
        {:unk cache}))))

(defn snapshot
  "Returns a snapshot of an unk-placed memoization cache.  By snapshot
   you can infer that what you get is only the cache contents at a
   moment in time."
  [memoized-fn]
  (when-let [cache (:unk (meta memoized-fn))]
    (.cache (.cache @cache))))

(defn memoized?
  "Returns true if a function has an unk-placed cache, false otherwise."
  [f]
  (boolean (:unk (meta f))))

(def ^{:private true
       :doc "Returns a function's cache identity."}
  cache-id #(:unk (meta %)))

(defn memo-clear!
  "Reaches into an unk-memoized function and clears the cache.  This is a
   destructive operation and should be used with care.

   Keep in mind that depending on what other threads or doing, an
   immediate call to `snapshot` may not yield an empty cache.  That's
   cool though, we've learned to deal with that stuff in Clojure by
   now."
  [f]
  (when-let [cache (cache-id f)]
    (swap! cache (constantly (clear @cache)))))


