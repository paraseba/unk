(ns unk.test.core
  (:use [fogus.unk] :reload-all)
  (:use [clojure.test])
  (:import [fogus.unk BasicCache]))

(deftest test-basic-cache-lookup
  (testing "that the BasicCache can lookup as expected"
    (is (= :robot (lookup (miss (BasicCache. {}) '(servo) :robot) '(servo))))))

(def id (memo identity))

(defn- test-type-transparency
  [factory]
  (let [mine (factory identity)
        them (memoize identity)]
    (testing "That the memo function works the same as core.memoize"
      (are [x y] =
           (mine 42) (them 42)
           (mine ()) (them ())
           (mine []) (them [])
           (mine #{}) (them #{})
           (mine {}) (them {})
           (mine nil) (them nil)))
    (testing "That the memo function has a proper cache"
      (is (memoized? mine))
      (is (not (memoized? them)))
      (is (= 42 (mine 42)))
      (is (not (empty? (snapshot mine))))
      (is (memo-clear! mine))
      (is (empty? (snapshot mine))))))

(deftest test-memo
  (test-type-transparency memo))

(deftest test-memo-fifo
  (let [mine (memo-fifo identity 2)]
    ;; First check that the basic memo behavior holds
    (test-type-transparency #(memo-fifo % 10))

    ;; Now check FIFO-specific behavior
    (testing "that when the limit threshold is not breached, the cache works like the basic version"
      (is (= 42 (mine 42)))
      (is (= {[42] 42} (snapshot mine)))
      (is (= 43 (mine 43)))
      (is (= {[42] 42, [43] 43} (snapshot mine)))
      (is (= 42 (mine 42)))
      (is (= {[42] 42, [43] 43} (snapshot mine))))
    (testing "that when the limit is breached, the oldest value is dropped"
      (is (= 44 (mine 44)))
      (is (= {[44] 44, [43] 43} (snapshot mine))))))

(deftest test-memo-lru
  (test-type-transparency #(memo-lru % 10)))

(deftest test-memoization-utils
  (let [CACHE_IDENTITY (:unk (meta id))]
    (testing "That an unk-populated function looks correct at its inception"
      (is (memoized? id))
      (is (snapshot id))
      (is (empty? (snapshot id))))
    (testing "That an unk-populated function looks correct after some interactions"
      ;; Memoize once
      (is (= 42 (id 42)))
      ;; Now check to see if it looks right.
      (is (find (snapshot id) '(42)))
      (is (= 1 (count (snapshot id))))
      ;; Memoize again
      (is (= [] (id [])))
      (is (find (snapshot id) '([])))
      (is (= 2 (count (snapshot id))))
      (testing "that upon memoizing again, the cache should not change"
        (is (= [] (id [])))
        (is (find (snapshot id) '([])))
        (is (= 2 (count (snapshot id)))))
      (testing "if clearing the cache works as expected"
        (is (memo-clear! id))
        (is (empty? (snapshot id)))))
    (testing "that after all manipulations, the cache maintains its identity"
      (is (identical? CACHE_IDENTITY (:unk (meta id)))))
    (testing "that a cache can be seeded and used normally"
      (is (memo-swap! id {[42] 42}))
      (is (= 42 (id 42)))
      (is (= {[42] 42} (snapshot id)))
      (is (= 108 (id 108)))
      (is (= {[42] 42 [108] 108} (snapshot id))))
    (testing "that we can get back the original function"
      (is (memo-clear! id))
      (is (memo-swap! id {[42] 24}))
      (is 24 (id 42))
      (is 42 ((memo-unwrap id) 42)))))

