(ns unk.test.core
  (:use [fogus.unk] :reload-all)
  (:use [clojure.test])
  (:import [fogus.unk BasicCache]))

(deftest test-lookup
  (is (= :robot (lookup (miss (BasicCache. {}) '(servo) :robot) '(servo)))))

(def id (memo identity))

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

