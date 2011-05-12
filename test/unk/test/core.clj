(ns unk.test.core
  (:use [fogus.unk] :reload-all)
  (:use [clojure.test])
  (:import [fogus.unk BasicCache]))

(deftest test-lookup
  (is (= :robot (lookup (miss (BasicCache. {}) '(servo) :robot) '(servo)))))

(def id (memo identity))

(deftest test-cache-innards
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
        (is (clear-cache! id))
        (is (empty? (snapshot id)))))
    (testing "that after all manipulations, the cache maintains its identity"
      (is (identical? CACHE_IDENTITY (:unk (meta id)))))))

