(ns unk.test.core
  (:use [fogus.unk] :reload-all)
  (:use [clojure.test])
  (:import [fogus.unk BasicCache]))

(deftest test-lookup
  (is (= :robot (lookup (miss (BasicCache. {}) '(servo) :robot) '(servo)))))
