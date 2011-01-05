(ns keepsake.test.core
  (:use [fogus.keepsake] :reload-all)
  (:use [clojure.test])
  (:import [fogus.keepsake BasicCache]))

(deftest test-lookup
  (is (= :robot (lookup (miss (BasicCache. {}) '(servo) :robot) '(servo)))))
