(ns keepsake.test.core
  (:use [fogus.anamnesis] :reload-all)
  (:use [clojure.test])
  (:import [fogus.anamnesis BasicCache]))

(deftest test-lookup
  (is (= :robot (lookup (miss (BasicCache. {}) '(servo) :robot) '(servo)))))
