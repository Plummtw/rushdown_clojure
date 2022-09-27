(ns rushhour.main-test
  (:require [clojure.test :refer [deftest testing is]]
            [rushhour.main :as main]))

(deftest test-one
  (testing "Test One"
    (is (= "one" (main/one)))))