(ns advent-of-code.core-test
  (:use [clojure.test :refer :all])
  (:use [advent-of-code.day7 :only [get-all-aba get-all-abba]]))

(deftest abba-test
  (testing "day 7"
    (is (= (get-all-aba "aawbwnwnee") ["wbw" "wnw" "nwn"]))
    (is (= (get-all-aba "wbwnwn") ["wbw" "wnw" "nwn"]))
    (is (= (get-all-abba "aawbbwnnwee") ["wbbw" "wnnw"]))
    (is (= (get-all-abba "wbbwnnw") ["wbbw" "wnnw"]))))
