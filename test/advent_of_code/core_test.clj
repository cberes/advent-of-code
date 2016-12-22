(ns advent-of-code.core-test
  (:use [clojure.test :refer :all])
  (:use [advent-of-code.day7  :only [get-all-aba get-all-abba]])
  (:use [advent-of-code.day9  :only [find-repetition]])
  (:use [advent-of-code.day17 :only [solve-shortest solve-longest]])
  (:use [advent-of-code.day18 :only [count-safe-tiles]]))

(deftest abba-test
  (testing "day 7"
    (is (= (get-all-aba "aawbwnwnee") ["wbw" "wnw" "nwn"]))
    (is (= (get-all-aba "wbwnwn") ["wbw" "wnw" "nwn"]))
    (is (= (get-all-abba "aawbbwnnwee") ["wbbw" "wnnw"]))
    (is (= (get-all-abba "wbbwnnw") ["wbbw" "wnnw"]))))

(deftest compression-test
  (testing "day 9"
    (is (= (find-repetition "abc") ["abc" nil]))
    (is (= (find-repetition "abc(50x51)") ["abc" "(50x51)"]))
    (is (= (find-repetition "(50x51)abc") ["" "(50x51)"]))
    (is (= (find-repetition "abc(50x51)def") ["abc" "(50x51)"]))
    (is (= (find-repetition "[[]//abc") ["" nil]))))

(deftest solve-vault-test
  (testing "day 17"
    (is (= (solve-shortest [0 0] "ihgpwlah") "DDRRRD"))
    (is (= (solve-shortest [0 0] "kglvqrro") "DDUDRLRRUDRD"))
    (is (= (solve-shortest [0 0] "ulqzkmiv") "DRURDRUDDLLDLUURRDULRLDUUDDDRR"))
    (is (= (solve-longest [0 0] "ihgpwlah") 370))
    (is (= (solve-longest [0 0] "kglvqrro") 492))
    (is (= (solve-longest [0 0] "ulqzkmiv") 830))))

(deftest day-18-test
  (testing "day 18"
    (is (= (count-safe-tiles 3 "..^^.")) 6)
    (is (= (count-safe-tiles 10 ".^^.^.^^^^")) 38)))
