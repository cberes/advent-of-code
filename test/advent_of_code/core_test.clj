(ns advent-of-code.core-test
  (:use [clojure.test :refer :all])
  (:use [advent-of-code.day7  :only [get-all-aba get-all-abba]])
  (:use [advent-of-code.day9  :only [find-repetition]])
  (:use [advent-of-code.day17 :only [solve-shortest solve-longest]])
  (:use [advent-of-code.day18 :only [count-safe-tiles]])
  (:use [advent-of-code.day19 :only [play-game]])
  (:use [advent-of-code.day21 :only [command-replace command-swap command-rotate
                                     command-rotate-extra command-reverse command-move]]))

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

(deftest white-elephant-test
  (testing "day 19"
    (is (= (play-game 5) 2))))

(deftest strings-test
  (testing "day 21"
    (is (= (command-swap [4 0] (into [] "abcde"))        (into [] "ebcda")))
    (is (= (command-replace [\d \b] (into [] "ebcda"))   (into [] "edcba")))
    (is (= (command-reverse [0 4] (into [] "edcba"))     (into [] "abcde")))
    (is (= (command-rotate [:left 1] (into [] "abcde"))  (into [] "bcdea")))
    (is (= (command-move [1 4] (into [] "bcdea"))        (into [] "bdeac")))
    (is (= (command-move [3 0] (into [] "bdeac"))        (into [] "abdec")))
    (is (= (command-rotate-extra [\b] (into [] "abdec")) (into [] "ecabd")))
    (is (= (command-rotate-extra [\d] (into [] "ecabd")) (into [] "decab")))
    (is (= (command-move [1 2] [\f \e \d \b \c \h \a \g]) [\f \d \e \b \c \h \a \g]))))
