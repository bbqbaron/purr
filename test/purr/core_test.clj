(ns purr.core-test
  (:require [clojure.test :refer [deftest is run-tests]]
            [purr.run :refer [run]]))

(deftest add
  (is (= (run "1 2 +") "3")))

(deftest sub
  (is (= (run "1 2 -") "-1")))

(deftest div
  (is (= (run "1 2 /") "1/2")))

(deftest mult
  (is (= (run "3 2 *") "6")))

(deftest dup
  (is (= (run "1 dup") "1 1")))

(deftest dup2
  (is (= (run "3 1 2 dup2") "3 1 2 1 2")))

(deftest rot
  (is (= (run "4 1 2 3 rot") "4 2 3 1")))

(deftest over
  (is (= (run "4 1 2 over") "4 1 2 1")))

(deftest swap
  (is (= (run "3 1 2 swap") "3 2 1")))

(deftest drop-op
  (is (= (run "3 1 2 drop") "3 1")))

(deftest gt
  (is (= (run "1 2 >") "false"))
  (is (= (run "2 1 >") "true")))

(deftest if
  (is (= (run "2 1 > \"yep\" \"nope\" if") "yep"))
  (is (= (run "2 1 < \"yep\" \"nope\" if") "nope"))
  (is (= (run "3 2 1 > [1 +] [1 -] if") "4"))
  (is (= (run "3 2 1 < [1 +] [1 -] if") "2")))

(deftest run-block
  (is (= (run "[2 3]") "[2 3]")))

(deftest dip
  (is (= (run "4 3 [2] dip 1") "4 2 3 1")))

(deftest dip2
  (is (= (run "3 2 [5 4] dip2 1") "5 4 3 2 1")))

(deftest apply-op
  (is (= (run "3 [1 1] apply +") "3 2")))

(deftest fibonacci
  (is (= (run "1 1 [dup2 +] 5 repeat") "1 1 2 3 5 8 13")))

(deftest def-word-alias
  (is (= (run "1 \"one\" def one") "1")))

(deftest def-words
  (is (= (run "[1 +] \"inc\" def 2 inc") "3")))

(run-tests 'purr.core-test)
