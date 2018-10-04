(ns purr.parse-test
  (:require
    [clojure.test :refer :all]
    [purr.parse :refer [parse]]))

(deftest split
  (is (= (parse "1 2") [1 2])))

(deftest symbols
  (is (= (parse "1 2 +") [1 2 '+])))

(deftest strings
  (is (= (parse "\"yep\"") ["yep"])))

(deftest words
  (is (= (parse "1 2 dup") [1 2 'dup])))

(deftest block
  (is (= (parse "1 [2 3]") [1 [2 3]])))

(run-tests 'purr.parse-test)
