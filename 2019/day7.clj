(ns day7
  (:require
   [clojure.repl :refer :all]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.test :refer :all]
   [intcode :as intcode])
  (:import [java.io StringReader BufferedReader]))

(def input
  (slurp "input/day7.txt"))

(def op-codes (intcode/op-codes input))

(def phase [3,1,2,4,0])

(defn run-amplifiers [op-codes phase]
  (loop [phase phase
         input 0]
    (if (empty? phase)
      input
      (recur (rest phase)
             (first
              (intcode/run op-codes [(first phase) input]))))))

(defn run-amplifiers-linked [op-codes phases]
  (let [amplifiers (map (fn [phase]
                          {:memory op-codes
                           :input [phase]
                           :output []
                           :pc 0}) phases)]
    (loop [amplifiers amplifiers]
        (if (= :done (:pc (first amplifiers)))
          (:output (:last amplifiers))
          (recur (map intcode/run-re-entrant amplifiers))))))

(deftest test
  (is (= 43210
         (run-amplifiers
          [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
          [4,3,2,1,0])))

  (is (= 54321
         (run-amplifiers
          [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]
          [0,1,2,3,4])))

  (is (= 65210
         (run-amplifiers
          [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
          [1,0,4,3,2])))

  (let [ranges
        (for [a (range 5)
              b (range 5)
              c (range 5)
              d (range 5)
              e (range 5)
              :when (= 5 (count (set [a b c d e])))]
          (run-amplifiers op-codes
                          [a b c d e]))]
    (is (= 12111395 (apply max ranges)))))


(deftest part-two
  (is (= ::a (run-amplifiers-linked op-codes [1 2 3 4 5]))))

(run-tests)
(try

  (run-amplifiers-linked op-codes [1 2 3 4 5])
  (catch Exception e e))
