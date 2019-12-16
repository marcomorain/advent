(ns day2
  (:require
   [clojure.string :as str]
   [intcode]
   [clojure.test :refer :all]))

(def input (slurp "input/day2.txt"))
(def op-codes (intcode/op-codes input))

(defn program [noun verb]
  (assoc op-codes 1 noun 2 verb))

(deftest part1
  (is (= 5305097
         (-> (intcode/run* (program 12 2) [])
             :memory
             (first)))))

(deftest part2
  (is (= 4925
         (get
          (into {} (for [noun (range 0 100)
                         verb (range 0 100)]
                     [(first (:memory (intcode/run* (program noun verb) [])))
                      (+ verb (* 100 noun))]))
          19690720))))

(run-tests)
