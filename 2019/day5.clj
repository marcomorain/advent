(ns day5
  (:require
   [intcode :as intcode]
   [clojure.repl :refer :all]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.test :refer :all]))

(def input (slurp "input/day5.txt"))


(deftest intcode-test
  (is (= [3 0 0 0 0 0 0 0 0 13346482]
         (intcode/run2 (intcode/op-codes input) [1])))

  (is (= [12111395]
         (intcode/run2 (intcode/op-codes input) [5]))))

(run-tests)
