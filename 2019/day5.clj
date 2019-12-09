(ns day5
  (:require
   [intcode :as intcode]
   [clojure.repl :refer :all]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.test :refer :all]))

(def input (slurp "input/day5.txt"))

(try
  (intcode/run (intcode/op-codes input) [1])
  (catch Exception e e))
*e

(try
  (intcode/run (intcode/op-codes input) [5])
  (catch Exception e e))
