(ns day9
  (:require
   [clojure.repl :refer :all]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.test :refer :all]
   [intcode :as intcode])
  (:import [java.io StringReader BufferedReader]
           [javax.imageio ImageIO]
           [java.awt.image BufferedImage]
           [java.awt Color]))

(def input
  (slurp "input/day9.txt"))

(deftest part1
  (let [program [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]]
    (is (= program
           (intcode/run program [0]))))

  (is (= [1219070632396864]
         (intcode/run [1102,34915192,34915192,7,4,7,99,0] [])))

  (is (= [1125899906842624]
         (intcode/run [104,1125899906842624,99] [])))

  (is (= [2671328082]
         (intcode/run (intcode/op-codes input) [1]))))


(deftest part2
  ;; slow
  #_(is (= [59095]
           (intcode/run (intcode/op-codes input) [2]))))

(run-tests)
