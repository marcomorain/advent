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


(defn pass-input [s1 s2]
  (assoc s2 :input (concat (:input s2) (:output s1))
         :output []))


(pass-input {:input [1] :output [2 3]}
            {:input [:a :b] :output [:c :d]})

(defn dbg [amp]
  (-> amp
      (update :memory (partial take 10))
      (assoc :op (nth (:memory amp) (:pc amp)))
      (prn)))

(defn run-amplifiers-linked [op-codes phases]
  (let [amplifiers (mapv (fn [phase]
                           {:memory op-codes
                            :input [phase]
                            :output []
                            :pc 0}) phases)
        amplifiers (update-in amplifiers [0 :input] conj 0)]
    (loop [amplifiers amplifiers
           depth 100]
      ; #_(doseq [a amplifiers]
      ;   (dbg a))
      ; #_(println)
      (if (and (= :done (:status (first amplifiers)))
               (seq (:input (first amplifiers))))
        (first (:input (first amplifiers)))
        (recur (map
                (fn [prev-a this-a]
                  (intcode/run-re-entrant (pass-input prev-a this-a)))
                (drop 4 (cycle amplifiers))
                amplifiers)
               (dec depth))))))

(deftest part-one
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

  (is (= 139629729
         (run-amplifiers-linked
          [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
          [9,8,7,6,5])))
  
  
  (is (= 18216
         (run-amplifiers-linked
          [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]
          [9,7,8,5,6])))
  
  (let [ranges
        (for [a (range 5 10)
              b (range 5 10)
              c (range 5 10)
              d (range 5 10)
              e (range 5 10)
              :when (= 5 (count (set [a b c d e])))]
          (run-amplifiers-linked op-codes
                                 [a b c d e]))]
    (is (= 4248984 (apply max ranges)))))


(test-var #'part-two)
