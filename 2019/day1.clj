(ns day1
  (:require
   [clojure.string :as str]
   [clojure.test :refer :all]))


(defn fuel [mass]
  (int (- (Math/floor (/ mass 3)) 2)))

(defn fuel-ex [mass]
  (loop [mass mass
         acc 0]
    (let [f (fuel mass)]
      (if (pos? f)
        (recur f (+ acc f))
        acc))))
  

(fuel-ex 100756 )


(reduce + (mapv (comp fuel-ex #(Integer/parseInt %)) (str/split-lines  (slurp "input/day1.txt"))));)
