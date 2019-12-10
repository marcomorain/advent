(ns day4
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.test :refer :all]))

(def input [231832 767346])

(defn valid? [^Long x]
  (let  [repr (seq (Long/toString x))]
    (and (= 6 (count repr))
         (= repr (sort repr))
         (< 1 (apply max (vals (frequencies repr)))))))

(defn valid2? [^Long x]
  (let  [repr (seq (Long/toString x))]
    (and (= 6 (count repr))
         (= repr (sort repr))
         (contains? (set (vals (frequencies repr))) 2))))

(count (filter valid2? (range 231832 (inc 767346))))
