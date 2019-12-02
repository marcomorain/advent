(ns advent.core
  (:require
   [clojure.string :as str])
  (:gen-class))

(defn rotate [n coll]
  (let [start (drop n coll)
        end (take n coll)]
    (concat start end)))

(defn captcha [n s]
  (let [a (seq s)
        b (rotate n s)
        value (fn [a b]
                (if (= a b)
                  (Character/digit a 10)
                  0))
        pairs (map value a b)]
    (reduce + pairs)))

(def captcha1 (partial captcha 1))

(defn captcha2 [s]
  (captcha (/ (count s) 2) s))

(defn day1 []
  (println (format "Day 1: %s" (captcha1 (str/trim (slurp "resources/day/1/input"))))))
  
  (defn day2 []
    (println (format "Day 2: %s" (captcha2 (str/trim (slurp "resources/day/1/input"))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (day1)
  (day1))

(day1)
