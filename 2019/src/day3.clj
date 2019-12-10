(ns day3
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.test :refer :all]))

(def lines (line-seq (io/reader "input/day3.txt")))

(defn parse [s]
  (let [[_ direction distance-str] (re-find #"(.)(\d+)" s)
        distance (Integer/parseInt distance-str 10)]
    (case direction
      "U" [0 distance]
      "D" [0 (- distance)]
      "L" [(- distance) 0]
      "R" [distance 0])))

(defn parse [s]
  (doall (let [[_ direction distance-str] (re-find #"(.)(\d+)" s)
               distance (Integer/parseInt distance-str 10)
               coords (range 1 (inc distance))]
           (case direction
             "U" (for [c coords]
                   [0 1])
             "D" (for [c coords]
                   [0 -1])
             "L" (for [c coords]
                   [-1 0])
             "R" (for [c coords]
                   [1 0])))))

(mapcat parse (str/split (first lines) #","))

(defn points [directions]
  (reductions (fn [a b]
                (mapv + a b))
              directions))
  

(defn run [path-1 path-2]
  (let [a  (points (mapcat parse (str/split path-1 #",")))
        b  (points (mapcat parse (str/split path-2 #",")))]
    (prn b)
    (set/intersection (set (rest a)) (set (rest b)))))



(defn manhattan [[x y]]
  (+ (Math/abs x)
     (Math/abs y)))




(map manhattan (run "R75,D30,R83,U83,L12,D49,R71,U7,L72"
      "U62,R66,U55,R34,D71,R55,D58,R83"))

(map manhattan (run "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                    "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))



;; part 2

(defn signal-delay [path-a path-b [x y :as p]]
  (let [a-index (first (keep-indexed (fn [index item]
                                       (when (= p item)
                                         index)) path-a))
        b-index (first (keep-indexed (fn [index item]
                                       (when (= p item)
                                         index)) path-b))]
    (+ a-index b-index)))


(defn run2 [path-1 path-2]
  (let [a  (points (mapcat parse (str/split path-1 #",")))
        b  (points (mapcat parse (str/split path-2 #",")))
        intersections (set/intersection (set (rest a)) 
                                        (set (rest b)))
        closest (apply min (map (partial signal-delay a b) intersections))]
(+ 2 closest)
    ))

(run2 "R75,D30,R83,U83,L12,D49,R71,U7,L72"
      "U62,R66,U55,R34,D71,R55,D58,R83")

(run2 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
      "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

(time (apply run2 lines))
