(ns day6
  (:require
   [clojure.repl :refer :all]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.test :refer :all])
  (:import [java.io StringReader
            BufferedReader]))

(def lines
  (io/reader "input/day6.txt"))


(def sample
  (BufferedReader.
   (StringReader.
    "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN")))

(defn parse [input-reader]
  (into {} (for [line (line-seq input-reader)]
             (-> (str/split line #"\)")
                 (reverse)
                 (vec)))))

(def orbits
  (parse lines))

(def sample-orbits (parse sample))

(defn num-orbits [system body]
  (loop [body body
         depth 0]
    (if-let [parent (get system body)]
      (recur parent (inc depth))
      depth)))

(reduce + (map (partial num-orbits orbits) (keys orbits)))

(defn path-to-root [system body]
  (reverse
   (loop [body body
          path [body]]
     (if-let [parent (get system body)]
       (recur parent (conj path parent))
       path))))


(defn path-between-bodies [orbits a b]
  (let [path-a (path-to-root orbits a)
        path-b (path-to-root orbits b)
        common-root (->> (map vector path-a path-b)
                         (take-while (fn [[a b]] (= a b)))
                         (last)
                         (first))
        tail-a (count (drop-while (partial not= common-root) path-a))
        tail-b (count (drop-while (partial not= common-root) path-b))]

    (+ tail-a tail-b -4)))

(path-to-root sample-orbits "YOU")
(path-to-root sample-orbits "SAN")

(path-between-bodies sample-orbits "YOU" "SAN")
(path-between-bodies orbits "YOU" "SAN")

(get orbits "SAN")

(take 3 [1 2])

(take 2 orbits)
