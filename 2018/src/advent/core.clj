(ns advent.core
  (:require [clojure.string :as s]
            [clojure.test :refer :all]))

(defn ->long [^String s]
  (Long/parseLong s 10))

(defn parse [^String s]
  (map s/trim (s/split s #"[,\n]")))

(defn freq [^String s]
  (reduce + (map ->long (parse s))))

(deftest day1
  (is (= 3 (freq "+1, +1, +1")))
  (is (= 0 (freq "+1, +1, -2")))
  (is (= -6 (freq "-1, -2, -3"))))

(defn first-duplicate [coll]
  (loop [seen #{}
         [h & t] coll]
    (cond
      (nil? h) nil
      (contains? seen h) h
      :else (recur (conj seen h) t))))

(defn freq2 [^String s]
  (first-duplicate (reductions + (cycle (map ->long (parse s))))))

(defn freq2 [^String s]
  (reductions + (map ->long (parse s))))

(run-tests)

(freq2 "+1, -1")
(freq2 "+3, +3, +4, -2, -4")
(freq2 "-6, +3, +8, +5, -6")
(freq2 "+7, +7, -2, -7, -4")

(freq2 (slurp "input/1"))

(defn box-id [^String s]
  (set (vals (frequencies s))))

(def day2 (s/split-lines (slurp "input/2")))

(def ids (map box-id day2))

ids
(def n2s (count (filter #(contains? % 2) ids)))
(def n3s (count (filter #(contains? % 3) ids)))

(* n2s n3s)

(defn diffs [^String a ^String b]
  (= 1 (get (frequencies (map = a b)) false)))

(diffs "aaa" "aac")

(box-id "bababc")

(def box-ids (set (flatten (for [a day2
                                 b day2
                                 :when (diffs a b)]
                             [a b]))))


box-ids


(prn (apply str (map first  (filter #(apply = %) (partition 2 (apply interleave box-ids))))))

(filter (fn [[a b]] (not= a b)) (map vector box-ids))

(defn parse-claim [^String claim]
  (apply hash-map
         (interleave [:id :x :y :w :h]
                     (map ->long (rest (re-matches #"#(\d*) @ (\d+),(\d+): (\d+)x(\d+)" claim))))))

(parse-claim "#8 @ 869,850: 17x19")

(def claims
  (map parse-claim (s/split-lines (slurp "input/3"))))

(defn squares [{:keys [id x y w h]}]
  (reduce merge (for [i (range x (+ x w))
        j (range y (+ y h))]
    {[i j] id})))

(defn overlap?  [ax aw bx bw]
  (or (< ax bx (+ ax aw))
      (< bx ax (+ bx bw))))

(defn intersect? [a b]
  (and (overlap? (:x a) (:w a) (:x b) (:w b))
       (overlap? (:y a) (:h a) (:y b) (:h b))))

(for [a claims
      b claims
      :when (and (not= (:id a) (:id b)
                       )
                 (intersect? a b)
                 )
    ]
     
     
     
     )
