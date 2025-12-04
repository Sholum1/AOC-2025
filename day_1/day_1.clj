(ns day-1
  (:require
   [clojure.string :as str]))

(def inp (slurp "day_1/input.in"))

(defn parser
  [s]
  (-> s
      (str/replace #"L|R" {"L" "-", "R" "+"})
      str/split-lines
      (->> (map parse-long))))

(defn zero-counter
  [entries init]
  (loop [total   init
         entries entries
         zeros   0]
    (let [res  (some-> entries
                       first
                       (+ total)
                       (mod 100))
          rest (next entries)]
      (cond
        (nil?  res) zeros
        (zero? res) (recur res rest (inc zeros))
        :else       (recur res rest zeros)))))

(defn first-half
  []
  (-> inp
      parser
      (zero-counter 50)))

(defn to-1s
  [entries]
  (->> entries
       (mapcat #(if (pos-int? %)
                  (range 0 %)
                  (range % 0)))
       (map #(if (neg-int? %)
               -1
               1))))

(defn second-half
  []
  (-> inp
      parser
      to-1s
      (zero-counter 50)))
