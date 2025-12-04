(ns day-2
  (:require
   [clojure.string :as str]))

(def inp (slurp "day_2/input.in"))

(defn parser
  [s]
  (-> s
      (str/split #",")
      (->> (map #(str/split % #"-"))
           (map (fn [[n1 n2]] [(parse-long n1) (-> n2 parse-long inc)])))))

(defn silly?
  [n]
  (let [s                  (str n)
        len                (count s)
        half-len-or-zero   (if (even? len) (/ len 2) 0)
        half-string        (subs s 0 half-len-or-zero)
        second-half-string (subs s half-len-or-zero)]
    (= half-string second-half-string)))

(defn sol
  [pred]
  (->> inp
       parser
       (mapcat #(apply range %))
       (filter pred)
       (reduce +)))

(defn first-half
  []
  (sol silly?))

(first-half) ; => 44487518055

(defn silly-2-helper?
  [s p len]
  (loop [i  0
         p' p]
    (if (<= p' (- len p))
      (and (= (subs s i p')
              (subs s p' (+ p p')))
           (recur p' (+ p' p)))
      true)))

(defn divisors
  [n]
  (->> n
       (range 1)
       (filter #(-> n (mod %) zero?))))

(defn silly-2?
  [n]
  (let [s   (str n)
        len (count s)
        ds  (divisors len)]
    (some #(silly-2-helper? s % len) ds)))

(defn second-half
  []
  (sol silly-2?))

(second-half) ; => 53481866137
