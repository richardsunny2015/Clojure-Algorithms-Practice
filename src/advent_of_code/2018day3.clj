(ns advent-of-code.2018day3
  (:require
   [clojure.set :as set]
   [clojure.string :as string]))

;; https://adventofcode.com/2018/day/3

;; plan:
;; parse input into coordinates
;; then...
;;   maintain two sets: the all-set and the intersection-set
;;   for each claim
;;     ask all-set if intersection, then add to intersection-set
;;   count intersection set

(defn claim->coords [claim-line]
  (let [[_  x y w d] (->> claim-line
                          (re-matches #"#\d+ @ (\d+),(\d+): (\d+)x(\d+)")
                          (map parse-long))]
    (set (for [i (range x (+ x w))
               j (range y (+ y d))]
           [i j]))))

;; part 1
(->> (string/split-lines (slurp "resources/2018_day_3"))
     (map claim->coords)
     (reduce (fn [acc coords]
               (-> acc
                   (update :all #(set/union % coords))
                   (update :overlaps #(set/union % (set/intersection coords (:all acc))))))
             {:all #{}
              :overlaps #{}})
     :overlaps
     count)

;; part 2

(let [all-coords (->> (slurp "resources/2018_day_3")
                  (string/split-lines)
                  (map claim->coords))
      overlaps (->> all-coords
                    (reduce (fn [acc coords]
                              (-> acc
                                  (update :all #(set/union % coords))
                                  (update :overlaps #(set/union % (set/intersection coords (:all acc))))))
                            {:all #{}
                             :overlaps #{}})
                    :overlaps)]
  (->> all-coords
       (keep-indexed
        (fn [i coords]
          (when (empty? (set/intersection coords overlaps))
            i)))
       (first)
       (inc)))