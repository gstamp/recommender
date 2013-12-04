(ns recommender.core
  (:use [clojure.set]))

;; A dictionary of movie critics and their ratings of a small
;; set of movies
(def critics
  {"Lisa Rose" {"Lady in the Water" 2.5, "Snakes on a Plane" 3.5,
                 "Just My Luck" 3.0, "Superman Returns" 3.5, "You, Me and Dupree" 2.5,
                 "The Night Listener" 3.0},
   "Gene Seymour" {"Lady in the Water" 3.0, "Snakes on a Plane" 3.5,
                    "Just My Luck" 1.5, "Superman Returns" 5.0, "The Night Listener" 3.0,
                    "You, Me and Dupree" 3.5},
   "Michael Phillips" {"Lady in the Water" 2.5, "Snakes on a Plane" 3.0,
                        "Superman Returns" 3.5, "The Night Listener" 4.0},
   "Claudia Puig" {"Snakes on a Plane" 3.5, "Just My Luck" 3.0,
                    "The Night Listener" 4.5, "Superman Returns" 4.0,
                    "You, Me and Dupree" 2.5},
   "Mick LaSalle" {"Lady in the Water" 3.0, "Snakes on a Plane" 4.0,
                    "Just My Luck" 2.0, "Superman Returns" 3.0, "The Night Listener" 3.0,
                    "You, Me and Dupree" 2.0},
   "Jack Matthews" {"Lady in the Water" 3.0, "Snakes on a Plane" 4.0,
                     "The Night Listener" 3.0, "Superman Returns" 5.0, "You, Me and Dupree" 3.5},
   "Toby" {"Snakes on a Plane" 4.5,"You, Me and Dupree" 1.0,"Superman Returns" 4.0}})

(defn shared [prefs person1 person2]
  (intersection (set (keys (prefs person1)))
                (set (keys (prefs person2)))))

(defn sum [values] (reduce + values))
(defn sqr [value] (* value value))
(defn sqrt [value] (Math/sqrt value))

(comment
  (shared critics "Mick LaSalle" "Jack Matthews")
  )

(defn calc-inverse-sum-of-squares [prefs shared-movies person1 person2]
  (/ 1
     (+ 1
        (sum (for [movie shared-movies]
               (sqr (- ((prefs person1) movie)
                       ((prefs person2) movie))))))))

(defn sim-distance[prefs person1 person2]
  (let [shared-movies (shared prefs person1 person2)]
    (if (= 0 (count shared-movies))
      0
      (calc-inverse-sum-of-squares prefs shared-movies person1 person2))))

(comment
  (sim-distance critics "Jack Matthews" "Mich LaSalle")
  (sim-distance critics "Jack Matthews" "Gene Seymour")
  (sim-distance critics "Jack Matthews" "Toby")
  )

(defn ratings-for [prefs person movies]
  (vals (select-keys (prefs person) movies)))

(defn sim-pearson[prefs person1 person2]
  (let [movies (shared prefs person1 person2)]
    (if (= 0 (count movies))
      0
      (let [ratings1        (ratings-for prefs person1 movies)
            ratings2        (ratings-for prefs person2 movies)
            sum1            (sum ratings1)
            sum2            (sum ratings2)
            sum1-sqr        (sum (map sqr ratings1))
            sum2-sqr        (sum (map sqr ratings2))
            sum-of-products (sum (map * ratings1 ratings2))
            numerator       (- sum-of-products (/ (* sum1 sum2) (count movies)))
            denominator     (sqrt (* (- sum1-sqr (/ (sqr sum1) (count movies)))
                                     (- sum2-sqr (/ (sqr sum2) (count movies)))))]
        (if (= 0 denominator)
          0
          (/ numerator denominator))))))

(comment
  (sim-pearson critics "Jack Matthews" "Gene Seymour")
  (sim-pearson critics "Jack Matthews" "Toby")
  (sim-pearson critics "Lisa Rose" "Gene Seymour")
  (ratings-for critics "Lisa Rose" movies)
  )

(defn top-matches
  ([prefs person] (top-matches prefs person 5 sim-pearson))
  ([prefs person n similarity]
     (take n
           (-> (for [[other _] prefs :when (not= other person)]
                     [(similarity prefs person other) other]) sort
                     reverse))))

(comment
  (top-matches critics "Jack Matthews")
  (top-matches critics "Toby")
  )

;; Group by the first column then reduce the second and third columns
;; using function f.
(defn reduce-hash-by[f hash]
  (for [[key values] (group-by first hash)]
    [key
     (reduce f (map second values))
     (reduce f (map #(nth % 2) values))]))

(comment
  (reduce-hash-by + [[:a 1 2] [:b 1 2] [:a 3 3]])
  )

(defn item-similarities[prefs person similarity]
  (reduce-hash-by +
                  (for [[other _] prefs
                        :when     (not= other person)
                        :let      [sim (similarity prefs person other)]
                        :when     (> sim 0)
                        [item _]  (prefs other)
                        :when     (not (contains? (prefs person) item))]
                    [item (* ((prefs other) item) sim) sim])))

(defn get-recommendations
  ([prefs person] (get-recommendations prefs person sim-pearson))
  ([prefs person similarity]
     (reverse
      (sort
       (for [[item total sim] (item-similarities prefs person similarity)]
         [(/ total sim) item])))))

(comment
  (get-recommendations critics "Toby")
  (item-similarities critics "Toby" sim-pearson)
  )


;; todo: hard to read
(defn transform-prefs[prefs]
  (into {}
        (for [[item ratings] (group-by first
                                       (for [[person ratings] prefs
                                             [item value] ratings]
                                         [item person value]))]
          [item
           (into {} (for [[_ movie rating] ratings] [movie rating]))])))

(comment
  (top-matches (transform-prefs critics) "Superman Returns")
  (get-recommendations (transform-prefs critics) "Just My Luck")
  )

(defn calculate-similar-items
  ([prefs] (calculate-similar-items prefs 10))
  ([prefs n]
     (let [item-prefs (transform-prefs prefs)]
       (into {}
             (for [[item _] item-prefs]
               [item (top-matches item-prefs item n sim-distance)])))))

(comment
  (calculate-similar-items critics)
  )

(defn get-recommended-items[prefs item-match user]
  (let [user-ratings   (prefs user)
        scores-by-item (reduce-hash-by +
                                       (for [[item rating]      user-ratings
                                             [similarity item2] (item-match item)
                                             :when              (not (user-ratings item2))]
                                         (do
                                           (println "item " item "item2" item2)
                                           [item2
                                            (* similarity rating)
                                            similarity])))]
    (reverse (sort (for [[item score total-sim] scores-by-item]
                     [(/ score total-sim) item])))))

(comment
  (get-recommended-items critics (calculate-similar-items critics) "Toby")
  )

