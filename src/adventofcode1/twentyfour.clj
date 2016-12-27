(ns adventofcode1.twentyfour
  "http://adventofcode.com/2016/day/24"
  (:require
   [clojure.spec :as s]
   [clojure.spec.gen :as gen]
   [clojure.spec.test :as stest]
   [com.gfredericks.test.chuck.generators :as gen']
   [clojure.test.check :as tc]
   [clojure.test.check.generators :as tcgen]
   [clojure.test.check.properties :as tcprop]

   [clojure.core.match :refer [match]]
   [clojure.math.combinatorics :as combo]
   [clojure.string :as str]
   [com.rpl.specter :as sp]

   spyscope.core
   mw.utils
   adventofcode1.spec-test-instrument-debug))

(declare puzzle-input-a)

;; alternative implementations
;; 1. core.logic
;; 1. clpfd which each cell a fd-domain of 0,1,2
;; 1. raw breadth search in clojure
;; 1.
;; 1.
;; 1.

;; 1. instead of array represenation of cells, just handle the sub-paths
;;    instead of
;;     01234567890
;;    0###########
;;    1#0.1.....2#
;;    2#.#######.#
;;    3#4.......3#
;;    4###########
;;    use
;;    1.1 - 3.1
;;    3.1 - 9.1
;;    1.1 - 1.3
;;    1.3 - 9.3
;;    9.1 - 9.3
;;
;; How to find these path
;; 1. by walking the mace
;; 2. trying all horizontal and vertical line and ending path if more than 2 neighbors or number or wall

;; performance tips:
;; * The map is big! So, if we do depth-first-search, we need to avoid infinite paths
;; * Never visit the same cell more than twice in a path (if dead-end, you might have to go back)
;; * Never go back unless dead-end or visited numbered cell

;; specter ideas: for example how to go to any eelement
;; https://github.com/nathanmarz/specter/issues/57


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; parallel ideas.
;; stest/gen uses pmap, which uses (future x) + (deref x)
;; it derefs (.. Runtime getRuntime availableProcessors) (== 4)
;; elements at a time


;; (def max-width 10)
;; (def max-height 5)


(def sample1
  "###########
  #0.1.....2#
  #.#######.#
  #4.......3#
  ###########")

(def sample2
  "###########
  #0.1.....2#
  #.###.###.#
  #4....6..3#
  ###########")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; types

;; (s/def ::full-name (s/with-gen string? #(gen'/string-from-regex #"([a-c]+-){3,}[0-9]+\[[a-z]+\]"))) ; => YES
;; (s/def ::name (s/with-gen string? #(gen'/string-from-regex #"([a-c]+-){3,}"))) ; => YES

(s/def ::cell #{\# \. \0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(defn board-chars?
  "Return true of only valid board chars in board row `s`."
  [s]
  (every? #{\# \. \0 \1 \2 \3 \4 \5 \6 \7 \8 \9} (seq s)))

;; (s/def ::board-row (s/coll-of #{\# \. \1 \2 \3 \4 \5 \6 \7 \8 \9} :count max-width))
(s/def ::board-row (s/with-gen (s/and string? board-chars?) #(gen'/string-from-regex #"#([0-9.#]{10})#")))
(s/def ::board (s/coll-of ::board-row)) ;; :count max-height)

(s/def ::from (s/tuple integer? integer?))
(s/def ::to   (s/tuple integer? integer?))
(s/def ::path  (s/keys :req-un [::from ::to]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; board parsing

(defn board-width
  [b]
  (count (nth b 0)))

(defn board-height
  [b]
  (count b))

(s/fdef board-cell
        :args (s/cat :board ::board :row integer? :col integer?)
        :ret  ::cell)

(defn board-cell
  "Get the char at `row` `col` in `board`."
  [board row col]
  (assert (and (>= row 0)
               (>= col 0)
               (< row (board-height board))
               (< col (board-width board)))
          "Accessing cell outside board!")
  (nth (nth board row) col))

(defn board-make
  [s]
  (let [board (vec (map str/trim (str/split-lines s)))]
    (assert (every? #(= (count %)(board-width board))
                    board)
            "Board rows different lenght")
    board))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; identify all paths
;;
;; all path start and end are inclusive, since otherwise I cannot reverse them

(s/fdef count-neighbors
        :args (s/cat :board ::board :row integer? :col integer?)
        :ret  integer?)

;; (defn count-neighbors-including-diagonals
;;   "Count non-wall neighbors."
;;   [board row col]
;;   (reduce +
;;           (for [r (range -1 2)]
;;            (reduce +
;;             (for [c (range -1 2)]
;;               (if (and (zero? r)(zero? c))
;;                 0
;;                 (if (= \# (board-cell board (+ row r) (+ col c)))
;;                   0
;;                   1)))))))

(defn count-neighbors
  "Count non-wall neighbors."
  [board row col]
  (reduce +
          (for [[r c] [[-1 0][0 -1][0 1][1 0]]]
            (if (= \# (board-cell board (+ row r) (+ col c)))
              0
              1))))

(defn is-numbered-cell?
  [board row col]
  (let [cnt (board-cell board row col)]
    (and (not= cnt \#)(not= cnt \.))))

(s/fdef horizontal-paths
        :args (s/alt
               :start (s/cat :board ::board)
               :row   (s/cat :board ::board :row integer?)
               :recur (s/cat :found (s/coll-of ::path) :board ::board :row integer? :start-col integer? :end-col integer?))
        :ret  (s/coll-of ::path))

(defn horizontal-paths
  "Return all horizontal path on `board` at `row`.
   Only handle borders. Splitting due to 3 neighbors is separate function."
  ([board]
   (mapcat #(horizontal-paths board %)(range 1 (dec (board-height board)))))
  ([board row]
   (horizontal-paths [] board row 1 1))
  ([found board row start-col end-col]
   (if (>= start-col (board-width board))
     found
     ;; we are at a wall, finish current path
     (if (= \# (board-cell board row end-col))
       ;; ignore path that is starts and ends in the same single wall
       ;; ignore path that is a single cell
       (if (or (= start-col end-col)(= start-col (dec end-col)))
         (recur found board row (inc end-col)(inc end-col))
         (let [found2 (cons {:from [row start-col] :to [row (dec end-col)]} found)]
           (recur found2 board row (inc end-col)(inc end-col))))
       (recur found board row start-col (inc end-col))))))


(s/fdef vertical-paths
        :args (s/alt
               :start (s/cat :board ::board)
               :row   (s/cat :board ::board :row integer?)
               :recur (s/cat :found (s/coll-of ::path) :board ::board :row integer? :start-col integer? :end-col integer?))
        :ret  (s/coll-of ::path))

;; warning row and col are not the correct names
(defn vertical-paths
  "Return all vertical path on `board` at `row`.
   Only handle borders. Splitting due to 3 neighbors is separate function."
  ([board]
   (mapcat #(vertical-paths board %)(range 1 (dec (board-width board)))))
  ([board row]
   (vertical-paths [] board row 1 1))
  ([found board row start-col end-col]
   (if (>= start-col (board-height board))
     found
     ;; we are at a wall, finish current path
     (if (= \# (board-cell board end-col row))
       ;; ignore path that is starts and ends in the same single wall
       ;; ignore path that is a single cell
       (if (or (= start-col end-col)(= start-col (dec end-col)))
         (recur found board row (inc end-col)(inc end-col))
         (let [found2 (cons {:from [start-col row] :to [(dec end-col) row]} found)]
           (recur found2 board row (inc end-col)(inc end-col))))
       (recur found board row start-col (inc end-col))))))


(s/fdef split-horizontal-path-at
        :args (s/cat :board ::board :path ::path)
        :ret  (s/coll-of ::path))

(defn split-horizontal-path-at
  [board path]
  (assert (= (first (:from path))
             (first (:to   path)))
          "Must be on same row!")
  (filter #(> (first %) 2)
        (map
         #(list (+
                 ;; break if many neighbors or cell contain a number
                 (if (is-numbered-cell? board (first (:from path)) %) 99 0)
                 (count-neighbors board (first (:from path)) %))
                (first (:from path))
                %)
         (range (second (:from path))(inc (second (:to path)))))))


(s/fdef split-vertical-path-at
        :args (s/cat :board ::board :path ::path)
        :ret  (s/coll-of ::path))


(defn split-vertical-path-at
  [board path]
  (assert (= (second (:from path))
             (second (:to   path)))
          "Must be on same column!")
  ;; >2 since split if more than 2 neighbors or numbered cell
  (filter #(> (first %) 2)
        (map
         #(list (+
                 ;; break if many neighbors or if this cell contain a number
                 (if (is-numbered-cell? board % (second (:from path))) 99 0)
                 (count-neighbors board % (second (:from path))))
                %
                (second (:from path)))
         (range (first (:from path))(inc (first (:to path)))))))


(defn split-horizontal-path-2
  [acc path split-at]
  (let [{:keys [from to]} path
        [from_row from_col] from
        [to_row to_col] to
        [_ row col] split-at]
    (assert (and
             (= row from_row)
             (= from_row to_row))
            "All row should be the same!")
    (assert (and
             (>= col from_col)
             (<= col to_col))
            "Split point must be within path!")
    (if (= from_col col)
      ;; ignore split at head, (and tail) since no purpose
      [acc path]
      [(cons {:from from  :to [row col]} acc)
       {:from [row col] :to to}])))


(defn split-horizontal-path
  ([path split-ats]
   (split-horizontal-path [] path split-ats))
  ([acc path split-ats]
   (if (empty? split-ats)
     (if (or (= (first acc) path) (= (:from path)(:to path)))
       acc
       (cons path acc))
     (let [[acc2 new-path] (split-horizontal-path-2 acc path (first split-ats))]
       (recur acc2 new-path (rest split-ats))))))



(defn split-vertical-path-2
  [acc path split-at]
  (let [{:keys [from to]} path
        [from_row from_col] from
        [to_row to_col] to
        [_ row col] split-at]
    (assert (and
             (= col from_col)
             (= from_col to_col))
            "All col should be the same!")
    (assert (and
             (>= row from_row)
             (<= row to_row))
            "Split point must be within path!")
    (if (= from_row row)
      ;; ignore split at head, (and tail) since no purpose
      [acc path]
      [(cons {:from from  :to [row col]} acc)
       {:from [row col] :to to}])))



(defn split-vertical-path
  ([path split-ats]
   (split-vertical-path [] path split-ats))
  ([acc path split-ats]
   (if (empty? split-ats)
     (if (or (= (first acc) path) (= (:from path)(:to path)))
       acc
       (cons path acc))
     (let [[acc2 new-path] (split-vertical-path-2 acc path (first split-ats))]
       (recur acc2 new-path (rest split-ats))))))


(defn assoc-duplicates
  "The map `m` contains `k` to several `v`.
   Add the new value `v` to `m`."
  [m [k v]]
  (assoc m k (conj (or (get m k) []) v)))

(defn into-assoc-duplicates
  "The map `m` contains `k` to several `v`.
   Add all value [k v] in `kvs` to `m`."
  [m kvs]
  (reduce assoc-duplicates m kvs))

(defn into-assoc-duplicates-reverse
  "The map `m` contains `k` to several `v`.
   Add all value [v k] in `kvs` to `m`."
  [m kvs]
  (reduce (fn [m [k v]] (assoc-duplicates m [(:to v) v])) m kvs))


(defn horizontal-splitted-paths
  [all board]
  (let [paths (horizontal-paths board)
        splitted-paths-one-direction
        (mapcat #(split-horizontal-path % (split-horizontal-path-at board %)) paths)
        kvs (map #(vec (list (:from %) %)) splitted-paths-one-direction)
        all2 (into-assoc-duplicates all kvs)
        all3 (into-assoc-duplicates-reverse all2 kvs)]
    all3))

(defn vertical-splitted-paths
  [all board]
  (let [paths (vertical-paths board)
        splitted-paths-one-direction
        (mapcat #(split-vertical-path % (split-vertical-path-at board %)) paths)
        kvs (map #(vec (list (:from %) %)) splitted-paths-one-direction)
        all2 (into-assoc-duplicates all kvs)
        all3 (into-assoc-duplicates-reverse all2 kvs)]
    all3))


(s/fdef all-paths
        :args (s/cat :board ::board)
        :ret  (s/map-of ::from (s/coll-of ::path)))

(defn all-paths
  "Get all path from board, split them, reverse them, and add to big map."
  [board]
  (let [dir1 (vertical-splitted-paths {} board)
        dir1b (horizontal-splitted-paths dir1 board)]
    dir1b))

;; Not checking :ret, why?
;;
;; (horizontal-paths (board-make sample1))
;; (({:from [1 1], :to [1 9]}) [] ({:from [3 1], :to [3 9]}))
;; adventofcode1.twentyfour> (s/conform ::path (({:from [1 1], :to [1 9]}) [] ({:from [3 1], :to [3 9]})))
;; ArityException Wrong number of args (0) passed to: PersistentArrayMap  clojure.lang.AFn.throwArity (AFn.java:429)
;; adventofcode1.twentyfour> (s/conform (s/coll-of ::path) '(({:from [1 1], :to [1 9]}) [] ({:from [3 1], :to [3 9]})))
;; :clojure.spec/invalid
;; adventofcode1.twentyfour>

(clojure.spec.test/instrument)

(def puzzle-input-a
  "#######################################################################################################################################################################################))
#.....................#.....#.#.......#.......#...#.....#.#...#.........#...........#...#.........#...#...#...#...#.........#.#.....#.........#.#.#.....#.....#.....#.#.#.............#
#.#.###.#.###.#.###.#.#.###.#.###.#########.#.#####.#####.#.###.#.#.#.#.###.#.###.#.#.#.#.###.#.#.#.#.###.#.#.#.#.###.#.#.#.#.#.#.#.#.###.#.#.#.#.#.#####.###.#.#.#.#.#.###.#.#.#.#.#.#
#.........#.......#...#.....#.#.#.#.......#.#.....#...#.....#.....#.....#...............#.#.#...#...#.#.....#.......#.#...#.....#.......#...#.#.#...#2#...#.................#...#.....#
#.###.#.###.#.#.#.#.#.#.#.#.#.#.#.#####.#.#.#.###.#.###.#.#.#.###.#.#.#.###.#.#.###.#.#####.#.#.#####.#.#.#.#######.#.#####.###.###.###.#.#.#.#.#.#.#.#.###.###.###.###.#.#.#.#.#######
#.#....1#.....#...#.......#...#.#.#.....#.....#.....................#.#...........#...#.....#.....#.....#.......#.....#.#.......#...........#...#.#...#...#...............#.#.#.#.....#
#.#.#######.#.#.#.#.#############.#.###.###.###.#.#.#########.#.###.#.#.#.#.#.#.#.#####.#.#.#.###.#.#.#.#.#.#.#.#.#.###.#########.#.#.#############.#######.#.#.#.###.###.###.#####.###
#.#.#...#.........#.....#.........#.....#.#.#...#...........#...#.........#...#.#.....#.#...............#...#.....#.#.#.............#.....#...#.....#...........#.#.#.....#...#.......#
#.###.#.#.#.#.#######.#.#.#.#.###.###.#.#.#.#.#.#.#.#####.###.#.#.#.#.#.#######.#.###.#.#.#.###.#.###.#.#.###.#.#.#.#.#.#.#.#.#.#######.#.###.#.###.#.###.#.#.###.#.###.#.#.#.#.#.#.###
#...#.#.........#.#...#.....#...#.......#.#.#.......#.#.#...#.........#.....#.#...#.#.#...#...#...#.....#.........#.#.....#.......#.....#.......#.#...#...#.....#.....#.......#.#.#...#
#.#.#.#.#.#.###.#.#.#.#.###.#####.#####.#######.#.###.#.#.#.#####.#.#####.###.#.#.#.#.#.#.###.#.#.#.#####.#.#####.#.###.#####.#.#.#.###.###.###.#.#.#.#####.#.#.#####.#.#######.#.#.###
#.....#.#.....#.........#.#...#.......#...#.......#.........#...#.#.#.#.......#...#...............#.#...................#.#.#.......#.#.........#....0#...#.#.......#.#.#.#...#.....#.#
#######.#.#.#.#######.###.#.###.#.#.#.#.#.#.#.#.###.###.#####.#.#.###.#.#.###.#####.#.#.#.#.#####.#.###.###.#.#.#######.#.#.#.#######.#.#.#.#.#.#.#.#.#.###.#.###.#.###.#.#####.#.#.#.#
#.........#...#...#.....#.........#.......#...#...........#.#.#.#...#...#.#...#.#...#.#.#.......#.......#...#.....#.....#.#...#.#.#.#.........#...............#...#.#...........#.....#
#.#.#.#.#.#####.###.#####.#.#######.#.#.#.#.#.#.#.#####.###.#.#.#.#.#.###.#.#.#.#.#.#.#.#.#.#.#.###.#.###.#.###.#.#.#.#.#.###.#.#.#.#.#.#.#.#.#.#####.###.###.#.#######.#.###.#.#######
#.#...#...#...#.....#.#.#.#...#.................#...#...#.#...#...#...#.#.#.#.....#.#.#.....#.#...#.#.......#.#.#...#...#.#...#.....#...#.#...#...#.....#.....#...#.....#.....#.#...#.#
#.###.###.###.###.#.#.#.#.#.#.#.#.#.###.#.###.#####.#.###.#.###.###.#.#.#.#.#.###.#.###.#####.###.###.#.#.#.#.#.#.#.#.#.#.#.###.#.#.#.###.#####.#.###########.#.#.#.###.#.#######.#.#.#
#..3#.#.......#...#.............#.....#.....#...#...#.#.....#.......#.....#...#.....#.#.#.........#...#.#.........#.....#...............#.........#...#...........#.#.#...#7#.#.....#.#
###.#.#.###.#.#.###.#############.###.#.#######.#.#####.#######.#####.#.###.#.#.#.#.#.#.#.#.#.#.#.###.#.#########.#.#.#.#.#.#.###.#.###.#####.#####.###.#.###.#.#.#.#.#.#.#.#.#.###.#.#
#.#...#.....#.#.#.#.#.#...........................#.......#...#.#.....#.#...#...#.#...#.........#...........#.#.....#.....#...#...#.#.......#.#.#...#...#.#.......#.......#...#.....#.#
#.#########.#.#.#.#.#.#.#.#####.###.#######.#.#####.#.#.#.#.###.#.#.#.#.#####.#######.#.#####.#.#.#.#####.#.#.#.###.#####.#.#######.#######.###.###.#.#.#.###.###.#.#########.#.#.###.#
#.#.......#...#.....#.#.......#.#...#.....#...........#.#...........#...#.....#.........#.......#...#.........#.#.....#.#...#.............#.............#...........#.....#...#.#.#.#.#
#.#.###.#.#.#.#.#.###.###.#####.#.#.#.#.#.###.#.#####.#.#.#.#.###.#####.#######.#.#.#.#.#.#.#.#.#.#.###.#.###.#.#.#.#.#.#.###.#.###.#.###.#######.###.#.#.#.#.###.###.#.#.#.#.#.#.#.###
#.#.......................#.......#.......#.#.#.......#...........#...#...#.....#.#.#...#...#...#.......#.......#.#.#...#.....#.....#...#...#.....#.#...#...#...........#.....#...#...#
#.#.###.#####.#.###.#.#.###.#.#.#.#######.#.#####.#.#.###.#.#######.#.#.#.#.#.#.###.#.#.#.#.#.#.#.###.#.###.#.###.###.#######.#.###.###.#.#.#.###.#.###.###.###.#.#.#.#.#.#.#.#.###.#.#
#...#.....#.........#...#.#.....#...#...#.#.#...#.....#.#.#...#.......#.....#.......#.....#.....#.#.....#...#.#.#.......#...#.#...#.........#...#...........#...#...#...#.....#.#.#.#.#
#########.###.#####.#.#.#.#.#.###.#.#.###.#.#.#.#.###.#.#.#.#.#.#.###.#.###.#.#####.#######.#.###.#.###.#.#.###.#.#.#####.#######.#.###.#.###.###.#.###.#.#.#.###.#.#######.#.###.#.#.#
#.#...#...#...#.....#...#.#6......#.#.....#.....#.....#.#...#...#...#.#.....#...#...#...#...#.............#.#...#.......#.#...#...........#.#..5#...#.#.....#...#.....#...#...#.......#
#.###.#.#.###.###.#.#.#.#.#####.#.#.#####.#.#.###.#.#.#.#.#.#.###.#.#.###.#.#########.###.###.#######.#.#.#.#.#.#.#####.#.#.#.#.#.###.#.###.#####.#.#.#.#.#.#.#.###.#.###.#.###.#.#.#.#
#.#...#...#...#.........#...#.................#.....#.....#...............#.....#.#...#.....#.......#...#.....#.#.#.......#.............#.#.#.#...#.#...#.#.#...#.....#.......#.......#
#.###.#.###.###.###.#.#.#.#.#.###.###.#.#.###.###.###############.#####.#.#######.#.#.#.###########.###.#.#.#.#.#.#.#.###.#.#####.#######.###.#.#.#.#.###.#.#.#.#.###.###.###.#.#.###.#
#...................#...........#.#.#.........#.....#...#...#...#.#.....#.........#...#.#.......#...#...............#.#.................#.....#.....#.#.....#...#...#.#.....#...#.....#
#.#.#.#.#.###.###.#.###.#.#####.#.#.#########.#.#.###.#.###.#####.#.#.#.#.#.#.#######.#####.#.#.#.###.#.#.###.#####.#.#.#.###.###.###.###.#######.#.#.#.###.#.#.#.#.#.#.#.###.#.#.###.#
#.#...#.#.#.#.......#.#...#.#.....#.......#.#.#.....#.#.#.......#...#.#.....#.#...#...#.........#.......#.....#.#...#...#.......#.#...#...#.#.........#...#.........#.........#.#.#...#
###.#####.#.#.###.#.#.#.###.#.#.#.#.#.#.#.#.#.#####.###.#.###.#.#.#.#.#.#.#.#######.#.###.#.#####.###.#####.###.#.#.###.#.#.#.###.#.###.#.#.#.#.#####.#.#####.###.#.#.#.###.#####.#.#.#
#...........#...................#.....#.....#...............#...#.#.....#.......#...#...#...#...#.......#...#...#.....#...#...#...#.....#4#...#...#...#.....#.............#.#...#.....#
#######################################################################################################################################################################################")
