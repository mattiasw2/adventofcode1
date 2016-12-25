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

;; (def max-width 10)
;; (def max-height 5)


(def sample1
  "###########
  #0.1.....2#
  #.#######.#
  #4.......3#
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

(defn count-neighbors
  "Count non-wall neighbors."
  [board row col]
  (reduce +
          (for [r (range -1 2)]
            (for [c (range -1 2)]
              (if (and (zero? r)(zero? c))
                0
                (if (= \# (board-cell board (+ row r) (+ col c)))
                  0
                  1))))))

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
         (let [found2 (cons {:from [row start-col] :to [row (dec end-col)]} found)]
           (recur found2 board row (inc end-col)(inc end-col))))
       (recur found board row start-col (inc end-col))))))


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
  "#######################################################################################################################################################################################
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
