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

;;([1 1] [1 3] [1 5] [1 9] [1 5] [3 5] [3 3] [3 5] [5 5] [5 6] [5 9] [3 9] [3 7])
;;([1 1] [1 3] [1 5] [1 9] [1 5] [3 5] [3 3] [3 1] [5 1] [5 5] [5 6] [5 9] [3 9] [3 7])
;;([1 1] [1 3] [1 1] [1 3] [1 5] [1 9] [3 9] [3 7] [3 9] [5 9] [5 6] [5 5] [3 5] [3 3])
;;([1 1] [1 3] [1 1] [1 3] [1 5] [1 9] [1 5] [3 5] [3 3] [3 5] [5 5] [5 6] [5 9] [3 9] [3 7])


(def sample3
  "###########
  #0.1.....2#
  #.###.###.#
  #..4..#6..#
  #.###.###.#
  #.....3...#
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
(s/def ::board (s/coll-of ::board-row :min-count 1)) ;; :count max-height)

(s/def ::from (s/tuple integer? integer?))
(s/def ::to   (s/tuple integer? integer?))
(s/def ::path  (s/keys :req-un [::from ::to]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; board parsing

(s/fdef board-width
        :args (s/cat :board ::board)
        :ret  int?)

(defn board-width
  [b]
  (count (nth b 0)))


(defn foo
  []
  (stest/instrument `board-width)
  (stest/check `board-cell))


(s/fdef board-height
        :args (s/cat :board ::board)
        :ret  int?)

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

(s/def ::puzzle-cell    #{\# \. \0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
(s/def ::digit          #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
(s/def ::numbered-cell  (s/keys :req-un [::digit ::to]))
(s/def ::numbered-cells (s/coll-of ::numbered-cell))

(s/fdef numbered-cells
        :args (s/alt
               :start (s/cat :board ::board)
               :row   (s/cat :board ::board :row integer?)
               :recur (s/cat :found ::numbered-cells :board ::board :row integer? :col integer?))
        :ret  ::numbered-cells)

(defn numbered-cells
  "Return all numbered cells on `board` at `row`."
  ([board]
   (mapcat #(numbered-cells board %)(range 1 (dec (board-height board)))))
  ([board row]
   (numbered-cells [] board row 1))
  ([found board row col]
   (if (>= col (board-width board))
     found
     (if (is-numbered-cell? board row col)
       (let [found2 (cons {:digit (board-cell board row col) :to [row col]} found)]
         (recur found2 board row (inc col)))
       (recur found board row (inc col))))))

(s/def ::dp (s/map-of ::digit (s/tuple integer? integer?)))
(s/fdef digit-pos-map
        :args (s/cat :numcells ::numbered-cells)
        :ret  ::dp)

(defn digit-pos-map
  "Convert the numbered cells into a map with digit lookup.
   Used to find the \0 cells and similar."
  [numcells]
  (into {} (map (fn [{:keys [digit to]}] [digit to]) numcells)))

(s/def ::pd (s/map-of (s/tuple integer? integer?) ::digit))
(s/fdef pos-digit-map
        :args (s/cat :numcells ::numbered-cells)
        :ret  ::pd)

(defn pos-digit-map
  "Convert the numbered cells into a map with pos lookup.
   Used to check if we are in a numbered cell without looking into the board"
  [numcells]
  (into {} (map (fn [{:keys [digit to]}] [to digit]) numcells)))

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

(s/def ::neighbor-count
  (s/cat :count int? :row int? :col int?))

(s/fdef split-horizontal-path-at
        :args (s/cat :board ::board :path ::path)
        :ret  (s/coll-of ::neighbor-count))

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
        :ret  (s/coll-of ::neighbor-count))


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

(s/fdef assoc-duplicates
        :args (s/cat :m map? :kv (s/tuple (constantly true) (constantly true)))
        :ret  map?)

(defn assoc-duplicates
  "The map `m` contains `k` to several `v`.
   Add the new value `v` to `m`.
   Note that duplicates of identical v is also allowed, since vector is used."
  [m [k v]]
  ;; if no duplicate v:s (assoc m k (conj (or (get m k) #{}) v))
  (assoc m k (conj (or (get m k) []) v)))

(s/fdef into-assoc-duplicates
        :args (s/cat :m map? :kvs (s/coll-of (s/tuple (constantly true) (constantly true))))
        :ret  map?)

(defn into-assoc-duplicates
  "The map `m` contains `k` to several `v`.
   Add all value [k v] in `kvs` to `m`."
  [m kvs]
  (reduce assoc-duplicates m kvs))

;; this is not a generic function since uses :to
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

(s/def ::all (s/map-of ::from (s/coll-of ::path)))
(s/fdef all-paths
        :args (s/cat :board ::board)
        :ret  ::all)

(defn all-paths
  "Get all path from board, split them, reverse them, and add to big map."
  [board]
  (let [dir1 (vertical-splitted-paths {} board)
        dir1b (horizontal-splitted-paths dir1 board)]
    dir1b))

;; path = [current & previous]
;; digits = the list of digits we have passed. when all numbers are there, we are finished!
;; 0. Is there a digit at current? if so, add it to digits
;; 1. is digits complete, if so, return path and we are finished
;; 2. find the list of possible continuations from current
;;    a. If none, return :dead-end
;;    b. if just one option, add it to front of path and recur.
;;    c. if several possibilites, try each, one after the other.
;;       keep the non-:dead-end alternatives.
;;       if more than one, return the shortest
;;
;; optimizations:
;; A. When looking for alternatives, only allow 2nd use of arc if we found a digit in between
;; B. Send around an atom with the shortest COMPLETE path sofar, when looking for conituations, skip those that already are too long
;;

(s/def ::breadcrumb  (s/tuple int? int?))
(s/def ::breadcrumbs (s/or :deadend #{:deadend} :success (s/coll-of ::breadcrumb)))
;; ::digits cannot generate well! SOLVED!! set? in spec is a really bad idea if you want to generate.
;; (gen/sample (s/gen ::digits)) => (#{} #{} #{} #{} #{} #{} #{} #{} #{} #{})
;; this works nicely, (into #{} (first (gen/sample (s/gen (s/coll-of ::digit)) 1))) => #{\0 \1 \2 \4 \5 \6 \7 \8 \9}
;; but is not a generator, and also, it is not minimuizable
;; (s/def ::digits      (s/with-gen (s/and set? (s/every ::digit)) #(into #{} (first (gen/sample (s/gen (s/coll-of ::digit)) 1)))))
;; (s/def ::digits      (s/and set? (s/every ::digit)))
(s/def ::digits      (s/coll-of ::digit :into #{} ::max-count 30))
(s/def ::puzzle      (s/keys :req-un [::all ::dp ::pd]))

(s/fdef finished?
        :args (s/cat :breadcrumbs ::breadcrumbs :digits ::digits :puzzle ::puzzle)
        :ret  boolean?)

(defn finished?
  "We are finished if all digits have been found."
  [breadcrumbs digits puzzle]
  (= (count digits)(count (:dp puzzle))))

(s/fdef path->breadcrump-alts
        :args (s/cat  :breadcrumbs ::breadcrumbs :paths (s/coll-of ::path))
        :ret  (s/coll-of ::from))

(defn path->breadcrump-alts
  [breadcrumbs paths]
  (let [current (first breadcrumbs)
        bread-ext
        (map
         (fn [{:keys [from to]}]
           (let [next (if (= current from) to from)]
             [next
              ;; make the visited path be last
              (not (mw.utils/in? breadcrumbs next))]))
         paths)]
    (map first (sort bread-ext))))


(s/fdef possibilites
        :args (s/cat :breadcrumbs ::breadcrumbs :digits ::digits :puzzle ::puzzle)
        :ret  ::breadcrumbs)

(defn possibilities
  "Find all possible next steps from current."
  [breadcrumbs digits puzzle]
  (let [current (first breadcrumbs)
        p (-> puzzle :all (get current))
        breadcrumb (path->breadcrump-alts breadcrumbs p)]
    breadcrumb))




(s/fdef circle?
        :args (s/cat :breadcrumbs ::breadcrumbs :digits ::digits :puzzle ::puzzle)
        :ret  boolean?)

(defn circle?
  "Abort looping by forbidden the same cell to visited more than 2 times."
  [breadcrumbs digits puzzle]
  (let [current (first breadcrumbs)]
    (> (count (filter #(= % current) breadcrumbs))
       2)))

(s/fdef find-breadcrumbs
        :args (s/alt
               :base  (s/cat :puzzle ::puzzle)
               :recur (s/cat :breadcrumbs ::breadcrumbs :digits ::digits :puzzle ::puzzle)))
;; cannot check :ret since we return a tree
;;        :ret  ::breadcrumbs

;; single threaded (map)
;; adventofcode1.twentyfour> (mw.utils/timed "foo" (solve-1 sample3))
;; "Timed: foo solve-1: 13.047683 msecs"
;; nil
;; adventofcode1.twentyfour> (mw.utils/timed "foo" (solve-1 puzzle-input-a))
;; "Timed: foo solve-1: 26944.700927 msecs"
;; nil

(defn find-breadcrumbs
  ([puzzle](find-breadcrumbs [(get (:dp puzzle) \0)] #{} puzzle))
  ([breadcrumbs digits puzzle]
   (let [[current & rest] breadcrumbs
         digit (get (:pd puzzle) current)
         digits2 (if digit (conj digits digit) digits)]
     (if (finished? breadcrumbs digits2 puzzle)
       {:result breadcrumbs}
       (if (circle? breadcrumbs digits2 puzzle)
         :deadend
         (let [possibilities (possibilities breadcrumbs digits2 puzzle)]
          (if (empty? possibilities)
           :deadend
           (let [res (map #(find-breadcrumbs (cons % breadcrumbs) digits2 puzzle) possibilities)]
             res))))))))

(defn print-res
  [res]
  (if (map? res)
    (println (reverse (:result res)))
    (if (= res :deadend)
      nil
      (doseq [x res] (print-res x)))))

(defn count-res
  [res]
  (if (map? res)
    1
    (if (= res :deadend)
      0
      (apply + (map count-res res)))))

(defn solve-1
  [puzzle]
  (let [board (board-make puzzle)
        all (all-paths board)
        ;; _ (println all)
        numcells (numbered-cells board)
        pd (pos-digit-map numcells)
        dp (digit-pos-map numcells)
        res (find-breadcrumbs {:all all :dp dp :pd pd})]
;;    (print-res res)
    (count-res res)))

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
