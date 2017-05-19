(ns p-p-p-pokerface)

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)

(defn rank [card]
  (let [[fst _] card
        rank-values {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (rank-values fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn rank-freqs [hand]
  (frequencies (map rank hand)))

(defn suit-freqs [hand]
  (frequencies (map suit hand)))

(defn pair? [hand]
  (let [two? (fn [x] (= x 2))
        freq-vals (vals (rank-freqs hand))
        pairs (filter two? freq-vals)]
    (= (count pairs) 1)))

(defn three-of-a-kind? [hand]
  (let [three? (fn [x] (= x 3))
        freq-vals (vals (rank-freqs hand))
        threes (filter three? freq-vals)]
    (= (count threes) 1)))

(defn four-of-a-kind? [hand]
  (let [freq-vals (vals (rank-freqs hand))
        max-freq (apply max freq-vals)]
    (= max-freq 4)))

(defn flush? [hand]
  (let [freq-vals (vals (suit-freqs hand))]
    (= (count freq-vals) 1)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [two? (fn [x] (= x 2))
        freq-vals (vals (rank-freqs hand))
        pairs (filter two? freq-vals)]
    (or (= (count pairs) 2)
        (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        first-rank (first sorted-ranks)
        second-last-rank (nth sorted-ranks 3)
        final-ranks (if (or (= first-rank 1) (= second-last-rank 13))
                      sorted-ranks
                      (sort (replace {14 1} sorted-ranks)))
        min (apply min final-ranks)
        comparison-ranks (range min (+ min 5))]
    (= final-ranks comparison-ranks)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

; All hands have a high card.
(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        checker-match? (fn [[check? _]] (check? hand))
        hand-values (map second (filter checker-match? checkers))]
    (apply max hand-values)))

