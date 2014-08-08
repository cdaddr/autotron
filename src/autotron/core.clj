(ns autotron.core
  (:require [clojure [set :as cset]]
            [clojure [string :as s]]
            [clojure.java [io :as io]]))

(def DUMMY :_)

(def LOG? false)
(defmacro log [& body]
  (when LOG?
    `(println ~@body)))

(defn make-deck [ncards cards-and-counts]
  (let [deck (reduce-kv #(into %1 (repeat %3 %2))
                        [] cards-and-counts)
        leftover (- ncards (count deck))]
    (into deck (repeat leftover DUMMY))))


(def DECK (make-deck 60 {:urza-mine 4
                         :urza-powerplant 4
                         :urza-tower 4
                         :island 9
                         :nonblue-land 2
                         :expedition-map 4
                         :thirst-for-knowledge 4
                         :remand 4
                         :condescend 4
                         :talisman 3}))

(def blueland? #{:island})
(def bluemana? #{:island :talisman})
(def otherland? #{:urza-mine :urza-powerplant :urza-tower :nonblue-land})
(def urzaland? #{:urza-mine :urza-powerplant :urza-tower})
(defn land? [x] (or (blueland? x) (otherland? x)))

(defn in-hand? [state card]
  (some #{card} (:hand state)))

(defn can-afford? [state cost]
  (let [state (update-in state [:mana :blue] (fnil - 0) (:blue cost 0))]
    (when (>= (-> state :mana :blue) 0)
      (>= (+ (-> state :mana (:blue 0))
             (-> state :mana (:colorless 0)))
          (:colorless cost 0)))))

(defn on-battlefield? [state card]
  (some #{card} (:battlefield state)))


(defn tron? [state]
  (every? #(on-battlefield? state %) [:urza-tower :urza-powerplant :urza-mine]))

(defn remove-first [pred [x & xs :as lst]]
  (when (seq lst)
   (if (pred x)
     xs
     (cons x (remove-first pred xs)))))

(defn update-mana [{:keys [battlefield] :as state}]
  (let [blue (count (filter bluemana? battlefield))
        colorless (count (filter otherland? battlefield))]
    (update-in state [:mana] assoc :blue blue :colorless colorless)))

(defn hand-to-bf [{:keys [hand battlefield] :as state} spell]
  (assoc state
    :battlefield (conj battlefield spell)
    :hand (remove-first #{spell} hand)))

(defn to-gy [{:keys [battlefield graveyard] :as state} card]
  (assoc state
    :graveyard (conj graveyard card)
    :battlefield (remove-first #{card} battlefield)))

(defn hand-to-gy [{:keys [hand graveyard] :as state} card]
  (assoc state
    :graveyard (conj graveyard card)
    :hand (remove-first #{card} hand)))

(defn tutor [{:keys [deck hand] :as state} card]
  (assoc state
    :deck (shuffle (remove-first #{card} deck))
    :hand (conj hand card)))

(defn pay [state cost]
  (let [state (-> state
                  (update-in [:mana :blue] - (:blue cost 0))
                  (update-in [:mana :colorless] - (:colorless cost 0))) ]
    (if (neg? (-> state :mana :colorless))
      (-> state
          (update-in [:mana :blue] + (-> state :mana :colorless))
          (assoc-in [:mana :colorless] 0))
      state)))

(defn shuffle-deck [state]
  (update-in state [:deck] shuffle))

(defn draw [{:keys [deck hand] :as state} ncards]
  (assoc state
    :hand (into hand (take ncards deck))
    :deck (drop ncards deck)))

(defn discard-priority [x]
  (cond
   (= x DUMMY) 0
   (land? x) 50
   (urzaland? x) 100
   :else 25))

(defn discard [{:keys [hand] :as state} ncards]
  (let [sorted-hand (sort-by discard-priority hand)
        new-hand (drop ncards sorted-hand)]
    (log "  (Discarding" (take ncards sorted-hand) ")" )
    (assoc state :hand new-hand)))

(defn make-state []
  {:deck (shuffle DECK) :hand [] :battlefield [] :mana {} :played-land? false :turn 0})

(defn init-game [state]
  (-> state
      shuffle-deck
      (draw 7)
      update-mana))

(defn scry [state]
  state)

(defn step [state]
  (-> state
      (draw 1)
      update-mana
      (assoc :played-land? false)
      (update-in [:turn] inc)))

(defn try-to-do [play-fn state card cost & and-then]
  #_(prn "ttd" card cost (in-hand? state card) (can-afford? state cost))
  (if (and (in-hand? state card)
           (can-afford? state cost))
    (do
      (log "  Playing" card)
      (let [state (-> state
                      (pay cost)
                      (play-fn card))]
        (reduce #(%2 %1) state and-then)))
    state))

(defn try-to-cast [state card cost & and-then]
  (apply try-to-do hand-to-gy state card cost and-then))

(defn try-to-play [state card cost & and-then]
  (apply try-to-do hand-to-bf state card cost and-then))

(defn unplayed-urzalands [state]
  (let [played (set (filter urzaland? (:battlefield state)))
        unplayed (cset/difference urzaland? played)]
    unplayed))

(defn use-map [state]
  (let [unplayed (unplayed-urzalands state)]
    (if (and (on-battlefield? state :expedition-map)
             (can-afford? state {:colorless 2})
             (seq unplayed))
      (let [card (first unplayed)]
        (log "  Using map to find" card)
        (-> state
            (pay {:colorless 2})
            (to-gy :expedition-map)
            (tutor card)))
      state)))

(defn play-map [state]
  (try-to-play state :expedition-map {:colorless 1}))

(defn play-land [state land]
  (-> state
      (try-to-play land {})
      (assoc :played-land? true)
      (update-in [:mana (if (blueland? land) :blue :colorless)] inc)))

(defn play-new-urza-land [state]
  (let [unplayed (unplayed-urzalands state)
        unplayed-in-hand (filter #(in-hand? state %) unplayed)]
    (if (and (not (:played-land? state))
             (seq unplayed-in-hand))
      (play-land state (first unplayed-in-hand))
      state)))

(defn play-blue-land [state]
  (let [land (first (filter blueland? (:hand state)))]
    (if (and (not (:played-land? state))
             land)
      (play-land state land)
      state)))

(defn play-other-land [state]
  (let [land (first (filter otherland? (:hand state)))]
    (if (and (not (:played-land? state))
             land)
      (play-land state land)
      state)))

(defn play-talisman [state]
  (try-to-cast state :talisman {:colorless 2} #(update-in % [:mana :blue] (fnil inc 1))))

(defn cast-remand [state]
  (try-to-cast state :remand {:blue 1 :colorless 1}
               #(draw % 1)))
(defn cast-condescend [state]
  (try-to-cast state :condescend {:blue 1 :colorless 1}))
(defn cast-thirst-for-knowledge [state]
  (try-to-cast state :thirst-for-knowledge {:blue 1 :colorless 2}
               #(draw % 3) #(discard % 2)))

(def STRATEGY
  [use-map
   play-new-urza-land
   play-blue-land
   play-other-land
   use-map
   play-talisman
   play-map
   cast-remand
   cast-condescend
   cast-thirst-for-knowledge])

(def STRATEGY
  [play-new-urza-land])

(defn try-strategy [state]
  (loop [state state]
    (let [new-state (reduce #(%2 %1) state STRATEGY)]
      (if (= state new-state)
        state
        (recur new-state)))))

(defn simulate-game []
  (loop [state (init-game (make-state))]
    (let [new-state (step state)]
      (log "Turn" (:turn new-state) "| HAND" (:hand new-state) "| MANA" (:mana new-state) "| BF" (:battlefield new-state))
      (let [new-state (try-strategy new-state)]
        (if (tron? new-state)
          (do 
            (log "TRON ON TURN" (:turn new-state))
            new-state)
          (recur new-state))))))

(defn simulate-n [n]
  (reduce (fn [acc i]
            (when (zero? (mod i (/ n 10)))
              (println "iter" i))
            (let [state (simulate-game)]
              (update-in acc [:turn (:turn state)] (fnil inc 0))))
          {}
          (range 1 (inc n))))

(defn turn-report [data]
  (with-open [fh (io/writer "turns.csv")]
    (let [xs (:turn data)
          n (apply max (map first xs))]
      (prn xs n)
      (doseq [x (range 1 (inc n))]
        (.write fh (str x "," (get xs x) "\n"))))))
