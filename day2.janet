(def max-values @{"red" 12 "green" 13 "blue" 14})

(def grammar
'{
:color  (choice "red" "green" "blue")
:dice   (group (sequence (number :d+) (capture :color) (any ",")))
:tirada (group (sequence (some :dice) ";"))
:game   (sequence "Game" (number :d+) ":")
:main   (sequence :game (some :tirada) -1)})

(defn contar-una-tirada [tiradas]
    (def tally @{"red" 0 "green" 0 "blue" 0})
    (each [cant color] tiradas
        (def old (get tally color))
        (set (tally color) (+ old cant)))
    tally)

(defn contar-juego [juego]
    (def [_ & tiradas] juego)
    (map contar-una-tirada tiradas))

(defn tirada-valida [tally]
    (and
        (<= (get tally "red") (get max-values "red"))
        (<= (get tally "green") (get max-values "green"))
        (<= (get tally "blue") (get max-values "blue"))))

(defn tiradas-validas [tallies]
    (all tirada-valida tallies))

(defn clean-line [line]
    (def l (string/replace-all " " "" line))
    (string l ";"))

(defn clean-lines [lines]
    (map clean-line lines))

(defn juego-valido [game]
    (def [game-number & tiradas] game)
    (def tallies (contar-una-tirada tiradas))
    (tiradas-validas tallies))

(defn numero-de-juego [game]
    (get game 0))

(defn parse-line-into-game [line]
    (peg/match grammar line))

# Part 2
(defn only-color [color tallies]
    (map (fn [t] (get t color)) tallies))

(defn min-for-color [color tallies]
    (def oc (only-color color tallies))
    (apply max (only-color color tallies)))

(defn power [tallies]
    (def min-red (min-for-color "red" tallies))
    (def min-green (min-for-color "green" tallies))
    (def min-blue (min-for-color "blue" tallies))
    (* min-red min-green min-blue))

(defn file-to-lines [fl] (string/split "\n" (file/read fl :all)))
(with [fl (file/open "day2.in")]
    (def lines (file-to-lines fl))
    (def lines (clean-lines lines))
    (def parsed-lines (map parse-line-into-game lines))
    # 1st star
    (def juegos-contados (map contar-juego parsed-lines))
    (pp (get juegos-contados 0))
    #(def filtradas (filter juego-valido juegos-contados))
    #(def numeros (map numero-de-juego filtradas))
    #(print (sum numeros))
    # 2nd star
    (def juego-1 (get juegos-contados 3))
    (def powers (map power juegos-contados))
    (pp (sum powers)))



