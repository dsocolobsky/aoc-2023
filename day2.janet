(def max-values @{"red" 12 "green" 13 "blue" 14})

(def grammar
'{
:color  (choice "red" "green" "blue")
:dice   (group (sequence (number :d+) (capture :color) (any ",")))
:tirada (group (sequence (some :dice) ";"))
:game   (sequence "Game" (number :d+) ":")
:main   (sequence :game (some :tirada) -1)})

(defn contar-tirada [tirada]
    (def tally @{"red" 0 "green" 0 "blue" 0})
    (each [cant color] tirada
        (def old (get tally color))
        (set (tally color) (+ old cant)))
    tally)

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
    (def tallies (map contar-tirada tiradas))
    (tiradas-validas tallies))

(defn numero-de-juego [game]
    (get game 0))

(defn parse-line-into-game [line]
    (peg/match grammar line))

(defn file-to-lines [fl] (string/split "\n" (file/read fl :all)))
(with [fl (file/open "day2.in")]
    (def lines (file-to-lines fl))
    (def lines (clean-lines lines))
    (def parsed-lines (map parse-line-into-game lines))
    (def filtradas (filter juego-valido parsed-lines))
    (def numeros (map numero-de-juego filtradas))
    (print (sum numeros)))

