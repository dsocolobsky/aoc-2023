(def grammar
'{
:number  (* (number :d+) :s+)
:winning (group (some :number))
:owned   (group (some :number))
:main    (* "Card" :s+ (number :d+) ":" :s+ :winning "|" :s+ :owned -1)
})

(defn member [x xs]
	(not (nil? (index-of x xs))))

(defn my-winners [mine winners]
	(filter (fn [x] (member x winners)) mine))

(defn calculate-points [winners]
	(def len (length winners))
	(cond
		(= len 0) 0
		(> len 0) (math/exp2 (- len 1))))
		
(defn process-line [[_ winners mine]]
	(calculate-points (my-winners mine winners)))

(defn add-space-to-end-of-line [line]
	(string line " "))

(defn parse-line [line]
	(peg/match grammar line))

(defn file-to-lines [fl] (string/split "\n" (file/read fl :all)))
(with [fl (file/open "day3.in")]
    (def lines
		(map add-space-to-end-of-line (file-to-lines fl)))
    (def parsed-lines 
		(map parse-line lines))
	(print (sum 
		(map process-line parsed-lines))))
