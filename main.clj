;;A simplified breakdown of the steps in the current error evaluation process in the Clojush and Propeller genetic programming software packages and the implications of possible alternatives


(defn errorfunction [input, adjustment]
  "base function - return error for a given case-behavior pairing"
  "problem: return the input squared"
  (- (* input input) adjustment))


(defn generateBehavior [candidate case] ((get candidate :strategy) case))
(defn generateBehaviors [candidate cases] (map (get candidate :strategy) cases))

(defn testify [candidate case] (errorfunction case (generateBehavior candidate case)))

(defn fullTestimony [candidate cases]  (let [errors (map #(testify candidate %) cases )] (assoc candidate :errors errors :total-error (apply + errors))))

(defn groupOfFullTestimonies [cases testifiers] (map #(fullTestimony % cases) testifiers))


(defn groupTestimony [case testifiers] (map #(testify % case) testifiers))

;(defn groupTestimony [case testifiers] (map #(testify % case) testifiers))

(defn fullGroupTestimony [cases testifiers] (map #(groupTestimony % testifiers) cases))


;(apply mapv vector (groupOfFullTestimonies cases population))
(defn pseudoShuffle [cases] 
(into (sorted-map) (map #(hash-map (keyword (str %3) ) {:case %2 :index %1}) (range (count cases)) cases (shuffle (range (count cases))))))

;(defn getShuffledCases [briefcase]  (map val briefcase))
;(defn getShuffledCases2 [briefcase]  (map #(keyword (str %)) (range (count briefcase))))


(defn getShuffledCase [briefcase caseNumber] ((keyword (str caseNumber))briefcase))

;(map #(%1 %2) (getShuffledCases (pseudoShuffle cases)) (pseudoShuffle cases))
(defn partialPrep [cases testifiers]
  (do
    (map #(assoc % :errors (vector (take (count cases) (repeat "bupkis")))) testifiers)
    (pseudoShuffle cases)
    ))

(defn partialPrep2 [cases testifiers]
    (map #(assoc % :errors (vec (repeat (count cases) "bupkis"))) testifiers)
    )


(defn doubleBlindPartialTestimony [shuffledCaseIndex shuffledCases testifiers]
  (groupTestimony (:case ((keyword shuffledCaseIndex) shuffledCases)) testifiers))

;(doubleBlindPartialTestimony (first (getShuffledCases2 cases)) (partialPrep cases population) population)

(defn currentStrategy [cases testifiers] (groupOfFullTestimonies cases testifiers))

;(currentStrategy cases population)

(defn recordedPartialTestimony
    [shuffledCases caseNumber testifiers]
  (let [index (:index (getShuffledCase shuffledCases caseNumber))
        case (:case (getShuffledCase shuffledCases caseNumber))
        caseErrors (groupTestimony case testifiers)]
        (map #(assoc-in %1 [:errors index] %2) testifiers caseErrors)))

(defn personifier [intensityFactor]  (hash-map :strategy #(* intensityFactor %) :errors [] :pa-error 0 :id intensityFactor :total-error -1))

(defn populate [popNum upperLimit](repeatedly popNum #(personifier  (rand upperLimit))))

(defn makeInputs [numInputs, upperLimit]
  (take numInputs (repeatedly #(rand upperLimit))))

(def person {:strategy #(* % %) :errors [] :pa-error 0 :id 1 :total-error -1})

(def cases (makeInputs 4 20))
(def cases2 (makeInputs 4 20))

(def population (populate 4 20))
(def population2 (populate 4 10))



(recordedPartialTestimony (pseudoShuffle cases) 1 (partialPrep2 cases population))

;(partialPrep2 cases population)
;population
;(getShuffledCase (pseudoShuffle cases) 1)
;(pseudoShuffle cases)

;(fullTestimony2 (personifier 20) cases)

;(assoc (first population) :errors (repeat (count cases) 0))



;(defn fullTestimony2 [candidate cases] (map errorfunction cases (generateBehaviors candidate cases)))
;(defn groupOfFullTestimonies2 [cases testifiers] (map #(fullTestimony2 % cases) testifiers))