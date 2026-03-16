(use-modules (ice-9 hash-table))
(read-set! keywords 'prefix)

;; TODO:
;; - make-i - create an instrument abstraction, provided a list of allowed keyword parameters and default values in i's order

;;;

(define (render-i inst)
  (string-join (cons "i" inst) " "))

(def i1 (make-i 1 :gain 0.6 :freq 440))
(i1 :freq 220 :gain 8)

(define (choose-random . notes)
  (list-ref notes (random (length notes))))

(def b (make-i "Bass" :freq 440 :gain 0.2))
(define (bb n) (b :freq (mtof (- n 24))))

(def bd (make-i "BD" :freq 440 :gain 0.9))
(def sn (make-i "CHH" :freq 440 :gain 0.9))

(cs-render-score-loop
  0 (beat->sec 1) (beat->sec 16)
  (S (bb D-5) (bb F-4) (bb G-4) (λ(attr) (bb (choose-random A4 C5 D5)))))

(cs-render-score-loop
  (beat->sec 4) (beat->sec 1) (beat->sec 16)
  (S #f bd #f bd))

(cs-render-score-loop
  (beat->sec 8) (beat->sec 2) (beat->sec 16)
  (S sn sn 
     sn (S sn sn #f #f)))
