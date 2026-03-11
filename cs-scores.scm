(add-to-load-path ".")
(use-modules (schsq))

(define (choose-random . notes)
  (list-ref notes (random (length notes))))

(define (n notenum)
  (list "Bass" (mtof (- notenum 24)) 0.1))
(define (bd)
  (list "BD" 440 0.9))

(define (my-score)
  (cs-render-score-loop
    0 1 20
    (S (n D-4) (n F-4) (n G-4) (λ(attr) (n (choose-random A4 C5 D5)))))
  (cs-render-score-loop
    4 1 20
    (S #f (bd) #f (bd))))

(with-output-to-file
  (list-ref (command-line) 2)
  my-score)
