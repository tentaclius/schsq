(use-modules (ice-9 hash-table))

;; TODO:
;; - make-i - create an instrument abstraction, provided a list of allowed keyword parameters and default values in i's order

;;;

(define (render-i inst)
  (string-join (cons "i" inst) " "))

(define (make-i name . args) ; args is a list of (keyword-name default-value)
  (lambda params
    (let ((param-table (alist->hash-table params)))
      (let loop ((p args) (result (list)))
        (cond
          ((null? p) result)
          (#t (loop (cdr p)
                    (cons
                      (let ((pn (hash-ref param-table (car p))))
                        (:w
                          ))))))))))

(define (choose-random . notes)
  (list-ref notes (random (length notes))))

(define (n notenum)
  (list "Bass" (mtof (- notenum 24)) 0.1))

(define (bd . rest)
  (list "BD" 440 0.9))

(define (sn . rest)
  (list "CHH" 440 0.9))

(cs-render-score-loop
  0 (beat->sec 1) (beat->sec 16)
  (S (n D-4) (n F-4) (n G-4) (λ(attr) (n (choose-random A4 C5 D5)))))

(cs-render-score-loop
  (beat->sec 4) (beat->sec 1) (beat->sec 16)
  (S #f bd #f bd))

(cs-render-score-loop
  (beat->sec 8) (beat->sec 2) (beat->sec 16)
  (S sn sn 
     sn (S sn sn #f #f)))
