(add-to-load-path ".")
(use-modules (schsq))
(read-set! keywords 'prefix)

;(tracker :play
;  #(0    0     0     1     2)
;  A-3    _     _     A-1   _
;  B-3    _     _     _     A-1)

(define (play-evt n tm dur)
  (midi-note-on n #:at tm #:duration dur))

(define (tracker channels . notes)
  (let loop ((nts notes) (tm (+ (now) 200)))
    (if (< (length nts) (vector-length channels))
      (loop notes tm)
      (for-each
        (λ(n) (play-evt n tm (* 1 SEC)))
        (list-head nts (vector-length channels))))
    (schedule (+ (now) (* 1 SEC) -200) loop
              (list (list-tail nts (vector-length channels)) (now+sec 1)))))

;;; Launchpad lightshow
;;; {{{

(midi-init "CL")

(define *pads*
  (vector 
    (vector 36 37 38 39 68 69 70 71)
    (vector 40 41 42 43 72 73 74 75)
    (vector 44 45 46 47 76 77 78 79)
    (vector 48 49 50 51 80 81 82 83)
    (vector 52 53 54 55 84 85 86 87)
    (vector 56 57 58 59 88 89 90 91)
    (vector 60 61 62 63 92 93 94 95)
    (vector 64 65 66 67 96 97 98 99)))

(define (pad x y)
  (vector-ref (vector-ref *pads* (modulo x 8)) y))

(define color 0)
(define l0 (list 
             36 37 38 39 68 69 70 71
             40 41 42 43 72 73 74 75
             44 45 46 47 76 77 78 79
             48 49 50 51 80 81 82 83
             52 53 54 55 84 85 86 87
             56 57 58 59 88 89 90 91
             60 61 62 63 92 93 94 95
             64 65 66 67 96 97 98 99))
(define ll '())

(define (choose-pad)
  (when (= 0 (random 70))
    (set! color (random 128)))
  (when (null? ll)
    (set! color (random 128))
    (set! ll l0))
  (let* ((ind (random (length ll)))
         (elm (list-tail ll ind)))
    (set! ll (append (list-head ll ind) (cdr elm)))
    (car elm)))

(define ii 0)
(define (choose-pad)
  (set! ii (1+ ii))
  (when (= 0 (random 70))
    (set! color (random 128))
    (set! ii (random 128)))
  (when (= 0 (modulo ii 64))
    (set! color (random 128)))
  (pad (- 7 (remainder ii 8))
       (- 7 (modulo (quotient ii 8) 8))))

(define (looper tm)
  (midi-note-on (choose-pad) #:velo (modulo color 128) #:at tm)
  (schedule (+ tm (* 1/6 SEC) -200) looper (list (+ tm (* 1/6 SEC)))))

(schedule (now) looper (list (now)))

;;; }}}


(midi-init "CL")
(metro-start)

(metro-stop)

;;;

(define play-midi
  (λ(el attr)
    (let ((at (hash-ref attr :start (now)))
          (dr (hash-ref attr :note-len 1)))
      (midi-note-on el
                    :at (beat->time at)
                    :duration (beat->time dr)))))

(metro-add :solo
           (λ(beat)
             (Sa (ht :fn play-midi :note-len 1/12)
                 (if (even? beat)
                   (S  D-3 F-3 G-3 A-3)
                   (S  F-3 G-3 A-3 C-4)))))

(metro-add :tick (Sa (ht :fn play-midi :dur 1/12)
                     C-3))

(metro-add :solo #f)

(metro-add :tick #f)

