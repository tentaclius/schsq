(add-to-load-path ".")
(use-modules (schsq))
(read-set! keywords 'prefix)

;(tracker :play
;  #(0    0     0     1     2)
;  A-3    _     _     A-1   _
;  B-3    _     _     _     A-1)

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


;;;

(add-to-load-path ".")
(use-modules (schsq))
(read-set! keywords 'prefix)
;
(midi-init "CL")
(metro-start)
;
(define play-midi
  (λ(el attr)
    (let ((at (hash-ref attr :start (beats)))
          (dr (hash-ref attr :dur 1))
          (nl (hash-ref attr :note-len 1)))
      (midi-note-on el
                    :at (beat->time at)
                    :duration (beat->time (* dr nl))))))
;
(sleep 1)
;
(metro-add
  :bass
  (λ(i)
    (Sa (ht :fn play-midi)
      (case (modulo i 8)
        ((0) (S C-3 C-2 C-2))
        ((1) (S C-3 C-3 C-2))
        ((2) (S C-3 C-2 C-2))
        ((3) (S C-3 C-3 C-2))
        ((4) (S A#0 A#1 A#1))
        ((5) (S A#0 A#2 A#1))
        (else #f)
        ))))

((λ(i)
    (Sa (ht :fn play-midi)
      (case (modulo i 8)
        ((0) (S C-3 C-2 C-2))
        ((1) (S C-3 C-3 C-2))
        ((2) (S C-3 C-2 C-2))
        ((3) (S C-3 C-3 C-2))
        ((4) (S A#0 A#1 A#1))
        ((5) (S A#0 A#2 A#1))
        ((6) (S F-3 F-2 F-3))
        ((7) (S G-2 G-1 G-1))
        ))) 1)

(case (modulo 3 8)
  ((0) (S C-3 C-2 C-2))
  ((1) (S C-3 C-3 C-2))
  ((2) (S C-3 C-2 C-2))
  ((3) (S C-3 C-3 C-2))
  ((4) (S A#0 A#1 A#1))
  ((5) (S A#0 A#2 A#1))
  ((6) (S F-3 F-2 F-3))
  ((7) (S G-2 G-1 G-1))
  )

(metro-add :bass #f)

(metro-add
  :solo
  (λ(i) 
    (Sal (ht :note-len 1/4 :fn play-midi)
         (per-beat
           i
           (chord #:c7 :root C-3 :shift 0 :scale *minor* :steps (random 7))
           (chord #:sus4 :root C-3 :shift 0 :scale *minor* :steps (random 16))
           (chord #:sus2 :root C-3 :shift 0 :scale *minor* :steps (random 7))
           (chord #:c7sus4 :root C-3 :shift 0 :scale *minor* :steps (random 7))
           (chord #:c7 :root C-3 :shift 4 :scale *minor* :steps (random 9))
           (chord #:c7 :root C-3 :shift 5 :scale *minor* :steps (random 9))
           (chord #:c7 :root C-3 :shift 2 :scale *minor* :steps (random 9))
           (chord #:c7 :root C-3 :shift 3 :scale *minor* :steps (random 9))))))

(chord #:c7 :root C-3 :shift 3 :scale *minor* :steps (random 9))

(metro-add :solo #f)

(shuffle '(1 2 3 4))

(rotate '(1 2 3 4) (random 7))

(random-pick '(1 2 3 4))

(let ((i -1) (ll '(1 2 3 4)))
  (if (< i 0) (+ (length ll) i -1) i))


(metro-add
  :solo
  (λ(i) 
    (Sal (ht :fn play-midi :note-len 1/3)
         (map (λ(n) (+ D-2 (sc *pentatonic* n)))
              (list 0 1 (+ 2 (random 5)) 1 2 (if (odd? i) 4 5))))))

(metro-add
  :solo1
  (λ(i) 
    (Sal (ht :fn play-midi :note-len 1/3)
         (map (λ(n) (+ D-2 (sc *pentatonic* (+ 5 n))))
              (list 0 1 3 1 2 (if (odd? i) 4 5))))))

(metro-add :solo #f)

(metro-add :solo
  (lambda (i)
    (let* ((sq (euclidian 5 18 D-4 A-4))
           (ln (length sq)))
      (Sal (ht :fn play-midi :note-len 1/3)
           (per-beat i
                     (list-head sq (floor (/ ln 2)))
                     (list-head (list-tail sq (floor (/ ln 2)))
                                (floor (/ ln 2))))))))

(metro-add :solo #f)

;;;

(osc-send "localhost" 20202 "/test" 
          "hiiaaaaaaaaaaaaaaaaaaaaaaaaaaaa" #\space 1.1)

(osc-recv 5555)
