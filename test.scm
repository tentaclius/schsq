(load "schsq.scm")

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

(define (looper tm) #t)

;;;

(midi-init "CL")

(define (midi-handler event)
  (writeln event))

(call-with-new-thread
  (lambda ()
    (let loop ()
      (midi-handler (midi-receive))
      (loop))))

(midi-note-on C-4)

;;;;

(midi-init "CL")

(ev-schedule (make <sim> :events (list 1 2 3))
             (ht :fn (λ(el attrs) (writeln (ht->str attrs)))
                 :start (beat-quant 1)
                 :dur 1/4))

(ev-schedule (make <seq> :events (list 1 2 3))
             (ht :fn (λ(el attr) (writeln (ht->str attr)))
                 :start (beat-quant 1)
                 :dur 2))

(at-beat b (midi-note-on ev :dur dur))

(define-macro (at-beat beat ))

(ev-schedule (make <seq> :events (list D-4
                                       (make <seq> :events (list F-4 F-5))
                                       (make <sim> :events (list G-4 C-5))
                                       A-4))
             (ht :fn (λ(e attr)
                       (midi-note-on e :at (beat->time (hash-ref attr :start))
                                     :duration (beat->time (hash-ref attr :dur))))
                 :dur 2))

(beat->time 1/3)

(let ((n (now)))
  (midi-note-on C-4 :at n :duration (* 1 SEC))
  (midi-note-on D-4 :at (+ n (* 1 SEC)) :duration (* 1 SEC)))

(midi-note-on C-4 :duration (* 1 SEC))

(beats)
