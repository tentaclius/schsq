;;; {{{

(use-modules (oop goops))
(use-modules (oop goops describe))
(use-modules (system foreign))
(use-modules (system foreign-library))
(use-modules (ice-9 exceptions))
(use-modules (ice-9 threads))
(use-modules (rnrs bytevectors))

(define lib-path "./schsq")

(define MIDI_NOTEON 6)
(define MIDI_NOTEOFF 7)
(define SEC 1000000000)

(read-set! keywords 'prefix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILITARY FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (λ args . body)
  `(lambda ,args ,@body))

(define (cat . args)
  (with-output-to-string (λ() (for-each display args))) )

(define (writeln . args)
  (display (apply cat args))
  (newline))

(define (ht->str ht)
  (cat "{"
       (string-join (hash-map->list (λ(key val) (cat key " " val)) ht)
                    ", ")
       "}"))

(define-macro (def . vals)
  `(begin ,@(let loop ((p vals) (defs (list)))
              (if (or (null? p) (null? (cdr p)))
                (reverse defs)
                (loop (cddr p) (cons `(define ,(car p) ,(cadr p)) defs))))))

(define-macro (ht . pairs)
  (let ((h (gensym)))
    `(let ((,h (make-hash-table)))
       ,@(let loop ((p pairs) (result (list)))
           (cond
             ((or (null? p) (null? (cdr p))) result)
             (else (loop (cddr p) (cons `(hash-set! ,h ,(car p) ,(cadr p)) result)))))
       ,h)))

(define (nsec+sec ns . s)
  (+ ns (* SEC (apply + s))))

(define (to-c-time tm)
  (make-c-struct (list long long)
                 (list (inexact->exact (floor (/ tm SEC)))
                       (inexact->exact (modulo (floor tm) SEC)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FFI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Scheduler

(def *scheduler* #f
     *midi* #f)

(define sch-init (foreign-library-function lib-path "sch_run" #:return-type '* #:arg-types '()))

(define sch-stop (foreign-library-function lib-path "sch_stop" #:return-type void #:arg-types '(*)))

(define now 
  (let ((ff (foreign-library-function lib-path "sch_now" #:return-type (list long long) #:arg-types '())))
    (lambda ()
      (let ((tm (parse-c-struct (ff) (list long long))))
        (+ (* (car tm) SEC) (cadr tm))))))

(define (now+sec . n)
  (+ (now) (* (apply + n) SEC)))

(define schedule
  (let ((ff (foreign-library-function lib-path "schedule_guile"
                                     #:return-type void  #:arg-types (list '* (list long long) '*))))
    (lambda* (tm proc param #:optional (sch *scheduler*))
      (ff sch
          (to-c-time tm)
          (procedure->pointer
            void
            (lambda ()
              (with-exception-handler
                (λ(e) (writeln "ERROR: " e) #f)
                (λ()  (apply 
                         (cond
                           ((symbol? proc) (eval proc (interaction-environment)))
                           ((procedure? proc) proc)
                           (else (λ(i) #f)))
                         param))
                #:unwind? #t))
            (list))))))

;;; Beats

(define *bpm* 60)

(define (time->beat tm)
  (* *bpm* (/ tm 60 SEC)))

(define (beat->time bt)
  (* bt 60 SEC (/ *bpm*)))

(define (beats)
  (time->beats (now)))

(define (beat-quant n)
  (let ((b (ceiling (beats))))
    (+ b (- n (modulo b n)))))

;;; MIDI

(def
  C-0 12   C0  12      C-1 24   C1  24      C-2 36   C2  36      C-3 48   C3  48      C-4 60   C4  60
  C#0 13   Db0 13      C#1 25   Db1 25      C#2 37   Db2 37      C#3 49   Db3 49      C#4 61   Db4 61
  D-0 14   D0  14      D-1 26   D1  26      D-2 38   D2  38      D-3 50   D3  50      D-4 62   D4  62
  D#0 15   Eb0 15      D#1 27   Eb1 27      D#2 39   Eb2 39      D#3 51   Eb3 51      D#4 63   Eb4 63
  E-0 16   E0  16      E-1 28   E1  28      E-2 40   E2  40      E-3 52   E3  52      E-4 64   E4  64
  F-0 17   F0  17      F-1 29   F1  29      F-2 41   F2  41      F-3 53   F3  53      F-4 65   F4  65
  F#0 18   Gb0 18      F#1 30   Gb1 30      F#2 42   Gb2 42      F#3 54   Gb3 54      F#4 66   Gb4 66
  G-0 19   G0  19      G-1 31   G1  31      G-2 43   G2  43      G-3 55   G3  55      G-4 67   G4  67
  G#0 20   Ab0 20      G#1 32   Ab1 32      G#2 44   Ab2 44      G#3 56   Ab3 56      G#4 68   Ab4 68
  A-0 21   A0  21      A-1 33   A1  33      A-2 45   A2  45      A-3 57   A3  57      A-4 69   A4  69
  A#0 22   Bb0 22      A#1 34   Bb1 34      A#2 46   Bb2 46      A#3 58   Bb3 58      A#4 70   Bb4 70
  B-0 23   B0  23      B-1 35   B1  35      B-2 47   B2  47      B-3 59   B3  59      B-4 71   B4  71
  ;
  C-5 72   C5  72      C-6 84   C6  84      C-7 96   C7  96      C-8 108  C8  108     C-9 120  C9  120
  C#5 73   Db5 73      C#6 85   Db6 85      C#7 97   Db7 97      C#8 109  Db8 109     C#9 121  Db9 121
  D-5 74   D5  74      D-6 86   D6  86      D-7 98   D7  98      D-8 110  D8  110     D-9 122  D9  122
  D#5 75   Eb5 75      D#6 87   Eb6 87      D#7 99   Eb7 99      D#8 111  Eb8 111     D#9 123  Eb9 123
  E-5 76   E5  76      E-6 88   E6  88      E-7 100  E7  100     E-8 112  E8  112     E-9 124  E9  124
  F-5 77   F5  77      F-6 89   F6  89      F-7 101  F7  101     F-8 113  F8  113     F-9 125  F9  125
  F#5 78   Gb5 78      F#6 90   Gb6 90      F#7 102  Gb7 102     F#8 114  Gb8 114     F#9 126  Gb9 126
  G-5 79   G5  79      G-6 91   G6  91      G-7 103  G7  103     G-8 115  G8  115     G-9 127  G9  127
  G#5 80   Ab5 80      G#6 92   Ab6 92      G#7 104  Ab7 104     G#8 116  Ab8 116
  A-5 81   A5  81      A-6 93   A6  93      A-7 105  A7  105     A-8 117  A8  117
  A#5 82   Bb5 82      A#6 94   Bb6 94      A#7 106  Bb7 106     A#8 118  Bb8 118
  B-5 83   B5  83      B-6 95   B6  95      B-7 107  B7  107     B-8 119  B8  119)

(define midi-init
  (let ((ff (foreign-library-function lib-path "midi_init"
                                     #:return-type '*  #:arg-types (list '*))))
    (lambda (name)
      (when (not *scheduler*)
        (set! *scheduler* (sch-init)))
      (set! *midi* (ff (string->pointer name)))
      *midi*)))

(define* (midi-receive #:optional (midi *midi*))
  (let* ((ff (foreign-library-function lib-path "midi_receive"
                                       #:return-type '* #:arg-types (list '*)))
         (ev (ff midi)))
    (cond
      ((null-pointer? ev) #vu8())
      (else (pointer->bytevector ev 28)))))

(define midi-schedule-event
  (let ((ff (foreign-library-function lib-path "midi_schedule_event"
                                      #:return-type void #:arg-types (list '* '* (list long long) '*))))
    (lambda* (data #:optional (at (now)) (midi *midi*) (scheduler *scheduler*))
      (ff midi scheduler
          (to-c-time at)
          (bytevector->pointer data)))))

(define midi-schedule-note
  (let ((ff (foreign-library-function lib-path "midi_schedule_note"
                                     #:return-type int  #:arg-types (list '* '* (list long long) uint32 uint32 uint32 uint32))))
    (lambda* (tm type note velo chan #:optional (midi *midi*) (sch *scheduler*))
      (ff midi sch (to-c-time tm) type note velo chan))))

(define* (midi-note-on #:optional (note C-4) #:key (at (now)) (duration #f) (velo 127) (chan 0) (midi *midi*) (scheduler *scheduler*))
  (midi-schedule-note at MIDI_NOTEON note velo chan midi scheduler)
  (when duration
    (midi-schedule-note (+ at duration) MIDI_NOTEOFF note velo chan midi scheduler)))

(define* (midi-note-off #:optional (note C-4) #:key (at (now)) (velo 0) (chan 0) (midi *midi*) (scheduler *scheduler*))
  (midi-schedule-note at MIDI_NOTEOFF note velo chan midi scheduler))

(define midi-send-ctrl
  (let ((ff (foreign-library-function lib-path "midi_schedule_ctrl"
                                      #:return-type void  #:arg-types (list '* '* (list long long) uint32 uint32 uint32))))
    (lambda* (ctrl val #:key (midi *midi*) (scheduler *scheduler*) (at (now)) (chan 0))
             (ff midi scheduler (to-c-time at) ctrl val chan))))

(define* (make-midi-note #:optional (type MIDI_NOTEON) (note C-4) (velo 127) (chan 0))
  (u8-list->bytevector (list type 0 0 253 0 0 0 0 0 0 0 0 0 0 254 253 chan note velo 0 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCHEME ABSTRACTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (merge-attrs . hn)
  (let ((result (make-hash-table)))
    (for-each
      (λ(hs) (hash-for-each
                (λ(key value) (hash-set! result key value))
                hs))
      hn)
    result))

(define-class <events> ()
  (events #:init-keyword #:events
          #:init-form (list)
          #:accessor events)
  (attrib #:init-keyword #:attrib
          #:init-form (make-hash-table)
          #:accessor attrib))

(define-class <seq> (<events>))
(define-class <sim> (<events>))

(define-method (ev-schedule (el <seq>) attr)
   (let ((start (hash-ref attr #:start (beats)))
         (dur   (hash-ref attr #:dur 1))
         (len   (length (events el))))
     (let loop ((i 0) (evx (events el)))
       (when (not (null? evx))
           (ev-schedule (car evx)
                        (merge-attrs attr (attrib el)
                                     (ht #:start (+ start (* dur (/ i len))) #:dur (/ dur len))))
           (loop (1+ i) (cdr evx))))))

(define-method (ev-schedule (el <events>) attr)
   (for-each (λ(x) (ev-schedule x (merge-attrs attr (attrib el))))
             (events el)))

(define-method (ev-schedule el attr)
   (let ((fun (hash-ref attr #:fn)))
     (when fun (fun el attr))))

;;; }}}
