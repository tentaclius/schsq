(define-module (schsq)
               #:export (MIDI_NOTEON MIDI_NOTEOFF SEC
                         cat writeln ht->str def ht htl nsec+sec now+sec random-pick shuffle rotate register-here-string
                         sch-init sch-stop now schedule set-default-scheduler
                         bpm time->beat beat->time beats beat-quant
                         midi-init midi-receive midi-schedule-event midi-schedule-note
                         midi-note-on midi-note-off midi-send-ctrl make-midi-note
                         osc-send osc-recv
                         merge-attrs <events> <seq> <sim> ev-schedule S Sa Sl Sal U Ua Ul Ual A seq-map
                         euclidian sc *chromatic* *pentatonic* *major* *minor* chord per-beat repeat-sequence
                         metro-trigger metro-add metro-start metro-stop
                         cs-init cs-send cs-close cs-render-score cs-render-score-loop beat make-i mtof beat->sec
                         ;
                         C-0 C0   C-1  C1   C-2  C2   C-3  C3   C-4  C4    C-5 C5   C-6  C6   C-7  C7   C-8  C8   C-9  C9   
                         C#0 Db0  C#1  Db1  C#2  Db2  C#3  Db3  C#4  Db4   C#5 Db5  C#6  Db6  C#7  Db7  C#8  Db8  C#9  Db9 
                         D-0 D0   D-1  D1   D-2  D2   D-3  D3   D-4  D4    D-5 D5   D-6  D6   D-7  D7   D-8  D8   D-9  D9  
                         D#0 Eb0  D#1  Eb1  D#2  Eb2  D#3  Eb3  D#4  Eb4   D#5 Eb5  D#6  Eb6  D#7  Eb7  D#8  Eb8  D#9  Eb9 
                         E-0 E0   E-1  E1   E-2  E2   E-3  E3   E-4  E4    E-5 E5   E-6  E6   E-7  E7   E-8  E8   E-9  E9  
                         F-0 F0   F-1  F1   F-2  F2   F-3  F3   F-4  F4    F-5 F5   F-6  F6   F-7  F7   F-8  F8   F-9  F9  
                         F#0 Gb0  F#1  Gb1  F#2  Gb2  F#3  Gb3  F#4  Gb4   F#5 Gb5  F#6  Gb6  F#7  Gb7  F#8  Gb8  F#9  Gb9 
                         G-0 G0   G-1  G1   G-2  G2   G-3  G3   G-4  G4    G-5 G5   G-6  G6   G-7  G7   G-8  G8   G-9  G9  
                         G#0 Ab0  G#1  Ab1  G#2  Ab2  G#3  Ab3  G#4  Ab4   G#5 Ab5  G#6  Ab6  G#7  Ab7  G#8  Ab8
                         A-0 A0   A-1  A1   A-2  A2   A-3  A3   A-4  A4    A-5 A5   A-6  A6   A-7  A7   A-8  A8 
                         A#0 Bb0  A#1  Bb1  A#2  Bb2  A#3  Bb3  A#4  Bb4   A#5 Bb5  A#6  Bb6  A#7  Bb7  A#8  Bb8
                         B-0 B0   B-1  B1   B-2  B2   B-3  B3   B-4  B4    B-5 B5   B-6  B6   B-7  B7   B-8  B8 
                         ))

(use-modules (oop goops))
(use-modules (oop goops describe))
(use-modules (system foreign))
(use-modules (system foreign-library))
(use-modules (ice-9 exceptions))
(use-modules (ice-9 threads))
(use-modules (rnrs bytevectors))
(use-modules (ice-9 receive))
(use-modules (ice-9 iconv))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 textual-ports))

(define lib-path "./schsq")

(define MIDI_NOTEON 6)
(define MIDI_NOTEOFF 7)
(define SEC 1000000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILITARY FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cat . args)
  (with-output-to-string (λ() (for-each display args))) )

(define (writeln . args)
  (display (apply cat args))
  (newline))

(define (ht->str ht)
  (cat "{"
       (string-join (hash-map->list (λ(key val) (cat key " " val)) ht) ", ")
       "}"))

(define-macro (def . vals)
  `(begin ,@(let loop ((p vals) (defs (list)))
              (if (or (null? p) (null? (cdr p)))
                (reverse defs)
                (loop (cddr p) (cons `(define ,(car p) ,(cadr p)) defs))))))

(define-macro (qq . words)
  (string-join (map cat words)))

(define-macro (ht . pairs)
  (let ((h (gensym)))
    `(let ((,h (make-hash-table)))
       ,@(let loop ((p pairs) (result (list)))
           (cond
             ((or (null? p) (null? (cdr p))) result)
             (else (loop (cddr p) (cons `(hash-set! ,h ,(car p) ,(cadr p)) result)))))
       ,h)))

(define (htl pairs)
  (let ((h (make-hash-table)))
    (let loop ((p pairs))
      (cond
        ((or (null? p) (null? (cdr p))) h)
        (else (begin
                (hash-set! h (car p) (cadr p))
                (loop (cddr p))))))))

(define (nsec+sec ns . s)
  (+ ns (* SEC (apply + s))))

(define (to-c-time tm)
  (make-c-struct (list long long)
                 (list (inexact->exact (floor (/ tm SEC)))
                       (inexact->exact (modulo (floor tm) SEC)))))

(define (random-pick ll)
  (let* ((ind (random (length ll)))
         (elm (list-tail ll ind)))
    (values (car elm)
            (append (list-head ll ind) (cdr elm)))))

(define (shuffle ll)
  (let loop ((p ll) (r '()))
    (if (null? p) r
      (receive (value rest) (random-pick p)
               (loop rest (cons value r))))))

(define (rotate ll i)
  (let* ((len (length ll)) (ii (modulo (abs (if (< i 0) (+ len i) i)) len)))
    (append (list-tail ll ii) (list-head ll ii))))

(define (register-here-string delim)
  (let ((delim-len (string-length delim)))
    (read-hash-extend
      #\/
      (lambda (chr port)
        (let loop ((lines '()))
          (let* ((line (read-line port))
                 (trimmed-line (string-trim line)))
            (if (and (>= (string-length trimmed-line) delim-len)
                     (string=? (substring trimmed-line 0 delim-len) delim))
              (begin
                (unget-string port (substring trimmed-line delim-len))
                (string-join (reverse lines) "\n"))
              (loop (cons line lines)))))))))

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

(define (set-default-scheduler sch) (set! *scheduler* sch))

(define (now+sec . n)
  (+ (now) (* (apply + n) SEC)))

(define schedule
  (let ((ff (foreign-library-function lib-path "schedule_guile"
                                     #:return-type void  #:arg-types (list '* (list long long) '*))))
    (lambda* (tm proc param #:optional (sch *scheduler*))
      (writeln sch)
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


(define midi-frequency-table
  (vector 8.176 8.662 9.177 9.723 10.301 10.913 11.562 12.250 12.978 13.750 14.568 15.434
          16.352 17.324 18.354 19.445 20.602 21.827 23.125 24.500 25.957 27.500 29.135 30.868
          32.703 34.648 36.708 38.891 41.203 43.654 46.249 48.999 51.913 55.000 58.270 61.735
          65.406 69.296 73.416 77.782 82.407 87.307 92.499 97.999 103.826 110.000 116.541 123.471
          130.813 138.591 146.832 155.563 164.814 174.614 184.997 195.998 207.652 220.000 233.082
          246.942 261.626 277.183 293.665 311.127 329.628 349.228 369.994 391.995 415.305 440.000
          466.164 493.883 523.251 554.365 587.330 622.254 659.255 698.456 739.989 783.991 830.609
          880.000 932.328 987.767 1046.502 1108.731 1174.659 1244.508 1318.510 1396.913 1479.978
          1567.982 1661.219 1760.000 1864.655 1975.533 2093.005 2217.461 2349.318 2489.016 2637.020
          2793.826 2959.955 3135.963 3322.438 3520.000 3729.310 3951.066 4186.009 4434.922
          4698.636 4978.032 5274.041 5587.652 5919.911 6271.927 6644.875 7040.000 7458.620 7902.133
          8372.018 8869.844 9397.273 9956.063 10548.080 11175.300 11839.820 12543.850))

(define (mtof n)
  (vector-ref midi-frequency-table n))

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

;;; OSC

(define osc-make-msg
  (foreign-library-function lib-path "osc_create_message"
                            #:return-type '*  #:arg-types (list)))

(define osc-free-msg
  (foreign-library-function lib-path "osc_free_message"
                            #:arg-types (list '*)))

(define osc-msg-add-int32
  (foreign-library-function lib-path "osc_message_add_int32"
                            #:arg-types (list '* int32)))

(define osc-msg-add-int64
  (foreign-library-function lib-path "osc_message_add_int64"
                            #:arg-types (list '* int64)))

(define osc-msg-add-char
  (let ((ff (foreign-library-function lib-path "osc_message_add_char"
                                      #:arg-types (list '* uint8))))
    (lambda (msg val)
      (ff msg (char->integer val)))))

(define osc-msg-add-double
  (foreign-library-function lib-path "osc_message_add_double"
                            #:arg-types (list '* double)))

(define osc-msg-add-str
  (let ((ff (foreign-library-function lib-path "osc_message_add_string"
                                      #:arg-types (list '* '*))))
    (lambda (msg val)
      (ff msg (string->pointer val)))))

(define osc-send-msg
  (let ((ff (foreign-library-function lib-path "osc_send_message"
                                      #:arg-types (list '* '* '* '*))))
    (lambda (msg host port path)
      (ff msg (string->pointer host) (string->pointer port) (string->pointer path)))))

(define (osc-send host port path . args)
  (let ((msg (osc-make-msg)))
    (for-each
      (lambda (arg)
        (cond
          ((eqv? (class-of arg) <integer>)
           (if (or (<= arg -2147483647) (>= arg 2147483647))
             (osc-msg-add-int64 msg arg)
             (osc-msg-add-int32 msg arg)))
          ((eqv? (class-of arg) <real>)
           (osc-msg-add-double msg arg))
          ((eqv? (class-of arg) <string>)
           (osc-msg-add-str msg arg))
          ((eqv? (class-of arg) <char>)
           (osc-msg-add-char msg arg))))
      args)
    (osc-send-msg msg host (cat port) path)
    (osc-free-msg msg)))

(define osc-recv
  (foreign-library-function lib-path "osc_receive"
                            #:return-type '* #:arg-types (list uint16)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SEQUENCING ABSTRACTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Beats

(define *bpm* 60)

(define* (bpm #:optional val)
  (if val
    (set! *bpm* val)
    *bpm*))

(define (time->beat tm)
  (* *bpm* (/ tm 60 SEC)))

(define (beat->time bt)
  (* bt 60 SEC (/ *bpm*)))

(define (beat->sec b)
  (/ (* b 60) *bpm*))

(define (beats)
  (time->beat (now)))

(define (beat-quant n)
  (let ((b (ceiling (beats))))
    (+ b (- n (modulo b n)))))

;;;

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
                        (merge-attrs attr
                                     (ht #:start (+ start (* dur (/ i len))) #:dur (/ dur len))
                                     (attrib el)))
           (loop (1+ i) (cdr evx))))))

(define-method (ev-schedule (el <events>) attr)
   (for-each (λ(x) (ev-schedule x (merge-attrs attr (attrib el))))
             (events el)))

(define-method (ev-schedule el attr)
   (let ((fun (hash-ref attr #:fn)))
     (when fun (fun el attr))))

(define (S . events) (make <seq> #:events events))
(define (Sl events)  (make <seq> #:events events))
(define (U . events) (make <sim> #:events events))
(define (Ul events)  (make <sim> #:events events))
(define (Sa attrs . events)
  (make <seq> #:events events #:attrib attrs))
(define (Sal attrs events)
  (make <seq> #:events events #:attrib attrs))
(define (Ua attrs . events)
  (make <sim> #:events events #:attrib attrs))
(define (Ual attrs events)
  (make <sim> #:events events #:attrib attrs))
;(define (A attrs . events)
;  (make <sim> #:events events #:attrib attrs))

(define (seq-map fn sq)
  (let ((ret (shallow-clone sq)))
    (slot-set! ret 'events
               (map (λ(el) (if (is-a? el <events>)
                               (seq-map fn el)
                               (fn el)))
                    (slot-ref sq 'events)))
    ret))

(define* (euclidian n m snt #:optional (nl #f))
  (let ((ii 0))
    (map
      (λ(i) (if (= (floor (* ii (/ m n))) i)
                (let ((ret (if (procedure? snt) (snt) snt)))
                  (set! ii (1+ ii))
                  ret)
                nl))
      (iota (1- m)))))

(define (sc scale step)
  (when (list? scale)
    (set! scale (list->vector scale)))
  (if (list? step)
    (map (λ(n) (sc scale n)) step)
    (+ (vector-ref scale (modulo step (vector-length scale)))
       (* 12 (floor (/ step (vector-length scale)))) )))

(define *chromatic* #(0 1 2 3 4 5 6 7 8 9 10 11))
(define *pentatonic* #(0 3 5 7 10))
(define *major* #(0 2 4 5 7 9 11))
(define *minor* #(0 2 3 5 7 8 10))

(define* (chord mod #:key (shift 0) (root 0) (scale *chromatic*) steps)
  (let ((ch (map
              (λ(n) (+ root (sc scale (+ n shift))))
              (case mod
                ((#f #:c3)  (list 0 2 4))
                ((#:c7)     (list 0 2 4 6))
                ((#:c5)     (list 0 4 7))
                ((#:sus4)   (list 0 2 3))
                ((#:sus2)   (list 0 1 4))
                ((#:sus9)   (list 0 4 8))
                ((#:c7sus4) (list 0 2 3 6))))))
    (if steps
      (sc ch (iota steps))
      ch)))

(define-macro (per-beat i . evs)
   `(case (modulo ,i ,(length evs))
      ,@(let loop ((j 0) (p evs) (result '()))
          (if (null? p) result
            (loop (1+ j) (cdr p)
                  (cons (list (list j) (car p)) result))))))

(define-method (repeat-sequence n (sequence <seq>))
  (let loop ((n-events-left (* (length (events sequence)) n))
             (ev-list (events sequence))
             (accumulator (list)))
    (cond
      ((<= n-events-left 0) (make <seq> #:events (reverse accumulator) #:attrib (attrib sequence)))
      ((null? ev-list) (loop n-events-left (events sequence) accumulator))
      (else (loop (1- n-events-left) (cdr ev-list) (cons (car ev-list) accumulator))))))

;;; Metro

(define *metro-seqs* (make-hash-table))
(define *metro-running* #f)

(define* (metro-add name seq #:optional quant)
  (if quant
    (schedule (beat->time (- (beat-quant quant) 1/2))
              (λ() (hash-set! *metro-seqs* name seq))
              (list))
    (hash-set! *metro-seqs* name seq)))

(define (metro-play time i)
  (when *metro-running*
    (metro-trigger (time->beat time) i)
    (let ((tm (+ time (beat->time 1))))
      (schedule (- tm 1000)
                metro-play (list tm (1+ i))))))

(define (metro-trigger beat i)
  (hash-for-each
    (lambda (key value)
      (with-exception-handler
        (λ(e) (writeln "ERROR: " e) #f)
        (λ() (if (procedure? value)
                 (let ((ret (value i)))
                   (when (is-a? ret <events>)
                     (ev-schedule ret (ht #:start beat))))
                 (ev-schedule value (ht #:start beat))))
        #:unwind? #t))
    *metro-seqs*))

(define (metro-start)
  (set! *metro-running* #t)
  (let ((b (beat->time (beat-quant 1))))
    (schedule (- b 1000)
              metro-play (list b 0))))

(define (metro-stop)
  (set! *metro-running* #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CSOUND INTERACTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *cs-handle* #f)

;; UDP
(define (cs-init port)
  (set! *cs-handle*
    (vector
      (socket PF_INET SOCK_DGRAM 0)
      (make-socket-address PF_INET (inet-pton PF_INET "127.0.0.1") port))))

(define (cs-send . messages)
  (sendto (vector-ref *cs-handle* 0)
          (string->bytevector (cat (string-join messages "\n") "\n") "utf8")
          (vector-ref *cs-handle* 1)))

(define (cs-close)
  (close (vector-ref *cs-handle* 0))
  (vector-set! *cs-handle* 0 #f))

(define (cs-render-an-event event attr)
  (when event
    (let* ((trigger (if (procedure? event) (event attr) event))
           (instr-number (if (list? trigger) (car trigger) trigger))
           (instr-args   (if (list? trigger) (cdr trigger) '())))
      (writeln
        "i " (if (number? instr-number) instr-number (cat #\" instr-number #\"))
        " " (exact->inexact (hash-ref attr #:start))
        " " (exact->inexact (hash-ref attr #:dur))
        " " (string-join (map cat instr-args))))))

(define (cs-render-score start duration events)
  (ev-schedule events (ht #:fn cs-render-an-event #:start start #:dur duration events)))

(define (cs-render-score-loop start duration loop-until-time events)
  (let ((loop-number (/ (- loop-until-time start) duration)))
    (cs-render-score start (* duration loop-number) (repeat-sequence loop-number events))))

(define (make-i name . args)
  (lambda params
    (let ((param-table (htl params)))
      (let loop ((p args) (result (list)))
        (cond
          ((null? p) (cons name (reverse result)))
          (else (loop (cddr p) (cons (hash-ref param-table (car p) (cadr p)) result))))))))
