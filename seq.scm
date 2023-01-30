(use-modules (system foreign))
(use-modules (system foreign-library))

(define lib-path "./schsq")

(define MIDI_NOTEON 6)
(define MIDI_NOTEOFF 7)
(define SEC 1000000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILITARY FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (λ args . body)
  `(lambda ,args ,@body))

(define (cat . args)
  (with-output-to-string (lambda() (for-each display args))) )

(define (writeln . args)
  (display (apply cat args))
  (newline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FFI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sch-init (foreign-library-function lib-path "sch_run" #:return-type '* #:arg-types '()))

(define sch-stop (foreign-library-function lib-path "sch_stop" #:return-type void #:arg-types '(*)))

(define now 
  (let ((ff (foreign-library-function lib-path "sch_now" #:return-type (list long long) #:arg-types '())))
    (lambda ()
      (let ((tm (parse-c-struct (ff) (list long long))))
        (+ (* (car tm) SEC) (cadr tm))))))

(define schedule-event
  (let ((ff (foreign-library-function lib-path "schedule_guile"
                                     #:return-type void  #:arg-types (list '* (list long long) '*))))
    (lambda (sch tm proc)
      (ff sch (make-c-struct (list long long) (list (floor (/ tm SEC)) (modulo tm SEC)))
         (procedure->pointer void proc (list))))))

(define midi-init
  (let ((ff (foreign-library-function lib-path "midi_init"
                                     #:return-type '*  #:arg-types (list '*))))
    (lambda (name)
      (ff (string->pointer name)))))

(define midi-send-noteon
  (let ((ff (foreign-library-function lib-path "midi_send_note"
                                     #:return-type int  #:arg-types (list '* uint32 uint32 uint32 uint32))))
    (lambda (handle note velo chan)
      (ff handle 6 note velo chan))))

(define midi-send-noteoff
  (let ((ff (foreign-library-function lib-path "midi_send_note"
                                     #:return-type int  #:arg-types (list '* uint32 uint32 uint32 uint32))))
    (lambda (handle note velo chan)
      (ff handle 7 note velo chan))))

(define schedule-midi-note
  (let ((ff (foreign-library-function lib-path "schedule_midi_note"
                                     #:return-type int  #:arg-types (list '* '* (list long long) uint32 uint32 uint32 uint32))))
    (lambda (midi sch tm type note velo chan)
      (ff midi sch (make-c-struct (list long long) (list (floor (/ tm SEC)) (modulo tm SEC))) type note velo chan))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define s (sch-init))
(define m (midi-init "Client"))

;(let loop ()
;   (read)
;   (writeln 0)
;   (schedule-midi-note m s (+ (now)) MIDI_NOTEON 50 127 0)
;   (schedule-midi-note m s (+ (now) (* 1 SEC)) MIDI_NOTEOFF 50 0 0)
;   (schedule-midi-note m s (+ (now) (* 2 SEC)) MIDI_NOTEON 62 127 0)
;   (schedule-midi-note m s (+ (now) (* 3 SEC)) MIDI_NOTEOFF 62 127 0)
;   (loop))

;
;(define lp
;  (let ((i 0))
;    (lambda ()
;      (display i) (newline)
;      (set! i (1+ i))
;      (schedule-event s (time-add-nsec (now) (/ 1000000000 4)) lp))))
;
;(schedule-event s (time-add-nsec (now) (/ 1000000000 4)) lp)
;
;(sleep 10)

