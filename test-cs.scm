(add-to-load-path ".")
(use-modules (schsq))
(read-set! keywords 'prefix)

(set-default-scheduler (sch-init))
(schedule (now) (lambda() (writeln "hello")) (list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; udp comm with cs

(cs-init 1234)
(cs-send "schedule 1,0,1,.9,440" "schedule 1,1,1,.9,220")
(cs-close)

(cs-send "&i \"Bass\" 0 1 110 0.2")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc

(cs-render-score-loop
  0 10 12
  (S (list 1 0.8 440)
     (U (list 2 0.4 220))
     (Ua (ht :start 0) 5)
     "Sin"))

(repeat-sequence 3.1 (S 1 2 3 4))
