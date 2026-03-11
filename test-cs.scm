(add-to-load-path ".")
(use-modules (schsq))
(read-set! keywords 'prefix)

(set-default-scheduler (sch-init))
(schedule (now) (lambda() (writeln "hello")) (list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; udp comm with cs

(def h (cs-init 1234))
(cs-send h "hello")
(cs-close h)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc

(cs-render-score-loop
  0 10 12
  (S (list 1 0.8 440)
     (U (list 2 0.4 220))
     (Ua (ht :start 0) 5)
     "Sin"))

(repeat-sequence 3.1 (S 1 2 3 4))

