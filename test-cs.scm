(add-to-load-path ".")
(use-modules (schsq))
(read-set! keywords 'prefix)
(register-here-string "/EOT")

(set-default-scheduler (sch-init))
(schedule (now) (λ() (writeln "hello")) (list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; udp comm with cs

(cs-init 1234)
(cs-send "schedule 1,0,1,.9,440" "schedule 1,1,1,.9,220")

(cs-send #/
instr 11
  aSig vco2 0.9,440
  outall aSig
endin
/EOT)

(cs-send "schedule 11,0,1")

(cs-close)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc

(cs-render-score-loop
  0 10 12
  (S (list 1 0.8 440)
     (U (list 2 0.4 220))
     (Ua (ht :start 0) 5)
     "Sin"))

(repeat-sequence 3.1 (S 1 2 3 4))

(writeln #/
  hello
  world
  /EOT
  )
