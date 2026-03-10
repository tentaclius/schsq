(add-to-load-path ".")
(use-modules (schsq))
(read-set! keywords 'prefix)
(use-modules (ice-9 iconv))

(set-default-scheduler (sch-init))
(schedule (now) (lambda() (writeln "hello")) (list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; udp comm with cs
(define (init-sc port)
  (vector
    (socket PF_INET SOCK_DGRAM 0)
    (make-socket-address PF_INET (inet-pton PF_INET "127.0.0.1") port)))

(def h (init-sc 1234))

(define (sc-send handle message)
  (sendto (vector-ref handle 0) (string->bytevector message "utf8") (vector-ref handle 1)))

(sc-send h "hello")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc
(ev-schedule (Sa (ht :fn (λ(e attr) (writeln e ": " (ht->str attr)))) 1 2 3) (ht :start 0))
