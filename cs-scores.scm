(add-to-load-path ".")
(use-modules (schsq))
(use-modules (ice-9 eval-string))
(use-modules (rnrs io ports))

;(with-output-to-file
  ;(list-ref (command-line) 3)
  ;(λ() (load (list-ref (command-line) 1))))

(with-output-to-file
  (list-ref (command-line) 2)
  (λ() (eval-string (get-string-all (open-file-input-port (list-ref (command-line) 1))))))
