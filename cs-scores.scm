(add-to-load-path ".")
(use-modules (schsq))
(use-modules (ice-9 textual-ports))
(use-modules (ice-9 eval-string))

(with-output-to-file
  (list-ref (command-line) 3)
  (λ() (load (list-ref (command-line) 1))))
