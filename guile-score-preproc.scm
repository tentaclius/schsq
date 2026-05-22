(add-to-load-path ".")
(use-modules (ice-9 rdelim)
             (ice-9 textual-ports)
             (rnrs io ports)
             (schsq))

(define (process port)
  (let loop ()
    (let ((c (read-char port)))
      (cond
        ((eof-object? c)
         #t)

        ((char=? c #\%)
         (display (eval (read port) (current-module)))
         (loop))

        (else
          (write-char c)
          (loop))))))

(let* ((input-file-name (list-ref (command-line) 1))
       (input-port (open-file-input-port input-file-name))
       (output-file-name (list-ref (command-line) 2)))
  (with-output-to-file output-file-name
                       (λ() (process input-port))))
