(add-to-load-path ".")
(use-modules (ice-9 rdelim)
             (ice-9 textual-ports)
             (rnrs io ports)
             (schsq))

(define (read-bracketed port)
  ;; Read until matching ']'
  (let loop ((chars '()))
    (let ((c (read-char port)))
      (cond
        ((eof-object? c)
         (error "Unterminated [ ... ] block"))

        ((char=? c #\])
         (list->string (reverse chars)))

        (else
          (loop (cons c chars)))))))

(define (eval-to-string code)
  (eval (read (open-input-string code))
        (current-module)))

(define (process port)
  (let loop ()
    (let ((c (read-char port)))
      (cond
        ((eof-object? c)
         #t)

        ((char=? c #\[)
         (let ((val (eval-to-string (read-bracketed port))))
           (unless (unspecified? val) (display val)))
         (loop))

        (else
          (write-char c)
          (loop))))))

(let* ((input-file-name (list-ref (command-line) 1))
       (input-port (open-file-input-port input-file-name))
       (output-file-name (list-ref (command-line) 2)))
  (with-output-to-file output-file-name
                       (λ() (process input-port))))
