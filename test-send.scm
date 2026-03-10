(add-to-load-path ".")
(use-modules (schsq))
(read-set! keywords 'prefix)

(osc-send "localhost" 5555 "/test" 
          "hiiaaaaaa" #\space 1.1)

