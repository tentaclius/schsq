(add-to-load-path ".")
(use-modules (schsq))
(read-set! keywords 'prefix)
;
(midi-init "CL")
(metro-start)

(define play-midi
  (λ(el attr)
    (let ((at (hash-ref attr :start (beats)))
          (dr (hash-ref attr :dur 1))
          (nl (hash-ref attr :note-len 1)))
      (when el
        (midi-note-on el
                      :at (beat->time at)
                      :duration (beat->time (* dr nl)))))))

(metro-add
  :bass
  (λ(i)
    (seq-map
      (λ(e) (- e 12))
      (Sa (ht :fn play-midi)
          (per-beat
            i
            (S D-3 E-3 G-3 A-3)
            (S G-3 B-3 C-4))))))

(metro-add :bass '())

(sleep 5)
