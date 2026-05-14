<CsoundSynthesizer>
<CsOptions>
-o dac
-+skip_seconds=0
-m 4
;-t60
</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 4
nchnls = 2
0dbfs = 1

#include "lib.orc"

;; Instruments
instr Ding
  iFreq mtof p4
  print p4
  print iFreq
  iGain def p5, 0.5
  iDecay def p6, 0.7
  iAttack = 0.01
  ;
  kEnv transeg 0, iAttack, 6, iGain, iDecay, -6, 0
  if trigger(kEnv, 0, 1) == 1 then
    turnoff
  endif
  aSig poscil kEnv, iFreq
  outall aSig
endin

instr Bd
  iGain def p4, 1
  iFreq def p5, 330
  iDur def p6, 0.06
  ;
  kEnv linseg iGain, iDur*3, 0
  kFreq linseg iFreq, iDur, 10
  aSig poscil 1, kFreq
  aBass poscil iGain, 60
  ;
  aSig = (aSig + aBass) * kEnv / 2
  out aSig, aSig
endin

instr Hh
  iGain def p4, 0.5
  iFreq def p5, 3000
  iDur def p6, 0.07
  ;
  kEnv linseg iGain, iDur, 0
  aSig noise kEnv, 0
  aSig mvchpf aSig, iFreq, 0.9
  ;
  out aSig, aSig
endin
  
</CsInstruments>
<CsScore bin="guile guile-score-preproc.scm">

[(define sq (let* ((sequence (list 0 2 5 7)) (ptr sequence))
  (lambda ()
    (+ C-4 (cond
      ((null? ptr) (set! ptr (cdr sequence)) (car sequence))
      (else (let ((x (car ptr))) (set! ptr (cdr ptr)) x)))))))]

t0 [(* 60 2)]
i"Ding" 0 1 [(sq)]
i. + . [(sq)]
i. + . [(sq)]
i. + . [(sq)]

B2
[(for-each
  (lambda(s) (writeln "i\"Ding\" " (exact->inexact (/ s 20)) " 2 " s " 0.3"))
  (chord C-4 *major* #:c7))]

</CsScore>
</CsoundSynthesizer>
