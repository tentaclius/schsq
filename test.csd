<CsoundSynthesizer>
<CsOptions>
-o dac ;--port=1234 -L stdin
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 4
0dbfs=1
nchnls=2

instr 1, Sin
  iAmp = p4
  iFreq = p5
  iDur = p3
  ;
  kAmp linseg 0,.01,iAmp,.1,iAmp,.2,0
  aSig poscil kAmp,iFreq
  outall aSig
endin

#include "livecode.orc"

</CsInstruments>
<CsScore bin="guile cs-scores.scm">
(load "my-score.scm")
</CsScore>
</CsoundSynthesizer>
