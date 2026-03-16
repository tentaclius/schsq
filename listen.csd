<CsoundSynthesizer>
<CsOptions>
-o dac --port=1234 -L stdin
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
  aAmp linseg 0,.01,iAmp,.1,iAmp,.2,0,1,0
  aSig poscil aAmp,iFreq
  outall aSig
endin

giBPM = 120
opcode nextbeat, i, 0
  iTime times
  iBeatPeriod = 60 / giBPM
  iBeats = int(iTime / iBeatPeriod + 1e-9)
  xout (iBeats + 1) * iBeatPeriod - iTime
endop

#include "livecode.orc"

</CsInstruments>
<CsScore>
</CsScore>
</CsoundSynthesizer>
