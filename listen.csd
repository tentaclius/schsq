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
  kAmp linseg 0,.01,iAmp,.1,iAmp,.2,0
  aSig poscil kAmp,iFreq
  outall aSig
endin

giBeatPeriod = 0.4
opcode nextbeat, i,0
  iTime times
  iBeats = int(iTime / giBeatPeriod + 1e-9)
  xout (iBeats + 1) * giBeatPeriod - iTime
endop

instr 3
  scorelinei {{
    i1 0 1 .8 880
  }}
endin

</CsInstruments>
<CsScore>
i1 0 1 .8 440 
i3 0 1
</CsScore>
</CsoundSynthesizer>
