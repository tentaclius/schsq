<CsoundSynthesizer>
<CsOptions>
  -o dac
</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 4
0dbfs = 1
nchnls = 3

;; helper opcode
opcode def, i, ii  ;; set a value or use the default value in case of 0
  iVal, iDflt xin
  if iVal == 0 then
    iVal = iDflt
  endif
  xout iVal
endop

opcode ignore, 0, i
  ignore xin
endop

instr MetronomeSnd
  iFq def p4, 1000
  ;
  kEnv linseg 1, 0.051, 0
  aSig noise kEnv/2, 0.5
  aSig mvchpf aSig, iFq, 0.9
  aSig *= kEnv
  ;
  aSin poscil kEnv/2, iFq
  aSig += aSin
  ;
  outall aSig
endin

instr MetronomeStart
  schedule "MetronomeSnd", 0, 1, 1000
  schedule "MetronomeSnd", 1/2, 1, 500
  schedule "MetronomeSnd", 2/2, 1, 500
  schedule "MetronomeSnd", 3/2, 1, 500
  schedule "Metronome", 2, 1
endin

</CsInstruments>
<CsScore>
i "MetronomeSnd" 0 0.5 1000
i . + . 500
i . +
i . +
</CsScore>
</CsoundSynthesizer>
