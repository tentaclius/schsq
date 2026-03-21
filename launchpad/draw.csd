<CsoundSynthesizer>
<CsOptions>
  -o dac
  -+rtaudio=alsa
  -+rtmidi=alsaseq
  -Ma -Qa
  --port=1234
  -L stdin
</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 4
0dbfs = 1
nchnls = 2

giColorWhite = 3
giColorInd = 0
giColorArr[] fillarray 72, 84, 13, 21, 78, 67, 0

opcode drw_clean, 0,0
  iNote = 36
loop:
  noteon 1, iNote, giColorWhite
  loople iNote, 1, 99, loop
  noteon 1, 98, giColorArr[giColorInd]
  noteon 1, 99, 71
endop

massign 0, 1
instr 1
  iNote notnum
  if iNote == 98 then
    giColorInd = (giColorInd + 1) % 7
    noteon 1, 98, giColorArr[giColorInd]
  elseif iNote == 99 then
    drw_clean
  else
    noteon 1, iNote, giColorArr[giColorInd]
  endif
endin

instr 33
  drw_clean
endin

;; light up Launchpad keys
instr 2
#include "colors.orc"
endin

;; play a sample
instr 3
  iTable = p4
  iGain = p5
  iFreq = p6
  xtratim ftlen(iTable) * 261.626/iFreq / sr
  aSig,aSig2 loscil3 iGain, iFreq, iTable, 261.626, 0
  outall aSig
endin

</CsInstruments>
<CsScore>
;f101 0 0 1 "kick1.wav" 0 0 0
;i3 0 1 101 1 120
i2 0 1
</CsScore>
</CsoundSynthesizer>
