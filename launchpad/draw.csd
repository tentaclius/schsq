<CsoundSynthesizer>
<CsOptions>
  -o dac -+rtaudio=alsa -+rtmidi=alsaseq -Ma -Qa --port=1234 -L stdin
</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 4
0dbfs = 1
nchnls = 2

giColorWhite = 0
giColorInd = 0
giColorArr[] fillarray 72, 84, 13, 21, 78, 67, 3

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

</CsInstruments>
<CsScore>
</CsScore>
</CsoundSynthesizer>
