<CsoundSynthesizer>
<CsOptions>
-o dac -+rtaudio=alsa -+rtmidi=alsaseq -Ma --port=1234 -L stdin
</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 4
0dbfs=1
nchnls=2

massign 0, 1

instr 1
  iNote notnum
  print iNote
endin

#include "livecode.orc"

</CsInstruments>
<CsScore>
</CsScore>
</CsoundSynthesizer>
