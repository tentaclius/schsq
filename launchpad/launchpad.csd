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
0dbfs=1
nchnls=2

;; some decent instruments from livecode's lib by Steven Yi
;#include "livecode.orc"

;; load samples
giKick2 ftgen 0,0,0,1,"s/kick2.wav",0,0,0
giKick ftgen 0,0,0,1,"s/kick1.wav",0,0,0
giHh1 ftgen 0,0,0,1,"s/hh1.wav",0,0,0
giHh2 ftgen 0,0,0,1,"s/hh2.wav",0,0,0
giClick ftgen 0,0,0,1,"s/click.wav",0,0,0
giCowbell ftgen 0,0,0,1,"s/cowbell.wav",0,0,0
giHhLong ftgen 0,0,0,1,"s/hh-long.wav",0,0,0
giHhRim ftgen 0,0,0,1,"s/hh-rim.wav",0,0,0
giSnare1 ftgen 0,0,0,1,"s/snare1.wav",0,0,0
giSnare2 ftgen 0,0,0,1,"s/snare2.wav",0,0,0

;; 64 65 66 67 96 97 98 99
;; 60 61 62 63 92 93 94 95
;; 56 57 58 59 88 89 90 91
;; 52 53 54 55 84 85 86 87
;; 48 49 50 51 80 81 82 83
;; 44 45 46 47 76 77 78 79
;; 40 41 42 43 72 73 74 75
;; 36 37 38 39 68 69 70 71

;; MIDI dispatcher
#define COLOR_BLANK #0#
giColor = 45
massign 0, 1
instr 1
  iNote notnum
  iVelo veloc
  if iNote == 43 || iNote == 72 then
    schedule 3,0,1,giKick,(iVelo/127),mtof(60)
  elseif iNote == 42 || iNote == 73 then
    schedule 3,0,1,giKick2,(iVelo/127),mtof(60)
  elseif iNote == 41 || iNote == 74 then
    schedule 3,0,1,giKick2,(iVelo/127),mtof(60-24)
  elseif iNote == 40 || iNote == 75 then
    schedule 3,0,1,giKick2,(iVelo/127),mtof(60+12)
  ;
  elseif iNote == 47 || iNote == 76 then
    schedule 3,0,1,giHh1,(iVelo/127),mtof(60)
  elseif iNote == 46 || iNote == 77 then
    schedule 3,0,1,giHh2,(iVelo/127),mtof(60)
  elseif iNote == 45 || iNote == 78 then
    schedule 3,0,1,giHh2,(iVelo/127),mtof(60-24)
  ;
  elseif iNote == 51 || iNote == 80 then
    schedule 3,0,1,giSnare1,(iVelo/127),mtof(60)
  elseif iNote == 50 || iNote == 81 then
    schedule 3,0,1,giSnare2,(iVelo/127),mtof(60)
  elseif iNote == 49 || iNote == 82 then
    schedule 3,0,1,giHhRim,(iVelo/127),mtof(60)
  elseif iNote == 48 || iNote == 83 then
    schedule 3,0,1,giHhLong,(iVelo/127),mtof(60)
  endif
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
