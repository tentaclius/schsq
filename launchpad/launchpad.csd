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
#include "livecode.orc"

;; helper opcode
opcode def, i, ii  ;; set a value, use default value in case of 0
  iVal, iDflt xin
  if iVal == 0 then
    iVal = iDflt
  endif
  xout iVal
endop

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

#define COLOR_BLANK #0#
#define PAD_L #22#
#define PAD_R #27#
#define INSTR #80#

#define MIDI_COLOR #0#
#define MIDI_INSTR #1#
#define MIDI_DUR #2#
#define MIDI_GAIN #3#
#define MIDI_FREQ #4#
#define MIDI_SAMPLE #5#

giMidiMap[][] init 100, 6
;; color, instr, dur, gain, freq, sample
giMidiMap setrow fillarray(PAD_L, nstrnum("Bd"), 1, .5, 110, 0), 43
giMidiMap setrow fillarray(PAD_R, nstrnum("Bd"), 1, .5, 110, 0), 72
giMidiMap setrow fillarray(PAD_L, 3, 1, 0, mtof(60), giKick2), 42
giMidiMap setrow fillarray(PAD_R, 3, 1, 0, mtof(60), giKick2), 73
giMidiMap setrow fillarray(PAD_L, 3, 1, 0, mtof(36), giKick2), 41
giMidiMap setrow fillarray(PAD_R, 3, 1, 0, mtof(36), giKick2), 74
giMidiMap setrow fillarray(PAD_L, nstrnum("Hh"), .05, 1, 5000, 0), 47
giMidiMap setrow fillarray(PAD_R, nstrnum("Hh"), .05, 1, 5000, 0), 76

instr 101 ;; will replace instr 1
  iNote notnum
  iVelo veloc
  if giMidiMap[iNote][MIDI_INSTR] > 0 then
    iGain def giMidiMap[iNote][MIDI_GAIN], iVelo/127
    iFreq def giMidiMap[iNote][MIDI_FREQ], mtof(iNote)
    schedule giMidiMap[iNote][MIDI_INSTR], 0, giMidiMap[iNote][MIDI_DUR], iGain, iFreq, giMidiMap[iNote][MIDI_SAMPLE]
  endif
endin

;; MIDI dispatcher
giColor = 45
massign 0, 1
instr 1
  iNote notnum
  iVelo veloc
  ; bass drums
  if iNote == 43 || iNote == 72 then
    schedule "Bd",0,1,.05125,1,110
  elseif iNote == 42 || iNote == 73 then
    schedule 3,0,1,giKick2,(iVelo/127),mtof(60)
  elseif iNote == 41 || iNote == 74 then
    schedule 3,0,1,giKick2,(iVelo/127),mtof(60-24)
  elseif iNote == 40 || iNote == 75 then
    schedule 3,0,1,giKick2,(iVelo/127),mtof(60+12)
  ; high hats
  elseif iNote == 47 || iNote == 76 then
    schedule "Hh", 0, 1, .05, 1, 5000
  elseif iNote == 46 || iNote == 77 then
    schedule 3,0,1,giHh1,(iVelo/127),mtof(60)
  elseif iNote == 45 || iNote == 78 then
    schedule 3,0,1,giHh2,(iVelo/127),mtof(60-24)
  ; cymbals
  elseif iNote == 51 || iNote == 80 then
    schedule 3,0,1,giSnare1,(iVelo/127),mtof(60)
  elseif iNote == 50 || iNote == 81 then
    schedule 3,0,1,giSnare2,(iVelo/127),mtof(60)
  elseif iNote == 49 || iNote == 82 then
    schedule 3,0,1,giHhRim,(iVelo/127),mtof(60)
  elseif iNote == 48 || iNote == 83 then
    schedule 3,0,1,giHhLong,(iVelo/127),mtof(60)
  ; pads
  elseif iNote == 36 then
    schedule "PadsR", 0, 3, 0.6, ntom("2A")
  elseif iNote == 37 then
    schedule "PadsR", 0, 3, 0.6, ntom("3C")
  elseif iNote == 38 then
    schedule "PadsR", 0, 3, 0.6, ntom("3D")
  elseif iNote == 39 then
    schedule "PadsR", 0, 3, 0.6, ntom("3E")
  elseif iNote == 68 then
    schedule "PadsR", 0, 3, 0.6, ntom("3G")
  elseif iNote == 69 then
    schedule "PadsR", 0, 3, 0.6, ntom("3A")
  elseif iNote == 70 then
    schedule "PadsR", 0, 3, 0.6, ntom("4C")
  elseif iNote == 71 then
    schedule "PadsR", 0, 3, 0.6, ntom("4D")
  endif
endin

;; light up Launchpad keys
instr 2
#include "colors.orc"
endin

instr 102 ;; will replace instr 2
  for iColor, iIndex in getcol(giMidiMap, 0) do
    noteon 1, iIndex, iColor
  od
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

instr Bd
  iGain def p5, 1
  iFreq def p6, 330
  iDur def p4, 0.1
  ;
  kEnv linseg iGain, iDur*3, 0
  kFreq linseg iFreq, iDur, 10
  aSig poscil 1, kFreq
  ;
  aBass poscil 0.7 * iGain, 60
  ;
  aSig = (aSig + aBass) * kEnv
  outall aSig
endin

instr Hh
  iDur = p3
  iGain def p4, 1
  iFreq def p5, 3000
  ;
  kEnv linseg 0, 0.005, iGain, iDur, 0
  aSig noise kEnv, 0
  aSig mvchpf aSig, iFreq, 0.9
  ;
  outall aSig
endin

instr Pads
  iDur = p3
  iGain def p4, 1
  iNote def p5, 60
  ;
  aSig poscil 1/2, mtof(iNote)
  aSig += poscil(1/4, mtof(iNote + 7)-1)
  aSig += poscil(1/7, mtof(iNote + 12)+1)
  aSig += poscil(1/20, mtof(iNote + 31))
  aSig *= linseg(0, 0.1, iGain, iDur-1, iGain, 0.3, 0)
  ;
  aSigL, aSigR freeverb aSig, aSig, 0.75, 0.3
  kEnv madsr 0.05, 0.1, 0.8, 0.2
  out aSigL*kEnv, aSigR*kEnv
endin

instr PadsR
  iDur = p3
  iGain def p4, 1/2
  iNote def p5, 60
  turnoff2 "Pads", 0, 1
  schedule "Pads", 0.001, iDur, iGain, iNote
  turnoff
endin

</CsInstruments>
<CsScore>
i2 0 1
</CsScore>
</CsoundSynthesizer>
