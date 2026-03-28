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

;; colors we are going to use
#define COLOR_BLANK #0#
#define PAD_L #22#
#define PAD_R #27#
#define INSTR #80#
#define COLOR_SYS #5#

;; midi mapping structure
#define MIDI_COLOR #0#
#define MIDI_INSTR #1#
#define MIDI_DUR #2#
#define MIDI_GAIN #3#
#define MIDI_FREQ #4#
#define MIDI_SAMPLE #5#

giMidiMap[][] init 100, 6 
opcode LaunchpadMap, 0, iiSiiii
  iNote, iColor, SInstr, iDur, iGain, iFreq, iSample xin
  giMidiMap setrow fillarray(iColor, nstrnum(SInstr), iDur, iGain, iFreq, iSample), iNote
endop

instr WrapBD
  iGain = p4
  iFreq = p5
  ignore p6
  schedule "BD", 0, 1, iFreq, iGain
endin

;; system
LaunchpadMap 99, $COLOR_SYS, "RefreshColors", 0.1, 1, 1, 0
;; drums
LaunchpadMap 43, $PAD_L, "Bd", 1, 0.5, 110, 0.1
LaunchpadMap 72, $PAD_R, "Bd", 1, 0.5, 110, 0.1
LaunchpadMap 42, $PAD_L, "WrapBD", 1, 0.5, mtof:i(60), 0
LaunchpadMap 73, $PAD_R, "WrapBD", 1, 0.5, mtof:i(60), 0
LaunchpadMap 41, $PAD_L, "Sample", 1, 0, mtof:i(60), giKick2
LaunchpadMap 74, $PAD_R, "Sample", 1, 0, mtof:i(60), giKick2
LaunchpadMap 40, $PAD_L, "Bdo", 1, 0, 60, 0.23
LaunchpadMap 75, $PAD_R, "Bdo", 1, 0, 60, 0.23
;; hats
LaunchpadMap 47, $PAD_L, "Hh", 1, 1, 5000, 0.1
LaunchpadMap 76, $PAD_R, "Hh", 1, 1, 5000, 0.1
LaunchpadMap 46, $PAD_L, "Sample", 1, 0, mtof:i(60), giHh1
LaunchpadMap 77, $PAD_R, "Sample", 1, 0, mtof:i(60), giHh1
LaunchpadMap 45, $PAD_L, "Sample", 1, 0, mtof:i(36), giHh2
LaunchpadMap 78, $PAD_R, "Sample", 1, 0, mtof:i(36), giHh2
;; cymbals
LaunchpadMap 51, $PAD_L, "Sample", 1, 0, mtof:i(60), giSnare1
LaunchpadMap 80, $PAD_R, "Sample", 1, 0, mtof:i(60), giSnare1
LaunchpadMap 50, $PAD_L, "Sample", 1, 0, mtof:i(60), giSnare2
LaunchpadMap 81, $PAD_R, "Sample", 1, 0, mtof:i(60), giSnare2
LaunchpadMap 49, $PAD_L, "Sample", 1, 0, mtof:i(60), giHhRim
LaunchpadMap 82, $PAD_R, "Sample", 1, 0, mtof:i(60), giHhRim
LaunchpadMap 48, $PAD_L, "Sample", 1, 0, mtof:i(60), giHhLong
LaunchpadMap 83, $PAD_R, "Sample", 1, 0, mtof:i(60), giHhLong
;; pads
LaunchpadMap 36, $INSTR, "PadsR", 3, 0.6, ntom("2A"), 0
LaunchpadMap 37, $INSTR, "PadsR", 3, 0.6, ntom("3C"), 0
LaunchpadMap 38, $INSTR, "PadsR", 3, 0.6, ntom("3D"), 0
LaunchpadMap 39, $INSTR, "PadsR", 3, 0.6, ntom("3E"), 0
LaunchpadMap 68, $INSTR, "PadsR", 3, 0.6, ntom("3G"), 0
LaunchpadMap 69, $INSTR, "PadsR", 3, 0.6, ntom("3A"), 0
LaunchpadMap 70, $INSTR, "PadsR", 3, 0.6, ntom("4C"), 0
LaunchpadMap 71, $INSTR, "PadsR", 3, 0.6, ntom("4D"), 0

massign 0, "MidiHandler"
instr MidiHandler
  iNote notnum
  iVelo veloc
  if giMidiMap[iNote][$MIDI_INSTR] > 0 then
    iGain def giMidiMap[iNote][$MIDI_GAIN], iVelo/127
    iFreq def giMidiMap[iNote][$MIDI_FREQ], mtof:i(iNote)
    schedule giMidiMap[iNote][$MIDI_INSTR], 0, giMidiMap[iNote][$MIDI_DUR], iGain, iFreq, giMidiMap[iNote][$MIDI_SAMPLE]
  endif
endin

;; light up Launchpad keys
instr RefreshColors
  for iColor, iIndex in getcol(giMidiMap, 0) do
    noteon 1, iIndex, iColor
  od
  turnoff
endin

;; play a sample
instr Sample
  iGain = p4
  iFreq = p5
  iTable = p6
  xtratim ftlen(iTable) * 261.626/iFreq / sr
  aSig,aSig2 loscil3 iGain, iFreq, iTable, 261.626, 0
  outall aSig
endin

instr Bd
  iGain def p4, 1
  iFreq def p5, 330
  iDur def p6, 0.1
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

instr Bdo
  iGain def p4, 1
  iFreq def p5, 60
  iDur def p6, 0.23
  ;
  kEnv linseg 0, 0.008, iGain, iDur, 0
  aSig poscil kEnv, iFreq
  ;
  outall aSig
endin

instr Hh
  iGain def p4, 1
  iFreq def p5, 3000
  iDur def p6, 0.1
  ;
  kEnv linseg 0, 0.005, iGain, iDur, 0
  aSig noise kEnv, 0
  aSig mvchpf aSig, iFreq, 0.9
  ;
  outall aSig
endin

;giDistortFn ftgen 0, 0, 257, 9, .5, 1, 270
instr Pads
  iDur = p3
  iGain def p4, 1
  iNote def p5, 60
  ;
  aSig vco2 iGain, mtof(iNote)
  ;aSig wrap aSig, -0.1, 0.7
  aSig mvchpf aSig, mtof:i(iNote)*3, 0.35
  aSig *= linseg(0, 0.1, iGain, iDur-1, iGain, 0.3, 0)
  ;
  aSigL, aSigR freeverb aSig, aSig, 0.75, 0.3
  kEnv madsr 0.05, 0.1, 0.8, 0.2
  out aSig*kEnv, aSig*kEnv
  out aSig, aSig
endin

instr PadsR
  iDur = p3
  iGain def p4, 1/2
  iNote def p5, 60
  ignore p6
  turnoff2 "Pads", 0, 1
  schedule "Pads", 0.001, iDur, iGain, iNote
  turnoff
endin

</CsInstruments>
<CsScore>
i "RefreshColors" 0 1
</CsScore>
</CsoundSynthesizer>
