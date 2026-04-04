<CsoundSynthesizer>
<CsOptions>
  -o dac
  -+rtaudio=jack
  -+rtmidi=alsaseq
  -Ma -Qa
  --port=1234
  -L stdin
  -b 16
  -B 272
</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 4
0dbfs = 1
nchnls = 3

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
; looper colors
#define LOOPER_RECORD #5#
#define LOOPER_SELECT #40#
#define LOOPER_SELECTED #45#
#define LOOPER_OVER #6#
#define LOOPER_UNDO #19#
#define LOOPER_MUTE #12#

giMidiMap[][] init 100, 6 
opcode LaunchpadMap, 0, iiSiiii
  iNote, iColor, SInstr, iDur, iGain, iFreq, iSample xin
  iInstrNum = SInstr == "" ? 0 : nstrnum(SInstr)
  giMidiMap setrow fillarray(iColor, iInstrNum, iDur, iGain, iFreq, iSample), iNote
endop

;; looper
LaunchpadMap 64, $LOOPER_RECORD, "", 1, 1, 1, 0 ; record
LaunchpadMap 65, $LOOPER_OVER, "", 1, 1, 1, 0 ; overdub
LaunchpadMap 66, $LOOPER_UNDO, "", 1, 1, 1, 0 ; undo
LaunchpadMap 67, $LOOPER_UNDO, "", 1, 1, 1, 0 ; redo
LaunchpadMap 96, $LOOPER_MUTE, "", 1, 1, 1, 0 ; mute
LaunchpadMap 97, $LOOPER_MUTE, "", 1, 1, 1, 0 ; solo
LaunchpadMap 98, $LOOPER_MUTE, "", 1, 1, 1, 0 ; once
LaunchpadMap 60, $LOOPER_SELECT, "SelectSLTrack", 0.1, 60, 0, 0 ; select track 1
LaunchpadMap 61, $LOOPER_SELECT, "SelectSLTrack", 0.1, 61, 1, 0 ; select track 2
LaunchpadMap 62, $LOOPER_SELECT, "SelectSLTrack", 0.1, 62, 1, 0 ; select track 3
LaunchpadMap 63, $LOOPER_SELECT, "SelectSLTrack", 0.1, 63, 1, 0 ; select track 4
LaunchpadMap 92, $LOOPER_SELECT, "SelectSLTrack", 0.1, 92, 1, 0 ; select track 5
LaunchpadMap 93, $LOOPER_SELECT, "SelectSLTrack", 0.1, 93, 1, 0 ; select track 6
LaunchpadMap 95, $LOOPER_MUTE, "SelectSLTrack", 0.1, 0, 1, 0 ; select all tracks
;; system
LaunchpadMap 71, $COLOR_SYS, "RefreshColors", 0.1, 1, 1, 0
;; drums
LaunchpadMap 43, $PAD_L, "Bd", 1, 0.5, 110, 0.1
LaunchpadMap 72, $PAD_R, "Bd", 1, 0.5, 110, 0.1
LaunchpadMap 42, $PAD_L, "Sample", 1, 0, mtof:i(60), giKick2
LaunchpadMap 73, $PAD_R, "Sample", 1, 0, mtof:i(60), giKick2
;; hats
LaunchpadMap 47, $PAD_L, "Hh", 1, 1, 5000, 0.1
LaunchpadMap 76, $PAD_R, "Hh", 1, 1, 5000, 0.1
LaunchpadMap 46, $PAD_L, "Sample", 1, 0, mtof:i(60), giHh1
LaunchpadMap 77, $PAD_R, "Sample", 1, 0, mtof:i(60), giHh1
LaunchpadMap 45, $PAD_L, "Sample", 1, 0, mtof:i(36), giHh2
LaunchpadMap 78, $PAD_R, "Sample", 1, 0, mtof:i(36), giHh2
;; cymbals
LaunchpadMap 51, $PAD_L, "Sample", 1, -1, mtof:i(60), giSnare1
LaunchpadMap 80, $PAD_R, "Sample", 1, -1, mtof:i(60), giSnare1
LaunchpadMap 50, $PAD_L, "Sample", 1, -8, mtof:i(60), giSnare2
LaunchpadMap 81, $PAD_R, "Sample", 1, -8, mtof:i(60), giSnare2
LaunchpadMap 49, $PAD_L, "Sample", 1, -8, mtof:i(60), giHhRim
LaunchpadMap 82, $PAD_R, "Sample", 1, -8, mtof:i(60), giHhRim
LaunchpadMap 48, $PAD_L, "Sample", 1, -10, mtof:i(60), giHhLong
LaunchpadMap 83, $PAD_R, "Sample", 1, -10, mtof:i(60), giHhLong
;; toms
LaunchpadMap 54, $PAD_L, "LiveCodeInstrWrapper", 1, 0, 0.7, nstrnum("LowTom")
LaunchpadMap 55, $PAD_L, "LiveCodeInstrWrapper", 1, 0, 0.7, nstrnum("MidTom")
LaunchpadMap 84, $PAD_R, "LiveCodeInstrWrapper", 1, 0, 0.7, nstrnum("HiTom")
;; pads
LaunchpadMap 36, $INSTR, "PadsR", 3, 0, ntom("2A"), 0
LaunchpadMap 37, $INSTR, "PadsR", 3, 0, ntom("3C"), 0
LaunchpadMap 38, $INSTR, "PadsR", 3, 0, ntom("3D"), 0
LaunchpadMap 39, $INSTR, "PadsR", 3, 0, ntom("3E"), 0
LaunchpadMap 68, $INSTR, "PadsR", 3, 0, ntom("3G"), 0
LaunchpadMap 69, $INSTR, "PadsR", 3, 0, ntom("3A"), 0
LaunchpadMap 70, $INSTR, "PadsR", 3, 0, ntom("4C"), 0

massign 1, "MidiHandler"
massign 2, "PadsM"

instr MidiHandler
  iNote notnum
  iVelo veloc
  ;
  if giMidiMap[iNote][$MIDI_INSTR] > 0 then
    iGain = giMidiMap[iNote][$MIDI_GAIN]
    if iGain <= 0 then
      iGain = sqrt(iVelo/127) * ampdb(iGain)
    endif
    iFreq def giMidiMap[iNote][$MIDI_FREQ], mtof:i(iNote)
    schedule giMidiMap[iNote][$MIDI_INSTR], 0, giMidiMap[iNote][$MIDI_DUR], iGain, iFreq, giMidiMap[iNote][$MIDI_SAMPLE]
  endif
endin

;; light up Launchpad keys
instr RefreshColors
  ignore p4
  ignore p5
  ignore p6
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
  ;
  xtratim ftlen(iTable) * 261.626/iFreq / sr
  aSig,aSig2 loscil3 iGain, iFreq, iTable, 261.626, 0
  ;
  out aSig, aSig
endin

;; Synthesized Instruments
;; =================================================================================

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
  out aSig, aSig
endin

instr Hh
  iGain def p4, 1
  iFreq def p5, 3000
  iDur def p6, 0.1
  ;
  kEnv linseg iGain, iDur, 0
  aSig noise kEnv, 0
  aSig mvchpf aSig, iFreq, 0.9
  ;
  out aSig, aSig
endin

giChoosenSLTrack = 0
instr SelectSLTrack
  iIgnore = p5 + p6
  noteon 1, giChoosenSLTrack, $LOOPER_SELECT
  giChoosenSLTrack = p4
  if giChoosenSLTrack != 0 then
    noteon 1, giChoosenSLTrack, $LOOPER_SELECTED
  endif
endin

;giDistortFn ftgen 0, 0, 257, 9, .5, 1, 270
instr Pads
  iGain = p4
  iFreq mtof p5
  ;
  kEnv madsr 0.02, 0.07, 0.7, 0.3
  kEnv *= iGain
  aSig vco2 kEnv, iFreq
  aSig vclpf aSig, kEnv*iFreq*5+100, 0.35
  ;
  out aSig, aSig
endin

instr PadsR  ;; a wrapper stopping the previous instance
  iInstr = nstrnum("VoxHumana")
  iDur = p3
  iGain def p4, 1/2
  iNote def p5, 60
  ignore p6
  ;
  iFreq mtof iNote
  turnoff2 iInstr, 0, 1
  schedule iInstr, 0.0001, iDur, mtof(iNote), iGain
  turnoff
endin

instr PadsM  ;; for playing with MIDI keyboard
  iGain = veloc()/127
  iFreq = mtof(notnum())
  ;
  kEnv madsr 0.02, 0.1, 0.7, 0.3
  kEnv *= iGain
  aSig vco2 kEnv, iFreq
  kFilterEnv = kEnv * iFreq * 5 + 200
  aSig vclpf aSig, kFilterEnv, 0.55
  ;
  out aSig, aSig
  xtratim 0.5
endin

instr SinPads
  xtratim 0.4
  iGain def p4, 1/2
  iFreq def p5, 440
  ignore p6
  ;
  kEnv madsr 0.1, 0.2, 0.45, 0.4
  kEnv *= iGain
  aSig poscil kEnv * 0.5, iFreq
  aSig1 poscil kEnv * 0.3, iFreq * 1.5 - 1
  aSig2 poscil kEnv * 0.2, iFreq * 2 + 1
  aSig3 poscil kEnv * 0.1, iFreq * 3 + 2
  aSig = aSig + aSig1 + aSig2 + aSig3
  ;
  outall aSig
endin

instr LiveCodeInstrWrapper
  iDur = p3
  iGain = p4
  iFreq = p5
  iInstr = p6
  print iGain
  print iFreq
  schedule iInstr, 0, p3, iGain, iFreq
endin

</CsInstruments>
<CsScore>
i "RefreshColors" 0 1
</CsScore>
</CsoundSynthesizer>
