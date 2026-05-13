opcode def, i, ii  ;; set a value or use the default value in case of 0
  iVal, iDflt xin
  if iVal == 0 then
    iVal = iDflt
  endif
  xout iVal
endop

opcode def, k, kk  ;; set a value or use the default value in case of 0
  kVal, kDflt xin
  if kVal == 0 then
    kVal = kDflt
  endif
  xout kVal
endop

opcode param_or_chn, k, iS
  iParam, SChName xin
  kValue = iParam
  ;; if given argument is greater than zero, then jump away; otherwise, use the value from the channel
  if kValue == 0 then
    kValue chnget SChName
  endif
  xout kValue
endop
  
giTempo = 60
instr Tempo
  ;; needs -t60 option enabled to work
  giTempo = 60/p4
  tempo p4, 60
  turnoff
endin

instr ChnLine
  iDur = p3
  SName = p4
  iStart = p5
  iFinish = p6
  kVal linseg iStart, iDur, iFinish
  chnset kVal, SName
endin

instr ChnSet
  SName = p3
  iValue = p4
  chnset iValue, SName
endin
opcode def, i, ii  ;; set a value or use the default value in case of 0
  iVal, iDflt xin
  if iVal == 0 then
    iVal = iDflt
  endif
  xout iVal
endop

;; MIDI abstractions
#define MIDI_COLOR #0#
#define MIDI_INSTR #1#
#define MIDI_DUR #2#
#define MIDI_GAIN #3#
#define MIDI_FREQ #4#
#define MIDI_PARAM1 #5#
#define MIDI_PARAM2 #6#
#define MIDI_PARAM3 #7#
#define MIDI_LAST #8#

giMidiMap[][] init 100, $MIDI_LAST
opcode MidiMap, 0, iiSiioooo
  iNote, iColor, SInstr, iDur, iGain, iFreq, iParam1, iParam2, iParam3 xin
  iInstrNum = SInstr == "" ? 0 : nstrnum(SInstr)
  giMidiMap setrow fillarray(iColor, iInstrNum, iDur, iGain, iFreq, iParam1, iParam2, iParam3), iNote
endop

massign 1, "MidiHandler"

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
    schedule giMidiMap[iNote][$MIDI_INSTR], 0, giMidiMap[iNote][$MIDI_DUR], iGain, iFreq, giMidiMap[iNote][$MIDI_PARAM1], giMidiMap[iNote][$MIDI_PARAM2], giMidiMap[iNote][$MIDI_PARAM3]
  endif
endin
