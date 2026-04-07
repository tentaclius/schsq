<CsoundSynthesizer>
<CsOptions>
-o dac
</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 4
nchnls = 2
0dbfs = 1

#define LFO(tb'pos'fq) #tabw (lfo(tabi($pos, $tb)/3, $fq) + 2*tabi($pos, $tb)/3), $pos, $tb#

opcode Sines, a, iiki
  iNst, iSize, kFreq, iGainFt xin
  aRec init 0
  kGain tab iNst-1, iGainFt
  aSig poscil 1, kFreq*iNst
  aSig = aSig/iSize * kGain
  if iNst < iSize then
    aRec Sines iNst+1, iSize, kFreq, iGainFt
  endif
  xout aSig+aRec
endop

opcode Tb, i, oooooooooooooooooooo
  i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i21,i22,i23,i24,i25,i26,i27,i28,i29,i30 xin
  iFt ftgentmp 0,0,10,2, i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i21,i22,i23,i24,i25,i26,i27,i28,i29,i30
  xout iFt
endop

instr 1
  iGain = p4
  iFreq = p5
 
  kEnv xadsr 1, 0.1, 1, 2
  kEnv *= iGain

  iGainsFt Tb 1, 1/2, 1/3, 1/4, 1/15, 1/6, 1/18, 1/9, 1/20, 1/11, 1/12
  $LFO(iGainsFt' 1' iFreq/400)
  $LFO(iGainsFt' 2' iFreq/300)
  $LFO(iGainsFt' 3' iFreq/200)
  $LFO(iGainsFt' 4' iFreq/100)

  aSig Sines 1, 11, iFreq, iGainsFt
  aSig *= kEnv

  outall aSig
endin

</CsInstruments>
<CsScore>
i1 0 5 0.5 110
</CsScore>
</CsoundSynthesizer>
