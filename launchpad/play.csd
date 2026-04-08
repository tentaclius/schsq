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

instr Sins
  iGain = p4
  iFreq = p5
 
  kEnv xadsr 1, 0.1, 1, 2
  kEnv *= iGain

  iGainsFt Tb 1, 1/3, 1/2, 1/5, 1/4, 1/7, 1/6, 1/9, 1/20, 1/11, 1/12
  $LFO(iGainsFt' 1' iFreq/400)
  $LFO(iGainsFt' 2' iFreq/300)
  $LFO(iGainsFt' 3' iFreq/200)
  $LFO(iGainsFt' 4' iFreq/100)

  aSig Sines 1, 4, iFreq, iGainsFt
  aSigL, aSigR freeverb aSig, aSig, 0.5, 0.4
  aSig *= kEnv

  outall aSig
endin

instr SinsN
  iFreq mtof p4
  iGain = p5 > 0 ? p5 : 1/2
  schedule "Sins", 0, p3, iGain, iFreq
endin

instr SawHf
  iGain = p4
  iFreq = p5

  xtratim 2
  kEnv xadsr 1, 0.11, 1, 2
  kEnv *= iGain
  
  aSig vco2 1, iFreq
  aSigHi mvchpf aSig, iFreq*8, 0.5
  aSigLo vclpf aSig, iFreq, 0.9

  aNoise noise 1/8, 0.9
  aNoise mvchpf aNoise, iFreq*10, 0.9

  aSub poscil 1, iFreq/2

  outall (aSigHi + aSigLo + aNoise + aSub) / 4 * kEnv
endin

instr SawHfN
  iFreq mtof p4
  iGain = p5 > 0 ? p5 : 1/2
  schedule "SawHf", 0, p3, iGain, iFreq
endin

</CsInstruments>
<CsScore>
i "SawHfN" 0 5 48
</CsScore>
</CsoundSynthesizer>
