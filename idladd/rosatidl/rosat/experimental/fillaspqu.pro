pro fillaspqu,sctime,sctqual,qualflag,starflag,qualpl,starpl
;
; procedure to fill in quality and star tracker flags for plotting
; plotting vectors will have same numbers of elements as sctime
; it's a bit tricky, as qualflag seems to give the aspect quality flags
; for the time period *before* sctqual
; I *think* this is true for starflag as well
;
ntime = n_elements(sctime)       ;number of aspect meas within obi
nqual = n_elements(sctqual)      ;number of aspect quality meas within obi
qualpl = bytarr(ntime)
starpl = qualpl
;
tabinv,sctime,sctqual,iqual
iqb = iqual(0) + 0.5
;
for jj=1,nqual-1 do begin        ;by construction, nqual >= 2 always
  iqj = iqual(jj) + 0.5 
  if (iqj ge iqb) then begin
    qualpl(iqb) = qualflag(jj) + bytarr(iqj-iqb+1)
    starpl(iqb) = starflag(jj) + bytarr(iqj-iqb+1)
  endif
  iqb = iqj+1
endfor
;
return
end           ;pro fillaspqu
