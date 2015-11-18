pro ad_bin,oev,ebin,em,elo,ehi,nd=nd,emin=emin,emax=emax
;+
; Program:
; oev - vector of source luminosities
; ebin - output adaptively binned numbers
; em - mean luminosity value of the bins
; nd - number of divisions of the luminosity range (def =10)
; emin, emax - lower and upper boundaries of the luminosity ranges
;              (def = the lowest and highest luminosity values)
;                
; written by wqd, Sept, 13, 2003
;-
if n_params() lt 2 then begin
   print,'Calling Seq. - ad_bin,oev,ebin,em,elo,ehi,nd=nd,emin=emin,emax=emax'
   return
endif
ev=oev(sort(oev))
if n_elements(nd) eq 0 then nd=10
nev=n_elements(ev)
if n_elements(emin) eq 0 then emin=ev(0)
if n_elements(emax) eq 0 then emax=ev(nev-1) else begin
    if ev(nev-1) gt emax then begin
        ss=where(ev le emax,nev)
        if nev ne 0 then ev=ev(ss) else $
          stop,'no count within the specified range!'
    endif 
endelse
cth=nev/fix(nd)
eind=(lindgen(nd-1)+1)*cth
elo=(ev(eind)+ev(eind+1))*0.5
elo=[emin,elo]
ehi=[elo(1:*),emax]
eind=[0,eind,nev-1]
em=fltarr(nd)
for k=0,nd-1 do em(k)=avg(ev(eind(k:k+1)))
ebin=[replicate(cth,nd-1),nev-(nd-1)*cth]
if !debug eq 1 then stop
return
end
