;===========================================
function get_mpder,para
common shared,timevc,op,gop,ncomp

sz=size(timevc)
if sz(0) ge 2 then n_band=sz(2) else n_band=1
npara=n_elements(para)
mpder=fltarr(sz(1),npara)
parab=reform(para,ncomp,n_band)

for band=0,n_band-1 do begin
 mpder(*,band*ncomp)=timevc
 mpder(*,band*ncomp+1)=timevc*exp(parab(2,band)*op(*,band)+gop(*,band))
 mpder(*,band*ncomp+2)=timevc*parab(1,band)*exp(parab(2,band)*op(*,band)+gop(*,band))*op(*,band)
endfor
;mcount=reform(mcount,n_elements(mcount))
if !debug eq 1 then stop
return,mpder
end