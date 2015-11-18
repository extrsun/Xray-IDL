;===========================================
function get_mcount,para
common shared,timevc,op,gop,ncomp

sz=size(timevc)
if sz(0) ge 2 then n_band=sz(2) else n_band=1
mcount=double(timevc)
npara=n_elements(para)
parab=reform(para,ncomp,n_band)

for band=0,n_band-1 do begin
  mcount(*,band)=timevc(*,band)*(parab(0,band) $
	+parab(1,band)*exp(parab(2,band)*op(*,band)+gop(*,band)))
endfor
mcount=reform(mcount,n_elements(mcount))
;stop
return,mcount
end
