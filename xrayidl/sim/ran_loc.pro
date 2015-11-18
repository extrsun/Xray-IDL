pro ran_loc,expect,loc,seed,nran=nran
;
; works only for uniform distribution, no use yet
;
if N_params() eq 0 then begin
print,'CALLING SEQUENCE -  ran_loc,expect,loc,seed'
return
endif
;
sz=size(expect)
dimloc=sz(sz(0)+2)
imloc=lindgen(dimloc)
mn=total(expect) ;total expected number of counts

acc=fltarr(dimloc)
for k=1,dimloc-1 do acc(k)=acc(k-1)+expect(k)
acc=acc/mn

; see whether to randomize the total number
if n_elements(nran) ne 0 then rn=(randomn(seed,1)*sqrt(mn)+mn) else rn=mn
	;assuming mn > 10 for using Normal distribution!
if rn le 0. then stop,'the number ',mn,' is too small to use the normal dist!'

rv=randomu(seed,long(rn+0.5))
loc=imloc(long(rv*dimloc))
stop
return
end