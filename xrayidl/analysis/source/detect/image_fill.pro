pro image_fill,cb,tb,cbf,cblimit=cblimit,fvalue=fvalue

sf=where(tb le 0,nsf) 
if n_elements(fvalue) eq 0 then begin
if n_elements(cblimit) eq 0 then cblimit=avg(cb(where(tb gt 0,nss))) > 1
print,'cblimit = ',cblimit
ss=where(tb gt 0 and cb le cblimit,nss) 
loc=long(randomu(seed,nsf)*nss)
cbf=cb
cbf(sf)=cb(ss(loc))
endif else begin
	cbf(sf)=fvalue
endelse 
return
end