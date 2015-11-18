pro get_bflux,fb,dim=dim,slow=slow,e1l=e1l,e1h=e1h,e2l=e2l,e2h=e2h,exptail=exptail,block=block
;- 
; get a broad band flux map
;+
np=n_params()
if np eq 0 then begin
print,'CALLING SEQUENCE -- get_bflux,fb,dim=dim,slow=slow'
print,',e1l=e1l,e1h=e1h,e2l=e2l,e2h=e2h,exptail=exptail,block=block'
return
endif
if n_elements(dim) eq 0 then dim=512
if n_elements(e1l) eq 0 then e1l=4
if n_elements(e1h) eq 0 then e1h=e1l+1
if n_elements(e2l) eq 0 then e2l=e1h+1
if n_elements(e2h) eq 0 then e2h=e2l+1
if n_elements(exptail) eq 0 then exptail=''
get_image,t1,c1,dim=dim,slow=slow,blow=e1l,bhigh=e1h,exptail=exptail,image_f=f1 $
,block=block,factor=factor,soufile=soufile,psffile=psffile $
,perclimit=perclimit,cntrlimit=cntrlimit,cra=cra,cdec=cdec,/rec
get_image,t2,c2,dim=dim,slow=slow,blow=e2l,bhigh=e2h,exptail=exptail,image_f=f2 $
,block=block,factor=factorr,soufile=soufile,psffile=psffile $
,perclimit=perclimit,cntrlimit=cntrlimit,cra=cra,cdec=cdec,/rec
fb=f1+f2
end

