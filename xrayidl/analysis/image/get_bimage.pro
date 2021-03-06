pro get_bimage,cb,tb,tbs,dim=dim,slow=slow,e1l=e1l,e1h=e1h,e2l=e2l,e2h=e2h,exptail=exptail,block=block,factor=factorr,soufile=soufile,radius=radius $
,perclimit=perclimit,cntrlimit=cntrlimit,cra=cra,cdec=cdec
;- 
; get composite images from multiple bands with proper weighted exposure maps
;+
np=n_params()
if np eq 0 then begin
print,'CALLING SEQUENCE -- get_bimage,cb,tb,tbs,dim=dim,slow=slow,e1l=e1l,e1h=e1h,e2l=e2l,e2h=e2h,exptail=exptail,block=block,factor=factorr,soufile=soufile'
print,',radius=radius,perclimit=perclimit,cntrlimit=cntrlimit,cra=cra,cdec=cdec'
return
endif
if n_elements(radius) eq 0 then begin
	radius=60 
	print,'radius = ',radius, ' bins'
endif
if n_elements(dim) eq 0 then dim=512
if n_elements(slow) eq 0 then slow=3.
if n_elements(e1l) eq 0 then e1l=4
if n_elements(e1h) eq 0 then e1h=e1l+1
if n_elements(e2l) eq 0 then e2l=e1h+1
if n_elements(e2h) eq 0 then e2h=e2l+1
if n_elements(exptail) eq 0 then exptail=''
;!data_dir='/data1/wqd/archives/'+!seq_no+'/'
psffile='~/rosatdata/source/'+ $
	'psf_pw_2_0.03_'+strtrim(e1l,2)+strtrim(e2h,2)+'.dat'
	; so the source subtraction is the same in the two exposure maps
if np lt 3 then begin
get_image,t1,c1,dim=dim,slow=slow,blow=e1l,bhigh=e1h,exptail=exptail,block=block,cra=cra,cdec=cdec
get_image,t2,c2,dim=dim,slow=slow,blow=e2l,bhigh=e2h,exptail=exptail,block=block,cra=cra,cdec=cdec
endif else begin
get_image,t1,c1,dim=dim,slow=slow,blow=e1l,bhigh=e1h,exptail=exptail,ts $
,block=block,factor=factor,soufile=soufile,psffile=psffile $
,perclimit=perclimit,cntrlimit=cntrlimit,cra=cra,cdec=cdec,/rec
get_image,t2,c2,dim=dim,slow=slow,blow=e2l,bhigh=e2h,exptail=exptail,ts $
,block=block,factor=factorr,soufile=soufile,psffile=psffile $
,perclimit=perclimit,cntrlimit=cntrlimit,cra=cra,cdec=cdec,/rec
endelse
t=fltarr(dim,dim,2)
c=intarr(dim,dim,2)
t(*,*,0)=t1
t(*,*,1)=t2
c(*,*,0)=c1
c(*,*,1)=c2
cb=c1+c2
if np lt 3 then ts=t1 
exp_broad,radius,c,t,ts,tb

if np eq 3 then begin
	sel=where(ts le 0.)
	tbs=tb
	tbs(sel)=ts(sel)
endif
end

