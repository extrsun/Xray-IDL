pro psf_im,distp,psfp,psfim,block=block,rlimit=rlimit,xc=xc,yc=yc,chlow=chlow,chhigh=chhigh,spfil=spfil,binrat=binrat
;
;+
; xc, yc - sass pixel position of the source for calculating offaxis angle
; in make_psf.pro
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -  fit_rbp,dist,rbp,rbperr,block=block,rlimit=rlimit'
print,',xc=xc,yc=yc,chlow=chlow,chhigh=chhigh,spfil=spfil,yfit=yfit,psfim=psfim,binrat=binrat''
return
endif
if n_elements(rlimit) eq 0 then rlimit=180.
if n_elements(block) eq 0 then block=1
if n_elements(chlow) eq 0 then chlow=12
if n_elements(chhigh) eq 0 then chhigh=29
if n_elements(binrat) eq 0 then binrat=5
dim=2.*rlimit/(block*0.5)+1
;
emin=!group(chlow,0) 
emax=!group(chhigh,1)
print,'emin, emax = ', emin, emax
if n_elements(spfil) eq 0 then begin
	spfil='~/spectrum/po17.dat'
	print,' get model spectral rate and group...'
 	modelrate,spfil,chlow,chhigh,rate
	group=!group(chlow:chhigh,*)
endif
;

inputs='instr='+!instr+',sptyp=PHA,imgfil=NONE,proftab=NONE'+ $
	',block='+strtrim(block,2)+',binsiz='+strtrim(block*0.5)+',binrat='+ $
	strtrim(binrat,2)+',emin='+strtrim(emin,2)+',emax='+strtrim(emax,2) + $
	',pixel=BL'+',nbins='+strtrim(dim,2)+',spfil=NONE';+spfil+
if n_elements(xc) ne 0 then begin
 case !instr of
   'h' : if xc gt 4096+2400 or xc lt 1696 or yc gt 4096+2400 or yc lt 1696 $
	then 		stop,'the position is not right'
   'p' : if xc gt 7680+7200 or xc lt 480 or yc gt 7680+7200 or yc lt 480 then $
		stop,'the position is not right'
 endcase
 inputs=inputs+',xcen='+strtrim(xc/block,2)+',ycen='+strtrim(yc/block,2)
endif

make_psf,inputs,offang,prof,psfim,rate=rate,group=group
;
if !debug eq 1 then stop,'before rbp'
rbp,psfim,distp,psfp,block=block,radiushi=rlimit,/plotoff
;
stop
return
end