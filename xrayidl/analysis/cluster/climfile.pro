pro climfile,dim,pplo,pphi,binrat=binrat,hpsfdim=hpsfdim,npp=npp,block=block $
,offaxis=offaxis,outfile=outfile

if n_elements(outfile) eq 0 then outfile='clima.dat'
if n_elements(binrat) eq 0 then binrat=1
if n_elements(hpsfdim) eq 0 then hpsfdim=8
if n_elements(hpsfdim) eq 0 then begin
	hpsfdim=120/block
	print,' the half dimension of the psf is ',hpsfdim
endif
psfdim=2*fix(hpsfdim)+1 ; make sure that the psf has an odd dim
get_psfim,psfim,dim=psfdim,block=block,binrat=7,offaxis=offaxis $
	,chlow=chlow,chhigh=chhigh
	;binrat = 7 should be enough for rosat images
	
ppv0=pplo(0)+(pphi(0)-pplo(0))/npp(0)*findgen(npp(0)+1)
ppv1=pplo(1)+(pphi(1)-pplo(1))/npp(1)*findgen(npp(1)+1)
np0=npp(0)+1
np1=npp(1)+1
npt=np0*np1
index_conv,lindgen(npt),[np0,np1],index
pp=index*0.
pp(0,0)=[ppv0(index(0,*)),ppv1(index(1,*))]
hdim=dim/2
climt=fltarr(hdim,hdim,npt)

for k=0,npt-1 do begin
cl_imc,dim,pp(*,k),clim,binrat=binrat,psfim=psfim,psfft=psfft,hpsfdim=hpsfdim
climt(*,*,k)=clim(0:hdim-1,0:hdim-1)
endfor

tim=total(total(climt,1),1)
openw,un,outfile,/get_lun
writeu,un,hdim,np0,np1
writeu,un,ppv0,ppv1,climt,tim
free_lun,un
return
end
;======================================
pro cl_imc,dim,para,clim,binrat=binrat,psfim=psfim,psfft=psfft,hpsfdim=hpsfdim
;+
; calculate a PSF convolved cluster model image
;-
if n_params() eq 0 then begin 
 print,'cl_imc,dim,para,clim,binrat=binrat,psfim=psfim,psfft=psfft'
 print,',hpsfdim=hpsfdim'
 return
endif
if n_elements(binrat) eq 0 then binrat=1
 calcl,dim+2*hpsfdim,para(1),clim,binrat=binrat,modelp=para,chatt=0
if !debug eq 2 then stop,'before convolution'
if hpsfdim ne 0 then begin
	clim=convlv(clim,psfim,psfft=psfft)
if !debug eq 2 then stop
	if n_elements(hpsfdim) ne 0 then $
		clim=clim(hpsfdim:dim-1+hpsfdim,hpsfdim:dim-1+hpsfdim)
endif
;if !debug eq 2 then stop,'at the buttom of cl_imc'
return
end
;==================================
;+
; program called by calcl.pro
;-
pro cal_model,xx,wlloc,modelp=modelp
modelp(0)=modelp(0) 
wlloc=(1.+xx^2)^(-3.*modelp(0)+0.5)
return
end
;======================================