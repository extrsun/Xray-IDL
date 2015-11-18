pro get_ca,pplo,pphi,ppd,dim,loc,bflux,ca,ppv0,ppv1,block=block $
	,binrat=binrat,hpsfdim=hpsfdim,offaxis=offaxis
;+
; Calculate the grid of the C statistics with a two-parameter model
;
;*INPUTS:
; pplo, pphi - vectors, lower and upper boundaries of the parameters
; ppd - vector, number of division of the parameter intervals
; dim - dimension of the count image
; loc - location of counts in the image
; bflux - background counts per image bin
; block - size of the image bin (in units of pixels)
; binrat - division of each bin for calculating the model value in the image
; hpsfdim - half dimension of the PSF image for convolution
; offaxis - the offaxis of the source away from the PSPC axis
;
;*OUTPUTS:
; ca - array, C-statistics of the parameter grid 
; ppv0, ppv1 - vectors containing the parameter axis values of grid elements
; 
;*EXAMPLE:
; list_image,ls,xmin,ymin,c,dim,block=block,loc=loc ; to get loc
; get_ca,[0.55,20.],[0.75,45.],[1,1]*20,dim,loc,bflux,ca,p0,p1,block=block
; conf_cont,ca,p0,p1,pxl,pyl,pmin=[0.641,5./0.155175],slev=2.71 ;get limits
;
; written by wqd, 8/5/96
;-
if n_elements(binrat) eq 0 then binrat=1
if n_elements(block) eq 0 then block=10
;----------------------------------------------------------
; get PSF image
if n_elements(hpsfdim) eq 0 then begin
	hpsfdim=120/block
	print,' the half dimension of the psf is ',hpsfdim
endif
psfdim=2*fix(hpsfdim)+1 ; make sure that the psf has an odd dim
get_psfim,psfim,dim=psfdim,block=block,binrat=7,offaxis=offaxis $
	,chlow=chlow,chhigh=chhigh
	;binrat = 7 should be enough for rosat images
;----------------------------------------------------------
; calculate the grid coordinates	
ppv0=pplo(0)+(pphi(0)-pplo(0))/ppd(0)*findgen(ppd(0)+1)
ppv1=pplo(1)+(pphi(1)-pplo(1))/ppd(1)*findgen(ppd(1)+1)
np0=ppd(0)+1
np1=ppd(1)+1
npt=np0*np1
;----------------------------------------------------------
; calculate the parameter values of grid elements
gloc=lindgen(npt)
xx=gloc mod np0
yy=gloc/long(np1)
pp=fltarr(2,npt)
pp(0,*)=ppv0(xx)
pp(1,*)=ppv1(yy)
;pp=index*0.
;index_conv,lindgen(npt),[np0,np1],index
;pp(0,0)=[ppv0(index(0,*)),ppv1(index(1,*))]
;----------------------------------------------------------
; calculate the C statistics in the grid 
tc=n_elements(loc)
nsc=tc-bflux*float(dim)^2
ca=fltarr(np0,np1)

for k=0,npt-1 do begin
 cl_imc,dim,pp(*,k),clim,binrat=binrat,psfim=psfim,psfft=psfft $
	,hpsfdim=hpsfdim,block=block
 so=nsc/total(clim)
 ca(k)=total(alog(bflux+so*clim(loc)))
 print,'cc= ',ca(k)
endfor
 ca=-2.*ca
;----------------------------------------------------------
;stop
return
end
