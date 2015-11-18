pro galactc_fit,crad,cdecd,im,imout,pixexc=pixexc,block=block $
,imin=imin,imax=imax
;-
; calculate a latitude-dependent galactic background in an IRAS image
; INPUTS: 
; im - array
; pixexc - index of the bins in the array to be excluded in the fit
; imin,imax - the minimum and maxmum values of the image bins to be 
; 		included in the fit
; crad, cdecd - ra and dec of the image center in units of degree
; OUTPUTS:
; imout  - the output image containing the calculated background
; Subroutines called :
; glactc_image.pro
;
; writen by WQD, 5/20/93
;+
; 
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - galactc_fit,crad,cdecd,im,imout,pixexc=pixexc'
print,',block=block,imin=imin,imax=imax'
return
endif
if n_elements(imin) eq 0 then imin=0.
imnew=im
if n_elements(pixexc) ne 0 then imnew(pixexc)=imin-1. 
;these bins will be excluded
glactc_image,imnew,crad,cdecd,lodeg,ladeg,binval,nsel,imin=imin,imax=imax,block=block
larad=ladeg*(!pi/180.)
;
; get mean value
mvalue=total(binval)/float(nsel)
print,'mean bin value = ',mvalue
	ncomp=2
	w=1./(binval > 0.1*mvalue) ;arbitrarily assumed value
	func=fltarr(nsel,ncomp)
  	func(*,0)=1.
  	func(*,1)=1./sin(abs(larad))
  	coef=funcfitw(larad,binval,w,func,ncomp,yfit,yband,sigma,var)
;
  	for kk=0,(ncomp-1) do begin
		print,'coef = ', coef(kk),' +- ',sqrt(var(kk,kk))
  	endfor
  	ndf=(nsel-ncomp)
  	chi=sigma*sigma*(nsel-ncomp)
 	print,'chi = ', chi,(nsel-ncomp)
;
glactc_image,im,crad,cdecd,lodeg,ladeg,imin=-1.e20,block=block
larad=ladeg*(!pi/180.)
imout=im*0.
imout(*,*)=coef(0)+coef(1)/sin(abs(larad))
stop
end
