pro get_msolar,count,back,expt,sk,skerr,chi2,ndof $
,countmin=countmin,sfixnum=sfixnum,sfixval=sfixval
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME:
; get_msolar
;
;*PURPOSE:
; Calculate solar X-ray flux in individual images. 
;
;*CALLING SEQUENCE:
; get_msolar,count,back,expt,sk,skerr,chi2,ndof,imax=imax,sminset=sminset,countmin=countmin
;
;*PARAMETERS:
; INPUTS:
; count,back,expt - two dimensional array containing count, background 
;		(in units of counts), and exposure in individual images
; 		covering  overlapped regions
; countmin - used for selecting bins with the expected counts larger than
;		this value (def = 6)
;
;*OUTPUTS:
; sk - vector including solar X-ray fluxes (in units of count/expt) 
;		in each of images
; skerr - estimated statistical errors associated with sk
; chi2,ndof - the chi^2 and the number degrees of freedom of the fit
; 		compared with the data.
;
;*PROCEDURE:
; The count, back, expt need to be produced by solar_mult
;
; Assumption: the solar X-rays (or any other time varying enhancements)
; have a flat distribution in each image.
;
; Use an iteraction approach to get relative solar fluxes in individual images
; First, assuming solar fluxes to be zero in all images, the procedure 
; calculates (using least square fits) X-ray flux in each bin, then uses this `; flux as the model to estimate the solar fluxes in all image, and next goes 
; to the next round, using these new estimates of solar fluxes, calculating
; X-ray fluxes. This iteration goes on until the threshold is reached.
; At the end of each round, the solar fluxes are shifted so that the 
; minimum solar flux equals to zero. The statistical error associated
; with a flux is estimated with the couting error of the difference between
; the overlapping count rates of the image and the total of the other images
;
;*EXAMPLES:
;
;*RESTRICTIONS:
; Each image is supposed to be overlapped partially with at least one 
; other image.
;
;*NOTES:
;
;*SUBROUTINES CALLED:
; none
;
;*MODIFICATION HISTORY:
; writen by WQD, 3/18/93
; use the model fluxes as the weight and include an estimate of the flux
; errors,  WQD, 3/24/93
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_msolar,count,back,expt,sk,skerr,chi2,ndof'
print,',countmin=countmin,sfixnum=sfixnum,sfixval=sfixval'
return
endif
;
 imax=50
if n_elements(countmin) eq 0 then countmin=6.
;
sel=where(expt eq 0.,nsel)
if nsel ne 0 then begin
	back(sel)=0.
	count(sel)=0
endif 

sel=where(expt gt 0.,nsel)
; calculate the threshold
;
fluxm=total(count(sel)-back(sel))/total(expt(sel))
print,'the mean flux = ',fluxm,' is used as sminset'
sminset=0.0000001
sminset=sminset*fluxm

sz=size(count)
ndof=nsel-sz(1)-sz(2)
nsfix=n_elements(sfixnum)
if nsfix ne 0 then ndof=ndof+nsfix
;
if n_elements(sk) ne sz(2) then sk=fltarr(sz(2))
osk=sk
mcount=fltarr(sz(1),sz(2))
mcount(sel)=float(count(sel) > countmin) ;to contain total model counts
mcountx=fltarr(sz(1),sz(2)) ; to contain model true x-ray counts
mcounts=mcountx ; to contain model solar x-ray counts
xcount=fltarr(sz(1),sz(2)) 
xcount=count-back ;net total X-ray counts
answer='y'
ck=fltarr(sz(2))
en=fltarr(sz(1))
; 
; start of the interation
;
for i=0,imax do begin
;
; n and k represent the bin and image numbers
; notation as in my notes
;
	wnk=imdiv(expt,count > countmin)
	bnk=expt*wnk
	ak=total(xcount*wnk,1)
	ck=total(bnk,1)
	if where(ck eq 0.) ne -1 then $
		stop,'One image is not overlapped'
	ck=1./ck
	dn=total(xcount*wnk,2)
	en=total(bnk,2) 
	cen=where(en gt 0.)
	en(cen)=1./en(cen)

; get X-ray flux in each bin
	fn=(dn-bnk#sk)*en

	; get solar fluxes in images
	sk=(ak-fn#bnk)*ck

	smin=min(sk,sminloc)
;	print,'minmum solar flux = ',smin
	; shift the absolute values of the fluxes
	fn=fn+smin
	sk=sk-smin
	if nsfix ne 0 then sk(sfixnum)=sfixval
	; get the model fluxes
	for n=0,sz(1)-1 do mcountx(n,*)=fn(n)*expt(n,*)
	for k=0,sz(2)-1 do mcounts(*,k)=sk(k)*expt(*,k)
	mcount=mcountx+mcounts+back 

	; check wether the threshold has been reached
	if abs(total(sk-osk)) lt sminset then goto,done 
	osk=sk

	if i eq imax then begin
		print,'want more iteration?'
		read,answer
		yesno,answer
		if answer eq 1 then i=0
		answer=''
	endif
endfor

done: 
; error estimates of the solar fluxes
skerr=sk*0.
for i=0,sz(2)-1 do begin
	c=where(expt(*,i) ne 0)
	nc1=total(xcount(c,i))
	t1=total(expt(c,i))
	nc2=total(xcount(c,*))-nc1
	t2=total(expt(c,*))-t1
	skerr(i)=sqrt(nc1/(t1*t1)+nc2/(t2*t2))
endfor
print,'solar fluxes in images = ',sk
print,'solar fluxes errors    =',skerr

print,'mean X-ray flux = ',total(fn(cen))/n_elements(cen)
;
; calculate the chi square of the fit
;
chi2=imdiv((xcount+back-mcount)*(xcount+back-mcount),count > countmin)
chi2=total(chi2(sel))
print,'chi2,ndof = ',chi2,ndof
;
if !debug eq 2 then stop
end
