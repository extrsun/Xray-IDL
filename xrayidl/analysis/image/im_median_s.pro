;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME
;	im_median_s
;
;*PURPOSE:
;	obtain a median filtered and size-reduced image. 
;
;*CALLING SEQUENCE:
;	im_median_s,maxbox,influx,ba,et,f,etn,binmin=binmin,lmin=lmin
;
;*PARAMETERS:
; INPUTS:
;	maxbox - one-side dimension of the maximum average box
;	influx,ba,et - image (e.g. flux), background, exposure images
;	binmin - the minmum number of bins to be included for calculating 
;		the median flux at a bin in the image
;
; OUTPUTS
;	f - the median filtered image 
;
;*PROCEDURE:
;       The median filter box size increases from 1 to maxbox (in regions 
; where sources 
; are subtracted the limitation on maxbox is relaxed). The median flux is 
; calculated when the number bins included in the surround area exceeds
; binmin. Otherwise, the exposure of that bin
; is set equal to zero
;
;*EXAMPLES:
;	image_median,2,array_f,array_b,array_t,f_median,binmin=9
;
;*RESTRICTIONS:
;
;*NOTES:
;
;
;*SUBROUTINES CALLED:
;
;*MODIFICATION HISTORY:
;	writen by wqd Feb 26 1997
;
;-
;---------------------------------------------------------------------------
pro im_median_s,maxbox,influx,ba,et,f,binmin=binmin,lmin=lmin,influxe=influxe,fmax=fmax
;
;------------------------------------------------------------------------
if n_params() eq 0 then begin
print,'CALL SEQUENCE - image_median,maxbox,influx,ba,et,f,binmin=binmin,lmin=lmin'
return
endif
;
if n_elements(fmax) eq 0 then fmax=1.e30
if n_elements(lmin) eq 0 then lmin=1
if n_elements(binmin) eq 0 then binmin=9 ;this 3x3 box smoothing
sz=size(influx)
xdim=sz(1)
ydim=sz(2)
idim=xdim/(2*long(lmin)+1)
jdim=ydim/(2*long(lmin)+1)
loc=lindgen(idim*jdim)
indexo=(1+(loc/idim)*3L)*xdim+(1+(loc mod idim)*3L)
sel=where(ba(indexo) gt 0.,nsel)
if nsel eq 0 then stop,'nsel eq 0'
loc=loc(sel)
indexo=indexo(sel)
jv=indexo/xdim & iv=indexo mod xdim
f=fltarr(idim,jdim)
;
;---------------------------------------------------
for k =0,nsel-1 do begin
  i=iv(k) & j=jv(k)
  kk=loc(k)
  if influx(i,j) gt fmax then begin
	f(kk)=influx(i,j)
	goto,back
  endif
  Lm=Lmin                     
  repeat begin 
;	mkk=(2*Lm+1)*(2*Lm+1)
	mink=(i-Lm) 
	maxk=(i+Lm)
	minm=(j-Lm) 
	maxm=(j+Lm)
	ets=et(mink:maxk,minm:maxm)
	influxs=influx(mink:maxk,minm:maxm)
	good=where(ets GT 0. and influxs lt fmax,count)
	
	if count ge binmin  then begin
	  hc=count/2
	  if hc*2 eq count then begin
		list=influxs(good)
		list=list(sort(list))
		f(kk)=(list(hc)+list(hc-1))*0.5
	  endif else f(kk)=median(influxs(good))
	  goto, back
	endif 

	if et(i,j) EQ 0. then Lmax=maxbox+2 else Lmax=maxbox

	Lm=Lm+1
  endrep until Lm EQ Lmax+1
back: 
endfor 
if !debug eq 1 then stop
;
return
end

