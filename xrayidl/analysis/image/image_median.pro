;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME
;	image_median
;
;*PURPOSE:
;	obtain a median filtered image. Values can be extrapolated into
;	bins with zero et value, but with non zero ba values (i.e., in
;	source subtracted area.
;
;*CALLING SEQUENCE:
;	image_median,maxbox,influx,ba,et,f,etn,binmin=binmin,lmin=lmin
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
; 	etn-exposure map with new zero exposure pixels
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
;	writen 20 AUG 1993 (WQD)
; move lm=lm+1 to the end of the repeat loop. wqd, Oact 22, 1993
;
;-
;---------------------------------------------------------------------------
pro image_median,maxbox,influx,ba,et,f,fe,etn,binmin=binmin,lmin=lmin,influxe=influxe,fmax=fmax
;
; maxbox --- the maximum bin number in one side of the average
;------------------------------------------------------------------------
if n_params() eq 0 then begin
print,'CALL SEQUENCE - image_median,maxbox,influx,ba,et,f,fe,etn,binmin=binmin,lmin=lmin,influxe=influxe'
return
endif
;
if n_elements(influxe) ne 0 then geterr=1 else geterr=0; to get estimated error for the 
					; output image
if n_elements(fmax) eq 0 then fmax=1.e30
if n_elements(lmin) eq 0 then lmin=1
if n_elements(binmin) eq 0 then binmin=9 ;this 3x3 box smoothing
sz=size(influx)
xdim=sz(1)
ydim=sz(2)
;
if n_elements(fluxerr) eq 0 then begin
c=where(et ne 0.)
flux_mean=avg(influx(c))
print,'the mean flux = ',flux_mean
endif
;
etn=et
f=et*0.
fe=f
;---------------------------------------------------
for j=0,(ydim-1) do begin & for i=0,(xdim-1) do begin
 if ba(i,j) NE 0.  then begin ; exposure at this bin
  if influx(i,j) gt fmax then begin
	f(i,j)=influx(i,j)
	goto,back
  endif
  Lm=Lmin                     
  repeat begin 
;	mkk=(2*Lm+1)*(2*Lm+1)
	mink=(i-Lm) > 0
	maxk=(i+Lm) < (xdim-1)
	minm=(j-Lm) > 0
	maxm=(j+Lm) < (ydim-1)
	ets=et(mink:maxk,minm:maxm)
	fs=influx(mink:maxk,minm:maxm)
	influxs=influx(mink:maxk,minm:maxm)
	good=where(ets GT 0. and fs lt fmax,count)
	hc=count/2
	if count ge binmin  then begin
	  if hc*2 eq count then begin
		list=influxs(good)
		list=list(sort(list))
		f(i,j)=(list(hc)+list(hc-1))*0.5
	  endif else f(i,j)=median(influxs(good))
	  if geterr ne 0 then begin
		influxes=influxe(mink:maxk,minm:maxm)
	  	err=influxes(good)
	  	fe(i,j)=1./sqrt(total(1./(err*err)))
	  endif
	  goto, back
	endif 
;
	if et(i,j) EQ 0. then Lmax=maxbox+2 $
	 else Lmax=maxbox
;

	Lm=Lm+1
  endrep until Lm EQ Lmax+1
	etn(i,j)=0.
 endif
;
back: 
endfor & endfor
if !debug eq 1 then stop
;
return
end

