;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME
;	image_median_f
;
;*PURPOSE:
;	obtain a median filtered image with a map filter for user
;	input bin selection. Values can be extrapolated into
;	bins with zero et value, but with non zero ba values (i.e., in
;	source subtracted area.
;
;*CALLING SEQUENCE:
;	image_median_f,maxbox,influx,ba,et,f,etn,binmin=binmin,lmin=lmin,
;		filters=filters
;
;*PARAMETERS:
; INPUTS:
;	maxbox - one-side dimension of the maximum average box
;	influx,ba,et - image (e.g. flux), background, exposure images
;	binmin - the minmum number of bins to be included for calculating 
;		the median flux at a bin in the image
;	filters - a filter for selecting data in small maps
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
;	image_median_f,2,array_f,array_b,array_t,f_median,lmin=1,/filters
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
; modified the image_median.pro to include a filter for small maps of median
; average. wqd, Oct. 22, 1993
;
;-
;---------------------------------------------------------------------------
pro image_median_f,maxbox,influx,ba,et,f,fe,etn,binmin=binmin,lmin=lmin,influxe=influxe,filters=filters
;
; maxbox --- the maximum bin number in one side of the average
;------------------------------------------------------------------------
if n_params() eq 0 then begin
print,'CALL SEQUENCE - image_median,maxbox,influx,ba,et,f,fe,etn,binmin=binmin,lmin=lmin,influxe=influxe,filters=filters'
return
endif
;
if keyword_set(filters) ne 0 then begin
	if n_elements(filters) le 1 then begin
	 filters=[0,1,0,1,1,1,0,1,0]
	 nfs=9
	 cc=where(filters eq 1,binmin)
	 print,'Use filter for small maps= ',filters
	 print,'binmin = ',binmin
	endif
endif
if n_elements(influxe) ne 0 then geterr=1 else geterr=0; to get estimated error for the 
					; output image
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
 if ba(i,j) NE 0. then begin ; exposure at this bin
	Lm=Lmin                     
  repeat begin 
	mink=(i-Lm) > 0
	maxk=(i+Lm) < (xdim-1)
	minm=(j-Lm) > 0
	maxm=(j+Lm) < (ydim-1)
	ets=et(mink:maxk,minm:maxm)
	influxs=influx(mink:maxk,minm:maxm)
;	mkk=(2*Lm+1)*(2*Lm+1)
	if keyword_set(filters) eq 0 then good=where(ets GT 0.,count) $
	 else begin
	  	if n_elements(influxs) ne nfs then $
	 	good=where(ets GT 0.,count) else $
			good=where(ets gt 0. and filters,count)
	 endelse
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

