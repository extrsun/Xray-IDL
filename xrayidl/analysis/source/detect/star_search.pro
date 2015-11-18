;     SUBROUTINE STAR_SEARCH
;-
;     This subroutine is to search for sources from the input array SIGMA 
;	using a varying source searching area and is called by scan_v.pro
; star_area - the radius of the area used to search for a signal-to-noise peak
;		in units of array bins
; writen by WQD, April, 16 1996
; various modifications made to be called by anal_v.pro. WQD Sept 13, 1993
;+
pro star_search,sigma_array,cntr_array,n_newsource,x_core,y_core $
	,sigma,cntr,array_size=array_size,threshold=threshold $
	,star_area=star_area
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - star_search,sigma_array,cntr_array,n_newsource'
print,',x_core,y_core,sigma,cntr,array_size=array_size'
print,',threshold=threshold,star_area=star_area'
return
endif

sz=size(sigma_array)
if n_elements(array_size)  eq 0 then array_size=sz(1)
if n_elements(threshold) eq 0 then threshold=!threshold

bin_sel=where (sigma_array ge threshold,nbin)

if nbin eq 0 or nbin eq 0 then begin
   n_newsource=0
   print,'No useful bin having sigma larger than the threshold in the image'
   return
endif 
;
j=bin_sel/long(array_size) & i=bin_sel MOD long(array_size) 
;
;calculate the min and max of the i,j for the box
;
istar_area=fix(star_area)
jmin=(j-istar_area) > 0 
jmax=(j+istar_area) < (array_size-1)
imin=(i-istar_area) > 0 
imax=(i+istar_area) < (array_size-1)

; obtain the distances of the bins from the center of the box
boxdim=2*istar_area+1              ;dimension of the box
dist_circle,dis,boxdim,istar_area,istar_area
inarea=dis le star_area   
;
sigmamax=lonarr(nbin) ; to store the locations of local sigma  maxima
;
boxdimo=0
for k=0L,(nbin-1) do begin
      sigmabox=sigma_array(imin(k):imax(k),jmin(k):jmax(k))   
      good=where(inarea and (sigmabox gt sigma_array(bin_sel(k))),count)
      if count eq 0 then sigmamax(k)=1
endfor

bin_sel_star=bin_sel(where(sigmamax eq 1,n_newsource))
if(n_newsource ne 0) then begin
	sigma=sigma_array(bin_sel_star)
	cntr=cntr_array(bin_sel_star)
	y_core=bin_sel_star/long(array_size) 
	x_core=bin_sel_star MOD long(array_size) 
endif
;
end
