;     SUBROUTINE STAR_SEARCH
;-
;     This subroutine is to search for sources from the input
;     array SIGMA
;+
pro star_search,sigma_array,cntr_array,n_newsource,x_core,y_core,sigma,cntr, $
array_size=array_size,threshold=threshold,sr=sr
;
if n_params() eq 0 then begin
print,'CALLLING SEQUENCE - star_search,sigma_array,cntr_array'
print,',n_newsource,x_core,y_core,sigma,cntr,array_size=array_size'
print,',threshold=threshold,sr=sr'
return
endif

sz=size(sigma_array)
if n_elements(array_size)  eq 0 then array_size=sz(1)
if n_elements(threshold) eq 0 then threshold=!threshold
srb=nint(sr)
bin_sel=where (sigma_array ge threshold,nbin)

if nbin ne 0 then begin
j=bin_sel/long(array_size) & i=bin_sel MOD long(array_size) 
outsel=where(i le srb or i ge array_size-1-srb or $
	j le srb or j ge array_size-1-srb,nsel)
if nsel ne 0 then begin
	remove,outsel,j,i,bin_sel
	nbin=nbin-nsel
endif
endif
if nbin eq 0 then begin
  n_newsource=0
  print,'No useful bin having sigma larger than the threshold in the image'
  return
endif 
;
;calculate the min and max of the i,j for the box
;
jmin=(j-srb) 
jmax=(j+srb)
imin=(i-srb)
imax=(i+srb)
;
; obtain the distances of the bins from the center of the box
;
boxdim=2*srb+1              ;dimension of the box
dist_circle,dis,boxdim,float(srb),float(srb)
;
;set 1 for those bins within the srb
inarea=dis le sr 
;
sigmamax=lonarr(nbin) ; to store the locations of local sigma  maxima
;
for k=0L,(nbin-1) do begin
      sigmabox=sigma_array(imin(k):imax(k),jmin(k):jmax(k))
      good=where(inarea and (sigmabox gt sigma_array(bin_sel(k))))
      if good(0) eq -1 then  sigmamax(k)=1
endfor

bin_sel_star=bin_sel(where(sigmamax eq 1,n_newsource))
if(n_newsource ne 0) then begin
	sigma=sigma_array(bin_sel_star)
	cntr=cntr_array(bin_sel_star)
	y_core=bin_sel_star/long(array_size) 
	x_core=bin_sel_star MOD long(array_size) 
endif
;
if !debug eq 2 then stop
end
