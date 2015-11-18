;     SUBROUTINE GET_SIGMA
;-
;    This subroutine is to map  the sigma -- the signal to
;    noise ratio and count rate from the input arraies of photon count 
;    and exposure time.
; radius - the maximum off-axis angle within which sources are going to 
;		be searched (units = bins) and can be overrided by bin_sel
; written by WQD, April 16,1996
;+

pro get_sigma_maps, array_c,array_b,array_t, sigma_array, cntr_array,  $
     bin_sel=bin_sel,core_size=core_size,radius=radius,sfrac=sfrac

sz=size(array_c)
if n_elements(bin_sel) eq 0 then begin 
	dist_circle,dis,sz(1),(sz(1)-1.)/2.,(sz(2)-1.)/2.
	if n_elements(radius) eq 0. then radius=core_size
	bin_sel=where(array_t gt 0. and dis lt radius,nbin)
endif else nbin=n_elements(bin_sel)
;
; find the i,j positions of the selected bins in the image
j=bin_sel/sz(1) & i=bin_sel MOD sz(1) 
;
;calculate the min and max of the i,j for each box
icore_size=nint(core_size)
jmin=(j-icore_size) 
jmax=(j+icore_size) 
imin=(i-icore_size) 
imax=(i+icore_size) 
boxdim=2*icore_size+1            ;dimension of the boxes

; the above four parameters have to be within the boundaries of the arrays
; but have to keep to form square boxes because dis_circle.pro is used later
;
core_count=lonarr(nbin) ;to store count numbers in the cores
core_back=fltarr(nbin)
core_time=fltarr(nbin)
core_bin=intarr(nbin)
;
for k=0L,(nbin-1) do begin
      dist_circle,dis,boxdim(k),float(icore_size(k)),float(icore_size(k)) 
      cbox=array_c(imin(k):imax(k),jmin(k):jmax(k))
      bbox=array_b(imin(k):imax(k),jmin(k):jmax(k))
      tbox=array_t(imin(k):imax(k),jmin(k):jmax(k))
      bin_core=where (dis le core_size(k),count) ; and  (tbox GT 0.),count) 
      core_bin(k)=count
;      if count ne 0 then begin
      	core_back(k)=total(bbox(bin_core))
	core_count(k)=total(cbox(bin_core))
      	core_time(k)=total(tbox(bin_core))
;      endif
endfor
;
sigma_array=fltarr(sz(1),sz(2))
cntr_array=fltarr(sz(1),sz(2))
;
get_snr,core_count,core_time,core_back,nbin_b,csn,snr,sfrac=sfrac,bmap=1
cntr_array(bin_sel)=csn*(float(core_bin)/core_time)
sigma_array(bin_sel)=snr
return
end
