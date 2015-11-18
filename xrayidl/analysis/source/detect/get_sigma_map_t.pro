;     SUBROUTINE GET_SIGMA
;-
;    This subroutine is to map  the sigma -- the signal to
;    noise ratio and count rate from the input arrays of photon count 
;    and exposure time.
; bin_sel - selected image bins for consideration, which may be changed in
;	the program.
; radius - the maximum off-axis angle within which sources are going to 
;		be searched (units = bins) and can be overrided by bin_sel
; core_size - single value or an array of the same dimension as bin_sel
; written by WQD, April 16,1996
; modified to exclude bins which are near edges of the searching image.
; by wqd, 9/29/99
;+

pro get_sigma_map, array_c,array_b,array_t, sigma_array, cntr_array,  $
 bin_sel=bin_sel,core_size=core_size,radius=radius,sfrac=sfrac,ebmap=ebmap $
 ,core_count=core_count,core_back=core_back

sz=size(array_c)
if n_elements(bin_sel) eq 0 then begin 
	dist_circle,dis,sz(1),(sz(1)-1.)/2.,(sz(2)-1.)/2.
	bin_sel=where(array_t gt 0. and dis lt radius,nbin)
endif else nbin=n_elements(bin_sel)
;
; find the i,j positions of the selected bins in the image
j=bin_sel/sz(1) & i=bin_sel MOD sz(1) 
;

;calculate the min and max of the i,j for each box
icore_size=nint(core_size)
sel=where(j ge icore_size and j lt (sz(2)-icore_size) and $
	i ge icore_size and i lt (sz(1)-icore_size),nbin)
if nbin eq 0 then stop,'no bin is suitable for the search' $
	else begin
		j=j(sel)
		i=i(sel)
		bin_sel=bin_sel(sel) ;This changes the size!
		icore_size=icore_size(sel)
		core_size=core_size(sel) ;This changes the size!
	endelse
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
icore_size=float(icore_size)
;
for k=0L,(nbin-1) do begin
      dist_circle,dis,boxdim(k),icore_size(k),icore_size(k)
      cbox=array_c(imin(k):imax(k),jmin(k):jmax(k))
      bbox=array_b(imin(k):imax(k),jmin(k):jmax(k))
      tbox=array_t(imin(k):imax(k),jmin(k):jmax(k))
      bin_core=where (dis le core_size(k) and (tbox GT 0.),count) 
      core_bin(k)=count
      if count ne 0 then begin
      	core_back(k)=total(bbox(bin_core))
	core_count(k)=total(cbox(bin_core))
      	core_time(k)=total(tbox(bin_core))
      endif
endfor
;
sigma_array=fltarr(sz(1),sz(2))
cntr_array=fltarr(sz(1),sz(2))
;
if n_elements(ebmap) eq 0 then $
get_snr,core_count,core_time,core_back,nbin_b,csn,snr,sfrac=sfrac,bmap=1 else $
get_snr,core_count,core_time,core_back,nbin_b,csn,snr,sfrac=sfrac,bmap=1,ebmap=ebmap(bin_sel)
cntr_array(bin_sel)=csn*(float(core_bin)/core_time)
sigma_array(bin_sel)=snr
ss=where(array_t le 0.,nss) 
if nss ne 0 then sigma_array(ss)=0.
return
end
