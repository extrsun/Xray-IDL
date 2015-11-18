pro map_extract,core_size, array_1,array_2,array_3,bin_sel=bin_sel $
,core_1=core_1,core_2=core_2,core_3=core_3 $
,core_bin=core_bin,nbin=nbin
;+
; extract sums around a selected pixels in images; called by sou_main,
; map_ratio, and scan_map.
;
;*Inputs:
; core_size - the radius of the region around each pixel for the sum,
;             units (pixel)
; array_1,array_2,array_3 - arrays, which could have
;   multiple bands, Up to three such maps may be supplied. array_1
;   should be an exposure map or equivalent for masking the pixel # in
;   the sum
; bin_sel - selected pixels in the 2-D map
;
;*Outputs:
; nbin - total selected number of bins of the region
; core_1, core_2, and core_3 - the summed values of the corresponding
;                              arrays
; core_bin - number of bins in each sum
;
; written by wqd, April, 29, 2005
;-
np=n_params()
if np eq 0 then begin
print,'CALLLING SEQUENCE - map_extract,core_size, array_1,array_2,array_3'
print,',bin_sel=bin_sel,core_1=core_1,core_2=core_2,core_3=core_3 '
print,',core_bin=core_bin,nbin=nbin'
endif

sz=size(array_1)
if n_elements(bin_sel) eq 0 then begin 
            nbin=sz(1)*sz(2)
            bin_sel=lindgen(nbin)
endif else nbin=n_elements(bin_sel)
;
; find the i,j positions of the selected bins in the image
j=bin_sel/sz(1) & i=bin_sel MOD sz(1) 
;
icore_size=nint(core_size)
imin=(i-icore_size) > 0         
imax=(i+icore_size) < (sz(1)-1) 
jmin=(j-icore_size) > 0
jmax=(j+icore_size) < (sz(2)-1)
ic=float(i-imin)
jc=float(j-jmin)
boxdimx=imax-imin+1 ;dimension of the boxes
boxdimy=jmax-jmin+1
;
if !debug eq 4 then stop
if sz(0) eq 3 then nband=sz(4) else nband=1
core_1=fltarr(nbin,nband) 
if np gt 2 then core_2=fltarr(nbin,nband)
if np gt 3 then core_3=fltarr(nbin,nband)
core_bin=intarr(nbin)
for k=0L,(nbin-1) do begin
    dist_circle,dis,[boxdimx(k),boxdimy(k)],ic(k),jc(k)
      tmask=array_1(imin(k):imax(k),jmin(k):jmax(k))
      bin_core=where (dis le core_size(k) and (tmask GT 0.),count) 
      if count ne 0 then begin
        core_bin(k)=count
        for n=0,nband-1 do begin
          tbox=array_1(imin(k):imax(k),jmin(k):jmax(k),n)
          core_1(k,n)=total(tbox(bin_core))
          if np gt 2 then bbox=array_2(imin(k):imax(k),jmin(k):jmax(k),n)
          if np gt 3 then cbox=array_3(imin(k):imax(k),jmin(k):jmax(k),n)
          if np gt 2 then core_2(k,n)=total(bbox(bin_core))
          if np gt 3 then core_3(k,n)=total(cbox(bin_core))
        endfor 
      endif
endfor
return
end
