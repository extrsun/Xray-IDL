;     SUBROUTINE GET_SIGMA
;-
;    This subroutine is to map  the sigma -- the signal to
;    noise ratio and count rate from the input arraies of photon count 
;    and exposure time.
; radius - the maximum off-axis angle within which sources are going to 
;		be searched (units = bins) and can be overrided by bin_sel
; written by WQD, April 16,1996
;+

pro get_sigma, count_array, time_array, sigma_array, cntr_array,  $
     bin_sel=bin_sel,core_size=core_size,annulus_in=annulus_in $
	,annulus_out=annulus_out,radius=radius,sfrac=sfrac,bfrac=bfrac $
	,snb_array=snb_array

sz=size(count_array)
if n_elements(bin_sel) eq 0 then begin 
	dist_circle,dis,sz(1),(sz(1)-1.)/2.,(sz(2)-1.)/2.
	if n_elements(radius) eq 0. then radius=core_size
	bin_sel=where(time_array gt 0. and dis le radius,nbin)
endif else nbin=n_elements(bin_sel)
;
; find the i,j positions of the selected bins in the image
j=bin_sel/sz(1) & i=bin_sel MOD sz(1) 
;
;calculate the min and max of the i,j for each box
iannulus_out=nint(annulus_out)
jmin=(j-iannulus_out) 
jmax=(j+iannulus_out) 
imin=(i-iannulus_out) 
imax=(i+iannulus_out) 
; the above four parameters have to be within the boundaries of the arrays
; but have to keep to form square boxes because dis_circle.pro is used later
;
boxdim=2*iannulus_out+1              ;dimension of the boxes
dist_circle,dis,boxdim,float(iannulus_out),float(iannulus_out) 
incore=dis le core_size
inannulus=(dis le float(annulus_out)) and (dis gt float(annulus_in))
; 
core_count=lonarr(nbin) ;to store count numbers in the cores
core_time=fltarr(nbin)
core_bin=intarr(nbin)
annulus_count=lonarr(nbin)
annulus_time=fltarr(nbin)
;
for k=0L,(nbin-1) do begin
      
      cbox=count_array(imin(k):imax(k),jmin(k):jmax(k))
      tbox=time_array(imin(k):imax(k),jmin(k):jmax(k))
                   
      bin_core=where (incore and  (tbox GT 0.),count) 
      core_bin(k)=count
      if count ne 0 then begin
      	core_count(k)=total(cbox(bin_core))
      	core_time(k)=total(tbox(bin_core))
      endif
 
      bin_annulus=where (inannulus  and (tbox GT 0.),count)
      if count ne 0 then begin
        annulus_count(k)=total(cbox(bin_annulus))
        annulus_time(k)=total(tbox(bin_annulus))
      endif
if !debug eq 2 then stop
endfor
;
sigma_array=fltarr(sz(1),sz(2))
cntr_array=fltarr(sz(1),sz(2))
;snb_array=fltarr(sz(1),sz(2))
;
get_snr,core_count,core_time,annulus_count,annulus_time,csn,snr,snb=snb $
	,sfrac=sfrac,bfrac=bfrac
cntr_array(bin_sel)=csn*(float(core_bin)/core_time)
sigma_array(bin_sel)=snr
;snb_array(bin_sel)=snb
return
end
