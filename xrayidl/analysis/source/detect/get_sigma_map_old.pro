;     SUBROUTINE GET_SIGMA_MAP

;    This subroutine is to map  the sigma -- the signal to
;    noise ratio and count rate from the input arraies of photon count 
;    and exposure time. The background 

pro get_sigma_map, count_array, time_array, sigma_array, cntr_array,  $
      block=block, radius=radius, bflux=bflux

sz=size(count_array)
array_size=sz(1)
if n_elements(block) eq 0 then block=!block

bin_arcmin = float(block)*0.5/60. ; in units of arcminutes
detect_params,array_size*0.5*bin_arcmin,core_size,ann_in,ann_out

if n_elements(radius) eq 0 then begin
	radius=fix(array_size/2-ann_out)
	print,'The adopted radius of the source search is ', radius,' bins'
endif 
if radius gt (array_size*0.5 - ann_out) then begin
	print,'The specified radius is outside of the allowed range'
	return
endif
; select regions within the radius
dist_circle,dis,array_size,(array_size-1.)/2.,(array_size-1.)/2.
bin_sel=where(time_array gt 0. and dis le radius,nbin)
;
; get the spatially varying detection parameters
;
dis=dis(bin_sel)*bin_arcmin
c=sort(dis)
dis=dis(c)
bin_sel=bin_sel(c)
detect_params,dis,core_size,annulus_in,annulus_out
if !debug eq 1 then stop
core_size = nint(core_size / bin_arcmin)
;
if nbin eq 0 or nbin eq 0 then begin
	print,'No useful bin or count in the image'
	return
endif 
; find the i,j positions of the selected bins in the image
j=bin_sel/long(array_size) & i=bin_sel MOD long(array_size) 
;
;calculate the min and max of the i,j for each box
;
jmin=(j-core_size) > 0 
jmax=(j+core_size) < (array_size-1)
imin=(i-core_size) > 0.
imax=(i+core_size) < (array_size-1)
;
boxdim=2*core_size+1              ;dimension of the boxes
; 
core_count=lonarr(nbin) ;to store count numbers in the cores
core_time=fltarr(nbin)
core_bin=intarr(nbin)
;
boxdimo=0
if !debug eq 1 then stop
for k=0L,(nbin-1) do begin
      
      cbox=count_array(imin(k):imax(k),jmin(k):jmax(k))
      tbox=time_array(imin(k):imax(k),jmin(k):jmax(k))
      if boxdim(k) ne boxdimo then begin
      	dist_circle,dis,boxdim(k),float(core_size(k)),float(core_size(k)) 
	incore=dis le core_size(k)
	boxdimo=boxdim(k)
      endif
      ;set 1 for those bins within the core_size                      
      bin_core=where (incore and  (tbox GT 0.),count) 
      core_bin(k)=count
      if count ne 0 then begin
      	core_count(k)=total(cbox(bin_core))
      	core_time(k)=total(tbox(bin_core))
      endif
 
if !debug eq 2 then stop
endfor
;
sigma_array=fltarr(sz(1),sz(2))
cntr_array=fltarr(sz(1),sz(2))
;
flux_source=(core_count/core_time-bflux)
cntr_array(bin_sel)=flux_source*float(core_bin)
sigma_array(bin_sel)=  core_time * flux_source / $
      	sqrt(float(core_time)*(bflux+(flux_source > 0.)))
	
;     return to the calling routine:
if !debug eq 1 then stop
      return
      end
