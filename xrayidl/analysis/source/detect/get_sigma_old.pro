;     SUBROUTINE GET_SIGMA

;    This subroutine is to map  the sigma -- the signal to
;    noise ratio and count rate from the input arraies of photon count 
;    and exposure time.

pro get_sigma, count_array, time_array, sigma_array, cntr_array,  $
      block=block, radius=radius, $
      annulus_out=annulus_out, annulus_in=annulus_in, core_size=core_size

sz=size(count_array)
array_size=sz(1)
if n_elements(block) eq 0 then block=!block
if n_elements(annulus_out) eq 0 then annulus_out=!annulus_out
if n_elements(annulus_in) eq 0 then annulus_in=!annulus_in
if n_elements(core_size) eq 0 then core_size=!core_size

bin_real = float(block)*0.5/60. ; in units of arcminutes
annulus_out = annulus_out  / bin_real
annulus_in = annulus_in  / bin_real
core_size = core_size / bin_real

if n_elements(radius eq 0 then begin
	radius=fix(array_size/2-annulus_out)
	print,'The adopted radius of the source search is ', radius,' bins'
endif else if radius gt (array_size*0.5 - annulus_out) then
	print,'The specified radius is outside of the allowed range'
	return
endelse
; select regions within the radius
dist_circle,dis,array_size,(array_size-1.)/2.,(array_size-1.)/2.
bin_sel=where(time_array gt 0. and dis le radius,nbin)

if nbin eq 0 or nbin eq 0 then begin
print,'No useful bin or count in the image'
return
endif 
; find the i,j positions of the selected bins in the image
j=bin_sel/long(array_size) & i=bin_sel MOD long(array_size) 
;
;calculate the min and max of the i,j for each box
;
jmin=(j-annulus_out) 
jmax=(j+annulus_out) 
imin=(i-annulus_out) 
imax=(i+annulus_out) 
;
; obtain the distances of the bins from the center of the box
;
boxdim=2*annulus_out+1              ;dimension of the box
dist_circle,dis,boxdim,float(annulus_out),float(annulus_out)
;
;set 1 for those bins within the annulus_out
inannulus=(dis le float(annulus_out) and dis gt float(annulus_in))
incore=dis le core_size
; 
core_count=lonarr(nbin) ;to store count numbers in the cores
core_time=fltarr(nbin)
core_bin=intarr(nbin)
annulus_count=lonarr(nbin)
annulus_time=fltarr(nbin)
annulus_bin=intarr(nbin)
;
for k=0L,(nbin-1) do begin
      
      cbox=count_array(imin(k):imax(k),jmin(k):jmax(k))
      tbox=time_array(imin(k):imax(k),jmin(k):jmax(k))

      bin_core=where ( incore and (tbox GT 0.),count)
      core_bin(k)=count
      core_count(k)=total(cbox(bin_core))
      core_time(k)=total(tbox(bin_core))
 
      bin_annulus=where ( inannulus and (tbox GT 0.),count)
      annulus_bin(k)=count
      if bin_annulus(0) ne -1 then begin
        annulus_count(k)=total(cbox(bin_annulus))
        annulus_time(k)=total(tbox(bin_annulus))
      endif

endfor
;
sigma_array=fltarr(sz(1),sz(2))
cntr_array=fltarr(sz(1),sz(2))
;
count_back_err=sqrt(float(annulus_count) > 1)*core_time/annulus_time
flux_source=(core_count/core_time-annulus_count/annulus_time)
      	cntr_array(bin_sel)=flux_source*float(core_bin)
      	sigma_array(bin_sel)=  core_time * flux_source / $
      	sqrt((float(core_count) + count_back_err*count_back_err) > 1)
	
;     return to the calling routine:
if !debug eq 2 then stop
      return
      end
