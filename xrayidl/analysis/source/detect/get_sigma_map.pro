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
; revised to use map_extract, by wqd, April, 29, 2005
;+
pro get_sigma_map,array_c,array_b,array_t, sigma_array, cntr_array,  $
 bin_sel=bin_sel,core_size=core_size,ebmap=ebmap $
 ,core_count=core_count,core_back=core_back,core_time=core_time,core_bin=core_bin,sfrac=sfrac ;,radius=radius

map_extract,core_size,array_t,array_b,array_c,bin_sel=bin_sel,core_3=core_count,core_2=core_back,core_1=core_time,core_bin=core_bin
;
sigma_array=array_t*0.
cntr_array=array_t*0.
;
if n_elements(ebmap) eq 0 then $
get_snr,core_count,core_time,core_back,nbin_b,csn,snr,sfrac=sfrac,bmap=1 else $
get_snr,core_count,core_time,core_back,nbin_b,csn,snr,sfrac=sfrac,bmap=1,ebmap=ebmap(bin_sel)
cntr_array(bin_sel)=csn*(float(core_bin)/core_time)
sigma_array(bin_sel)=snr
return
end
