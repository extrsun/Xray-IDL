;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME:
; scan
;
;*PURPOSE:
; scan over an image for sources with a certain signal-to-noise ratio greater
; than a threshold, using the moving box technique
;
;*CALLING SEQUENCE:
; scan, center_ra,center_dec, array_c, array_t, array_tsub, block=block,
; radius=radius, threshold=threshold, star_area=star_area, /noiterate,
; dir=dir, file=file, append =append
;
;*PARAMETERS:
;*INPUTS:
; center_ra, center_dec - the ra and dec of the image center (radian)
; array_c - the array containing counts of the image
; array_t - the array containing the exposure of the image
; block - the bin size of the image in units of 0".5
; radius - the radius (in units of pixels) of the circle
; 	(centered at the image center) within which
;	   the source scan is going to be conducted
;	(def = 0.5*array_size-!annulus_out)
; threshold - the lower limit of the signal-to-noise ratio of a peak which
;	is considered to be a source candidate (def = !threshold)
; star_area - the radius of a circle within which the maximum signal-to-noise
;           ratio is searched to find the position of a source candidate
;		(def =!star_area)
; noiterate - if set no iteration of source searching will be performed
; dir - the directory to store the output file (def =!data_dir)
; file - the output source file name. if file='no', there will be no
; 	output file
; append - if set, the output will append to the existing file
;
;*OUTPUTS:
; array_tsub - the array containing the exposure after sources subtractions
; *_sou.dat - the source file name if the keyword file is not provided
;
;*PROCEDURE:
; use moving box technique to calculate the sigma (signal-to-noise ratio)
; of each pixels in the image and then search for the maximum local peaks
; (within circles defined by parameter star_area) which are above the
; detect threshold. These peaks are considered to be source candidates and
; are subtracted from the exposure image.
;
;*EXAMPLES:
; scan,center_ra,center_dec,array_c,array_t,array_tsub,radius=80
;
;*RESTRICTIONS:
; at this point, the two dimension of the array are assumed to be same and
; should be larger than 2*(radius+annulus_out)
;
;*NOTES:
; various system variables should be defined first in your rosatsysv.pro,
; including:
; !core_size - the radius of the circle within which source counts are
;		collected
; !annulus_in - the inner radius of the annulus within which a background flux
;		is calculated
; !annulus_out - the outer radius of the above annulus
; !threshod ; !star_area
;
;*SUBROUTINES CALLED:
; get_sigma
; star_search
; source_sub
;
;*MODIFICATION HISTORY:
; writen Sept 11 (WQD)
;
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro scan, center_ra,center_dec, array_c, array_t, array_tsub, block=block, $
radius=radius, threshold=threshold, star_area=star_area, noiterate=noiterate, $
dir=dir, file=file, append=append
;
if n_params() eq 0 then begin
print,'CALLLING SEQUENCE - scan, center_ra,center_dec, array_c, array_t,'
print,' array_tsub, block=block, radius=radius, threshold=threshold,'
print,' star_area=star_area, /noiterate, dir=dir, file=file, append=append'
return
endif
;
array_tsub=array_t
sz=size(array_c)
array_size=sz(1)
if n_elements(threshold) eq 0 then threshold=!threshold
if n_elements(star_area) eq 0 then star_area=!star_area
if n_elements(block) eq 0 then block=!block
;
if n_elements(dir) eq 0 then dir=!data_dir
;

if n_elements(file) eq 0 then $
fname=dir+!seq_no+'_sou.dat' $ ;to store the information of sources
else fname=dir+strtrim(file,2)
file='yes'
if (strtrim(strupcase(file)) ne 'NO') then begin
	if keyword_set(append) eq 0 then openw,un,fname,/get_lun else $
	openw,un,fname,/get_lun,/append
endif

;
star_number=0
n_newsource=1

while n_newsource ge 1 do begin

; map out the sigma velues in the field
	get_sigma, array_c,array_tsub,sigma_array,cntr_array,radius=radius

; find the sources corresponding the local sigma maxima in the field
	star_search,sigma_array,cntr_array, n_newsource, $
 	 x_core, y_core, source_sn, cntr, array_size=array_size, block=block

	if n_newsource eq 0 then goto,complete

	ra_dist = (float(x_core) - float(array_size - 1)*0.5) * float(block)
	dec_dist = (float(y_core) - float(array_size - 1)*0.5) * float(block)
;
     	trans_loct,ra_dist,dec_dist,center_ra,center_dec,star_ra,star_dec

;     	convert the RA and DEC into unit of hour, degree:

        trans_degree,star_ra,star_dec,ra_hour, $
	 ra_min,ra_sec,dec_deg,dec_min,dec_sec

; 	record this source into output file:
	newstar=indgen(n_newsource)+1

	for k=0,(n_newsource-1) do begin
	 print, star_number+newstar(k), ra_hour(k),ra_min(k),ra_sec(k) $
	 ,dec_deg(k),dec_min(k),dec_sec(k), source_sn(k), cntr(k), $
	 x_core(k),y_core(k),format='(I3, 2(2i4, f7.2), f9.2, f11.5,2I4)'

	 if (strtrim(strupcase(file)) ne 'NO') then  $
	  printf,un, star_number+newstar(k), ra_hour(k),ra_min(k),ra_sec(k) $
	 ,dec_deg(k),dec_min(k),dec_sec(k),source_sn(k),cntr(k), $
	 x_core(k),y_core(k),format='(I3,2(2i4, f7.2), f9.2, f11.5,2I4)'
	endfor

;	subtract detected sources from the field
	array_tsub=source_sub(array_tsub, $
	center_ra,center_dec,star_ra,star_dec,block=block,factor=0.7)

	star_number=star_number+n_newsource

	if keyword_set(noiterate) eq 1 then goto,complete

endwhile

complete:
if (strtrim(strupcase(file)) ne 'NO') then free_lun,un
print,'There are totally ',star_number,' sources found in this area'
end
