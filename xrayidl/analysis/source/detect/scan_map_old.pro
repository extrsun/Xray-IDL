;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME:
; scan_map
;
;*PURPOSE:
; scan over an image for sources with a certain signal-to-noise ratio greater
; than a threshold, using the moving box technique
;
;*CALLING SEQUENCE:
; scan_map, center_ra,center_dec, array_c, array_t, array_tsub, block=block,
; radius=radius, threshold=threshold,iterate, file=file, /append
;
;*PARAMETERS:
;*INPUTS:
; center_ra, center_dec - the ra and dec of the image center (radian)
; array_c - the array containing counts of the image
; array_t - the array containing the exposure of the image
; block - the bin size of the image in units of 0".5
; radius - the radius (in units of bins) of the circle 
; 	(centered at the image center) within which 
;	   the source scan is going to be conducted 
;	(def = 0.5*array_size-!annulus_out)
; threshold - the lower limit of the signal-to-noise ratio of a peak which
;	is considered to be a source candidate (def = !threshold)
; iterate - the number of iterations of source searching will be performed
;	   def=2
; file - the output source file name. if file='no', there will be no 
; 	output file
; append - if set, the output will append to the existing file
; frac - the fraction of the core_size used as the radius to search for
;	local maxima of signal-to-noise ratios.
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
; The source detection parameters are presented in the procedure detect_params.
; !threshod is used as a system variable
; 
;*SUBROUTINES CALLED:
; get_sigma
; star_search
; source_sub
;
;*MODIFICATION HISTORY:
; writen Sept 11 (WQD) 
; renamed from scan after modifying the program to use spatially varying 
; detection parameters. WQD, Nov 30, 1992
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro scan_map, center_ra,center_dec, array_c, array_t, array_tsub, block=block, $
radius=radius, threshold=threshold, iterate=iterate,file=file $
,append=append,frac=frac,sigma_array=sigma_array
;
if n_params() eq 0 then begin
print,'CALLLING SEQUENCE - scan_map, center_ra,center_dec, array_c, array_t,'
print,' array_tsub, block=block, radius=radius, threshold=threshold,'
print,' iterate=, file=file, append=append, frac=frac'
return
endif
;
array_tsub=array_t
sz=size(array_c)
array_size=sz(1)
if n_elements(block) eq 0 then block=!block
if n_elements(iterate) eq 0 then iterate=2
;
if n_elements(file) eq 0 then $
	fname=!seq_no+'_sou.dat' $ ;to store the information of sources 
	else fname=strtrim(file,2)
if keyword_set(append) eq 0 then $
	openw,un,!data_dir+fname,/get_lun else $
	openw,un,!data_dir+fname,/get_lun,/append
; get the background flux in the image
image_bflux,array_c,array_tsub,bflux
star_number=0
n_newsource=1
ni=0

;while n_newsource ge 1 do begin

;	n_newsource=0
; map out the sigma velues in the field
	get_sigma_map, array_c,array_t,sigma_array,cntr_array, $
	block=block,radius=radius,bflux=bflux
stop
; find the sources corresponding the local sigma maxima in the field
	star_search_v,sigma_array,cntr_array, n_newsource,threshold=threshold $
 	 ,x_core, y_core, source_sn, cntr, array_size=array_size, block=block

;	if n_newsource eq 0 then goto,complete

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

	 printf,un, star_number+newstar(k),' |', ra_hour(k),ra_min(k) $
	,ra_sec(k),' |',dec_deg(k),dec_min(k),dec_sec(k),' |',source_sn(k), $
	' |',cntr(k),' |', x_core(k),' |',y_core(k),' |' $
	,format='(I3,a2,2(2i4, f7.2,a2), f9.2,a2, f11.5,a2,2(I4,a2))'
	endfor

;	subtract detected sources from the field
	array_tsub=source_sub_v(array_tsub, $
	center_ra,center_dec,star_ra,star_dec,cntr,block=block,factor=1.5)

	star_number=star_number+n_newsource

;	ni=ni+1
;	if ni eq iterate then goto,complete

;endwhile

;complete: 
free_lun,un
print,'There are totally ',star_number,' sources found in this area'
if !debug eq 1 then stop
end