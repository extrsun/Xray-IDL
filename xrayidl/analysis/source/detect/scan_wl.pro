;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;*NAME:
; scan_wl
;
;*PURPOSE:
; scan over a wavelet image for sources with a certain signal-to-noise ratio 
; greater than a threshold
;
;*CALLING SEQUENCE:
; scan_wl, center_ra,center_dec, array_wl, block=block, radius=radius, 
; threshold=threshold,file=file 
; ,append=append,sigma_array=sigma_array,sr=sr
;
;*PARAMETERS:
;*INPUTS:
; center_ra, center_dec - the ra and dec of the image center (deg)
; array_wl - the array containing the wavelet image
; block - the bin size of the image in units of 0".5
; radius - the outer radius (in units of bins) of the circle 
; 	(centered at the image center) within which the source search
;	is going to be conducted
; inr - the inner radius
;
; threshold - the lower limit of the signal-to-noise ratio of a peak which
;	is considered to be a source candidate (def = !threshold)
; file - the output source file name. if file='no', there will be no 
; 	output file, def = !seq_no+'_sou_wl.dat'
; append - if set, the output will append to the existing file
; sr - the radius within which the location of the maxium sigma
;		is searched for a source (def = !star_area= 15./60. arcmin; 
;		the PSPC FWHM/2 on axis).
;
;*OUTPUTS:
; sigma_array - array containing the sigma values 
;
;*PROCEDURE:
; use the median and 68% pixel values of the wavelet array as the 
; the estimate of the significance (sigma) of individual pixel deviations
; from the background, and then search for the maximum local peaks
; (within circles defined by parameter star_area) which are above the 
; detect threshold. These peaks are considered to be source candidates with
; a position accuracy of the halp pixel size.
; the cntr in the output is the wavelet filtered value
;
;*EXAMPLES:
; scan_wl,center_ra,center_dec,array_wl,thre=3.
;
;*RESTRICTIONS:
; Pixels located within sr from edges of the input array
; will not be search for sources.
;
;*SUBROUTINES CALLED:
; star_search
; avg_median
; trans_*
;*MODIFICATION HISTORY:
;
; writen by wqd, Jan 29, 1995
;-
pro scan_wl, center_ra,center_dec,array_wl,sarray, block=block, $
	radius=radius,inr=inr,threshold=threshold,file=file,append=append,sr=sr
;
if n_params() eq 0 then begin
print,'CALLLING SEQUENCE - scan_wl, center_ra,center_dec, array_wl'
print,', block=block, radius=radius,inr=inr,threshold=threshold,file=file '
print,',append=append,sigma_array=sigma_array,sr=sr'
return
endif
;
sz=size(array_wl)
array_size=sz(1)
if n_elements(block) eq 0 then block=!block
if n_elements(sr) eq 0 then sr=15./(block*!size_pixel) ; about the half of the PSF FWHM
if n_elements(file) eq 0 then $
	fname=!seq_no+'_sou_wl.dat' $ ;to store the information of sources 
	else fname=strtrim(file,2)
if keyword_set(append) eq 0 then $
	openw,un,fname,/get_lun else $
	openw,un,fname,/get_lun,/append
;
;star_number=0
; get the sigma velues in the field
;	avg_median,array_wl,qm,qm1,qm2
;	print,'the mean and 1 sigma levels are ',qm,qm1,qm2 
;	sigma_array=array_wl/(qm2-qm)
; find the sources corresponding the local sigma maxima in the field
	sigma_array=sarray
	if n_elements(radius) ne 0 or n_elements(inr) ne 0 then begin
            if n_elements(inr) eq 0 then inr=0.
            if n_elements(radius) eq 0 then radius=1.e10
		dist_circle,dis,array_size $
			,(array_size-1.)*0.5,(array_size-1.)*0.5
		sel=where(dis lt inr*(60./block/!size_pixel) or $
                          dis gt radius*(60./block/!size_pixel),nsel)
		if nsel ne 0 then sigma_array(sel)=0.
	endif
; search for sources 
	star_search,sigma_array,array_wl, n_newsource,threshold=threshold $
 	 ,x_core, y_core, source_sn, cntr, array_size=array_size,star=sr
        if n_newsource eq 0 then goto,done
	ra_dist = (float(x_core) - float(array_size - 1)*0.5) * float(block)
	dec_dist = (float(y_core) - float(array_size - 1)*0.5) * float(block)
;
     	trans_loct,ra_dist,dec_dist,center_ra,center_dec,star_ra,star_dec,/deg

;     	convert the RA and DEC into unit of hour, degree:

        trans_degree,star_ra,star_dec,ra_hour, $
	 ra_min,ra_sec,dec_deg,dec_min,dec_sec,/deg

	dis=sqrt(ra_dist^2+dec_dist^2)*(!size_pixel/60.)
; 	record this source into output file:
	newstar=indgen(n_newsource)+1

	for k=0,(n_newsource-1) do begin
	 print, newstar(k), ra_hour(k),ra_min(k),ra_sec(k) $
	 ,dec_deg(k),dec_min(k),dec_sec(k), source_sn(k), cntr(k), $
	 dis(k),format='(I4, 2(2i4, f7.2), f9.2, f11.5,f7.2)'
	;the cntr in the output is the wavelet filtered value

	 printf,un, newstar(k),' |', ra_hour(k),ra_min(k) $
	,ra_sec(k),' |',dec_deg(k),dec_min(k),dec_sec(k),' |',source_sn(k), $
	' |',cntr(k),' |', dis(k),' |' $
	,format='(I4,a2,2(2i4, f7.2,a2), f9.2,a2, f11.5,a2,(f7.2,a2))'
	endfor
;	star_number=star_number+n_newsource
free_lun,un
;print,'There are totally ',star_number,' sources found in this area'
done: print,'There are totally ',n_newsource,' sources found in this area'
return
end
