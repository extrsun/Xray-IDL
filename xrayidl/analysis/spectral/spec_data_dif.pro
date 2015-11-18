;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME
; spec_data_dif
;
;*PURPOSE:
; get two lists of counts in two regions defined with image_t from 
; the count list produced by make_list.pro
;
;
;*CALLING SEQUENCE:
; spec_data_dif,list,filter,xmin,ymin,list_s,list_b,numpix_s,numpix_b,block=block 
;
;*PARAMETERS:
; INPUTS:
; list - count list
; filter - the image used to extract source and background spectral lists
;           regions with values equal to zero will be excluded;
;	    regions with values less than zero will be considered as sources
;	    regions with values greater than zero will be considered as backg
; xmin,ymin - the low right position of the filter in the 512x512 image
; 		For a filter of 512*512 and 15" pixel, xmin=0, ymin=0
; block - the pixel size in the ROSAT unit of 0.5 arcsec
;
; OUTPUTS
; list_s,list_b - the count lists of the source and background
; numpix_s,numpix_b - the number of pixels in the circle and annulus
;
;*PROCEDURE:
; obvious
;
;*EXAMPLES:
;*SUBROUTINES CALLED:
; none
;
;
;*MODIFICATION HISTORY:
; writen on Sept 20 1992 (WQD)
;
;-
;----------------------------------------------------------------------
pro spec_data_dif,list,filter,xmin,ymin,list_s,list_b,numpix_s,numpix_b,block=block,noimage=noimage
;
if n_params() eq 0 then begin
print,'CALL SEQUENC - spec_data_dif,list,filter,xmin,ymin,list_s,list_b'
print,',numpix_s,numpix_b,block=block,noimage=noimage'
return
endif
;
if n_elements(block) eq 0 then block = !block
;------------------------------
; 	
;find the event locations in the image filter
tsz=size(filter)
loc=((list.y-ymin)/block)*tsz(1)+(list.x-xmin)/block ;the two dimension sizes 
				       ;of the list should be same as filter 
c=where(filter lt 0.,numpix_s)
if numpix_s eq 0 then stop,'no source region is selected'
backloc=where(filter(loc) gt 0.,nsel_b) ;regions going to be used
if nsel_b ne 0 then list_b=list(backloc) else $
	list_b=-1
c=where(filter gt 0,numpix_b)
if numpix_b eq 0 then stop,'no background region is selected'
souloc=where(filter(loc) lt 0,nsel_s) ;regions considered to be sources
if nsel_s ne 0 then list_s=list(souloc) else $
	list_s=-1
if keyword_set(noimage) eq 0 then begin
print,'Do you want me to show the image showing the core and annulus? y or n'
answer='Y'
read,answer
yesno,answer
if answer eq 1 then begin
	list_image,[list_s,list_b],xmin,ymin,im,tsz(1),block=block
	tv,bscale(im),0
endif
endif
end

