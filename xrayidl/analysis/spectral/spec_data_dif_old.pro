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
;
;*RESTRICTIONS:
; fixed circle and annulus as defined in specsysv.pro
;
;*NOTES:
;
;*SUBROUTINES CALLED:
; none
;
;
;*MODIFICATION HISTORY:
; writen on Sept 20 1992 (WQD)
;
;-
;----------------------------------------------------------------------
pro spec_data_dif,list,filter,xmin,ymin,list_s,list_b,numpix_s,numpix_b,block=block 
;
if n_params() eq 0 then begin
print,'CALL SEQUENC - spec_data_dif,list,filter,list_s,list_b'
print,',numpix_s,numpix_b,block=block'
endif
;
if n_elements(block) eq 0 then block = !block
;------------------------------
; 	
tsz=size(filter)
badloc=where(filter eq 0.,nbadloc) ;regions going to be rejected
souloc=where(filter lt 0.,nsouloc) ;regions considered to be sources

numpix_s=nsouloc*(block*block)
numpix_b=(tsz(4)-nbadloc-nsouloc)*(block*block)

;find the event locations in the image filter
loc=((list.y-ymin)/block)*tsz(1)+(list.x-xmin)/block ;the two dimension sizes 
				       ;of the list should be same as filter 
locsz=n_elements(loc)

if nsouloc ne 0 then begin

	;find the subscripts of the events which are coicident in
	; position with the elements covered by sources
    	sou=lonarr(locsz)
    	kk=0
    	for k=0,(nsouloc-1) do begin
    	    sousel=where(loc eq souloc(k),nsousel)
    	    if nsousel ne 0 then begin
    	    	sou(kk:kk+nsousel-1)=sousel
    	    	kk=kk+nsousel
    	    endif
    	endfor

    	;select the events using the subscripts
    	if kk ne 0 then begin
    		sou=sou(0:kk-1)
    		list_s=list(sou)
		list_b=list ;though it contains events in bad pixels
			    ;they will not be used in make_sepc.pro
    		remove,sou,list_b
	endif else begin
		; if no events in filtered region, return -1
    		list_s=-1
    		list_b=-1
	endelse

endif
if !debug eq 2 then stop
;
end

