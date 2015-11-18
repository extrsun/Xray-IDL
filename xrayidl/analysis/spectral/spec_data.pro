;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME
; spec_data
;
;*PURPOSE:
; get two lists of counts in a source circle and in a background annulus from 
; the count list produced by make_list.pro
;
;
;*CALLING SEQUENCE:
; spec_data,list,xs_pix,ys_pix,list_s,list_b,numpix_s,numpix_b,block=block
; ,radius_s=radius_s,radius_b1=radius_b1,radius_b2=radius_b2
;*PARAMETERS:
; INPUTS:
; list - count list
; xs_pix,ys_pix - the pixel position of the center of the circle 
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
; writen on Aug 17 1992 (WQD)
;
;-
;----------------------------------------------------------------------
pro spec_data,list,xs_pix,ys_pix,list_s,list_b,numpix_s,numpix_b $
,radius_s=radius_s,radius_b1=radius_b1,radius_b2=radius_b2
;
if n_params() eq 0 then begin
print,'CALL SEQUENC - spec_data,list,xs_pix,ys_pix,list_s,list_b'
print,',numpix_s,numpix_b,radius_s=radius_s,radius_b1=radius_b1,radius_b2=radius_b2'
endif
;
if keyword_set(radius_s) ne 0 then begin
	!radius_s=radius_s
	!radius_b1=radius_b1
	!radius_b2=radius_b2
endif else begin
print,'Extract the source and background spectra using !radius_s = ',!radius_s, $
' !radius_b1 = ',!radius_b1,' and !radius_b2 = ',!radius_b2, ' arcminutes'
stop,'stop: You may make changes here and type .c to continue'
endelse
;
rrs=!radius_s*60./!size_pixel ; photon list coordinates
rrb1=!radius_b1*60./!size_pixel; radii are in units of arcminutes
rrb2=!radius_b2*60./!size_pixel
numpix_s=!pi*rrs*rrs
numpix_b=!pi*(rrb2*rrb2-rrb1*rrb1)
;
;
xs_pix=float(xs_pix)
ys_pix=float(ys_pix)
dist=(list.x-xs_pix)*(list.x-xs_pix)+(list.y-ys_pix)*(list.y-ys_pix)
inds=where(dist le rrs^2,ninds)
indb=where((dist le rrb2^2) and (dist gt rrb1^2),nindb)
;
if (ninds ne 0) then list_s=list(inds) else list_s = -1  
if (nindb ne 0) then list_b=list(indb) else list_b = -1 
; if no events in filtered region, return -1
;
end

