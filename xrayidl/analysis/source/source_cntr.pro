pro source_cntr,xdv,ydv,piv,etime,cntr,cntre
;+
; get source cntr based on a list of counts and the analytical vigneting
; piv is not used for RHRI
;-
if !instr eq 'p' then begin
dcx=4119. ;see the key words xs_xdopt in the header of *.fits 
dcy=3929.
pixel_size=0.93420756 ;arcsec, or 14".947/16
endif else begin
dcx=2224.1 ;see the key words xs_xdopt in the header of *.fits
dcy=2084.8 
pixel_size=0.6 ;arcsec; see the keyword xs_inpxx
endelse
;
; calculate the effective exposure time
offax=sqrt((xdv-dcx)*(xdv-dcx)+(ydv-dcy)*(ydv-dcy) > 1.e-9) $
	*(pixel_size/60.)		;in units of arcminutes
;if !instr eq 'p' then begin
;off_ax_a_new,offax,piv,vig,ierr 
;if ierr eq 1 then stop,'ierr = 1 in off_ax_a_new' 
;	print,'No vigenting is done; area.oar file in OFF_AX_A_NEW is corrupted; 	because of conversion from Salaries to Lunix.'
;	vig=offax*0.+1
;endif else 
vig=1.-1.49e-3*offax-3.07e-4*offax^2 ;David et al. 1993
cntr=total(1./vig)/etime ; the total count rate in the entire energy range
cntre=cntr*sqrt(1./n_elements(piv))
if !debug eq 1 then stop
end