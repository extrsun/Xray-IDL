pro off_ax_a_new,offax,ee,vig,ierr
;-
; calculate vignetting corrections using the file area.oar created with
; get_area.pro and the off-axis file rp*.oar
; inputs:
; offax - scalar or vector in units of arcminutes
; ee - ADC channels with dimension same as that of offax
; outputs:
; vig - the vignetting correction with dimension same as that of offax
; writen by WQD 5/13/93
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - off_ax_a_new,offax,ee,vig,ierr'
return
endif
;
ierr=0
if min(offax) lt 0. or max(offax) gt 60. or min(ee) lt 7. or max(ee) gt 247 $
	then begin
	ierr=1 
endif
ee=(ee < 247) > 7
if n_elements(ee) eq 1 and n_elements(offax) gt 1 then ev=offax*0.+ee else $
ev=ee
openr,7,'~/rosatdata/expmaps/area.oar',/f77
angle=fltarr(14)
area=fltarr(14,247)
readu,7,angle,area
close,7
vig=offax*0.
aa=fltarr(14)
for k=7,247 do begin
	c=where(nint(ev) eq k,nc)
	if nc ne 0 then begin
		aa(*)=area(*,k-1)
		linterp,angle,aa,offax(c),aac
		vig(c)=aac
	endif
endfor
end

