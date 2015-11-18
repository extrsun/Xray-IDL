pro get_gti,fname,ext,gti,tlo,thi,hdr=hdr
;+
;
; get the good time intervals from an extension 
;
;*INPUTS:
; fname - event file name (e.g., 'acisf00945N001_evt2.fits')
; ext - extension number (starts from 0)
;
;*OUTPUTS:
; gti - array containing the good time intervals:
;	column 0: start time
;	column 1: stop time
;
; hdr - original fits header of the extension
; tlo,thi - lower and upper time limits of the gti
;
;written by wqd, 6/22/2001
;
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_gti,fname,ext,gti,tlo,thi,hdr=hdr'
return
endif
;On_error,2    
if n_elements(fdir) eq 0 then fdir=!data_dir
tab=readfits(fname,ext=ext,hdr)
print,'HDUNAME = ',sxpar(hdr,'HDUNAME')
print,'Is the above the extension that you want?'

nr=sxpar(hdr,'naxis2')
gti=dblarr(2,nr)
gti(0,*)=fits_get(hdr,tab,'start')
gti(1,*)=fits_get(hdr,tab,'stop')

print,'GTI intervals:'
print,gti(0,1:nr-1)-gti(0,0:nr-2)
print,'Gaps between GTI intervals:'
print,gti(0,1:nr-1)-gti(1,0:nr-2)
tlo=gti(0,0)
thi=gti(1,nr-1)
return
end