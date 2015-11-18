pro file_params_xmmm,fname,dhdr,cra,cdec,expt,roll,hdr=hdr
;+
; get basic data file information and create a simple fits header
;
;*INPUTS:
; fname - event file name (e.g., 'acisf00945N001_evt2.fits')
;*OUTPUTS:
; dhdr - fits header of the data
; cra, cdec - RA and Dec of the reference (aiming) point (deg)
; expt - exposure time (s)
; roll - pointing roll angle of the observation (deg)
; hdr - original fits header of the event file
;
;written by wqd, 6/4/2001
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - file_params_xmmm,fname,dhdr,cra,cdec,expt,roll,hdr=hdr'
return
endif
;On_error,2
if n_elements(fdir) eq 0 then fdir=!data_dir
if n_elements(tail) eq 0 then tail=''
if n_elements(fname) eq 0 then fname=!seq_no+tail+'_evt2.fits'
hdr=headfits(fname,ext=1)
if !err eq -7 then stop,'there is a problem in reading the fits header!'

;find the reference coordinates:
ctype=sxpar(hdr,'REFXCTYP*',count=nc)
if nc eq 0 then stop,'there is no fits key word: '+'REFXCTYP*'
;sel=where(ctype eq 'RA---TAN',nsel)
;if nsel eq 0 then stop,'there is no fits key word: '+'RA---TAN'
cra=sxpar(hdr,'REFXCRVAL')
cdec=sxpar(hdr,'REFYCRVAL')
ddim=long(!pref)*2
get_fitshead,0,dhdr,equi=2000,crval=[cra,cdec],del=!size_pixel/3600.,cpx=!pref,cpy=!pref,dim=ddim,type=2

;exposure and roll angle:
expt=sxpar(hdr,'LIVETIME',count=nc)
if nc eq 0 then stop,'there is no fits key word: '+'LIVETIMEEXPOSURE'
roll=sxpar(hdr,'PA_PNT',count=nc)
if nc eq 0 then roll=sxpar(hdr,'ROLL_NOM',count=nc)
if nc eq 0 then begin
	print,'there is no fits key word: '+'ROLL_PNT'
	roll=0.
endif
if !debug eq 2 then stop
return
end
