;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME
; getspec
;
;*PURPOSE:
; extract spectral data from ROSAT PSPC data files and 
; export the spectral data in the SF format for spectral analysis with XSPEC
;
;*CALLING SEQUENCE:
; getspec,[rate_s=,rate_b=,dir=,seq_no=,source_no=,radius_s=,radius_b1=,radius_b2=,silence=]
;
;*PARAMETERS:
;  dir - the data directory of the PSPC files (default: dir=!data_dir)
;  seq_no - the PSPC file sequence No (i.e., rp123456) (default: seq_no=!seq_no)
;  radius_s, radius_b1, radius_b2 - the radii of the source circle and 
;            the background annulus (default: radius_* =!radius_*)
;  silence - if set, the program will not ask for confirmation of the obove
;            parameters
;
;* DATA files used
;  seq_no.fits
;  seq_no_im1.fits
;  seq_no_mex.fits
; 
; OUTPUTS
; seq_no_source_no_s.pha
; seq_no_source_no_b.pha
;
;*PROCEDURE:
; If the source_no (mplsx No) is correctly given in !source_no, the 
; program finds the source location from the file seq_no_src.fits.
; Otherwise, you need to supply the RA and DEC of the source or point
; the source out on the screen.
;
; The program then uses the extraction radii (!radius_*) to
; extract the source and background spectral data after all other sources
; have been subtracted.
;
; Finally, the program exports the data in the SF format.
;
;*EXAMPLES:
; getspec
;
;*RESTRICTIONS:
; on-axis sources
; fixed spectral extracting radii
;
;*NOTES:
; The exposure map (seq_no_mex) used is good only for a single energy band.
; For an off-axis source, this exposure map may have to be replaced.
;
;*SUBROUTINES CALLED:
; tvimage - show the count image on the screen
; source_read - read source information (structure)
; sourceid_get - get the source's identity using the source's RA and DEC
; sourceid_tv - get the source's identity by pointing it on the screen
; source_excl - exclude the source from the source list
; source_sub - subtract sources in the source list from the exposure image.
; spec_get - extract the source and background count rates and export
;
;*MODIFICATION HISTORY:
; writen Aug 17 1992 (WQD)
;
;-
;------------------------------------------------------------------------------
pro getspec,dir=dir,seq_no=seq_no,source_no=source_no,radius_s=radius_s $
    ,radius_b1=radius_b1,radius_b2=radius_b2,rate_s=rate_s,rate_b=rate_b $
    ,silence=silence
;
; The following sysv are defined in specsysv.pro
if n_elements(dir) ne 0 then !data_dir=dir
if n_elements(seq_no) ne 0 then !seq_no=seq_no
if n_elements(source_no) ne 0 then !source_no=source_no
if n_elements(radius_s) ne 0 then !radius_s=radius_s
if n_elements(radius_b1) ne 0 then !radius_b1=radius_b1
if n_elements(radius_b2) ne 0 then !radius_b2=radius_b2
if n_elements(silence) ne 0 then !silence=silence
;
; When you are not confident in the setup, Make the !silence=0
;if !silence eq 0 then begin
print,'Here is a list of the defaults.'
print,'If anything is not right, please make it right'
print,''
print,'!data_dir = ',!data_dir
print,'!seq_no = ',!seq_no
print,'!source_no = ',!source_no,"; make !source_no='' to get !source_no on the screen"
print,''
print,'Default radii for spectral data extractions:'
print,'!radius_s = ',!radius_s
print,'!radius_b1 = ',!radius_b1
print,'!radius_b2 = ',!radius_b2
print,''
print,'If everything is now all right, please type .c to continue'
stop
;endif
;
print,"Let's get some information about the count and exposure images"
print,'If !silence eq 0, the count image will appear on the screen'
tvimage,image_c,h,image_t,h
;
; get exposure
; 
etime=sxpar(h,'xs-livti')
if etime eq 0. then etime=sxpar(h,'xs-onti')
;
print,"get source list (structure)"
source_read,struct
print,'ignore the above mess which is caused by the improper fits format'
;
print,'mplsx source number will be used'
  source_id=struct.mplsx_src
  source_xp=struct.im_x
  source_yp=struct.im_y
  source_ra=struct.ra/!radeg
  source_dec=struct.dec/!radeg

;
; if !source_no is not available, get it from RA and DEC or from the screen
if strtrim(!source_no) eq 0 then  begin
;
yn=''
read,'Do you know the RA and DEC of the source?',yn
yesno,yn
;
 if yn eq 1 then begin
  sourceid_get,source_id,source_ra,source_dec,sourceid
 endif else begin
 print,"Let's get the source position by poiting it on the screen"
  sourceid_tv,source_id,source_xp,source_yp,sourceid
 endelse
;
  !source_no=fix(strtrim(source_id(sourceid)))
endif else sourceid=where(source_id eq !source_no)	
xs_pix=float(source_xp(sourceid))
ys_pix=float(source_yp(sourceid))
;
; get an exposure image with sources (!source_no is excluded) removed.
crval=sxpar(h,'crval*')/!radeg ; convert degree into radian
	image_ra=crval(0)
	image_dec=crval(1)
source_excl=!source_no
source_excl,source_id,source_ra,source_dec,source_excl ;exclude source_excl from the lists
;
image_t=source_sub(image_t,image_ra,image_dec,source_ra,source_dec,factor=1.)
;
; finally, let's get the spectra and export them in the SF format
;
spec_get,!source_no,xs_pix,ys_pix,image_t=image_t,etime=etime,rate_s=rate_s,rate_b=rate_b
;
if !debug eq 1 then stop
end ;Pro getspec
