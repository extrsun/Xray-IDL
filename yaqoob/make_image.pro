;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;            make_image
;
;*PURPOSE:
; A procedure to convert a photon list into a 2 dimensional image.
; If the photon list is not defined, then an input FITS file must be
; specified. Various output image sizes, pixel ranges, and blocking factors
; can be specified. 
; The output image may either be a map of the photon intensity, or of
; the average PHI (pulse height invariant) channel in each output pixel.
;
;*CALLING SEQUENCE:
;	make_image,inputs,image,imginfo,plist,plinfo,plhdr,oparms=oparms,$
;                  listparms=listparms
;
;*PARAMETERS:
; INPUTS:
;       inputs - input parameter description.
;                0 - use all defaults
;                1 - interative selection of parameters
;                string - parameter defintion of the form
;                   'parname1=value1,parname2=value2,,,,'
;                string array - each element of the array has
;                   form 'value' (entries must be given in correct order)
;                string of form '$filename' where filename is the
;                   name of the file containing one parameter
;                   per line in the form: parname=value
;       Any parameter not specified is set to its default.
;       Defaults for the parameters can be found by using interactive
;       selection params=1 or examinining the default (text) files,
;       ZDEF:MAKE_IMAGE.DEF.
;       The following parameters are availble.
;
;        PLINP       If PLIST is not defined, then tells how inputs for
;                    list selection will be defined. Set to 1 (interactive
;                    input) or to the name of a .def file in directory ZDEF
;                    If keyword LISTPARMS is set, then PLINP will be ignored
;        IMGFIL      (Full) name of output PSF FITS image (or NONE)
;                    (default extension is '_img.fits')
;        ZVAL        Value along z-axis. Can be photon intensity (=INT)
;                    or average PHI channel (=PHI)
;        INSTR       Instrument 
;                    (default is P for PSPC)
;        IMTYP       Tells whether output image will be in detector (DET) 
;                    or sky (SKY) coordinates
;                    (default = SKY, or same as for PLIST)
;        XCEN        X coordinate for center of output image
;        YCEN        Y coordinate for center of output image
;                    if set to 0, then use either --
;                       1) the on axis center (if PLIST is not defined)
;                       2) the center of the list (if PLIST is defined)
;        PIXEL       Pixel convention. = UNBL for unblocked, BL for blocked
;                    pixels. (def = UNBL)
;        BLOCK       Blocking factor for pixels. (def = 1 for UNBL pixels)
;        BINSIZ      Size of output image pixel (in arcsec; default = 15)
;        NXBIN       Number of image pixels in X direction
;        NYBIN       Number of image pixels in Y direction
;                    For memory reasons, NXBIN and NYBIN must be <= 1024
;        TMIN        Start time of interval to be included
;                    default = 0 means start time of list
;        TMAX        End time of interval to be included
;                    default = 0 means end time of list
;        PIMIN       Minimum PHI channel to be included 
;                    default = 0 means minimum PHI channel of list events
;        PIMAX       Maximum PHI channel to be included
;                    default = 0 means maximum PHI channel of list events
;        CHATTER     Controls program feedback to user ("chattiness")
;                    (default = 1)
;
; OPTIONAL INPUTS:
;        PLIST       A structure variable containing the information from 
;                    a photon list (e.g., as created by makelist)
;        PLINFO      Data structure containing info concerning extraction.
;                    of the events list
;                    If PLIST is defined, then PLINFO must also be given
;        PLHDR       The FITS header for the events list FITS file
;                    If PLIST is defined, then PLHDR must also be given
;                    If PLIST is not defined, then the procedure will define 
;                    PLIST, PLINFO, and PLHDR
;        LISTPARMS   Keyword containing parsed form of inputs used for plist
;                    selection. LISTPARMS controls the inputs
;                    OBSEQ, DIR, EXTYP, PROC, TRIM, PIXEL, BLOCK, CTYP,
;                    XMIN, XMAX, YMIN, YMAX, TMIN, TMAX for plist
;
;                    If PLIST is not defined, then 
;                      1) input PLINP (in OPARMS) should be set to 1
;                         (interactive mode)
;                      2) input PLINP should be set to the name of a .def
;                         file in directory ZDEF
;                      3) the keyword LISTPARMS must be defined acc to 
;                         syntax in example 3
;                    If PLIST is defined, then LISTPARMS is ignored
;
; OUTPUTS:
;        IMAGE       The output image.
;        IMGINFO     Data structure containing info concerning creation and
;                    characteristics of the image (relevant info from
;                    PLINFO and PLHDR)
;        OPARMS      String array which is the parsed form of inputs (same as
;                    LIST in example 3 below). Allows program to be rerun using 
;                    same values w/o resetting.
;
;*EXAMPLES: There are 4 different ways that the inputs may be specified
;    mode 1) Interactive
;        IDL>make_image,1,image,op=oparms
;             ?obseq=rp123456
;	      ?dir=mydirectory
;             ?instr=H
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydirectory,instr=H'
;        IDL>make_image,1,list,image,oparms
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,25)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydirectory'
;        IDL>list(3)='H'
;        IDL>make_image,1,list,image,oparms
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;	     instr=P
;            *exit
;        IDL>make_image,1,list,image,oparms
;
;*RESTRICTIONS:
;    For memory reasons, XSIZE and YSIZE must be <= 1024.
;
;*NOTES:
;
;*SUBROUTINES CALLED:
;  RSGETPAR
;  MATCH_FILES
;  GET_LIST
;  TBREAD
;  TBGET
;  LIST2IMG
;  XYIMAGE
;  MAKE_ARRAY
;
;*MODIFICATION HISTORY:
;    written  9 Jan 1993 by GAR
;    modified 15 Feb 1993 (GAR) to be compatible with new MAKE_LIST
;    modified 24 Feb 1993 (GAR) to change index variable in for loop to
;       longword integer
;-
;-------------------------------------------------------------------------------
pro make_image,inputs,image,imginfo,plist,plinfo,plhdr,oparms=oparms,$
               listparms=listparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' MAKE_IMAGE, inputs, IMAGE, IMGINFO, plist, plinfo, plhdr,' $
       +' OPARMS=OPARMS, '
  print,'             listparms = listparms'
  print,'   '
  print,'   Uses inputs PLINP, IMGFIL, ZVAL, INSTR, IMTYP, XCEN, YCEN, PIXEL,' $
       +' BLOCK, '
  print,'               BINSIZ, NXBIN, NYBIN, PIMIN, PIMAX, TMIN, TMAX, and' $ 
       +' CHATTER'
  print,'   from make_image.def'
  print,'  '
  print,' If PLIST is not defined, then input PLINP or keyword LISTPARMS' $
       +' must be defined'
  print,'  '
  print,' If PLIST is defined, then PLINFO and PLHDR must be defined as well'
  print,'   '
  retall
endif
;
; test whether PLIST is defined. If it is, then PLINFO and PLHDR must be
; as well
;
getlist = 1                      ;call make_list to create events list?
if (npar gt 3) then begin        ;PLIST included in call statement
  check = n_elements(plist)
  if (check ne 0) then begin          ;PLIST is defined
    getlist = 0                       ;so don't call make_list
    check2 = n_elements(plinfo)       ;PLINFO must also be defined
    if (check2 eq 0) then begin
      print,' PLINFO is not defined. Check inputs and try again. Returning.'
      retall & endif
;
    check2 = n_elements(plhdr)        ;and so must PLHDR
    if (check2 eq 0) then begin
      print,' PLHDR is not defined. Check inputs and try again. Returning.'
      retall & endif
  endif
endif 
;
dfile = 'make_image'
rsgetpar,inputs,dfile,oparms
zval = strupcase( strtrim(oparms(2),2) )
if (zval eq '') then zval = 'INT'         ;default is image of intensities
chatter = fix(oparms(29))
;
if (getlist) then begin                   ;PLIST is not defined
  plinp = strtrim(oparms(0),2)            
  if (plinp eq '') then begin             ;LISTPARMS must be defined
    check = n_elements(listparms) 
    if (check eq 0) then begin
      print,' Either input PLINP or keyword LISTPARMS must be defined.'
      print,' Please check your parameters. Returning.'
      retall & endif
  endif else begin
    if ( (plinp eq '1') or (plinp eq '0') ) then plinp = fix(plinp)
    rsgetpar,plinp,'make_list',listparms  ;use PLINP to define LISTPARMS
  endelse
;
  def_listparms,oparms,listparms          ;use OPARMS to fill in blanks
  if (!debug eq 1) then $
     stop,'Stopping in make_image after listparms defined.'
  make_list,listparms,plist,plinfo,plhdr  ;define PLIST, PLINFO, and PLHDR
endif 
;
imtyp = strupcase(strtrim(oparms(4),2))
if (imtyp eq '') then imtyp = plinfo.ctyp
tmin = double(oparms(21))
if (tmin eq 0) then tmin = min(plist.time)
tmax = double(oparms(22))
if (tmax eq 0) then tmax = max(plist.time)
pimin = double(oparms(23))
if (pimin eq 0) then pimin = min(plist.pi)
pimax = double(oparms(24))
if (pimax eq 0) then pimax = max(plist.pi)
;
xcen = float(oparms(6))
ycen = float(oparms(7))
pixel = strtrim(oparms(8),2)
if (pixel eq '') then pixel = 'UNBL'      ;default is for unblocked pixels
if (strupcase(pixel) ne 'UNBL') then begin
  block = fix(oparms(9))
  if (block eq 0) then block = 1
endif else block = 1
xcen = xcen*block
ycen = ycen*block
;
instr = strupcase(strtrim(oparms(3),2))
pixsiz = 0.5
if (imtyp ne 'SKY') then begin            ;detector coordinates
  pixsiz = 1.0                            
  if (instr ne 'H') then pixsiz = 0.5*7680./4096.
endif
;
binsize = float(oparms(12))
nxbin = float(oparms(13))
nybin = float(oparms(14))
;
binrat = binsize/pixsiz
if (binrat ne fix(binrat)) then begin
  binrat = fix(binrat)
  if (chatter eq 1) then $
     print,' Changing bin size from ',binsize,' to ',binrat*pixsiz
  binsize = binrat*pixsiz
  oparms(12) = strtrim(string(binsize),2)
endif
;
; now use PLINFO to redefine XCEN, YCEN, if necessary (& if PLINFO was
; originally defined)
;
if (getlist eq 0) then begin
  if (xcen eq 0) then xcen = (plinfo.xmin + plinfo.xmax)/2.
  if (ycen eq 0) then ycen = (plinfo.ymin + plinfo.ymax)/2.
endif
xrange = binrat*nxbin
yrange = binrat*nybin
if (fix(binrat/2.) ne (binrat/2.)) then begin        ;binrat is odd
  xcen = fix(xcen) + 0.5 
  ycen = fix(ycen) + 0.5
endif else begin
  xcen = nint(xcen)
  ycen = nint(ycen)
endelse
ndel =  fix(nxbin/2.)
xbot = xcen - binrat/2. - binrat*ndel
ndel =  fix((nxbin-1)/2.)
xtop = xcen + binrat/2. + binrat*ndel
ndel =  fix(nybin/2.)
ybot = ycen - binrat/2. - binrat*ndel
ndel =  fix((nybin-1)/2.)
ytop = ycen + binrat/2. + binrat*ndel
;
if (!debug eq 1) then $
   stop,'Stopping in make_image after X, Y  centers defined.'
;
; Now accumulate image (intensity or average energy,as set by ZVAL)
;
if (imtyp ne 'SKY') then begin
  ix = plist.dx 
  iy = plist.dy
endif else begin
  ix = plist.x 
  iy = plist.y
endelse
pi = plist.pi
;
if ( (tmin gt min(plist.time)) or (tmax lt max(plist.time)) ) then begin
  if (chatter eq 1) then print,' Selecting events for image in time'
  isel = where( (plist.time ge tmin) and (plist.time le tmax),nsel)
  if (nsel gt 0) then begin
    ix = ix(isel)
    iy = iy(isel)
    pi = pi(isel)
  endif else begin
    print,' No events within specified time range. Returning.'
    retall
  endelse
endif
;
if ( (pimin gt min(pi)) or (pimax lt max(pi)) ) then begin
  if (chatter eq 1) then print,' Selecting events for image in PI'
  isel = where( (pi ge pimin) and (pi le pimax),nsel)
  if (nsel gt 0) then begin
    ix = ix(isel)
    iy = iy(isel)
    pi = pi(isel)
  endif else begin
    print,' No events within specified PI channel range. Returning.'
    retall
  endelse
endif
;
if (chatter eq 1) then print,' Selecting events for image in position'
isel = where( (ix ge xbot) and (ix lt xtop) and (iy ge ybot) and $
              (iy lt ytop), nsel)
if (nsel gt 0) then begin
  ix = fix( (ix(isel) - xbot)/binrat )        ;image position in X direction
  iy = fix( (iy(isel) - ybot)/binrat )        ;image position in Y direction
  pi = pi(isel)
endif else begin
  print,' No events within specified X and Y ranges. Returning.'
  retall
endelse
;
loc_old = ix + nybin * iy                    ;compute Location indices.
get_posi,loc_old,loci,Kdup,nloci
if (zval ne 'INT') then begin
  if (chatter eq 1) then $
     print,' Now sorting locations for calculation of average PHI channel'
  isort = sort(loc_old)           ;will need this to get avg PI at each loci
  pi = pi(isort)
  isort = 0
endif
if (nloci ge 1) then begin
  image = intarr(nxbin,nybin)
  if (chatter eq 1) then print,' Now accumulating image'
  kbeg = 0
  for ii=0L,nloci-1 do begin
    if (zval ne 'INT') then begin
       kend = kbeg + kdup(ii) - 1
       addval = avg( pi(kbeg:kend) )
       kbeg = kbeg + kdup(ii)
    endif else addval = kdup(ii)      
    image(loci(ii)) = image(loci(ii)) + addval
  endfor
endif else image = 0
;
return
end         ;pro make_image
