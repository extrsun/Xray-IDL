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
;        IMGFIL      (Full) name of output file for FITS image (or NONE)
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
;  MAKE_IMG_HDR
;
;*MODIFICATION HISTORY:
;    written  9 Jan 1993 by GAR
;    modified 15 Feb 1993 (GAR) to be compatible with new MAKE_LIST
;    modified 24 Feb 1993 (GAR) to change index variable in for loop to
;       longword integer
;    modified 12 May 1993 (GAR) to be compatible with make_img_hdr
;    modified 09 Aug 1993 (GAR) to add the accepted time intervals and
;      total exposure time to the structure variable imginfo
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
imgfil = strtrim(oparms(1),2)             ;name of output FITS image
imgfiluc = strupcase(imgfil)
;
plinp = strtrim(oparms(0),2)     ;PLINP must be def if LISTPARMS, PLIST not
if (getlist) then begin                   ;PLIST is not defined
  check = n_elements(listparms)           ;Is LISTPARMS defined?
  if (check eq 0) then begin              
    if (plinp eq '') then begin   
      print,' Either input PLINP or keyword LISTPARMS must be defined.'
      print,' Please check your parameters. Returning.'
      retall & endif
;
    if ( (plinp eq '1') or (plinp eq '0') ) then plinp = fix(plinp)
    rsgetpar,plinp,'make_list',listparms  ;use PLINP to define LISTPARMS
  endif 
;
  savlistp = listparms
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
;xbot = xcen - binrat/2. - binrat*ndel
xbot = xcen - binrat*ndel
;ndel =  fix((nxbin-1)/2.)
;xtop = xcen + binrat/2. + binrat*ndel
xtop = xcen + binrat*ndel
ndel =  fix(nybin/2.)
;ybot = ycen - binrat/2. - binrat*ndel
ybot = ycen - binrat*ndel
;ndel =  fix((nybin-1)/2.)
;ytop = ycen + binrat/2. + binrat*ndel
ytop = ycen + binrat*ndel
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
; Find the unique values of loc = ix + nxbin*iy
; Use GET_POSI if only an intensity map is desired, as this is fastest
; If we need to know, say, the average pi value for each pixel, then we will
; need to use VSORT or VSORT_LARGE to get the indices of the unique values
; of loc in the ix, iy vectors.
;
nsize = 10000.                   ;size of largest section for loc sorting
if (!version.os eq 'vms') then nsize = 2*nsize  
multloc = 1.0D0
if (chatter eq 1) then print,' Finding unique values of X, Y location.'
;
loc_old = ix + nxbin * iy                    ;compute Location indices.
nloc = n_elements(loc_old)
;
if (zval eq 'INT') then get_posi,loc_old,loci,Kdup,nloci else begin
  if (nloc le nsize) then begin
    if (!debug ge 2) then print,' Sorting in one section, using VSORT'
    vsort,loc_old,multloc,loci,isort,kbeg,kend,nloci
  endif else begin
    if (!debug ge 2) then print,' Sorting in sections, using VSORT_LARGE'
    vsort_large,loc_old,multloc,loci,isort,kbeg,kend,nloci,nsize=nsize
  endelse
endelse
;
if (nloci ge 1) then begin
  image = intarr(nxbin,nybin)
  if (chatter eq 1) then print,' Now accumulating image'
;
  if (zval ne 'INT') then pi = pi(indsrt)
  isort = 0
;
  for ii=0L,nloci-1 do begin
    if (zval ne 'INT') then addval = avg( pi(kbeg(ii):kend(ii)) ) else $
                            addval = kdup(ii)      
    image(loci(ii)) = image(loci(ii)) + addval
  endfor
;  
; now store information in structure variable imginfo
;
  imginfo = {xyimg,obseq:'',filename:'',proc:'',ctyp:'',zval:'', $
            xcen:0.0,ycen:0.0,binsiz:0.0,nxbin:0.0,nybin:0.0,$
            xmin:0.0,xmax:0.0,ymin:0.0,ymax:0.0,region:'',numpix:0.0D0, $
            pimin:0.0,pimax:0.0,totexp:0.0D0,$
            ntimes:0,tbeg:dblarr(200),tend:dblarr(200)}
;
  imginfo.obseq = plinfo.obseq
  imginfo.filename = plinfo.filename
  imginfo.proc = plinfo.proc
  imginfo.ctyp = imtyp
  imginfo.zval = zval
;
  imginfo.xcen = xcen
  imginfo.ycen = ycen
  imginfo.binsiz = binsize
  imginfo.nxbin = nxbin
  imginfo.nybin = nybin
;
  imginfo.xmin = xbot
  imginfo.xmax = xtop
  imginfo.ymin = ybot
  imginfo.ymax = ytop
;
  f = '$(f12.3)'
  region = 'box ('+string(form=f,xcen)+','+string(form=f,ycen)+','
  region = region+string(form=f,xtop-xbot)+','+string(form=f,ytop-ybot)+')'
  imginfo.region = region
  imginfo.numpix = (ytop - ybot + 1)*(xtop - xbot + 1)
;
  imginfo.pimin = plinfo.pimin > pimin
  imginfo.pimax = plinfo.pimax < pimax
;
  tbeg = plinfo.tbeg
  tend = plinfo.tend
  ntimes = plinfo.ntimes
  tbeg = tbeg(0:ntimes-1)
  tend = tend(0:ntimes-1)
  timeintsect,tmin,tmax,tbeg,tend,newtbeg,newtend,nact
  nact = nact < 200
  imginfo.ntimes = nact
  imginfo.tbeg = newtbeg(0:nact-1)
  imginfo.tend = newtend(0:nact-1)
  imginfo.totexp = total(newtend - newtbeg + 1)
;
  if (chatter eq 1) then print,' Finished accumulating image'
;
  if (imgfiluc ne 'NONE') then begin           ;if writing a FITS file
    fdecomp,imgfil,disk2,dir2,name,ext,ver
    if (name eq '') then name = obseq          ;default is to use obseq
    if (ext eq '') then ext = '_img.fits'
    imgfil = disk2+dir2+name+ext
    make_img_hdr,image,imginfo,instr,imgfil,imghdr
;
; Now write accumulated image to output FITS file (imgfil) if desired
;
    map = image
    outhdr = imghdr
    if (chatter eq 1) then $
      print,' Writing image FITS file ',imgfil
    writefits,imgfil,map,outhdr
    map = 0
  endif
;        
endif else image = 0
;
;if (getlist) then listparms = savlistp
;        
if (chatter eq 1) then print,' Normal Termination.'
;
return
end         ;pro make_image
