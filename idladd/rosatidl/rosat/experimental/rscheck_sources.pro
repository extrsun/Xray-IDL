;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
; NAME:
;	RSCHECK_SOURCES
;
; PURPOSE:
;       Check sources in SASS source table against objects in SIMBAD sky
;       table (also from SASS), and select those for which there is a
;       SIMBAD object which falls within a given search radius.
;
; CALLING SEQUENCE:
;	RSCHECK_SOURCES, obseq, SRCDAT, SKYDAT, dir=dir, proc=proc, $
;                        poserr=poserr, xdel=xdel, ydel=ydel, textout=textout
;
; INPUTS:
;	obseq  -  ROSAT SASS sequence number (string), e.g. 'RP200020'
;
; OPTIONAL INPUT PARAMETERS:
;	dir     - directory where file is located (if not current directory)
;       proc    - Format of processed files (e.g., US, MPE, RDF)
;                 If not supplied, assumes 'US'.
;       poserr  - Size of search radius (in arcsec). If not supplied, 
;                 assumes 150.
;       xdel    - optional X offset (in original pixels) to get from
;                 source positions to sky positions
;       ydel    - optional Y offset (in original pixels) to get from
;                 source positions to sky positions
;       textout - specifies output device.
;                 textout=1        TERMINAL using /more option
;                 textout=2        TERMINAL without /more option
;                 textout=3        <program>.prt
;                 textout=4        laser.tmp
;                 textout=5        user must open file
;                 textout = filename (default extension of .prt)
;
; OUTPUTS:
;	srcdat -  IDL structure with info from source FITS table.
;                 If not defined, srcdat is read from obseq+'_src.fits'
;	skydat -  IDL structure with info from source FITS table.
;                 If not defined, skydat is read from obseq+'_sky.fits'
;
; NOTES:
;  Now reads FITS file directly. Do NOT convert to pseudo SDAS format!
;
; RESTRICTIONS:
;  Procedure reads header from events table of .FITS file. The .fits, 
;     _src.fits, and _sky.fits files must all be in the same directory,
;     and must have names of the form obseq.fits, obseq_src.fits, and
;     obseq_sky.fits.
;  If the files do not follow this naming convention, then use rs_sources
;     to read srcdat & skydat, and use these as inputs to rscheck_sources.
;
; SUBROUTINES CALLED:
;  HEADFITS   (IDL Astronomical Users' Library)
;  RSGETPAR
;  RS_SOURCES
;  READFITS        "                "
;  ZPARCHECK       "                "
;  SXOPEN          "                "
;  SXHREAD         "                "
;  SXPAR           "                "
;  SXREAD          "                "
;  REMCHAR         "                "
;  GETTOK          "                "
;  CREATE_STRUCT
;  FTGET    (IDL Astronomical Users' Library)
;  FTSIZE          "                "
;  FTINFO          "                "
;  ADSTRING        "                "
;  RADEC           "                "
;  NINT            "                "
;
; MODIFICATION HISTORY:
;  written  06 Jul 1992 (GAR)
;  modified 05 Jan 1994 (GAR) to work with RDF format _src.fits files
;-
; ==================================================================
pro rscheck_sources,obseq,srcdat,skydat,dir=dir,proc=proc,poserr=poserr,$
    xdel=xdel,ydel=ydel,textout=textout
;
; procedure to check SASS sources agains SIMBAD objects
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RSCHECK_SOURCES, obseq, SRCDAT, SKYDAT, dir=dir, proc=proc,'
  print,'                  poserr=poserr (def = 150), xdel=xdel, ydel=ydel,'
  print,'                  textout=textout'
  retall
endif
if not keyword_set(textout) then textout=!textout
if (n_elements(dir) eq 0) then dir = ''
if (n_elements(proc) eq 0) then proc = ''
proc = strupcase(proc)
if (proc eq '') then proc = 'US'
;
; Check to make sure this is a supported data format
;
case proc of
  'US':  begin
         ext = '.fits'
         extnum = 3
         end
  'MPE': begin
         print,proc,' is not yet a supported format.'
         print,' Valid choices are US, RDF. Returning.'
         retall
         end
  'RDF': begin
         ext = '_bas.fits'
         extnum = 2
         end
  else : begin
         print,proc,' is not a valid choice for processing format.'
         print,' Valid choices are US, RDF. Returning.'
         retall
         end
endcase
textopen,'checksources',TEXTOUT=textout
;
; Read header from events table. Needed to convert RA & Dec from sky data
; structure to pixels.
;
match_files,dir,obseq,ext,name,nlist        ;look for the right files
name = name(0)                              ;use latest version
hev = headfits(name,ext=extnum)
case proc of
  'US':  extast,hev,cd,crpix,crval,noparams
  'RDF': rdf_extast,hev,cd,crpix,crval,noparams
endcase
if (!debug gt 2) then stop,' Stopping in RSCHECK_SOURCES after extast'
instr = fxpar(hev,'INSTRUME')               ;get the instrument
instr = strupcase(strtrim(instr,2))
instr = strmid(instr,0,1)                   ;just want the first letter
;
; Read source and sky data structures (if undefined)
;
if (n_elements(srcdat) eq 0) then begin
  dfile = 'rs_sources'
  rsgetpar,0,dfile,oparms              ;get input parameter default values
  oparms(0) = obseq
  oparms(1) = dir
  oparms(2) = 'src'
  oparms(3) = instr
  oparms(4) = proc                     ;processing format - US, MPE, or RDF
  if (!debug gt 5) then stop,' Stopping in RSCHECK_SOURCES before RS_SOURCES'
  rs_sources,oparms,hdr,srcdat         ;read source data structure
endif
if (!debug gt 2) then stop,' Stopping after srcdat defined'
if (n_elements(skydat) eq 0) then begin
  oparms(2) = 'sky'
  rs_sources,oparms,hdr,skydat         ;read sky data structure
endif
if (!debug gt 2) then stop,' Stopping after skydat defined'
hdr = 0
;
nsrc = n_elements(srcdat)
if (n_elements(poserr) eq 0) then pixerr = 150. else pixerr = poserr
if (n_elements(xdel) eq 0) then xdel = 0
if (n_elements(ydel) eq 0) then ydel = 0
pixerr = pixerr * 2.                         ;convert to pixels
if (n_elements(pixerr) eq 1) then pixerr = pixerr + fltarr(nsrc)
;
; Sort sources by count rate. Highest count rate sources will be first.
;
case proc of
  'US':  begin
         srcnum = srcdat.src
         ccts = srcdat.ccts
         ccts_err = srcdat.ccts_err
         psn0 = srcdat.psn0
         end
  'RDF': begin
         srcnum = srcdat.mplsx_id
         ccts = srcdat.net_rt
         ccts_err = srcdat.net_rt_err
         psn0 = srcdat.max_like
         end
endcase
ind = sort(ccts)
ind = ind(nsrc-1-indgen(nsrc))
srcdat = srcdat(ind)                        ;sort sources by count rate
if (!debug gt 2) then stop,' Stopping after srcdat sorted by count rate'
;
; Convert RA & Dec for sky (SIMBAD) objects to x & y pixels (0.5 arcsec)
;
rasky = skydat.ra
decsky = skydat.dec
;adxy,hev,rasky,decsky,xsky,ysky
if (!debug gt 2) then stop,' Stopping in RSCHECK_SOURCES before ad2xy'
cd = cd/!radeg  
crval = crval/!radeg
ad2xy,rasky/!radeg,decsky/!radeg,cd,crpix,crval,xsky,ysky
;
; For each source, find the closest SIMBAD object. Add the indices to the
; list if the minimum radius is <= pixerr (for that source).
; Use textout (which set !textunit) to direct output of results 
;
printf,!textunit,' Results of checking SASS versus SIMBAD sources for '$
      ,name
printf,!textunit,' Search radius ranges from ',min(pixerr)/2.,' to '$
      ,max(pixerr)/2.,' arcsec'
printf,!textunit,'  '
outstr = ' Coincidences found by adding '+string(f='$(f6.2)',xdel)+' and '
outstr = outstr+string(f='$(f6.2)',ydel)+' to Source X and Y'
printf,!textunit,outstr
printf,!textunit,'  '
outstr = '  Src Sky  Source X  Source Y    OffAng    Sky  X    Sky  Y'
outstr = outstr + '  Distance'
printf,!textunit,outstr
printf,!textunit,'  '
;
srcsel = [-1] & skysel = [-1]
xsrc = srcdat.x 
ysrc = srcdat.y 
offax = srcdat.offax
skynum = skydat.src
num = 0
if (!debug gt 2) then stop,' Stopping before source selection'
;
xtry = xsrc + xdel
ytry = ysrc + ydel
;xyad,hev,xtry,ytry,ratry,dectry
if (!debug gt 2) then stop,' Stopping in RSCHECK_SOURCES before xy2ad'
xy2ad,xsky,ysky,cd,crpix,crval,ratry,dectry
ratry = ratry*!radeg
dectry = dectry*!radeg
if (!debug gt 2) then stop,' Stopping in RSCHECK_SOURCES after xy2ad'
;
for n=0,nsrc-1 do begin
  rad=sqrt((xtry(n)-xsky)*(xtry(n)-xsky)+(ytry(n)-ysky)*(ytry(n)-ysky))
  ind=where(rad eq min(rad)) 
  ind=ind(0)
  if (rad(ind) le pixerr(n)) then begin
     srcsel = [srcsel,n]
     skysel = [skysel,ind]
     num = num + 1
     printf,!textunit,format='$(1x,2i4,6f10.3)',$
     srcnum(n),skynum(ind),$
     xsrc(n),ysrc(n),offax(n),xsky(ind),ysky(ind),rad(ind)
  endif
endfor
if (!debug gt 2) then stop,' Stopping after source selection'
;
if (num eq 0) then begin
  printf,!textunit,$
         ' No coincidences found within search radii'
  textclose,TEXTOUT=textout
  srcdat = 0
  skydat = 0
endif else begin
  srcsel = srcsel(1:*)                 ;strip off bogus initial values
  skysel = skysel(1:*)
  nsrc = num
  printf,!textunit,'  '
  printf,!textunit,$
         nsrc,' coincidences found within search radii'
;
; Now make a plot of the field (in RA and Dec). Draw circles around the
; sources and sky objects that were selected.
;
  rasrc = srcdat.ra 
  decsrc = srcdat.dec
  xbot = min(rasrc) < min(rasky)
  xtop = max(rasrc) > max(rasky)
  ybot = min(decsrc) < min(decsky)
  ytop = max(decsrc) > max(decsky)
  xmin = !xmin & xmax = !xmax
  ymin = !ymin & ymax = !ymax
  set_xy,xbot,xtop,ybot,ytop
;
  plot,ratry,dectry,psym=7,/xst,/yst
  oplot,rasky,decsky,psym=1
  ang = findgen(361)*3.14159/180.
  cosang = cos(ang) 
  sinang = sin(ang)
  for n=0,nsrc-1 do begin
    xc = pixerr(n)*cosang
    yc = pixerr(n)*sinang
;    xyad,hev,xtry(srcsel(n))+xc,ytry(srcsel(n))+yc,rapl,decpl
    xy2ad,xtry(srcsel(n))+xc,ytry(srcsel(n))+yc,cd,crpix,crval,rapl,decpl
    rapl = rapl*!radeg
    decpl = decpl*!radeg
    oplot,rapl,decpl
;    xyad,hev,xsky(skysel(n))+xc,ysky(skysel(n))+yc,rapl,decpl
    xy2ad,xsky(skysel(n))+xc,ysky(skysel(n))+yc,cd,crpix,crval,rapl,decpl
    rapl = rapl*!radeg
    decpl = decpl*!radeg
    oplot,rapl,decpl,line=1
  endfor
  !xmin = xmin & !xmax = xmax
  !ymin = ymin & !ymax = ymax
;
  srcdat = srcdat(srcsel)
  skydat = skydat(skysel)
  srcdat.ra = ratry(srcsel)
  srcdat.dec = dectry(srcsel)
  radec = strarr(nsrc)
  for n=0,nsrc-1 do radec(n) = adstring(ratry(srcsel(n)),dectry(srcsel(n)))
  srcdat.radec = radec
;
; Now write more information to !textunit
;
  printf,!textunit,'  '
  outstr = ' Src Num    Source RA     Dec'
  outstr = outstr + '         Rate (cts/s) +/- Err       Src Prob.'
  printf,!textunit,outstr
  outstr = ' Sky Num    SIMBAD RA     Dec'
  outstr = outstr + '    Object ID  Name     Spmor  Bmag   Dist
  printf,!textunit,outstr
  printf,!textunit,'  '
  forprint,textout=5 $
     ,fix(srcnum),'  '+srcdat.radec,ccts,ccts_err $
     ,psn0,fix(skynum),'  '+skydat.radec,fix(skydat.id) $
     ,'  '+skydat.obj,' '+skydat.spmor,' '+skydat.bmag,' '+skydat.dist
  textclose,TEXTOUT=textout
endelse
;
return         ;pro rscheck_sources
end
