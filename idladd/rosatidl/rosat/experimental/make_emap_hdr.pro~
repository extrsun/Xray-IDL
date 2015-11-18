;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   make_emap_hdr
;
;*PURPOSE:
;   A procedure to make a header for an exposure map image
;
;*CALLING SEQUENCE:
;   make_emap_hdr,hdmap,poehdr,proc,actime,emaphdr
;
;*PARAMETERS:
; INPUTS:
;   hdmap    - FITS header of detector map used to create exposure map
;   poehdr   - FITS header for position ordered events table
;   proc     - Format of processed files (def = 'US', or 'MPE')
;   actime   - Array containing start and stop times of accepted time 
;              intervals
;              actimes(*,0) = start times, actimes(*,1) = stop times
;
;*OPTIONAL INPUTS:
;
; OUTPUTS:
;   emaphdr   - FITS header for exposure map FITS file
;
;*NOTES: 
;
;*SUBROUTINES CALLED:
;  HEADFITS
;  FXPAR
;  FXADDPAR
;  SXDELPAR
;
;*MODIFICATION HISTORY:
;    written  02 Sep 1992 by GAR
;    modified 20 Sep 1992 (GAR) to add accepted times to history records
;    modified 07 Aug 1993 (GAR) to add information about the shift in
;       x & y to the output header
;    modified 14 Sep 1993 (GAR) to take info about shift out, as the shift
;       is no longer necessary
;    modified 27 Sep 1993 (GAR) to start from detector map FITS header,
;       to use Bill Thompson's FX... routines, to extract header info from
;       MPE as well as US processed events table headers, and to make a
;       proper FITS header
;    modified 19 Nov 1993 (GAR) to add keywords to conform to OGIP standards
;    modified 30 Dec 1993 (GAR) to work with RDF format headers as well
;    modified 02 Feb 1994 (GAR) to work with files that have been processed
;      in qp format by PROS 2.3 and written back out to FITS format
;-
;-------------------------------------------------------------------------------
pro make_emap_hdr,hdmap,poehdr,proc,actime,emaphdr
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' MAKE_EMAP_HDR, hdmap, poehdr, proc ("US"),' $
       +' actime, EMAPHDR'
  retall
endif
if (proc eq '') then proc = 'US'      ;default is US format
;
actbeg = actime(*,0)
actend = actime(*,1)
tstart = min(actbeg)
tstop = max(actend)
ontime = total(actend - actbeg)
;
; look at systime(0) to construct creation date
;
months = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct',$
          'Nov','Dec']
stime = systime(0)
mm = strmid(stime,4,3)
im = where(months eq mm)
mm = strtrim(string(im(0) + 1),2)
if (strlen(mm) eq 1) then mm = '0'+mm
dd = strmid(stime,8,2)
if (strlen(dd) eq 1) then dd = '0'+dd
yy = strmid(stime,22,2)
creation = dd+'/'+mm+'/'+yy
if (!debug gt 3) then stop,' Stopping in MAKE_EMAP_HDR after creation date'
;
; Get keywords from detector map header
;
emaphdr = hdmap(0:28)
emaphdr = [emaphdr,'   ']
sxdelpar,emaphdr,'origin'
sxdelpar,emaphdr,'pixsize'
pixsize = fxpar(hdmap,'pixsize')
blkfact = fxpar(hdmap,'blkfact')
pixdet = pixsize/blkfact
dimdet = 512*blkfact
xdopt = fxpar(hdmap,'crval1')
ydopt = fxpar(hdmap,'crval2')
if (!debug gt 3) then stop,' Stopping in MAKE_EMAP_HDR after ydopt defined'
;
; Now get remaining keywords from events table header
; PROS 2.3 will update the header keywords of any processed FITS files to 
; their RDF names (but will not change the structure of the file)
;
prochdr = proc
case proc of          ;see if header has been updated to RDF
  'US':  begin
         testproc = fxpar(poehdr,'xs-mode')       
         if (!err lt 0) then prochdr = 'RDF'
         end
;
  'MPE': begin
         gethistval,poehdr,'FILTER_ID',testproc,lnum
         if (lnum lt 0) then prochdr = 'RDF'
         end
;
  'RDF': continue
;
  else:  begin
         print,proc,' is not a valid choice for processing format.'
         print,' Valid choices are US, MPE, RDF. Returning.'
         retall
         end
endcase
;
case prochdr of
  'US':  begin
         radecsys = fxpar(poehdr,'radecsys') 
         equinox = fxpar(poehdr,'equinox')
         equinox = string(equinox,'$(e12.6)')
         ctype1 = fxpar(poehdr,'ctype1')
         ctype2 = fxpar(poehdr,'ctype2')
         if (!debug gt 3) then $ 
            stop,' Stopping in MAKE_EMAP_HDR after ctype2 defined'
;
         rapt = fxpar(poehdr,'xs-rapt')
         decpt = fxpar(poehdr,'xs-decpt')
;
         crval1 = fxpar(poehdr,'crval1')
         crval2 = fxpar(poehdr,'crval2')
;
         datobs = fxpar(poehdr,'date-obs')
         datend = fxpar(poehdr,'date-end')
         if (!err lt 0) then datend = fxpar(poehdr,'date_end')   ;in case
         if (!debug gt 3) then $ 
            stop,' Stopping in MAKE_EMAP_HDR after datend defined'
;
         timobs = fxpar(poehdr,'time-obs')
         timend = fxpar(poehdr,'time-end')
         if (!err lt 0) then timend = fxpar(poehdr,'time_end')   ;in case
;
         mjdobs = fxpar(poehdr,'mjd-obs')
         if (!debug gt 3) then $ 
            stop,' Stopping in MAKE_EMAP_HDR after mjdobs defined'
;
         obsid = fxpar(poehdr,'xs-obsid')
         xsfiltr = fxpar(poehdr,'xs-filtr')
         if (xsfiltr eq 0) then filter = 'NONE'
         if (xsfiltr eq 1) then filter = 'BORON'
         xsmode = fxpar(poehdr,'xs-mode')
         if (xsmode eq 1) then mode = 'POINTING'
         if (xsmode eq 2) then mode = 'SLEW'
         if (xsmode eq 3) then mode = 'SCAN'
         if (!debug gt 3) then $
            stop,' Stopping in MAKE_EMAP_HDR after mode defined'
         end
;
  'MPE': begin
         radecsys = 'FK5     '
         gethistval,poehdr,'COORD_SYS',value,lnum
         hdrlin = poehdr(lnum(0)+1)  
         try = gettok(hdrlin,' ')
         hdrlin = strtrim(hdrlin,2)
         try = gettok(hdrlin,' ')
         equinox = strtrim(hdrlin,2)
         equinox = string(equinox,'$(e12.6)')
         ctype1 = 'RA---TAN'
         ctype2 = 'DEC--TAN'
         if (!debug gt 3) then $ 
            stop,' Stopping in MAKE_EMAP_HDR after ctype2 defined'
;
         gethistval,poehdr,'POINT_LONG',value,lnum
         hdrlin = poehdr(lnum(0)+1)  
         try = gettok(hdrlin,' ')
         rah = gettok(hdrlin,'H')
         ram = gettok(hdrlin,'M')  
         ras = gettok(hdrlin,'S')
         rapt = 15.*ten(rah,ram,ras)
         gethistval,poehdr,'POINT_LAT',value,lnum
         hdrlin = poehdr(lnum(0)+1)  
         try = gettok(hdrlin,' ')
         decd = gettok(hdrlin,'D')
         decm = gettok(hdrlin,'M')  
         decs = gettok(hdrlin,'S')
         decpt = ten(decd,decm,decs)
         if (!debug gt 3) then $ 
            stop,' Stopping in MAKE_EMAP_HDR after decpt defined'
;
         crval1 = rapt
         crval2 = decpt
;
         gethistval,poehdr,'OBS_DATE',value,lnum
         hdrlin = poehdr(lnum(0)+1)  
         try = gettok(hdrlin,' ')
         hdrlin = strtrim(hdrlin,2)
         try = gettok(hdrlin,' ')
         dd = gettok(try,'-')
         mm = gettok(try,'-')
         yy = strmid(try,2,2)
         im = where(strupcase(mm) eq strupcase(months))
         mm = strtrim(string(im(0) + 1),2)
         if (strlen(mm) eq 1) then mm = '0'+mm
         datobs = dd+'/'+mm+'/'+yy
         if (!debug gt 3) then $ 
            stop,' Stopping in MAKE_EMAP_HDR after datobs defined'
;  
         day = fix(dd)
         mon = fix(mm)
         year = 1900. + fix(yy)
;
         hdrlin = strtrim(hdrlin,2)
         dd = gettok(hdrlin,'-')
         mm = gettok(hdrlin,'-')
         yy = strmid(hdrlin,2,2)
         im = where(strupcase(mm) eq strupcase(months))
         mm = strtrim(string(im(0) + 1),2)
         if (strlen(mm) eq 1) then mm = '0'+mm
         datend = dd+'/'+mm+'/'+yy
         if (!debug gt 3) then $ 
            stop,' Stopping in MAKE_EMAP_HDR after datend defined'
;  
         gethistval,poehdr,'OBS_UT',value,lnum
         hdrlin = poehdr(lnum(0)+1)  
         try = gettok(hdrlin,' ')
         hdrlin = strtrim(hdrlin,2)
         timobs = gettok(hdrlin,' ')
         timend = strtrim(hdrlin,2)
         if (!debug gt 3) then $ 
            stop,' Stopping in MAKE_EMAP_HDR after timend defined'
;
         uts = timobs
         uth = gettok(uts,':')
         utm = gettok(uts,':')
         ut = ten(uth,utm,uts)
         jdcnv2,year,mon,day,ut,mjdobs,reduced=1
         if (!debug gt 3) then $ 
            stop,' Stopping in MAKE_EMAP_HDR after mjdobs defined'
;
         gethistval,poehdr,'OBS_ID',value,lnum
         hdrlin = poehdr(lnum(0)+1)  
         try = gettok(hdrlin,' ')
         obsid = strtrim(hdrlin,2)
         gethistval,poehdr,'FILTER_ID',value,lnum
         hdrlin = poehdr(lnum(0)+1)  
         try = gettok(hdrlin,' ')
         filter = strtrim(hdrlin,2)
         if (filter eq 'OFF') then filter = 'NONE'
         gethistval,poehdr,'SC_MODE',value,lnum
         hdrlin = poehdr(lnum(0)+1)  
         try = gettok(hdrlin,' ')
         mode = strtrim(hdrlin,2)
         if (!debug gt 3) then $ 
            stop,' Stopping in MAKE_EMAP_HDR after mode defined'
         end
;
  'RDF': begin
         radecsys = fxpar(poehdr,'radecsys') 
         equinox = fxpar(poehdr,'equinox')
         equinox = string(equinox,'$(e12.6)')
         ctype1 = fxpar(poehdr,'tctyp1')
         ctype2 = fxpar(poehdr,'tctyp2')
         if (!debug gt 3) then $ 
            stop,' Stopping in MAKE_EMAP_HDR after ctype2 defined'
;
         rapt = fxpar(poehdr,'ra_nom')
         decpt = fxpar(poehdr,'dec_nom')
;
         crval1 = fxpar(poehdr,'tcrvl1')
         crval2 = fxpar(poehdr,'tcrvl2')
;
         datobs = fxpar(poehdr,'date-obs')
         datend = fxpar(poehdr,'date-end')
         if (!err lt 0) then datend = fxpar(poehdr,'date_end')   ;in case
         if (!debug gt 3) then $ 
            stop,' Stopping in MAKE_EMAP_HDR after datend defined'
;
         timobs = fxpar(poehdr,'time-obs')
         timend = fxpar(poehdr,'time-end')
         if (!err lt 0) then timend = fxpar(poehdr,'time_end')   ;in case
;
         mjdobs = fxpar(poehdr,'mjd-obs')
         if (!debug gt 3) then $ 
            stop,' Stopping in MAKE_EMAP_HDR after mjdobs defined'
;
         obsid = fxpar(poehdr,'obs_id')
         filter = fxpar(poehdr,'filter')
         mode = fxpar(poehdr,'obs_mode')
         if (!err lt 0) then mode = fxpar(poehdr,'obs-mode')   ;in case
         if (!debug gt 3) then $
            stop,' Stopping in MAKE_EMAP_HDR after mode defined'
         end
endcase                  ;extracting info from poe header
;
; Now add this information to the exposure map FITS header
;
fxaddpar,emaphdr,'bitpix',-32
fxaddpar,emaphdr,after='instrume','radecsys',radecsys,$
         ' WCS for this file (e.g. FK4)'
fxaddpar,emaphdr,after='radecsys','equinox',equinox,$
         ' Equinox (epoch) for WCS'
fxaddpar,emaphdr,after='blocked','deadapp','T',$
         ' Dead time correction applied'
fxaddpar,emaphdr,after='deadapp','vignapp','T',$
         ' Vignetting correction applied'
;
fxaddpar,emaphdr,after='filter','hduclass','ogip',$
         'format conforms to OGIP standard'
fxaddpar,emaphdr,after='hduclass','hduclas1','image',$
         'dataset is an image'
fxaddpar,emaphdr,after='hduclas1','hduclas2','exposure',$
         'dataset is an exposure map'
fxaddpar,emaphdr,after='hduclas2','hduvers1','1.0.0',$
         'version of family of formats'
fxaddpar,emaphdr,after='hduvers1','hduvers1','1.0.0',$
         'version of format'
;
fxaddpar,emaphdr,'ctype1',ctype1,' axis type for dim. 1'
fxaddpar,emaphdr,'ctype2',ctype2,' axis type for dim. 2'
fxaddpar,emaphdr,'crval1',crval1,' sky coord of 1st axis (deg)'
fxaddpar,emaphdr,'crval2',crval2,' sky coord of 2nd axis (deg)'
fxaddpar,emaphdr,'cdelt1',pixsize,' x degree per pixel'
fxaddpar,emaphdr,'cdelt2',pixsize,' y degree per pixel'
fxaddpar,emaphdr,'crpix1',256.0,' x pixel of tangent plane direction'
fxaddpar,emaphdr,'crpix2',256.0,' y pixel of tangent plane direction'
fxaddpar,emaphdr,'cunit1','deg',' units of dim. 1'
fxaddpar,emaphdr,'cunit2','deg',' units of dim. 2'
;
fxaddpar,emaphdr,'date',creation,' Creation date'
fxaddpar,emaphdr,'bscale',1.0,' Scaling factor for image'
fxaddpar,emaphdr,after='bzero','bunit','s',' Units of exposure map data'
;
fxaddpar,emaphdr,'ra_nom',rapt,' Nominal right ascension (deg)'
fxaddpar,emaphdr,'dec_nom',decpt,' Nominal declination (deg)'
fxaddpar,emaphdr,'ra_pnt',0.0,' Livetime weighted pointing RA (deg)'
fxaddpar,emaphdr,'dec_pnt',0.0,' Livetime weighted pointing Dec (deg)'
fxaddpar,emaphdr,'pa_pnt',0.0,' Livetime weighted pointing roll angle (deg)'
;
fxaddpar,emaphdr,'timesys','SCC',' Time system -- Space Craft Clock times'
fxaddpar,emaphdr,'tstart',tstart,' Start time for summing exposure map'
fxaddpar,emaphdr,'tstop',tstop,' Stop time for summing exposure map'
fxaddpar,emaphdr,'timeunit','s',' Units for start, stop times'
fxaddpar,emaphdr,'ontime',ontime,' Total time for summation of exp. map (sec)'
fxaddpar,emaphdr,'livetime',0.0,' Livetime weighted summation time (sec)'
;
fxaddpar,emaphdr,'mjd-obs',mjdobs,' MJD of start of obs.'
fxaddpar,emaphdr,'date-obs',datobs,' Start date of observation'
fxaddpar,emaphdr,'time-obs',timobs,' Start time of observation'
fxaddpar,emaphdr,'date-end',datend,' End date of observation'
fxaddpar,emaphdr,'time-end',timend,' End time of observation'
fxaddpar,emaphdr,'obs_id',obsid,' Observation ID'
fxaddpar,emaphdr,'filter',filter,' Filter used during observation'
fxaddpar,emaphdr,'obs_mode',mode,' Pointing mode'
;
; These next keywords will be updated in the main program
;
fxaddpar,emaphdr,'livetmin',0.0,' Minimum live time factor'
fxaddpar,emaphdr,'livetmax',0.0,' Maximum live time factor'
fxaddpar,emaphdr,'datamin',0.0,'  Minimum value in image'
fxaddpar,emaphdr,'datamax',0.0,'  Maximum value in image'
;
fxaddpar,emaphdr,'pixdetx',pixdet,' Unblocked det. map deg per pixel (X)'
fxaddpar,emaphdr,'pixdety',pixdet,' Unblocked det. map deg per pixel (Y)'
fxaddpar,emaphdr,'dimdetx',dimdet,' X dimension unblocked det. map'
fxaddpar,emaphdr,'dimdety',dimdet,' Y dimension unblocked det. map'
fxaddpar,emaphdr,'optdetx',xdopt,' Detector opt. axis X in unblocked pixels'
fxaddpar,emaphdr,'optdety',ydopt,' Detector opt. axis Y in unblocked pixels'
;
fxaddpar,emaphdr,'gtifile','  ',' Accepted time intervals file'
fxaddpar,emaphdr,'evrfile','  ',' Events rates file'
fxaddpar,emaphdr,'aspfile','  ',' Aspect offsets file'
fxaddpar,emaphdr,'detmfile','  ',' Det. map file'
;
; Now add accepted time intervals to history records
;
ntim = n_elements(actbeg)
hdradd = strarr(1+ntim)
histrec = ' Mean Exposure Map over the following time intervals:'
hdradd(0) = histrec
for ii=1,ntim do begin
  temp = string(ii,format='$(i3)')
  histrec = temp+string(actbeg(ii-1))+'  '+string(actend(ii-1))
  hdradd(ii) = histrec
endfor
;
for ii=0,ntim do begin
  histrec = hdradd(ii)
  fxaddpar,emaphdr,'HISTORY','  '+histrec
endfor
;
return
end           ;pro make_emap_hdr
