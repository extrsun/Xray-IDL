;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       make_list
;
;*PURPOSE:
; Retrieve data from a ROSAT photon list file and store in a photon list
; data structure. Allows user to select data with X & Y values within
; xmin-xmax, ymin-ymax, row numbers within nmin-nmax, pi channels within
; pimin-pimax and times within tmin-tmax (&/or intervals within a file), to
; be retrieved. 
; (do NOT convert file to ST SDAS format!)
;
;*CALLING SEQUENCE:
;       make_list,inputs,plist,plinfo,plhdr,oparms=oparms
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
;       selection params=1 or examinining the default (text) file,
;       ZDEF:MAKE_LIST.DEF.
;
;       The following parameters are available.
;
;        OBSEQ       Root name of input data file 
;                    (null string not allowed, must be specified)
;        DIR         Directory containing input file 
;                    (default is current directory)
;        EXTYP       Extension of input file (e.g., FITS, MDS)
;                    (default is FITS for US files, TFITS for MPE files)
;        INSTR       Instrument 
;                    (default is P for PSPC)
;        PROC        Format of processed files (e.g., US, MPE)
;                    (default is US)
;        TRIM        Determines whether or not data is trimmed to Good
;                    Time Intervals (default = Y)
;        CHATTER     Controls program feedback to user
;                    (default = 1)
;        NMIN        Minimum event number for photon list.
;        NMAX        Maximum event number for photon list.
;        XMIN        Minimum X value for photon list.
;        XMAX        Maximum X value for photon list
;                    (defaults = 0 for min & max X values of list)
;        YMIN        Minimum Y value for photon list.
;        YMAX        Maximum Y value for photon list
;                    (defaults = 0 for min & max Y values of list)
;        PIXEL       Pixel convention. = UNBL for unblocked, BL for blocked
;                    pixels. (def = UNBL)
;        BLOCK       Blocking factor for pixels. (def = 1 for UNBL pixels)
;        CTYP        Tells whether photons will be selected by detector (DET)
;                    or sky (SKY) coordinates. (def = SKY)
;        ACTFIL      Name of file containing time intervals to be accepted
;                    (default is NONE)
;        TMIN        Minimum spacecraft time for photon list.
;        TMAX        Maximum spacecraft time for photon list.
;        PIMIN       Minimum pi channel for photon list.
;        PIMAX       Maximum pi channel for photon list.
;
; OUTPUTS:
;       plist    data structure containing x & y, dx & dy positions,
;                arrival times, and pha & phi channels of photons.
;                has the structure of replicate(row,num), where
;                row={xevent,x:0,y:0,pha:0,pi:0,time:0.0D0,dx:0,dy:0}
;                and num is the total number of photons
;       plinfo   data structure containing info concerning extraction.
;                has the structure {xevinfo,obseq:'',filename:'',proc:'',
;                ctyp:'',xmin:0.0,xmax:0.0,ymin:0.0,ymax:0.0,
;                region:'',numpix:0.0D0,regpixsiz:0.0D0,
;                skypixsiz:0.0D0,detpixsiz:0.0D0,
;                totexp:0.0D0,ntimes:0,tbeg:dblarr(200),tend:dblarr(200)}
;
;         where  xmin,xmax,ymin & ymax are limits of box containing region,
;                region is the ASCII descriptor for the extraction region,
;                numpix = area of region (in pixels) of extraction region,
;                pixsiz = size of pixel (in arcsec),
;                totexp = total time interval for extraction,
;                ntimes = number of time intervals, tbeg & tend are vectors
;                containing start & stop times of extraction intervals
;
;       plhdr  - the FITS header for the events file
;                
;       oparms - string array which is the parsed form of inputs (same as
;                LIST in example 3 below). Allows program to be rerun using 
;                same values w/o resetting.
;
;*PROCEDURE:
;   Reads the photon list info from the SDAS formatted file and creates
;   structure. Allows user to select data within a specified rectangle
;   or time interval first, before storing in the photon list. 
;   This will save memory when dealing with large photon lists.
;
;*EXAMPLES: There are 4 different ways that the inputs may be specified
;    mode 1) Interactive
;        IDL>make_list,1,plist,plinfo,plhdr
;             ?obseq=rp123456
;             ?dir=mydir
;	      ?xmin=240.
;             ?xmax=272.
;             ?ymin=240.
;             ?ymax=272.
;             ?pixel=bl
;             ?block=30
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydir,xmin=240.,xmax=272.'
;        IDL>list=list+',ymin=240.,ymax=272.,pixel=bl,block=30'
;        IDL>make_list,list,plist,plinfo,plhdr
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,25)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydir'
;        IDL>list(12)=240.
;        IDL>list(13)=272.
;        IDL>list(14)=240.
;        IDL>list(15)=272.
;        IDL>list(16)='bl'
;        IDL>list(17)=30
;        IDL>make_list,list,plist,plinfo,plhdr
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;            block=30
;            *exit
;        IDL>make_list,'myinput.dat',plist,plinfo,plhdr
;
;*RESTRICTIONS:
;   Only FITS files allowed for now.
;
;*NOTES:
;   Will read events from FITS file directly. Do NOT convert to ST SDAS format.
;   When reading in events from a large events file, it will generally be
;     more efficient to read in the data for all times, and time filter
;     afterwards (unless the file is very large, and you run into memory
;     problems in IDL)
;   Only the first 200 time intervals will be stored in structure variable 
;     plinfo (tbeg and tend)
;
;*SUBROUTINES CALLED:
;  RSGETPAR
;  MATCH_FILES
;  HEADFITS
;  READFITS
;  FITS_GET
;  TBGET
;  FTGET
;  SET_DEFCEN
;  RSGTIMES
;  MPE_GTIMES
;  US_GTIMES
;  RDF_GTIMES
;  TIMEINTSECT
;  RDACTFIL
;  TINTSECT2
;
;*MODIFICATION HISTORY:
;    written 10 Feb 92 (GAR)
;    modified 19 Feb 1992 to return tbeg & tend (GAR)
;    modified 12 Jun 1992 to use READFITS, new version of MATCH_FILES, 
;      and to return OPARMS (GAR)
;    modified 26 Jun 1992 to use new READFITS (to read in parts of the
;      events file) (GAR)
;    modified 10 Jul 1992 to use input variable BLOCK (replaces ZOOM), and
;      to allow selection by detector coordinates
;    modified 25 Aug 1992 (GAR) to read MPE format files (input variable PROC 
;      added; most calls to TBGET changed to FITS_GET & variable FTYPE used)
;      Several bugs in old version also fixed
;   modified 11 Oct 1992 (GAR) to fix bug in defining ftype (from 25 Aug)
;      and to combine numpix, tbeg, tend, and other info in structure variable 
;      plinfo, and to return FITS header for events file
;   modified 29 Oct 1992 (GAR) to fix bug in defining variable CTYP
;   modified 15 Feb 1992 (GAR) to fix bug in defining plist when entire list
;      is read, and to add selection by PI channel, and to prevent program
;      "bombing" when no photons were found, and to be compatible with
;      MAKE_IMAGE, and to use SET_DEFCEN to set default X & Y center
;   modified 01 Mar 1993 (GAR) to fix bug when TBGET tried to read too many
;      rows when selecting by time or pi channel
;   modified 22 Apr 1993 (GAR) to change definition of plinfo structure
;   modified 14 Jul 1993 (GAR) to update prologue, to allow selection
;     by event number &/or for a number of accepted time intervals, and to
;     use TIMEINTSECT
;   modified 13 Aug 1993 (GAR) to work with MPE format (PSPC) data, and
;     to simplify the time selection logic by using subroutines RSGTIMES,
;     RDACTFIL, and TINTSECT2
;   modified 17 Nov 1993 (GAR) to add region, sky, and detector pixel sizes 
;     to structure variable PLINFO
;   modified 16 Dec 1993 (GAR) to read data from RDF format _bas.fits files
;   modified 04 Jan 1994 (GAR) to fix bug when no photons were found within
;     specified time interval
;   modified 23 Feb 1994 (GAR) to fix bug when the total number of photons 
;     selected is less than 20000. 
;-
;-------------------------------------------------------------------------------
pro make_list,inputs,plist,plinfo,plhdr,oparms=oparms
;
if (n_params(0) eq 0) then begin
  print,'MAKE_LIST, inputs, PLIST, PLINFO, PLHDR, [OPARMS=OPARMS]'
  print,'  '
  print,'   Uses inputs OBSEQ, DIR, EXTYP, INSTR, PROC, TRIM, CHATTER, 
  print,'               NMIN, NMAX, XMIN, XMAX, YMIN, YMAX, PIXEL, BLOCK,'$
       +' CTYP, '
  print,'               ACTFIL, TMIN, TMAX, PIMIN, and PIMAX '
  print,'   from make_list.def'
  print,'  '
  print,'   If both ACTFIL and (TMIN,TMAX) are specified, then the intersection'
  print,'   is used.'
  print,'  '
  retall
end
;
dfile = 'make_list'
rsgetpar,inputs,dfile,oparms
obseq = strtrim(oparms(0),2)
dir = strtrim(oparms(1),2)
proc = strtrim(oparms(4),2)            ;processing format - US, MPE, or RDF
proc = strupcase(proc)
if (!debug gt 2) then print,obseq,dir,proc
;
extyp = 'fits'                         ;only FITS extension allowed for now
case proc of
  'US': ext = '.'+extyp
  'MPE': ext = '_events.t'+extyp
  'RDF': ext = '_bas.'+extyp
  else : begin
         print,proc,' is not a valid choice for processing format.'
         print,' Valid choices are US, MPE, RDF. Returning.'
         retall
         end
endcase
;
; Now use match_files to get the whole file name for the photon list.
;
match_files,dir,obseq,ext,name,nlist        ;look for the right files
name = name(0)                              ;default = take latest version
oparms(0) = strtrim(obseq,2)                ;obseq & dir may have been corrected
oparms(1) = strtrim(dir,2)
;if (!debug gt 1) then stop,' Stopping after files have been found'
;
trim = strtrim(oparms(8),2)
chatter = fix(oparms(9))
;
; Read good time (or selected time) intervals
; If actfil eq 'NONE', then use these time intervals for plinfo
; If actfil ne 'NONE', then intervals will be read from a file; use
;   tintsect2 to find the intersections with the good time intervals
; If there are no intersections, then don't bother accumulating the list
;
rsgtimes,obseq,bgti,egti,ngti,dir=dir,proc=proc
;
actfil = strtrim(oparms(20),2)
tmin = double(oparms(21))               ;if both tmin & tmax eq 0, then
tmax = double(oparms(22))               ;no time selection will be done
if (tmin eq 0) then tbot = min(bgti) else tbot = tmin   ;tmin & tmax used to
if (tmax eq 0) then ttop = max(egti) else ttop = tmax   ;define tsel later
if (!debug gt 2) then print,tmin,tmax,ttop,tbot
;
if (strupcase(actfil) eq 'NONE') then begin 
  actbeg = tbot                             ;in case tsel ne 0
  actend = ttop                             ;in case tsel ne 0
  nact = 1
  timeintsect,tbot,ttop,bgti,egti,tbeg,tend,ntimes      ;intersecting GTIs
endif else begin
  rdactfil,actfil,actbeg,actend,nact        ;read accepted times file
  isort = sort(actbeg)
  actbeg = actbeg(isort)      ;make sure time intervals are in time order
  actend = actend(isort)
  timeintsect,tbot,ttop,actbeg,actend,tbeg,tend,nact    ;intersect w tbot,ttop
  actbeg = tbeg
  actend = tend
;
  tintsect2,bgti,egti,actbeg,actend,tbeg,tend,ntimes    ;intersecting GTIs
  if (nact eq 0) then begin
    print,' No overlap between selected times and observation GTIs.'
    print,' Please check your inputs. Returning.'
    retall
  endif 
endelse               ;finished defining accepted time intervals
;
rowmin = long(oparms(10))              ;for selection by event number
rowmax = long(oparms(11))
;
xlim = [float(oparms(12)),float(oparms(13))]
ylim = [float(oparms(14)),float(oparms(15))]
;
pixel = strtrim(oparms(16),2)
if (pixel eq '') then pixel = 'UNBL'      ;default is for unblocked pixels
if (strupcase(pixel) ne 'UNBL') then begin
  block = fix(oparms(17))
  if (block eq 0) then block = 1
endif else block = 1
;
ctyp = strupcase(strtrim(oparms(18),2))
if (ctyp eq '') then ctyp = 'SKY'         ;default is select by sky coord.
ans = ''
while ( (ctyp ne 'DET') and (ctyp ne 'SKY') ) do begin
  print,ctyp,' is not a valid option for coordinate selection.'
  print,' Valid options are SKY, DET.'
  read,' Enter a valid option or type q to quit',ans
  if ( (ans eq 'q') or (ans eq 'Q')) then retall
endwhile  
;
instr = strupcase(strtrim(oparms(3),2))
set_defcen,instr,ctyp,defxcen,defycen,regpixsiz,defxlim,defylim
;
; Change (13 Aug 1993): In MPE format photons lists, the sky X & Y values
; now range from -7680 to +7680 (or -defxcen to defxcen)
; The detector X & Y values still range from 0 to 8192 (or 4096)
; As no German HRI data have been released to the archives yet, I don't 
; know if this is also true for the HRI. Let's assume for now that it is.
; 
; Everything will still be specified in the US convention from 0 to 15,360
; Use addx & addy to make sure that the limit checking is right, though.
;
addx = 0
addy = 0
if ((proc eq 'MPE') and (ctyp eq 'SKY')) then begin
  addx = -defxcen
  addy = -defycen
endif
;
pimin = fix(oparms(23))
pimax = fix(oparms(24))
;
xlim = block*xlim                            ;change to unblocked pixels
ylim = block*ylim
xmin = long( xlim(0) - 1.*(xlim(0) lt 0) )   ;find integers which completely
xmax = long( xlim(1) + 1.*(xlim(1) gt 0) )   ;enclose the specified region
ymin = long( ylim(0) - 1.*(ylim(0) lt 0) )
ymax = long( ylim(1) + 1.*(ylim(1) gt 0) )
;
; Now read photon list. First read FITS header to get the total number of events
; Read header information from 3rd table for US processing, 1rst for MPE,
; 2nd for RDF format files
;
if (chatter) then $
   print,' Reading photon list data from file ',name
;
case proc of
  'US': extnum = 3
  'MPE': extnum = 1
  'RDF': extnum = 2
endcase
plhdr = headfits(name,ext=extnum)
nphot = fxpar(plhdr,'NAXIS2')     ;this replaces the calls to fits_info & gettok
if (rowmax eq 0) then rowmax = nphot
;
xsel = abs(xmin) + abs(xmax)
if ( (xmin le 0) and (xmax ge 2.*defxcen)) then xsel = 0   ;full range in x
ysel = abs(ymin) + abs(ymax)
if ( (ymin le 0) and (ymax ge 2.*defycen)) then ysel = 0   ;full range in y
tsel = abs(tmin) + abs(tmax)
pisel = abs(pimin) + abs(pimax)
allsel = xsel + ysel + tsel + pisel
;
start = rowmin
nphot = nphot < (rowmax - rowmin + 1)
num = 20000. < nphot              ;in case fewer than NUM photons in file
num2 = num
nct = 0 
stop = 0
if (allsel eq 0) then begin
  if (chatter) then $
     print,' Now reading data for ',nphot,' photons from ',$
           rowmin,' to ',rowmax
;
  yev = intarr(nphot)
  xev = yev
  dyev = yev 
  dxev = yev 
  phaev = yev 
  piev = yev
  tev = dblarr(nphot)
;
  while ( (start lt nphot) and (stop eq 0) ) do begin
    hdr = 0 & tab = 0 
    if ( (start+num) ge nphot) then num2 = nphot - start 
    if (chatter) then print,'   ',start,' to ',start+num2-1
    tab = readfits(name,hdr,ext=extnum,/sil,st=start,num=num2)
;
    case proc of
      'US': ftype = 'y'
      'MPE': ftype = 'ypix'
      'RDF': ftype = 'y'
    endcase
    yev(nct) = fits_get(hdr,tab,ftype)
    case proc of
      'US': ftype = 'x'
      'MPE': ftype = 'xpix'
      'RDF': ftype = 'x'
    endcase
    xev(nct) = fits_get(hdr,tab,ftype)
    tev(nct) = fits_get(hdr,tab,'time')       
;
    case proc of
      'US': ftype = 'dx'
      'MPE': ftype = 'xdet'
      'RDF': ftype = 'detx'
    endcase
    dxev(nct) = fits_get(hdr,tab,ftype)
    case proc of
      'US': ftype = 'dy'
      'MPE': ftype = 'ydet'
      'RDF': ftype = 'dety'
    endcase
    dyev(nct) = fits_get(hdr,tab,ftype)
    case proc of
      'US': ftype = 'pha'
      'MPE': ftype = 'raw_ampl'
      'RDF': ftype = 'pha'
    endcase
    phaev(nct) = fits_get(hdr,tab,ftype)
    case proc of
      'US': ftype = 'pi'
      'MPE': ftype = 'ampl'
      'RDF': ftype = 'pi'
    endcase
    piev(nct) = fits_get(hdr,tab,ftype)
;
    nct = nct + num
    start = start + num
  endwhile
;
  ymin = min(yev)
  ymax = max(yev)
  xmin = min(xev)
  xmax = max(xev)
endif else begin                 ;allsel ne 0 --> list selection
  if (chatter) then $
     print,' Now checking limits for ',nphot,' photons'
;
  rows = lonarr(nphot)
  ybot = ymin & ytop = ymax & xbot = xmin & xtop = xmax
  tbot = tmin & ttop = tmax
  pibot = pimin & pitop = pimax
;
  while ( (start lt nphot) and (stop eq 0) ) do begin
    hdr = 0 & tab = 0 & try = 0 
    indy = 0 & indx = 0 & indt = 0 & indpi=0
;
    if ( (start+num) ge nphot) then num2 = nphot - start
    if (chatter) then $
       print,' Checking limits for photons ',start,' to ',start+num2-1
    tab=readfits(name,hdr,ext=extnum,/sil,st=start,num=num2)
    np = num2
    check = 1
;
    if (ysel ne 0) then begin
      if (!debug gt 2) then print,'           Checking Y'
      case proc of
        'US':  begin
               ftype = 'y'
               if (ctyp eq 'DET') then ftype = 'dy'  
               end
        'MPE': begin
               ftype = 'ypix'
               if (ctyp eq 'DET') then ftype = 'ydet'
               end
        'RDF': begin
               ftype = 'y'
               if (ctyp eq 'DET') then ftype = 'dety'
               end
      endcase
      try = fits_get(hdr,tab,ftype)
      if (ymin eq 0) then ybot = min(try)
      if (ymax eq 0) then ytop = max(try)
      indy = where( (try ge (ybot+addy)) and (try le (ytop+addy)), nsel )
      if (nsel le 0) then begin
        check = 0
        np = 0
      endif
    endif else indy = indgen(num2)
    if (!debug gt 2) then print,'    After Y check: check, np:',check,np
    if ( (check) and (!debug gt 2) ) then $
       print,'    Y check: ',minmax(try(indy)),ybot,ytop,n_elements(indy)
;
    if ( (xsel ne 0) and (check) ) then begin
      if (!debug gt 2) then print,'           Checking X'
      try = 0
      case proc of
        'US':  begin
               ftype = 'x'
               if (ctyp eq 'DET') then ftype = 'dx'  
               end
        'MPE': begin
               ftype = 'xpix'
               if (ctyp eq 'DET') then ftype = 'xdet'
               end
        'RDF': begin
               ftype = 'x'
               if (ctyp eq 'DET') then ftype = 'detx'
               end
      endcase
      try = fits_get(hdr,tab,ftype,indy)
      if (xmin eq 0) then xbot = min(try)
      if (xmax eq 0) then xtop = max(try)
      indx = where( (try ge (xbot+addx)) and (try le (xtop+addx)), nsel )
      if (nsel le 0) then begin
        check = 0 
        np = 0
      endif else indy = indy(indx)
    endif
    if (!debug gt 2) then print,'    After X check: check, np:',check,np
    if ( (check) and (!debug gt 2) ) then $
       print,'    X check: ',minmax(try(indy)),xbot,xtop,n_elements(indx)
;
    if ( (tsel ne 0) and (check) ) then begin
      if (!debug gt 2) then print,'           Checking T'
      try = 0
      indx = 0
      try = fits_get(hdr,tab,'time',indy)        
      nsel = 0
      iit = 0
      while (iit lt nact) do begin
        tbot = actbeg(iit)
        ttop = actend(iit)
        if (tbot eq 0) then tbot = min(try)
        if (ttop eq 0) then ttop = max(try)
        indtry = where( (try ge tbot) and (try le ttop), ntry )
        if ( (check) and (!debug gt 3) ) then $
            stop,'    T loop: ',iit,nact,tbot,ttop,ntry
        if (ntry gt 0) then if (iit eq 0) then indt = indtry else $
           indt = [indt,indtry]
        iit = iit + 1
        nsel = nsel + ntry
      endwhile
      if (nsel le 0) then begin
        check = 0
        np = 0 
      endif else begin
        indy = indy(indt)
      endelse
    endif
    if (!debug gt 2) then print,'    After T check: check, np:',check,np
    if ( (check) and (!debug gt 2) ) then $
       print,'    T check: ',minmax(try(indy)),tbot,ttop,actbeg,actend $
            ,n_elements(indt)
;
    if ( (pisel ne 0) and (check) ) then begin
      if (!debug gt 2) then print,'           Checking PI'
      try = 0
      indx = 0
      case proc of
        'US': ftype = 'pi'
        'MPE': ftype = 'ampl'
        'RDF': ftype = 'pi'
      endcase
      ftype = 'pi'
      if (proc eq 'MPE') then ftype = 'ampl'
      try = fits_get(hdr,tab,ftype,indy)        
      if (pimin eq 0) then pibot = min(try)
      if (pimax eq 0) then pitop = max(try)
      indpi = where( (try ge pibot) and (try le pitop), nsel )
      if (nsel le 0) then begin
        check = 0
        np = 0 
      endif else indy = indy(indpi)
    endif
    if (!debug gt 2) then print,'    After PI check: check, np:',check,np
    if ( (check) and (!debug gt 2) ) then $
       print,'    PI check: ',minmax(try(indy)),tbot,ttop,n_elements(indpi)
;
    if (check) then begin
      if (!debug gt 2) then print,'     Adding to rows: nct, np, start:', $
         nct,np,start
      rows(nct) = indy + start
      np = n_elements(indy)
    endif
;
    nct = nct + np
    start = start + num
    case proc of
      'US': stop = (ctyp eq 'SKY')*(indy(0) lt 0)*nct
      'MPE': stop = (ctyp eq 'SKY')*(indy(0) lt 0)*nct
      'RDF': stop = (indt(0) lt 0)*nct       ;RDF files ordered by time, not y
    endcase
    if (!debug gt 2) then print,'    nct,np,start,check,stop:',nct,np,start $
       ,check,stop
;
    if ( (chatter) and (not stop) and (np ne 0) ) then $
       print,np,' photons found within specified limits'
    if ( (chatter) and (stop ne 0) ) then $
       print,' Finished checking.'
;    if (!debug gt 2) then stop,' Stopping in loop for checking:'
  endwhile
;  if (!debug gt 1) then stop,' Stopping after photons checked'
;
  nphot = nct
;
  if (nphot eq 0) then begin     ;no events matching criteria were found
    print,' No events which match the selection criteria were found.'
    print,' Selection based on'
    if (ysel) then print,'           ymin =', ymin,'   ymax =',ymax
    if (xsel) then print,'           xmin =', xmin,'   xmax =',xmax
    if (tsel) then print,'           tmin =', tmin,'   tmax =',tmax
    if (pisel) then print,'         pimin =',pimin,'  pimax =',pimax
    print,' Returning.'
    retall
  endif                           
;
  rows = rows(0:nphot-1) 
  yev = intarr(nphot) 
  xev = yev
  dyev = yev 
  dxev = yev 
  phaev = yev 
  piev = yev
  tev = dblarr(nphot)
;
  ib = 0 
  start = min(rows) 
  stop = max(rows)
  nsect = fix( (stop - start + 1)/num)
  frac = float(stop - start + 1)/num
  nsect = nsect + (frac ne 0)              ;add a section if the remainder ne 0
;
  if (chatter) then $
     print,' Now reading data for ',nphot,' photons',' in ',nsect,' sections.'
;
  for nn=0,nsect-1 do begin
    hdr = 0 
    tab = 0 
    rowsel = 0
    tabinv,rows,start+num-1,ie 
    ie = long(ie) 
;
    if (chatter) then $
       print,nn+1,' Photons ',start,' to ',(start+num-1)<stop
    tab=readfits(name,hdr,ext=extnum,/sil,st=start,num=num)
    rowsel = rows(ib:ie) - start
;
    case proc of
      'US': ftype = 'y'
      'MPE': ftype = 'ypix'
      'RDF': ftype = 'y'
    endcase
    yev(ib) = fits_get(hdr,tab,ftype,rowsel)
    case proc of
      'US': ftype = 'x'
      'MPE': ftype = 'xpix'
      'RDF': ftype = 'x'
    endcase
    xev(ib) = fits_get(hdr,tab,ftype,rowsel)
    tev(ib) = fits_get(hdr,tab,'time',rowsel)   
;
    case proc of
      'US': ftype = 'dx'
      'MPE': ftype = 'xdet'
      'RDF': ftype = 'detx'
    endcase
    dxev(ib) = fits_get(hdr,tab,ftype,rowsel)
    case proc of
      'US': ftype = 'dy'
      'MPE': ftype = 'ydet'
      'RDF': ftype = 'dety'
    endcase
    dyev(ib) = fits_get(hdr,tab,ftype,rowsel)
    case proc of
      'US': ftype = 'pha'
      'MPE': ftype = 'raw_ampl'
      'RDF': ftype = 'pha'
    endcase
    phaev(ib) = fits_get(hdr,tab,ftype,rowsel)
    case proc of
      'US': ftype = 'pi'
      'MPE': ftype = 'ampl'
      'RDF': ftype = 'pi'
    endcase
    piev(ib) = fits_get(hdr,tab,ftype,rowsel)
;
    ib = ie + 1
    start = start + num
  endfor
endelse
;
if (proc eq 'MPE') then begin              ;add default X & Y centers
  xev = temporary(xev + defxcen)
  yev = temporary(yev + defycen)
endif
if (xsel eq 0) then begin
  xmin = defxlim(0)
  xmax = defxlim(1)
endif
if (ysel eq 0) then begin
  ymin = defylim(0)
  ymax = defylim(1)
endif
;
if (pisel eq 0) then begin
  pimin = min(piev)
  pimax = max(piev)
endif
;
row = {xevent,x:0,y:0,pha:0,pi:0,time:0.0D0,dx:0,dy:0}
plist = replicate(row,nphot)
plist.x = xev
plist.y = yev
plist.pha = phaev
plist.pi = piev
plist.time = tev
plist.dx = dxev
plist.dy = dyev
;
numpix = (ymax - ymin + 1)*(xmax - xmin + 1)
;
; Have already figured out interval start & stop times that intersect with
;   the good time intervals.
;
; Now store information in structure variable plinfo
;
xcen = (xmin + xmax)/2.
xdel = xmax - xmin
ycen = (ymin + ymax)/2.
ydel = ymax - ymin
;
plinfo = {xevinfo,obseq:'',filename:'',proc:'',ctyp:'', $
          xmin:0.0,xmax:0.0,ymin:0.0,ymax:0.0,region:'',numpix:0.0D0, $
          regpixsiz:0.0D0,skypixsiz:0.0D0,detpixsiz:0.0D0,$
          pimin:0.0,pimax:0.0,totexp:0.0D0,$
          ntimes:0,tbeg:dblarr(200),tend:dblarr(200)}
plinfo.obseq = oparms(0)
plinfo.filename = name
plinfo.proc = proc
plinfo.ctyp = ctyp
;
plinfo.xmin = xmin
plinfo.xmax = xmax
plinfo.ymin = ymin
plinfo.ymax = ymax
;
f = '$(f12.3)'
region = 'box ('+string(form=f,xcen)+','+string(form=f,ycen)+','
region = region+string(form=f,xdel)+','+string(form=f,ydel)+')'
plinfo.region = region
plinfo.numpix = numpix
plinfo.regpixsiz = regpixsiz
;
set_defcen,instr,'SKY',dxc,dyc,skypixsiz
set_defcen,instr,'DET',dxc,dyc,detpixsiz
plinfo.skypixsiz = skypixsiz
plinfo.detpixsiz = detpixsiz
;
plinfo.pimin = pimin
plinfo.pimax = pimax
;
plinfo.totexp = total(tend - tbeg)
ntimes = n_elements(tbeg)
plinfo.ntimes = n_elements(tbeg)
ntimes = ntimes < 200
plinfo.tbeg = tbeg(0:ntimes - 1)
plinfo.tend = tend(0:ntimes - 1)
;
return
end                ;pro make_list
