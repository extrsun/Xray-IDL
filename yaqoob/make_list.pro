;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       make_list
;
;*PURPOSE:
; Retrieve data from a ROSAT photon list file and store in a photon list
; data structure. Allows only data with X & Y values within xmin-xmax,
; ymin-ymax, and times within tmin-tmax, to be retrieved.
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
;                region:'',numpix:0.0D0,
;                totexp:0.0D0,ntimes:0,tbeg:dblarr(200),tend:dblarr(200)}
;
;         where  xmin,xmax,ymin & ymax are limits of box containing region,
;                region is the ASCII descriptor for the extraction region,
;                numpix = area of region (in pixels) of extraction region,
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
;     afterwards.
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
;-
;-------------------------------------------------------------------------------
pro make_list,inputs,plist,plinfo,plhdr,oparms=oparms
;
COMMON descriptor,file_descript
;
if (n_params(0) eq 0) then begin
  print,'MAKE_LIST, inputs, PLIST, PLINFO, PLHDR, [OPARMS=OPARMS]'
  print,'  '
  print,'   Uses inputs OBSEQ, DIR, EXTYP, INSTR, PROC, TRIM, CHATTER, XMIN, '$
       +'XMAX, '
  print,'               YMIN, YMAX, PIXEL, BLOCK, TMIN, TMAX, and CTYP'
  print,'   from make_list.def'
  print,'  '
  retall
end
;
dfile = 'make_list'
rsgetpar,inputs,dfile,oparms
obseq = strtrim(oparms(0),2)
dir = strtrim(oparms(1),2)
proc = strtrim(oparms(4),2)            ;processing format - US or MPE
proc = strupcase(proc)
extyp = 'fits'                         ;only FITS extension allowed for now
if (proc eq 'MPE') then extyp = 'tfits'
trim = strtrim(oparms(8),2)
chatter = fix(oparms(9))
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
set_defcen,instr,ctyp,defxcen,defycen
;
tmin = double(oparms(21))
tmax = double(oparms(22))
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
; now read photon list. Use match_files.
;
;ext = '_'+extyp+'_3.hhh'
;match_files,dir,obseq,ext,names,nlist       ;look for the right files
ext = '.'+extyp
match_files,dir,obseq,ext,name,nlist        ;look for the right files
name = name(0)                              ;default = take latest version
oparms(0) = strtrim(obseq,2)        ;obseq & dir may have been corrected
oparms(1) = strtrim(dir,2)
;if (!debug gt 1) then stop,' Stopping after files have been found'
;
;name = names(0)
if (chatter) then $
   print,' Reading photon list data from file ',name
;
fits_info,name,/sil
dum = file_descript
while (dum ne '') do try = gettok(dum,'BINTABLE')
dum = try
while (dum ne '') do try = gettok(dum,' ')
nphot = long(try)
;
xsel = abs(xmin) + abs(xmax)
if ( (xmin le 0) and (xmax ge 2.*defxcen)) then xsel = 0   ;full range in x
ysel = abs(ymin) + abs(ymax)
if ( (ymin le 0) and (ymax ge 2.*defycen)) then ysel = 0   ;full range in y
tsel = abs(tmin) + abs(tmax)
pisel = abs(pimin) + abs(pimax)
allsel = xsel + ysel + tsel + pisel
;
; read in header information - from 3rd table for US processing, 1rst for MPE
;
extnum = 3
if (proc eq 'MPE') then extnum = 1
plhdr = headfits(name,ext=extnum)
;
start = 0 
num = 20000. < nphot              ;in case fewer than NUM photons in file
num2 = num
nct = 0 
stop = 0
if (allsel eq 0) then begin
  if (chatter) then $
     print,' Now reading data for ',nphot,' photons'
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
    if ( (start+num) ge nphot) then num2 = nphot - start-2 
    tab=readfits(name,hdr,ext=extnum,/sil,st=start,num=num2)
;
    ftype = 'y'
    if (proc eq 'MPE') then ftype = 'ypix'
    yev(nct) = fits_get(hdr,tab,ftype)
    ftype = 'x'
    if (proc eq 'MPE') then ftype = 'xpix'
    xev(nct) = fits_get(hdr,tab,ftype)
    tev(nct) = fits_get(hdr,tab,'time')       
;
    ftype = 'dx'
    if (proc eq 'MPE') then ftype = 'xdet'
    dxev(nct) = fits_get(hdr,tab,ftype)
    ftype = 'dy'
    if (proc eq 'MPE') then ftype = 'ydet'
    dyev(nct) = fits_get(hdr,tab,ftype)
    ftype = 'pha'
    if (proc eq 'MPE') then ftype = 'raw_ampl'
    phaev(nct) = fits_get(hdr,tab,ftype)
    ftype = 'pi'
    if (proc eq 'MPE') then ftype = 'ampl'
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
endif else begin
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
    if ( (start+num) ge nphot) then num2 = nphot - start-2
    if (chatter) then $
       print,' Checking limits for photons ',start,' to ',start+num2-1
    tab=readfits(name,hdr,ext=extnum,/sil,st=start,num=num2)
    np = num2
    check = 1
;
    if (ysel ne 0) then begin
      if (!debug gt 2) then print,'           Checking Y'
      if (proc eq 'MPE') then begin
        ftype = 'ypix'
        if (ctyp eq 'DET') then ftype = 'ydet'
      endif else begin
        ftype = 'y'
        if (ctyp eq 'DET') then ftype = 'dy'  
      endelse
      try = fits_get(hdr,tab,ftype)
;      if (ctyp eq 'SKY') then try = fits_get(hdr,tab,'y') else $
;                              try = fits_get(hdr,tab,'dy')
      if (ymin eq 0) then ybot = min(try)
      if (ymax eq 0) then ytop = max(try)
      indy = where( (try ge ybot) and (try le ytop) )
      if (indy(0) lt 0) then begin
        check = 0
        np = 0
      endif
    endif else indy = indgen(num2)
    if (!debug gt 2) then print,'    After Y check: check, np:',check,np
    if ( (check) and (!debug gt 2) ) then $
       print,'    Y check: ',minmax(try(indy)),ybot,ytop
;
    if ( (xsel ne 0) and (check) ) then begin
      if (!debug gt 2) then print,'           Checking X'
      try = 0
      if (proc eq 'MPE') then begin
        ftype = 'xpix'
        if (ctyp eq 'DET') then ftype = 'xdet'
      endif else begin
        ftype = 'x'
        if (ctyp eq 'DET') then ftype = 'dx'  
      endelse
      try = fits_get(hdr,tab,ftype,indy)
;      if (ctyp eq 'SKY') then try = fits_get(hdr,tab,'x',indy) else $
;                              try = fits_get(hdr,tab,'dx',indy)
      if (xmin eq 0) then xbot = min(try)
      if (xmax eq 0) then xtop = max(try)
      indx = where( (try ge xbot) and (try le xtop) )
      if (indx(0) lt 0) then begin
        check = 0 
        np = 0
      endif else indy = indy(indx)
    endif
    if (!debug gt 2) then print,'    After X check: check, np:',check,np
    if ( (check) and (!debug gt 2) ) then $
       print,'    X check: ',minmax(try(indy)),xbot,xtop
;
    if ( (tsel ne 0) and (check) ) then begin
      if (!debug gt 2) then print,'           Checking T'
      try = 0
      indx = 0
      try = fits_get(hdr,tab,'time',indy)        
      if (tmin eq 0) then tbot = min(try)
      if (tmax eq 0) then ttop = max(try)
      indt = where( (try ge tbot) and (try le ttop) )
      if (indt(0) lt 0) then begin
        check = 0
        np = 0 
      endif else indy = indy(indt)
    endif
    if (!debug gt 2) then print,'    After T check: check, np:',check,np
    if ( (check) and (!debug gt 2) ) then $
       print,'    T check: ',minmax(try(indy)),tbot,ttop
;
    if ( (pisel ne 0) and (check) ) then begin
      if (!debug gt 2) then print,'           Checking PI'
      try = 0
      indx = 0
      try = fits_get(hdr,tab,'pi',indy)        
      if (pimin eq 0) then pibot = min(try)
      if (pimax eq 0) then pitop = max(try)
      indpi = where( (try ge pibot) and (try le pitop) )
      if (indpi(0) lt 0) then begin
        check = 0
        np = 0 
      endif else indy = indy(indpi)
    endif
    if (!debug gt 2) then print,'    After PI check: check, np:',check,np
    if ( (check) and (!debug gt 2) ) then $
       print,'    PI check: ',minmax(try(indy)),tbot,ttop
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
;    stop = (indy(0) lt 0)*nct 
    stop = (ctyp eq 'SKY')*(indy(0) lt 0)*nct
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
  nphot = nct
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
  nsect = fix( (stop - start + 1)/num) + 1
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
    ftype = 'y'
    if (proc eq 'MPE') then ftype = 'ypix'
    yev(ib) = fits_get(hdr,tab,ftype,rowsel)
    ftype = 'x'
    if (proc eq 'MPE') then ftype = 'xpix'
    xev(ib) = fits_get(hdr,tab,ftype,rowsel)
    tev(ib) = fits_get(hdr,tab,'time',rowsel)   
;
    ftype = 'dx'
    if (proc eq 'MPE') then ftype = 'xdet'
    dxev(ib) = fits_get(hdr,tab,ftype,rowsel)
    ftype = 'dy'
    if (proc eq 'MPE') then ftype = 'ydet'
    dyev(ib) = fits_get(hdr,tab,ftype,rowsel)
    ftype = 'pha'
    if (proc eq 'MPE') then ftype = 'raw_ampl'
    phaev(ib) = fits_get(hdr,tab,ftype,rowsel)
    ftype = 'pi'
    if (proc eq 'MPE') then ftype = 'ampl'
    piev(ib) = fits_get(hdr,tab,ftype,rowsel)
;
    ib = ie + 1
    start = start + num
  endfor
endelse
;
if (proc eq 'US') then begin              ;Use header info, if US processing
  axlen1 = sxpar(plhdr,'AXLEN1')
  axlen2 = sxpar(plhdr,'AXLEN2')
  xdet = sxpar(plhdr,'XS-XDET')
  ydet = sxpar(plhdr,'YD-DET')
  if (xsel eq 0) then begin
    xmin = 0.
    xmax = axlen1*(ctyp eq 'SKY') + xdet*(ctyp eq 'DET')
  endif
  if (ysel eq 0) then begin
    ymin = 0.
    ymax = axlen2*(ctyp eq 'SKY') + ydet*(ctyp eq 'DET')
  endif
endif else begin  
  if (xsel eq 0) then begin
    xmin = min(xev)
    xmax = max(xev)
  endif
  if (ysel eq 0) then begin
    ymin = min(yev)
    ymax = max(yev)
  endif
endelse
;
if (tsel eq 0) then begin
  tmin = min(tev)
  tmax = max(tev)
endif
;
if (nphot eq 0) then begin
  print,' No events which match the selection criteria were found.'
  print,' Selection based on'
  if (ysel) then print,'           ymin =', ymin,'   ymax =',ymax
  if (xsel) then print,'           xmin =', xmin,'   xmax =',xmax
  if (tsel) then print,'           tmin =', tmin,'   tmax =',tmax
  if (pisel) then print,'         pimin =',pimin,'  pimax =',pimax
  print,' Returning.'
endif else begin                ;events matching criteria were found
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
; now figure out interval start & stop times. Need to read good time intervals.
;
  hdr=0 & tab=0
  if (proc eq 'US') then begin
    tab = readfits(name,hdr,ext=1,/sil)
    bgti = tbget(hdr,tab,1)               ;start times of good time intervals
    egti = tbget(hdr,tab,2)               ;end times of good time intervals
    ngti = n_elements(bgti)
;
    if (tsel eq 0) then begin
      i1 = 0
      i2 = ngti - 1
    endif else begin                      ;data selected by time
      i1 = max(where(bgti le tmin))
      i2 = min(where(egti ge tmax))
    endelse
    tbeg = bgti(i1:i2)
    tend = egti(i1:i2)
    tbeg(0) = tmin
    tend(i2-i1) = tmax
  endif else begin                        ;just for now, for MPE processing
    tbeg = min(time)
    tend = max(time)
  endelse
;  
; now store information in structure variable plinfo
;
  xcen = (xmin + xmax)/2.
  xdel = xmax - xmin
  ycen = (ymin + ymax)/2.
  ydel = ymax - ymin
;
  plinfo = {xevinfo,obseq:'',filename:'',proc:'',ctyp:'', $
            xmin:0.0,xmax:0.0,ymin:0.0,ymax:0.0,region:'',numpix:0.0D0, $
            totexp:0.0D0,ntimes:0,tbeg:dblarr(200),tend:dblarr(200)}
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
;
  totexp = total(tend - tbeg + 1)
  ntimes = n_elements(tbeg)
  plinfo.totexp = totexp
  plinfo.ntimes = n_elements(tbeg)
  ntimes = ntimes < 200
  plinfo.tbeg = tbeg(0:ntimes - 1)
  plinfo.tend = tend(0:ntimes - 1)
endelse
;
return
end                ;pro make_list
