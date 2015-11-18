;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       oldgetasp
;
;*PURPOSE:
;   A procedure to read aspect data (roll angle, changes in X and Y pointing)
;   from Rosat aspect (.CAS, .SAS, .SA for PSPC; .AO for HRI) FITS files
;   Reads data from both US format and MPE format files
;
;*CALLING SEQUENCE:
;       OLDGETASP, inputs, obinum, SCTIME, ROLL, DELX, DELY, NOMASP,
;                  OBINFO=OBINFO, OPARMS=OPARMS
;
;*PARAMETERS:
; INPUTS:
;       inputs - input parameter description.
;                0 - use all defaults
;                1 - interative selection of parameters
;                string - parameter defintion of the form
;                   'parname1=value1,parname2=value2,,,,'
;                string array - each element of the array has
;                   form 'value' (elements must be in correct order)
;                string of form '$filename' where filename is the
;                   name of the file containing one parameter
;                   per line in the form: parname=value
;       Any parameter not specified is set to its default.
;       Defaults for the parameters can be found by using interactive
;       selection params=1 or examinining the default (text) file,
;       ZDEF:RSGET.DEF.
;
;       The following parameters are availble.
;
;        OBSEQ       Root name of input data file 
;                    (null string not allowed, must be specified)
;        DIR         Directory containing input file 
;                    (default is current directory)
;        EXTYP       Extension of input file (e.g., CAS, ASP)
;                    (default is CAS)
;        INSTR       Instrument 
;                    (default is P for PSPC)
;        PROC        Format of processed files (e.g., US, MPE)
;                    (default is 'US')
;        TRIM        Determines whether or not data is trimmed to Good
;                    Time Intervals (default = 1)
;        CHATTER     Controls program feedback to user
;                    (default = 1)
;        READORB     Controls whether to read orbit data or not
;                    (default = 'N')
;
; OPTIONAL INPUTS:
;       obinum  - vector containing numbers of OBIs to read (0 for all)
;
; OUTPUTS:
;       sctime  - spacecraft times for which event rates were determined
;       roll    - roll angle of detector (in degrees)
;       delx    - x translation in NS system (in units of 0.5 arcsec)
;               = change in pointing RA relative to nominal (in pixels)
;       dely    - y translation in NS system (in units of 0.5 arcsec)
;               = change in pointing Dec relative to nominal (in pixels)
;       nomasp  - 2D array containing nominal RA, Dec and Roll for each
;                 obi segment
;       asperrs - data structure containing RA, Dec, and Roll angle 
;                 uncertainties for each time of aspect measurement
;                 (in 0.5 arcsec pixels for RA, Dec; degrees for Roll angle)
;                 has the structure of replicate(row,ndat) where
;                 row={aspecterrors,ra:0.0D0,dec:0.0D0,roll:0.0D0}
;                 and ndat is the total number of data points
;                 0 is returned if the file does not contain the uncertainties
;
;       obinfo -  data structure containing dates, times, & indices of
;                 beginnings & ends of obi segments
;                 has the structure of replicate(row,num), where
;                 row={obitime,sctbeg:0.0D0,yrbeg:0,daybeg:0,utsbeg:0.0D0,
;                      sctend:0.0D0,yrend:0,dayend:0,utsend:0.0D0,
;                      ibeg:0L,iend:0L}
;                 and num is the total number of intervals
;         ibeg  - indices of values in SCTIME equal to values in SCTBEG
;         iend  - indices of values in SCTIME equal to values in SCTEND
;
;       oparms  - string array which is the parsed form of inputs (same as
;                 LIST in example 3 below). Allows program to be rerun using 
;                 same values w/o resetting.
;
;*EXAMPLES: There are 4 different ways that the inputs may be specified
;    mode 1) Interactive
;        IDL>oldgetasp,1,obinum,sct,roll,delx,dely, ...
;             ?obseq=rp123456
;	      ?dir=mydirectory
;	      ?extyp=ao
;	      ?chatter=0
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydirectory,extyp=ao,chatter=0'
;        IDL>oldgetasp,list,obinum,sctime,roll,delx,dely, ...
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,10)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydirectory'
;        IDL>list(2)='ao'
;        IDL>list(9)='0'
;        IDL>oldgetasp,list,obinum,sctime,roll,delx,dely, ...
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;	     chatter=0
;            *exit
;        IDL>oldgetasp,'myinput.dat',obinum,sctime,roll,delx,dely, ...
;
;*RESTRICTIONS:
;
;*NOTES:
;  Reads FITS files directly. Do NOT convert to ST SDAS format.
;  Input parameters INSTR and TRIM are ignored (and need not be specified).
;
;*SUBROUTINES CALLED:
;  RSGETPAR
;  MATCH_FILES
;  READFITS
;  FITS_INFO
;  FITS_GET
;  FTGET
;  TBGET
;  RS_NOMASP
;  RSOBITIMES
;
;*MODIFICATION HISTORY:
;    written 17 July 1991 by GAR
;    modified 31 July 1991 by GAR to read HRI aspect data
;    modified 28 Aug 1991 by GAR to return IBEG
;    modified 30 Aug 1991 by GAR to allow selection of OBI numbers
;    modified 18 Sep 1991 by GAR to return sctbeg as keyword, to use
;                                subroutine RSOBITIMES, to return utbeg &
;                                utend as keywords
;    modifed to use GHRS derived parameter setting routines 05 Oct 1991 GAR
;    modified to combine sctbeg & obibeg, sctend & obiend, and to make into
;        keywords  08 Oct 1991  GAR
;    modified 5 Nov 1991 for compatibility with Sun Unix (GAR)
;    modified 18 Feb 1992 to return OBI info as a data structure (GAR)
;    modified 04 Jun 1992 to use READFITS, FITS_INFO, and new version
;        of MATCH_FILES, and to return OPARMS (GAR)
;    modified 08 Jul 1992 to fix bug (GAR)
;    modified 25 Aug 1992 (GAR) to read MPE format files (input variable PROC 
;      added; most calls to TBGET changed to FITS_GET)
;    24 Oct 1992 (GAR) BUG IN DETERMINING ASPECT TIMES FIXED (i.e., where
;      there appeared to be series of duplicate times)
;    modified 10 Aug 1993 (GAR) to use extension for archival MPE format 
;      data
;    modified 13 Aug 1993 (GAR) to replace call to GETNOMASP with call to
;      RS_NOMASP
;    modified 19 Dec 1993 (GAR) to return data structure asperrs; name 
;      changed from RSGETASP to OLDGETASP (to allow RSGETASP to read RDF 
;      format files as well as US & MPE format)
;-
;-------------------------------------------------------------------------------
pro oldgetasp,inputs,obinum,sctime,roll,delx,dely,nomasp,asperrs,obinfo=obinfo,$
              oparms=oparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' OLDGETASP, inputs, obinum (0 for all), SCTIME, ROLL, DELX, DELY,'
  print,'            NOMASP, ASPERRS, OBINFO=OBINFO, OPARMS=OPARMS'
  print,'   Uses inputs OBSEQ, DIR, EXTYP, INSTR, PROC, and CHATTER ' $
       +'from RSGET.DEF'
  return
endif
;
dfile = 'rsget'
rsgetpar,inputs,dfile,oparms
obseq = strtrim(oparms(0),2)
dir = strtrim(oparms(1),2)
extyp = strtrim(oparms(2),2)
instr = strtrim(oparms(3),2)
proc = strtrim(oparms(4),2)            ;processing format - US or MPE
proc = strupcase(proc)
case proc of
  'US':  begin
         if (extyp eq '') then extyp = 'cas'
         ext = '.'+extyp
         extuc = strupcase(extyp)
         if ( (extuc ne 'AO') and (extuc ne 'CAS') and (extuc ne 'SA') and $
            (extuc ne 'SAS') ) then begin
           print,extyp,' is not a valid extension for a US format aspect', $
                       ' data file.'
           print,' Valid options are CAS, SA, SAS (PSPC) or AO (HRI).', $
                 ' Returning.'
           retall 
         endif
         end
  'MPE': begin
         ext = '_attitude.tfits'
         extuc = strupcase(ext)
         end
  else : begin
         print,' This routine only reads aspect data from US or MPE aspect', $
               ' data files.'
         print,' Requested format is ',proc,'. Please check your inputs.', $
               ' Returning.'
         retall
         end
endcase
chatter = fix(oparms(9))
if (!debug gt 2) then stop,' Stopping after ext has been defined'
;
if (obseq eq '') then begin                      ;obseq must be defined
  read,'Please enter root name of input data file: ',obseq
  obseq = strtrim(obseq,2)
  oparms(0) = obseq
endif  
;
if (npar lt 2) then obinum = 0                 ;default is to read all
if (n_elements(obinum) eq 1) then obinum = obinum + intarr(1)
;
match_files,dir,obseq,ext,name,nlist        ;look for the right files
name = name(0)                              ;default = take latest version
oparms(0) = strtrim(obseq,2)        ;obseq & dir may have been corrected
oparms(1) = strtrim(dir,2)
if (!debug gt 2) then stop,' Stopping after files have been found'
;
fits_info,name,/silent,n_ext=next      ;get number of OBI extensions
if (obinum(0) eq 0) then $
   obinum = indgen(next) + 1           ;OBI numbers start with 1
nobi = n_elements(obinum)              ;if OBIs are specified
if (!debug gt 2) then stop,' Stopping after fits_info'
;
sctime = dblarr(1) - 999.
sctbeg = dblarr(nobi)
sctend = sctbeg
yrbeg = intarr(nobi)
yrend = yrbeg
daybeg = intarr(nobi)
dayend = daybeg
utsbeg = dblarr(nobi)
utsend = utsbeg
;
nomasp = dblarr(3,nobi)
roll = [-999] & delx = roll & dely = roll
erroll = roll & errx = delx & erry = dely
asperrs = 0
ibeg = lonarr(nobi) & iend = ibeg
;
ict = 0
if (chatter eq 1) then $
   print,' Reading ',extyp,' aspect data file ',name
for jj=0,nobi-1 do begin
  jobi = obinum(jj)
  if (chatter eq 1) then print,'    OBI ',jobi
  tab = readfits(name,hdr,ext=jobi,/sil)
  if (!debug gt 3) then stop,'Stopping in program OLDGETASP'
;
  ftype = 'IT1_CAS'
  if (extuc eq 'SA') then ftype = 'ITI1_SA'
  if (extuc eq 'AO') then ftype = 'ASPECT_TIME'
  if (proc eq 'MPE') then ftype = 'TIME'
  entry = fits_get(hdr,tab,ftype)                  ;Time of aspect (s/c, sec)
  if ( (extuc ne 'AO') and (proc ne 'MPE') ) then begin
    ftype = 'IT2_CAS'
    if (extuc eq 'SA') then ftype = 'ITI2_SA'
    entry = entry + double(fits_get(hdr,tab,ftype)/64.)  ;time in subseconds
  endif
;
; read OBI start and stop time info from header. Extract valid times (ie, 
; between sctb - 100. and scte + 100.). Include larger range to allow for small
; discrepancies between header info & actual start & stop times in vector
;
  rsobitimes,hdr,sctb,scte,utb,ute,proc=proc
  nument = n_elements(entry)                  ;number in this section
  if (!debug gt 2) then print,nument,minmax(entry),entry(0),entry(nument-1)
  ind = where( (entry ge (sctb-100.)) and (entry le (scte+100.)) )   
  entry = entry(ind)
  nument = n_elements(entry)                  ;number in this section
  if (!debug gt 2) then print,nument,minmax(entry)
;
  sctime = [sctime,entry]
  begt = entry(0)
  endt = entry(nument-1)
  ibeg(jj) = ict
  ict = ict + nument
  if (!debug gt 2) then print,nument,begt,endt,ibeg(jj),ict
;
; calculate OBI spacecraft and UT start times
; correct UT start time if spacecraft start time read from header
; is not same as first spacecraft time in time array
;
  if (!debug gt 2) then print,begt,sctb,begt-sctb,utb
  if (sctb ne begt) then utb(2) = utb(2) + begt - sctb
  sctbeg(jj) = begt
  yrbeg(jj) = utb(0)
  daybeg(jj) = utb(1)
  utsbeg(jj) = utb(2)
  if (!debug gt 2) then print,sctbeg,yrbeg,daybeg,utsbeg
;
; calculate OBI spacecraft and UT stop times
; correct spacecraft stop time if UT stop time read from header
; is not same as last UT time in time array
;
  if (!debug gt 2) then print,endt,scte,endt-scte,ute
  if (scte ne endt) then ute(2) = ute(2) + endt - scte 
  sctend(jj) = endt
  yrend(jj) = ute(0)
  dayend(jj) = ute(1)
  utsend(jj) = ute(2)
  if (!debug gt 2) then print,sctend,yrend,dayend,utsend
;
  if ( (npar ge 7) or (extuc eq 'AO') or (proc eq 'MPE') ) then begin
    rs_nomasp,hdr,nra,ndec,nroll,proc=proc
    nomasp(0,jj) = [nra,ndec,nroll]
    if (!debug ge 3) then stop,'Stopping after getting nominal aspect'
  endif
;
; now read other parameters, if desired
;
  if (npar ge 4) then begin
    ftype = 'IRO_CAS'
    if (extuc eq 'SA') then ftype = 'IRO_SA'
    if (extuc eq 'AO') then ftype = 'ASPECT_ROLL'
    if (proc eq 'MPE') then ftype = 'ROLL'
    rfact = 1./2./3600.
    if (extuc eq 'AO') then rfact = 180./3.14159   ;HRI roll given in radians
    entry = fits_get(hdr,tab,ftype)                ;Roll angle (in degrees)
    entry = rfact*entry(ind)
    if (extuc eq 'AO') then entry = entry + nroll   ;convert to absolute roll
    roll = [roll,entry]
  endif
;
  if (npar ge 5) then begin
    ftype = 'IXN_CAS'
    if (extuc eq 'SA') then ftype = 'IRA_SA'
    if (extuc eq 'AO') then ftype = 'ASPECT_XOFF'
    if (proc eq 'MPE') then ftype = 'XOFFSET'
    entry = fits_get(hdr,tab,ftype)     ;X translation (in NS system, 0.5")
    entry = entry(ind)
    delx = [delx,entry]
  endif
;
  if (npar ge 6) then begin
    ftype = 'IYN_CAS'
    if (extuc eq 'SA') then ftype = 'IDE_SA'
    if (extuc eq 'AO') then ftype = 'ASPECT_YOFF'
    if (proc eq 'MPE') then ftype = 'YOFFSET'
    entry = fits_get(hdr,tab,ftype)       ;Y translation (in NS system, 0.5")
    entry = entry(ind)
    dely = [dely,entry]
  endif
;
  if (extuc eq 'SA') then begin
    delx = delx - nra                    ;convert to relative change in RA
    dely = dely - ndec                   ;and Dec
;
    if (npar ge 8) then begin            ;read aspect uncertainties
      ftype = 'ISRA_SA'
      entry = fits_get(hdr,tab,ftype)    ;1 sigma error in X (0.5 arcsec pixels)
      entry = entry(ind)
      errx = [errx,entry]
;
      ftype = 'ISDE_SA'
      entry = fits_get(hdr,tab,ftype)    ;1 sigma error in Y (0.5 arcsec pixels)
      entry = entry(ind)
      erry = [erry,entry]
;
      ftype = 'ISRO_SA'
      entry = fits_get(hdr,tab,ftype)    ;1 sigma error in Roll angle (degrees)
      entry = rfact*entry(ind)
      erroll = [erroll,entry]
    endif
  endif
;
endfor
;
sctime = sctime(1:*)
if (npar ge 4) then roll = roll(1:*)
if (npar ge 5) then delx = delx(1:*)
if (npar ge 6) then dely = dely(1:*)
;
; Define ASPERRS data structure, if desired & if file contains that data
;
if ( (npar ge 8) and (extuc eq 'SA') ) then begin      
  errx = errx(1:*)
  erry = erry(1:*)
  erroll = erroll(1:*)
  ndat = n_elements(errx)
;
  row={aspecterrors,ra:0.0D0,dec:0.0D0,roll:0.0D0}
  asperrs = replicate(row,ndat)
  if (ndat gt 1) then begin
    asperrs.ra = errx
    asperrs.dec = erry
    asperrs.roll = erroll
  endif else begin
    asperrs.ra = errx(0)
    asperrs.dec = erry(0)
    asperrs.roll = erroll(0)
  endelse
endif
if (nobi gt 1) then iend = [ibeg(1:*)-1,n_elements(sctime)-1] else $
                    iend = [n_elements(sctime)-1]
if (!debug gt 2) then stop,' Stopping after all obis read'
;
row = {obitime,sctbeg:0.0D0,yrbeg:0,daybeg:0,utsbeg:0.0D0,$
               sctend:0.0D0,yrend:0,dayend:0,utsend:0.0D0,ibeg:0L,iend:0L}
obinfo = replicate(row,nobi)
if (nobi gt 1) then begin
  obinfo.sctbeg = sctbeg
  obinfo.yrbeg = yrbeg
  obinfo.daybeg = daybeg
  obinfo.utsbeg = utsbeg
  obinfo.sctend = sctend
  obinfo.yrend = yrend
  obinfo.dayend = dayend
  obinfo.utsend = utsend
  obinfo.ibeg = ibeg
  obinfo.iend = iend
endif else begin
  obinfo.sctbeg = sctbeg(0)
  obinfo.yrbeg = yrbeg(0)
  obinfo.daybeg = daybeg(0)
  obinfo.utsbeg = utsbeg(0)
  obinfo.sctend = sctend(0)
  obinfo.yrend = yrend(0)
  obinfo.dayend = dayend(0)
  obinfo.utsend = utsend(0)
  obinfo.ibeg = ibeg(0)
  obinfo.iend = iend(0)
endelse
if (!debug gt 2) then stop,' Stopping after obinfo defined'
;
sz=size(inputs)
if (sz(0) ne 0) then inputs = oparms        ;inputs = parameter string array
;
return
end         ;pro oldgetasp
