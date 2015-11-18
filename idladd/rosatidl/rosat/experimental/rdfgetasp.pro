;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       rdfgetasp
;
;*PURPOSE:
;   A procedure to read aspect data (roll angle, pointing RA and Dec, etc.)
;   from the aspect extensions of RDF format ancillary FITS files
;
;*CALLING SEQUENCE:
;       RDFGETASP, inputs, obinum, SCTIME, ROLL, DELX, DELY, NOMASP, 
;                  ASPERRS, SCASP, FLAGS, OBINFO=OBINFO, OPARMS=OPARMS
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
;       obinum - vector containing numbers of OBIs to read (0 for all)
;
; OUTPUTS:
;       sctime  - spacecraft times for which event rates were determined
;       roll    - roll angle of detector (in degrees)
;                 PSPC: = IRO_CAS or IRO_SA (US), ROLL (MPE), ROAN_CAS (RDF)
;                 HRI:  = ASPECT_ROLL (US), ROAN_SC (RDF)
;       delx    - x translation in NS system (in units of DEGREES)
;               = change in pointing RA relative to nominal
;                 PSPC: = IXN_CAS or IRA_SA (US), XOFFSET (MPE), RA_CAS (RDF)
;                 HRI:  = ASPECT_XOFF (US), RA_SC - nominal RA (MPE)
;       dely    - y translation in NS system (in units of DEGREES)
;               = change in pointing Dec relative to nominal 
;                 PSPC: = IYN_CAS or IDE_SA (US), YOFFSET (MPE), DEC_CAS (RDF)
;                 HRI:  = ASPECT_YOFF (US), DEC_SC - nominal DEC (MPE)
;       nomasp  - 2D array containing nominal RA, Dec and Roll for each
;                 obi segment
;       asperrs - data structure containing RA, Dec, and Roll angle 
;                 uncertainties for each time of aspect measurement
;                 (in degrees for RA, Dec, Roll angle)
;                 = ISRA_SA, ISDE_SA, ISRO_SA (US)
;                 = RA_ERR, DEC_ERR, ROAN_ERR (RDF)
;                 has the structure of replicate(row,ndat) where
;                 row={aspecterrors,ra:0.0D0,dec:0.0D0,roll:0.0D0}
;                 and ndat is the total number of data points
;
;       scasp   - data structure containing RA, Dec and Roll angles "of the
;                 spacecraft pointing" (uncorrected?) for each time of
;                 aspect measurement
;                 (in degrees for RA, Dec, Roll angle)
;                 = RA_SC, DEC_SC, ROAN_SC (RDF)
;                 has the structure of replicate(row,ndat) where
;                 row={scaspects,ra:0.0D0,dec:0.0D0,roll:0.0D0}
;                 and ndat is the total number of data points
;
;       flags   - data structure containing aspect quality and star tracker 
;                 flags, and aspect status
;                 has the structure of replicate(row,ndat) where
;                 row={scaspflags,qual:0.0D0,stt:0.0D0,status:0}
;                 and ndat is the total number of data points
;
;       obinfo  - data structure containing dates, times, & indices of
;                 beginnings & ends of obi segments
;                 has the structure of replicate(row,num), where
;                 row={obitime,sctbeg:0.0D0,yrbeg:0,daybeg:0,utsbeg:0.0D0,
;                      sctend:0.0D0,yrend:0,dayend:0,utsend:0.0D0,
;                      ibeg:0L,iend:0L}
;                 and num is the total number of intervals
;         ibeg  - indices of values in SCTIME equal to values in SCTBEG
;         iend  - indices of values in SCTIME equal to values in SCTEND
;
;       oparms - string array which is the parsed form of inputs (same as
;                LIST in example 3 below). Allows program to be rerun using 
;                same values w/o resetting.
;
;*EXAMPLES: There are 4 different ways that the inputs may be specified
;    mode 1) Interactive
;        IDL>rdfgetasp,1,obinum,sct,roll,delx,dely, ...
;             ?obseq=rp123456
;	      ?dir=mydirectory
;	      ?extyp=ao
;	      ?chatter=0
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydirectory,extyp=ao,chatter=0'
;        IDL>rdfgetasp,list,obinum,sctime,roll,delx,dely, ...
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,10)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydirectory'
;        IDL>list(2)='ao'
;        IDL>list(9)='0'
;        IDL>rdfgetasp,list,obinum,sctime,roll,delx,dely, ...
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;	     chatter=0
;            *exit
;        IDL>rdfgetasp,'myinput.dat',obinum,sctime,roll,delx,dely, ...
;
;*RESTRICTIONS:
;
;*NOTES:
;  Reads FITS files directly. Do NOT convert to ST SDAS format.
;  Input parameters INSTR and TRIM are ignored (and need not be specified).
;
;  RSGETASP and OLDGETASP return delx, dely, etc. in units of 0.5 arcsec pixels.
;  This routine returns them in units of degrees (as they are written in the
;    files).
;
;*SUBROUTINES CALLED:
;  RSGETPAR
;  MATCH_FILES
;  READFITS
;  RSOBITIMES
;  FITS_GET
;  FTGET
;  TBGET
;  TABINV
;  NINT
;  RS_NOMASP
;
;*MODIFICATION HISTORY:
;    written  19 Dec 1993 by GAR (adapted from RSGETASP)
;    modified 22 Dec 1993 (GAR) to return aspect quality & star tracker flags,
;      aspect status in data structure FLAGS
;    modified 23 Dec 1993 (GAR) to read from HRI RDF ancillary files
;-
;-------------------------------------------------------------------------------
pro rdfgetasp,inputs,obinum,sctime,roll,delx,dely,nomasp,asperrs,scasp,$
              flags,obinfo=obinfo,oparms=oparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RDFGETASP, inputs, obinum (0 for all), SCTIME, ROLL, DELX, DELY,'
  print,'            NOMASP, ASPERRS, SCASP, FLAGS, OBINFO=OBINFO,', $ 
        ' OPARMS=OPARMS'
  print,'   Uses inputs OBSEQ, DIR, EXTYP, INSTR, PROC, and CHATTER ' $
       +'from RSGET.DEF'
  return
endif
;
dfile = 'rsget'
rsgetpar,inputs,dfile,oparms
obseq = strtrim(oparms(0),2)
dir = strtrim(oparms(1),2)
;
;extyp = strtrim(oparms(2),2)          ;don't care what extyp is for now
ext = '_anc.fits'
;
instr = strtrim(oparms(3),2)
instr = strupcase(instr)
proc = strtrim(oparms(4),2)            ;processing format - RDF only
proc = strupcase(proc)
if (proc ne 'RDF') then begin
  print,' This routine only reads aspect data from RDF format ancillary files.'
  print,' Requested format is ',proc,'. Please check your inputs. Returning.'
  retall
endif  
chatter = fix(oparms(9))
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
; Now use match_files to get the whole file name for the data file.
;
match_files,dir,obseq,ext,name,nlist        ;look for the right files
name = name(0)                              ;default = take latest version
oparms(0) = strtrim(obseq,2)        ;obseq & dir may have been corrected
oparms(1) = strtrim(dir,2)
if (!debug gt 2) then stop,' Stopping after files have been found'
;
; Get number of OBIs from the main header
;
hdr = headfits(name)                 
next = fxpar(hdr,'NUM_OBIS')
if (obinum(0) eq 0) then begin
  obisel = 0                          ;will not restrict data by OBI
  obinum = indgen(next) + 1           ;OBI numbers start with 1
endif else obisel = 1                 ;will restrict data by OBI
nobi = n_elements(obinum)              ;if OBIs are specified
if (!debug gt 2) then stop,' Stopping after getting the number of OBIs'
;
; Get OBI information. 
;
rsobitimes,hdr,sctbeg,sctend,utbeg,utend,proc=proc
;
yrbeg = transpose(utbeg(0,*))
daybeg = transpose(utbeg(1,*))
utsbeg = transpose(utbeg(2,*))
yrend = transpose(utend(0,*))
dayend = transpose(utend(1,*))
utsend = transpose(utend(2,*))
;
if (obisel) then begin
  sctbeg = sctbeg(obinum-1)
  yrbeg = yrbeg(obinum-1)
  daybeg = daybeg(obinum-1)
  utsbeg = utsbeg(obinum-1)
  sctend = sctend(obinum-1)
  yrend = yrend(obinum-1)
  dayend = dayend(obinum-1)
  utsend = utsend(obinum-1)
endif
;
if (chatter eq 1) then $
   print,' Reading aspect data from RDF ancillary data file ',name
tab=readfits(name,hdr,ext=2,/sil)         ;aspect data in 2nd extension
sctime=fits_get(hdr,tab,'TIME')
ndat = n_elements(sctime)
;
; Get the number of unique OBIs, and the indices where these change
;
obival = fits_get(hdr,tab,'OBI_NUM')
nsize = 10000.
if (!version.os eq 'vms') then nsize = 2*nsize  
if (ndat le nsize) then vsort,obival,1,trueval,indsrt,ibeg,iend,ntrue,mid=1 $
   else vsort_large,obival,1,trueval,indsrt,ibeg,iend,ntrue,nsize=nsize
if (!debug gt 2) then stop,' Stopping after ibeg, iend defined'
;
if (obisel) then begin             ;set up indices to select by OBI, if desired
  indsel = [-999]
  for jj=0,nobi-1 do begin
    ib = ibeg(jj)
    ie = iend(jj)
    indsel = [indsel,ib + lindgen(ie-ib+1)]
  endfor
  indsel = indsel(1:*)
  ndat = n_elements(indsel)
  sctime = sctime(indsel)
endif
if (!debug gt 2) then stop,' Stopping after indsel, ndat defined'
;
; now read other parameters, if desired
;
if (npar ge 4) then begin
  ftype = 'ROAN_CAS'
  if (instr eq 'H') then ftype = 'ROAN_SC'
  roll = fits_get(hdr,tab,ftype)      ;Roll angle already given in degrees
  if (obisel) then roll = roll(indsel)
endif
;
if ( (npar ge 7) or (instr eq 'H') ) then begin
  rs_nomasp,hdr,nomra,nomdec,nomroll,proc=proc
  nomasp = dblarr(3,nobi)
  for jj=0,nobi-1 do nomasp(0,jj) = [nomra,nomdec,nomroll]
endif
;
if (npar ge 5) then begin
  ftype = 'RA_CAS'
  if (instr eq 'H') then ftype = 'RA_SC'
  delx = fits_get(hdr,tab,ftype)         ;X translation (in NS system, degrees)
  if (obisel) then delx = delx(indsel)
  if (instr eq 'H') then delx = delx - nomasp(0)/7200.
endif
;
if (npar ge 6) then begin
  ftype = 'DEC_CAS'
  if (instr eq 'H') then ftype = 'DEC_SC'
  dely = fits_get(hdr,tab,ftype)         ;Y translation (in NS system, degrees)
  if (obisel) then dely = dely(indsel)
  if (instr eq 'H') then dely = dely - nomasp(1)/7200.
endif
;
if (npar ge 8) then begin
  ftype = 'RA_ERR'
  raerr = fits_get(hdr,tab,ftype)
  if (obisel) then raerr = raerr(indsel)
  ftype = 'DEC_ERR'
  decerr = fits_get(hdr,tab,ftype)
  if (obisel) then decerr = decerr(indsel)
  ftype = 'ROAN_ERR'
  rollerr = fits_get(hdr,tab,ftype)
  if (obisel) then rollerr = rollerr(indsel)
;
  row = {aspecterrors,ra:0.0D0,dec:0.0D0,roll:0.0D0}
  asperrs = replicate(row,ndat)
  if (ndat gt 1) then begin
    asperrs.ra = raerr
    asperrs.dec = decerr
    asperrs.roll = rollerr
  endif else begin
    asperrs.ra = raerr(0)
    asperrs.dec = decerr(0)
    asperrs.roll = rollerr(0)
  endelse
;
  raerr = 0           ;to save memory
  decerr = 0
  rollerr = 0
endif
;
if (npar ge 9) then begin
  ftype = 'RA_SC'
  scra = fits_get(hdr,tab,ftype)
  if (obisel) then scra = scra(indsel)
  ftype = 'DEC_SC'
  scdec = fits_get(hdr,tab,ftype)
  if (obisel) then scdec = scdec(indsel)
  ftype = 'ROAN_SC'
  scroll = fits_get(hdr,tab,ftype)  
  if (obisel) then scroll = scroll(indsel)
;
  row = {scaspects,ra:0.0D0,dec:0.0D0,roll:0.0D0}
  scasp = replicate(row,ndat)
  if (ndat gt 1) then begin
    scasp.ra = scra
    scasp.dec = scdec
    scasp.roll = scroll
  endif else begin
    scasp.ra = scra(0)
    scasp.dec = scdec(0)
    scasp.roll = scroll(0)
  endelse
;
  scra = 0           ;to save memory
  scdec = 0
  scroll = 0
endif
;
if (npar ge 10) then begin
  ftype = 'ASP_QUAL'
  qual = fits_get(hdr,tab,ftype)
  if (obisel) then qual = qual(indsel)
  ftype = 'STT_QUAL'
  stt = fits_get(hdr,tab,ftype)
  if (obisel) then stt = stt(indsel)
  if (instr eq 'P') then begin
    ftype = 'STATUS'
    aspstat = fits_get(hdr,tab,ftype)
    if (obisel) then aspstat = aspstat(indsel)
  endif
;
  row = {scaspflags,qual:0.0D0,stt:0.0D0,status:0}
  flags = replicate(row,ndat)
  if (ndat gt 1) then begin
    flags.qual = qual
    flags.stt = stt
    if (instr eq 'P') then flags.status = aspstat
  endif else begin
    flags.qual = qual(0)
    flags.stt = stt(0)
    if (instr eq 'P') then flags.status = aspstat(0)
  endelse
;
  qual = 0           ;to save memory
  stt = 0
  aspstat = 0
endif
;
; Define the data structure OBINFO & fill in the values
;
row = {obitime,sctbeg:0.0D0,yrbeg:0,daybeg:0,utsbeg:0.0D0,$
               sctend:0.0D0,yrend:0,dayend:0,utsend:0.0D0,ibeg:0L,iend:0L}
obinfo = replicate(row,nobi)
if (obisel) then begin
  ibmin = min(ibeg)
  ibeg = ibeg - ibmin
  iend = iend - ibmin
endif
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
end         ;pro rdfgetasp
