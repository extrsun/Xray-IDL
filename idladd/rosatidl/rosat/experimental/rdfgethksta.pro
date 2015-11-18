;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       rdfgethksta
;
;*PURPOSE:
;   A procedure to read housekeeping status data from Rosat housekeeping 
;   status data (.STA) files (do NOT convert files to ST SDAS format!)
;
;*CALLING SEQUENCE:
;       RDFGETHKSTA, inputs, obinum, SCTIME, HKFLAGS, OBINFO=OBINFO,
;                    OPARMS=OPARMS
;
;*PARAMETERS:
; INPUTS:
;       inputs - input parameter description.
;                0 - use all defaults
;                1 - interative selection of parameters
;                string - parameter defintion of the form
;                   'parname1=value1,parname2=value2,,,,'
;                string array - each element of the array has
;                   form 'value'  (entries must be given in correct order)
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
;        EXTYP       Extension of input file (e.g., STA)
;                    (default is STA)
;        INSTR       Instrument 
;                    (default is P for PSPC)
;        PROC        Format of processed files (e.g., US, MPE)
;                    (default is 'US')
;        TRIM        Determines whether or not data is trimmed to Good
;                    Time Intervals (default = 1)
;        CHATTER     Controls program feedback to user
;                    (default = 1)
;
; OPTIONAL INPUTS:
;       obinum - vector containing numbers of OBIs to read (0 for all)
;
; OUTPUTS:
;       sctime   - spacecraft times for which aspect quality was determined
;       hkflags  - data structure containing the following:
;         highv    - Status of high voltage flag
;                    1 = OK, 0 = not OK, -1 = no data
;                    = IHV_STA (US), HIGHVOLT_S (MPE), HV_STA (RDF)
;         carousel - Status of carousel flag
;                    1 = OK, 0 = not OK, -1 = no data
;                    = ICR_STA (US), CAROUSEL_S (MPE), CARR_STA (RDF)
;         gas      - Status of gas system flag
;                    1 = OK, 0 = not OK, -1 = no data
;                    = IGS_STA (US), GAS_S (MPE), GAS_STA (RDF)
;         det      - Status of detector flag
;                    2 = calibration, 1 = OK, 0 = not OK, -1 = no data
;                    = IDT_STA (US), DETECTOR_S (MPE), DET_STA (RDF)
;         temp     - Status of temperature flag
;                    1 = OK, 0 = not OK, -1 = no data
;                    = ITE_STA (US), TEMPERAT_S (MPE), TEMP_STA (RDF)
;         lowv     - Status of low voltage flag
;                    1 = OK, 0 = not OK, -1 = no data
;                    = ILV_STA (US), LOWVOLT_S (MPE), LV_STA (RDF)
;         current  - Status of current flag
;                    1 = OK, 0 = not OK, -1 = no data
;                    = ICU_STA (US), CURRENT_S (MPE), CURR_STA (RDF)
;         inuse    - Instrument in use 
;                    1 = PSPC C, 2 = PSPC B, 3 = HRI, 0 = other, -1=no data
;                    = IIN_STA (US), INST_STA (RDF)
;         fwpos    - Filter wheel position
;                    0 = cup, 1 = calibration, 2 = open, 3 = closed,
;                    4 = filter, 5 = other, 6 = undefined, -1 = no data
;                    = IFR_STA (US), FILT_STA (RDF)
;         telem    - Send mode
;                    1 = normal interval, 2 = contact interval, -1 = no data
;                    = ITL_STA (US), TEL_STA (RDF)
;       has the structure of replicate(row,nstat), where
;       row = {hkstatflags,highv:0,carousel:0,gas:0,det:0,temp:0,lowv:0,$
;                          current:0,inuse:0,fwpos:0,telem:0}
;       and nstat is the total number of measruements
;
;       obinfo - data structure containing dates, times, & indices of
;                beginnings & ends of obi segments
;         ibeg - indices of values in SCTIME equal to values in SCTBEG
;         iend - indices of values in SCTIME equal to values in SCTEND
;       has the structure of replicate(row,num), where
;       row={obitime,sctbeg:0.0D0,yrbeg:0,daybeg:0,utsbeg:0.0D0,
;       sctend:0.0D0,yrend:0,dayend:0,utsend:0.0D0,
;       ibeg:0L,iend:0L}
;       and num is the total number of intervals
;
;       oparms - string array which is the parsed form of inputs (same as
;                LIST in example 3 below). Allows program to be rerun using 
;                same values w/o resetting.
;
;*EXAMPLES: There are 4 different ways that the inputs may be specified
;    mode 1) Interactive
;        IDL>rdfgethksta,1,obinum,sctime,highv,car, ...
;             ?obseq=rp123456
;	      ?dir=mydirectory
;	      ?trim=1
;	      ?chatter=0
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydirectory,trim=1,chatter=0'
;        IDL>rdfgethksta,list,obinum,sctime,hkflags, ...
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,10)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydirectory'
;        IDL>list(8)='1'
;        IDL>list(9)='0'
;        IDL>rdfgethksta,list,obinum,sctime,hkflags, ...
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;	     chatter=0
;            *exit
;        IDL>rdfgethksta,'myinput.dat',obinum,sctime,hkflags, ...
;
;*RESTRICTIONS:
;   Will not yet work for HRI
;
;*NOTES:
;   Will read FITS files directly. Do NOT convert to ST SDAS format.
;   The MPE format _quality.tfits file does not contain the
;   Instrument-in-use, filter wheel position, and telemetry status flag
;   information, and so these quantities are set to zeroes in the hkflags 
;   data structure 
;
;*SUBROUTINES CALLED:
;  RSGETPAR
;  MATCH_FILES
;  READFITS
;  TBGET
;  RS_SEQINFO
;  US_SEQINFO
;  MPE_SEQINFO
;  GETHISTVAL
;
;*MODIFICATION HISTORY:
;    written  22 Dec 1993 by GAR (adapted from RSGETHKSTA & OLDGETHKSTA)
;-
;-------------------------------------------------------------------------------
pro rdfgethksta,inputs,obinum,sctime,hkflags,obinfo=obinfo,oparms=oparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RDFGETHKSTA, inputs, obinum (0 for all), SCTIME, HKFLAGS,'
  print,'              OBINFO=OBINFO, OPARMS=OPARMS'
  print,' Uses inputs OBSEQ, DIR, EXTYP, INSTR, PROC, TRIM, and CHATTER'
  print,' from RSGET.DEF'
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
proc = strtrim(oparms(4),2)            ;processing format - RDF only
proc = strupcase(proc)
if (proc ne 'RDF') then begin
  print,' This routine only reads binned housekeeping data from RDF format', $
        ' ancillary files.'
  print,' Requested format is ',proc,'. Please check your inputs. Returning.'
  retall
endif  
trim = strtrim(oparms(8),2)
chatter = fix(oparms(9))
;
if (obseq eq '') then begin                      ;obseq must be defined
  read,'Please enter root name of input data file: ',obseq
  obseq = strtrim(obseq,2)
  oparms(0) = obseq
endif  
;
if (trim eq '') then begin                        ; if trim not set
  read,' Trim binned housekeeping data to good time intervals? (Y or N)',trim
  oparms(8) = trim
endif
yesno,trim
;
if (npar lt 2) then obinum = 0                 ;default is to read all
;
match_files,dir,obseq,ext,name,nlist        ;look for the right files
name = name(0)                              ;default = take latest version
oparms(0) = strtrim(obseq,2)        ;obseq & dir may have been corrected
oparms(1) = strtrim(dir,2)
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
   print,' Reading housekeeping flags from RDF ancillary data file ',name
tab=readfits(name,hdr,ext=5,/sil)         ;hk status flags in 5th extension
sctime=fits_get(hdr,tab,'TIME')
nstat = n_elements(sctime)
;
igt = where(sctime gt sctbeg(0),ngt)      ;in case some times are bogus zeroes
tabinv,sctime(igt),sctbeg,ibeg
tabinv,sctime(igt),sctend,iend
ibeg = nint(igt(ibeg))
iend = nint(igt(iend))
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
  nstat = n_elements(indsel)
  sctime = sctime(indsel)
endif
if (!debug gt 2) then stop,' Stopping after indsel, nhkb defined'
;
; now read other parameters, if desired
;
if (npar ge 4) then begin
  ftype = 'HV_STA'
  highv = fits_get(hdr,tab,ftype)                ;status of high voltage
  if (obisel) then highv = highv(indsel)
;
  ftype = 'CARR_STA'
  carousel = fits_get(hdr,tab,ftype)             ;status of carousel
  if (obisel) then carousel = carousel(indsel)
;
  ftype = 'GAS_STA'
  gas = fits_get(hdr,tab,ftype)                  ;status of gas system
  if (obisel) then gas = gas(indsel)
;
  ftype = 'DET_STA'
  det = fits_get(hdr,tab,ftype)                  ;status of detector
  if (obisel) then det = det(indsel)
;
  ftype = 'TEMP_STA'
  temp = fits_get(hdr,tab,ftype)                 ;status of temperature
  if (obisel) then temp = temp(indsel)
;
  ftype = 'LV_STA'
  lowv = fits_get(hdr,tab,ftype)                 ;status of low voltage
  if (obisel) then lowv = lowv(indsel)
;
  ftype = 'CURR_STA'
  current = fits_get(hdr,tab,ftype)              ;status of current
  if (obisel) then current = current(indsel)
;
  ftype = 'INST_STA'
  inuse = fits_get(hdr,tab,ftype)                ;instrument in use
  if (obisel) then inuse = inuse(indsel)
;
  ftype = 'FILT_STA'
  fwpos = fits_get(hdr,tab,ftype)                ;filter wheel position
  if (obisel) then fwpos = fwpos(indsel)
;
  ftype = 'TEL_STA'
  telem = fits_get(hdr,tab,ftype)                ;send mode
  if (obisel) then telem = telem(indsel)
;
  row = {hkstatflags,highv:0,carousel:0,gas:0,det:0,temp:0,lowv:0,$
         current:0,inuse:0,fwpos:0,telem:0}
  hkflags = replicate(row,nstat)
  hkflags.highv = highv
  hkflags.carousel = carousel
  hkflags.gas = gas
  hkflags.det = det
  hkflags.temp = temp
  hkflags.lowv = lowv
  hkflags.current = current
  hkflags.inuse = inuse
  hkflags.fwpos = fwpos
  hkflags.telem = telem
;
  highv = 0 & carousel = 0 & gas = 0 & det = 0
  temp = 0 & lowv = 0 & current = 0 & inuse = 0 
  fwpos = 0 & telem = 0
endif
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
;
sz=size(inputs)
if (sz(0) ne 0) then inputs = oparms        ;inputs = parameter string array
;
return
end         ;pro rdfgethksta
