;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       oldgethksta
;
;*PURPOSE:
;   A procedure to read housekeeping status data from Rosat housekeeping 
;   status data (.STA) files (do NOT convert files to ST SDAS format!)
;
;*CALLING SEQUENCE:
;       OLDGETHKSTA, inputs, obinum, SCTIME, HKFLAGS, OBINFO=OBINFO,
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
;        IDL>oldgethksta,1,obinum,sctime,highv,car, ...
;             ?obseq=rp123456
;	      ?dir=mydirectory
;	      ?trim=1
;	      ?chatter=0
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydirectory,trim=1,chatter=0'
;        IDL>oldgethksta,list,obinum,sctime,hkflags, ...
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,10)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydirectory'
;        IDL>list(8)='1'
;        IDL>list(9)='0'
;        IDL>oldgethksta,list,obinum,sctime,hkflags, ...
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;	     chatter=0
;            *exit
;        IDL>oldgethksta,'myinput.dat',obinum,sctime,hkflags, ...
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
;    written  14 Feb 1992 by GAR
;    modified 18 Feb 1992 to return OBI info as a data structure (GAR)
;    modified 01 Mar 1992 to return outputs in a data structure (GAR)
;    modified 05 Jun 1992 to use READFITS and new version of MATCH_FILES, 
;      and to return OPARMS (GAR)
;    modified 08 Jul 1992 to fix bug (GAR)
;    modified 13 Aug 1993 (GAR) to use read archival MPE format data, and
;      to replace call to RSGETSEQINFO with call to RS_SEQINFO
;    modified 22 Dec 1993 (GAR): name changed from RSGETHKSTA to OLDGETHKSTA
;      (to allow RSGETHKSTA to read RDF format files as well as US & MPE format)
;-
;-------------------------------------------------------------------------------
pro oldgethksta,inputs,obinum,sctime,hkflags,obinfo=obinfo,oparms=oparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' OLDGETHKSTA, inputs, obinum (0 for all), SCTIME, HKFLAGS,'
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
extyp = strtrim(oparms(2),2)
instr = strtrim(oparms(3),2)
proc = strtrim(oparms(4),2)            ;processing format - US or MPE
proc = strupcase(proc)
case proc of
  'US':  begin
         if (extyp eq '') then extyp = 'sta'
         ext = '.'+extyp
         extuc = strupcase(extyp)
         if (extuc ne 'STA') then begin
           print,extyp,' is not a valid extension for a US format binned', $
                       ' housekeeping data file.'
           print,' Valid option is STA. Returning.'
           retall 
         endif
         end
  'MPE': ext = '_quality.tfits'
  else : begin
         print,' This routine only reads aspect data from US or MPE binned', $
               ' housekeeping data files.'
         print,' Requested format is ',proc,'. Please check your inputs.', $
               ' Returning.'
         retall
         end
endcase
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
fits_info,name,/silent,n_ext=next      ;get number of OBI extensions
if (obinum(0) eq 0) then $
   obinum = indgen(next) + 1           ;OBI numbers start with 1
nobi = n_elements(obinum)              ;if OBIs are specified
;
rs_seqinfo,name,nobi,actim,nomasp,seqbeg,seqend,obibeg,obiend,proc=proc
;
sctime = dblarr(1) - 999.
if (nobi gt 1) then begin
  sctbeg = transpose( obibeg(0,*) )
  sctend = transpose( obiend(0,*) )
  yrbeg = transpose( fix(obibeg(1,*)) )
  yrend = transpose( fix(obiend(1,*)) )
  daybeg = transpose( fix(obibeg(2,*)) )
  dayend = transpose( fix(obiend(2,*)) )
  utsbeg = transpose( obibeg(3,*) )
  utsend = transpose( obiend(3,*) )
endif else begin
  sctbeg = obibeg(0,*)
  sctend = obiend(0,*)
  yrbeg = fix(obibeg(1,*))
  yrend = fix(obiend(1,*))
  daybeg = fix(obibeg(2,*))
  dayend = fix(obiend(2,*))
  utsbeg = obibeg(3,*)
  utsend = obiend(3,*)
endelse
;
carousel = [-999] & highv = carousel & det = carousel
gas = [-999] & temp = gas & lowv = gas
current = [-999] & inuse = current & fwpos = current & telem = current
ibeg = intarr(nobi)+long(0.) 
;
if (chatter eq 1) then $
   print,' Reading ',extyp,' housekeeping status flags file ',name
ict = 0
for jj=0,nobi-1 do begin
  jobi = obinum(jj)
  if (chatter eq 1) then print,'    OBI ',jobi
  tab = readfits(name,hdr,ext=jobi,/sil)
;
  ftype = 'ITI_STA'
  if (proc eq 'MPE') then ftype = 'TIME'
  entry = fits_get(hdr,tab,ftype)                  ;Time of hk data (s/c, sec)
  ind = where(entry gt 0)
;
  if ((trim eq 1) and (proc eq 'US')) then begin  ;trim time array, if desired
    ind = where( (entry ge sctbeg(jj)) and (entry le sctend(jj)) )
    if (ind(0) ge 1) then ind=[ind(0)-1,ind]
    nument = n_elements(entry)
    if (max(ind) le (nument-2)) then ind = [ind,max(ind)+1]
  endif
  entry = entry(ind)
;
  nument = n_elements(entry)                  ;number in this section
  sctime = [sctime,entry]
  begt = entry(0)
  endt = entry(nument-1)
  ibeg(jj) = ict
  ict = ict + nument
;
; correct UT start time if spacecraft start time read from header
; is not same as first spacecraft time in time array
;
  if (sctbeg(jj) ne begt) then utsbeg(jj) = utsbeg(jj) + begt - sctbeg(jj)
  sctbeg(jj) = begt
;
; correct spacecraft stop time if UT stop time read from header
; is not same as last UT time in time array
;
  if (sctend(jj) ne endt) then utsend(jj) = utsend(jj) + endt - sctend(jj)
  sctend(jj) = endt
;
; now read other parameters, if desired
;
  if (npar ge 4) then begin
    ftype = 'IHV_STA'
    if (proc eq 'MPE') then ftype = 'HIGHVOLT_S'
    entry = fits_get(hdr,tab,ftype)              ;status of high voltage
    entry = entry(ind)
    highv = [highv,entry]
;
    ftype = 'ICR_STA'
    if (proc eq 'MPE') then ftype = 'CAROUSEL_S'
    entry = fits_get(hdr,tab,ftype)              ;status of carousel
    entry = entry(ind)
    carousel = [carousel,entry]
;
    ftype = 'IGS_STA'
    if (proc eq 'MPE') then ftype = 'GAS_S'
    entry = fits_get(hdr,tab,ftype)              ;status of gas system
    entry = entry(ind)
    gas = [gas,entry]
;
    ftype = 'IDT_STA'
    if (proc eq 'MPE') then ftype = 'DETECTOR_S'
    entry = fits_get(hdr,tab,ftype)              ;status of detector
    entry = entry(ind)
    det = [det,entry]
;
    ftype = 'ITE_STA'
    if (proc eq 'MPE') then ftype = 'TEMPERAT_S'
    entry = fits_get(hdr,tab,ftype)              ;status of temperature
    entry = entry(ind)
    temp = [temp,entry]
;
    ftype = 'ILV_STA'
    if (proc eq 'MPE') then ftype = 'LOWVOLT_S'
    entry = fits_get(hdr,tab,ftype)              ;status of low voltage
    entry = entry(ind)
    lowv = [lowv,entry/10.]
;
    ftype = 'ICU_STA'
    if (proc eq 'MPE') then ftype = 'CURRENT_S'
    entry = fits_get(hdr,tab,ftype)              ;status of current
    entry = entry(ind)
    current = [current,entry]
;
    if (proc ne 'MPE') then begin
      ftype = 'IIN_STA'
      entry = fits_get(hdr,tab,ftype)            ;instrument in use
      entry = entry(ind)
      inuse = [inuse,entry]
    endif
;
    if (proc ne 'MPE') then begin
      ftype = 'IFR_STA'
      entry = fits_get(hdr,tab,ftype)            ;filter wheel position
      entry = entry(ind)
      fwpos = [fwpos,entry]
    endif
;
    if (proc ne 'MPE') then begin
      ftype = 'ITL_STA'
      entry = fits_get(hdr,tab,ftype)            ;send mode
      entry = entry(ind)
      telem = [telem,entry]
    endif
  endif
;
endfor
;
sctime = sctime(1:*)
nstat = n_elements(sctime)
if (npar ge 4) then begin
  highv = fix(highv(1:*))
  carousel = fix(carousel(1:*))
  gas = fix(gas(1:*))
  det = fix(det(1:*))
  temp = fix(temp(1:*))
  lowv = fix(lowv(1:*))
  current = fix(current(1:*))
  if (proc ne 'MPE') then inuse = fix(inuse(1:*))
  if (proc ne 'MPE') then fwpos = fix(fwpos(1:*))
  if (proc ne 'MPE') then telem = fix(telem(1:*))
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
  if (proc ne 'MPE') then hkflags.inuse = inuse
  if (proc ne 'MPE') then hkflags.fwpos = fwpos
  if (proc ne 'MPE') then hkflags.telem = telem
;
  highv = 0 & carousel = 0 & gas = 0 & det = 0
  temp = 0 & lowv = 0 & current = 0 & inuse = 0 
  fwpos = 0 & telem = 0
endif
;
if (nobi gt 1) then iend = [ibeg(1:*)-1,n_elements(sctime)-1] else $
                    iend = [n_elements(sctime)-1]
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
;
sz=size(inputs)
if (sz(0) ne 0) then inputs = oparms        ;inputs = parameter string array
;
return
end         ;pro oldgethksta
