;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       oldgethkb
;
;*PURPOSE:
;   A procedure to read housekeeping data (time, temperature, pressure,
;   high voltage, instrument in use, filter wheel flag & position,
;   missing hkb data flag) from Rosat binned housekeeping data (.HKB)
;   files  (do NOT convert files to ST SDAS format!)
;
;*CALLING SEQUENCE:
;       OLDGETHKB, inputs, obinum, SCTIME, HKDAT, OBINFO=OBINFO, OPARMS=OPARMS
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
;        EXTYP       Extension of input file (e.g., HKB)
;                    (default is HKB)
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
;       sctime - spacecraft times for which aspect quality was determined
;       hkdat  - data structure containing the following binned 
;                housekeeping data:
;         mhkb   - flag which tells when housekeeping data is missing
;                  0=okay, 1=last before window, 2=not complete after window,
;                  3=first after window, 4=last reading (time=end+1) after 
;                  window)
;         temp   - temp of PSPC
;         press  - pressure of PSPC
;         hv     - high voltage of PSPC (there is a rumor "floating around" 
;                that this number may not actually mean anything)
;         fwflag - flag that tells what was commanded for the filter wheel
;                (0=cup, 1=calib, 2=open, 3=closed, 4=filter, 5=other)
;         fwpos  - filter wheel position (in degrees)
;         inuse  - flag which tells which instrument was in use
;       has the structure of replicate(row,nhkb), where
;       row = {pspc_binhkdata,mhkb:0,temp:0,press:0,hv:0,fwflag:0,fwpos:0, $
;              inuse:0}
;       and nhkb is the total number of measurements
;
;       obinfo - data structure containing dates, times, & indices of
;                beginnings & ends of obi segments
;         ibeg - indices of values in SCTIME equal to values in SCTBEG
;         iend - indices of values in SCTIME equal to values in SCTEND
;       has the structure of replicate(row,num), where
;       row={obitime,sctbeg:0.0D0,yrbeg:0,daybeg:0,utsbeg:0.0D0,
;       sctend:0.0D0,yrend:0,dayend:0,utsend:0.0D0,ibeg:0L,iend:0L}
;       and num is the total number of intervals
;
;       oparms - string array which is the parsed form of inputs (same as
;                LIST in example 3 below). Allows program to be rerun using 
;                same values w/o resetting.
;
;*EXAMPLES: There are 4 different ways that the inputs may be specified
;    mode 1) Interactive
;        IDL>oldgethkb,1,obinum,sctime,hkdat, ...
;             ?obseq=rp123456
;	      ?dir=mydirectory
;	      ?trim=1
;	      ?chatter=0
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydirectory,trim=1,chatter=0'
;        IDL>oldgethkb,list,obinum,sctime,hkdat, ...
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,10)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydirectory'
;        IDL>list(8)='1'
;        IDL>list(9)='0'
;        IDL>oldgethkb,list,obinum,sctime,hkdat, ...
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;	     chatter=0
;            *exit
;        IDL>oldgethkb,'myinput.dat',obinum,sctime,hkdat, ...
;
;*RESTRICTIONS:
;   Will not yet work for HRI
;
;*NOTES:
;   Will now read FITS files directly. Do NOT convert to ST SDAS format.
;   The MPE format _quality.tfits file does not contain the INUSE or FWFLAG
;   information (or at least I couldn't find it), and so these quantities
;   are set to zeroes in the HKDAT structure variable.
;
;*SUBROUTINES CALLED:
;  RSGETPAR
;  MATCH_FILES
;  READFITS
;  FITS_GET
;  TBGET
;  FTGET
;  RS_SEQINFO
;  US_SEQINFO
;  MPE_SEQINFO
;  GETHISTVAL
;
;*MODIFICATION HISTORY:
;    written 14 Feb 1992 by GAR
;    modified 18 Feb 1992 to return OBI info as a data structure (GAR)
;    modified 01 Mar 1992 to return outputs as a data structure (GAR)
;    modified 05 Jun 1992 to use READFITS and new version of MATCH_FILES, 
;      and to return OPARMS (GAR)
;    modified 22 Jun 1992 to fix bug (GAR)
;    modified 13 Aug 1993 (GAR) to use read archival MPE format data, and
;      to replace call to RSGETSEQINFO with call to RS_SEQINFO
;    modified 22 Dec 1993 (GAR): name changed from RSGETHKB to OLDGETHKB 
;      (to allow RSGETHKB to read RDF format files as well as US & MPE format)
;-
;-------------------------------------------------------------------------------
pro oldgethkb,inputs,obinum,sctime,hkdat,obinfo=obinfo,oparms=oparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' OLDGETHKB, inputs, obinum (0 for all), SCTIME, HKDAT,'
  print,'            OBINFO=OBINFO, OPARMS=OPARMS'
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
         if (extyp eq '') then extyp = 'hkb'
         ext = '.'+extyp
         extuc = strupcase(extyp)
         if (extuc ne 'HKB') then begin
           print,extyp,' is not a valid extension for a US format binned', $
                       ' housekeeping data file.'
           print,' Valid option is HKB. Returning.'
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
if (!debug gt 2) then stop,' Stopping after ext has been defined'
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
;if (n_elements(obinum) eq 1) then obinum = obinum + intarr(1)
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
if (!debug gt 2) then stop,' Stopping after fits_info'
;
; right now, the individual .EVR headers do not contain the start & stop
; times of the OBIs. These must be read from the sequence header
; The MPE processed file headers do not contain any information for the
; individual OBIs at all
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
mhkb = [-999] & temp = mhkb & press= mhkb & hv = mhkb
fwflag = [-999] & fwpos = fwflag & inuse = fwflag
ibeg = intarr(nobi)+long(0.) 
;
if (chatter eq 1) then $
   print,' Reading ',extyp,' binned housekeeping data file ',name
ict = 0
for jj=0,nobi-1 do begin
  jobi = obinum(jj)
  if (chatter eq 1) then print,'    OBI ',jobi
  tab = readfits(name,hdr,ext=jobi,/sil)
;
  ftype = 'ITI_HKB'
  if (proc eq 'MPE') then ftype = 'TIME'
  entry = fits_get(hdr,tab,ftype)          ;Time of hk data (s/c, sec)
  ind = where(entry gt 0)
;
  if ((trim eq 1) and (proc eq 'US')) then begin   ;trim time array, if desired
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
    ftype = 'MIS_HKB'
    if (proc eq 'MPE') then ftype = 'MISSING_H'
    entry = fits_get(hdr,tab,ftype)            ;missing housekeeping data flag
    entry = entry(ind)
    mhkb = [mhkb,entry]
;
    ftype = 'ITE_HKB'
    if (proc eq 'MPE') then ftype = 'TEMPERAT_H'
    entry = fits_get(hdr,tab,ftype)            ;temperature of PSPC
    entry = entry(ind)
    temp = [temp,entry]
;
    ftype = 'IPR_HKB'
    if (proc eq 'MPE') then ftype = 'PRESSURE_H'
    entry = fits_get(hdr,tab,ftype)            ;pressure of PSPC
    entry = entry(ind)
    press = [press,entry]
;
    ftype = 'IHV_HKB'
    if (proc eq 'MPE') then ftype = 'HIGHVOLT_H'
    entry = fits_get(hdr,tab,ftype)            ;high voltage of PSPC
    entry = entry(ind)
    hv = [hv,entry]
;
    if (proc ne 'MPE') then begin
      ftype = 'IFR_HKB'
      entry = fits_get(hdr,tab,ftype)            ;filter wheel flag
      entry = entry(ind)
      fwflag = [fwflag,entry]
    endif
;
    ftype = 'IFS_HKB'
    if (proc eq 'MPE') then ftype = 'FILT_POS_H'
    entry = fits_get(hdr,tab,ftype)            ;filter wheel position (degrees)
    entry = entry(ind)
    fwpos = [fwpos,entry/10.]
;
    if (proc ne 'MPE') then begin
      ftype = 'IIN_HKB'
      entry = fits_get(hdr,tab,ftype)            ;instrument in use'
      entry = entry(ind)
      inuse = [inuse,entry]
    endif
  endif
;
endfor
;
sctime = sctime(1:*)
nhkb = n_elements(sctime)
if (npar ge 4) then begin
  mhkb = fix(mhkb(1:*))
  temp = fix(temp(1:*))
  press = fix(press(1:*))
  hv = fix(hv(1:*))
  if (proc ne 'MPE') then fwflag = fix(fwflag(1:*))
  fwpos = fix(fwpos(1:*))
  if (proc ne 'MPE') then inuse = fix(inuse(1:*))
;
  row = {pspc_binhkdata,mhkb:0,temp:0,press:0,hv:0,fwflag:0,fwpos:0,inuse:0}
  hkdat = replicate(row,nhkb)
  hkdat.mhkb = mhkb
  hkdat.temp = temp
  hkdat.press = press
  hkdat.hv = hv
  if (proc ne 'MPE') then hkdat.fwflag = fwflag
  hkdat.fwpos = fwpos
  if (proc ne 'MPE') then hkdat.inuse = inuse
;
  mhkb = 0 & temp = 0 & press = 0 & hv = 0
  fwflag = 0 & fwpos = 0 & inuse = 0
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
end         ;pro oldgethkb
