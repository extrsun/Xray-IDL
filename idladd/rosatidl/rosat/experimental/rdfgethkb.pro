;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       rdfgethkb
;
;*PURPOSE:
;   A procedure to read housekeeping data (time, temperature, pressure,
;   high voltage, instrument in use, filter wheel flag & position,
;   missing hkb data flag) from RDF format ancillary FITS files
;
;*CALLING SEQUENCE:
;       RDFGETHKB, inputs, obinum, SCTIME, HKDAT, OBINFO=OBINFO, OPARMS=OPARMS
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
;       hkdat  - data structure containing the following binned housekeeping 
;                data:
;
;       PSPC: data structure is pspc_binhkdata
;         mhkb   - flag which tells when housekeeping data is missing
;                  0=okay, 1=last before window, 2=not complete after window,
;                  3=first after window, 4=last reading (time=end+1) after 
;                  window)
;                  = MIS_HKB (US), MISSING_H (MPE), MISS (RDF)
;         temp   - temp of PSPC
;                  = ITE_HKB (US), TEMPERAT_H (MPE), TEMP (RDF)
;         press  - pressure of PSPC
;                  = IPR_HKB (US), PRESSURE_H (MPE), PRESS (RDF)
;         hv     - high voltage of PSPC (there is a rumor "floating around" 
;                  that this number may not actually mean anything)
;                  = IHV_HKB (US), HIGHVOLT_H (MPE), HVOLT (RDF)
;         fwflag - flag that tells what was commanded for the filter wheel
;                  (0=cup, 1=calib, 2=open, 3=closed, 4=filter, 5=other)
;                  = IFR_HKB (US), FILTYPE (MPE)
;         fwpos  - filter wheel position (in degrees)
;                  = IFS_HKB (US), FILT_POS_H (MPE), FILPOS (RDF)
;         inuse  - flag which tells which instrument was in use
;                  = IIN_HKB (US), INST_IN (RDF)
;       has the structure of replicate(row,nhkb), where
;       row = {pspc_binhkdata,mhkb:0,temp:0,press:0,hv:0,fwflag:0,fwpos:0,$
;              inuse:0}
;       and nhkb is the total number of measurements
;
;       HRI: data structure is hri_binhkdata
;         temp1  - temperature 1  = TEMP1 (US, RDF)
;         temp2  - temperature 2  = TEMP2 (US, RDF)
;         temp3  - temperature 3  = TEMP3 (US, RDF)
;         deltat - width of time bin (sec)
;       has the structure of replicate(row,nhkb), where
;       row = {hri_binhkdata,temp1:0.0,temp2:0.0,temp3:0.0,deltat:0.D0}
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
;        IDL>rdfgethkb,1,obinum,sctime,hkdat, ...
;             ?obseq=rp123456
;	      ?dir=mydirectory
;	      ?trim=1
;	      ?chatter=0
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydirectory,trim=1,chatter=0'
;        IDL>rdfgethkb,list,obinum,sctime,hkdat, ...
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,10)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydirectory'
;        IDL>list(8)='1'
;        IDL>list(9)='0'
;        IDL>rdfgethkb,list,obinum,sctime,hkdat, ...
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;	     chatter=0
;            *exit
;        IDL>rdfgethkb,'myinput.dat',obinum,sctime,hkdat, ...
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
;    written  22 Dec 1993 by GAR (adapted from RSGETHKB & OLDGETHKB)
;    modified 23 Dec 1993 (GAR) to read data from HRI RDF ancillary files;
;      also name of data structure changed to pspc_binhkdata for PSPC;
;      new data structure hri_binhkdata defined for HRI
;-
;-------------------------------------------------------------------------------
pro rdfgethkb,inputs,obinum,sctime,hkdat,obinfo=obinfo,oparms=oparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RDFGETHKB, inputs, obinum (0 for all), SCTIME, HKDAT,'
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
;
;extyp = strtrim(oparms(2),2)          ;don't care what extyp is for now
ext = '_anc.fits'
;
instr = strtrim(oparms(3),2)
instr = strupcase(instr)
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
   print,' Reading binned housekeeping data from RDF ancillary data file ',name
tab=readfits(name,hdr,ext=3,/sil)         ;binned hk data in 3rd extension
sctime=fits_get(hdr,tab,'TIME')
nhkb = n_elements(sctime)
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
  nhkb = n_elements(indsel)
  sctime = sctime(indsel)
endif
if (!debug gt 2) then stop,' Stopping after indsel, nhkb defined'
;
; now read other parameters, if desired
;
if (npar ge 4) then begin
  case instr of
    'P': begin
         ftype = 'MISS'
         mhkb = fits_get(hdr,tab,ftype)       ;missing housekeeping data flag
         if (obisel) then mhkb = mhkb(indsel)
;
         ftype = 'TEMP'
         temp = fits_get(hdr,tab,ftype)       ;temperature of PSPC
         if (obisel) then temp = temp(indsel)
;
         ftype = 'PRESS'
         press = fits_get(hdr,tab,ftype)      ;pressure of PSPC
         if (obisel) then press = press(indsel)
;
         ftype = 'HVOLT'
         hv = fits_get(hdr,tab,ftype)         ;high voltage of PSPC
         if (obisel) then hv = hv(indsel)
;
         ftype = 'FILTYPE'
         fwflag = fits_get(hdr,tab,ftype)     ;filter wheel flag
         if (obisel) then fwflag = fwflag(indsel)
;
         ftype = 'FILPOS'
         fwpos = fits_get(hdr,tab,ftype)      ;filter wheel position (degrees)
         if (obisel) then fwpos = fwpos(indsel)
;
         ftype = 'INST_IN'
         inuse = fits_get(hdr,tab,ftype)      ;instrument in use'
         if (obisel) then inuse = inuse(indsel)
;
         row = {pspc_binhkdata,mhkb:0,temp:0,press:0,hv:0,fwflag:0, $
                fwpos:0,inuse:0}
         hkdat = replicate(row,nhkb)
         hkdat.mhkb = mhkb
         hkdat.temp = temp
         hkdat.press = press
         hkdat.hv = hv
         hkdat.fwflag = fwflag
         hkdat.fwpos = fwpos
         hkdat.inuse = inuse
;
         mhkb = 0 & temp = 0 & press = 0 & hv = 0
         fwflag = 0 & fwpos = 0 & inuse = 0
         end
;
    'H': begin
         ftype = 'TEMP1'
         temp1 = fits_get(hdr,tab,ftype)       ;temperature 1
         if (obisel) then temp1 = temp1(indsel)
;
         ftype = 'TEMP2'
         temp2 = fits_get(hdr,tab,ftype)       ;temperature 2
         if (obisel) then temp2 = temp2(indsel)
;
         ftype = 'TEMP3'
         temp3 = fits_get(hdr,tab,ftype)       ;temperature 3
         if (obisel) then temp3 = temp3(indsel)
;
         ftype = 'DELTA_T'
         deltat = fits_get(hdr,tab,ftype)      ;width of time bin
         if (obisel) then deltat = deltat(indsel)
;
         row = {hri_binhkdata,temp1:0.0,temp2:0.0,temp3:0.0,deltat:0.D0}
         hkdat = replicate(row,nhkb)
         hkdat.temp1 = temp1
         hkdat.temp2 = temp2
         hkdat.temp3 = temp3
         hkdat.deltat = deltat
;
         temp1 = 0 & temp2 = 0 & temp3 = 0 & deltat = 0
         end
  endcase
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
end         ;pro rdfgethkb
