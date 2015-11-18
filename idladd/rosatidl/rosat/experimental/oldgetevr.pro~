;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   oldgetevr
;
;*PURPOSE:
;   A procedure to read event rates and other related data from Rosat
;   US or MPE format event rates FITS files (do NOT convert to ST SDAS format!)
;
;*CALLING SEQUENCE:
;       OLDGETEVR, inputs, obinum, SCTIME, RATES, COUNTS, RATES2, COUNTS2, 
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
;        EXTYP       Extension of input file (e.g., CAS, ASP)
;                    (default is EVR)
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
;       obinum - vector containing numbers of OBIs to be read (0 for all)
;
; OUTPUTS:
;       sctime  - spacecraft times for which event rates were determined
;       rates   - data structure containing the following rates:
;         mv    - Master Veto rate (sum of all vetoed events) 
;                 = IAC_EVR (US), EE_MV (MPE), MV_ACO (RDF)
;         trans - Transmitted x-ray Event rate (rate of all events 
;                 transmitted to ground)
;                 = IQE_EVR (US), EE_AEXE (MPE), XTRANSM (RDF)
;         a1l   - Rate of events in anode A1 above low level threshold 
;                 (rate of all events exceeding the main anode LLD, including 
;                 vetoed particle events)
;                 = IA1_EVR (US), EE_A1LL (MPE), A1_AL (RDF)
;         acc   - Accepted Event rate (rate of events not vetoed)
;                 = IAX_EVR (US), EE_AXE (MPE), XACC (RDF)
;       has the structure of replicate(row,nevr), where
;       row={evrates,mv:0.0,trans:0.0,a1l:0.0,acc:0.0}
;       and nevr is the total number of measurements
;
;       counts  - data structure containing the following counters:
;         mv    - counter used to derive Master Veto rate
;                 = NAC_EVR (US), MV_CNT (RDF)
;         trans - counter used to derive Transmitted x-ray Event rate
;                 = NQE_EVR (US), XTR_CNT (RDF)
;         a1l   - counter used to derive A1L rate
;                 = NA1_EVR (US), A1AL_CNT (RDF)
;         acc   - counter used to derive Accepted Event rate
;                 = NAX_EVR (US), XACC_CNT (RDF)
;       has the structure of replicate(row,nevr), where
;       row={evcounts,mv:0.0,trans:0.0,a1l:0.0,acc:0.0}
;       and nevr is the total number of measurements
;
;       rates2   - data structure containing the following rates:
;         a2l    - Rate of events in veto anode A2 above low level threshold 
;                  = IA2_EVR (US), EE_A2LL (MPE), A2_AL (RDF)
;         a1lmv  - a1rate in anticoincidence with the master veto rate
;                  = IXE_EVR (US), EE_XE (MPE), A1AL_MV (RDF)
;         sa     - SA-rate
;                  = ISR_EVR (US), SA_RATE (MPE), SA_RATE (RDF)
;         sda    - South Atlantic Anomaly Detector A rate
;                  = ISA_EVR (US), EE_SAAD_A (MPE), SAADA_RT (RDF)
;         sdb    - South Atlantic Anomaly Detector B rate
;                  = ISB_EVR (US), EE_SAAD_B (MPE), SAADB_RT (RDF)
;         a1h    - Rate of events in anode A1 above high level threshold 
;                  = IH1_EVR (US), EE_A1HL (MPE), A1_AH (RDF)
;         a2h    - Rate of events in veto anode A2 above high level threshold 
;                  = IH2_EVR (US), EE_A2HL (MPE), A2_AH (RDF)
;         k1     - Rate of events in edge strips of front cathode K1
;                  = IK1_EVR (US), EE_K1R (MPE), K1_EDGE (RDF)
;         k2     - Rate of events in edge strips of back cathode K2
;                  = IK2_EVR (US), EE_K2R (MPE), K2_EDGE (RDF)
;         k1pass - Rate of events in K1 passing event selection logic
;                  = IV1_EVR (US), EE_VAL1 (MPE), K1_PASS (RDF)
;         k2pass - Rate of events in K2 passing event selection logic
;                  = IV2_EVR (US), EE_VAL2 (MPE), K2_PASS (RDF)
;       has the structure of replicate(row,nevr), where
;       row={evrates2,a2l:0.0,a1lmv:0.0,sa:0.0,sda:0.0,sdb:0.0,a1h:0.0,
;            a2h:0.0,k1:0.0,k2:0.0,k1pass:0.0,k2pass:0.0}
;       and nevr is the total number of measurements
;
;       counts2  - data structure containing the following counters:
;         a2l    - counter used to derive A2L rate
;                  = NA2_EVR (US), A2AL_CNT (RDF)
;         a1lmv  - counter used to derive A1L rate in anticoincidence with MV
;                  = NXE_EVR (US), A1ALMVCT (RDF)
;         ping   - ping counter
;                  = NK3_EVR (US), PING_CNT (RDF)
;         sda    - counter used to derive South Atlantic Anomaly Detector A rate
;                  = NSA_EVR (US), SAADA_CT (RDF)
;         sdb    - counter used to derive South Atlantic Anomaly Detector B rate
;                  = NSB_EVR (US), SAADB_CT (RDF)
;         a1h    - counter used to derive A1H rate 
;                  = NH1_EVR (US), A1AH_CNT (RDF)
;         a2h    - counter used to derive A2H rate 
;                  = NH2_EVR (US), A2AH_CNT (RDF)
;         k1     - counter used to derive K1 rate
;                  = NH1_EVR (US), A1AH_CNT (RDF)
;         k2     - counter used to derive K2 rate
;                  = NH2_EVR (US), A2AH_CNT (RDF)
;         k1pass - counter used to derive K1 rate passing event selection logic
;                  = NK1_EVR (US), K1ED_CNT (RDF)
;         k2pass - counter used to derive K2 rate passing event selection logic
;                  = NK2_EVR (US), K2ED_CNT (RDF)
;       has the structure of replicate(row,nevr), where
;       row={evcounts2,a2l:0.0,a1lmv:0.0,ping:0.0,sda:0.0,sdb:0.0,a1h:0.0,
;            a2h:0.0,k1:0.0,k2:0.0,k1pass:0.0,k2pass:0.0}
;       and nevr is the total number of measurements
;
;       obinfo - data structure containing dates, times, & indices of
;                beginnings & ends of obi segments
;                has the structure of replicate(row,num), where
;                row={obitime,sctbeg:0.0D0,yrbeg:0,daybeg:0,utsbeg:0.0D0,
;                     sctend:0.0D0,yrend:0,dayend:0,utsend:0.0D0,
;                     ibeg:0L,iend:0L}
;                and num is the total number of intervals
;         ibeg - indices of values in SCTIME equal to values in SCTBEG
;         iend - indices of values in SCTIME equal to values in SCTEND
;
;       oparms - string array which is the parsed form of inputs (same as
;                LIST in example 3 below). Allows program to be rerun using 
;                same values w/o resetting.
;
;*EXAMPLES: There are 4 different ways that the inputs may be specified
;    mode 1) Interactive
;        IDL>oldgetevr,1,obinum,sctime,rates, ...
;             ?obseq=rp123456
;	      ?dir=mydirectory
;	      ?chatter=0
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydirectory,chatter=0'
;        IDL>oldgetevr,list,obinum,sctime,rates, ...
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,10)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydirectory'
;        IDL>list(9)='0'
;        IDL>oldgetevr,list,obinum,sctime,rates, ...
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;	     chatter=0
;            *exit
;        IDL>oldgetevr,'myinput.dat',obinum,sctime,rates, ...
;
;*RESTRICTIONS:
;
;*NOTES:
;  Input parameters INSTR and TRIM are ignored (and need not be specified).
;  Reads FITS files directly. Do NOT convert to ST SDAS format.
;
;*SUBROUTINES CALLED:
;  RSGETPAR
;  MATCH_FILES
;  FITS_INFO
;  RS_SEQINFO
;  MPE_SEQINFO
;  RSOBITIMES
;  RS_NOMASP
;  US_SEQINFO
;  READFITS
;  TBGET
;  RSOBITIMES
;
;*MODIFICATION HISTORY:
;    written  08 May 1991 by GAR
;    modified to use match_files 17 May 1991 by GAR
;    modified 29 Aug 1991 by GAR to return IBEG
;    modified 30 Aug 1991 by GAR to allow selection of OBIs
;    modified 25 Sep 1991 by GAR to make sctbeg into a keyword, remove sctend
;    modifed to use GHRS derived parameter setting routines 05 Oct 1991 GAR
;    modified to combine sctbeg & obibeg, sctend & obiend, and to make into
;        keywords  09 Oct 1991  GAR
;    modified 8 Nov 1991 for compatibility with Sun Unix (GAR)
;    modifed 18 Feb 1992 to return OBI info as a data structure (GAR)
;    modified 29 Feb 1992 to combine output rates & counters into 2 data
;        structures, RATES & COUNTS  (GAR)
;    modified 04 Jun 1992 to use READFITS, FITS_INFO, and new version
;        of MATCH_FILES, and to return OPARMS (GAR)
;    modified 08 Jul 1992 to fix bug (GAR)
;    modified 26 Aug 1992 (GAR) to read MPE format files (input variable PROC 
;      added; most calls to TBGET changed to FITS_GET, variable FTYPE used)
;    modified 10 Aug 1993 (GAR) to use extension for archival MPE format 
;      data
;    modified 13 Aug 1993 (GAR) to replace call to RSGETSEQINFO with call
;      to RS_SEQINFO, and to simplify logic of defining sctbeg, etc.
;    modified 02 Dec 1993 (GAR) to return more rates and counts, to
;      simplify tag names within the output structures, and to fix bug 
;      introduced with use of RS_SEQINFO
;    modified 20 Dec 1993 (GAR) to return yet more rates and counts and to
;      separate the output variables into rates & rates2, counts & counts2,
;      call statement changed to match; name also changed from RSGETEVR to 
;      OLDGETEVR (to allow RSGETASP to read RDF format files as well as 
;      US & MPE format)
;-
;-------------------------------------------------------------------------------
pro oldgetevr,inputs,obinum,sctime,rates,counts,rates2,counts2,obinfo=obinfo,$
              oparms=oparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' OLDGETEVR, inputs, obinum (def = 0 for all), SCTIME, RATES, COUNTS,'
  print,'            RATES2, COUNTS2, OBINFO=OBINFO, OPARMS=OPARMS'
  print,'   Uses inputs OBSEQ, DIR, EXTYP, INSTR, PROC, and CHATTER' $
       +' from RSGET.DEF'
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
if proc eq 'MPEUS' then proc='MPE'
proc = strupcase(proc)
case proc of
  'US':  begin
         if (extyp eq '') then extyp = 'evr'
         ext = '.'+extyp
         extuc = strupcase(extyp)
         if (extuc ne 'EVR') then begin
           print,extyp,' is not a valid extension for a US format event', $
                       ' rates data file.'
           print,' Valid option is EVR. Returning.'
           retall 
         endif
         end
  'MPE': ext = '_eventrates.tfits'
  else : begin
         print,' This routine only reads event rates data from US or MPE', $
               ' format files.'
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
if (npar lt 2) then obinum = 0                   ;default is to read all
;if (n_elements(obinum) eq 1) then obinum = obinum + intarr(1)
;
match_files,dir,obseq,ext,name,nlist      ;look for the right files
name = name(0)
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
; right now, the individual .EVR headers do not contain the start & stop
; times of the OBIs. These must be read from the sequence header
; The MPE processed file headers do not contain any information for the
; individual OBIs at all
;
rs_seqinfo,name,nobi2,actim,nomasp,seqbeg,seqend,obibeg,obiend,proc=proc
if (nobi2 gt 1) then begin
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
;  nobi2 is the total number of obis for the observation, while nobi is
;  the number of obis requested by the user
;  If unequal, then select the corresponding values of sctbeg, etc.
;
if (nobi ne nobi2) then begin
  sctbeg = sctbeg(obinum - 1)
  sctend = sctend(obinum - 1)
  yrbeg = yrbeg(obinum - 1)
  yrend = yrend(obinum - 1)
  daybeg = daybeg(obinum - 1)
  dayend = dayend (obinum - 1)
  utsbeg = utsbeg(obinum - 1)
  utsend = utsend(obinum - 1)
endif  
;
sctime = dblarr(1) - 999.
mvrate = [-999] & terate = mvrate & a1lrate = mvrate & aerate = mvrate
mvcount = [-999] & tecount = mvcount & a1lcount = mvcount & aecount = mvcount
a2lrate = mvrate & xerate = a2lrate 
sarate = [-999] & sdarate = sarate & sdbrate = sarate
a1hrate = [-999] & a2hrate = a1hrate
k1rate = [-999] & k2rate = k1rate & v1rate = k1rate & v2rate = k2rate
a2lcount = [-999] & xecount = a2lcount & ping = a2lcount
sdacount = [-999] & sdbcount = sdacount
a1hcount = [-999] & a2hcount = a1hcount
k1count = [-999] & k2count = k1count & v1count = k1count & v2count = k1count
ibeg = intarr(nobi)+long(0.) 
;
if (chatter eq 1) then $
   print,' Reading event rates data file ',name
ict = 0
for jj=0,nobi-1 do begin
  jobi = obinum(jj)
  if (chatter eq 1) then print,'    OBI ',jobi
  tab = readfits(name,hdr,ext=jobi,/sil)
;
  ftype = 'ITI_EVR'
  if (proc eq 'MPE') then ftype = 'TIME'
  entry = fits_get(hdr,tab,ftype)            ;Time of event rate (s/c, sec)
;
  nument = n_elements(entry)                  ;number in this section
  if (!debug gt 3) then print,nument,minmax(entry),entry(0),entry(nument-1)
  ind = where((entry ge (sctbeg(jj)-100.)) and (entry le (sctend(jj)+100.)))
  entry = entry(ind)
  nument = n_elements(entry)                  ;number in this section
  if (!debug gt 3) then print,nument,minmax(entry)
;
  sctime = [sctime,entry]
  begt = entry(0)                            ;beginning of OBI in sc sec
  endt = entry(nument-1)                     ;end of OBI in sc sec
  ibeg(jj) = ict                             ;beginning indices of OBIs
  ict = ict + nument
;
; calculate OBI spacecraft and UT start times
; correct UT start time if spacecraft start time read from header
; is not same as first spacecraft time in time array
;
  if (sctbeg(jj) ne begt) then utsbeg(jj) = utsbeg(jj) + (begt - sctbeg(jj))
  sctbeg(jj) = begt
;
; calculate OBI spacecraft and UT stop times
; correct spacecraft stop time if UT stop time read from header
; is not same as last UT time in time array
;
  if (sctend(jj) ne endt) then utsend(jj) = utsend(jj) + (endt - sctend(jj))
  sctend(jj) = endt
  if (!debug gt 3) then stop,' Stopping after correcting obi times'
;
; now read other parameters, if desired
;
  if (npar ge 4) then begin
    ftype = 'IAC_EVR'
    if (proc eq 'MPE') then ftype = 'EE_MV'
    entry = fits_get(hdr,tab,ftype)        ;Master Veto rate (cts/sec)
    entry = entry(ind)
    mvrate = [mvrate,entry]
;
    ftype = 'IQE_EVR'
    if (proc eq 'MPE') then ftype = 'EE_AEXE'     ;I think this is right
    entry = fits_get(hdr,tab,ftype)        ;Transmitted event rate (cts/sec)
    entry = entry(ind)
    terate = [terate,entry]
;
    ftype = 'IA1_EVR'
    if (proc eq 'MPE') then ftype = 'EE_A1LL'
    entry = fits_get(hdr,tab,ftype)  ;A1 rate above lower level thresh (cts/sec)
    entry = entry(ind)
    a1lrate = [a1lrate,entry]
;
    ftype = 'IAX_EVR'
    if (proc eq 'MPE') then ftype = 'EE_AXE'      ;I think this is right
    entry = fits_get(hdr,tab,ftype)        ;Accepted event rate (cts/sec)
    entry = entry(ind)
    aerate = [aerate,entry]
  endif
;
  if ( (npar ge 5) and (proc ne 'MPE') ) then begin
    entry = tbget(hdr,tab,'NAC_EVR')       ;Master Veto counter (cts/sec)
    entry = entry(ind)
    mvcount = [mvcount,entry]
;
    entry = tbget(hdr,tab,'NQE_EVR')       ;Transmitted event counter (cts/sec)
    entry = entry(ind)
    tecount = [tecount,entry]
;
    entry = tbget(hdr,tab,'NA1_EVR')       ;A1 counter (cts/sec)
    entry = entry(ind)
    a1lcount = [a1lcount,entry]
;
    entry = tbget(hdr,tab,'NAX_EVR')       ;Accepted event counter (cts/sec)
    entry = entry(ind)
    aecount = [aecount,entry]
  endif
;
  if (npar ge 6) then begin
    ftype = 'IA2_EVR'
    if (proc eq 'MPE') then ftype = 'EE_A2LL'
    entry = fits_get(hdr,tab,ftype)   ;in A2 above low level thresh (cts/sec)
    entry = entry(ind)
    a2lrate = [a2lrate,entry]
;
    ftype = 'IXE_EVR'
    if (proc eq 'MPE') then ftype = 'EE_XE'    
    entry = fits_get(hdr,tab,ftype)        ;A1LL in anticoincidence with MV
    entry = entry(ind)
    xerate = [xerate,entry]
;
    ftype = 'ISR_EVR'
    if (proc eq 'MPE') then ftype = 'SA_RATE'     
    entry = fits_get(hdr,tab,ftype)        ;SA-rate
    entry = entry(ind)
    sarate = [sarate,entry]
;
    ftype = 'ISA_EVR'
    if (proc eq 'MPE') then ftype = 'EE_SAAD_A'   
    entry = fits_get(hdr,tab,ftype)        ;SAAD-A count rate
    entry = entry(ind)
    sdarate = [sdarate,entry]
;
    ftype = 'ISB_EVR'
    if (proc eq 'MPE') then ftype = 'EE_SAAD_B' 
    entry = fits_get(hdr,tab,ftype)        ;SAAD-B count rate
    entry = entry(ind)
    sdbrate = [sdbrate,entry]
;
    ftype = 'IH1_EVR'
    if (proc eq 'MPE') then ftype = 'EE_A1HL'
    entry = fits_get(hdr,tab,ftype)   ;in A1 above high level thresh (cts/sec)
    entry = entry(ind)
    a1hrate = [a1hrate,entry]
;
    ftype = 'IH2_EVR'
    if (proc eq 'MPE') then ftype = 'EE_A2HL'
    entry = fits_get(hdr,tab,ftype)   ;in A2 above high level thresh (cts/sec)
    entry = entry(ind)
    a2hrate = [a2hrate,entry]
;
    ftype = 'IK1_EVR'
    if (proc eq 'MPE') then ftype = 'EE_K1R'      
    entry = fits_get(hdr,tab,ftype)        ;Rate of events in K1 edge strip
    entry = entry(ind)
    k1rate = [k1rate,entry]
;
    ftype = 'IK2_EVR'
    if (proc eq 'MPE') then ftype = 'EE_K2R'      
    entry = fits_get(hdr,tab,ftype)        ;Rate of events in K2 edge strip
    entry = entry(ind)
    k2rate = [k2rate,entry]
;
    ftype = 'IV1_EVR'
    if (proc eq 'MPE') then ftype = 'EE_VAL1'      
    entry = fits_get(hdr,tab,ftype)  ;K1 events passing event selection logic
    entry = entry(ind)
    v1rate = [v1rate,entry]
;
    ftype = 'IV2_EVR'
    if (proc eq 'MPE') then ftype = 'EE_VAL2'      
    entry = fits_get(hdr,tab,ftype)  ;K2 events passing event selection logic
    entry = entry(ind)
    v2rate = [v2rate,entry]
  endif
;
  if ( (npar ge 7) and (proc ne 'MPE') ) then begin
    entry = tbget(hdr,tab,'NA2_EVR')       ;A2 counter above low level thresh
    entry = entry(ind)                     ;(cts/sec)
    a2lcount = [a2lcount,entry]
;
    entry = tbget(hdr,tab,'NXE_EVR')  ;IA1_EVR in anticoincidence with IAC_EVR
    entry = entry(ind)
    xecount = [xecount,entry]
;
    entry = tbget(hdr,tab,'NK3_EVR')       ;Ping counter
    entry = entry(ind)
    ping = [ping,entry]
;
    entry = tbget(hdr,tab,'NSA_EVR')       ;SAAD-A counter (cts/sec)
    entry = entry(ind)
    sdacount = [sdacount,entry]
;
    entry = tbget(hdr,tab,'NSB_EVR')       ;SAAD-B counter (cts/sec)
    entry = entry(ind)
    sdbcount = [sdbcount,entry]
;
    entry = tbget(hdr,tab,'NH1_EVR')       ;A1 counter above high level thresh
    entry = entry(ind)                     ;(cts/sec)
    a1hcount = [a1hcount,entry]
;
    entry = tbget(hdr,tab,'NH2_EVR')       ;A2 counter above high level thresh
    entry = entry(ind)                     ;(cts/sec)
    a2hcount = [a2hcount,entry]
;
    entry = tbget(hdr,tab,'NK1_EVR')       ;K1 edge event counter
    entry = entry(ind)
    k1count = [k1count,entry]
;
    entry = tbget(hdr,tab,'NK2_EVR')       ;K2 edge event counter
    entry = entry(ind)
    k2count = [k2count,entry]
;
    entry = tbget(hdr,tab,'NV1_EVR')   ;K1 edge counts passing selection logic
    entry = entry(ind)
    v1count = [v1count,entry]
;
    entry = tbget(hdr,tab,'NV2_EVR')   ;K2 edge counts passing selection logic
    entry = entry(ind)
    v2count = [v2count,entry]
  endif
endfor
;
sctime = sctime(1:*)
nevr = n_elements(sctime)
if (npar ge 4) then begin                  ;strip off initial bad values
  mvrate = mvrate(1:*)
  terate = terate(1:*)
  a1lrate = a1lrate(1:*)
  aerate = aerate(1:*)
;
  row = {evrates,mv:0.0,trans:0.0,a1l:0.0,acc:0.0}
  rates = replicate(row,nevr)
  rates.mv = mvrate
  rates.trans = terate
  rates.a1l = a1lrate
  rates.acc = aerate
;
  mvrate = 0                               ;free up some memory
  terate = 0
  a1lrate = 0
  aerate = 0
endif
;
if ( (npar ge 5) and (proc ne 'MPE') ) then begin
  mvcount = mvcount(1:*)                   ;strip off initial bad values
  tecount = tecount(1:*)
  a1lcount = a1lcount(1:*)                 
  aecount = aecount(1:*)
;
  row = {evcounts,mv:0.0,trans:0.0,a1l:0.0,acc:0.0}
  counts = replicate(row,nevr)
  counts.mv = mvcount
  counts.trans = tecount
  counts.a1l = a1lcount
  counts.acc = aecount
;
  mvcount = 0                              ;free up some memory
  tecount = 0
  a1lcount = 0
  aecount = 0
endif
;
if (npar ge 6) then begin                  ;strip off initial bad values
  a2lrate = a2lrate(1:*)
  xerate = xerate(1:*)
  sarate = sarate(1:*)
  sdarate = sdarate(1:*)
  sdbrate = sdbrate(1:*)
  a1hrate = a1hrate(1:*)                     
  a2hrate = a2hrate(1:*)                     
  k1rate = k1rate(1:*)
  k2rate = k2rate(1:*)
  v1rate = v1rate(1:*)
  v2rate = v2rate(1:*)
;
  row = {evrates2,a2l:0.0,a1lmv:0.0,sa:0.0,sda:0.0,sdb:0.0,a1h:0.0, $
         a2h:0.0,k1:0.0,k2:0.0,k1pass:0.0,k2pass:0.0}
  rates2 = replicate(row,nevr)
  rates2.a2l = a2lrate
  rates2.a1lmv = xerate
  rates2.sa = sarate
  rates2.sda = sdarate
  rates2.sdb = sdbrate
  rates2.a1h = a1hrate
  rates2.a2h = a2hrate
  rates2.k1 = k1rate
  rates2.k2 = k2rate
  rates2.k1pass = v1rate
  rates2.k2pass = v2rate
;
  a2lrate = 0                      ;free up some memory
  xerate = 0
  sarate = 0
  sdarate = 0
  sdbrate = 0
  a1hrate = 0
  a2hrate = 0
  k1rate = 0
  k2rate = 0
  v1rate = 0
  v2rate = 0
endif
;
if ( (npar ge 7) and (proc ne 'MPE') ) then begin
  a2lcount = a2lcount(1:*)                 ;strip off initial bad values
  xecount = xecount(1:*)
  ping = ping(1:*)
  sdacount = sdacount(1:*)
  sdbcount = sdbcount(1:*)
  a1hcount = a1hcount(1:*)                     
  a2hcount = a2hcount(1:*)                     
  k1count = k1count(1:*)
  k2count = k2count(1:*)
  v1count = v1count(1:*)
  v2count = v2count(1:*)
;
  row = {evcounts2,a2l:0.0,a1lmv:0.0,ping:0.0,sda:0.0,sdb:0.0,a1h:0.0, $
         a2h:0.0,k1:0.0,k2:0.0,k1pass:0.0,k2pass:0.0}
  counts2 = replicate(row,nevr)
  counts2.a2l = a2lcount
  counts2.a1lmv = xecount
  counts2.ping = ping
  counts2.sda = sdacount
  counts2.sdb = sdbcount
  counts2.a1h = a1hcount
  counts2.a2h = a2hcount
  counts2.k1 = k1count
  counts2.k2 = k2count
  counts2.k1pass = v1count
  counts2.k2pass = v2count
;
  a2lcount = 0                             ;free up some memory
  xecount = 0
  ping = 0
  sdacount = 0
  sdbcount = 0
  a1hcount = 0
  a2hcount = 0
  k1count = 0
  k2count = 0
  v1count = 0
  v2count = 0
endif
;
if (proc eq 'MPE') then begin
  if (npar ge 5) then counts = 0
  if (npar ge 7) then counts2 = 0
endif
;
if (nobi gt 1) then iend = [ibeg(1:*)-1,n_elements(sctime)-1] else $
                    iend = [n_elements(sctime)-1]
row={obitime,sctbeg:0.0D0,yrbeg:0,daybeg:0,utsbeg:0.0D0,$
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
end         ;pro oldgetevr
