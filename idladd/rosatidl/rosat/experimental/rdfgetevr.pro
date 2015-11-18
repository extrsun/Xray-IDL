;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       rdfgetevr
;
;*PURPOSE:
;   A procedure to read event rates and other related data 
;   from Rosat RDF format ancillary FITS files 
;
;*CALLING SEQUENCE:
;       RDFGETEVR, inputs, obinum, SCTIME, RATES, COUNTS, RATES2, COUNTS2, 
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
;       rates   - data structure containing the following event rates:
;
;       PSPC: data structure is pspc_evrates
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
;       row={pspc_evrates,mv:0.0,trans:0.0,a1l:0.0,acc:0.0}
;       and nevr is the total number of measurements
;
;       HRI: data structure is hri_evrates
;         second  - Secondary science rate, = SECONDARY (RDF)
;         prim    - Primary science rate, = PRIMARY (RDF)
;         acc     - Accepted X-ray rate, = ACCEPTED (RDF)
;         invalid - Invalid event rate, = INVALID (RDF)
;         ltcor   - Live-time correction factor, = LIVT_COR (RDF)
;       has the structure of replicate(row,nevr), where
;       row={hri_evrates,second:0.0,prim:0.0,acc:0.0,invalid:0.0,ltcor:0.0}
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
;       Note: COUNTS is set to 0 for the HRI
;
;       rates2   - data structure containing the following event rates:
;
;       PSPC: data structure is pspc_evrates2
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
;       row={pspc_evrates2,a2l:0.0,a1lmv:0.0,sa:0.0,sda:0.0,sdb:0.0,a1h:0.0,
;            a2h:0.0,k1:0.0,k2:0.0,k1pass:0.0,k2pass:0.0}
;       and nevr is the total number of measurements
;
;       HRI: data structure is hri_evrates2
;         bkg    - Background level, = BACKLEV (RDF)
;         vg     - Viewing geometry, = VIEWGEOM (RDF)
;         asperr - Aspect error, = ASPERR (RDF)
;         sda    - South Atlantic Anomaly Detector A rate, = SAADA_RT (RDF)
;         sdb    - South Atlantic Anomaly Detector B rate, = SAADB_RT (RDF)
;       has the structure of replicate(row,nevr), where
;       row={hri_evrates2,bkg:0.0,vg:0.0,asperr:0.0,sda:0.0,sdb:0.0}
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
;       Note: COUNTS2 is set to 0 for the HRI
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
;        IDL>rdfgetevr,1,obinum,sctime,rates, ...
;             ?obseq=rp123456
;	      ?dir=mydirectory
;	      ?chatter=0
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydirectory,chatter=0'
;        IDL>rdfgetevr,list,obinum,sctime,rates, ...
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,10)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydirectory'
;        IDL>list(9)='0'
;        IDL>rdfgetevr,list,obinum,sctime,rates, ...
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;	     chatter=0
;            *exit
;        IDL>rdfgetevr,'myinput.dat',obinum,sctime,rates, ...
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
;  RSOBITIMES
;  RS_NOMASP
;  READFITS
;  TBGET
;  TABINV
;  NINT
;
;*MODIFICATION HISTORY:
;    written  20 Dec 1993 by GAR (adapted from RSGETEVR & OLDGETEVR)
;    modified 23 Dec 1993 (GAR) to read data from HRI RDF ancillary files;
;      names of data structures evrates & evrates changed to pspc_evrates
;      & pspc_evrates2; new data structures hri_evrates & hri_evrates2
;      defined for HRI
;-
;-------------------------------------------------------------------------------
pro rdfgetevr,inputs,obinum,sctime,rates,counts,rates2,counts2,obinfo=obinfo,$
              oparms=oparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RDFGETEVR, inputs, obinum (def = 0 for all), SCTIME, RATES, COUNTS,'
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
;
;extyp = strtrim(oparms(2),2)          ;don't care what extyp is for now
ext = '_anc.fits'
;
instr = strtrim(oparms(3),2)
instr = strupcase(instr)
proc = strtrim(oparms(4),2)            ;processing format - RDF only
proc = strupcase(proc)
if (proc ne 'RDF') then begin
  print,' This routine only reads event rates data from RDF format', $
        ' ancillary files.'
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
if (npar lt 2) then obinum = 0                   ;default is to read all
;if (n_elements(obinum) eq 1) then obinum = obinum + intarr(1)
;
match_files,dir,obseq,ext,name,nlist      ;look for the right files
name = name(0)
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
   print,' Reading event rates data from RDF ancillary data file ',name
tab=readfits(name,hdr,ext=4,/sil)         ;event rates data in 4th extension
sctime=fits_get(hdr,tab,'TIME')
nevr = n_elements(sctime)
;
; Get the number of unique OBIs, and the indices where these change
;
obival = fits_get(hdr,tab,'OBI_NUM')
nsize = 10000.
if (!version.os eq 'vms') then nsize = 2*nsize  
if (nevr le nsize) then vsort,obival,1,trueval,indsrt,ibeg,iend,ntrue,mid=1 $
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
  nevr = n_elements(indsel)
  sctime = sctime(indsel)
endif
if (!debug gt 2) then stop,' Stopping after indsel, nevr defined'
;
; now read other parameters, if desired
;
if (npar ge 4) then begin
  case instr of
    'P': begin
         ftype = 'MV_ACO'
         mvrate = fits_get(hdr,tab,ftype)     ;Master Veto rate (cts/sec)
         if (obisel) then mvrate = mvrate(indsel)
;
         ftype = 'XTRANSM'
         terate = fits_get(hdr,tab,ftype)     ;Transmitted event rate (cts/sec)
         if (obisel) then terate = terate(indsel)
;
         ftype = 'A1_AL'
         a1lrate = fits_get(hdr,tab,ftype)    ;A1 rate above lower thresh 
         if (obisel) then a1lrate = a1lrate(indsel)    ;(cts/sec)
;
         ftype = 'XACC'
         aerate = fits_get(hdr,tab,ftype)     ;Accepted event rate (cts/sec)
         if (obisel) then aerate = aerate(indsel)
;
         row = {pspc_evrates,mv:0.0,trans:0.0,a1l:0.0,acc:0.0}
         rates = replicate(row,nevr)
         if (nevr gt 1) then begin
           rates.mv = mvrate
           rates.trans = terate
           rates.a1l = a1lrate
           rates.acc = aerate
         endif else begin
           rates.mv = mvrate(0)
           rates.trans = terate(0)
           rates.a1l = a1lrate(0)
           rates.acc = aerate(0)
         endelse
;
         mvrate = 0           ;to save memory
         terate = 0
         a1lrate = 0
         aerate = 0
         end
;
    'H': begin
         ftype = 'SECONDARY'
         srate = fits_get(hdr,tab,ftype)      ;Secondary science rate (counts?)
         if (obisel) then srate = srate(indsel)
;
         ftype = 'PRIMARY'
         prate = fits_get(hdr,tab,ftype)      ;Primary science rate (counts?)
         if (obisel) then prate = prate(indsel)
;
         ftype = 'ACCEPTED'
         aerate = fits_get(hdr,tab,ftype)     ;Accepted event rate (counts?)
         if (obisel) then aerate = aerate(indsel)
;
         ftype = 'INVALID'
         invrate = fits_get(hdr,tab,ftype)    ;Invalid event rate (counts?)
         if (obisel) then invrate = invrate(indsel)
;
         ftype = 'LIVT_COR'
         ltcor = fits_get(hdr,tab,ftype)      ;Live-time correction factor
         if (obisel) then ltcor = ltcor(indsel)
;
         row = {hri_evrates,second:0.0,prim:0.0,acc:0.0,invalid:0.0,ltcor:0.0}
         rates = replicate(row,nevr)
         if (nevr gt 1) then begin
           rates.second = srate
           rates.prim = prate
           rates.acc = aerate
           rates.invalid = invrate
           rates.ltcor = ltcor
         endif else begin
           rates.second = srate(0)
           rates.prim = prate(0)
           rates.acc = aerate(0)
           rates.invalid = invrate(0)
           rates.ltcor = ltcor(0)
         endelse
;
         prate = 0            ;to save memory
         srate = 0
         aerate = 0
         invrate = 0
         ltcor = 0
         end
;
  else:  begin
         print,' Instrument must be specified as P for PSPC or as H for HRI.'
         print,' Please check your inputs. Returning.'
         retall
         end
  endcase
endif
;
if (npar ge 5) then begin
  case instr of
    'P': begin
         ftype = 'MV_CNT'
         mvcount = tbget(hdr,tab,ftype)           ;Master Veto counter (cts/sec)
         if (obisel) then mvcount = mvcount(indsel)
;
         ftype = 'XTR_CNT'
         tecount = tbget(hdr,tab,ftype)     ;Transmitted event counter (cts/sec)
         if (obisel) then tecount = tecount(indsel)
;
         ftype = 'A1AL_CNT'
         a1lcount = tbget(hdr,tab,ftype)          ;A1 counter (cts/sec)
         if (obisel) then a1lcount = a1lcount(indsel)
;
         ftype = 'XACC_CNT'
         aecount = tbget(hdr,tab,ftype)     ;Accepted event counter (cts/sec)
         if (obisel) then aecount = aecount(indsel)
;
         row = {evcounts,mv:0.0,trans:0.0,a1l:0.0,acc:0.0}
         counts = replicate(row,nevr)
         if (nevr gt 1) then begin
           counts.mv = mvcount
           counts.trans = tecount
           counts.a1l = a1lcount
           counts.acc = aecount
         endif else begin
           counts.mv = mvcount(0)
           counts.trans = tecount(0)
           counts.a1l = a1lcount(0)
           counts.acc = aecount(0)
         endelse
;
         mvcount = 0           ;to save memory
         tecount = 0
         a1lcount = 0
         aecount = 0
         end
;
    'H': counts = 0
  endcase
endif
;
if (npar ge 6) then begin
  case instr of
    'P': begin
         ftype = 'A2_AL'
         a2lrate = fits_get(hdr,tab,ftype)   ;in A2 above lower thresh (cts/sec)
         if (obisel) then a2lrate = a2lrate(indsel)
;
         ftype = 'A1AL_MV'
         xerate = fits_get(hdr,tab,ftype)    ;A1LL in anticoincidence with MV
         if (obisel) then xerate = xerate(indsel)
;
         ftype = 'SA_RATE'
         sarate = fits_get(hdr,tab,ftype)    ;SA-rate
         if (obisel) then sarate = sarate(indsel)
;
         ftype = 'SAADA_RT'
         sdarate = fits_get(hdr,tab,ftype)   ;SAAD-A count rate
         if (obisel) then sdarate = sdarate(indsel)
;
         ftype = 'SAADB_RT'
         sdbrate = fits_get(hdr,tab,ftype)   ;SAAD-B count rate
         if (obisel) then sdbrate = sdbrate(indsel)
;
         ftype = 'A1_AH'
         a1hrate = fits_get(hdr,tab,ftype)  ;in A1 above higher thresh (cts/sec)
         if (obisel) then a1hrate = a1hrate(indsel)
;
         ftype = 'A2_AH'
         a2hrate = fits_get(hdr,tab,ftype)  ;in A2 above higher thresh (cts/sec)
         if (obisel) then a2hrate = a2hrate(indsel)
;
         ftype = 'K1_EDGE'
         k1rate = fits_get(hdr,tab,ftype)    ;Rate of events in K1 edge strip
         if (obisel) then k1rate = k1rate(indsel)
;
         ftype = 'K2_EDGE'
         k2rate = fits_get(hdr,tab,ftype)    ;Rate of events in K2 edge strip
         if (obisel) then k2rate = k2rate(indsel)
;
         ftype = 'K1_PASS'
         v1rate = fits_get(hdr,tab,ftype)    ;K1 events passing event sel logic
         if (obisel) then v1rate = v1rate(indsel)
;
         ftype = 'K2_PASS'
         v2rate = fits_get(hdr,tab,ftype)    ;K2 events passing event sel logic
         if (obisel) then v2rate = v2rate(indsel)
;
         row = {pspc_evrates2,a2l:0.0,a1lmv:0.0,sa:0.0,sda:0.0,sdb:0.0, $
                              a1h:0.0,a2h:0.0,k1:0.0,k2:0.0,k1pass:0.0, $
                              k2pass:0.0}
         rates2 = replicate(row,nevr)
         if (nevr gt 1) then begin
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
         endif else begin
           rates2.a2l = a2lrate(0)
           rates2.a1lmv = xerate(0)
           rates2.sa = sarate(0)
           rates2.sda = sdarate(0)
           rates2.sdb = sdbrate(0)
           rates2.a1h = a1hrate(0)
           rates2.a2h = a2hrate(0)
           rates2.k1 = k1rate(0)
           rates2.k2 = k2rate(0)
           rates2.k1pass = v1rate(0)
           rates2.k2pass = v2rate(0)
         endelse
;
         a2lrate = 0           ;to save memory
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
         end
;
    'H': begin
         ftype = 'BACKLEV'
         bkrate = fits_get(hdr,tab,ftype)     ;Background level (counts?)
         if (obisel) then bkrate = bkrate(indsel)
;
         ftype = 'VIEWGEOM'
         vg = fits_get(hdr,tab,ftype)         ;Viewing geometry
         if (obisel) then vg = vg(indsel)
;
         ftype = 'ASPERR'
         asperr = fits_get(hdr,tab,ftype)     ;Aspect error
         if (obisel) then asperr = asperr(indsel)
;
         ftype = 'SAADA_RT'
         sdarate = fits_get(hdr,tab,ftype)   ;SAAD-A count rate
         if (obisel) then sdarate = sdarate(indsel)
;
         ftype = 'SAADB_RT'
         sdbrate = fits_get(hdr,tab,ftype)   ;SAAD-B count rate
         if (obisel) then sdbrate = sdbrate(indsel)
;
         row = {hri_evrates2,bkg:0.0,vg:0.0,asperr:0.0,sda:0.0,sdb:0.0}
         rates2 = replicate(row,nevr)
         if (nevr gt 1) then begin
           rates2.bkg = bkrate
           rates2.vg = vg
           rates2.asperr = asperr
           rates2.sda = sdarate
           rates2.sdb = sdbrate
         endif else begin
           rates2.bkg = bkrate(0)
           rates2.vg = vg(0)
           rates2.asperr = asperr(0)
           rates2.sda = sdarate(0)
           rates2.sdb = sdbrate(0)
         endelse
;
         bkrate = 0            ;to save memory
         vg = 0
         asperr = 0
         sdarate = 0
         sdbrate = 0
         end
  endcase
endif
;
if (npar ge 7) then begin
  case instr of
    'P': begin
         ftype = 'A2AL_CNT'
         a2lcount = tbget(hdr,tab,ftype)     ;A2 counter above low level thresh
         if (obisel) then a2lcount = a2lcount(indsel)
;
         ftype = 'A1ALMVCT'
         xecount = tbget(hdr,tab,ftype)      ;IA1_EVR in anticoinc with IAC_EVR
         if (obisel) then xecount = xecount(indsel)
;
         ftype = 'PING_CNT'
         ping = tbget(hdr,tab,ftype)              ;SAAD-B counter (cts/sec)
         if (obisel) then ping = ping(indsel)
;
         ftype = 'SAADA_CT'
         sdacount = tbget(hdr,tab,ftype)          ;SAAD-A counter (cts/sec)
         if (obisel) then sdacount = sdacount(indsel)
;
         ftype = 'SAADB_CT'
         sdbcount = tbget(hdr,tab,ftype)          ;SAAD-b counter (cts/sec)
         if (obisel) then sdbcount = sdbcount(indsel)
;
         ftype = 'A1AH_CNT'
         a1hcount = tbget(hdr,tab,ftype)    ;A1 counter above high level thresh
         if (obisel) then a1hcount = a1hcount(indsel)     ;(cts/sec)
;
         ftype = 'A2AH_CNT'
         a2hcount = tbget(hdr,tab,ftype)    ;A2 counter above high level thresh
         if (obisel) then a2hcount = a2hcount(indsel)     ;(cts/sec)
;
         ftype = 'K1ED_CNT'
         k1count = tbget(hdr,tab,ftype)           ;K1 edge event counter
         if (obisel) then k1count = k1count(indsel)
;
         ftype = 'K2ED_CNT'
         k2count = tbget(hdr,tab,ftype)           ;K2 edge event counter
         if (obisel) then k2count = k2count(indsel)
;
         ftype = 'K1PA_CNT'
         v1count = tbget(hdr,tab,ftype)       ;K1 edge counts passing sel logic
         if (obisel) then v1count = v1count(indsel)
;
         ftype = 'K2PA_CNT'
         v2count = tbget(hdr,tab,ftype)       ;K2 edge counts passing sel logic
         if (obisel) then v2count = v2count(indsel)
;
         row = {evcounts2,a2l:0.0,a1lmv:0.0,ping:0.0,sda:0.0,sdb:0.0,a1h:0.0, $
                          a2h:0.0,k1:0.0,k2:0.0,k1pass:0.0,k2pass:0.0}
         counts2 = replicate(row,nevr)
         if (nevr gt 1) then begin
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
         endif else begin
           counts2.a2l = a2lcount(0)
           counts2.a1lmv = xecount(0)
           counts2.ping = ping(0)
           counts2.sda = sdacount(0)
           counts2.sdb = sdbcount(0)
           counts2.a1h = a1hcount(0)
           counts2.a2h = a2hcount(0)
           counts2.k1 = k1count(0)
           counts2.k2 = k2count(0)
           counts2.k1pass = v1count(0)
           counts2.k2pass = v2count(0)
         endelse
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
         end
    'H': counts2 = 0
  endcase
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
;
sz=size(inputs)
if (sz(0) ne 0) then inputs = oparms        ;inputs = parameter string array
;
return
end         ;pro rdfgetevr
