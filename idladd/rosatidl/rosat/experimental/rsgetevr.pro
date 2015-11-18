;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       rsgetevr
;
;*PURPOSE:
;   A procedure to read event rates and other related data 
;   from US or MPE format Rosat aspect FITS files, or RDF format ancillary
;   files
;
;*CALLING SEQUENCE:
;       RSGETEVR, inputs, obinum, SCTIME, RATES, COUNTS, RATES2, COUNTS2, 
;                 OBINFO=OBINFO, OPARMS=OPARMS
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
;        IDL>rsgetevr,1,obinum,sctime,rates, ...
;             ?obseq=rp123456
;	      ?dir=mydirectory
;	      ?chatter=0
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydirectory,chatter=0'
;        IDL>rsgetevr,list,obinum,sctime,rates, ...
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,10)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydirectory'
;        IDL>list(9)='0'
;        IDL>rsgetevr,list,obinum,sctime,rates, ...
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;	     chatter=0
;            *exit
;        IDL>rsgetevr,'myinput.dat',obinum,sctime,rates, ...
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
;  TABINV
;  NINT
;
;*MODIFICATION HISTORY:
;    written 8 May 1991 by GAR
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
;    modified 20 Dec 1993 (GAR) to read event rates data from RDF format 
;      ancillary files: old RSGETEVR was renamed to OLDGETEVR; new code now
;      calls OLDGETEVR and the corresponding RDFGETEVR for reading RDF 
;      format files; also to return yet more rates and counts and to
;      separate the output variables into rates & rates2, counts & counts2;
;      call statement changed to match
;-
;-------------------------------------------------------------------------------
pro rsgetevr,inputs,obinum,sctime,rates,counts,rates2,counts2,obinfo=obinfo,$
             oparms=oparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RSGETEVR, inputs, obinum (def = 0 for all), SCTIME, RATES, COUNTS,'
  print,'           RATES2, COUNTS2, OBINFO=OBINFO, OPARMS=OPARMS'
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
proc = strtrim(oparms(4),2)            ;processing format - US, MPE, or RDF
proc = strupcase(proc)
instr = strtrim(oparms(3),2)
chatter = fix(oparms(9))
;
if (obseq eq '') then begin                      ;obseq must be defined
  read,'Please enter root name of input data file: ',obseq
  obseq = strtrim(obseq,2)
  oparms(0) = obseq
endif  
;
case 1 of
  (proc eq 'US') or (proc eq 'MPE')  or (proc eq 'MPEUS'): begin
     scasp = 0
     case npar of
       7: oldgetevr,oparms,obinum,sctime,rates,counts,rates2,counts2,$
                    obinfo=obinfo
       6: oldgetevr,oparms,obinum,sctime,rates,counts,rates2,obinfo=obinfo
       5: oldgetevr,oparms,obinum,sctime,rates,counts,obinfo=obinfo
       4: oldgetevr,oparms,obinum,sctime,rates,obinfo=obinfo
       3: oldgetevr,oparms,obinum,sctime,obinfo=obinfo
       else: begin
             print,' RSGETEVR needs at least 3 parameters to run. Returning.'
             retall
             end
      endcase
      end         
;
  (proc eq 'RDF'): begin
     case npar of
       7: rdfgetevr,oparms,obinum,sctime,rates,counts,rates2,counts2,$
                    obinfo=obinfo
       6: rdfgetevr,oparms,obinum,sctime,rates,counts,rates2,obinfo=obinfo
       5: rdfgetevr,oparms,obinum,sctime,rates,counts,obinfo=obinfo
       4: rdfgetevr,oparms,obinum,sctime,rates,obinfo=obinfo
       3: rdfgetevr,oparms,obinum,sctime,obinfo=obinfo
       else: begin
             print,' RSGETEVR needs at least 3 parameters to run. Returning.'
             retall
             end
     endcase
     end
;
  else : begin
         print,proc,' is not a valid choice for processing format.'
         print,' Valid choices are US, MPE, MPEUS, RDF. Returning.'
         retall
         end
endcase
;
return
end         ;pro rsgetevr
