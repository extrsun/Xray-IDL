;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       rsgethksta
;
;*PURPOSE:
;   A procedure to read housekeeping status data from US or MPE format Rosat 
;   housekeeping status data (.STA) FITS files, or RDF format ancillary files
;
;*CALLING SEQUENCE:
;       RSGETHKSTA, inputs, obinum, SCTIME, HKFLAGS, OBINFO=OBINFO,
;                   OPARMS=OPARMS
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
;        IDL>rsgethksta,1,obinum,sctime,highv,car, ...
;             ?obseq=rp123456
;	      ?dir=mydirectory
;	      ?trim=1
;	      ?chatter=0
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydirectory,trim=1,chatter=0'
;        IDL>rsgethksta,list,obinum,sctime,hkflags, ...
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,10)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydirectory'
;        IDL>list(8)='1'
;        IDL>list(9)='0'
;        IDL>rsgethksta,list,obinum,sctime,hkflags, ...
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;	     chatter=0
;            *exit
;        IDL>rsgethksta,'myinput.dat',obinum,sctime,hkflags, ...
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
;    modified 22 Dec 1993 (GAR) to read binned hk data from RDF format 
;      ancillary files: old RSGETHKSTA was renamed to OLDGETHKSTA; new code now
;      calls OLDGETHKSTA and the corresponding RDFGETHKSTA for reading RDF 
;      format files
;-
;-------------------------------------------------------------------------------
pro rsgethksta,inputs,obinum,sctime,hkflags,obinfo=obinfo,oparms=oparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RSGETHKSTA, inputs, obinum (0 for all), SCTIME, HKFLAGS,'
  print,'             OBINFO=OBINFO, OPARMS=OPARMS'
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
trim = strtrim(oparms(8),2)
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
case 1 of
  (proc eq 'US') or (proc eq 'MPE'):  begin
     case npar of
       4: oldgethksta,oparms,obinum,sctime,hkflags,obinfo=obinfo
       3: oldgethksta,oparms,obinum,sctime,obinfo=obinfo
       else: begin
             print,' RSGETHKSTA needs at least 3 parameters to run. Returning.'
             retall
             end
      endcase
      end         
;
  (proc eq 'RDF'): begin
     case npar of
       4: rdfgethksta,oparms,obinum,sctime,hkflags,obinfo=obinfo
       3: rdfgethksta,oparms,obinum,sctime,obinfo=obinfo
       else: begin
             print,' RSGETHKSTA needs at least 3 parameters to run. Returning.'
             retall
             end
     endcase
     end
;
  else : begin
         print,proc,' is not a valid choice for processing format.'
         print,' Valid choices are US, MPE, RDF. Returning.'
         retall
         end
endcase
;
return
end         ;pro rsgethksta
