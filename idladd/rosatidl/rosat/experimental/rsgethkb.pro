;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       rsgethkb
;
;*PURPOSE:
;   A procedure to read housekeeping data (time, temperature, pressure,
;   high voltage, instrument in use, filter wheel flag & position,
;   missing hkb data flag) from US or MPE format Rosat binned housekeeping data
;   (.HKB) FITS files, or RDF format ancillary files
;
;*CALLING SEQUENCE:
;       RSGETHKB, inputs, obinum, SCTIME, HKDAT, OBINFO=OBINFO, OPARMS=OPARMS
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
;       row = {binhkdata,mhkb:0,temp:0,press:0,hv:0,fwflag:0,fwpos:0,inuse:0}
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
;        IDL>rsgethkb,1,obinum,sctime,hkdat, ...
;             ?obseq=rp123456
;	      ?dir=mydirectory
;	      ?trim=1
;	      ?chatter=0
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydirectory,trim=1,chatter=0'
;        IDL>rsgethkb,list,obinum,sctime,hkdat, ...
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,10)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydirectory'
;        IDL>list(8)='1'
;        IDL>list(9)='0'
;        IDL>rsgethkb,list,obinum,sctime,hkdat, ...
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;	     chatter=0
;            *exit
;        IDL>rsgethkb,'myinput.dat',obinum,sctime,hkdat, ...
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
;    written  14 Feb 1992 by GAR
;    modified 18 Feb 1992 to return OBI info as a data structure (GAR)
;    modified 01 Mar 1992 to return outputs as a data structure (GAR)
;    modified 05 Jun 1992 to use READFITS and new version of MATCH_FILES, 
;      and to return OPARMS (GAR)
;    modified 22 Jun 1992 to fix bug (GAR)
;    modified 13 Aug 1993 (GAR) to use read archival MPE format data, and
;      to replace call to RSGETSEQINFO with call to RS_SEQINFO
;    modified 22 Dec 1993 (GAR) to read binned hk data from RDF format 
;      ancillary files: old RSGETHKB was renamed to OLDGETHKB; new code now
;      calls OLDGETHKB and the corresponding RDFGETHKB for reading RDF 
;      format files
;-
;-------------------------------------------------------------------------------
pro rsgethkb,inputs,obinum,sctime,hkdat,obinfo=obinfo,oparms=oparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RSGETHKB, inputs, obinum (0 for all), SCTIME, HKDAT,'
  print,'           OBINFO=OBINFO, OPARMS=OPARMS'
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
proc = strtrim(oparms(4),2)            ;processing format - US, MPE, or RDF
proc = strupcase(proc)
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
case 1 of
  (proc eq 'US') or (proc eq 'MPE'):  begin
     scasp = 0
     flags = 0
     case npar of
       4: oldgethkb,oparms,obinum,sctime,hkdat,obinfo=obinfo
       3: oldgethkb,oparms,obinum,sctime,obinfo=obinfo
       else: begin
             print,' RSGETHKB needs at least 3 parameters to run. Returning.'
             retall
             end
      endcase
      end         
;
  (proc eq 'RDF'): begin
     case npar of
       4: rdfgethkb,oparms,obinum,sctime,hkdat,obinfo=obinfo
       3: rdfgethkb,oparms,obinum,sctime,obinfo=obinfo
       else: begin
             print,' RSGETHKB needs at least 3 parameters to run. Returning.'
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
end         ;pro rsgethkb
