;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;        rschecktsi
;
;*PURPOSE:
; A procedure to check the Temporal Status Intervals table of a Rosat
; events list (FITS) file
; Reads FITS file directly (do NOT convert to ST SDAS format!)
;
;*CALLING SEQUENCE:
; rschecktsi,inputs,TSINFO,LEVELS,textout=textout,oparms=oparms
;
;*PARAMETERS:
; INPUTS:
;   INPUTS - input parameter description.
;        0 - use all defaults
;        1 - interative selection of parameters
;   string - parameter defintion of the form
;            'parname1=value1,parname2=value2,,,,'
;            string array - each element of the array has
;            form 'value' (entries must be given in correct order)
;   string of form '$filename' where filename is the
;      name of the file containing one parameter
;      per line in the form: parname=value
;   Any parameter not specified is set to its default.
;   Defaults for the parameters can be found by using interactive
;   selection params=1 or examinining the default (text) file,
;   ZDEF:RSGET.DEF.
;
; The following parameters are availble.
;
;   OBSEQ       Root name of input data file 
;                      (null string not allowed, must be specified)
;   DIR         Directory containing input file 
;                      (default is current directory)
;   INSTR       Instrument 
;                      (default is P for PSPC)
;   PROC        Format of processed files (e.g., US, MPE)
;                      (default is 'US')
;   CHATTER     Controls program feedback to user
;                      (default = 1)
;
; OPTIONAL KEYWORD INPUT:
;    TEXTOUT - Directs output display (see TEXTOPEN for more info)
;
;                textout=-1     no listing
;		 textout=1	TERMINAL using /more option
;		 textout=2	TERMINAL without /more option
;		 textout=3	<program>.prt
;		 textout=4	laser.tmp
;		 textout=5      user must open file
;                textout = filename (default extension of .prt)
;
; OUTPUTS:
;
;   TSINFO -  data structure containing the TSI bitcodes and ASCII
;             translations. The structure is as follows:
;     TSIBEG  - Start time of interval
;     TSIEND  - End time of interval
;               Consecutive intervals with the same values for FAILBITS
;               and STATBITS (and TELQBITS, for HRI), will be combined
;     FAILED  - String array giving ASCII translation of bit-encoded
;               failed flags for each interval. If no flags were set,
;               then FAILED(ii) = 'Good Time Interval'
;     STATUS  - String array giving ASCII translation of status
;               settings. According to the Data Products Guide, these
;               are the settings for determining whether flags are set.
;     TELQUAL - String array giving ASCII translation of telemetry
;               quality status settings.
;     FAILBITS- Binary string array, giving bit encoded failed flags for 
;               each interval
;     STATBITS- Binary string array, giving bit encoded status flags for
;               each interval
;     TELQBITS- Binary string array, giving bit encoded telemetry 
;               quality status flags for each interval
;   has the structure of replicate(row,ntsi), where
;   row = {tsi_info,tsibeg:0.D0,tsiend:0.D0,failed:'',status:'',$
;          telqual:'',failbits:'',statbits:'',telqbits:''}
;   and ntsi is the total number of (combined) TS intervals
;
;   LEVELS      Vector which contains levels for each interval.
;               Each element of the vector is a structure. The format of
;               the structure depends on the instrument:
;
;     for PSPC: uses the structure for pspc_tsi_levels
;               {pspc_tsi_levels,tstart:0.D0,tstop:0.D0,rmb:0,dfb:0} where
;       TSTART  start time for measurement of level
;       TSTOP   stop time for measurement of level
;               these intervals will *not* be combined
;       RMB     running mean background
;       DFB     difference between RMB and ??
;
;     for HRI:  uses the structure for hri_tsi_levels
;               {hri_tsi_levels,tstart:0.D0,tstop:0.D0,hibk:0,hvlev:0,
;                vg:0,aspstat:'',aspstvals:0,asperr:'',aspevals:0,
;                saadind:'',saada:0,saadb:0,temp1:0,temp2:0,temp3:0} where
;       TSTART  start time for measurement of level
;       TSTOP   stop time for measurement of level
;               these intervals will *not* be combined
;       HIBK    High Background Level (units 1e-7 counts/sq. pixel/sec)
;       HVLEV   High Voltage Level (16 levels, 0 to 15)
;       VG      Viewing Geometry (5 levels, 1 to 5)
;               see the Data Products guide for an explanation of the
;               viewing geometry codes. (1 is best, 5 is unusable.)
;       ASPSTAT Aspect Tracker Status (5 levels, 1 to 5)
;               these are translated into the ASCII codes (acc. to
;               the Data Products Guide)
;       ASPSTVALS the actual integer values of ASPSTAT
;       ASPERR  Aspect Error Status Quality (10 levels, 1 to 10)
;               these are translated into the ASCII codes (acc. to
;               the Data Products Guide)
;       ASPEVALS the actual integer values of ASPERR
;       SAADIND Indicates the active SAA detector (3 levels, 1 to 3)
;               these are also translated into ASCII
;       SAADA   level of SAA Detector A (4 levels, from 0 to 15 cts/sec)
;       SAADB      "              "   B     "                    "
;       TEMP1   temperature of Temperature Sensor 1 (8 levels, 10 to 25 deg C)
;       TEMP2      "              "          "    2     "                 "
;       TEMP3      "              "          "    3     "                 "
;
;       oparms - string array which is the parsed form of inputs (same as
;                LIST in example 3 below). Allows program to be rerun using 
;                same values w/o resetting.
;
;*EXAMPLES: There are 4 different ways that the inputs may be specified
;    mode 1) Interactive
;        IDL>rschecktsi,1,tsinfo,levels,...
;             ?obseq=rp123456
;	      ?dir=mydirectory
;             ?instr=P
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydirectory,instr=P'
;        IDL>rschecktsi,list,tsinfo,levels,...
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,10)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydirectory'
;        IDL>list(2)='P'
;        IDL>rschecktsi,list,tsinfo,levels,...
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;	     chatter=Y
;            *exit
;        IDL>rschecktsi,'myinput.dat',tsinfo,levels,...
;
;*NOTES:
;  Will read FITS file directly. Do NOT convert to ST SDAS format.
;
;*SUBROUTINES CALLED:
;  RSGETPAR
;  MATCH_FILES
;  READFITS
;  TBGET
;  TSI_BITCODES
;  TO_BIN
;  TO_HEX_LARGE
;  BITC2ASC
;  TSILIST
;
;*MODIFICATION HISTORY:
;  written 22 Nov 1991 by GAR
;  modified 25 Nov 1991 to work for HRI data, and so that consecutive
;  intervals with same flags will be combined, and to use subroutine
;  TSI_BITCODES (GAR)
;  modified 27 Nov 1991 to use subroutine BITC2ASC, and to return 
;  telemetry quality status flags and tsi levels, and to list TSI info
;  using keyword TEXTOUT (GAR)
;  modified 18 Feb 1992 to allow textout < 0 (GAR)
;  modified 05 Jun 1992 to use READFITS and new version of MATCH_FILES, 
;    to combine outputs into data structure TSINFO, and to return OPARMS (GAR)
;  modified 30 Dec 1993 (GAR) to read TSI data from RDF format 
;      files: old RSCHECKTSI was renamed to OLDCHECKTSI; new code now
;      calls OLDCHECKTSI and the corresponding RDFCHECKTSI for reading RDF 
;      format files; new data structures also defined for RDF format data
;-
;-------------------------------------------------------------------------------
pro rschecktsi,inputs,tsinfo,levels,textout=textout,oparms=oparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RSCHECKTSI, inputs, TSINFO, LEVELS, textout=textout, oparms=oparms'
  print,'   Uses inputs OBSEQ, DIR, INSTR, PROC, and CHATTER from RSGET.DEF'
  retall
endif
if not keyword_set(TEXTOUT) then textout=!TEXTOUT   ;use default output
;
dfile = 'rsget'
rsgetpar,inputs,dfile,oparms
obseq = strtrim(oparms(0),2)
dir = strtrim(oparms(1),2)
instr = strtrim(oparms(3),2)
instr = strupcase(instr)
proc = strtrim(oparms(4),2)            ;processing format - US, MPE, or RDF
proc = strupcase(proc)
chatter = fix(oparms(9))
;
if (obseq eq '') then begin                      ;obseq must be defined
  read,'Please enter root name of input data file: ',obseq
  obseq = strtrim(obseq,2)
  oparms(0) = obseq
endif  
;
case 1 of
  (proc eq 'US'):  begin
     case npar of
       3: oldchecktsi,oparms,tsinfo,levels,textout=textout
       2: oldchecktsi,oparms,tsinfo,textout=textout
       else: begin
             print,' RSCHECKTSI needs at least 2 parameters to run. Returning.'
             retall
             end
      endcase
      end         
;
  (proc eq 'RDF'): begin
     case npar of
       3: rdfchecktsi,oparms,tsinfo,levels,textout=textout
       2: rdfchecktsi,oparms,tsinfo,textout=textout
       else: begin
             print,' RSCHECKTSI needs at least 2 parameters to run. Returning.'
             retall
             end
     endcase
     end
;
  else : begin
         print,proc,' is not a valid choice for processing format.'
         print,' Valid choices are US, RDF. Returning.'
         retall
         end
endcase
;
return
end         ;pro rschecktsi
