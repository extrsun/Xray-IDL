;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;        rdfchecktsi
;
;*PURPOSE:
; A procedure to check the Temporal Status Intervals table of a Rosat
; events list (FITS) file
; Reads FITS file directly (do NOT convert to ST SDAS format!)
;
;*CALLING SEQUENCE:
; rdfchecktsi,inputs,TSINFO,LEVELS,textout=textout,oparms=oparms
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
;     for PSPC: uses the structure for pspc_rdf_tsilev
;               {pspc_rdf_tsilev,tstart:0.D0,tstop:0.D0,rmb:0,dfb:0} where
;       TSTART  start time for measurement of level
;       TSTOP   stop time for measurement of level
;               these intervals will *not* be combined
;       RMB     running mean background
;       DFB     difference between RMB and ??
;
;     for HRI:  uses the structure for hri_tsi_levels
;               {hri_tsi_levels,tstart:0.D0,tstop:0.D0,hibk:0,hvlev:0,
;                vg:0,aspstat:'',aspstvals:0,asperr:'',aspevals:0,
;                saadind:'',saada:0,saadb:0,temp1:0,temp2:0,temp3:0,obinum:0} 
;                where
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
;        IDL>rdfchecktsi,1,tsinfo,levels,...
;             ?obseq=rp123456
;	      ?dir=mydirectory
;             ?instr=P
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydirectory,instr=P'
;        IDL>rdfchecktsi,list,tsinfo,levels,...
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,10)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydirectory'
;        IDL>list(2)='P'
;        IDL>rdfchecktsi,list,tsinfo,levels,...
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;	     chatter=Y
;            *exit
;        IDL>rdfchecktsi,'myinput.dat',tsinfo,levels,...
;
;*NOTES:
;  Will read FITS file directly. Do NOT convert to ST SDAS format.
;
;*RESTRICTIONS:
;  Does not yet work for MPE format data.
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
;  written  27 Dec 1993 by GAR (adapted from RSCHECKTSI)
;-
;-------------------------------------------------------------------------------
pro rdfchecktsi,inputs,tsinfo,levels,textout=textout,oparms=oparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RDFCHECKTSI, inputs, TSINFO, LEVELS, textout=textout, oparms=oparms'
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
if ( (instr ne 'P') and (instr ne 'H') ) then begin
  print,' Instrument must be specified as P for PSPC or as H for HRI.'
  print,' Please check your inputs. Returning.'
  retall
endif
;
proc = strtrim(oparms(4),2)            ;processing format - RDF only
proc = strupcase(proc)
if (proc ne 'RDF') then begin
  print,' Sorry, this routine works only for RDF format data.'
  print,' Please check your inputs. Returning.'
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
; read temporal status interval (ext=2) tables
;
ext = '_bas.fits'
match_files,dir,obseq,ext,name,nlist        ;look for the right files
name = name(0)                              ;default = take latest version
oparms(0) = strtrim(obseq,2)        ;obseq & dir may have been corrected
oparms(1) = strtrim(dir,2)
;
if (chatter eq 1) then print,' Reading TSI table file ',name
tabtsi = readfits(name,htsi,ext=4,/sil)
tsibeg = tbget(htsi,tabtsi,'time')        ;start time of interval
ntsi = n_elements(tsibeg)
tsiend = [tsibeg(1:*),tsibeg(ntsi-1)]     ;end with last start time for now
;
if (npar ge 3) then begin                 ;user wishes to look at the levels
  case instr of
    'H': row = {hri_tsi_levels,tstart:0.D0,tstop:0.D0,hibk:0,hvlev:0, $
                vg:0,aspstat:'',aspstvals:0,asperr:'',aspevals:0,saadind:'', $ 
                saada:0,saadb:0,temp1:0,temp2:0,temp3:0,obinum:0}
    'P': row = {pspc_rdf_tsilev,tstart:0.D0,tstop:0.D0,temp:0,press:0,hvlev:0, $
                filtpos:0,misshk:0,det:0,sumstat:0,hvstat:0,carrstat:0, $ 
                gasstat:0,tempstat:0,lvstat:0,current:0,aspqual:0,calstat:0, $
                rmb:0,dfb:0,user1:0,user2:0L}
  endcase
  nlevels = ntsi
  levels = replicate(row,nlevels)         ;define output structure 
  levels.tstart = tsibeg                  ;these intervals will not be
  levels.tstop = tsiend                   ;combined
endif
;
if (instr eq '') then instr = 'P'         ;default is PSPC
tsi_bitcodes,'R'+instr,fail_codes,fail_vals,stat_codes,stat_badvals,$
             telq_codes,telq_badvals
nfail = n_elements(fail_codes)
nstat = n_elements(stat_codes) 
ntelq = n_elements(telq_codes)
;
failbits = tbget(htsi,tabtsi,'failed')
statbits = tbget(htsi,tabtsi,'logicals')
case instr of
  'H': telqbits = tbget(htsi,tabtsi,'hqual')  ;telemetry quality status flags
  'P': telqbits = intarr(ntsi)                ;set these to 0 for PSPC for now
endcase
;
; combine exactly subsequent intervals (i.e., with zero time gap between 
; end of first and start of second) which have exactly the same values for
; failbits and statbits
;
seltsi = intarr(ntsi) + 1
for jj=1,ntsi-1 do begin
  condj = (failbits(jj) eq failbits(jj-1)) * $
          (statbits(jj) eq statbits(jj-1))
  if (instr eq 'H') then $
     condj = condj * (telqbits(jj) eq telqbits(jj-1)) 
  if (condj eq 1) then seltsi(jj) = 0
endfor
seltsi = where(seltsi eq 1)
if (!debug gt 2) then stop,'Stopping in RDFCHECKTSI after seltsi defined'
;
if (n_elements(seltsi) ne ntsi) then begin    ;combine consecutive TSIs
  ntsi = n_elements(seltsi)                   ;redefine number of TSIs
  tsibeg = tsibeg(seltsi)
  tsiend = [tsibeg(1:*),max(tsiend)]          ;end with last end time 
  failbits = failbits(seltsi)
  statbits = statbits(seltsi)
  telqbits = telqbits(seltsi)
  if (!debug gt 2) then stop,'Stopping in RDFCHECKTSI after intervals combined'
endif
;
; find number of good time intervals (i.e., no flags set) and likewise bad
; time intervals
;
indgti = where(failbits eq 0,ngti)
indbti = where(failbits ne 0,nbti)
;
; convert failbits and statbits from decimal numbers to binary number
; (new data type will be string)
;
failbits = to_bin(failbits,nfail+4)
statbits = to_bin(statbits,nstat+4)
telqbits = to_bin(telqbits,ntelq+4)
;
; failed and status will have same value for all good time intervals
;
failed = strarr(ntsi)
status = strarr(ntsi)
telqual = strarr(ntsi)
if (ngti ne 0) then begin
  failed(indgti) = 'Good Time Interval //'
  status(indgti) = 'No flags set  //'
  telqual(indgti) = 'No flags set  //'
endif
if (!debug gt 2) then stop,'Stopping in RDFCHECKTSI after all GTIs'
;
; now (if there are bad time intervals), figure out which flags were set
; this is done in subroutine BITC2ASC
;
if (nbti ne 0) then begin
  for ii = 0,nbti-1 do begin
    ibti = indbti(ii)
    failed(ibti) = bitc2asc(failbits(ibti),fail_codes,fail_vals,nfail)
    status(ibti) = bitc2asc(statbits(ibti),stat_codes,stat_badvals,nstat)
    telqual(ibti) = bitc2asc(telqbits(ibti),telq_codes,telq_badvals,ntelq)
  endfor
endif
if (!debug gt 2) then stop,'Stopping in RDFCHECKTSI after ASCII translations'
;
; now store TSI times, translations, & bitcodes in data structure TSINFO
;
row = {tsi_info,tsibeg:0.D0,tsiend:0.D0,failed:'',status:'',$
                telqual:'',failbits:'',statbits:'',telqbits:''}
tsinfo = replicate(row,ntsi)
;
tsinfo.tsibeg = tsibeg
tsinfo.tsiend = tsiend
tsinfo.failed = failed
tsinfo.status = status
tsinfo.telqual = telqual
tsinfo.failbits = failbits
tsinfo.statbits = statbits
tsinfo.telqbits = telqbits
if (!debug gt 2) then stop,'Stopping in RDFCHECKTSI after tsinfo defined'
;
; define translations for various HRI level flags
;
saacodes = ['Bad value (<=0)','SAA Detector A active','SAA Detector B active'$
           ,'Both SAA Detectors active'] 
hri_aspstats = ['Bad value (<=0)','Missing frames have occurred'$
               ,'Gyro solution,  no trackers used'$
               ,'Aspect solution from tracker 1'$
               ,'Aspect solution from tracker 2'$
               ,'Aspect solution from both trackers']
hri_asperrs = ['Bad value (<=0)','Errors < 12.5"','12.5" < errors < 25"'$
              ,'25" < errors < 37.5"','37.5" < errors < 50"'$
              ,'50" < errors < 100"','100" < errors < 300"'$
              ,'300" < errors < 600"','600" < errors < 1500"'$
              ,'1500" < errors < 3000"','3000" < errors < 6000"']
;
; if user requested levels to be returned, then store levels into LEVELS
;
if (npar ge 3) then begin                 ;user wishes to look at the levels
  case instr of
    'P': begin
         levels.temp = tbget(htsi,tabtsi,'temp')
         levels.press = tbget(htsi,tabtsi,'press')
         levels.hvlev = tbget(htsi,tabtsi,'hvlevel')
         levels.filtpos = tbget(htsi,tabtsi,'filt_pos')
         levels.misshk = tbget(htsi,tabtsi,'miss_hk')
         levels.det = tbget(htsi,tabtsi,'detector')
         levels.sumstat = tbget(htsi,tabtsi,'summ_sta')
         levels.hvstat = tbget(htsi,tabtsi,'hvstatus')
         levels.carrstat = tbget(htsi,tabtsi,'carrstat')
         levels.gasstat = tbget(htsi,tabtsi,'gas_sys')
         levels.tempstat = tbget(htsi,tabtsi,'tempstat')
         levels.lvstat = tbget(htsi,tabtsi,'lvstatus')
         levels.current = tbget(htsi,tabtsi,'current')
         levels.aspqual = tbget(htsi,tabtsi,'aspqual')
         levels.calstat = tbget(htsi,tabtsi,'calstat')
         levels.rmb = tbget(htsi,tabtsi,'rmbflag')
         levels.dfb = tbget(htsi,tabtsi,'dfbflag') 
         levels.user1 = tbget(htsi,tabtsi,'user1')
         levels.user2 = tbget(htsi,tabtsi,'user2')
         end
;
    'H': begin
         levels.hibk = tbget(htsi,tabtsi,'hiback')
         levels.hvlev = tbget(htsi,tabtsi,'hvlevel')
         levels.vg = tbget(htsi,tabtsi,'viewgeom')
         aspstvals = tbget(htsi,tabtsi,'aspstat')
         aspevals = tbget(htsi,tabtsi,'asperr')
         saadind = tbget(htsi,tabtsi,'saadind')
         levels.saada = tbget(htsi,tabtsi,'saada')
         levels.saadb = tbget(htsi,tabtsi,'saadb')
         levels.temp1 = tbget(htsi,tabtsi,'temp1')
         levels.temp2 = tbget(htsi,tabtsi,'temp2')
         levels.temp3 = tbget(htsi,tabtsi,'temp3')
         levels.obinum = tbget(htsi,tabtsi,'obi_num')
;
         save = saadind                 ;translate saadind into ASCII
         saadind = strarr(nlevels)
;
         aspstat = strarr(nlevels)
;
         asperr = strarr(nlevels)
         for jj=0,nlevels-1 do begin
           saadind(jj) = saacodes(save(jj)>0)
           aspstat(jj) = hri_aspstats(aspstvals(jj)>0)
           if (aspevals(jj) lt 11) then $
              asperr(jj) = hri_asperrs(aspevals(jj)>0) else asperr(jj) = '??'
         endfor
;
         levels.aspstat = aspstat
         levels.aspstvals = aspstvals
         levels.asperr = asperr
         levels.aspevals = aspevals
         levels.saadind = saadind
         end
  endcase
endif           ;defining output structure LEVELS (npar ge 3)
if (!debug gt 2) then stop,'Stopping in RDFCHECKTSI after LEVELS defined'
;
if (textout ge 0) then $
  tsilist,name,tsibeg,tsiend,failed,status,telqual,textout=textout
;
return
end         ;pro rdfchecktsi
