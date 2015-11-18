;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       oldgetaspqu
;
;*PURPOSE:
;   A procedure to read aspect data (time, quality flag, star tracker flag)
;   from Rosat aspect quality (_ASP for PSPC; _?? for HRI) FITS files
;
;*CALLING SEQUENCE:
;       OLDGETASPQU, inputs, obinum, SCTIME, QUALFLAG, STARFLAG, NOMASP,
;                    OBINFO=OBINFO, OPARMS=OPARMS
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
;                    (default is ASP)
;        INSTR       Instrument 
;                    (default is P for PSPC)
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
;       qualflag - aspect quality flag (0-5,or 9 for gap)
;                  = IQL_ASP (US PSPC), ASP_QUAL (RDF)
;       starflag - star tracker flag (= -1 for missing frame)
;                  = IST_ASP (US PSPC), STT_QUAL (RDF)
;       nomasp   - 2D array containing nominal RA, Dec and Roll for each
;                  obi segment
;       obinfo   - data structure containing dates, times, & indices of
;                  beginnings & ends of obi segments
;                  has the structure of replicate(row,num), where
;                  row={obitime,sctbeg:0.0D0,yrbeg:0,daybeg:0,utsbeg:0.0D0,
;                       sctend:0.0D0,yrend:0,dayend:0,utsend:0.0D0,
;                       ibeg:0L,iend:0L}
;                  and num is the total number of intervals
;         ibeg   - indices of values in SCTIME equal to values in SCTBEG
;         iend   - indices of values in SCTIME equal to values in SCTEND
;
;       oparms   - string array which is the parsed form of inputs (same as
;                  LIST in example 3 below). Allows program to be rerun using 
;                  same values w/o resetting.
;
;*EXAMPLES: There are 4 different ways that the inputs may be specified
;    mode 1) Interactive
;        IDL>oldgetaspqu,1,obinum,sctime,qualflag,starflag, ...
;             ?obseq=rp123456
;	      ?dir=mydirectory
;	      ?trim=1
;	      ?chatter=0
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydirectory,trim=1,chatter=0'
;        IDL>oldgetaspqu,list,obinum,sctime,qualflag,starflag, ...
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,10)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydirectory'
;        IDL>list(8)='1'
;        IDL>list(9)='0'
;        IDL>oldgetaspqu,list,obinum,sctime,qualflag,starflag, ...
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;	     chatter=0
;            *exit
;        IDL>oldgetaspqu,'myinput.dat',obinum,sctime,qualflag,starflag, ...
;
;*RESTRICTIONS:
;   Will not yet work for HRI. Also does not work for MPE format files.
;
;*NOTES:
;   Will read FITS files directly. Do NOT convert to ST SDAS format.
;
;*SUBROUTINES CALLED:
;  RSGETPAR
;  MATCH_FILES
;  READFITS
;  FITS_INFO
;  TBGET
;  GETNOMASP
;  RSOBITIMES
;
;*MODIFICATION HISTORY:
;    written 28 Sep 1991 by GAR
;    modifed to use GHRS derived parameter setting routines 05 Oct 1991 GAR
;    modified to combine sctbeg & obibeg, sctend & obiend, and to make into
;        keywords  09 Oct 1991  GAR
;    modified 5 Nov 1991 for compatibility with Sun Unix (GAR)
;    modifed 18 Feb 1992 to return OBI info as a data structure (GAR)
;    modified 04 Jun 1992 to use READFITS, FITS_INFO, and new version
;        of MATCH_FILES, and to return OPARMS (GAR)
;    modified 08 Jul 1992 to fix bug (GAR)
;    modified 22 Dec 1993 (GAR): name changed from RSGETASPQU to OLDGETASPQU
;      (to allow RSGETASPQU to read RDF format files as well as US & MPE format)
;-
;-------------------------------------------------------------------------------
pro oldgetaspqu,inputs,obinum,sctime,qualflag,starflag,nomasp,obinfo=obinfo,$
                oparms=oparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' OLDGETASPQU, inputs, obinum (0 for all), SCTIME, QUALFLAG, STARFLAG,'
  print,'              NOMASP, OBINFO=OBINFO, OPARMS=OPARMS'
  print,'   Uses inputs OBSEQ, DIR, EXTYP, INSTR, TRIM, and CHATTER from', $
        ' RSGET.DEF'
  return
endif
;
dfile = 'rsget'
rsgetpar,inputs,dfile,oparms
obseq = strtrim(oparms(0),2)
dir = strtrim(oparms(1),2)
extyp = strtrim(oparms(2),2)
instr = strtrim(oparms(3),2)
if (strupcase(instr) ne 'P') then begin
  print,' Sorry, program only works for PSPC data. Requested instrument was', $
        instr,'.'
  print,' Please check your inputs. Returning.'
  retall
endif
;
proc = strtrim(oparms(4),2)            ;processing format - US or MPE
proc = strupcase(proc)
case proc of
  'US':  begin
         if (extyp eq '') then extyp = 'asp'
         ext = '.'+extyp
         extuc = strupcase(extyp)
         if (extuc ne 'ASP') then begin
           print,extyp,' is not a valid extension for a US format aspect', $
                       ' quality data file.'
           print,' Valid option is ASP (PSPC). Returning.'
           retall 
         endif
         end
  'MPE': begin
         print,' Sorry, program does not work for MPE format files. Returning.'
         retall
         end
  else : begin
         print,' This routine only reads aspect data from US PSPC aspect', $
               ' quality data files.'
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
  read,' Trim aspect quality data to good time intervals? (Y or N)',trim
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
if (!debug gt 2) then stop,' Stopping after files have been found'
;
fits_info,name,/silent,n_ext=next      ;get number of OBI extensions
if (obinum(0) eq 0) then $
   obinum = indgen(next) + 1           ;OBI numbers start with 1
nobi = n_elements(obinum)              ;if OBIs are specified
;
sctime = dblarr(1) - 999.
sctbeg = dblarr(nobi)
sctend = sctbeg
yrbeg = intarr(nobi)
yrend = yrbeg
daybeg = intarr(nobi)
dayend = daybeg
utsbeg = dblarr(nobi)
utsend = utsbeg
;
nomasp = dblarr(3,nobi)
qualflag = [-999] & starflag = qualflag
ibeg = intarr(nobi)+long(0.) 
;
if (chatter eq 1) then $
   print,' Reading ',extyp,' aspect quality data file ',name
ict = 0
for jj=0,nobi-1 do begin
  jobi = obinum(jj)
  if (chatter eq 1) then print,'    OBI ',jobi
  tab = readfits(name,hdr,ext=jobi,/sil)
;
  ftype = 'IT1_ASP'
  entry = tbget(hdr,tab,ftype)                  ;Time of aspect (s/c, sec)
  ftype = 'IT2_ASP'
  entry = entry + tbget(hdr,tab,ftype)/64.      ;time in subseconds
;
  rsobitimes,hdr,sctb,scte,utb,ute
  ind = where(entry gt 0)
  if (trim eq 1) then begin                     ;trim time array, if desired
    ind = where( (entry ge sctb) and (entry le scte))
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
; calculate OBI spacecraft and UT start times
; correct UT start time if spacecraft start time read from header
; is not same as first spacecraft time in time array
;
  if (sctb ne begt) then utb(2) = utb(2) + begt - sctb
  sctbeg(jj) = begt
  yrbeg(jj) = utb(0)
  daybeg(jj) = utb(1)
  utsbeg(jj) = utb(2)
;
; calculate OBI spacecraft and UT stop times
; correct spacecraft stop time if UT stop time read from header
; is not same as last UT time in time array
;
  if (scte ne endt) then ute(2) = ute(2) + endt - scte 
  sctend(jj) = endt
  yrend(jj) = ute(0)
  dayend(jj) = ute(1)
  utsend(jj) = ute(2)
;
; now read other parameters, if desired
;
  if (npar ge 4) then begin
    ftype = 'IQL_ASP'
    entry = tbget(hdr,tab,ftype)              ;aspect quality flag
    entry = entry(ind)
    qualflag = [qualflag,entry]
  endif
;
  if (npar ge 5) then begin
    ftype = 'IST_ASP'
    entry = tbget(hdr,tab,ftype)              ;star tracker flag
    entry = entry(ind)
    starflag = [starflag,entry]
  endif
;
  if (npar ge 6) then begin
    getnomasp,hdr,nra,ndec,nroll
    nomasp(0,jj) = [nra,ndec,nroll]
    if (!debug ge 3) then stop,'Stopping after getting nominal aspect'
  endif
;
endfor
;
sctime = sctime(1:*)
if (npar ge 4) then qualflag = fix(qualflag(1:*))
if (npar ge 5) then starflag = fix(starflag(1:*))
if (nobi gt 1) then iend = [ibeg(1:*)-1,n_elements(sctime)-1] else $
                    iend = [n_elements(sctime)-1]
;
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
end         ;pro oldgetaspqu
