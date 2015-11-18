;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       oldorb_pos
;
;*PURPOSE:
;   A procedure to read satellite orbital positions and other data 
;   from Rosat .SO FITS files   (do NOT convert to ST SDAS format!)
;
;*CALLING SEQUENCE:
;       oldorb_pos, inputs, obinum, time, satpos, orbvecs, relpos, $
;                   OBINFO=OBINFO, OPARMS=OPARMS
;
;*PARAMETERS:
; INPUTS:
;       inputs - input parameter description.
;                0 - use all defaults
;                1 - interative selection of parameters
;                string - parameter defintion of the form
;                   'parname1=value1,parname2=value2,,,,'
;                string array - each element of the array has
;                   form 'value' (elements must be in correct order)
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
;                    (default is SO)
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
;       time - times for which orbital positions were determined
;       satpos   - data structure containing the following:
;         lon    - satellite orbit longitudes (decimal degrees)
;         lat    - satellite orbit latitudes (decimal degrees)
;         height - height of satellite above Earth (in kilometers)
;       has the structure of replicate(row,npos), where
;       row={sporblonlat,lon:0.0,lat:0.0,height:0.0}
;       and npos is the total number of measurements
;
;       orbvecs - data structure containing the unit vectors pointing
;                 from the center of the Earth to the Sun, Moon, and 
;                 satellite. The structure is as follows:
;         sunx  - x component of unit vector
;         suny  - y component   "        "
;         sunz  - z component   "        "
;         moonx - x component of unit vector
;         moony - y component   "        "
;         moonz - z component   "        "
;         satx  - x component of unit vector
;         saty  - y component   "        "
;         satz  - z component   "        "
;       has the structure of replicate(row,npos), where
;       row={sporbvecs,sunx:0.0,suny:0.0,sunz:0.0,moonx:0.0,moony:0.0,$
;            moonz:0.0,satx:0.0,saty:0.0,satz:0.0}
;       and npos is the total number of measurements
;
;       relpos - data structure containing various satellite positions
;                relative to the Weilheim station, etc.:
;                (It's still not clear to me what az & el are)
;         az   - azimuth of satellite (decimal degrees)
;         el   - elevation of satellite (decimal degrees)
;         gha  - Greenwich Hour Angle of satellite (decimal degrees)
;         dist - Distance of satellite to Weilheim station (kilometers)
;       has the structure of replicate(row,npos), where
;       row={sporbazel,az:0.0,el:0.0,gha:0.0,dist:0.0}
;       and npos is the total number of measurements
;
;       obinfo - data structure containing dates, times, & indices of
;                beginnings & ends of obi segments
;                has the structure of replicate(row,num), where
;                row={obitime,sctbeg:0.0D0,yrbeg:0,daybeg:0,utsbeg:0.0D0,
;                     sctend:0.0D0,yrend:0,dayend:0,utsend:0.0D0,
;                     ibeg:0L,iend:0L}
;                and num is the total number of intervals
;         ibeg - indices of values in TIME equal to values in UTSBEG
;         iend - indices of values in TIME equal to values in UTSEND
;
;       oparms - string array which is the parsed form of inputs (same as
;                LIST in example 3 below). Allows program to be rerun using 
;                same values w/o resetting.
;
;*EXAMPLES: There are 4 different ways that the inputs may be specified
;    mode 1) Interactive
;        IDL>oldorb_pos,1,obinum,time,satpos, ...
;             ?obseq=rp123456
;	      ?dir=mydirectory
;             ?instr=P
;	      ?chatter=0
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydirectory,instr=P,chatter=0'
;        IDL>oldorb_pos,list,obinum,time,satpos, ...
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,10)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydirectory'
;        IDL>list(3)='P'
;        IDL>list(9)='0'
;        IDL>oldorb_pos,list,obinum,time,satpos, ...
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;	     chatter=0
;            *exit
;        IDL>oldorb_pos,'myinput.dat',obinum,time,satpos, ...
;
;*RESTRICTIONS:
;
;*NOTES:
;  Will read FITS files directly. Do NOT convert to ST SDAS format.
;  Input parameter TRIM is ignored (and need not be specified).
;
;*SUBROUTINES CALLED:
;  RSGETPAR
;  MATCH_FILES
;  FITS_INFO
;  READFITS
;  TBGET
;  RSOBITIMES
;  YMD2DN
;
;*MODIFICATION HISTORY:
;    written 30 Apr 1991 by GAR
;    modified to use match_files 17 May 1991 by GAR
;    modified 28 Aug 1991 by GAR to handle observations spanning > 1 day
;                                and to return IBEG 
;    modified 30 Aug 1991 by GAR to allow selection of OBIs to be read
;    modified 11 Sep 1991 by GAR to work under XWindows emulator
;    modified 17 Sep 1991 by GAR to put calculation of sctbeg into
;                                subroutine RSOBITIMES
;    modified 30 Sep 1991 by GAR because HRI field names are not same as
;                                field names for PSPC
;    modifed to use GHRS derived parameter setting routines 05 Oct 1991 GAR
;    modified to combine sctbeg & obibeg, sctend & obiend, and to make into
;        keywords  08 Oct 1991  GAR
;    modified 08 Nov 1991 for compatibility with Sun Unix (GAR)
;    modified 18 Feb 1992 to return OBI info as a data structure (GAR)
;    modified 01 Mar 1992 to return various outputs in data structures (GAR)
;    modified 04 Jun 1992 to use READFITS, FITS_INFO, and new version
;        of MATCH_FILES, and to return OPARMS (GAR)
;    modified 08 Jul 1992 to fix bug (GAR)
;    modified 26 Aug 1992 (GAR) to read MPE format files (input variable PROC 
;      added; most calls to TBGET changed to FITS_GET)
;    modified 10 Aug 1993 (GAR) to use extension for archival MPE format 
;      data
;    modified 20 Dec 1993 (GAR): name changed from RSORB_POS to OLDORB_POS
;      (to allow RSORB_POS to read RDF format files as well as US & MPE format)
;-
;-------------------------------------------------------------------------------
pro oldorb_pos,inputs,obinum,time,satpos,orbvecs,relpos,obinfo=obinfo,$
               oparms=oparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' OLDORB_POS, inputs, obinum (0 for all), TIME, SATPOS, ORBVECS, '
  print,'             RELPOS, OBINFO=OBINFO, OPARMS=OPARMS'
  print,'   Uses inputs OBSEQ, DIR, EXTYP, INSTR, PROC, and CHATTER ' $
       +'from RSGET.DEF'
  retall
endif
;
dfile = 'rsget'
rsgetpar,inputs,dfile,oparms
obseq = strtrim(oparms(0),2)
dir = strtrim(oparms(1),2)
extyp = strtrim(oparms(2),2)
instr = strtrim(oparms(3),2)
instr = strtrim(strupcase(instr),2)
proc = strtrim(oparms(4),2)            ;processing format - US or MPE
proc = strupcase(proc)
case proc of
  'US':  begin
         if (extyp eq '') then extyp = 'so'
         ext = '.'+extyp
         extuc = strupcase(extyp)
         if (extuc ne 'SO') then begin
           print,extyp,' is not a valid extension for a US format orbit', $
                       ' data file.'
           print,' Valid options are CAS, SA, SAS (PSPC) or AO (HRI).', $
                 ' Returning.'
           retall 
         endif
         end
  'MPE': ext = '_orbit.tfits'
  else : begin
         print,' This routine only reads orbit data from US or MPE format', $
               ' files.'
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
if ( (instr ne 'P') and (instr ne 'H')) then begin                  
  instr = ''
  read,' Please specify instrument - P = PSPC, H = HRI  ',instr
  instr = strupcase(instr)
  oparms(3) = instr
endif
;
if (npar lt 2) then obinum = 0                   ;default is to read all
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
time = [-999]
rs_lon = [-999] & rs_lat = rs_lon & height = rs_lon
;
sunx = [-999] & suny = sunx & sunz = sunx
moonx = [-999] & moony = moonx & moonz = moonx
satx = [-999] & saty = satx & satz = satx
;
sataz = [-999] & satel = sataz & satgha = sataz & satdist = sataz
;
ibeg = intarr(nobi)+long(0.) 
sctbeg = dblarr(nobi)
sctend = sctbeg
yrbeg = intarr(nobi)
yrend = yrbeg
daybeg = intarr(nobi)
dayend = daybeg
utsbeg = dblarr(nobi)
utsend = utsbeg
;
if (chatter eq 1) then $
   print,' Reading orbital data file ',name
ict = 0
for jj=0,nobi-1 do begin
  jobi = obinum(jj)
  if (chatter eq 1) then print,'    OBI ',jobi
  tab = readfits(name,hdr,ext=jobi,/sil)
;
  if (proc eq 'MPE') then entry = 0 else begin
    ftype = 'IUT3_SO'
    if (instr eq 'H') then ftype = 'UNIVERSAL TIME - SUBSECONDS'
    entry = tbget(hdr,tab,ftype)/10./1000.       ;subsecond time (in sec)
  endelse
  ftype = 'IUT2_SO'
  if (instr eq 'H') then ftype = 'UNIVERSAL TIME - SECONDS'
  if (proc eq 'MPE') then ftype = 'DAYSEC'
  entry = entry + fits_get(hdr,tab,ftype)         ;time in seconds
;
; read OBI start and stop time info from header.
;
  rsobitimes,hdr,sctb,scte,utb,ute,proc=proc
  nument = n_elements(entry)                  ;number in this section
  if (!debug gt 3) then print,nument,minmax(entry),entry(0),entry(nument-1)
  time = [time,entry]
;
  begt = entry(0)                            ;beginning of OBI in UT sec
  endt = entry(nument-1)                     ;end of OBI in UT sec
  ibeg(jj) = ict                             ;beginning indices of OBIs
  ict = ict + nument
;
  if (proc eq 'MPE') then ftype = 'DATE' else begin
    ftype = 'IUT1_SO'
    if (instr eq 'H') then ftype = 'UNIVERSAL TIME - DATE'
  endelse
  entry = fits_get(hdr,tab,ftype)                 ;date in YYMMDD format
  begdate = entry(0)
  enddate = entry(nument-1)
;
; calculate OBI spacecraft and UT start times
; correct spacecraft start time if UT start time read from header
; is not same as first UT time in time array
;
  if (utb(2) ne begt) then sctb = sctb + (begt - utb(2))
  sctbeg(jj) = sctb
  yy = fix(begdate/1.e4)  
  mm = fix( (begdate-yy*1.e4)/1.e2)
  dd = fix(begdate-yy*1.e4-mm*1.e2)
  yy = yy + 1900
  dayn = ymd2dn(yy,mm,dd)
  yrbeg(jj) = yy
  daybeg(jj) = dayn
  utsbeg(jj) = begt
;
; calculate OBI spacecraft and UT stop times
; correct spacecraft stop time if UT stop time read from header
; is not same as last UT time in time array
;
  if (ute(2) ne endt) then scte = scte + (endt - ute(2))
  sctend(jj) = scte
  yy = fix(enddate/1.e4)  
  mm = fix( (enddate-yy*1.e4)/1.e2)
  dd = fix(enddate-yy*1.e4-mm*1.e2)
  yy = yy + 1900
  dayn = ymd2dn(yy,mm,dd)
  yrend(jj) = yy
  dayend(jj) = dayn
  utsend(jj) = endt
;
; now read other parameters, if desired
;
  if (npar ge 4) then begin
    if (proc eq 'MPE') then ftype = 'LONGITUDE' else begin
      ftype = 'ILON_SO'
      if (instr eq 'H') then ftype = 'LONGITUDE EAST'
    endelse
    entry = fits_get(hdr,tab,ftype)/10./3600.  ;longitude in decimal degrees
    rs_lon = [rs_lon,entry]
;
    if (proc eq 'MPE') then ftype = 'LATITUDE' else begin
      ftype = 'ILAT_SO'
      if (instr eq 'H') then ftype = 'GEODETIC LATITUDE NORTH'
    endelse
    entry =fits_get(hdr,tab,ftype)/10./3600.   ;latitude in decimal degrees
    rs_lat = [rs_lat,entry]
;
    if (proc eq 'MPE') then ftype = 'ALTITUDE' else begin
      ftype = 'IALT_SO'
      if (instr eq 'H') then ftype = 'ALTITUDE'
    endelse
    entry = fits_get(hdr,tab,ftype)/1000.      ;satellite altitude in kilometers
    height = [height,entry]
  endif
;
  if (npar ge 5) then begin
    if (proc eq 'MPE') then ftype = 'XSUN' else begin
      ftype = 'ISUN_SOX'
      if (instr eq 'H') then ftype = 'SUN UNIT VECTOR - X COMPONENT'
    endelse
    entry = fits_get(hdr,tab,ftype)/1.e8       ;x comp Earth-Sun unit vector
    sunx = [sunx,entry]
;
    if (proc eq 'MPE') then ftype = 'YSUN' else begin
      ftype = 'ISUN_SOY'
      if (instr eq 'H') then ftype = 'SUN UNIT VECTOR - Y COMPONENT'
    endelse
    entry = fits_get(hdr,tab,ftype)/1.e8       ;y comp
    suny = [suny,entry]
;
    if (proc eq 'MPE') then ftype = 'ZSUN' else begin
      ftype = 'ISUN_SOZ'
      if (instr eq 'H') then ftype = 'SUN UNIT VECTOR - Z COMPONENT'
    endelse
    entry = fits_get(hdr,tab,ftype)/1.e8       ;z comp
    sunz  = [sunz ,entry]
;
    if (proc eq 'MPE') then ftype = 'XMOON' else begin
      ftype = 'MOON_SOX'
      if (instr eq 'H') then ftype = 'MOON UNIT VECTOR - X COMPONENT'
    endelse
    entry = fits_get(hdr,tab,ftype)/1.e8       ;x comp Earth-Moon unit vector
    moonx = [moonx,entry]
;
    if (proc eq 'MPE') then ftype = 'YMOON' else begin
      ftype = 'MOON_SOY'
      if (instr eq 'H') then ftype = 'MOON UNIT VECTOR - Y COMPONENT'
    endelse
    entry = fits_get(hdr,tab,ftype)/1.e8       ;y comp
    moony = [moony,entry]
;
    if (proc eq 'MPE') then ftype = 'ZMOON' else begin
      ftype = 'MOON_SOZ'
      if (instr eq 'H') then ftype = 'MOON UNIT VECTOR - Z COMPONENT'
    endelse
    entry = fits_get(hdr,tab,ftype)/1.e8       ;z comp
    moonz  = [moonz ,entry]
;
    if (proc eq 'MPE') then ftype = 'XSATELLITE' else begin
      ftype = 'ISAT_SOX'
      if (instr eq 'H') then ftype = 'SATELLITE POSITION VECTOR - X CO'
    endelse
    entry = fits_get(hdr,tab,ftype)/1.e8     ;x comp Earth-Satellite unit vector
    satx = [satx,entry]
;
    if (proc eq 'MPE') then ftype = 'YSATELLITE' else begin
      ftype = 'ISAT_SOY'
      if (instr eq 'H') then ftype = 'SATELITTE POSITION VECTOR - Y CO'
    endelse
    entry = fits_get(hdr,tab,ftype)/1.e8       ;y comp
    saty = [saty,entry]
;
    if (proc eq 'MPE') then ftype = 'ZSATELLITE' else begin
      ftype = 'ISAT_SOZ'
      if (instr eq 'H') then ftype = 'SATELLITE POSITION VECTOR - Z CO'
    endelse
    entry = fits_get(hdr,tab,ftype)/1.e8       ;z comp
    satz  = [satz ,entry]
  endif
;
  if (npar ge 6) then begin
    if (proc eq 'MPE') then ftype = 'AZIMUTSAT' else begin
      ftype = 'IAZS_SO'
      if (instr eq 'H') then ftype = 'AZIMUTH'
    endelse
    entry = fits_get(hdr,tab,ftype)/10./3600.  ;satellite azimuth in dec deg
    sataz = [sataz,entry]
;
    if (proc eq 'MPE') then ftype = 'ELEVATION' else begin
      ftype = 'IELS_SO'
      if (instr eq 'H') then ftype = 'ELEVATION'
    endelse
    entry = fits_get(hdr,tab,ftype)/10./3600.  ;satellite elevation in dec deg
    satel = [satel,entry]
;
    if (proc eq 'MPE') then ftype = 'HOURANGLE' else begin
      ftype = 'IGHA_SO'
      if (instr eq 'H') then ftype = 'GREENWICH HOUR ANGLE'
    endelse
    entry = fits_get(hdr,tab,ftype)/10./3600.  ;Greenwich Hour Angle in dec deg
    satgha = [satgha,entry]
;
    if (proc eq 'MPE') then ftype = 'DISTSAT' else begin
      ftype = 'IDIS_SO'
      if (instr eq 'H') then ftype = 'WEILHEIM STATION DISTANCE'
    endelse
    entry = fits_get(hdr,tab,ftype)/1000.      ;distance from station in kilom
    satdist = [satdist,entry]
  endif
;
endfor
;
time = time(1:*)
npos = n_elements(time)
if (npar ge 4) then begin
  rs_lon = rs_lon(1:*)
  rs_lat = rs_lat(1:*)
  height = height(1:*)
;
  row = {sporblonlat,lon:0.0,lat:0.0,height:0.0}
  satpos = replicate(row,npos)
  satpos.lon = rs_lon
  satpos.lat = rs_lat
  satpos.height = height
;
  rs_lon = 0                       ;free up some memory
  rs_lat = 0
  height = 0
endif
;
if (npar ge 5) then begin
  sunx = sunx(1:*)
  suny = suny(1:*)
  sunz = sunz(1:*)
;
  moonx = moonx(1:*)
  moony = moony(1:*)
  moonz = moonz(1:*)
;
  satx = satx(1:*)
  saty = saty(1:*)
  satz = satz(1:*)
  length = sqrt(satx*satx+saty*saty+satz*satz)  ;length of vector
  satx = satx/length
  saty = saty/length
  satz = satz/length
;
  row = {sporbvecs,sunx:0.0,suny:0.0,sunz:0.0,moonx:0.0,moony:0.0,moonz:0.0,$
         satx:0.0,saty:0.0,satz:0.0}
  orbvecs = replicate(row,npos)
  orbvecs.sunx = sunx
  orbvecs.suny = suny
  orbvecs.sunz = sunz
  orbvecs.moonx = moonx
  orbvecs.moony = moony
  orbvecs.moonz = moonz
  orbvecs.satx = satx
  orbvecs.saty = saty
  orbvecs.satz = satz
;
  sunx = 0                       ;free up some memory
  suny = 0
  sunz = 0
  moonx = 0
  moony = 0
  moonz = 0
  satx = 0
  saty = 0
  satz = 0
endif
;
if (npar ge 6) then begin
  sataz = sataz(1:*)
  satel = satel(1:*)
  satgha = satgha(1:*)
  satdist = satdist(1:*)
;
  row = {sporbazel,az:0.0,el:0.0,gha:0.0,dist:0.0}
  relpos = replicate(row,npos)
  relpos.az = sataz
  relpos.el = satel
  relpos.gha = satgha
  relpos.dist = satdist
;
  sataz = 0                       ;free up some memory
  satel = 0
  satgha = 0
  satdist = 0
endif
;
if (nobi gt 1) then iend = [ibeg(1:*)-1,n_elements(time)-1] else $
                    iend = [n_elements(time)-1]
if (!debug gt 2) then stop,'Stopping in oldorb_pos after iend defined'
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
sz = size(inputs)
if (sz(0) ne 0) then inputs = oparms        ;inputs = parameter string array
;
return
end         ;pro oldorb_pos
