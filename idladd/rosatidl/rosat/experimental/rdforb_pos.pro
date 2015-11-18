;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       rdforb_pos
;
;*PURPOSE:
;   A procedure to read satellite orbital positions and other data 
;   from Rosat .SO FITS files   (do NOT convert to ST SDAS format!)
;
;*CALLING SEQUENCE:
;       rdforb_pos, inputs, obinum, SCTIME, SATPOS, ORBVECS, RELPOS,
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
;       sctime - spacecraft times for which orbital positions were determined
;       satpos   - data structure containing the following:
;         lon    - satellite orbit longitudes (decimal degrees)
;         lat    - satellite orbit latitudes (decimal degrees)
;         height - height of satellite above Earth (in meters)
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
;         dist - Distance of satellite to Weilheim station (meters)
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
;         ibeg - indices of values in SCTIME equal to values in SCTBEG
;         iend - indices of values in SCTIME equal to values in SCTEND
;
;       oparms - string array which is the parsed form of inputs (same as
;                LIST in example 3 below). Allows program to be rerun using 
;                same values w/o resetting.
;
;*EXAMPLES: There are 4 different ways that the inputs may be specified
;    mode 1) Interactive
;        IDL>rdforb_pos,1,obinum,sctime,satpos, ...
;             ?obseq=rp123456
;	      ?dir=mydirectory
;             ?instr=P
;	      ?chatter=0
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydirectory,instr=P,chatter=0'
;        IDL>rdforb_pos,list,obinum,sctime,satpos, ...
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,10)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydirectory'
;        IDL>list(3)='P'
;        IDL>list(9)='0'
;        IDL>rdforb_pos,list,obinum,sctime,satpos, ...
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;	     chatter=0
;            *exit
;        IDL>rdforb_pos,'myinput.dat',obinum,sctime,satpos, ...
;
;*RESTRICTIONS:
;
;*NOTES:
;  Will read FITS files directly. Do NOT convert to ST SDAS format.
;  Input parameter TRIM is ignored (and need not be specified).
;
;  Unlike OLDORB_POS, returns spacecraft clock times (instead of UT times)
;  Also does not divide satellite unit vector by its length, and returns
;  satellite height and distance (from station) in meters (not kilometers)
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
;    written  21 Dec 1993 by GAR (adapted from RSORB_POS)
;    modified 21 Jan 1994 (GAR) to replace FXPAR calls with def for
;        reference (integer & fractional) MJD values
;-     
;-------------------------------------------------------------------------------
pro rdforb_pos,inputs,obinum,sctime,satpos,orbvecs,relpos,obinfo=obinfo,$
               oparms=oparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RDFORB_POS, inputs, obinum (0 for all), SCTIME, SATPOS, ORBVECS, '
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
;
;extyp = strtrim(oparms(2),2)          ;don't care what extyp is for now
ext = '_anc.fits'
;
instr = strtrim(oparms(3),2)
proc = strtrim(oparms(4),2)            ;processing format - RDF only
proc = strupcase(proc)
if (proc ne 'RDF') then begin
  print,' This routine only reads aspect data from RDF format ancillary files.'
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
instr = strtrim(strupcase(instr),2)
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
; Now use match_files to get the whole file name for the data file.
;
match_files,dir,obseq,ext,name,nlist        ;look for the right files
name = name(0)                              ;default = take latest version
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
; In this extension, the time vector is given as modified Julian days
; (integer and fractional parts)
; Will need to convert this to spacecraft clock times (the numbers for this
; are the most accurately given)
;
if (chatter eq 1) then $
   print,' Reading orbit data from RDF ancillary data file ',name
tab = readfits(name,hdr,ext=1,/sil)         ;orbit data in 1st extension
mjdint = fits_get(hdr,tab,'MJD_INT')        ;integer part MJD
mjdfrac = fits_get(hdr,tab,'MJD_FRAC')      ;fractional part MJD
;mjdrefi = fxpar(hdr,'MJDREFI')
;mjdreff = fxpar(hdr,'MJDREFF')
;mjdreff = 8.7973375D-01             ;MJD fraction SC clock start
mjdrefi = 48043L                    ;MJD integer SC clock start
mjdreff = 8.79745370370074D-01      ;MJD fraction SC clock start
sctime = 24.*3600.0*( (mjdint-mjdrefi) + (mjdfrac-mjdreff) )    ;SC clock time
npos = n_elements(sctime)
;
; Get the number of unique OBIs, and the indices where these change
;
obival = fits_get(hdr,tab,'OBI_NUM')
nsize = 10000.
if (!version.os eq 'vms') then nsize = 2*nsize  
nval = n_elements(obival)
if (nval le nsize) then vsort,obival,1,trueval,indsrt,ibeg,iend,ntrue,mid=1 $
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
  npos = n_elements(indsel)
  sctime = sctime(indsel)
endif
if (!debug gt 2) then stop,' Stopping after indsel, npos defined'
;
; now read other parameters, if desired
;
if (npar ge 4) then begin
  ftype = 'LON_EAST'
  rs_lon = fits_get(hdr,tab,ftype)       ;longitude in decimal degrees
;
  ftype = 'LAT_NORT'
  rs_lat = fits_get(hdr,tab,ftype)       ;latitude in decimal degrees
;
  ftype = 'ALT_SAT'
  height = fits_get(hdr,tab,ftype)       ;satellite altitude in meters
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
  ftype = 'SUN_X'
  sunx = fits_get(hdr,tab,ftype)         ;x comp Earth-Sun unit vector
;
  ftype = 'SUN_Y'
  suny = fits_get(hdr,tab,ftype)         ;y comp
;
  ftype = 'SUN_Z'
  sunz = fits_get(hdr,tab,ftype)         ;z comp
;
  ftype = 'MOON_X'
  moonx = fits_get(hdr,tab,ftype)        ;x comp Earth-Moon unit vector
;
  ftype = 'MOON_Y'
  moony = fits_get(hdr,tab,ftype)        ;y comp
;
  ftype = 'MOON_Z'
  moonz = fits_get(hdr,tab,ftype)        ;z comp
;
; The FITS header says that these should not be divided by 1.D8 (as the sun 
; and moon vectors were. OLDORB_POS does divide by 1.D8, and then by the length)
;
  ftype = 'SAT_X'
  satx = fits_get(hdr,tab,ftype)         ;x comp Earth-Satellite unit vector
;
  ftype = 'SAT_Y'
  saty = fits_get(hdr,tab,ftype)         ;y comp
;
  ftype = 'SAT_Z'
  satz = fits_get(hdr,tab,ftype)         ;z comp
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
  ftype = 'AZIM_SAT'
  sataz = fits_get(hdr,tab,ftype)        ;satellite azimuth in dec deg
;
  ftype = 'ELEV_SAT'
  satel = fits_get(hdr,tab,ftype)        ;satellite elevation in dec deg
;
  ftype = 'GHA'
  satgha = fits_get(hdr,tab,ftype)       ;Greenwich Hour Angle in dec deg
;
  ftype = 'DIS_GSTA'
  satdist = fits_get(hdr,tab,ftype)      ;distance from station in meters
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
end         ;pro rdforb_pos
