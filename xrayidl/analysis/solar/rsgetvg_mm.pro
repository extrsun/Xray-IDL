;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   rsgetvg
;
;*PURPOSE:
;   A procedure to read Rosat _SO FITS files and calculate viewing
;   geomtry. Also returns Sun-Earth_Satellite (SES) and 
;   Earth-Satellite-Target (EST) angles and numerical codes.
;
;*CALLING SEQUENCE:
;   rsgetvg, inputs, obinum, time, vginfo, [orbvecs, obinfo=obinfo,]
;            [oparms=oparms]
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
;       vginfo - data structure containing SES and EST angles, and values
;                of viewing geometry parameter. The structure is as follows:
;         lmst   - local mean sideral times (in hours)
;         sesang - Sun-Earth-Satellite angle (degrees)
;         estang - Earth-Satellite-Target angle (degrees)
;         sscode - numerical code for SES angle
;         epcode - numerical code for EST angle
;         vgcalc - numerical code for viewing geometry
;       has the structure of replicate(row,npos), where
;       row={viewgeom,lmst:0.0,sesang:0.0,estang:0.0,sscode:0,epcode:0,$
;                     vgcalc:0}
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
;        IDL>rsgetvg,1,obinum,time,vginfo, ...
;             ?obseq=rp123456
;	      ?dir=mydirectory
;             ?instr=P
;	      ?chatter=0
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=mydirectory,instr=P,chatter=0'
;        IDL>rsgetvg,list,obinum,time,vginfo, ...
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,10)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='mydirectory'
;        IDL>list(3)='P'
;        IDL>list(9)='0'
;        IDL>rsgetvg,list,obinum,time,vginfo, ...
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;	     chatter=0
;            *exit
;        IDL>rsgetvg,'myinput.dat',obinum,time,vginfo, ...
;
;*RESTRICTIONS:
;   Files must already have been converted to ST SDAS format.
;
;*NOTES:
;  Input parameter TRIM is ignored (and need not be specified).
;  Uses nominal pointing RA & Dec for entire sequence, and does not
;    correct for detector motion relative to nominal pointing direction
;    (e.g., wobble). Hence the results are only approximately correct.
;
;*SUBROUTINES CALLED:
;  RSORB_POS
;  RSGETPAR
;  MATCH_FILES
;  TBREAD
;  TBGET
;  HEADFITS
;  GETNOMASP
;  RSOBITIMES
;  YMD2DN
;
;*MODIFICATION HISTORY:
;    written 30 May 1992 by GAR
;    modified 10 Aug 1992 (GAR) sxhread replaced with headfits
;    modified 18 Aug 1992 (GAR) to fix vgcalc=0 bug when sscode=3
;-
;-------------------------------------------------------------------------------
pro rsgetvg,inputs,obinum,time,vginfo,orbvecs,obinfo=obinfo,oparms=oparms $
,SCTTIME=SCTTIME
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RSGETVG, inputs, obinum (0 for all), TIME, VGINFO, ORBVECS, '
  print,'       OBINFO=OBINFO, OPARMS=OPARMS'
  print,'   Uses inputs OBSEQ, DIR, EXTYP, INSTR, and CHATTER from RSGET.DEF'
  retall
endif
;
seslim = [125.,110.,90.]
estlim = [120.,86.,75.]
;
; First read orbital vectors from _SO FITS file
;
dfile = 'rsget'
rsgetpar,inputs,dfile,oparms

proc = strtrim(oparms(4),2)            ;processing format - US or MPE
proc = strupcase(proc)
if (proc eq 'MPE') then oparms(0) = oparms(0)+'_orbit'
obseq = strtrim(oparms(0),2)

dir = strtrim(oparms(1),2)
extyp = strtrim(oparms(2),2)
;
if (obseq eq '') then begin                      ;obseq must be defined
  read,'Please enter root name of input data file: ',obseq
  obseq = strtrim(obseq,2)
  oparms(0) = obseq
endif  
;if (extyp eq '') then begin
  if proc eq 'MPE' then extyp='tfits' else extyp = 'so'
  oparms(2) = extyp
;endif

;
if (npar lt 2) then obinum = 0            ; default is to read all OBIs
if (n_elements(obinum) eq 1) then obinum = obinum + intarr(1)
;
rsorb_pos,oparms,obinum,time,satpos,orbvecs,relpos,obinfo=obinfo,SCTTIME=SCTTIME
;
nobseq = strtrim(oparms(0),2)
if (obseq ne nobseq) then obseq = nobseq    ;obseq may have been corrected
ndir = strtrim(oparms(1),2)
if (dir ne ndir) then dir = ndir            ;dir may have been as well
nextyp = strtrim(oparms(2),2)
if (extyp ne nextyp) then extyp = nextyp    ;extyp may have been also
;
;hdrfil = dir+obseq+'_'+extyp+'.hhh'         ;file containing sequence header

;sxhread,hdrfil,hdr

;hdrfil = dir+obseq+'.'+extyp                 
;
hdrfil = dir+obseq+'.'+extyp   
if proc eq 'US' then $              
hdr = headfits(hdrfil)  else hdr = headfits(hdrfil,ext=1)  ;get hdr of FITS file directly
getnomasp,hdr,nomra,nomdec,nomroll,proc=proc
nomra = nomra/2./3600.                      ;nominal RA for entire seq
nomdec = nomdec/2./3600.                    ;convert to degrees

;
; Now calculate SES & EST angles. First we will need to convert the orbit
; times (time) to julian dates (djul)
;
yrbeg = obinfo.yrbeg 
yrend = obinfo.yrend
daybeg = obinfo.daybeg 
dayend = obinfo.dayend
ibeg = obinfo.ibeg 
iend = obinfo.iend
nobi = n_elements(ibeg)
;
djul = time*0.
for nn=0,nobi-1 do begin
  ib=ibeg(nn) 
  ie=iend(nn) 
  tsel = time(ib:ie)
  if (daybeg(nn) ne dayend(nn)) then begin      ;ie, if cross day boundary
    ipos = where( tsel(1:*) lt tsel) 
;    ipos = ipos(0)
;    tsel(ipos+1) = tsel(ipos+1:*) + 24.*3600.
	FOR K = 0, N_ELEMENTS(IPOS)-1 DO BEGIN ;if cross several days
    		IP=IPOS(K)                     ;added by wqd, July 10,92
    		TSEL(IP+1) = TSEL(IP+1:*) + 24.*3600.
    		SCTTIME(IP+1) = SCTTIME(IP+1:IE) + 24.*3600.
	ENDFOR
  endif

  yr = yrbeg(nn) + time*0.
  ydn2md,yrbeg(nn),daybeg(nn),mn,dy
  mn = mn + time*0. 
  dy = dy + time*0. 
  jdcnv2,yr,mn,dy,tsel/3600.,jdobi,red=1
  djul(ib) = jdobi
endfor
;
; Now convert civil times (reduced julian dates) to local mean sidereal 
; times (in hours)
;
ct2lst2,lmst,0,0,djul,red=1
theta = lmst*3.14159/12.          ;convert from hours to radians
;
; The orbital vectors sunx, ... sunz, satx, ... satz are given in a frame
; fixed on the Earth which rotates with the Earth. We need to convert
; these into a nonrotating frame of reference - sunx2, suny2, satx2, saty2.
; The angle for the conversion is just the local mean sideral time.
; The z coordinates will not change.
;
sunx = orbvecs.sunx 
suny = orbvecs.suny 
sunz = orbvecs.sunz
satx = orbvecs.satx 
saty = orbvecs.saty 
satz = orbvecs.satz
sunx2 = sunx*cos(theta) - suny*sin(theta)
suny2 = sunx*sin(theta) + suny*cos(theta)
satx2 = satx*cos(theta) - saty*sin(theta)
saty2 = satx*sin(theta) + saty*cos(theta)
;
diffx = sunx2 - satx2 
diffy = suny2 - saty2 
diffz = sunz - satz
dot = diffx*diffx + diffy*diffy + diffz*diffz       ;length of diff vector
indlt = where(dot lt 4.0)
indeq = where(dot eq 4.0)
sesang = dot*0.
if (indlt(0) ge 0) then $
   sesang(indlt) = 2.0*atan( sqrt(dot(indlt))/sqrt(4.-dot(indlt)) )  
if (indeq(0) ge 0) then sesang(indeq) = 3.14159     ;SES angle in radians
;
targx = time*0.
targy = targx 
targz = targx
cf = 3.14159/180.
for nn=0,nobi-1 do begin
;  nomra = nomasp(0,nn)/3600./2.         ;convert to degrees
;  nomdec = nomasp(1,nn)/3600./2.
  ib = ibeg(nn) 
  ie=iend(nn)
  targx(ib) = cos(cf*nomra)*cos(cf*nomdec) + fltarr(ie-ib+1)
  targy(ib) = sin(cf*nomra)*cos(cf*nomdec) + fltarr(ie-ib+1)
  targz(ib) = sin(cf*nomdec) + fltarr(ie-ib+1)
endfor
;
diffx = targx - satx2 
diffy = targy - saty2 
diffz = targz - satz
dot = diffx*diffx + diffy*diffy + diffz*diffz        ;length of diff vector
indlt = where(dot lt 4.0)
indeq = where(dot eq 4.0)
estang = dot*0.
if (indlt(0) ge 0) then $
   estang = 3.14159 - 2.0*atan( sqrt(dot(indlt))/sqrt(4.-dot(indlt)) ) 
if (indeq(0) ge 0) then estang(indeq) = 0.           ;EST angle in radians
;
vgcalc = sesang*0. 
sscode = sesang*0. 
epcode = estang*0.
sesang = sesang*180./3.14159        ;convert from radians to degrees
estang = estang*180./3.14159
;
; Use SES limits (seslim) to calculate numerical code for SES (sscode)
;
ind = where( sesang ge seslim(0) )
if (ind(0) ge 0) then sscode(ind) = 0
ind = where( (sesang ge seslim(1)) and (sesang lt seslim(0)) )
if (ind(0) ge 0) then sscode(ind) = 1
ind = where( (sesang ge seslim(2)) and (sesang lt seslim(1)) )
if (ind(0) ge 0) then sscode(ind) = 2
ind = where( sesang lt seslim(2) ) & 
if (ind(0) ge 0) then sscode(ind) = 3
;
; Use EST limits (estlim) to calculate numerical code for EST (epcode)
;
ind = where( estang ge estlim(0) )
if (ind(0) ge 0) then epcode(ind) = 0
ind = where( (estang ge estlim(1)) and (estang lt estlim(0)) ) 
if (ind(0) ge 0) then epcode(ind) = 1
ind = where( (estang ge estlim(2)) and (estang lt estlim(1)) )
if (ind(0) ge 0) then epcode(ind) = 2
ind = where( estang lt estlim(2) )
if (ind(0) ge 0) then epcode(ind) = 3
;
; Use SES & EST codes to calculate code for viewing geometry
;
ind = where( (sscode eq 0) and (epcode lt 3) )
if (ind(0) ge 0) then vgcalc(ind) = 1
ind = where( (sscode gt 0) and (epcode eq 0) )
if (ind(0) ge 0) then vgcalc(ind) = 2
ind = where( (sscode eq 1) and (epcode gt 0) )      ;except for epcode=3
if (ind(0) ge 0) then vgcalc(ind) = 3
ind = where( (sscode gt 1) and (epcode eq 1) )
if (ind(0) ge 0) then vgcalc(ind) = 3
ind = where( (sscode gt 1) and (epcode eq 2) )
if (ind(0) ge 0) then vgcalc(ind) = 4
ind = where( (epcode eq 3) )                        ;also fixes case above
if (ind(0) ge 0) then vgcalc(ind) = 5
;
npos = n_elements(time)
row = {viewgeom,lmst:0.0,sesang:0.0,estang:0.0,sscode:0,epcode:0,vgcalc:0}
if (npar ge 4) then begin
  vginfo = replicate(row,npos)
  vginfo.lmst = lmst
  vginfo.sesang = sesang
  vginfo.estang = estang
  vginfo.sscode = sscode
  vginfo.epcode = epcode
  vginfo.vgcalc = vgcalc
endif
;
return        ;pro rsgetvg
end
