;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   rsmake_prof
;*PURPOSE:
; A procedure to create a source radial profile from an events list structure
; Applies the mean exposure map correction, if desired.
;
;*CALLING SEQUENCE:
;   rsmake_prof,inputs,plist,plinfo,binang,radp,rprof,sigpr,listrad,$
;               mexmap=mexmap,oparms=oparms
;
;*PARAMETERS:
; INPUTS:
;   INPUTS - input parameter description.
;                0 - use all defaults
;                1 - interative selection of parameters
;                string - parameter defintion of the form
;                   'parname1=value1,parname2=value2,,,,'
;                string array - each element of the array has
;                   form 'value' (entries must be given in correct order)
;                string of form '$filename' where filename is the
;                   name of the file containing one parameter
;                   per line in the form: parname=value
;       Any parameter not specified is set to its default.
;       Defaults for the parameters can be found by using interactive
;       selection inputs=1 or examinining the default (text) file,
;       ZDEF:RSMAKE_PROF.DEF
;       The following parameters are available:
;
;       INSTR       Instrument (default is P for PSPC)
;       SREGION     String descriptor which gives x, y center to be used
;                   in constructing the profile.
;                   Note: for now, only circular regions allowed. 
;                   Only the X & Y center values are important for this
;                   procedure; the radius is ignored.
;       MEXFIL      File name for mean exposure map image (or NONE)
;                   (default is NONE)
;       BKG         Background count rate to be subtracted (counts/sq pixel)
;                   (default is zero)
;       PIMIN       Minimum PHI channel to be included 
;                   default = 0 means minimum PHI channel of list events
;       PIMAX       Maximum PHI channel to be included
;                   default = 0 means maximum PHI channel of list events
;       CHATTER     Controls program feedback to user ("chattiness")
;                   (default = 1)
;
;   PLIST    Structure of type xevent containing photon events (may have been 
;            filtered acc. to energy, time, etc.)
;   PLINFO   Structure of type xevinfo which contains info concerning extraction
;            and observation (total exposure time, in particular)
;   BINANG   Angular bins (in unblocked pixels) over which to bin the 
;            observed radial profile.
;            Give only the outer radii; a first value of zero will be
;            assumed
;
; OPTIONAL INPUTS:
;   MEXMAP   Mean exposure map, binned in approx. 15" pixels (e.g., made by
;            make_emap)
;            Specify this only if the mean exposure correction is to be
;            applied
;
; OUTPUTS:
;   RADP     The central radii of the angular bins given for the profile
;            (in arcsec)
;   RPROF    The radially averaged brightness profile, in counts/sq arcsec
;   SIGPR    Brightness profile uncertainties
;   LISTRAD  Vector of distances from source center position for all 
;            photons within given channel range 
;            Returned to save computation time in RSPROFILE
;   OPARMS   String array which is the parsed form of inputs (same as
;            LIST in example 3 below). Allows program to be rerun using 
;            same values w/o resetting.
;
;*EXAMPLES: There are 4 different ways that the inputs may be specified
;    mode 1) Interactive
;        IDL>rsmake_prof,1,plist,plinfo,binang,radp,rprof,sigpr,op=oparms
;             ?sregion=cir(7690.,7640.,250.)
;	      ?pimin=19
;             ?pimax=201
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='sregion=cir(7690.,7640.,250.),pimin=52,pimax=201'
;        IDL>rsmake_prof,list,plist,plinfo,binang,radp,rprof,sigpr,op=oparms
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,12)
;        IDL>list(5)='cir(7690.,7640.,250.)'
;        IDL>list(8)='52'
;        IDL>list(9)='201'
;        IDL>rsmake_prof,list,plist,plinfo,binang,radp,rprof,sigpr,op=oparms
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            sregion=cir(7690.,7640.,250.)
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;            *exit
;        IDL>rsmake_prof,'myinput.dat',plist,plinfo,binang,radp,rprof,sigpr,$
;            op=oparms
;
;*RESTRICTIONS:
;  If the mean exposure map is specified as a keyword (MEXMAP), then
;  the program assumes that it was blocked by a factor of 29.894656 
;  (e.g., as calculated by make_emap for the PSPC)
;  Otherwise, the blocking factor is computed from information given in the
;  FITS header
;
;   So far, only circular regions are allowed for constructing the profile.
;   (The value for the radius will be ignored)
;   Source region descriptor must be given in UNBLOCKED pixels
;
;*NOTES:
;
;*SUBROUTINES CALLED:
;  PARSE_SIMPLE
;  LIMIT_MEX
;
;*MODIFICATION HISTORY:
;    written 17 Nov 1993 (GAR)
;-
;-------------------------------------------------------------------------------
pro rsmake_prof,inputs,plist,plinfo,binang,radp,rprof,sigpr,listrad,$
                mexmap=mexmap,oparms=oparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RSMAKE_PROF,inputs,plist,plinfo,binang,RADP,RPROF,SIGPR,LISTRAD,'
  print,'             mexmap=mexmap,OPARMS=OPARMS'
  retall
endif
;
dfile = 'rsmake_prof'
rsgetpar,inputs,dfile,oparms
chatter = fix(oparms(11))
;
instr = strupcase(strtrim(oparms(3),2))
instruc = strupcase(instr)
if ((instruc ne 'H') and (instruc ne 'P')) then begin
  print,' Instrument ',instr,' not allowed. Must be H or P.'
  print,' Please check your inputs. Returning.'
  retall
endif
;
; Specifying a mean exposure map using both keyword MEXMAP and input
; parameter MEXFIL is not allowed
;
mexfil = strtrim(oparms(6),2)
mexcor = (mexfil ne 'NONE') + (n_elements(mexmap) ne 0)
if (mexcor gt 1) then begin
  print,' You may not enter an exposure map as both a keyword (MEXMAP)'
  print,' and an input parameter (MEXFIL)'
  print,' Please check your inputs. Returning.'
  retall
endif
;
; Mean exposure map correction can only be applied for PSPC. Check this.
;
if ((instruc ne 'P') and (mexcor ne 0)) then begin
  print,' Exposure map correction can only be applied for PSPC.'
  print,' Please check your inputs. Returning.'
  retall
endif
;
mexbin = 14.947328  + 0.D0           ;make_emap value for PSPC assumed
if (mexfil ne 'NONE') then begin     ;read exposure map from specified file
  mexmap = readfits(mexfil,emaphdr)
  mexbin = fxpar(emaphdr,'pixdetx')*fxpar(emaphdr,'blkfact')*3600.
endif 
bkg = float(oparms(7))
;
; Parse the source region descriptor to get the center and radius. Remember,
; only circular source regions are allowed
;
sregion = strtrim(oparms(5),2)
parse_simple,sregion,regtyp,args,incl
if (regtyp ne 'circle') then begin
  print,' Specified source region is not a circle. Please check your inputs.'
  print,' Returning.'
  retall
endif
xcen = args(0)
ycen = args(1)
pixsiz = plinfo.skypixsiz
;
; If zeroes are given for XCEN and/or YCEN, then give the center of the
; photon list as defined by the structure variable PLINFO
; If that happens, then redefine the corresponding value in OPARMS
;
if (xcen eq 0) then xcen = (plinfo.xmin + plinfo.xmax)/2.
if (ycen eq 0) then ycen = (plinfo.ymin + plinfo.ymax)/2.
if ( (xcen ne args(0)) or (ycen ne args(1)) ) then begin
  sregion = 'circle(' + strtrim(string(xcen),2) + ','
  sregion = sregion + strtrim(string(ycen),2) + ','
  sregion = sregion + strtrim(string(args(2)),2) + ')'
  oparms(5) = sregion
endif
if (chatter ge 1) then print,' Source center ',xcen,ycen
;
pimin = fix(oparms(8))
pimax = fix(oparms(9))
pisel = 0
if ((pimin ne 0) or (pimax ne 0)) then pisel = 1  ;will filter by pi channel
;
; Calculate photon distances from x,y center.
; Filter photon list by channel range, if desired (pisel = 1).
;
if (pisel) then begin
  isel = where((plist.pi ge pimin) and (plist.pi le pimax),nsel)
  if (nsel le 0) then begin
    print,' No list photons within allowed channel bounds. Returning.'
    retall 
  endif
  listrad = sqrt( (plist(isel).x-xcen)^2 + (plist(isel).y-ycen)^2)
endif else listrad = sqrt( (plist.x-xcen)^2 + (plist.y-ycen)^2)
;
; The mean exposure map correction will be applied photon by photon
; (if desired). First, get the correction factors
;
if (mexcor eq 1) then begin
  thresh = 0.05          ;default value from mexdiv
  time = plinfo.totexp
  limit_mex,mexmap,time,thresh,corrfacs
; 
; Find the exposure map bin for each photon. Make an array with as many
; elements as the list, and fill in the correction factors element by 
; element
;
  mexblk = mexbin/pixsiz
  ixbin = fix(plist.x/mexblk) 
  iybin = fix(plist.y/mexblk)
  nlist = n_elements(plist)
  cfacs = fltarr(nlist)
  for ii=0,nlist-1 do cfacs(ii) = corrfacs(ixbin(ii),iybin(ii))
endif
corrfacs = 0             ;to save memory
;
; Bin the source counts over the specified angular bins and compute
; radially averaged profile
;
nbin = n_elements(binang)
prof = fltarr(nbin)
for ii=0,nbin-1 do begin
  jj = where(listrad le binang(ii),nrad)
  if (mexcor eq 0) then prof(ii) = nrad else $
     prof(ii) = total(cfacs(jj))   ;sum correction factors for these photons
endfor
;
radp = [binang(0)/2.,(binang(1:*)+binang)/2.]*pixsiz
areas = !pi*[binang(0)*binang(0),binang(1:*)*binang(1:*)-binang*binang]
bkcounts = bkg*areas            ;bkg given in counts/sq pixel
areas = float(areas*pixsiz*pixsiz)
rprof = [prof(0),(prof(1:*)-prof)]
sigpr = sqrt(rprof + bkcounts)
rprof = (rprof-bkcounts)/areas
sigpr = sigpr/areas
;
return
end            ;rsmake_prof
