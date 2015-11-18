;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   rsprofile
;*PURPOSE:
; A procedure to create a source radial profile from an events list structure
; and to compare this with the PSF calculated for the appropriate spectrum
; Applies the mean exposure map correction, if desired.
;
;*CALLING SEQUENCE:
;   rsprofile,inputs,plist,plinfo,binang,radp,rprof,sigpr,psf,chisq,$
;             mexmap=mexmap,rate=rate,group=group,oparms=oparms
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
;                   in constructing the profile and (for the PSPC)
;                   extracting the spectrum to be used in computing the
;                   PSF, if desired.
;                   Note: for now, only circular regions allowed. 
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
;            spatially filtered (using pfilt), temporally filtered (using tfilt)
;            etc.
;   PLINFO   Structure of type xevinfo which contains info concerning extraction
;            and observation (total exposure time, in particular)
;   BINANG   Angular bins (in unblocked pixels) over which to bin the 
;            observed radial profile.
;            Give only the outer radii; a first value of zero will be
;            assumed
;
; OPTIONAL INPUTS:
;   RATE     Spectrum to be used for PSF calculation. If not defined, will
;            be extracted over region defined by SREGION
;   GROUP    2d array giving beginning and ending pi channel boundaries
;            (default = boundaries given by !group)
;   CHATTER  Controls program feedback to user (default = 1)
;
; OUTPUTS:
;   RADP     The central radii of the angular bins given for the profile
;            (in arcsec)
;   RPROF    The radially averaged brightness profile, in counts/sq arcsec
;   SIGPR    Brightness profile uncertainties
;   PSF      The radially averaged point spread function, normalized
;            to best match the observed profile
;   CHISQ    Chi-square between RPROF and PSF
;   OPARMS   String array which is the parsed form of inputs (same as
;            LIST in example 3 below). Allows program to be rerun using 
;            same values w/o resetting.
;
;*EXAMPLES: There are 4 different ways that the inputs may be specified
;    mode 1) Interactive
;        IDL>rsprofile,1,plist,plinfo,binang,radp,rprof,sigpr,psf,chisq,$
;            gr=group,op=oparms
;             ?sregion=cir(7690.,7640.,250.)
;	      ?pimin=19
;             ?pimax=201
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='sregion=cir(7690.,7640.,250.),pimin=52,pimax=201'
;        IDL>rsprofile,list,plist,plinfo,binang,radp,rprof,sigpr,psf,chisq,$
;            gr=group,op=oparms
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,12)
;        IDL>list(5)='cir(7690.,7640.,250.)'
;        IDL>list(8)='52'
;        IDL>list(9)='201'
;        IDL>rsprofile,list,plist,plinfo,binang,radp,rprof,sigpr,psf,chisq,$
;            gr=group,op=oparms
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            sregion=cir(7690.,7640.,250.)
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;            *exit
;        IDL>rsprofile,'myinput.dat',plist,plinfo,binang,radp,rprof,sigpr,$
;            psf,chisq,gr=group,op=oparms
;
;*RESTRICTIONS:
;   So far, only circular regions are allowed for deriving the source
;   spectrum
;   Source region descriptor must be given in UNBLOCKED pixels
;
;*NOTES:
;    For the PSPC, only the spectrum over energies in the range 0.15 to
;      2.0 keV will be used to compute the PSF
;
;*SUBROUTINES CALLED:
;  RSMAKE_PROF
;  PARSE_SIMPLE
;  LIMIT_MEX
;  SET_DEFCEN
;  LIMIT_GROUP
;  CHAN2ENERGY
;  MAKE_SPEC
;  RSPSFP
;  RSPSFH
;  SET_PSYM
;
;*MODIFICATION HISTORY:
;    written 17 Nov 1993 (GAR)
;-
;-------------------------------------------------------------------------------
pro rsprofile,inputs,plist,plinfo,binang,radp,rprof,sigpr,psf,chisq,$
              mexmap=mexmap,rate=rate,group=group,oparms=oparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RSPROFILE,inputs,plist,plinfo,binang,radp,rprof,sigpr,psf,chisq,'
  print,'           mexmap=mexmap,rate=rate,group=group,oparms=oparms'
  retall
endif
if (n_elements(group) eq 0) then group = !group     ;use !group as default
getspec = 0
if (n_elements(rate) eq 0) then getspec = 1      ;need to accumulate spectrum
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
oparms2 = oparms
pimin = fix(oparms(8))
pimax = fix(oparms(9))
pibot = min(plist.pi)
pitop = max(plist.pi)
if (pimin eq 0) then begin
  pimin = pibot
  oparms2(8) = strtrim(string(pimin),2)
endif
if (pimax eq 0) then begin
  pimax = pitop
  oparms2(9) = strtrim(string(pimax),2)
endif
;
; If PSPC, check the grouping vector to make sure that some energies 
; are between 0.15 and 2.0 keV. Only these energies will be included
; in the PSF calculation.
;
if (instruc eq 'P') then begin
  limit_group,group,ngroup,pilim=[pimin,pimax]
  limit_group,ngroup*1.,ngroup,elim=[0.07,2.0]
  chanbot = min(ngroup)
  chantop = max(ngroup)
  if (chatter eq 1) then $
     print,' Restricting channels to range ',chanbot,' to ',chantop
endif
;
; Reset the minimum and maximum pi limits in oparms2 to the values found
; in LIMIT_GROUP. This will ensure that the profile and spectrum are
; accumulated over the same channel ranges
;
if (pimin lt chanbot) then begin
  pimin = chanbot
  oparms2(8) = strtrim(string(pimin),2)
  if (chatter eq 1) then $
     print,'  Resetting lower channel limit from ',pimin, ' to ',chanbot
endif
if (pimax gt chantop) then begin
  pimax = chantop
  oparms2(9) = strtrim(string(pimax),2)
  if (chatter eq 1) then $
     print,'  Resetting upper channel limit from ',pimax, ' to ',chantop
endif
pisel = 0
if ( (pimin ne pibot) or (pimax ne pitop) ) then pisel = 1
;
; Accumulate the source radial profile
;
rsmake_prof,oparms2,plist,plinfo,binang,radp,rprof,sigpr,listrad,mexmap=mexmap
;
; Parse the source region descriptor to get the center and radius. Remember,
; only circular source regions are allowed
;
sregion = strtrim(oparms(5),2)
parse_simple,sregion,regtyp,args,incl
xcen = args(0)
ycen = args(1)
if (xcen eq 0) then xcen = (plinfo.xmin + plinfo.xmax)/2.
if (ycen eq 0) then ycen = (plinfo.ymin + plinfo.ymax)/2.
srad = args(2)
bkg = float(oparms(7))
;
; Calculate the off axis angle for the source center
;
ctyp = 'SKY'
set_defcen,instr,ctyp,defxcen,defycen,pixsiz
offcen = sqrt( (xcen-defxcen)^2 + (ycen-defycen)^2 )*pixsiz/60.  ;in arcmin
if (chatter eq 1) then print,'        Off axis angle = ',offcen,' arcmin'
;
; If PSPC, accumulate the spectrum, if desired (keyword RATE not specified 
; and/or defined)
; LISTRAD is returned by RSMAKE_PROF
;
if ( (instruc eq 'P') and (getspec) ) then begin
  isel = where(listrad le srad,nsel)
  if (nsel le 0) then begin
    print,' No photons within source extraction region. Returning.'
    retall
  endif else make_spec,plist(isel),plinfo,rate,sigr,group=ngroup
endif
;
; Compute expected PSF. Use program RSPSFPROF
;
rspsfprof,instr,offcen,radp,psf,rate=rate,group=ngroup
;  
; Compute the normalization of the PSF to best match the observed profile
;
sum1 = total(rprof*psf/sigpr/sigpr)
sum2 = total(psf*psf/sigpr/sigpr)
mult = sum1/sum2
psf = mult*psf
chisq = total( (rprof-psf)*(rprof-psf)/sigpr/sigpr)
;
set_psym,2,1
nrad = n_elements(radp)
plot,radp,rprof,psym=8,tit='Radially averaged profile',$
     xtit='Radius (arcsec)',ytit='Counts/sq arcsec'
for ii=0,nrad-1 do oplot,radp(ii)+[0,0],rprof(ii)+sigpr(ii)*[-1.,1.]
oplot,radp,psf,line=2
;
return
end            ;rsprofile
