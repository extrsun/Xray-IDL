;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   rsget_emap_data
;*PURPOSE:
; A procedure to collect the aspect and livetime data needed to create
; an exposure map
;
;*CALLING SEQUENCE:
;   rsget_emap_data,inputs,sctcas,roll,delx,dely,flive,isegs,$
;                   actime=actime,deadtp=deadtp,a1llmin=a1llmin,oparms=oparms
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
;                    (default is CAS)
;        INSTR       Instrument 
;                    (default is P for PSPC)
;        PROC        Format of processed files (e.g., US, MPE)
;                    (default is 'US')
;        TRIM        Determines whether or not data is trimmed to Good
;                    Time Intervals (default = 1)
;        CHATTER     Controls program feedback to user
;                    (default = 1)
;        READORB     Controls whether to read orbit data or not
;                    (default = 'N')
;
;   
; OPTIONAL INPUTS:
;   ACTIME  - array containing start and stop times of accepted time intervals
;             actime(*,0) = start times, actime(*,1) = stop times
;             if not specified, then the start and stop times of the good 
;             time intervals are assumed
;             these intervals are redefined within the procedure to include 
;             only those times for which A1LL > A1LLMIN
;   DEADTP  - Dead time parameter (musec, according to the recipe in the
;             TN-ROS-ME-ZA00-025) (default = 234.)
;   A1LLMIN - Minimum value allowed for A1LL lower level rate (default = 30.)
;
; OUTPUTS:
;   SCTCAS - Times of aspect measurement
;   ROLL   - Roll angles (in degrees)
;   DELX   - x translations in NS system (in units of 0.5 arcsec)
;            = change in pointing RA relative to nominal (in pixels)
;   DELY   - y translations in NS system (in units of 0.5 arcsec)
;            = change in pointing Dec relative to nominal (in pixels)
;   FLIVE  -  Vector containing PSPC livetime factor
;   ISEGS  - 2 dimensional array containing indices of beginnings and
;            ends of contiguous segments of aspect data
;            isegs(*,0) = start indices, isegs(*,1) = stop indices
;   OPARMS - the internally parsed form of INPUTS (same form as list
;            in example 3 below)
;
;*EXAMPLES: There are 4 different ways that the inputs may be specified
;    mode 1) Interactive
;        IDL>rsget_emap_data,1,oparms=oparms
;             ?spfil=iraf/rp123456_obs
;             ?sptyp=pros
;             ?mapfil=iraf/rp123456_psf.fits
;             ?proftab=iraf/rp123456_prof.tab
;             ?xcen=250
;             ?ycen=250
;             ?pixel=bl
;             ?block=30
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='spfil=NONE,mapfil=iraf/rp123456_psf.fits'
;        IDL>rsget_emap_data,list,oparms=oparms,rate=rate,group=group
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,25)
;        IDL>list(4)='iraf/rp123456_src1.pha'
;        IDL>list(5)='PHA'
;        IDL>list(6)='iraf/rp123456_psf'
;        IDL>rsget_emap_data,list,oparms=oparms,rate=rate,ecen=ecen
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            spfil=rp123456_src1.plot
;            mapfil=NONE
;            proftab=rp123456_prof.tab
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;            xcen=7700
;            ycen=7640
;            *exit
;        IDL>rsget_emap_data,list,oparms=oparms
;
;*RESTRICTIONS:
;    So far, works only for PSPC
;
;*NOTES:
;  Called by make_emap (replaces const_exp_data in const_exp_map)
;
;  A bug was found in rsgetasp which sometimes caused the aspect times to
;      look strange (like series of duplicate values). This was due to
;      the data type of the fractional seconds in the FITS file, which
;      was only used for PSPC files processed in the US. This bug has now
;      been fixed. (24 Oct 1992 GAR)
;
;   The accepted time intervals are redefined within the procedure to include 
;      only those times for which the A1LL rate is above the minimum allowed
;      value
;
;*SUBROUTINES CALLED:
;  RSGETPAR
;  RSGETDEFPAR
;  RSGTIMES
;  MPE_GTIMES
;  US_GTIMES
;  MATCH_FILES
;  READFITS
;
;*MODIFICATION HISTORY:
;    adapted from const_exp_data.pro, written by J. A. Mendenhall (Penn State)
;    const_exp_maps.pro and subroutines were adapted from Fortran code,
;      written April 1992 by Steve Snowden and G. R. Hasinger, (MPE) 
;    modified 03 Sep 1992 (GAR) for inclusion in Rosat IDL Library
;    modified 24 Oct 1992 (GAR) to redefine accepted time intervals to include
;      only times with allowed A1LL rates, and to remove "fix" introduced
;      due to bug in rsgetasp
;    modified 06 Feb 1993 (GAR) to fix fill-in logic when a good data segment
;      (sctcas & sctevr both within an accepted time interval, and a1rate
;      above a1llmin) contains noncontiguous aspect segments, and to return
;      correct segments array isegs
;    modified 10 Aug 1993 (GAR) to work with MPE format (PSPC) data
;    modified 13 Aug 1993 (GAR) to use RSGTIMES to read GTIs
;    modified 23 Sep 1993 (GAR) to clean things up a bit
;    modified 28 Dec 1993 (GAR) to be compatible with new versions of
;      RSGETEVR and RSGETASP that also read data from RDF format files
;-
;-------------------------------------------------------------------------------
pro rsget_emap_data,inputs,sctcas,roll,delx,dely,flive,isegs,actime=actime,$
                    deadtp=deadtp,a1llmin=a1llmin,oparms=oparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' RSGET_EMAP_DATA, inputs, SCTCAS, ROLL, DELX, DELY, FLIVE, ISEGS,'
  print,'                  actime=actime, deadtp=deadtp, a1llmin=a1llmin,'
  print,'                  OPARMS=OPARMS'
  print,'   Uses inputs OBSEQ, DIR, EXTYP, INSTR, PROC, and CHATTER ' $
       +'from RSGET.DEF'
  return
endif
if (n_elements(deadtp) eq 0) then deadtp = 234.      ;default is 234.
if (n_elements(a1llmin) eq 0) then a1llmin = 30.     ;default is 30.
;
dfile = 'rsget'
rsgetpar,inputs,dfile,oparms
obseq = strtrim(oparms(0),2)
dir = strtrim(oparms(1),2)
extyp = strtrim(oparms(2),2)
instr = strtrim(oparms(3),2)
proc = strtrim(oparms(4),2)            ;processing format - US or MPE
proc = strupcase(proc)
chatter = fix(oparms(9))
;
if (obseq eq '') then begin                      ;obseq must be defined
  read,'Please enter root name of input data file: ',obseq
  obseq = strtrim(obseq,2)
  oparms(0) = obseq
endif  
;
; Read good time intervals, if actime is not defined
;
if (n_elements(actime) eq 0) then begin      ;time intervals not defined
  rsgtimes,obseq,actbeg,actend,nact,dir=dir,proc=proc
  oparms(0) = strtrim(obseq,2)        ;obseq & dir may have been corrected
  oparms(1) = strtrim(dir,2)
  actime = dblarr(nact,2)
  actime(0) = actbeg
  actime(nact) = actend
endif else begin
  actbeg = actime(*,0)
  actend = actime(*,1)
  nact = n_elements(actbeg)
endelse
;
; First, read in event rates data and calculate livetime factor
;
rsgetevr,oparms,0,sctevr,rates
rslivetime,rates,flivevr,ierr,deadtp=deadtp
a1rate = rates.a1l                  ;will need this later to select times
rates = 0
;
; Now, read in aspect data. Sometimes, entries in aspect file are
;   duplicated (every two consecutive times are equal).
; Fix duplicate time entries in aspect data file (assume first is
;   correct, and then add 1.0 to the next. The aspect info is not
;   duplicated, just the time entries.)
;
rsgetasp,oparms,0,sctcas,roll,delx,dely,nomasp,obinfo=obinfo
;
; Now find times for which aspect and event rates data are within accepted
; time intervals
; Also reject times where a1ll le 30 (another condition in const_exp_maps.pro)
; Finally, fill in vector of live time factors (section by section) to
;   match aspect times
; Do not interpolate the livetime factors (for now). Also remember that the
;   measurements are given at the *end* of each time interval, not at the 
;   beginning
;
ibeg = [-1]
indsel = long(sctcas*0)
flive = sctcas*0.
nct = 0
for ii=0,nact-1 do begin
  a1temp = 0
  flivtmp = 0
  ind1 = 0
  ind2 = 0
;
  ind1 = where( (sctcas ge actbeg(ii)) and (sctcas le actend(ii)), ngood1 )
  ind2 = where( (sctevr ge actbeg(ii)) and (sctevr le actend(ii)), ngood2 )
;
  if ( (ngood1 gt 0) and (ngood2 gt 0) ) then begin
;    linterp,sctevr(ind2),a1rate(ind2),sctcas(ind1),a1temp
;    linterp,sctevr(ind2),flivevr(ind2),sctcas(ind1),flivtmp
    tabinv,sctevr(ind2),sctcas(ind1),ifill
    ifill = fix(ifill + 1)
    a1temp = a1rate(ind2(ifill))
    flivtmp = flivevr(ind2(ifill))
;
    ind2 = 0
    ind2 = where(a1temp gt a1llmin,ngood2)
    if (ngood2 gt 0) then begin
      indsel(nct) = ind1(ind2)      ;we're only going to save aspect data
      flive(nct) = flivtmp(ind2)
      ibeg = [ibeg,nct]             ;beginnings of contiguous sections sctcas
      nct = nct + n_elements(ind1(ind2))
    endif
  endif
endfor
;
if (nct ge 1) then begin
  indsel = indsel(0:nct-1)        ;truncate to nonzero values
  flive = flive(0:nct-1)
  ibeg = ibeg(1:*)                ;strip off bogus first value
  if (nact gt 1) then iend = [ibeg(1:*)-1,nct-1] else iend = [nct-1]
  sctcas = sctcas(indsel)
  roll = roll(indsel)
  delx = delx(indsel)
  dely = dely(indsel)
endif else begin
  print,' No aspect times were within accepted time intervals! Returning.'
  retall
endelse
indsel = 0
;
; Get the new accepted time intervals for this segment
; Remember that the aspect measurements are given at the *ends* of the
;   aspect time intervals. 
; Assume that the first time interval is 1.0 sec, and adjust beginnings
;   of accepted time intervals so that actend - actbeg = total time in each
;   interval
;
nsegs = n_elements(ibeg)
newtbeg = dblarr(1) - 999.
newtend = newtbeg
newibeg = lonarr(1) - 999
newiend = newibeg
for ii=0,nsegs-1 do begin 
  loc = [sctcas(ibeg(ii):iend(ii)),0]
  loc = where ((loc(1:*)-loc) gt 1.0,nloc)
  if (nloc eq 0) then begin               ;all aspect times are contiguous
    kbeg = ibeg(ii)
    kend = iend(ii)
  endif else begin
    kbeg = [ibeg(ii),ibeg(ii)+loc+1]
    kend = [ibeg(ii)+loc(0:nloc-1),iend(ii)]
  endelse
  newtbeg = [newtbeg,sctcas(kbeg)-1.]
  newtend = [newtend,sctcas(kend)]
  newibeg = [newibeg,kbeg]
  newiend = [newiend,kend]
endfor
newtbeg=newtbeg(1:*)
newtend=newtend(1:*)
newibeg=newibeg(1:*)
newiend=newiend(1:*)
ntimes = n_elements(newtbeg)
actime = dblarr(ntimes,2)
actime(0) = newtbeg
actime(ntimes) = newtend
;
nsegs = n_elements(newibeg)
isegs = lonarr(nsegs,2)
isegs(0) = newibeg
isegs(nsegs) = newiend
flivevr = 0
sctevr = 0
a1rate = 0
;
return
end        ;pro rsget_emap_data
