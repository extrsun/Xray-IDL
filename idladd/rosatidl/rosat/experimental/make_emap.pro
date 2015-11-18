;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;            make_emap
;
;*PURPOSE:
; A procedure to generate energy dependent exposure maps for Rosat pointed 
; observations.
;
;*CALLING SEQUENCE:
;	make_emap,inputs,expmap,totexp,emaphdr,actime=actime,bands=bands,$
;                 imapdir=imapdir,oparms=oparms
;
;*PARAMETERS:
; INPUTS:
;       inputs - input parameter description.
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
;       ZDEF:MAKE_EMAP.DEF
;       The following parameters are availble.
;
;        OBSEQ       Root name of input data file 
;                    (null string not allowed, must be specified)
;        DIR         Directory containing input file 
;                    (default is current directory)
;        INSTR       Instrument 
;                    (default is P for PSPC)
;        PROC        Processing format of input files (e.g., US, MPE)
;                    (default is 'US')
;        ACTFIL      Name of file containing time intervals to be accepted
;                    (default is NONE)
;        BAND        Number of channel band of instrument map
;                    Choices for PSPC are 1 (channels 8-19), 2 (20-41), 
;                    3 (42-51), 4 (52-69), 5 (70-90), 6 (91-131),
;                    7 (132-201), 8 (11-19), 9 (8-41), 10 (52-90),
;                    11 (91-201), 12 (42-131), 13 (42-201)
;        MAPFIL      (Full) name of output mean exposure map image (or NONE)
;                    (default extension is '_emap'+band+'.fits')
;        CHATTER     Controls program feedback to user ("chattiness")
;                    (default = 1)
;
; OPTIONAL INPUTS:
;        ACTIME -  array containing start and stop times of accepted time 
;                  intervals
;                  actimes(0,*) = start times, actimes(*,1) = stop times
;                  if not specified, and input ACTFIL = 'NONE', then the
;                  start and stop times of the good time intervals are assumed
;        BANDS  -  vector containing numbers of channel bands over which
;                  to calculate exposure map
;                  allows user to calculate exposure map for >1 band for
;                  same image and accepted time intervals
;                  if BANDS is specified, then input BAND is ignored, and
;                  the maps are written to mapfil+'_emap_'+band+'.fits'
;        IMAPDIR - directory containing detector maps ('' for default)
;
; OUTPUTS:
;        EXPMAP  - the two dimensional image of the exposure map (in sec)
;        TOTEXP  - the total exposure time (in seconds)
;        OPARMS  - the internally parsed form of INPUTS (same form as list
;                  in example 3 below)
;
;*EXAMPLES: There are 4 different ways that the inputs may be specified
;    mode 1) Interactive
;        IDL>make_emap,1,oparms=oparms
;             ?obseq=rp123456
;             ?dir=iraf/
;             ?actfil=iraf/rp123456_gti.tab
;             ?band=3
;             ?mapfil=test
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='obseq=rp123456,dir=iraf/,actfil=none,mapfil=none'
;        IDL>band=indgen(7)+1
;        IDL>make_emap,list,expmap,totexp,band=band,actime=actime,oparms=oparms
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,25)
;        IDL>list(0)='rp123456'
;        IDL>list(1)='iraf/'
;        IDL>list(5)='iraf/rp123456_gti.tab'
;        IDL>list(6)=3
;        IDL>list(7)='test'
;        IDL>make_emap,list,oparms=oparms
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            obseq=rp123456
;            dir=iraf/
;            actfil=NONE
;            mapfil=test
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;            chatter=0
;            *exit
;        IDL>make_emap,list,actime=actime,oparms=oparms
;
;*RESTRICTIONS:
;    So far, works only for PSPC
;    If the input spectral file ACTFIL = 'NONE', and keyword ACTIME
;       is not entered on the command line, then the good time intervals will
;       be used.
;
;*NOTES:
;	** THIS PROGRAM IS AN IDL TRANSLATION OF THE ORIGINAL 
;	VAX FORTRAN PROGRAM CAST_EXP CREATED BY STEVE SNOWDEN OF 
;	MPE. **
;		- jeffrey a. mendenhall
;
; This program is used to cast the band-correct exposure map for pointed
; observations.  It uses detector maps created for various PI bands from
; survey data. Initially, the detector maps are read along with the aspect
; pointing and events rates history data for the particular pointed
; observation. Live time fractions are then calculated based on event rates.
; The aspect roll angles and X and Y aspect offsets are then quantized (in
; units of 74.7366 arc sec and 14.94733 arc sec, respectively) and the unique
; values of roll angle and offset dwell point are found. For each unique roll
; angle and offset dwell point, the detector map is rotated, shifted in
; position, weighted by the cumulative live time at that dwell point, and
; added to a cumulative sum. The final cumulative sum is returned as the
; mean exposure map.
;
; The accepted times can be those given by the good time intervals table 
; in the observation data set. However, Steve Snowden suggests verifying 
; the good time intervals by hand.
;
; The program follows the suggestions of Snowden et al. (1992, ApJ, 
; 393 819) and Plucinsky et al. (1993, ApJ, in press) to exclude
; regions of the PSPC near the edges of the PSPC which are strongly
; affected by the particle background, the "bright line" regions.
; The program also assumes that a selection has been done on the
; data to exclude all events which follow within 0.5 ms of a 
; "precursor" event.  This excludes some of the low pulse-height
; crud which affects data collected after May 1992.
;
;  A bug was found in rsgetasp which sometimes caused the aspect times to
;      look strange (like series of duplicate values). This was due to
;      the data type of the fractional seconds in the FITS file, which
;      was only used for PSPC files processed in the US. This bug has now
;      been fixed. (24 Oct 1992 GAR)
;
;  Nonlocal users may wish to edit the procedure file defimap.pro in order
;  to redefine the default detector map directory &/or the names of the 
;  detector map files
;
;*SUBROUTINES CALLED:
;  DEFIMAP
;  RSGETPAR
;  RSGETDEFPAR
;  MATCH_FILES
;  RSOBSDATE
;  GETHISTVAL
;  RSGET_EMAP_DATA
;  RSGTIMES
;  MPE_GTIMES
;  US_GTIMES
;  RSGETEVR
;  RS_SEQINFO
;  MPE_SEQINFO
;  US_SEQINFO
;  RS_NOMASP
;  RSOBITIMES
;  RSLIVETIME
;  RSGETASP
;  MAKE_EMAP_HDR
;  VSORT_LARGE
;  VSORT
;  ROT
;  READFITS
;  FXADDPAR
;  WRITEFITS
;
;*MODIFICATION HISTORY:
;    adapted from const_exp_maps.pro, written by J. A. Mendenhall (Penn State)
;    const_exp_maps.pro and subroutines were adapted from Fortran code,
;      written April 1992 by Steve Snowden (MPE) 
;    modified 05 Sep 1992 (GAR) for inclusion in Rosat IDL Library
;    modified 18 Sep 1992 (GAR) to fix bug in defining unique roll angles
;    modified 20 Sep 1992 (GAR) to add total exposure time to FITS header
;      of each output file, and to return last FITS header
;    modified 17 Feb 1993 (GAR) to comment out line which shifts centered
;      instrument map (suggested by Qingde Wang), and update list of
;      instrument maps and scaling factors provided by Jeff Mendenhall
;    modified 22 Feb 1993 (GAR) for change in default directory on Vaxes 
;      and to distinguish between VMS and UNIX when summing exposure map 
;    modified 06 Aug 1993 (GAR) to use subroutine DEFIMAP to set up the
;      vectors for the instrument map file names and the scale factors
;    modified 07 Aug 1993 (GAR) to fix logic in defining actime and to
;      add print statements suggested by J. Mendenhall, and to allow 
;      the user to shift the map in x & y by adding keyword shift to the
;      call statement
;    modified 14 Sep 1993 (GAR) to imbed detector matrix in a 513 by 513
;      array before rotation, so that ROT will work properly; also to
;      remove user ability to shift map in x & y as this is no longer needed
;    modified 23 Sep 1993 (GAR) to use CALDB detector maps which contain the
;      scale factor as a keyword, and to include more comment lines from
;      Steve Snowden's original program
;    modified 28 Sep 1993 (GAR) to add information to FITS header created
;      by new make_emap_hdr (and to recreate this for every band)
;    modified 30 Sep 1993 (GAR) to add keyword IMAPDIR to allow instrument
;      maps to be read from different directories
;    modified 03 Oct 1993 (GAR) to make a FITS header and update it even if
;      no output file is written
;    modified 16 Nov 1993 (GAR) to update choices for PSPC bands in
;      documentation prologue, to fix bug when accepted time intervals
;      file contains one (or no) time intervals, and to delete the ';*'
;      from ext = '.fits;*' when using match_files to search for the file
;    modified 30 Dec 1993 (GAR) to work with new version of RSGET_EMAP_DATA
;      and RDF format files, and to print '% done' in casting loop when
;      CHATTER ge 2
;-
;-------------------------------------------------------------------------------
pro make_emap,inputs,expmap,totexp,emaphdr,actime=actime,bands=bands,$
    imapdir=imapdir,oparms=oparms
;
;  get instrument map info
;
if (n_elements(imapdir) eq 0) then imapdir = ''
defimap,instmap,nimap,chans,sufx,chatter=0,dir=imapdir
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' MAKE_EMAP, inputs, EXPMAP, TOTEXP, EMAPHDR, actime=actime, '
  print,'            bands=bands, imapdir=imapdir, OPARMS=OPARMS'
  print,'   '
  print,'   Uses inputs OBSEQ, DIR, INSTR, PROC, ACTFIL, BAND, MAPFIL, '
  print,'               and CHATTER from ZDEF:make_psf.def'
  print,'   '
  print,'   If ACTFIL = NONE, then accepted time intervals must be' $
       +' entered as keyword'
  print,'   '
  print,'   For PSPC, choices for BAND are channels'
  for ii=0,nimap-1,7 do begin
    iend = (nimap-1) < (ii+6)
    print,'      '
    print,f='$(i7,6i10)',ii+indgen(iend-ii+1)+1
    print,f='$(7a10)',chans(ii:iend)
  endfor
  print,'   '
  print,'   If keyword BANDS is specified, then input BAND is ignored'
  print,'   '
  retall
endif
;
dfile = 'make_emap'
rsgetpar,inputs,dfile,oparms
obseq = strtrim(oparms(0),2)
dir = strtrim(oparms(1),2)
instr = strupcase( strmid(oparms(3),0,1) )
if (instr ne 'P') then begin
  print,' Sorry, as yet procedure only works for PSPC. Returning.'
  retall
endif
;
proc = strtrim(oparms(4),2)            ;processing format - US, MPE, or RDF
proc = strupcase(proc)
chatter = fix(oparms(9))
;
if ( (obseq eq '') and (proc eq 'US') ) then begin   ;obseq must be defined
  read,'Please enter root name of input data file: ',obseq
  obseq = strtrim(obseq,2)
  oparms(0) = obseq
endif  
;
actfil = strtrim(oparms(5),2)
gtifile = 'USER'                               ;for exposure map FITS header
if (strupcase(actfil) ne 'NONE') then begin    
  if (n_elements(actime) ne 0) then begin
    print,' You may not both use keyword ACTIME *and*' $
         ,' define an input time intervals file.'
    print,' Please use one or the other. Returning.'
    retall
  endif else begin              ;intervals will be read from a file
    openr,un,actfil,/get_lun    ;get free logical unit & open input ASCII file 
    actbeg = [-1]
    actend = [-1]
    check = 1
    while ( (not eof(un)) and (check) ) do begin
      readf,un,tb,te
      check = (tb+te) gt 0
      if (check) then begin
        actbeg = [actbeg,tb]
        actend = [actend,te]
      endif
    endwhile
;
    nact = n_elements(actbeg)
    if (nact eq 1) then begin     ;if no time intervals were found
      print,' No time intervals were included in accepted time intervals',$
            ' file ',actfil
      print,' Please check your inputs. Returning.'
      retall
    endif else begin
      actbeg = actbeg(1:*)        ;strip off bogus initial values
      actend = actend(1:*)
    endelse
;
    nact = n_elements(actbeg)
    if (chatter ge 2) then begin
      print,actbeg
      print,actend
      print,nact
    endif
    actime = dblarr(nact,2)
    actime(0,0) = actbeg
    actime(0,1) = actend
;
    gtifile = actfil            ;for exposure map FITS header
  endelse
endif               ;finished defining accepted time intervals
;
; Now read the position ordered FITS table to find the observation date
; Use the date to determine which set of instrument maps to use
;   PSPC B if before Jan 25, 1991, PSPC C if after (acc to Jeff Mendenhall)
; Change as of Aug 6, 1993 -- in the new maps, Jeff says the instrument maps
;   are labelled C for before Jan 25, 1991 and B for afterwards
;
case proc of
  'US':  begin
         ext = '.fits'
         extnum = 3
         end
  'MPE': begin
         ext = '_events.tfits'
         extnum = 1
         end
  'RDF': begin
         ext = '_bas.fits'
         extnum = 2
         end
  else:  begin
         print,proc,' is not a valid choice for processing format.'
         print,' Valid choices are US, MPE, RDF. Returning.'
         retall
         end
endcase
;
match_files,dir,obseq,ext,name,nlist        ;look for the right files
poefil = name(0)                            ;default = take latest version
oparms(0) = strtrim(obseq,2)        ;obseq & dir may have been corrected
oparms(1) = strtrim(dir,2)
poehdr = headfits(poefil,ext=extnum)      ;read header from POE table
rsobsdate,poehdr,date,proc=proc           ;get the observation date
if (date lt 910125.) then bandadd = 0 else bandadd = 1
if (date lt 911011.) then igain = 0 else igain = 1    ;high vs low gain
if (chatter ge 1) then begin
  print,' Observation date = ',date
  if (bandadd eq 0) then print,' Using maps for PSPC C' else $
     print,' Using maps for PSPC B'
  if (igain eq 0) then $
      print,' Detector map will be zeroed over high gain limits' else $
      print,' Detector map will be zeroed over low gain limits'
endif
;
mapfil = strtrim(oparms(7),2)               ;name of output FITS image
mapfiluc = strupcase(mapfil)
nbands = n_elements(bands)
if (nbands eq 0) then begin      ;use input BAND
  band = fix(oparms(6))
  if ((band lt 1) or (band gt nimap)) then begin
    print,band,' is outside the allowed range of values (1 to ',$
         nimap,' ). Returning.'
    print,'  '
    print,' Enter the number of the instrument map that you wish to use:'
    print,'    (or Q to quit)'
    print,'  '
    forprint,indgen(nimap)+1,'  '+instmap(bandadd*nimap:(nimap-1)+bandadd*nimap)
    print,'   '
    ans = ''
    read,'Band -->',ans
    if ( (ans eq 'Q') or (ans eq 'q') ) then begin
      print,' Returning.'
      retall
    endif else band = fix(ans)
  endif 
  if (chatter ge 1) then begin
     print,' Exposure map will be calculated for band:'
     print,band,'   '+instmap(band+bandadd*nimap-1)
  endif
  bands = [band]
endif else begin               ;keyword BANDS specified - ignore input BAND
  if ( (mapfiluc eq 'NONE') and (nbands gt 1) ) then begin
    print,' You must specify an output file if calculating >1 map.' $
         +' Returning.'
    retall
  endif
  ind = where( (bands ge 1) and (bands le nimap), ngood )
  if (ngood eq 0) then begin
    print,' Allowed range of values for band = 1 to ',nimap,'.'
    print,' No bands found within this range. Try again. Returning.'
    retall
  endif else bands = bands(ind)
  if (chatter ge 1) then begin
     print,' Exposure maps will be calculated for bands:'
     forprint,bands,'   '+instmap(bands+bandadd*nimap-1)
  endif
endelse  
nbands = n_elements(bands)
;
; Define dead time parameter
;	
deadtp = 234.
;
; Read in Attitude and Livetime data
; Replace call to const_exp_data with rsget_emap_data (which in turn
; calls rsgetasp & rsgetevr)
;
rsget_emap_data,oparms,sctcas,roll,delx,dely,flive,isegs,$
                actime=actime,deadtp=deadtp
;
; Get names of events rates and aspect offsets files for exposure map
; FITS header
;
obseq2 = oparms(0)
dir2 = oparms(1)
case proc of
  'US':  ext2 = '.evr'
  'MPE': ext2 = '_eventrates.tfits'
  'RDF': ext2 = '_anc.fits'
endcase
match_files,dir2,obseq2,ext2,name2,nlist2        ;look for the right files
evrfile = name2(0)                               ;name of events rates file
case proc of
  'US':  ext2 = '.cas'
  'MPE': ext2 = '_attitude.tfits'
  'RDF': ext2 = '_anc.fits'
endcase
match_files,dir2,obseq2,ext2,name2,nlist2        ;look for the right files
aspfile = name2(0)                               ;name of aspect offsets file
;
; Flip the Y value for consistancy.  Increasing Y in SASS is downward
; in declination.  The instrument map will also be flipped.
;
dely = -dely                                  ;need to "flip" y offset
;
; Calculate weighted averages of aspect offsets and roll angle to
; add to exposure map FITS header
;
avdelx = total(flive*delx)/total(flive)/2.    ;in arc sec
avdely = total(flive*dely)/total(flive)/2.
avroll = total(flive*roll)/total(flive)
;
; If desired, define name of output file for writing exposure map
;
if (mapfiluc ne 'NONE') then begin            ;if writing a FITS file
  fdecomp,mapfil,disk2,dir2,name,ext,ver
  if (name eq '') then name = obseq          ;default is to use obseq
  mapfil = disk2+dir2+name+ext
endif
;
; Correct the times between aspect measurements by the live time factors
; Remember that the measurements will be in noncontiguous segments.
; Assume that the first measurement of each segment covered a 1 sec time
; interval.
;
ibeg = isegs(*,0)          ;indices of beginnings of segments
iend = isegs(*,1)          ;indices of ends of segments
nsegs = n_elements(ibeg)
mult = sctcas*0.
for ii=0,nsegs-1 do begin
  ib = ibeg(ii) 
  ie = iend(ii)
  delt = sctcas(ib:ie)
  mult(ib) = delt - [delt(0)-1.,delt(0:ie-ib)] 
endfor
flive = mult*flive         ;now flive = live time for each aspect measurement
mult = 0                   ;again, save memory
sctcas = 0
;
; Define the indices for the aspect steps. Roll is already in units of 
; degrees. Delx and Dely are in units of half arc sec, so delx & dely 
; step sizes will be 14.94733 arc sec (historical reasons).
; Roll step will be 74.7366 arc sec (149.4733/2.) = 0.2076 degrees.
;
mult = 7200./149.4733 + 0.D0     ;try making this double precision
nsize = 10000.
if (!version.os eq 'vms') then nsize = 2*nsize  
;
; Find the unique values of nint(mult*roll), and the indices of these in 
; the roll vector.
; We will need this later, when we accumulate the exposure map.
;
if (chatter ge 1) then print,' Finding unique values of roll angle.'
nroll = n_elements(roll)
if (nroll le nsize) then begin
  vsort,roll,mult,trueroll,indsrt,jbeg,jend,npr
endif else begin
  vsort_large,roll,mult,trueroll,indsrt,jbeg,jend,npr,nsize=nsize
endelse
;
roll = 0
roll = trueroll              ;now roll = unique values of binned roll 
trueroll = 0
;
; Read the exposure map. Do not rotate to the average angle as this will
; introduce more uncertainty in the final exposure map.
;
for ib = 0,nbands-1 do begin          ;now calculate maps, band by band
;
  ibb = bands(ib) + bandadd*nimap     ;don't forget to use the right set
  if (chatter ge 1) then $
     print,' Calculating exposure map for band ',ibb,'  '+sufx(ibb-1)
  detmap = instmap(ibb-1)
;
;  Read the instrument map, center it, invert the Y-axis, and turn it real.
;  Exclude the "bright line" regions of enhanced particle 
;  background as suggested by Plucinsky et al. (1993).  
;
  if (chatter ge 1) then print,'    Reading instrument map ',detmap
  rmap = readfits(detmap,hdmap)
;
; update header for output fits file (change IRAFNAME, )
;
  make_emap_hdr,hdmap,poehdr,proc,actime,emaphdr
;
; update info in exposure map FITS header
;
  fxaddpar,emaphdr,'livetmin',min(flive)
  fxaddpar,emaphdr,'livetmax',max(flive)
  ranom = sxpar(emaphdr,'ra_nom')           ;nominal RA in degrees
  decnom = sxpar(emaphdr,'dec_nom')         ;nominal Dec in degrees
  fxaddpar,emaphdr,'ra_pnt',ranom + avdelx/3600.D0     
  fxaddpar,emaphdr,'dec_pnt',decnom + avdely/3600.D0
  fxaddpar,emaphdr,'pa_pnt',avroll
;
  fxaddpar,emaphdr,'gtifile',gtifile
  fxaddpar,emaphdr,'aspfile',aspfile
  fxaddpar,emaphdr,'evrfile',evrfile
  fxaddpar,emaphdr,'detmfile',detmap
;
  if (mapfiluc ne 'NONE') then begin
    fdecomp,mapfil,disk,dir,name,ext,ver
    if (ext eq '') then ext = '_emap'+sufx(ibb-1)+'.fits' else $
                        ext = sufx(ibb-1)+ext
    outfil = disk+dir+name+ext
  endif
;
; zero out regions that are contaminated by high particle fluxes
; (from Steve Snowden  8 Sep 1993)
; Rows 34 and 455 should not be completely zeroed
;
  if (igain eq 0) then begin          ;high gain regions for zeroing
    for ii=0,33 do rmap(*,ii) = 0
    for ii=456,511 do rmap(*,ii) = 0
    rmap(0,34) = 0.25*rmap(*,34)
    rmap(0,455) = 0.44*rmap(*,455)
  endif else begin                    ;low gain regions for zeroing
    for ii=0,45 do rmap(*,ii) = 0
    for ii=443,511 do rmap(*,ii) = 0
    rmap(0,46) = 0.50*rmap(*,46)
    rmap(0,442) = 0.625*rmap(*,442)
  endelse
;
; The optical axis of the detector is at 4119, 3929 in detector coordinates.
; In the Fortran program, the optical axis of the detector is therefore at
; IX=4119/16=257.4, IY=3929/16=245.6.  The X positions are left the same, but
; the Y positions need to be flipped because the ROSAT coordinate system has
; Y increasing going downward. The Fortran program centered and inverted the
; map using the equation 514-II-12, putting old pixel 245, which contains the
; position 245.6, into new pixel 257. This put the Y position of the center
; is at 257.4, and the detector center at (257.4, 257.4) 
;
; In IDL, indices start from zero (not 1), so the optical axis of the 
; detector is at 256.4, 244.6. After the centering and inversion, the 
; detector center should be at (256.4, 256.4). The equation in IDL then
; becomes 500-II (instead of 502-II).
;  
  i = indgen(464) + 12                    ;here the error is corrected
  map = rmap*0
  map(*,500-i) = rmap(*,i)
  rmap = 0                                ;set to zero to save memory
;
; Now imbed the 512 by 512 detector map in a 513 by 513 element array, so
; that the rotated matrix will be centered on the same pixel as the
; nonrotated matrix. This will only happen if the matrix has an odd number
; of elements (and appears to be a problem with rot ...)
; (this from tests with Steve Snowden 8 Sep 1993)
;  
  rmap = fltarr(513,513)
  rmap(0,0) = map
  map = rmap                              ;the code expects map later on
  rmap = 0
;
; Now accumulate the exposure map.
; Iterate through unique values of roll. For each step nn,
; indsrt(jbeg(ii):jend(ii)) will give the positions (in the old roll vector)
; of the unique values of roll, and hence of the corresponding values of
; delx, dely, and flive.
;
  if (chatter ge 1) then print,' Accumulating exposure map for ',npr,$ 
     ' roll angles'
  expmap = fltarr(512,512)
  s = size(expmap)
  nxdim = s(1)
  nydim = s(2)
  totexp = 0.0
  for nn=0,npr-1 do begin
;
; Find values of delx, dely, and flive which correspond to roll(nn). 
; Bin delx & dely in steps of 14.94733 arc sec.
;
    if (chatter ge 1) then print,nn,npr-1,' Roll = ',roll(nn)
    jb = jbeg(nn)
    je = jend(nn)
    xoff = delx( indsrt(jb:je) )
    yoff = dely( indsrt(jb:je) )
    exptime = flive( indsrt(jb:je) )
    mult = 1.D0/29.894656         ;try with the extra precision
;
; from conversation with Steve Snowden Sep 9 1993:
; We will want the central sky pixel to be 256,256 (IDL indices). 
; (These numbers refer to the lower left corner of the pixel.)
; Since the optical axis is 256.4, 256.4, this is -0.1 difference in x & y
; from the center of the central sky pixel. Hence, we will want the quantized
; xoffsets between -0.1 and 0.9 to be mapped into 0 offset (and likewise for
; y). 
;
    xoff = nint( mult*xoff - 0.4)  ;center the map on the optical axis in x
    yoff = nint( mult*yoff - 0.4 ) ;center the map on the optical axis in y
;
; Now find the unique values of the vector offsets, defined by the unique
; values of the pairs (ix,iy). 
;
    npx = max(xoff-min(xoff)) + (min(xoff) lt 0)     ;number of delx steps
    Loc = xoff + npx * yoff         ;unique values of loc give unique (ix,iy)
    nLoc = je - jb + 1
    if (nLoc le nsize) then begin
      vsort,Loc,1.0,trueLoc,soL,kbeg,kend,noff
    endif else begin
      vsort_large,Loc,1.0,trueLoc,soL,kbeg,kend,noff,nsize=nsize
    endelse
;
    loc = 0
    trueloc = 0
    xoff = xoff( sol(kend) )  
    yoff = yoff( sol(kend) )
    expt = xoff*0.          ;expt will be the total exposure at each (ix,iy)
    for ii=0,noff-1 do expt(ii) = total( exptime( sol(kbeg(ii):kend(ii)) ) )
    exptime = 0
    sol = 0
    kbeg = 0
    kend = 0
    if (chatter ge 1) then $
       print,' There are ',noff,' unique offset points for this roll angle'
;
; Now rotate instrument map by roll(nn). For each vector offset, shift,
;   weight by total exposure time at that offset (expt), and add to
;   accumulated exposure map.
; 08 Sep 1993  from tests with Steve Snowden, it appears that the center
;   must be specified explicitly in order that the center of the rotated 
;   map coincide with the center of the unrotated map
;
; Remember, the optical axis is at 256.4, 256.4, so pixel 256,256
; (whose center is 256.5, 256.5) is 0.1,0.1 pixels offset from the
; center.
;
    rmap = rot(map,roll(nn),1.,256.4,256.4)    ;rotate cwise by roll(nn) deg
;
; Find the start and stop x & y indices of the overlapping regions
;
    ixb = -xoff*(xoff lt 0)         ;-xoff, 0, 0 for xoff <0, =0, >0
    ixe = nxdim - 1 - xoff*(xoff gt 0)  ;nxdim-1, nxdim-1, or nxdim-1-xoff
    jxb = xoff*(xoff gt 0)          ;0, 0, or xoff
    jxe = jxb + (ixe - ixb)         ; jxe - jxb equals ixe - ixb
;
    iyb = -yoff*(yoff lt 0)         ;-yoff, 0, 0 for yoff <0, =0, >0
    iye = nydim - 1 - yoff*(yoff gt 0)  ;nydim-1, nydim-1, or nydim-1-yoff
    jyb = yoff*(yoff gt 0)          ;0, 0, or yoff
    jye = jyb + (iye - iyb)         ; jye - jyb equals iye - iyb
;
    for ii = 0,noff-1 do begin
;
; add line by line just for Vax, else it's quicker to add the whole submap
; to the cumulative sum under Unix
;
      if (!version.os eq 'vms') then begin
        for jj=0,iye(ii)-iyb(ii) do begin
          line = expt(ii)*rmap(ixb(ii):ixe(ii),iyb(ii)+jj)
          line = expmap(jxb(ii):jxe(ii),jyb(ii)+jj) + line
          expmap(jxb(ii),jyb(ii)+jj) = line
        endfor
      endif else expmap(jxb(ii),jyb(ii)) = $
          expmap(jxb(ii):jxe(ii),jyb(ii):jye(ii)) + $
          expt(ii) * rmap(ixb(ii):ixe(ii),iyb(ii):iye(ii))
;
      if (chatter ge 2) then begin
        check = (fix(ii/10.)*10 eq ii)*(ii ne 0)
        if (check) then print,'   ',nint(float(ii)/noff*100.),'  % done'
      endif
    endfor
    totexp = totexp + total(expt)
;
    ixb = 0                 ;set these to zero to save memory
    ixe = 0
    jxb = 0
    jxe = 0
    iyb = 0
    iye = 0
    jyb = 0
    jye = 0
;
    xoff = 0
    yoff = 0
    expt = 0
  endfor
  if (chatter ge 1) then print,' Finished accumulating exposure map.'
;
; Now write accumulated exposure map to output FITS file (outfil)
; Update EMAPHDR in any case
;
  map = 0
  rmap = 0
;
  fxaddpar,emaphdr,'datamin',min(expmap)
  fxaddpar,emaphdr,'datamax',max(expmap)
  fxaddpar,emaphdr,'livetime',totexp
;
  histrec = ' Total exposure time ='+string(totexp)+' sec'
  fxaddpar,emaphdr,'HISTORY','  '+histrec
  histrec = ' Instrument map '+detmap+' was used.'
  fxaddpar,emaphdr,'HISTORY','  '+histrec
  histrec = ' Map was zeroed over regions for '
  if (igain eq 0) then histrec = histrec+'high gain' else $
                       histrec = histrec+'low gain'
  fxaddpar,emaphdr,'HISTORY','  '+histrec
;
  if (mapfiluc ne 'NONE') then begin
    map = expmap
    if (chatter ge 1) then $
      print,' Writing exposure map FITS file ',outfil
    writefits,outfil,map,emaphdr
    map = 0
  endif
endfor
;        
if (chatter ge 1) then print,' Normal Termination.'
;        
return
end        ;pro make_emap
