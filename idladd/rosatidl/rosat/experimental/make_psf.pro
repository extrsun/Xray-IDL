;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;            make_psf
;
;*PURPOSE:
; A procedure to construct a 2 dimensional image of the (Rosat) point spread 
; function.
;
;*CALLING SEQUENCE:
;	make_psf,inputs,offang,prof,psfimg,header,profhdr,tcb,rate=rate,$
;               ,group=group,ecen=ecen,oparms=oparms
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
;       ZDEF:MAKE_PSF.DEF
;
;       The following parameters are availble.
;
;        INSTR       Instrument 
;                    (default is P for PSPC)
;        SPFIL       (Full) name of input spectral file (or NONE)
;        SPTYP       Type of input spectral file (def=QDP, PROS, PHA)
;                    (default is QDP for QDP .plot or ASCII file)
;        IMGFIL      (Full) name of output PSF FITS image (or NONE)
;                    (default extension is '_psf.fits')
;        PROFTAB     (Full) name of output table for PSF profile (or NONE)
;                    (default is NONE; default extension is '_psf.tab')
;        CHATTER     Controls program feedback to user ("chattiness")
;                    (default = 1)
;        XCEN        X coordinate giving center of output image
;        YCEN        Y coordinate giving center of output image
;                    default = 0 means on axis
;        PIXEL       Tells whether XCEN and YCEN are given in blocked (=BL)
;                    or unblocked (=UNBL = default) pixels
;        BLOCK       Blocking factor (default = 1)
;        EMIN        Gives minimum energy (in keV) to be included
;                    used only if spectrum is read from a file
;        EMAX        Gives maximum energy (in keV) to be included
;                    used only if spectrum is read from a file
;        NBINS       Number of spatial bins in output PSF image 
;        BINSIZ      Size of output PSF bin (in arcsec; default = 16)
;        BINRAT      Ratio of number internal pixels to number of PSF bins
;                    (default = 11)
;        RESCL       Tells whether the peaks of the PSF image and profile
;                    should be rescaled so that the maximum value = 1
;                    default = Y for Yes; if Yes, then a history record
;                    will be added to the headers which will tell what
;                    scaling factor to use to recover the probability/pixel
;                    if N for No, then values will be probability/pixel
;
; OPTIONAL INPUTS:
;        RATE    - vector containing observed (or assumed) counting rate for 
;                  each of the channels specified by GROUP (or ECEN)
;                  must be entered on command line (with GROUP or ECEN)
;                  if SPTYP = 'NONE'
;                  should be in units of counts per PHA bin
;        GROUP   - pha channel boundaries (e.g., as specified by !group)
;        ECEN    - central energies (in keV) for each element in RATE
;                  Note: both GROUP and ECEN may not be entered
;
; OUTPUTS:
;        OFFANG  - off axis angle = angle from target position (arcsec)
;                  Note -- these are now different angles than those
;                  returned by calcpsfp and calcpsfh 
;        PROF    - point spread function (PSF) radial profile (at OFFANG)
;        PSFIMG  - 2D image of PSF
;        HEADER  - FITS header for PSF image file
;        PROFHDR - FITS type header for PSF profile table
;        TCB     - table control block for PSF profile table
;        OPARMS  - the internally parsed form of INPUTS (same form as list
;                  in example 3 below)
;
;*EXAMPLES: There are 4 different ways that the inputs may be specified
;    mode 1) Interactive
;        IDL>make_psf,1,oparms=oparms
;             ?spfil=iraf/rp123456_obs
;             ?sptyp=pros
;             ?imgfil=iraf/rp123456_psf.fits
;             ?proftab=iraf/rp123456_prof.tab
;             ?xcen=250
;             ?ycen=250
;             ?pixel=bl
;             ?block=30
;	      ?end
;
;    mode 2) command line (for mode 1) using string
;        IDL>list='spfil=NONE,imgfil=iraf/rp123456_psf.fits'
;        IDL>make_psf,list,oparms=oparms,rate=rate,group=group
;  
;    mode 3) command line (for mode 1) using parameter array
;        IDL>list=strarr(20,25)
;        IDL>list(4)='iraf/rp123456_src1.pha'
;        IDL>list(5)='PHA'
;        IDL>list(6)='iraf/rp123456_psf'
;        IDL>make_psf,list,oparms=oparms,rate=rate,ecen=ecen
;  
;    mode 4) disk file
;        IDL>$edit myinput.dat
;            spfil=rp123456_src1.plot
;            imgfil=NONE
;            proftab=rp123456_prof.tab
;                 .
;                 :		{ 1 line/parameter (include name & =)}
;            xcen=7700
;            ycen=7640
;            *exit
;        IDL>make_psf,list,oparms=oparms
;
;*RESTRICTIONS:
;    So far, works only on axis (i.e., off axis angle = 0.0 arcmin)
;    If the input spectral file SPTYP = 'NONE', then keywords ECEN (or GROUP)
;       and RATE must be entered on the command line.
;    Only one of keywords ECEN and GROUP may be entered on the command line.
;
;*NOTES:
;    Uses READ_SPEC to read the input spectrum (if an input file is given),
;       CALC_PSFP (or CALCPSFH) to caluclate the point spread function, 
;       WRITEFITS to write the PSF image to a FITS file, and TAB_WRITE to 
;       write the PSF profile to a ST SDAS binary table (if desired)
;    Spectral information is not needed (and is ignored) when calculating the
;       HRI PSF
;    CALCPSFP and CALCPSFH both return the radial profile defined over the
;       unique angles of the internal fine grid used for the computation.
;       MAKE_PSF now modifies these to return the radial psf profile
;       defined on the same pixel scales as the image
;
;   The following is taken from the comments written by G. R. Hasinger:
;
;   The PSPC on-axis PSF is a combination of three, physically well 
;   understood terms:
;
;   1. a Gaussian for the intrinsic PSPC resolution due to the inherent
;      statistics of the primary electron generation. Theoretically
;      the Gaussian Sigma is propotrional to 1/SQRT(Energy)
;
;   2. an exponential function due to the finite penetration depth of
;      the X-rays in the counter gas combined with the 8.5 degree cone
;      angle. The PSPC is focussed for 1 keV; the 'chromatic aberration'
;      is largest for large energies
;
;   3. A Lorentz function for the mirror scattering; theoretically, the
;      behavior increases as the square of the energy if the grazing 
;      angle is constant, so diffraction forces the shape parameters to
;      vary as inverse energy.
;
;   Technically, these 3 components should be folded together, but their
;   angular domains are sufficiently separated that simply adding them
;   together is reasonably accurate.  Detailed comparison with calibration
;   data at monochromatic energies of 0.28, 0.93, 1.49, and 1.70keV provide
;   the parameter values.  No fit is possible below 0.15keV (channel 15) 
;   due to the PSPC electronics which give rise to additional 'ghost
;   images'. These events should be avoided as far as possible in PSF 
;   modelling.
;
;   The off-axis blur of the telescope, although highly structured and
;   asymmetric, can be modeled by a simple Gaussiane in its radially
;   integrated profile.  This Gaussian is added in quadrature to the
;   Gaussian of the detector.  Since the PSF is not convolved, but a 
;   simple addition of terms, the contribution of the exponential term
;   must be diminished while the Gaussian is "eating up" the exponential.
;   This is modelled as a Gaussian decay of the exponential term as a 
;   function of the off-axis angle.
;
;*SUBROUTINES CALLED:
;  RSGETPAR
;  RSGETDEFPAR
;  GETLOG
;  FDECOMP
;  MATCH_FILES
;  READ_SPEC
;  READ_ASPC
;  CHAN2ENERGY
;  CALCPSFP
;  CALCPSFH
;  DIST_CIRCLE
;  RSPSFP
;  RSPSFH
;  MAKE_PSF_HDR
;  MKHDR
;  SXPAR
;  SXADDPAR
;  WRITEFITS
;  WRITE_PSF_TAB
;  TAB_EXPAND
;  TAB_PUT
;  TAB_WRITE
;
;*MODIFICATION HISTORY:
;    written 14 Jul 1992 by GAR
;    modified 28 Jul 1992 (GAR) to make sure that spectrum is always
;      given in counts/bin
;    modified 07 Aug 92 (GAR) to allow peak value to be rescaled to 1,
;      and to allow a range of channel energies to be specified (if
;      the input spectrum is read from a file)
;    modified 17 Aug 92 (GAR) to calculate profile and PSF image for HRI
;    modified 12 Oct 1992 (GAR) region descriptor changed from square to box
;    modified 30 Oct 1992 (GAR) to define region descriptor in original pixels
;    modified 10 Jul 1993 (GAR) to complete documentation prologue, to
;      return the PSF radial profile defined on the same pixel scales as
;      the 2D PSF image, and to switch order of parameters PIXEL and BLOCK
;      with EMIN and EMAX (also changed in make_psf.def)
;    modified 12 July 1993 (GAR) to include PSF description in prologue and
;      to incorporate changes for calculating off-axis psf
;    modified 03 Mar 1994 (GAR) to be compatible with new version of RSPSFH
;      which includes off-axis parameterization.
;-
;-------------------------------------------------------------------------------
pro make_psf,inputs,offang,prof,psfimg,header,profhdr,tcb,rate=rate,$
             group=group,ecen=ecen,oparms=oparms
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' MAKE_PSF, inputs, OFFANG, PROF, PSFIMG, HEADER, PROFHDR, TCB,'
  print,'           rate=rate, group=group, ecen=ecen, OPARMS=OPARMS'
  print,'   '
  print,'   Uses inputs INSTR, SPFIL, SPTYP, IMGFIL, PROFTAB, CHATTER,'
  print,'               XCEN, YCEN, PIXEL, BLOCK, EMIN, EMAX, NBINS,'
  print,'               BINSIZ, BINRAT, and RESCL from ZDEF:make_psf.def'
  print,'   '
  print,'   If SPFIL = NONE, then channel groups (OR central energies)'
  print,'      and count rates must be entered as keywords (for PSPC)'
  print,'   '
  print,'   Spectral information is ignored when INSTR = H (HRI)'
  print,'   '
  retall
endif
;
dfile = 'make_psf'
rsgetpar,inputs,dfile,oparms
instr = strupcase( strtrim(oparms(3),2) )
if ((instr ne 'P') and (instr ne 'H')) then begin
  print,' Sorry, ',instr,' is not a valid instrument.'
  print,' Choices are P or H. Returning.'
  retall
endif
chatter = fix(oparms(9))
;
spfil = strtrim(oparms(4),2)                  ;name of input spectral file
sptyp = strupcase(strtrim(oparms(5),2))       ;type (origin) of input spectrum
;
if (instr eq 'P') then begin
  if (strupcase(spfil) eq 'NONE') then begin    ;keywords will be used   
    if (n_elements(rate) eq 0) then begin       
      print,' The input spectrum is not defined. '
      print,' Please enter an input spectral file, or use the keywords' $
           ,' GROUP or ECEN, and RATE'
      print,' Returning.'
      retall
    endif
;
    if (n_elements(ecen) ne 0) then begin
      if (n_elements(group) ne 0) then begin
        print,' You may not use both ECEN *and* GROUP. Please use one or' $
             ,' the other.'
        print,' Returning.'
      endif
      ecalc = 0                   ;use central energies
    endif else begin
      ecalc = 1                   ;use channel groupings
      if (n_elements(group) eq 0) then group = !group   ;use !group as default
    endelse
  endif else begin                ;spectrum will be read from a file
    if (n_elements(rate) ne 0) then begin
      print,' You may not both use keyword RATE *and*' $
           ,' define an input spectral file.'
      print,' Please use one or the other. Returning.'
      retall
    endif
;
    ecalc = 0                     ;use central energies
    fdecomp,spfil,disk,dir,name,ext,ver
    if (ext eq '') then begin
      if (sptyp eq 'QDP') then ext = '.plot'
      if (sptyp eq 'PHA') then ext = '.pha'
      if (sptyp eq 'PROS') then ext = '.tab'
      if (sptyp eq 'ASPC') then begin
        pos = strpos(name,'_asc')
        pos = pos(0)
        if (pos lt 0) then name = name + '_asc'
        if (ext eq '') then ext = '.spc' else ext = '.'+ext
      endif
    endif else ext = '.'+ext
    if (ver ne '') then ver = ';'+ver else ver = ';*'
;
    match_files,disk+dir,name,ext+ver,spname,nlist  ;look for the right files
    spfil = spname(0)                           ;default = take latest version
    oparms(4) = spfil
  endelse
endif
;
imgfil = strtrim(oparms(6),2)
proftab = strtrim(oparms(7),2)
if (imgfil eq '') then $
   read,' Please enter an output file for the PSF image > ',imgfil
if (strupcase(imgfil) ne 'NONE') then begin
  fdecomp,imgfil,disk,dir,name,ext,ver
  if (ext eq '') then ext = '_psf.fits' else ext = '.'+ext
  if (ver ne '') then ver = ';'+ver
  imgfil = disk+dir+name+ext+ver
  oparms(6) = imgfil
endif
if (strupcase(proftab) ne 'NONE') then begin
  fdecomp,proftab,disk,dir,name,ext,ver
  if (ext eq '') then ext = '_psf.tab' else ext = '.'+ext
  if (ver ne '') then ver = ';'+ver
  proftab = disk+dir+name+ext+ver
  oparms(7) = proftab
endif
;
pixel = strtrim(oparms(12),2)
if (pixel eq '') then pixel = 'UNBL'      ;default is for unblocked pixels
if (strupcase(pixel) ne 'UNBL') then begin
  block = fix(oparms(13))
  if (block eq 0) then block = 1
endif else block = 1
;
xcen = float(oparms(10))*block
ycen = float(oparms(11))*block
case instr of                       ;define default centers & binsizes
  'P': begin
       xdef = 7680.                 ;center in original pixels
       defbinsz = 0.5               ;arcsec/original pixel
       end
  'H': begin
       xdef = 4096.
       defbinsz = 0.5               ;arcsec/original pixel
       end
endcase
ydef = xdef
;
; Update so that program now works for off-axis PSFs as well
; Offcen is the off-axis angle of the PSF center from the center of the
;    detector, in arcmin
;
if (xcen eq 0) then xcen = xdef
if (ycen eq 0) then ycen = ydef
offcen = sqrt( (xcen-xdef)*(xcen-xdef) + (ycen-ydef)*(ycen-ydef) )/120.
if (chatter ge 1) then print,' PSF off-axis angle =',offcen,' (arcmin)'
;
emin = float(oparms(16))
emax = float(oparms(17))
if (emin gt emax) then begin
  esav = min
  emin = emax
  emax = esav
endif
;
nbins = fix(oparms(19))              ;number of bins in output PSF image (51)
binsize = float(oparms(20))          ;size of PSF bins in arcsec (16)
binrat = fix(oparms(23))             ;number internal pixels/PSF bins (11)
binrat = float(binrat)
rescl = strtrim(oparms(24),2)        ;rescale so peakvalue is 1? or not?
rescl = strupcase( strmid(rescl,0,1) )
;
; Make sure that nbins and binrat are both odd integers
;
check = 2.*fix(nbins/2.)
if (check eq nbins) then begin
  print,'Number of bins',nbins,' is not an odd integer. Resetting to '$
       ,nbins+1
  nbins = nbins + 1
  oparms(19) = nbins
endif
check = 2.*fix(binrat/2.)
if (check eq binrat) then begin
  print,' Bin ratio',binrat,' is not an odd integer. Resetting to '$
       ,binrat+1.
  binrat = binrat + 1.
  oparms(23) = binrat
endif
;  
if (!debug gt 1) then stop,' Stopping in MAKE_PSF after inputs defined'
;
; Now read in spectral file (if spfil ne 'NONE'). Use READ_SPEC
; Don't if instr = 'H'
;
if (instr eq 'P') then begin
  if (strupcase(spfil) ne 'NONE') then begin
     read_spec,spfil,sptyp,ecen,edel,rate,sigrate,chatter=chatter
     ind = where( ((ecen+edel/2.) ge emin) and ((ecen-edel/2.) le emax) )
     if (ind(0) lt 0) then begin
       print,' Min and max energies of spectrum are :',minmax(ecen)
       print,' Specified limits: ',emin,emax,' contain no channels. Returning.'
       retall
     endif
     if (chatter eq 1) then $
        print,' Restricting energy range of spectrum to ',emin,emax
     ecen = ecen(ind)
     edel = edel(ind)
     rate = rate(ind)
     sigrate = sigrate(ind)
  endif
  if (strupcase(spfil) eq 'QDP') then begin         ;need to multiply by edel
    rate = rate*edel
    sigrate = sigrate*edel
  endif
;
  if (ecalc) then $
     calcpsfp,nbins,binsize,offcen,rate,offang,prof,psfimg,$
              group=group,binrat=binrat,chatter=chatter $
     else $
     calcpsfp,nbins,binsize,offcen,rate,offang,prof,psfimg,$
              ecen=ecen,binrat=binrat,chatter=chatter
endif
if (instr eq 'H') then $
   calcpsfh,nbins,binsize,offcen,offang,prof,psfimg,binrat=binrat,$
            chatter=chatter
;
; rescale PSF image & radially averaged profile, if desired
;
if (rescl eq 'Y') then begin
  imgmult = max(psfimg)
  profmult = max(prof)
  if (imgmult eq 0.0) then imgmult = 1.0        ;don't divide by zero!!
  if (profmult eq 0.0) then profmult = 1.0
  psfimg = psfimg/imgmult
  prof = prof/profmult
endif else begin
  imgmult = 1.0
  profmult = 1.0
endelse
;
;Return radial profile binned to same pixel size as 2D image
;
nbinang = fix(nbins*sqrt(2)) + 1
binang = binsize/2. + binsize*findgen(nbinang)
tabinv,offang,binang,ibin
ibin = nint(ibin)
if (!debug gt 1) then stop,'Stopping in MAKE_PSF after calculating ibin'
;
prof = prof(ibin)
offang = offang(ibin)
;
; define ASCII region descriptor for region over which PSF was defined
;
f = '$(f9.2)'
psfreg = 'box ('+string(xcen,format=f)+','+string(ycen,format=f)
psfreg = psfreg+','+string(nbins*binsize/defbinsz,format=f)
psfreg = psfreg+','+string(nbins*binsize/defbinsz,format=f)
psfreg = psfreg+')'
;
; now write PSF image to simple FITS file, if IMGFIL ne 'NONE'
; First, make the FITS header. Add history records to tell how the image
; was constructed. (use MAKE_PSF_HDR)
;
if (strupcase(imgfil) ne 'NONE') then begin
  if (chatter eq 1) then $
     print,' Writing PSF image to FITS file ',imgfil
  mkhdr,header,psfimg
  make_psf_hdr,psfreg,nbins,instr,spfil,sptyp,binsize,binrat,rate,imgfil,$ 
               header,ecen=ecen,group=group,imgmult=imgmult
  if (!debug gt 1) then stop,' Stopping in MAKE_PSF before writing FITS file'
  writefits, imgfil, psfimg, header 
endif
;      
; now write PSF profile to binary ST SDAS file, if PROFTAB ne 'NONE'
; table type will be _psf.tab 
;
if (strupcase(proftab) ne 'NONE') then begin
  if (chatter eq 1) then $
     print,' Writing PSF profile to ST SDAS binary table ',proftab
  write_psf_tab,psfreg,imgfil,proftab,offang,prof,tcb,profhdr,profmult=profmult
endif
;  
if (!debug gt 1) then stop,' Stopping in MAKE_PSF before end of program'
;
return
end         ;pro make_psf
