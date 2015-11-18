;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   make_psf_hdr
;
;*PURPOSE:
;   A procedure to make a header for a PSF image
;
;*CALLING SEQUENCE:
;   make_psf_hdr,psfreg,nbins,instr,spfil,sptyp,binsize,binrat,rate,imgfil,$ 
;                header,ecen=ecen,group=group,imgmult=imgmult
;
;*PARAMETERS:
; INPUTS:
;   psfreg   - ASCII region descriptor for region over which PSF was
;              calculated
;   nbins    - number of spatial bins in PSF image
;   instr    - the instrument used (e.g., 'P' for PSPC, 'H' for HRI)
;   spfil    - the input spectrum from which the PSF image was calculated
;              (or 'NONE')
;   sptyp    - the type of file that the input spectrum was read from
;              (e.g., 'PROS','QDP','PHA','ASPC'; see READ_SPEC)
;   binsize  - size of PSF spatial bin (in arcsec)
;   binrat   - ratio of number of PSF spatial bins to number of internal pixels
;   rate     - spectral count rate
;   imgfil   - name of output FITS file
;   header   - simple FITS header for output FITS file (from MKHDR)
;
; OPTIONAL INPUTS:
;   ecen     - central energies for each element of RATE
;   group    - low and high channel bounds of groups used for RATE (see, e.g.,
;              !group)
;              (!group is assumed if neither ecen nor group are defined)
;   imgmult  - factor by which PSF image should be multiply to recover
;              probability/pixel (so integral is 1)
;              (default = 1.0)
;
; OUTPUTS:
;   header   - modified FITS header for output FITS file
;
;*NOTES: 
;
;*SUBROUTINES CALLED:
;  SXPAR
;  SXADDPAR
;
;*MODIFICATION HISTORY:
;    written 14 July 1992 by GAR
;    modified 07 Aug 1992 (GAR) to be compatible with new version of
;      make_psf
;    modified 17 Aug 1992 (GAR) to make right header for HRI PSF
;-
;-------------------------------------------------------------------------------
pro make_psf_hdr,psfreg,nbins,instr,spfil,sptyp,binsize,binrat,rate,imgfil,$
    header,ecen=ecen,group=group,imgmult=imgmult
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' MAKE_PSF_HDR, psfreg, nbins, instr, spfil, sptyp, binsize, binrat,'
  print,'               rate, imgfil, HEADER, ecen=ecen, group=group' $
       +', imgmult=imgmult (1)'
  retall
endif
if (n_elements(imgmult) eq 0) then imgmult = 1.0    ;default is 1.0
;
; check to make sure instrument is valid
;
instuc = strupcase(instr)
if ( (instuc ne 'P') and (instuc ne 'H') ) then begin
  print,' Sorry, ',instr,' is not a valid instrument.'
  print,' Choices are P or H. Returning.'
  retall
endif
;
; if neither ecen nor group are entered, then set group = !group as default
; only add spectral information for PSPC
;
if (instuc eq 'P') then begin
  if (n_elements(ecen) eq 0) then begin               ;ecen not defined
    if (n_elements(group) eq 0) then group = !group   ;use !group as default
    s = size(group) & nchan = s(1)
  endif else nchan = n_elements(ecen)                 ;ecen defined
endif 
if (instuc eq 'H') then nchan = 0                     ;no channels if HRI
;
; now add the relevant info to the header
;
date=sxpar(header,'DATE')
sxaddpar,header,'DATE',date,'  FITS creation date'
sxaddpar,header,'BSCALE','1.0','  Real = data*BSCALE + BZERO'
sxaddpar,header,'BZERO','0.0','  Real = data*BSCALE + BZERO'
sxaddpar,header,'HISTORY','  '
;
if (instuc eq 'P') then hdradd = strarr(9+nchan)
if (instuc eq 'H') then hdradd = strarr(9)
temp = string(nbins,format='$(i4)')+' bin'
histrec = ' '+temp+' by '+temp+' PSF image calculated for'
if (instuc eq 'P') then temp = ' Rosat PSPC'
if (instuc eq 'H') then temp = ' Rosat HRI'
histrec = histrec+temp
hdradd(0) = histrec
;
if (instuc eq 'P') then begin
  histrec = ' Input spectrum : '+spfil
  hdradd(1) = histrec
  if (strupcase(spfil) ne 'NONE') then $
     histrec = '       Spectrum is of type  '+sptyp 
  hdradd(2) = histrec
endif
;
hdradd(3) = ' PSF region = '+psfreg
;
histrec = '  Binsize ='+string(binsize,format='$(f7.2)')+' arcsec '
histrec = histrec+'  Bin to internal pixel ratio = '
histrec = histrec+string(binrat,format='$(f7.2)')
hdradd(4) = histrec
;
if (imgmult ne 1.0) then begin
  histrec = ' PSF values were rescaled to make maximum value = 1.0'
  hdradd(5) = histrec
  temp = string(imgmult,format='$(e12.4)')
  histrec = ' Multiply values by '+temp+' to recover probability/pixel'
  hdradd(6) = histrec
endif else begin
  histrec = ' PSF values are given in units of probability/pixel'
  hdradd(5) = histrec
  histrec = ' Integral of PSF over all pixels = 1.0'
  hdradd(6) = histrec
endelse
; 
if (instuc eq 'P') then begin
  nchan = n_elements(rate)
  if (n_elements(ecen) eq 0) then histrec = '  Channel groups used' else $
     histrec = ' Central energies used'
  hdradd(7) = histrec
;
  for ii=0,nchan-1 do begin
    histrec = '  '
    if (n_elements(group) ne 0) then $ 
       histrec = histrec+string(group(ii,0))+'  '+string(group(ii,1)) 
    if (n_elements(ecen) ne 0) then $
       histrec = histrec+string(ecen(ii))
    histrec = histrec+'  '+string(rate(ii))
    hdradd(9+ii) = histrec
  endfor
endif
;
; now add this information to HEADER, as history records
;
for ii=0,nchan+8 do begin
  histrec = hdradd(ii)
  sxaddpar,header,'HISTORY','  '+histrec
endfor
;
return
end           ;pro make_psf_hdr
