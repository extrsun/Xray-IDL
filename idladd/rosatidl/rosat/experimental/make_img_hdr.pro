;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;   make_img_hdr
;
;*PURPOSE:
;   A procedure to make a header for an image made from a qpoe events list
;
;*CALLING SEQUENCE:
;   make_img_hdr,image,imginfo,instr,imgfil,imghdr
;
;*PARAMETERS:
; INPUTS:
;   imginfo  - Data structure containing info concerning creation of
;              the image
;   instr    - Instrument used ('P' for PSPC = def, or 'HRI')
;   imgfil   - Name of output fits file for exposure map
;
;*OPTIONAL INPUTS:
;
; OUTPUTS:
;   imghdr   - modified FITS header for output image FITS file
;
;*NOTES:
;
;*SUBROUTINES CALLED:
;  SXPAR
;  SXDELPAR
;  SXADDPAR
;  HEADFITS
;
;*MODIFICATION HISTORY:
;    written 18 Apr 1993 by GAR
;    modified 12 May 1993 (GAR) because we really don't need PLINFO
;    modified 9 Aug 1993 (GAR) to add accepted time intervals to output
;      header (these will come from the imginfo structure variable)
;-
;-------------------------------------------------------------------------------
pro make_img_hdr,image,imginfo,instr,imgfil,imghdr
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' MAKE_IMG_HDR,image,imginfo,instr,imgfil,imghdr'
  retall
endif
if (instr eq '') then instr = 'P'     ;default is P for PSPC
proc = imginfo.proc
if (proc eq '') then proc = 'US'      ;default is US format
totexp = imginfo.totexp
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
mkhdr,imghdr,image                   ;make a basic FITS header
;sxdelpar,imghdr,'DATATYPE'          ;don't think we want this (?)
;
sxaddpar,imghdr,'BSCALE',1.0,'','TELESCOP'
sxaddpar,imghdr,'BZERO',0.0,'','TELESCOP'
fdecomp,imgfil,disk,dir,name,ext,ver
irafname = disk+dir+name+'_img.imh'            ;IRAF name for output file
sxaddpar,imghdr,'IRAFNAME','./'+irafname,'Output IRAF image name',$
         'TELESCOP'
sxaddpar,imghdr,'HISTORY','   '
;
histrec = ' Image created for '+imginfo.obseq+' from events in photon list'+$
          imginfo.filename
sxaddpar,outhdr,'HISTORY','  '+histrec
histrec = ' Created with xcen = '+string(form='$(f12.3)',imginfo.xcen)+$
          ', ycen = '+string(form='$(f12.3)',imginfo.ycen)
sxaddpar,outhdr,'HISTORY','  '+histrec
histrec = '         with '+string(form='$(i5)',imginfo.nxbin)+' by '
histrec = histrec+string(form='$(i5)',imginfo.nybin)+' bins of size '
histrec = histrec+string(form='$(f8.3)',imginfo.binsiz)
sxaddpar,outhdr,'HISTORY','  '+histrec
histrec = ' Z-scale = '+imginfo.zval
sxaddpar,outhdr,'HISTORY','  '+histrec
histrec = '         PI channel range = '+$
          string(form='$(f7.2)',imginfo.pimin)+' to '+$
          string(form='$(f7.2)',imginfo.pimax)
sxaddpar,outhdr,'HISTORY','  '+histrec
;
; Now add accepted time intervals and total exposure time to history records
;
totexp = imginfo.totexp
histrec = ' Total exposure time ='+string(totexp)+' sec'
sxaddpar,outhdr,'HISTORY','  '+histrec
;
ntimes = imginfo.ntimes
actbeg = imginfo.actbeg
actend = inginfo.actend
actbeg = actbeg(0:ntimes-1)
actend = actend(0:ntimes-1)
;
hdradd = strarr(1+ntimes)
histrec = ' Events were selected over the following time intervals:'
hdradd(0) = histrec
for ii=1,ntim do begin
  temp = string(ii,format='$(i3)')
  histrec = temp+string(actbeg(ii-1))+'  '+string(actend(ii-1))
  hdradd(ii) = histrec
endfor
;
for ii=0,ntim+1 do begin
  histrec = hdradd(ii)
  sxaddpar,emaphdr,'HISTORY','  '+histrec
endfor
;
return
end           ;pro make_img_hdr
