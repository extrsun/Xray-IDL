pro cal_background,fname,bfname,ccd,emin=emin,emax=emax,filter=filter,bnorm=bnorm,ebnorm=ebnorm
;+
; Calibrate the normalization of the stowed background, using the
; outer energy range where the cosmic-ray contribution dominates.
; fname, bfname - input event list and corresponding stowed background event list
; ccd - selected ccd chips for the calcuation
; emin, emax - minimum and maximum energy range for the calibration
;              (def =10-14 keV for BI chips and 10-12 keV for FI chips)
;bnorm - the output normalization factor for each chip (effectively reducing the
;        exposure of the stowed background)
;ebnorm - the uncertainty in the normalization factor
;filter - an input 2-D array, allowing for calibration using a selected
;         region (e.g., removing bright sources)
; Originally written by Zhiyuan Li
; revised by wqd June 6, 2007
;-
if n_params() eq 0 then begin
print,'CALL SEQUENCE -  cal_background,fname,bfname,ccd,emin=emin,emax=emax,filter=filter,bnorm=bnorm,ebnorm=ebnorm'
return
endif

if n_elements(emin) eq 0 then emin=10*1e3
if n_elements(emax) eq 0 then emax=14*1e3
emax0=emax
nccd=n_elements(ccd)
cr=fltarr(nccd)
br=fltarr(nccd)
tr=fltarr(nccd)

row={energy:0,ccd_id:0,x:0,y:0}
fhdr=headfits(fname,ext=1)
bfhdr=headfits(bfname,ext=1)
bexpt=sxpar(bfhdr,'livetime')
if bexpt eq 0 then bexpt=sxpar(bfhdr,'exposure')
if bexpt eq 0 then stop,'the background file exposure = 0! need a fix.'
bexpt=replicate(bexpt,nccd)

if n_elements(filter) ne 0 then begin
       sz=size(filter) ; for Chandra ACIS
       block=8192/sz[1]
       xmin=1.
       ymin=1.
endif

for i=0,nccd-1 do begin
    if ccd[i] ne 5 and ccd[i] ne 7 then emax=12*1e3
        ;the energy of the stow data for FI chips only goes to 13 keV.
    expt=sxpar(fhdr,'exposur'+strtrim(i,1))
    if expt eq 0 then expt=sxpar(fhdr,'exposure')
    if expt eq 0 then stop,'the source file exposure = 0! need a fix.'

    list_xray,fname,list,emin=emin,emax=emax,ccd=ccd[i],row=row,nct=nc
    list_xray,bfname,blist,emin=emin,emax=emax,ccd=ccd[i],row=row,nct=nb
    print,'nc= ',nc,' nb= ',nb, ' expt =',expt
    tr(i)=expt
    if (nc gt 0 and nb gt 0) then begin
       if n_elements(filter) eq 0 then begin
          cr(i)=nc
          br(i)=nb
       endif else begin
          list_image,list,xmin,ymin,cmap,sz[1],sz[2],block=block
          list_image,blist,xmin,ymin,bmap,sz[1],sz[2],block=block
          pix=where(filter gt 0, npix)
          cr(i)=total(cmap(pix))
          br(i)=total(bmap(pix))
       endelse
    endif else begin
       cr(i)=0
       br(i)=0
    endelse
    emax=emax0
endfor
bnorm=imdiv(imdiv(cr,tr),imdiv(br,float(bexpt)))
ebnorm=bnorm*sqrt(imdiv(1.,cr)+imdiv(1.,br))
print,"bnorm = ",bnorm," +- ",ebnorm
print,"they all should be close to 1 (within 30% or so; otherwise, something may be wrong the dat, e.g., mismatch of the gain files)"
    if !debug eq 3 then stop

end
