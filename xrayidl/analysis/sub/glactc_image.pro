pro glactc_image,im,crad,cdecd,lodeg,ladeg,binval,nsel,imin=imin,imax=imax,block=block
;-
; calculate galactic coordinates of individual image pixels
; INputs:
; im - array
; crad, cdecd - ra and dec of the image center in units of degree
; imin,imax - the minimum and maxmum values of the image bins to be 
; 		included in the fit (def=0,10^20)
; OUTPUTS:
; lodeg, ladeg - the logitudes and latitutes of the selected bins (degree)
; binval - the bins selected in the calculation
; nsel - number of bins selected in the calculation
;
; called by galactc_fit.pro
;
; writen by WQD, 5/20/1993
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - glactc_image,im,crad,cdecd,lodeg,ladeg,binval,nsel'
print,',imin=imin,imax=imax,block=block'
return
endif
if n_elements(imin) eq 0 then imin=0.
if n_elements(imax) eq 0 then imax=1.e20
if n_elements(block) eq 0 then block=1.5*60./!size_pixel
;assuming iras  SSA image in units of !block 
;
sz=size(im)
xdim=sz(1)
ydim=sz(2)
xbc=0.5*(xdim-1.)
ybc=0.5*(ydim-1.)

pixloc=where(im ge imin and im le imax,nsel)
if nsel eq 0 then stop,'there is no bin selected'
binval=fltarr(nsel)
binval(0:*)=im(pixloc)
trans_loct,((pixloc mod xdim)-xbc)*block,((pixloc/xdim)-ybc)*block, $
  crad,cdecd,frad,fdecd,/degree
glactc_m,frad*(12./180.),fdecd,2000.,lodeg,ladeg,1
end
