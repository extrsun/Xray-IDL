pro blist_image,ba,blist,xmin,ymin,bmin,bmax,imdim,imblock,bnorm,hdr,imccd=imccd,nofits=nofits,bimfroot=bimfroot,bbout=bbout
;+
; cast a background event list into images in various energy bands
; 
; for multiple pointings, fidv needs to be provided (not yet tested)
; ba - output image array 
; blist - input event list
; xmin,ymin,imdim,bmin,bmax,imblock - all these parameters should be the
;                               same as those in evt_image
; bnorm - normalization of the background image (e.g., exposure
;         ratio), which may be chip-dependent
; imccd - vector containing ccd numbers of bnorm correction (same dimension)
; hdr - header of a 2-D image (used for fits image output)
; nofits - if set, no individual band fits images will be outputed 
; bimfroot - the root name of the output background image 
;           files (def = 'b_')
; bbout  - if set, the summed broad band map is outputed
;
; written by wqd, June 7, 2007
; revised to account for bnorm correction for individual ccd chips,
; wqd, June 29, 2007
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- blist_image,ba,blist,xmin,ymin,bmin,bmax,imdim,imblock,bnorm,hdr,imccd=imccd,nofits=nofits,bimfroot=bimfroot,bbout=bbout'
return
endif

;imdim=sxpar(hdr,'naxis*')
bnorm=float(bnorm)
nbfname=n_elements(bnorm)
if nbfname eq 1 then nccd=1 else begin
    nccd=n_elements(imccd)
    if nccd ne nbfname then begin
        print,'# of ccd needs to be the same as # of bnorm!!'
    endif
endelse
nb=n_elements(bmin)
if n_elements(bimfroot) eq 0 then bimfroot='b_'
ba=fltarr(imdim(0),imdim(1),nb)
;eba=fltarr(imdim(0),imdim(1),nb) ;csmooth does not seem to use such
;background error!!!
;ebb=0.
for k=0,nb-1 do begin
    bb=0
         if nccd eq 1 then begin
              list_image,blist,xmin,ymin,imarr,imdim(0) $
                ,imdim(1),block=imblock,emin=bmin(k),emax=bmax(k),nsel=nsel
              bb=bb+bnorm(0)*imarr
;              ebb=ebb+(bnorm(0))^2*imarr
         endif else begin
              for n=0,nccd-1 do begin
                  sel=where(blist.ccd_id eq imccd(n),nsel)
                 if nsel ne 0 then begin
                     list_image,blist(sel),xmin,ymin,imarr,imdim(0), $
                       imdim(1),block=imblock,emin=bmin(k),emax=bmax(k)
;                     print,n,mean(imarr)
                     bb=bb+bnorm(n)*imarr
                 endif 
             endfor 
          endelse 
        if n_elements(nofits) eq 0 then $
		writefits,bimfroot+strtrim(k+1,2)+'.fits',bb,hdr
        ba(*,*,k)=bb
endfor 
if keyword_set(bbout) then begin
    tba=total(ba,3)
    writefits,bimfroot+'_b.fits',tba,hdr
;    writefits,'eb_b.fits',sqrt(ebb),hdr
    writefits,'e'+bimfroot+'_b.fits',tba*0.,hdr
endif
if !debug eq 3 then stop
return
end
