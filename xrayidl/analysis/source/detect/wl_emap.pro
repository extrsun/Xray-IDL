pro wl_emap,tb,tblock,l,xmin,ymin,dim,fb,tbe,fbe,sbe,block=block,emin=emin,emax=emax,sb=sb,cbe=cbe,sel=sel,xmine=xmine,ymine=ymine,imcoff=imcoff,impsf=impsf,eimpsf=eimpsf
;+
; get the flux map from the exposure map and count list and
; enlarge exposure map (and float background map), if needed to match
; the size of the count map. To be called by sou_main.pro
;
; tb - exposure map
; tblock - the block of pixel size
; block - the desired block of the output maps
; xmin,ymin - the lower left corner pixel coordinates of the original
;             tb image, used to get new coordinates for output count image
; l - list of events
; dim - the dimension of new image
; emin, emax - lower and upper limit of count energies for the output
;              image
; sb - background image
; impsf - input psf image 
; eimpsf - output sub psf image
;
;*Outputs:
; fb,fbe - output flux and flux error maps (background subtracted)
; tbe,sbe - exposure and back ground maps
; sel - index of selected counts in the list for the output image
; xmine,ymine - the lower left corner pixel coordinates of the
;                central image
; imcoff -
;
; written by wqd, June 18, 2003
; include the sub psf image extraction, wqd, April 7, 2006
;-
if n_params() eq 0 then begin
print,'Calling procedure - wl_emap,tb,tblock,l,xmin,ymin,dim,fb,tbe,fbe,sbe'
print,',block=block,emin=emin,emax=emax,sb=sb,cbe=cbe,sel=sel,xmine=xmine,ymine=ymine,imcoff=imcoff,impsf=impsf,impsfe=impsfe'
return
endif
if n_elements(imcoff) eq 0 then imcoff=[0,0]
sz=size(tb)
if sz(0) ne 2 then begin
    print,'the first item should be a 2-D array!!!'
    return
endif
if sz(1) ne sz(2) then begin
    print,'the exposure map needs to be a square!!!'
    return
endif

if n_elements(block) eq 0 then begin
    block=tblock 
    fra=1
endif else begin
    fra=tblock/block ;ratio of the block sizes, assuming square image
    if fra ne float(tblock)/block then begin
        stop,'tblock/block ne tblock/block!' 
        return
    endif
endelse
nsb=n_elements(sb) 
if fra ne 1 then begin
    hodim=nint(dim*0.5/fra) ;assuming square image
    tbe=image_comp(image_cut(tb,hodim,/rec,/pix,imcoff=imcoff),fra)
    if n_elements(impsf) ne 0 then $
      eimpsf=image_comp(image_cut(impsf,hodim,/rec,/pix,imcoff=imcoff),fra)
    if nsb ne 0 then $
      sbe=image_comp(image_cut(sb,hodim,/rec,/pix,imcoff=imcoff),fra)/fra^2
    dim=hodim*2*fra ;redefine the dimension so that they are consistent
    dhdim=(sz(1)*long(tblock)-long(dim)*block)/2 
                  ;assuming new and old maps are co-centered 
;    xmine=xmin+dhdim
;    ymine=ymin+dhdim
endif else begin
;    xmine=xmin
;    ymine=ymin
    if nsb ne 0 then sbe=sb
    tbe=tb
endelse 
;the low left pixel of the subimage
xmine=nint(!pref+imcoff(0)*tblock-float(dim)*block/2.) 
ymine=nint(!pref+imcoff(1)*tblock-float(dim)*block/2.)


list_image,l,xmine,ymine,cbe,dim,block=block,emin=emin,emax=emax,sel=sel
if nsb ne 0 then begin
    fb=imdiv(cbe-sbe,tbe)
    fbe=imdiv(sbe,tbe^2) 
endif else begin
    fb=imdiv(cbe,tbe)
    fbe=0 ;basically not properly defined so that it will be 
                                ;caught later if used.
endelse
return
end
