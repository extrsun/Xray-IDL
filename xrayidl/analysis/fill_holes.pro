pro fill_holes,cbm,cims,tim,mfilter,cimf,xmin=xmin,ymin=ymin,menergy=menergy,xsm=xsm,ysm=ysm,esm=esm,block=block
;+
; Name:
;  fill_holes
; PURPOSE:
;  fill holes in count image(s) (left from source removal) with
; random counts determined from smoothed diffuse count image(s)
;
;*INPUTS:
; cbm - smoothed diffuse count image(s), which could include several bands
; cims - count image(s) with the holes
; tim - exposure map(s) without the holes
; mfilter - image(s) contains the fraction of exposure that need to be
;          filled, e.g., filter=(t-ts)/t
;
;*Optional Inputs:
; xmin,ymin - lower left corner pixel position of the image (def =1)
; block - block size of the image. 
; menergy - mean energies of the bands
; The above optional parameters are needed only when parameters of 
; individual random counts are outputed in the original pixel coordinates
; 
;*OUTPUTS:
; cimf - count image(s) with the holes filled
;
; xsm,ysm, ysm - X and Y coordinates as well as mean band energies of
;                individual simulated counts.
;
;*MODIFICATION HISTORY:
; written by wqd May 25, 2003
; add outputs of individual simulated counts, wqd, Aug 23, 2003
;
;-
if n_params() eq 0 then begin
print,'CALL SEQUENCE - fill_holes,cbm,cims,tim,mfilter,cimf'
print,',xmin=xmin,ymin=ymin,menergy=menergy,xsm=xsm,ysm=ysm,esm=esm,block=block'
return
endif

if n_elements(xmin) eq 0 then xmin=1
if n_elements(ymin) eq 0 then ymin=1
if n_elements(block) eq 0 then block=1
sz=size(cims)
if sz(0) eq 2 then nim=1 else nim=sz(3)
neband=n_elements(menergy)
if neband ne 0 and neband ne nim then begin
    print,'Number of menergy elements = number of bands!!!'
    return
endif
xdim=sz(1)
ydim=sz(2)
cimf=cims*0

;determine how many filters are there (ie., a universal filter for all bands)
fsz=size(mfilter)
if fsz(0) eq 2 then nfilter=1 else nfilter=fsz(3)
if nfilter eq 1 then filter=mfilter ;single filter applies to all bands

for k=0,nim-1 do begin
    if nfilter gt 1 then filter=mfilter(*,*,k)
    tdif=tim(*,*,k)*filter
    ran_pos,where(tdif gt 0.),cbm(*,*,k)*filter,xmin,ymin,xs,ys,block=block
    list_image,l,xmin,ymin,rim,sz(1),sz(2),block=block,xp=xs,yp=ys,nsel=nsel
    cimf(*,*,k)=cims(*,*,k)+rim
    if neband ne 0 then begin
        if k eq 0 then begin
            xsm=xs & ysm=ys & esm=replicate(menergy(k),nsel)
        endif else begin
            xsm=[xsm,xs] & ysm=[ysm,ys] & esm=[esm,replicate(menergy(k),nsel)]
        endelse 
    endif 
endfor
return
end
