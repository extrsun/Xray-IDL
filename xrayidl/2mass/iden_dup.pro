pro iden_dup,ra,dec,rth,locout,pixsize=pixsize,loc=loc,dx=dx,dy=dy
;+
;  Find duplicated objects, which are detected in overlapping observations
;	and therefore have measured positions with a radius threshold
;
; ra, dec - ra and dec of individual objects (any unit should be fine)
; rth - radius threshold within which objects are considered to be identical
; 	units are the same as ra and dec
; pixsize - pixel size (the same units as ra and dec; def = rth)
; locout - vector contains 0 or 1; the latter is for objects with the nearest
; 	other object within rth.
;
; Algorithm: Place objects into a 2-D array for ease of locating neighboring
; objects
;
; Two steps are used: 1) find identical objects within each pixel (still
; use the radius
;	2) find if each object in a pixel is within the radius threshold
; of objects in the lower left, lower middle, lower right, and left pixels
; if yes, the object is idetical object of the other.
; Identical objects found  in step 1) are not used in step 2).
;
; written by wqd 5/28/2001 for analyzing raw 2MASS data
;
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - iden_dup,ra,dec,rth,locout,pixsize=pixsize'
return
endif

if n_elements(pixsize) eq 0 then pixsize=rth
srth=(double(rth)/pixsize)^2.
xx=(ra-min(ra))/pixsize ; x and y pixel locations
yy=(dec-min(dec))/pixsize
xl=long(xx)
yl=long(yy)
dx=max(xl)+1 ;x and y dimnsion of the array
dy=max(yl)+1

loc=yl*dx+xl ;array locations of the objects
;find identical objects in the same pixel
iden_sp,loc,xx,yy,srth,locout
;exclude those idetical objects from further consideration
ss=where(locout eq 0,nss)
;find identical objects relative to neighboring pixels
iden_np,loc(ss),xx(ss),yy(ss),dx,dy,srth,locout_n
locout(ss)=locout_n
stop
return
end

pro iden_sp,loc,xx,yy,srth,locout
;+
; find identical objects in the same pixel, to be called by iden_dup
;
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - iden_sp,loc,xx,yy,srth,locout'
return
endif

ns=n_elements(xx)
locout=lonarr(ns)

slocp=sort(loc)
sloc=loc(slocp) ;sorted pixel locations of objects
get_posi,sloc,loci,kdup,nloci
accum,kdup,akdup ;accumulated histogram of non-zero pixels 
ss=where(kdup gt 1,nss)
for k=0L,nss-1L do begin
 	sk=ss(k)
 	indxlo=akdup(sk)-kdup(sk)
 	indxhi=akdup(sk)-1
	slocps=slocp(indxlo:indxhi) ;pre-sort pixel locations
 	xv=xx(slocps)
 	yv=yy(slocps) 
 	locouts=intarr(kdup(sk))
 	for kk=kdup(sk)-1,1,-1 do begin ;work backwards 
 	 if(min((xv(kk)-xv(0:kk-1))^2+(yv(kk)-yv(0:kk-1))^2) lt srth) $
		then locouts(kk)=1
	print,kk,(xv(kk)-xv(0:kk-1))^2+(yv(kk)-yv(0:kk-1))^2
 	endfor 
 	locout(slocps)=locouts
endfor
stop
return
end

pro iden_np,loc,xx,yy,dx,dy,srth,locout
;+
; find identical objects relative to neighboring pixel, 
; to be called by iden_dup
;
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - iden_np,loc,xx,yy,dx,dy,srth,locout'
return
endif
ns=n_elements(xx)
locout=lonarr(ns)

slocp=sort(loc)
sloc=loc(slocp)
get_posi,sloc,loci,kdup,nloci
accum,kdup,akdup
ss=lindgen(nloci)

;get all objects in neighboring pixels together. To avoid redundancy, only
; the right, lower left, lower middel, and lower right pixels need to be
; included. These pixel locations are shifted to the reference pixel location 
loc_n=[loc-1,loc-(dx-1),loc-dx,loc-(dx+1)]
xx_n=[xx,xx,xx,xx]
yy_n=[yy,yy,yy,yy]

;sort the locations
slocp_n=sort(loc_n)
sel=where(loc_n(slocp_n) gt 0,nsel)
if nsel ne 0 then slocp_n=slocp_n(sel)
loc_n=loc_n(slocp_n)
xx_n=xx_n(slocp_n)
yy_n=yy_n(slocp_n)
; identify those pixels with non-zero neighboring objects
get_posi,loc_n,loci_n,kdup_n,nloci_n
accum,kdup_n,akdup_n
; identify the pixels that also contain non-zero original objects
tabinv_m,loci_n,loci,ssind_n ;locations in loci_n
ss_n=long(ssind_n)
ss_s=where(ssind_n eq ss_n,nss) 

;so we only need to consider objects in pixels with non-zero neighbors
if nss ne 0 then begin
 ss=ss(ss_s) ;locations of the first objects in the original pixels
 ss_n=ss_n(ss_s) ;locations of the first objects in the corresponding
		 ; neighboring pixels
 for k=0L,nss-1L do begin
 	sk=ss(k)
 	indxlo=akdup(sk)-kdup(sk)
 	indxhi=akdup(sk)-1
	slocps=slocp(indxlo:indxhi)
 	xv=xx(slocps)
 	yv=yy(slocps) 

	sk_n=ss_n(k)
 	indxlo_n=akdup_n(sk_n)-kdup_n(sk_n)
 	indxhi_n=akdup_n(sk_n)-1
 	xv_n=xx_n(indxlo_n:indxhi_n)
 	yv_n=yy_n(indxlo_n:indxhi_n)

 	locouts=intarr(kdup(sk))
 	for kk=0,kdup(sk)-1 do begin
 	 if(min((xv(kk)-xv_n)^2+(yv(kk)-yv_n)^2) lt srth) $
		then locouts(kk)=1
;print,kk,min((xv(kk)-xv_n)^2+(yv(kk)-yv_n)^2)
 	endfor 
 	locout(slocps)=locouts
 endfor
endif else print,'no overlapping stars in neighboring pixels'
stop
return
end
