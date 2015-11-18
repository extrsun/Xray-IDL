pro get_bmap,cbo,tbo,tbso,cbm,cth=cth,fra=fra,npo=npo,npd=npd,th1=th1,bmedian=bmedian
;+
; produce a background map for the map source detection
;
; cbo,tbo,tbso - count, exposure, and source-removed exposure maps
; cbm - the output background map
; fra - if given, the images will be binned by this factor
; bmedian - if set, use the median average
; cth - count threshold (def = 16 per bin after the rebining), only used 
;	if bmedian is set. 
; if bmedian is not set,
;	npo,npd,th1 - as in adp_gs_m.pro
;
; written by wqd, 6/25/2001
; modified to use the adaptive smoothing. wqd, 4/10/2002
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - pro get_bmap,cbo,tbo,tbso,cbm,cth=cth,fra=fra,npo=npo,npd=npd,th1=th1,bmedian=bmedian'
return
endif

cb=cbo
tb=tbo
tbs=tbso

if keyword_set(bmedian) then begin
    if n_elements(cth) eq 0 then cth=16.
    if n_elements(fra) eq 0 then begin
	fra=sqrt(cth/avg(cb(where(tbs gt 0))))
	nind=fix(alog(fra)/alog(2.))+1
	fra=2^nind
    endif
    image_comp3,cb,tb,tbs,fra=1./fra 
    print,'fra, average counts per bin = ',fra,avg(cb(where(tbs gt 0)))
;the resultant cb should contain at least 10 cts for a meaningful median avg.
    fb=imdiv(cb,tbs)
    image_median,3,fb,1.e-10*tb,tbs,fm,lmin=2,binmin=9
;1.e-10*tb is a fake background image
endif else begin
	if n_elements(fra) eq 0 then fra=4
	if n_elements(npo) eq 0 then npo=1
	if n_elements(npd) eq 0 then npd=0.2
	if n_elements(th1) eq 0 then th1=10
	cb(where(tbs eq 0))=0
	cb=image_comp(cb,1./fra)
	tb=image_comp(tb,1./fra)
	tbs=image_comp(tbs,1./fra)
	adp_gs_m,cb,tbs,cbm,tbm,npo=npo,npd=npd,th1=th1,filter=tb
	fm=imdiv(cbm,tbm)
    endelse
;to avoid the artificial reduction of background values at the edges of the
; image during the expansion of the image, it is preferred to assign a relatively high values.  
;assign empty bins with the average of the lower 1/5 bins
fmv=fm(where(tbs gt 0.,nbin))
fmv=fmv(sort(fmv))
fm(where(tb le 0.))=avg(fmv(0:nbin/5))
cbm=image_comp(fm,fra)*(1./fra^2)*tbo
return
end
