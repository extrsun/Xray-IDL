pro match_obj,xls,ols,mrs,outv,offset,mx,mo,mmn=mmn,tagout=tagout,tagch=tagch,mrslow=mrslow,nmatch=nmatch
;+
; Match positions of two lists of objects within a radius
; 
; xls, ols - two object structure lists (RA and Dec in units of deg)
; mrs - match radius (in units of arcsec)
; mx, mo - match indexes in the two lists
; mmn - # of matches per xls source
; offset - the position separation of the counterpart from the X-ray
;          source (arcsec)
; outv - output string vectors contain the info: offset distance
;        (arcsec) & magnitudes & number of matches
; tagout - tag names of the entries in ols to be included in outv;
;          def=['j','h','k'] 
; tagch -  the index number of the tagout entry to be used for
;         selecting the one with the maximum value in case of multiple
;         matches (def=0); =-1 for selecting the entry with the least
;         offset distance
; mrslow - lower limit of the match radius (arcsec), useful for
;          estimating chance coincidence 
; nmatch - number of matches
;
; written by wqd, 6/3/2005
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - match_out,xls,ols,mrs,outv,mx,mo,mmn=mmn,tagout=tagout'
print,',tagch=tagch,mrslow=mrslow,nmatch=nmatch'
return
endif

if n_elements(tagout) eq 0 then tagout=['j','h','k'] ; for 2mass source lsit
if n_elements(tagch) eq 0 then tagch=0
if n_elements(mrslow) eq 0 then  mrslow=0.
xra=xls.ra & xdec=xls.dec
ora=ols.ra & odec=ols.dec
mrslow=0.*xra+mrslow ;make sure of the same dimension
mrs=0.*xra+mrs
ep=mrs/3600.
print,'Match in RA:' 
if !debug eq 2 then stop
close_match_radec,xra,xdec,ora,odec,mx,mo,ep,100 
trans_dist,xra(mx),xdec(mx),ora(mo),odec(mo),/deg,/das,angle=xosd ;match in dis
sel=where(xosd ge mrslow(mx) and xosd lt mrs(mx),nmatch)
if nmatch ne 0 then begin
	mx=mx(sel) & mo=mo(sel) & xosd=xosd(sel)
endif else begin
	print,'Number of match = 0!!!'
	return
endelse	
if !debug eq 3 then stop

;x-ray sources with matches (including duplicates)
ss=sort(mx)
mx=mx(ss)
mo=mo(ss)
xosd=xosd(ss)
mxls=xls(mx)
mols=ols(mo)
get_posi,mx,umx,mmn,nmatch
outv=replicate('',nmatch)
moind=intarr(nmatch)

tagin=tag_names(ols)
tagout=strupcase(tagout)
match,tagin,tagout,tagoutn,count=ntagout
tagoutn=tagoutn(sort(tagoutn))
indlo=0
for k=0,nmatch-1 do begin
	indhi=indlo+mmn(k)-1
	smols=mols(indlo:indhi)
	sxosd=xosd(indlo:indhi)
	if tagch ge 0 then begin
		mtagv=max(smols.(tagoutn(tagch)),mind)
;		offset=sxosd(mind)
	endif else offset=min(sxosd,mind) 
;        outv(k)=outv(k)+string(offset,'(f4.1)')+'&'
	for n=0,ntagout-1 do begin
		outv(k)=outv(k)+' '+tagout(n)+'='+ $
                        string(smols(mind).(tagoutn(n)),'(f4.1)')
        endfor
        if mmn(k) ne 1 then outv(k)=outv(k)+'& #='+strtrim(mmn(k),2)
        moind(k)=indlo+mind
        indlo=indhi+1
endfor
mx=umx
mo=mo(moind)
mm=where(mmn gt 1,nmm)
offset=xosd(moind)
print,'# of matches = ',nmatch
print,'# of the multiple matches = ',nmm
if !debug eq 3 then stop
return
end
	
