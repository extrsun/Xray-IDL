pro map_back,ls,xmin,ymin,dim,bmin,bmax,tbo,tbso,ba,binfac=binfac,block=block,exam=exam,cth=cth
;+
; create a set of diffuse (source-removed) background maps and place them in
; an array to be used for map_ratio.pro
;
; ls - count list
; xmin,ymin - lower left corner pixel position
; dim - dimension of the image
; bmin, bmax - vectors containing lower and upper limits of the energy bands
;		in units of eV for Chandra data
; tbo - exposure mask
; tbso - source region removed exposure mask
; ba - output background array
; binfac - rebining factor used before the median smoothing
; block - image block factor
; exam - if set, one can exam individual background map during the running.
; cth - count threshold (def = 10 per bin after the rebining)
; 
; written by wqd, 6/8/2001
;-
if n_params() eq 0 then begin
print,'Calling procedure - map_back,ls,xmin,ymin,dim,bmin,bmax,tbo,tbso,ba'
print,',binfac=binfac,block=block,exam=exam'
return
endif
;the resultant cb should contain at least 10 cts for a meaningful median avg.
;
;if n_elements(binfac) eq 0 then binfac=8.
if n_elements(cth) eq 0 then cth=16.
nb=n_elements(bmin) 
sz=size(tbo)
ca=intarr(sz(1),sz(2),3)
ba=fltarr(sz(1),sz(2),3)
for k=0,nb-1 do begin
	list_image,ls,xmin,ymin,cb,dim,block=block $
		,emin=bmin(k),emax=bmax(k)
	tb=tbo & tbs=tbso

	binfac=sqrt(cth/avg(cb(where(tbs gt 0))))
	nind=fix(alog(binfac)/alog(2.))+1
	binfac=2^nind

	image_comp3,cb,tb,tbs,frac=1./binfac 
	print,'binfac, average counts per bin = ' $
		,binfac,avg(cb(where(tbs gt 0)))
;	ac=avg(cb(where(tbs gt 0)))
;	print,'avg(cb)= ',ac
;	if ac lt 20. then begin
;		print,'needs to be greater than 20 to get an uncertainty < 10%'
;		stop,'still proceed? Otherwise increase binfac.'
;	endif
	image_median,3,imdiv(cb,tbs),1.e-10*tb,tbs,fm,lmin=2,binmin=9
		;1.e-10*tb is a fake background image

	;to avoid the artificial reduction of background values at the edges 
	;of the image during the expansion of the image, it is preferred to 
	;assign a relatively high values. assign empty bins with the average 
	;of the lower 1/5 bins
	fmv=fm(where(tbs gt 0.,nbin))
	fmv=fmv(sort(fmv))
	fm(where(tb le 0.))=avg(fmv(0:nbin/5))

	cbm=image_comp(fm,binfac)/binfac^2
	if keyword_set(exam) then begin
		print,'you may type grey,cbm to examine the image'
		stop,'type c. to proceed.'
	endif
	ba(*,*,k)=cbm*tbo
endfor
return
end
