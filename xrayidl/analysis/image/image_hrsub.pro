;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME
;	image_hr
;
;*PURPOSE:
;	obtain a smoothed flux image by making arithmetic average of the data
;	including  adjacent bins in count, background, and exposure images.
;
;*CALLING SEQUENCE:
;	image_av,maxbox,npb,ba,et,f,
;	pixel_size=pixel_size,fluxerr=fluxerr
;
;*PARAMETERS:
; INPUTS:
;	maxbox - one-side dimension of the maximum average box
;	npb,ba,et - count, background, exposure images
;	pixel_size - pixel size (before a merge)
;	fluxerr - the criterion of the flux error in the average
;
; OUTPUTS
;	f - flux image (in units of ct / s / arcmin^2)
;
;*PROCEDURE:
;       The box size increases from 1 to maxbox (in regions where sources 
; are subtracted the limitation on maxbox is relaxed). The flux is calculated
; when  the flux error criterion is met. Otherwise, the exposure of that bin
; is set equal to zero
;
;*EXAMPLES:
;	image_av,5,array_c,array_b,array_t,f,fluxerr=0.0001
;
;*RESTRICTIONS:
;
;*NOTES:
;
;
;*SUBROUTINES CALLED:
;
;*MODIFICATION HISTORY:
;	writen 3 AUG 1992 (WQD)
;
;-
;---------------------------------------------------------------------------
pro image_hr,maxbox,npb,npb2,ba,et,f,fluxerr=fluxerr,lmin=lmin,sel=sel $
,bbh=bbh,bbs=bbs,nosub=nosub
;
; maxbox --- the maximum bin number in one side of the average
;------------------------------------------------------------------------
if n_params() eq 0 then begin
print,'CALL SEQUENCE -,maxbox,npb,npb2,ba,et,f'
print,',fluxerr=fluxerr,lmin=lmin'
return
endif
;
if !debug eq 1 then stop
etm=et
if n_elements(lmin) eq 0 then lmin=1
sz=size(npb)
xdim=sz(1)
ydim=sz(2)
;
if n_elements(fluxerr) eq 0 then fluxerr=0.15
fluxerr=fluxerr^2
;
f=fltarr(xdim,ydim)
ef=f
if n_elements(bbh) eq 0 then bbh=0
if n_elements(bbs) eq 0 then bbs=0
if keyword_set(nosub) ne 0 then begin
	bbdif=0 & bbtotal=0
endif else begin
	bbdif=bbh-bbs
	bbtotal=bbh+bbs
endelse
loc=lindgen(xdim,ydim)
ii=loc mod xdim
jj=loc/xdim
nsel=n_elements(sel)
if nsel ne 0 then begin
	ii=ii(sel)
	jj=jj(sel)
	loc=loc(sel)
endif else nsel=xdim*ydim
;---------------------------------------------------
for k=0,nsel-1 do begin
 i=ii(k)
 j=jj(k)
 if ba(i,j) NE 0. then begin ; exposure at this bin
	Lm=Lmin                     
  repeat begin 
	Lm=Lm+1
;	mkk=(2*Lm+1)*(2*Lm+1)
	mink=(i-Lm) > 0
	maxk=(i+Lm) < (xdim-1)
	minm=(j-Lm) > 0
	maxm=(j+Lm) < (ydim-1)
	etms=etm(mink:maxk,minm:maxm)
	npbs=npb(mink:maxk,minm:maxm)
	npbs2=npb2(mink:maxk,minm:maxm)
	good=where(etms GT 0.,count)
	if count eq 0 then goto, relax
	tc=total(npbs(good))
	tb=total(npbs2(good))
	tt=tc+tb-bbtotal*count
	if tt le 0 then goto, relax
	flux=(tc-tb-bbdif*count)/tt
	eflux=((1.-flux)/tt)^2*(tc > count*bbh)+((1.+flux)/tt)^2*(tb > count*bbs)
;
if !debug eq 2 then stop
	if eflux LT fluxerr  then begin
	f(i,j)=flux 
	ef(i,j)=eflux
	goto, back
	endif
;
	if etm(i,j) EQ 0. then begin
relax:	Lmax=maxbox+10
	endif else Lmax=maxbox
;
  endrep until Lm EQ Lmax
	et(i,j)=0.
	f(i,j)=-1.
 endif
;
back: 
endfor 
if !debug eq 1 then stop
;
return
end

