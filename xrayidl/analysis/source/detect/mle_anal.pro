;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro mle_anal,list,image_t,image_b,xp,yp,xpnew,ypnew,sse,snr,cntr,sz $
,tblock=tblock,blow=blow,bhigh=bhigh,sfrac=sfrac,dfac=dfac,chn=chn $
,sigma=sigma,rc=rc,verb=verb
;+
; conduct a maximum likelihood analsis of sources
;
; list - count list
; image_t - exposure map
; image_b - background count map
; tblock - block used for constructing image_t and image_b
; xp, yp - the estimated positions of the sources
; xpnew, ypnew - the output best ML positions of the sources
; sse, snr, cntr, sz - the estimates of errors (position error plus gaussian
;	size error if chn=2), S/N ratio, count rate, and gaussian size 
; blow, bhigh - PSPC band boundary
; sfrac, dfac - radius and count rate correction factors
; chn - choice for the ML fitting algorithm: chn=1 for x and y fitting only.
; 	chn=2 (def) fits both the position and the Gaussian size.
; sigma - Gaussian size if chn=1 or an initial value if chn=2
; rc - if given, will be used as the source region aperture
; written by wqd, 9/16/97 for shift_obs.pro
;- 
if n_params() eq 0 then begin
print,'mle_anal,list,image_t,image_b,xp,yp,xpnew,ypnew,sse,snr,cntr,sz'
print,',tblock=tblock,blow=blow,bhigh=bhigh,sfrac=sfrac,dfac=dfac,chn=chn'
print,',sigma=sigma,rc=rc,verb=verb'
return
endif
;
if n_elements(tblock) eq 0 then tblock=!block
if n_elements(chn) eq 0 then chn=2
if n_elements(dfac) eq 0 then begin
	if!instr eq 'h' then dfac=3 else dfac=1.
endif
;
;------------------------------------------------------------
dis=sqrt(xp^2+yp^2)*!size_pixel/60.
ns=n_elements(xp)

; get aperture sizes
if !instr eq 'p' then begin
	if n_elements(infile) eq 0 then $
		infile='sou_map'+strtrim(blow,2)+strtrim(bhigh,2)
	detect_params,dis,rs,blow=blow,bhigh=bhigh,perclimit=sfrac $
		,gsigma=sigma,gfrac=gfrac
;	if n_elements(gaus) ne 0 then sigma=gsigma
	sigma=sigma/!size_pixel ;in units of pixels
endif else begin
	rs=dfac*(2.35*0.5*(0.74^2+1.0+(1.3+0.0205*dis^2.349)^2)^0.5 > 3.)/60.
	; in arcmin
	; dfac x the 50% radius of the RHRI (p13 of the Guest Observing prog)
	if n_elements(sfrac) eq 0 then begin
		if dfac eq 3 then sfrac=0.8 else read,'stop: please give sfrac'
	endif
	; 0.8 see figure 3; should be good within about 10%.
	sigma=rs*0.+6. ;units of pixels
endelse
if n_elements(rc) ne 0 then rs=rs*0.+rc
;---------------------------------
; get source pixel positions
case !instr of
  'p':  hdim=7680.5 ;FORTRAN Position as in the count list
  'h': 	hdim=4096.5
endcase
sz=size(image_t)
xo=hdim+xp
yo=hdim+yp
;
;get exposures and background counts at the source positions

loc=long(yp/tblock+sz(2)*0.5)*sz(1)+long(xp/tblock+sz(1)*0.5)
if n_elements(image_t) ne 0 then expt=image_t(loc) $
 else expt=loc*0.+exptime

;----------------------------------
; loop over individual sources
snr=fltarr(ns)
cntr=fltarr(ns)
sz=cntr
gcntr=cntr
rs=rs*120. ;in units of pixels
nbin_s=rs^2*!pi 
bc=image_b(loc)*(nbin_s/float(tblock)^2) 
csv=cntr
dsv=cntr
if chn eq 2 then np=3 else np=2
xx=list.x & yy=list.y
if n_elements(maxni) eq 0 then maxni=500
sse=fltarr(ns,np)

for k=0L,(ns-1) do begin 
  xoo=xo(k) & yoo=yo(k)
  gsize=sigma(k)
  for ni=1,maxni do begin
	;get the counts within the source aperture
	sel=where(((xx-xoo)^2+(yy-yoo)^2) le rs(k)^2,cs)
	if cs gt 0 then $		
	   	mle_s,list(sel),bc(k),rs(k),para $
		,sigma=gsize,chn=chn,xo=xoo,yo=yoo,verb=verb,ffdd=ffdd
	if sqrt((para(0)-xoo)^2+(para(1)-yoo)^2) lt 0.5 or cs eq 0 then begin
	 	;if cs =0, the source will be removed in the output 
	 	csv(k)=cs 
	 	dsv(k)=sqrt((para(0)-xo(k))^2+(para(1)-yo(k))^2)
	 	xo(k)=para(0) & yo(k)=para(1)
	 	if chn ge 2 then begin
			sz(k)=para(2)
			if chn eq 3 then gcntr(k)=para(3)
	 	endif
	 	goto,next
	endif else begin
	 	xoo=para(0) & yoo=para(1)
	 	if chn ge 2 then begin
			gsize=para(2) 
			if chn eq 3 then tcs=para(3)
	 	endif
	endelse
  endfor
  print,'ni is greater than maxni'
  next:
  for kk =0, np-1 do sse(k,kk)=ffdd(kk,kk)
;  print,'source No = ',k
endfor
get_snr,csv,nbin_s,bc,nbin_s,csn,snr,sfrac=sfrac,/bmap ;nbin_s is not used
cntr=csn/expt
if chn eq 3 then gcntr=gcntr/(expt*gfrac)

;get position in a standard SASS image
xpnew = xo - hdim
ypnew =yo - hdim
sse=sqrt(sse) ; in units of pixels
return
end
