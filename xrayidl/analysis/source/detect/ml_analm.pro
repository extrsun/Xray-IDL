;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro ml_anal,list,cra,cdec,image_t,image_b,tblock=tblock, $
 radius=radius,threshold=threshold,infile=infile,outfile=outfile $
 ,append=append,blow=blow,bhigh=bhigh,sfrac=sfrac,dfac=dfac,chn=chn $
	,slow=slow,flow=flow,sigma=sigma,rc=rc,verb=verb,rsel=rsel
;-
; Fine analysis of individual sources using a maxium likelihood algorithm and
; a background map for estimating background contributions at source positions.
; The program can be ONLY used in the central region of a PSPC image where
; the PSF is well defined and vignetting is not a particularly serious problem.
; The PSF at large off-axis angles become strongly asymmetric.
; The background map is produced by a median smoothing source-subtracted 
; COUNT image. Outside the central region, one has to deal with the shadowing
; of the structures and severe vignetting differences in different energy 
; bands.
; Outside the central region, the program ANAL_V should be good enough with
; the poor resolution anyway.
;
; list - photon list containing counts of all on-source and background regions
; cra, cdec - the center of the observation
; image_t, image_b - ON-AXIS exposure and background COUNT images, don't 
; 			have to be the full size. The two images should
;			have the SAME size.
; tblock - the block size of the images (def = !block)
; radius - within which sources are to be analyzed (arcmin, def = 18')
; threshold - the threshold above which analyzed sources will be counted
;		(def = 3).
; infile - input file name (def = !seq_no+'_sou.dat') 
; outfile - output file name (def = !seq_no+'_souanal.dat')
; append - if set, append the source list to the existing file
; sfrac - fraction of counts in the on-source aperture 
;		(def = 0.9) used in get_snr.pro
; chn - if = 1, (def) only source positions will be fitted
;	if = 2, count rate and Gaussian width will also be fitted, and should
;		be used only when the S/N is large and the position is well
;		determined.
; flow, slow - lower limits to the flux and S/N ratio of a source to be 
;			analyzed.
; sigma - the gaussian size (arcsec; input) to be used in the fit if chn = 1,
;		overruling the 90% radius (def).
;	in units of 0.5" pixel (output).
; rc - the size of the aperture for collecting the counts used in the fit.
; 	if set, overruling the 90% radius and dfac.
; verb - if set, the initial and fitting parameters will be printed
; 
; writen by wqd, april 16, 1996
;+
if n_params() eq 0 then begin
print,'ml_anal,list,cra,cdec,image_t,image_b,tblock=tblock,'
print,' radius=radius,threshold=threshold,infile=infile,outfile=outfile'
print,' ,append=append,blow=blow,bhigh=bhigh,sfrac=sfrac,chn=chn'
print,' ,slow=slow,flow=flow,sigma=sigma,rc=rc,verb=verb,rsel=rsel'
return
endif
;
if n_elements(threshold) eq 0 then threshold=0.
if n_elements(tblock) eq 0 then tblock=!block
if n_elements(radius) eq 0 then radius=18. ;arcmin
if n_elements(chn) eq 0 then chn=1
if n_elements(dfac) eq 0 then begin
	if!instr eq 'h' then dfac=3 else dfac=1.
endif
if n_elements(infile) eq 0 then infile='sou_map'+strtrim(blow,2)+strtrim(bhigh,2)
;
if n_elements(outfile) eq 0 then outfile=infile+'_ml' 
if strupcase(outfile) ne 'NONE' then begin
	if keyword_set(append) eq 0 then $
	openw,un,outfile,/get_lun else $
	openw,un,outfile,/get_lun,/append
endif
;------------------------------------------------------------
; read the sources from the input source file
source_info,souno,sra,sdec,ssigma,scntr,ns=ns,soufile=infile $
	,slow=slow,flow=flow,/deg ;,/self

trans_dist,cra,cdec,sra,sdec,xp,yp,/deg
dis=sqrt(xp^2+yp^2)*!size_pixel/60.
if n_elements(radius) ne 0 then begin
	sel=where(dis le radius,nsel)
	if nsel eq 0 then stop,'stop: no source within the radius'
	dis=dis(sel)
	sra=sra(sel)
	sdec=sdec(sel)
	scntr=scntr(sel)
	xp=xp(sel)
	yp=yp(sel)
	ns=nsel
endif

; get aperture sizes
if !instr eq 'p' then begin
	if n_elements(sfrac) eq 0 then sfrac=0.85
	detect_params,dis,rs,blow=blow,bhigh=bhigh,perclimit=sfrac $
		,gsigma=sigma,gfrac=gfrac
	rs=rs*60./!size_pixel ;in units of pixels
;	if n_elements(gaus) ne 0 then sigma=gsigma
	sigma=sigma/!size_pixel ;in units of pixels
endif else begin
	if n_elements(sfrac) eq 0 then begin
 		if dfac eq 3 then sfrac=0.9 else begin
	 	; 0.9 see figure 3; should be good within about 10%.
		if dfac eq 1. then sfrac=0.5 else $
			read,'stop: please give sfrac'
 		endelse
	endif
	sigma=(2.35*0.5*(0.74^2+1.0+(1.3+0.0205*dis^2.349)^2)^0.5 > 3.)*2.
	;the 50% radius of the RHRI (p13 of the Guest Observing prog) 
	; in units of pixels
	rs=dfac*sigma
endelse
if n_elements(rc) ne 0 then rs=rs*0.+rc*120. ;in units of pixels
if !debug eq 1 then stop
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
nbin_s=rs^2*!pi 
bc=image_b(loc)*(nbin_s/float(tblock)^2) 
if chn eq 2 then np=3 else np=2
sse=fltarr(ns,np)
csv=fltarr(ns)
sz=csv
gcntr=csv
dsv=csv
status=0
xx=list.x & yy=list.y
if n_elements(maxni) eq 0 then maxni=100
k=0
rsel=[-999]
for kk=0L,(ns-1) do begin 
  xoo=xo(kk) & yoo=yo(kk)
  gsize=sigma(kk)
  for ni=1,maxni do begin
	;get the counts within the source aperture
	sel=where(((xx-xoo)^2+(yy-yoo)^2) le rs(kk)^2,cs)
	if cs gt 0 then $		
	   	mle_s,list(sel),bc(kk),rs(kk),para,status=status $
		,sigma=gsize,chn=chn,xo=xoo,yo=yoo,verb=verb,ffdd=ffdd
	if status ne 0 then begin
		status=0
		rsel=[rsel,kk]
		goto,aban
	endif
	if sqrt((para(0)-xoo)^2+(para(1)-yoo)^2) lt 0.5 then goto,next $
	else begin
	 	xoo=para(0) & yoo=para(1)
	 	if chn ge 2 then begin
			gsize=para(2) 
	 	endif
	endelse
  endfor
  print,'ni is greater than maxni'
  next:
  csv(k)=cs 
  dsv(k)=sqrt((para(0)-xo(kk))^2+(para(1)-yo(kk))^2)
  xo(k)=para(0) & yo(k)=para(1)
  if chn ge 2 then begin
	sz(k)=para(2)
	if chn eq 3 then gcntr(k)=para(3)
  endif
  for kkk =0, np-1 do sse(k,kkk)=ffdd(kkk,kkk)
  k=k+1
  print,'source No = ',k
  aban:
if !debug eq 2 then stop
endfor
ns=k
csv=csv(0:ns-1)
if n_elements(rsel) gt 1 then begin 
	rsel=rsel(1:*)
	remove,rsel,bc,rs,sigma,nbin_s
endif
get_snr,csv,nbin_s,bc,nbin_s,csn,snr,sfrac=sfrac,/bmap ;nbin_s is not used

;print,nbin_s,bc,csv,csn,snr,sfrac
cntr=csn/expt
if chn eq 3 then gcntr=gcntr/(expt*gfrac)

;get position in a standard SASS image
ra_dist = xo - hdim
dec_dist =yo - hdim
dis=sqrt(ra_dist^2+dec_dist^2)*(!size_pixel/60.)
trans_loct,ra_dist,dec_dist,cra,cdec,star_ra,star_dec,/deg
trans_degree,star_ra,star_dec,ra_hour,ra_min,ra_sec,dec_deg,dec_min,dec_sec,/deg
sse=sqrt(sse) ; in units of pixels
;record these source into output file:
kk=0
for k=0,(ns-1) do begin
	if snr(k) ge threshold then begin
	kk=kk+1
 	print, kk, ra_hour(k),ra_min(k),ra_sec(k) $
 	,dec_deg(k),dec_min(k),dec_sec(k),snr(k),cntr(k),scntr(k), $
 	dsv(k),dis(k),sse(k,0),sse(k,1),rs(k),sigma(k),sse(k,2) $
	,sz(k)-sigma(k),gcntr(k), $
		format='(I3, 2(2i4, f7.2), f9.2,2f9.5,1x,8f8.2,f9.5)'
	if strupcase(outfile) ne 'NO' then $
 	  printf,un, kk,' |', ra_hour(k),ra_min(k) $
	  ,ra_sec(k),' |',dec_deg(k),dec_min(k),dec_sec(k),' |',snr(k), $
   	  ' |',cntr(k),' |',scntr(k),' |', dsv(k),' |', dis(k),' |' $
		,sse(k,0),' |',sse(k,1),' |',rs(k),' |',sigma(k),' |' $
		,sse(k,2),' |',sz(k)-sigma(k),' |',gcntr(k),' |'  $
	  ,format='(I3,a2,2(2i4, f7.2,a2), f9.2,a2, 2(f9.5,a2),8(f8.2,a2),f9.5,a2)'
	endif
endfor
free_lun,un
return
end