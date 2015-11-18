;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro sou_rates,list,cra,cdec,image_t,image_b,tblock=tblock, $
 radius=radius,threshold=threshold,infile=infile,outfile=outfile $
 ,blow=blow,bhigh=bhigh,sfrac=sfrac,dfac=dfac $
	,slow=slow,flow=flow,rc=rc,verb=verb,ratioth=ratioth
;-
;
; list - photon list containing counts of all on-source and background regions
; cra, cdec - the center of the observation
; image_t, image_b - ON-AXIS exposure and background COUNT images, don't 
; 			have to be the full size. The two images should
;			have the SAME size.
; tblock - the block size of the images (def = !block)
; radius - within which sources are to be analyzed (arcmin, def = 18')
; ratioth - the threshold above which source may be identified as being
;	extended (def = 3).
; outfile - output file name (def = !seq_no+'_souanal.dat')
; sfrac - fraction of counts in the on-source aperture 
;		(def = 0.9) used in get_snr.pro
; flow, slow - lower limits to the flux and S/N ratio of a source to be 
;			analyzed.
; rc - the size of the aperture for collecting the counts used in the fit.
; 	if set, overruling the 90% radius and dfac.
; verb - if set, the initial and fitting parameters will be printed
; 
; writen by wqd, april 16, 1996
;+
if n_params() eq 0 then begin
print,'sou_rates,list,cra,cdec,image_t,image_b,tblock=tblock,'
print,' radius=radius,threshold=threshold,infile=infile,outfile=outfile'
print,' ,blow=blow,bhigh=bhigh,sfrac=sfrac,dfac=dfac '
print,'	,slow=slow,flow=flow,rc=rc,verb=verb,ratioth=ratioth'
return
endif
;
if n_elements(threshold) eq 0 then threshold=0.
if n_elements(tblock) eq 0 then tblock=!block
if n_elements(radius) eq 0 then radius=18. ;arcmin
if n_elements(dfac) eq 0 then begin
	if!instr eq 'h' then dfac=3 else dfac=1.
endif
if n_elements(infile) eq 0 then $
	infile='sou_map'+strtrim(blow,2)+strtrim(bhigh,2)
;
if n_elements(outfile) eq 0 then outfile=infile+'_rates' 
if strupcase(outfile) ne 'NONE' then begin
	if keyword_set(append) eq 0 then $
	openw,un,outfile,/get_lun else $
	openw,un,outfile,/get_lun,/append
endif
;------------------------------------------------------------
; read the sources from the input source file
source_info,souno,sra,sdec,snr,scntr,ns=ns,soufile=infile $
	,slow=slow,flow=flow,/deg,comm=comm ;,/self

trans_dist,cra,cdec,sra,sdec,xp,yp,/deg
dis=sqrt(xp^2+yp^2)*!size_pixel/60.
if n_elements(radius) ne 0 then begin
	sel=where(dis le radius,nsel)
	if nsel eq 0 then stop,'stop: no source within the radius'
	dis=dis(sel)
        snr=snr(sel)
	sra=sra(sel)
	sdec=sdec(sel)
	xp=xp(sel)
	yp=yp(sel)
	comm=comm(sel)
endif
ns=nsel

; get aperture sizes
if !instr eq 'p' then begin
	if n_elements(sfrac) eq 0 then sfrac=0.9
	detect_params,dis,rs,blow=blow,bhigh=bhigh,perclimit=sfrac $
		,gsigma=sigma,gfrac=gfrac
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
	rs=sigma ;assuming sfrac1=0.5
	rs2=dfac*sigma
endelse
if n_elements(rc) ne 0 then begin
	rs=rs+rc*2. ;in units of pixels
	rs2=rs2+rc*2. 
endif
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
nbin_s2=rs2^2*!pi 
bc2=image_b(loc)*(nbin_s2/float(tblock)^2) 

np=2
csv=fltarr(ns)
csv2=csv

xx=list.x & yy=list.y
if n_elements(maxni) eq 0 then maxni=100
for kk=0L,(ns-1) do begin 
  xoo=xo(kk) & yoo=yo(kk)
  sel=where(((xx-xoo)^2+(yy-yoo)^2) le rs2(kk)^2,cs2)
  csv2(kk)=cs2
  sel=where(((xx(sel)-xoo)^2+(yy(sel)-yoo)^2) le rs(kk)^2,cs)
  csv(kk)=cs
endfor
sfrac1=0.5
get_snr,csv,nbin_s,bc,nbin_s,csn,snr2,sfrac=sfrac1,/bmap ;nbin_s is not used
get_snr,csv2,nbin_s2,bc2,nbin_s2,csn2,snr2,sfrac=sfrac,/bmap 
cntr=csn/expt
cntr2=csn2/expt
cntre=sqrt(csv)/expt
cntre2=sqrt(csv2)/expt
cntrdife=sqrt((csv2-csv+bc2-bc)/sfrac^2+(csv+bc)*(1./sfrac-1./sfrac1)^2)/expt
ratio=(cntr2-cntr)/cntrdife
cntrr=cntr2/cntr

trans_degree,sra,sdec,ra_hour,ra_min,ra_sec,dec_deg,dec_min,dec_sec,/deg

;sre=sqrt(sse(*,0)^2+sse(*,1)^2)
;record these source into output 
kk=0
for k=0,(ns-1) do begin
	if snr(k) ge threshold then begin
	kk=kk+1
	if (ratio(k) gt ratioth and cntrr(k) gt 1.1) then $
		flagext=1 else flagext=0
 	print, kk, ra_hour(k),ra_min(k),ra_sec(k) $
 	,dec_deg(k),dec_min(k),dec_sec(k),snr(k),cntr(k),cntre(k),flagext $
 	,cntr2(k),cntre2(k),ratio(k),cntrr(k),cntrr(k),comm(k) $
	,format='(I3, 2(2i4, f7.2), 3f10.6,i3,2f10.6,f8.1,i3,a40)'
	if strupcase(outfile) ne 'NO' then $
 	  printf,un, kk,' |', ra_hour(k),ra_min(k) $
	  ,ra_sec(k),' |',dec_deg(k),dec_min(k),dec_sec(k), $
   	  ' |',snr(k),' |',cntr(k),' |',cntre(k),' |',flagext,' |' $
	,cntr2(k),' |',cntre2(k) $
	,' |' ,ratio(k),' |',cntrr(k),' |',comm(k) $
	  ,format='(I3,a2,2(2i4, f7.2,a2), 3(f10.6,a2),i3,a2,2(f10.6,a2),f8.1,a2,f8.1,a2,a40)'
	endif
endfor
free_lun,un
return
end