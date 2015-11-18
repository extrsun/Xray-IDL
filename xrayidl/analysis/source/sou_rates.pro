;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro sou_rates,list,cra,cdec,image_t,image_b,tblock=tblock, $
 radius=radius,threshold=threshold,infile=infile,outfile=outfile $
 ,blow=blow,bhigh=bhigh,sfrac=sfrac $
	,slow=slow,flow=flow,rc=rc,verb=verb,ratioth=ratioth,fluxth=fluxth
;-
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
;		(def = [0.5,0.9]) used in get_snr.pro
; flow, slow - lower limits to the flux and S/N ratio of a source to be 
;			analyzed.
; rc - the size of the aperture for collecting the counts used in the fit.
; 	if set, overruling the 90% radius.
; verb - if set, the initial and fitting parameters will be printed
; 
; writen by wqd, april 16, 1996
;+
if n_params() eq 0 then begin
print,'sou_rates,list,cra,cdec,image_t,image_b,tblock=tblock,'
print,' radius=radius,threshold=threshold,infile=infile,outfile=outfile'
print,' ,blow=blow,bhigh=bhigh,sfrac=sfrac '
print,'	,slow=slow,flow=flow,rc=rc,verb=verb,ratioth=ratioth'
return
endif
;
if n_elements(threshold) eq 0 then threshold=0.
if n_elements(tblock) eq 0 then tblock=!block
if n_elements(radius) eq 0 then radius=18. ;arcmin
if n_elements(sfrac) eq 0 then sfrac=[0.5,0.9]
if n_elements(infile) eq 0 then $
	infile='sou_map'+strtrim(blow,2)+strtrim(bhigh,2)
;
if n_elements(outfile) eq 0 then outfile=infile+'_ml' 
if strupcase(outfile) ne 'NONE' then begin
	if keyword_set(append) eq 0 then $
	openw,un,outfile,/get_lun else $
	openw,un,outfile,/get_lun,/append
endif
;------------------------------------------------------------
; read the sources from the input source file
source_info,souno,sra,sdec,snr,scntr,ns=ns,soufile=infile $
	,slow=slow,flow=flow,/deg,text=text ;,/self
text=strmid(text,83,30)
trans_dist,cra,cdec,sra,sdec,xp,yp,/deg
dis=sqrt(xp^2+yp^2)*(!size_pixel/60.)
if n_elements(radius) ne 0 then begin
	sel=where(dis le radius,nsel)
	if nsel eq 0 then stop,'stop: no source within the radius'
	dis=dis(sel)
        snr=snr(sel)
	sra=sra(sel)
	sdec=sdec(sel)
	xp=xp(sel)
	yp=yp(sel)
	text=text(sel)
endif
ns=nsel

; get aperture sizes
if !instr eq 'p' then begin
	detect_params,dis,rs,blow=blow,bhigh=bhigh,perclimit=sfrac $
		,gsigma=sigma,gfrac=gfrac
;	if n_elements(gaus) ne 0 then sigma=gsigma
	sigma=sigma/!size_pixel ;in units of pixels
endif else begin
	if !instr eq 'h' then begin
	 theta=findgen(40)/2.
	 sel=where(sfrac eq 0.5,nsel)
	 if sfrac(0) eq 0.5 then begin
		rs=(2.35*0.5*(0.74^2+1.0+(1.3+0.0205*dis^2.349)^2)^0.5 > 3.)*2.
		frac=sfrac(1)
	 endif else frac=sfrac
	 psf_hri_frac,theta,offs,frac=frac
	 offs=offs*2. ; in units of pixels
	 linterp,theta,offs,dis,rs2
;	 linterp,theta,offs(*,1),dis,rs2
;	 linterp,theta,offs(*,0),dis,rs
	endif else stop,'which instrument is this?'
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

nbin_s=rs^2*!pi 
bc=image_b(loc)*(nbin_s/float(tblock)^2) 
nbin_s2=rs2^2*!pi 
bc2=bc*(nbin_s2/nbin_s)

np=2
csv=fltarr(ns)
csv2=csv

xx=list.x & yy=list.y
;----------------------------------
; loop over individual sources

if n_elements(maxni) eq 0 then maxni=100
for kk=0L,(ns-1) do begin 
  xoo=xo(kk) & yoo=yo(kk)
  sel=where(((xx-xoo)^2+(yy-yoo)^2) le rs2(kk)^2,cs2)
  csv2(kk)=cs2
  sel=where(((xx(sel)-xoo)^2+(yy(sel)-yoo)^2) le rs(kk)^2,cs)
  csv(kk)=cs
endfor
factor=1.e3
get_snr,csv,nbin_s,bc,nbin_s,csn,snr1,sfrac=sfrac(0),/bmap ;nbin_s is not used
get_snr,csv2,nbin_s2,bc2,nbin_s2,csn2,snr2,sfrac=sfrac(1),/bmap 
cntr=csn/expt*factor
cntr2=csn2/expt*factor
cntre=sqrt(csv)/expt/sfrac(0)*factor
cntre2=sqrt(csv2)/expt/sfrac(1)*factor
cntrdife=sqrt((csv2-csv+bc2-bc)/sfrac(1)^2+(csv+bc)*(1./sfrac(1)-1./sfrac(0))^2)/expt*factor
cntrdife=sqrt(cntrdife^2+(0.1*cntr2)^2)
ratio=(cntr2-cntr)/cntrdife
cntrr=cntr2/cntr
snr=snr1 > snr2
trans_degree,sra,sdec,ra_hour,ra_min,ra_sec,dec_deg,dec_min,dec_sec,/deg

;sre=sqrt(sse(*,0)^2+sse(*,1)^2)
;record these source into output 
kk=0
if n_elements(fluxth) eq 0 then fluxth=1.3
for k=0,(ns-1) do begin
	if snr(k) ge threshold then begin
	kk=kk+1
	if (ratio(k) gt ratioth and cntrr(k) gt fluxth) then $
		flagext=1 else flagext=0
 	print, kk, ra_hour(k),ra_min(k),ra_sec(k) $
 	,dec_deg(k),dec_min(k),dec_sec(k),snr(k),cntr(k),cntre(k),flagext $
 	,cntr2(k),cntre2(k),ratio(k),cntrr(k),text(k) $
	,format='(I3, 2(2i4, f7.2), 3f10.6,i3,2f10.6,2f8.1,a30)'
	if strupcase(outfile) ne 'NO' then $
 	  printf,un, kk,' |', ra_hour(k),ra_min(k) $
	  ,ra_sec(k),' |',dec_deg(k),dec_min(k),dec_sec(k), $
   	  ' |',snr(k),' |',cntr(k),' |',cntre(k),' |',flagext,' |' $
	,cntr2(k),' |',cntre2(k) $
	,' |' ,ratio(k),' |',cntrr(k),' |',text(k) $
	  ,format='(I3,a2,2(2i4, f7.2,a2), 3(f10.6,a2),i3,a2,2(f10.6,a2),f8.1,a2,f8.1,a2,a30)'
	endif
endfor
free_lun,un
return
end