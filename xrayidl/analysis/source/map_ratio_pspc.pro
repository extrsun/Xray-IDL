;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro map_ratio,list,cra,cdec,image_t,image_b,tblock=tblock, $
 radius=radius,threshold=threshold,infile=infile,outfile=outfile $
 ,append=append,sfrac=sfrac,rc=rc
;-
; Calaculate count rates in individual bands with 
; background and exposure maps and a photon list
;
; list - photon list containing counts of all on-source and background regions
; cra, cdec - the center of the observation
; image_t, image_b - stacked exposure and background COUNT images in
; 		the 2-2, 4-5, and 6-7 bands.
;		The two images must have the SAME size.
; tblock - the block size of the images (def = !block)
; radius - within which sources are to be analyzed (arcmin, def = 18')
; threshold - the threshold above which analyzed sources will be counted
;		(def = 3).
; infile - input file name (def = !seq_no+'_sou.dat') 
; outfile - output file name (def = !seq_no+'_souanal.dat')
; append - if set, append the source list to the existing file
; sfrac - fraction of counts in the on-source aperture 
;		(def = 0.9) used in get_snr.pro
; rc - the size of the aperture for collecting the counts used in the fit.
; 	if set, overruling the 90% radius and dfac.
; verb - if set, the initial and fitting parameters will be printed
; 
; writen by wqd, april 24, 1996
;+
if n_params() eq 0 then begin
print,'Calling procedure - '
print,'map_ratio,list,cra,cdec,image_t,image_b,tblock=tblock,'
print,' radius=radius,threshold=threshold,infile=infile,outfile=outfile'
print,' ,append=append,sfrac=sfrac,rc=rc'
return
endif
;
if n_elements(threshold) eq 0 then threshold=0.
if n_elements(tblock) eq 0 then tblock=!block
if n_elements(radius) eq 0 then radius=18. ;arcmin
if n_elements(sfrac) eq 0 then sfrac=0.85
if n_elements(infile) eq 0 then infile='sou_22all'
;
nb=3
blow=[2,4,6]
bhigh=[2,5,7]
chlow=[20,52,91]
chhigh=[41,90,201]
if n_elements(outfile) eq 0 then outfile=infile+'_ratio' 
if strupcase(outfile) ne 'NO' then begin
	if keyword_set(append) eq 0 then $
	openw,un,outfile,/get_lun else $
	openw,un,outfile,/get_lun,/append
endif
;------------------------------------------------------------
; read the sources from the input source file
source_info,sn,sra,sdec,sigma,cntr,ns=ns,soufile=infile $
	,slow=threshold,/deg
trans_dist,cra,cdec,sra,sdec,xp,yp,/deg
dis=sqrt(xp^2+yp^2)*!size_pixel/60.
if n_elements(radius) ne 0 then begin
	sel=where(dis le radius,nsel)
	if nsel eq 0 then stop,'stop: no source within the radius'
	sn=sn(sel)
	dis=dis(sel)
	sra=sra(sel)
	sdec=sdec(sel)
	cntr=cntr(sel)
	sigma=sigma(sel)
	xp=xp(sel)
	yp=yp(sel)
	ns=nsel
endif

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
cntra=fltarr(ns,nb)
cntrea=cntra
csv=fltarr(ns)
for n=0,nb-1 do begin
	if n_elements(rc) ne 0 then rs=rc else $
	 detect_params,dis,rs,blow=blow(n),bhigh=bhigh(n),perclimit=sfrac
	image_t2=image_t(*,*,n)
	expt=image_t2(loc) 
	; loop over individual sources
	rs=rs*120. ;in units of pixels
	nbin_s=rs^2*!pi 
	image_b2=image_b(*,*,n)
	bc=image_b2(loc)*(nbin_s/float(tblock)^2)
	ls=list(where(list.pi ge chlow(n) and list.pi le chhigh(n)))
	for k=0L,(ns-1) do begin 
		sel=where(((ls.x-xo(k))^2+(ls.y-yo(k))^2) le rs(k)^2,cs)
		csv(k)=cs 
	endfor
	get_snr,csv,nbin_s,bc,nbin_s,csn,snr,sfrac=sfrac,/bmap 
		;nbin_s is not used
	cntra(*,n)=csn/expt
	cntrea(*,n)=snr
endfor
cntrea=cntra/cntrea

trans_degree,sra,sdec,ra_hour,ra_min,ra_sec,dec_deg,dec_min,dec_sec,/deg
for k=0,(ns-1) do begin
 	print, sn(k), ra_hour(k),ra_min(k),ra_sec(k) $
 	,dec_deg(k),dec_min(k),dec_sec(k),sigma(k),cntr(k) $
	,cntra(k,0),cntrea(k,0),cntra(k,1),cntrea(k,1),cntra(k,2),cntrea(k,2),$
		format='(I3, 2(2i4, f7.2), f9.2,7f9.5)'
	if strupcase(outfile) ne 'NO' then $
 	  printf,un, sn(k),' |', ra_hour(k),ra_min(k) $
	  ,ra_sec(k),' |',dec_deg(k),dec_min(k),dec_sec(k),' |',sigma(k), $
   	  ' |',cntr(k),' |',cntra(k,0),' |', cntrea(k,0),' |',cntra(k,1),' |' $
		,cntrea(k,1),' |',cntra(k,2),' |',cntrea(k,2),' |',dis(k),' |'  $
	  ,format='(I3,a2,2(2i3, f7.2,a2), f8.2,a2, 8(f8.5,a2))'
endfor
if strupcase(outfile) ne 'NO' then free_lun,un
end