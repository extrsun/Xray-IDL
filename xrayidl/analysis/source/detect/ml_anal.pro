;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro ml_anal,elist,cra,cdec,image_t,image_b,tblock=tblock,rxp=rxp,ryp=ryp $
 ,radius=radius,threshold=threshold,infile=infile,outfile=outfile $
 ,sfrac=sfrac,chn=chn,slow=slow,sigma=sigma,rc=rc,verb=verb $
 ,psffile=psffile,blow=blow,bhigh=bhigh,instr=instr,slist=slist,dig=dig $
 ,inslist=inslist,sradius=sradius,nooapsyserr=nooapsyserr,fac_ssr=fac_ssr $
,silent=silent,impsf=impsf,detagname=detagname
;+
; Analysis of individual sources using a maxium likelihood algorithm and
; a background map for estimating background contributions at source positions.
;
; elist - photon list containing counts of all on-source and background regions
; cra, cdec - axis RA and Dec (deg)
; image_t, image_b - ON-AXIS exposure and background COUNT images, don't 
; 			have to be the full size. The two images should
;			have the SAME size.
; tblock - the block size of the images (def = !block)
; rxp, ryp - pixel position of the image (image_t and image_b) center, 
; radius - the off-axis radius within which sources are to be analyzed 
;	(arcmin, def = 18')
; threshold - the threshold above which analyzed sources will be counted
;		(def = 3).
; infile - input file name (def = !seq_no+'_sou.dat') 
; outfile - output file name (def = !seq_no+'_souanal.dat')
; sfrac - fraction of counts in the on-source aperture 
;		(def = 0.9) used in get_snr.pro
; chn - if = 1, (def) only source positions will be fitted
;	if = 2, count rate and Gaussian width will also be fitted, and should
;		be used only when the S/N is large and the position is well
;		determined.
; slow - lower limits to the S/N ratio of a source to be 
;			analyzed.
; sigma - the gaussian size (arcsec; input) to be used in the fit if chn = 1,
;		overruling the 90% radius (def).
;	in units of 0.5" pixel (output).
; rc - the size of the aperture for collecting the counts used in the
;      fit (now in units of pixel).
; 	if set, overruling the 90% radius and dfac.
; fac_ssr - choose a fraction of sr and if not =1
; no scattered fraction will then be made
; verb - if set, the initial and fitting parameters will be printed
; nooapsyserr - if set, no offaxis position uncertainty dependence will be added
; sradius - on-axis systematic error (minimum radius) of the source positions
;		in units of arcsec. 
; detagname - an alternative coordinates 
;	with the tagname = detagname+'x' and detagname+'y'
;       for off-axis angle calculation of individual observations
;*parameters for the energy-encircled radius:
; blow,bhigh - lower and upper limits of the energy band for calculating
;	the energy-encircled radius 
; slist - source structure
; dig - digit used for calcuate the IAUID
;
; writen by wqd, april 16, 1996
; motified for Chandra ACIS images. wqd, 6/14/2001
; converted to output stucture and fits file. wqd, 4/25/2002
;-
if n_params() eq 0 then begin
print,'ml_anal,elist,cra,cdec,image_t,image_b,tblock=tblock,rxp=rxp,ryp=ryp,'
print,' radius=radius,threshold=threshold,infile=infile,outfile=outfile'
print,' ,sfrac=sfrac,chn=chn,slow=slow,sigma=sigma,rc=rc,verb=verb'
print,' ,psffile=psffile,blow=blow,bhigh=bhigh,instr=instr,slist=slist,dig=dig'
print,' ,inslist=inslist,sradius=sradius,nooapsyserr=nooapsyserr'
print,',silent=silent,fac_ssr=fac_ssr,impsf=impsf,detagname=detagname'
return
endif
;
if n_elements(silent) eq 0 then  silent=0
if n_elements(threshold) eq 0 then threshold=0.
if n_elements(tblock) eq 0 then tblock=!block
if n_elements(radius) eq 0 then radius=1.e10
if n_elements(chn) eq 0 then chn=1
;------------------------------------------------------------
; read the sources from the input source file
if n_elements(inslist) eq 0 then begin
;	source_info,souno,sra,sdec,soufile=infile,slow=slow,/deg $
    sou_fits_info,infile,inslist,slow=slow,/all
endif
sel=where(inslist.snr gt slow,nsel)
if nsel eq 0 then stop,'no source is selected!!!'
sra=inslist.ra
sdec=inslist.dec
bc=inslist.bcount
expt=inslist.expt

trans_dist,cra,cdec,sra,sdec,xp,yp,/deg,pixsize=!size_pixel
dis=sqrt(xp^2+yp^2)*(!size_pixel/60.) 
;off-axis distance, which may be different from off-image center distance

; get source pixel positions in the data coordinates
xo=!pref+xp 
yo=!pref+yp
;---------------------------------
; select sources
if n_elements(rxp) ne 0 then begin
	xic=xo-rxp	;off image center positions in units of data pixel
	yic=yo-ryp
endif else begin
	if !instr eq 'aciss' or !instr eq 'acisi' then $
		stop,'for '+!instr+' rxp and ryp are needed'
	xic=xo
	yic=yo
endelse
sz=size(image_t)
hdimx=sz(1)*0.5*tblock ;half image size in units of data pixel
hdimy=sz(2)*0.5*tblock

;select only sources within the off-axis radius and the image
s_sel=where(dis le radius and abs(xic) lt hdimx $
	and abs(yic) lt hdimy,ns)
if ns eq 0 then stop,'stop: no source within the radius'
msel,s_sel,dis,sra,sdec,sdec,xo,yo,xic,yic
;------------------------------------------------
; get aperture sizes
nimpsf=n_elements(impsf)
;if nimpsf ne 0 then rs=inslist.sradius else $
;  psf_params,dis,rs,perclimit=sfrac,gfrac=gfrac,psffile=psffile $
;             ,blow=blow,bhigh=bhigh 
rs=inslist.sradius
if n_elements(fac_ssr) eq 0 then fac_ssr=1.
rs=fac_ssr*rs > 1. ;to include at least the four neighboring pixels

if n_elements(sigma) eq 0 then sigma=rs*0.5 > 1.

if n_elements(rc) ne 0 then rs=rs*0.+rc ;in units of pixels
;----------------------------------
;get exposures and background counts at the source positions
;loc=long(yic/tblock+sz(2)*0.5)*sz(1)+long(xic/tblock+sz(1)*0.5)
;expt=image_t(loc)
srs=rs^2
nbin_s=srs*!pi 
;bc=image_b(loc)*(nbin_s/float(tblock)^2) 
;----------------------------------------------------------------
; loop over individual sources
if chn eq 2 then np=3 else np=2
sse=fltarr(ns,3)
csv=fltarr(ns)
sz=csv
gcntr=csv
dsv=csv
status=0

xp=elist.x & yp=elist.y

if nimpsf ne 0 then begin
    if n_elements (detagname) eq 0 then $
      dis=sqrt((xp-!pref)^2+(yp-!pref)^2)*(!size_pixel/60.) $
          else begin
        tagin=tag_names(elist)
        match,tagin,strupcase(detagname+'x'),xtagn
        match,tagin,strupcase(detagname+'y'),ytagn
        oxp=elist.(xtagn(0)) & oyp=elist.(ytagn(0)) ;xragn has to be scalar
        dis=sqrt((oxp-!pref)^2+(oyp-!pref)^2)*(!size_pixel/60.)
        psf_params,dis,crs,perclimit=sfrac,gfrac=gfrac,psffile=psffile $
               ,blow=blow,bhigh=bhigh 
        scrs=(crs > 1.)^2
    endelse
endif 

if n_elements(maxni) eq 0 then maxni=1000
;k=0
;rfactor=(1.+tblock/rs)
for kk=0L,(ns-1) do begin 
  if silent eq 0 then  print,'source No = ',kk,bc(kk),rs(kk)
  xoo=xo(kk) & yoo=yo(kk)
  gsize=sigma(kk)
  for ni=1,maxni do begin
	;get the counts within the source aperture
;	if nimpsf ne 0 then sel=where(((xp-xoo)^2+(yp-yoo)^2) le scrs,cs) $
;          else sel=where(((xp-xoo)^2+(yp-yoo)^2) le srs(kk),cs)
      if ni eq 1 then begin ;change made on July 30, 2007, adding kk to scrs
          if nimpsf ne 0 then r2=(sqrt(scrs(kk))+tblock*0.5)^2 else $
            r2=(sqrt(srs(kk))+tblock*0.5)^2
           ;increasing the radius for a factor of 2 for the initial centroiding
           ;because the pixalization of the map search
      endif else if nimpsf ne 0 then r2=scrs(kk) else r2=srs(kk)
      sel=where(((xp-xoo)^2+(yp-yoo)^2) le r2,cs)
	if cs gt bc(kk) then begin	
	   	mle_s,elist(sel),bc(kk),rs(kk),para,status=status $
		,sigma=gsize,chn=chn,xo=xoo,yo=yoo,verb=verb,ffdd=ffdd 
	endif else begin
		print,'cs < bc',cs,bc(kk)
		goto, next
	endelse
	if status ne 0 then begin 
		;print,'cs,bc(kk)= ',cs,bc(kk)
		goto, next
;		para=[xoo,yoo,0.]
	endif else begin
		;get_snr,cs,nbin_s,bc(kk),nbin_s,csn,snr,sfrac=sfrac,/bmap
		;print,cs,bc(kk),snr,csn,expt(kk),csn/expt(kk)
	 if ni gt 2 and sqrt((para(0)-xoo)^2+(para(1)-yoo)^2) le 0.1 $
	 	;accuracy limit is 0.1 pixel
	  or cs eq 0 or ni eq maxni then begin
	 	;if cs =0, the source will be removed in the output 
	 	csv(kk)=cs 
	 	dsv(kk)=sqrt((para(0)-xo(kk))^2+(para(1)-yo(kk))^2)
	 	xo(kk)=para(0) & yo(kk)=para(1)
	 	if chn ge 2 then begin
			sz(kk)=para(2)
			if chn eq 3 then gcntr(kk)=para(3)
	 	endif
		for kkk =0, np-1 do sse(kk,kkk)=ffdd(kkk,kkk)
		s_sel(kk)=1
	 	goto,next
	 endif else begin
	 	xoo=para(0) & yoo=para(1)
	 	if chn ge 2 then begin
			gsize=para(2) 
	 	endif
	 endelse
	endelse
	print,cs
  endfor
  print,'ni is greater than maxni'
  s_sel(kk)=1
  next:
  status=0 
;  k=k+1
endfor
            if !debug eq 3 then stop

ss=where(s_sel eq 1,ns) 
if ns eq 0 then stop,'No source is selected!'
msel,ss,xo,yo,dsv,csv,nbin_s,bc,expt,rs,sigma
sse=sse(ss,*)
if chn ge 2 then sz=sz(ss)
if chn eq 3 then gcntr=gcntr(ss)

if fac_ssr ne 1. then $
  get_snr,csv,nbin_s,bc,nbin_s,csn,snr,sfrac=1,/bmap $ ;no correction for scat
else get_snr,csv,nbin_s,bc,nbin_s,csn,snr,sfrac=sfrac,/bmap ;nbin_s is not used
if !debug eq 2 then stop

;print,nbin_s,bc,csv,csn,snr,sfrac
cntr=csn/expt
if chn eq 3 then gcntr=gcntr/(expt*gfrac)

;get position in a standard SASS image
ra_dist = xo - !pref
dec_dist =yo - !pref
dis=sqrt(ra_dist^2+dec_dist^2)*(!size_pixel/60.)
trans_loct,ra_dist,dec_dist,cra,cdec,star_ra,star_dec,/deg,pixsize=!size_pixel
trans_degree,star_ra,star_dec,ra_hour,ra_min,ra_sec,dec_deg,dec_min,dec_sec,/deg
sse=sqrt(sse) ; in units of pixels
;record these source into output file:
kk=0
perr=sqrt(sse(*,0)^2+sse(*,1)^2)*!size_pixel
;-----------------
poisig_v,bc,csv,prob
ss=where(prob lt threshold and abs(ra_dist) lt hdimx $
	and abs(dec_dist) lt hdimy,nss) 
if nss eq 0 then return
sn=lindgen(nss)+1
msel,ss,star_ra,star_dec,perr,prob,snr,cntr,dsv,dis,rs
msel,ss,csv,bc,expt
sse=sse(ss,*)
row={iauid:'',sn:0,ra:0.0D0,dec:0.0D0,perr:0.0,psyserr:0.0,prob:0.0,snr:0.0,cntr:0.0,offaxis:0.0,sradius:0.0,count:0,bcount:0.0,expt:0.0}
;row={iauid:'',sn:0,ra:0.0D0,dec:0.0D0,perr:0.0,psyserr:0.0,prob:0.0,snr:0.0,cntr:0.0,offaxis:0.0,count:0,bcount:0.0,expt:0.0}
slist = replicate(row,nss)
radec_out,star_ra,star_dec,dig=dig,iauname=iauid
slist.iauid=iauid
slist.sn=sn
slist.ra=star_ra
slist.dec=star_dec
slist.perr=perr
slist.prob=prob
slist.snr=snr
slist.cntr=cntr
slist.offaxis=dis
slist.sradius=rs
slist.count=csv
slist.bcount=bc
slist.expt=expt
;-------------
;systematic position error:
if n_elements(sradius) eq 0 then $
  get_psyserr,dis,psyserr,nooapsyserr=nooapsyserr $
else psyserr=sradius            ;in arcsec

slist.psyserr=psyserr
;--------------
sou_struct_out,slist,text
if silent eq 0 then  begin
    print,tag_names(slist)
    for k=0,(nss-1) do print,text(k)
endif
if n_elements(outfile) eq 0 then begin
	if n_elements(infile) ne 0 then outfile=infile+'_ml' else return
endif
sou_struct_fits,slist,outfile
return
end
