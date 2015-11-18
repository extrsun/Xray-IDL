;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro source_anal,list,cra,cdec,block=block,radius=radius $
,threshold=threshold,infile=infile,outfile=outfile,append=append,frac=frac $
,dfac=dfac,chn=chn,exptime=exptime,image_t=image_t,tblock=tblock $
,fchoice=fchoice $
,xshift=xshift,yshift=yshift,ashift=ashift,slow=slow,sigma=sigma $
,rc=rc,rbin=rbin,rbout=rbout
;-
; cra, cdec - the 
; image_t has to be the full image as defined in the SASS coordinate
; writen by wqd, 1995
;+
if n_params() eq 0 then begin
return
endif
;
if n_elements(threshold) eq 0 then threshold=0.
if n_elements(fchoice) eq 0 then fchoice=2
if n_elements(tblock) eq 0 then tblock=!block
if n_elements(block) eq 0 then block=2
if n_elements(exptime) eq 0 then exptime=1.
;if n_elements(radius) eq 0 then radius=50. ;arcmin
if n_elements(chn) eq 0 then chn=2
if n_elements(frac) eq 0 then frac=1.
if n_elements(dfac) eq 0 then begin
	if!instr eq 'h' then dfac=3 else dfac=1.
endif
if n_elements(infile) eq 0 then infile=!seq_no+'_sou.dat'

; read the sources from the input source file
source_info,souno,sra,sdec,ssigma,scntr,ns=ns,soufile=infile $
	,slow=slow,flow=0.,/deg ;,/self
;
if n_elements(outfile) eq 0 then outfile=!seq_no+'_souanal.dat' 
if strupcase(outfile) ne 'none' then begin
	if keyword_set(append) eq 0 then $
	openw,un,outfile,/get_lun else $
	openw,un,outfile,/get_lun,/append
endif

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

if !instr eq 'p' then begin
	detect_params,dis,rs,rb1,rb2,blow=blow,bhigh=bhigh,fac=dfac $
		,gaus=gaus,gsigma=gsigma,gfrac=gfrac
	if n_elements(gaus) ne 0 then sigma=gsigma
	if dfac eq 1 then cntrfac=0.9 else stop,' please give the cntrfac'
endif else begin
	rs=dfac*(2.35*0.5*(0.74^2+1.0+(1.3+0.0205*dis^2.349)^2)^0.5 > 3.)/60.
	; in arcmin
	; twice the 50% radius of the RHRI (p13 of the Guest Observing prog)
	rb1=rs*1.5
	rb2=rb1+2.
	if dfac eq 3 then cntrfac=0.8 else read,'stop: please give the cntrfac'
	; 0.8 see figure 3; should be good for about 10%
endelse
if n_elements(rbout) ne 0 then rb2=rbout
if n_elements(rbin) ne 0 then rb1=rbin
if n_elements(rc) ne 0 then rs=rc
;
; get source pixel positions
if !instr eq 'p' then begin
	hdim=7679.5 
	fdim=15360/tblock 
endif else begin
 	hdim=4095.5
	fdim=8192/tblock
endelse

;calculate the min and max of the i,j for each box
hboxdim=fix(rb2*60./(block*!size_pixel)+1)
imin=nint(xp+hdim)-hboxdim*block ;in IDL coordinates
jmin=nint(yp+hdim)-hboxdim*block
boxdim=2*hboxdim+1         ;dimension of the boxes

;put the approximate positions of the sources at image centers
; the positions are used as the initial values in MLE_s.pro
xo=imin+(hboxdim*block+1) 
yo=jmin+(hboxdim*block+1) ;in SASS (FORTRAN) coordinates
;
;get exposures at the source positions
x_core=hdim+xp(sel)
y_core=hdim+yp(sel)
loc=nint(y_core)/long(tblock)*fdim+nint(x_core)/long(tblock)
if n_elements(image_t) ne 0 then expt=image_t(loc) $
 else expt=loc*0.+exptime

; loop over individual sources
source_sn=fltarr(ns)
cntr=fltarr(ns)
sz=cntr
gcntr=cntr
for k=0L,(ns-1) do begin
	dim=boxdim(k)
	filter=replicate(1.,dim,dim) ;a filter centered on the source with
				     ;pixsize=block
	if fchoice eq 1 then $
	    	filter=source_sub_v(filter,sra(k),sdec(k),sra,sdec, $
			scntr,blow=blow,bhigh=bhigh,factor=frac)
	filter_sp,0,0,filter,filter_s,block=block $
		,rs=rs(k),rb1=rb1(k),rb2=rb2(k),/pixc
	mle_s,list,expt(k),para,imin=imin(k),jmin=jmin(k),filter=filter_s $
	,dim=dim,block=block,sigma=sigma,chn=chn,cntrfac=cntrfac $
		,xo=xo(k),yo=yo(k) 
	cntr(k)=para(0)
	source_sn(k)=para(1)
	x_core(k)=para(2)
	y_core(k)=para(3)
	if chn eq 2 then begin
		sz(k)=para(5)
		gcntr(k)=para(4)
	endif
endfor
if chn eq 2 then gcntr=gcntr/expt*1.e3
source_sn=cntr/source_sn
cntr=cntr*1.e3
sz=sqrt(sz)

;get position in a standard SASS image
shift_xya,x_core,y_core,xshift=xshift,yshift=yshift,ashift=ashift
ra_dist = x_core - (hdim+1)
dec_dist =y_core - (hdim+1)
trans_loct,ra_dist,dec_dist,cra,cdec,star_ra,star_dec,/deg
trans_degree,star_ra,star_dec,ra_hour,ra_min,ra_sec,dec_deg,dec_min,dec_sec,/deg

;record these source into output file:
newstar=indgen(ns)+1
for k=0,(ns-1) do begin
	if source_sn(k) ge threshold then begin
 	print, newstar(k), ra_hour(k),ra_min(k),ra_sec(k) $
 	,dec_deg(k),dec_min(k),dec_sec(k), source_sn(k), cntr(k), $
 	x_core(k),y_core(k),sz(k),gcntr(k), $
		format='(I3, 2(2i4, f7.2), f9.2, f11.1,1x,2f7.1,f10.1,f11.5)'
	if strupcase(outfile) ne 'NO' then $
 	  printf,un, newstar(k),' |', ra_hour(k),ra_min(k) $
	  ,ra_sec(k),' |',dec_deg(k),dec_min(k),dec_sec(k),' |',source_sn(k), $
   	  ' |',cntr(k),' |', x_core(k),' |',y_core(k),' |' $
		,sz(k),' |',gcntr(k),' |'  $
	  ,format='(I3,a2,2(2i4, f7.2,a2), f9.2,a2, f9.5,a2,2(f8.2,a2),f8.1,a2,f9.1,a2)'
	endif
endfor
stop
free_lun,un
end