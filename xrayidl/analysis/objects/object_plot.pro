pro object_plot,infile,corner,hdr,cra=cra,cdec=cdec,xarcmin=xarcmin $
,yarcmin=yarcmin,cpxarcmin=cpxarcmin,cpyarcmin=cpyarcmin,flow=flow,psym=psym $
,radius=radius,symsize=symsize,sel=sel,cat=cat,sou_no=sou_no,pri=pri $
,idsel=idsel,pcolor=pcolor,magmax=magmax,sig=sig,remplate=remplate $
,bmag=bmag
;-
; plot cosmos object positions in an existing image
; infile - the name of the source file
; cra,cdec - the center coordinates of the image (in radian)
; xarcmin,yarcmin - the dimension of the image (arcmin)
; cpxarcmin,cpyarcmin - the position of the reference pixel
; corner - a vector containing xmin, xmax, ymin,ymax of the image (normalized
;	coordinates from cont_grey.pro
; idsel and magmax are for R band only
;
; modified version of source_plot, writen by WQD 5/2/94
; add keyword cpxarcmin and cpyarcmin. wqd, Jun 14, 1994
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - object_plot,infile,corner,hdr,cra=cra,cdec=cdec'
print,',xarcmin=xarcmin,yarcmin=yarcmin,cpxarcmin=cpxarcmin,cpyarcmin=cpyarcmin,flow=flow,psym=psym,radius=radius'
print,',symsize=symsize,sel=sel,cat=cat,sou_no=sou_no,pri=pri,idsel=idsel'
print,',pcolor=pcolor,magmax=magmax,sig=sig,remplate=remplate,bmag=bmag'
return
endif
if n_elements(pcolor) eq 0 then begin
	if !d.name eq 'PS' then pcolor=0 else pcolor=!d.n_colors-1
endif 
if n_elements(cat) eq 0 then cat=1
if n_elements(symsize) eq 0 then symsize=1
if n_elements(flow) eq 0 then flow=1.e22
if n_elements(psym) eq 0 then psym=4
if n_params() gt 2 then begin
	crval=sxpar(hdr,'crval*')
	if n_elements(cra) eq 0 then cra=crval(0) 
	if n_elements(cdec) eq 0 then  cdec=crval(1)
	if n_elements(xarcmin) eq 0 then $
		xarcmin=sxpar(hdr,'naxis1')*abs(sxpar(hdr,'cdelt1'))*60.
	if n_elements(yarcmin) eq 0 then $
		yarcmin=sxpar(hdr,'naxis2')*abs(sxpar(hdr,'cdelt2'))*60.
	if n_elements(cpxarcmin) eq 0 then $
		cpxarcmin=sxpar(hdr,'crpix1')*abs(sxpar(hdr,'cdelt1'))*60.
	if n_elements(cpyarcmin) eq 0 then $
		cpyarcmin=sxpar(hdr,'crpix2')*abs(sxpar(hdr,'cdelt2'))*60.
endif
;source_info,sn,sra,sdec,sigma,cntr,xp,yp,/self,soufile=infile,slow=slow,flow=flow
if cat eq 1 then begin
	print,'COSMOS catalog is assumed'
	object_info,sn,sra,sdec,flux,id,soufile=infile $
		,idsel=idsel,magmax=magmax,bmag=bmag
endif 
if cat eq 2 then begin
	print,'GSC is assumed'
	gsc_info,infile,sn,sra,sdec,perr,flux,id,plate,idsel=idsel
endif
if cat eq 3 then begin
	print,'PPP is assumed'
	ppp_info,hdr,infile,sn,sra,sdec,flux,fluxe,id,idsel=idsel $
		,magmax=magmax
endif

if cat eq 4 then begin
	print,'APS is assumed'
	aps_info,sra,sdec,flux,sn,soufile=infile,idsel=idsel $
		,magmax=magmax
endif

if cat eq 5 then begin
	print,'APM is assumed'
	apm_info,sra,sdec,flux,id,idb,soufile=infile,idsel=idsel $
		,magmax=magmax
endif
xmid=corner(0)+(corner(1)-corner(0))*(cpxarcmin/xarcmin)
ymid=corner(2)+(corner(3)-corner(2))*(cpyarcmin/yarcmin)
xnorm=(corner(1)-corner(0))/(xarcmin*double(60))
ynorm=(corner(3)-corner(2))/(yarcmin*double(60))
;if norm ne ynorm then stop,'x and y axis not consistent'

trans_dist,cra,cdec,sra,sdec,xd,yd,/deg,/das
nrem=0
if n_elements(radius) eq 0 then radius=1.e32
xd=xmid+xd*xnorm
yd=ymid+yd*ynorm
rem=where(xd lt corner(0) or xd gt corner(1) or $
 yd lt corner(2) or yd gt corner(3) or flux gt flow or $
(xd*xd+yd*yd)/3600. gt radius*radius,nrem)

nsel=n_elements(xd)-nrem
if nsel ne 0 then begin
	if n_elements(sn) eq 0 then sn=lindgen(n_elements(xd))
	if nrem ne 0 then remove,rem,xd,yd,id,flux,sn,sra,sdec

	if n_elements(remplate) ne 0 then begin
		if nrem ne 0 then remove,rem,plate
		rem2=where(plate eq remplate,nrem2)
		if nrem2 ne 0 then begin	
			remove,rem2,xd,yd,id,flux,sn,sra,sdec
			remove,rem2,plate
		endif
	endif

	c=where(id eq 1 or id eq 5 or id eq 6,nc)
	if nc ne 0 then begin
	  if keyword_set(sou_no) ne 0 then begin
		xyouts,xd(c),yd(c),sn(c),/normal,size=symsize,alig=0.5
	  endif else $
		print,'def: triangle for id =1+5+6'
		plots,xd(c),yd(c),psym=psym+1,/normal,symsize=symsize $
			,color=pcolor
		trans_degree,sra(c),sdec(c),ih,im,is,jd,jm,js,/deg
		if keyword_set(pri) ne 0 then begin
			print,'others (id = 1+5+6): '
			forprint,ih,im,is,jd,jm,js,flux(c),id(c),sn(c)
		endif
	endif
	c=where(id eq 2 or id eq 7,nc)
	if nc ne 0 then begin
	  if keyword_set(sou_no) ne 0 then begin
		xyouts,xd(c),yd(c),sn(c),/normal,size=symsize,alig=0.5
	  endif else $
		print,'def: square for id =2+7'
		plots,xd(c),yd(c),psym=psym+2,/normal,symsize=symsize $
			,color=pcolor
		trans_degree,sra(c),sdec(c),ih,im,is,jd,jm,js,/deg
		if keyword_set(pri) ne 0 then begin
			print,'others (id = 2+7): '
			forprint,ih,im,is,jd,jm,js,flux(c),id(c),sn(c)
		endif
	endif
	c=where(id eq 3 or id eq 4,nc)
	if nc ne 0 then begin
	  if keyword_set(sou_no) ne 0 then begin
		xyouts,xd(c),yd(c),sn(c),/normal,size=symsize,alig=0.5
	  endif else $
		print,'def: cross for id =3+4'
		plots,xd(c),yd(c),psym=psym+3,/normal,symsize=symsize $
			,color=pcolor
		trans_degree,sra(c),sdec(c),ih,im,is,jd,jm,js,/deg
		if keyword_set(pri) ne 0 then begin
			print,'others (id = 3+4): '
			forprint,ih,im,is,jd,jm,js,flux(c),id(c),sn(c)
		endif
	endif
	c=where(id eq 0,nc)
	if nc ne 0 then begin
	  if keyword_set(sou_no) ne 0 then begin
		xyouts,xd(c),yd(c),sn(c),/normal,size=symsize,alig=0.5
	  endif else $
		print,'def: dia for id =0'
		plots,xd(c),yd(c),psym=psym,/normal,symsize=symsize $
			,color=pcolor
		trans_degree,sra(c),sdec(c),ih,im,is,jd,jm,js,/deg
		if keyword_set(pri) ne 0 then begin
			print,'others (id = 0): '
			forprint,ih,im,is,jd,jm,js,flux(c),id(c),sn(c)
		endif
	endif
	c=where(id lt 0,nc)
	if nc ne 0 then begin
	  if keyword_set(sou_no) ne 0 then begin
		xyouts,xd(c),yd(c),sn(c),/normal,size=symsize,alig=0.5
	  endif else $
		print,'def: plus for id < 0'
		plots,xd(c),yd(c),psym=psym-3,/normal,symsize=symsize $
			,color=pcolor
		trans_degree,sra(c),sdec(c),ih,im,is,jd,jm,js,/deg
		if keyword_set(pri) ne 0 then begin
			print,'else (id < 0): '
			forprint,ih,im,is,jd,jm,js,flux(c),id(c),sn(c)
		endif
	endif
endif
print,nsel,'sources included in the image'
if !debug eq 1 then stop,'stop at the end of the program'
end