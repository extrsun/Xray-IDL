pro source_plot,infile,corner,hdr,cra=cra,cdec=cdec $
,xarcmin=xarcmin,yarcmin=yarcmin,slow=slow,flow=flow,psym=psym,radius=radius $
,symsize=symsize,sou_no=sou_no,self=self,s_c=s_c
;-
; plot source position in an existing image
; infile - the name of the source file
; cra,cdec - the center coordinates of the image (in radian)
; xarcmin,yarcmin - the dimension of the image (arcmin)
; corner - a vector containing xmin, xmax, ymin,ymax of the image (normalized
;	coordinates from cont_grey.pro
; writen by WQD 4/23/93
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - infile,corner,hdr,cra=cra,cdec=cdec,'
print,'xarcmin=xarcmin,yarcmin=yarcmin,slow=slow,flow=flow,psym=psym,'
print,'radius=radius,symsize=symsize,self=self,s_c=s_c'
return
endif
if n_elements(s_c) eq 0 then s_c=0
if n_elements(symsize) eq 0 then symsize=1
if n_elements(slow) eq 0 then slow=3.5
if n_elements(psym) eq 0 then psym=7
if n_params() gt 2 then begin
	crval=sxpar(hdr,'crval*')*!pi/180.
if n_elements(cra) eq 0 then cra=crval(0) 
if n_elements(cdec) eq 0 then  cdec=crval(1)
if n_elements(xarcmin) eq 0 then $
	xarcmin=sxpar(hdr,'naxis1')*abs(sxpar(hdr,'cdelt1'))*60.
if n_elements(yarcmin) eq 0 then $
	yarcmin=sxpar(hdr,'naxis2')*abs(sxpar(hdr,'cdelt2'))*60.
endif
source_info,sn,sra,sdec,sigma,cntr,xp,yp $
	,soufile=infile,slow=slow,flow=flow,self=self
asperbin=60.
xmid=(corner(0)+corner(1))*0.5
ymid=(corner(2)+corner(3))*0.5
xnorm=!size_pixel*(corner(1)-corner(0))/(xarcmin*asperbin)
ynorm=!size_pixel*(corner(3)-corner(2))/(yarcmin*asperbin)
;if norm ne ynorm then stop,'x and y axis not consistent'

trans_dist,cra,cdec,sra,sdec,xd,yd
if n_elements(radius) ne 0 then begin
	sel=where((xd*xd+yd*yd)*(!size_pixel/60.)^2 lt radius*radius)
	xd=xd(sel)
	yd=yd(sel)
endif
xd=xmid+xd*xnorm
yd=ymid+yd*ynorm
c=where(xd ge corner(0) and xd le corner(1) and $
 yd ge corner(2) and yd le corner(3),nc)
if nc ne 0 then begin
	if keyword_set(sou_no) ne 0 then begin
	 xyouts,xd(c),yd(c),sn(c),/normal,size=symsize,alig=0.5,color=s_c
	endif else $
	plots,xd(c),yd(c),psym=psym,/normal,symsize=symsize,color=s_c
endif
print,nc,' sources included in the image'

end