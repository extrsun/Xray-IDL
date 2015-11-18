pro source_plot,infile,corner,hdr,slow=slow,flow=flow,probth=probth $
,psym=psym,radius=radius $
,symsize=symsize,sou_no=sou_no,s_c=s_c,sra=sra,sdec=sdec,sn=sn,thick=thick,linestyle=linestype,silent=silent,ctype=ctype,fits=fits,nssh=nssh,xsh=xsh,ysh=ysh,slist=slist,sel=sel,roll=roll
;+
; plot source position in an existing image
; infile - the name of the source file
; corner - a vector containing xmin, xmax, ymin,ymax of the image (normalized
;	coordinates from cont_grey.pro
; ctype - if = 1, sra, sdec are assumed to have equinox=2000 and converted into
;		the galactic coordinates.
; nssh - source numbers for position shifts for source labeling
; xsh,ysh - x and y position shifts in units of arcsec
; slist - output source list structure
; sel - selected indexes of sources
;
; writen by WQD 4/23/93
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - source_plot,infile,corner,hdr,slow=slow,flow=flow'
print,',probth=probth,psym=psym,radius=radius,symsize=symsize,sou_no=sou_no'
print,',s_c=s_c,sra=sra,sdec=sdec,sn=sn,thick=thick,silent=silent,ctype=ctype,fits=fits,nssh=nssh,xsh=xsh,ysh=ysh,slist=slist,sel=sel,roll=roll'
return
endif

if n_elements(thick) eq 0 then thick=1
if n_elements(s_c) eq 0 then begin
	if !d.name eq 'X' then s_c=!d.n_colors-1 else s_c=0
endif
if n_elements(symsize) eq 0 then symsize=1
if n_elements(slow) eq 0 then slow=0
if n_elements(psym) eq 0 then psym=7
crval=sxpar(hdr,'crval*')
cra=crval(0) & cdec=crval(1)
naxis=sxpar(hdr,'naxis*')
xarcmin=naxis(0)*abs(sxpar(hdr,'cdelt1'))*60.
yarcmin=naxis(1)*abs(sxpar(hdr,'cdelt2'))*60.
if n_elements(sra) eq 0 then begin
;--------------------------------------------
; if the source file is in the fits format:
    if keyword_set(fits) ne 0 then begin
	;row={sn:0,ra:0.0d,dec:0.0d,snr:0.0} ;,cntr:0.0}
	sou_fits_info,infile,slist,slow=slow,flow=flow,row=row,probth=probth $
		,nsel=ns,/all
	if ns eq 0 then return
	sn=slist.sn & sra=slist.ra & sdec=slist.dec 
	sigma=slist.snr ;& cntr=slist.cntrb
    endif else begin
	source_info,sn,sra,sdec,sigma,cntr $
	,soufile=infile,slow=slow,flow=flow,/deg ;prob is not implemented
        ns=n_elements(sra)
    endelse 
endif else ns=n_elements(sra)
if n_elements(ctype) ne 0 then begin
	if ctype eq 1 then glactc,sra,sdec,2000,sra,sdec,1,/deg
endif 
;assuming that the equinox=2000

asperbin=60.
xmid=corner(0)+(corner(1)-corner(0))*(sxpar(hdr,'crpix1')-0.5)/float(naxis(0))
ymid=corner(2)+(corner(3)-corner(2))*(sxpar(hdr,'crpix2')-0.5)/float(naxis(1))
;assuming the Fortran format of crpix*
xnorm=(corner(1)-corner(0))/(xarcmin*asperbin)
ynorm=(corner(3)-corner(2))/(yarcmin*asperbin)
;if norm ne ynorm then stop,'x and y axis not consistent'

trans_dist,cra,cdec,sra,sdec,xd,yd,/deg,/das,angle=ang
if n_elements(roll) ne 0 then rot_xy,xd,yd,-roll,block=1,xpref=0,ypref=0,/xyreal ;requiring anti-clockwise angle

if n_elements(radius) ne 0 then begin
	sel=where(ang lt radius*60.,nsel)
	if nsel eq 0 then stop,'no source within the radius!!!'
        xd=xd(sel)
	yd=yd(sel)
        sn=sn(sel)
    endif else sel=lindgen(ns)
xd=xmid+xd*xnorm
yd=ymid+yd*ynorm
ssel=where(xd ge corner(0) and xd le corner(1) and $
 yd ge corner(2) and yd le corner(3),nssel)
if nssel eq 0 then begin
    print,'No source in the field!!!'
    return
endif
if n_elements(xsh) eq 0 then xsh=replicate(0,nssel)
if n_elements(ysh) eq 0 then ysh=replicate(0,nssel)
if nssel ne 0 then begin
    xd=xd(ssel)
    yd=yd(ssel)
    nsh=n_elements(nssh)
    if nsh ne 0 or keyword_set(sou_no) then sn=sn(ssel) 
    if nsh ne 0 then begin
        for k=0,nsh-1 do begin
            ss=where(sn eq nssh(k),nss)
            if nss ne 0 then begin
                xd(ss)=xd(ss)+xsh(k)*xnorm
                yd(ss)=yd(ss)+ysh(k)*ynorm
            endif
        endfor
    endif
    if keyword_set(sou_no) then begin
	 xyouts,xd,yd,sn,/normal,size=symsize,alig=0.5,color=s_c
     endif else $
	plots,xd,yd,psym=psym,/normal,symsize=symsize,color=s_c $
		,thick=thick,linestyle=linestype
     sel=sel(ssel)
 endif else sel=-1
if keyword_set(silent) eq 0 then print,nssel,' sources included in the image'
if !debug eq 2 then stop
return
end
