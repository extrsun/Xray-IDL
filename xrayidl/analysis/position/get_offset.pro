pro get_offset,cra,cdec,xsou_sel,osou_sel,val,xfile=xfile,ofile=ofile,xra=xra,xdec=xdec,xperr=xperr,cat=cat
;-
; main program for obtaining x, y and angular offsets of x-ray image relative
; to an optical image by fitting X-ray source positions to relatively
; well defined optical or radio positions of the sources.
;
; cra,cdec - the assumed rotation center (e.g., image center) in degrees
; xsou_sel - vector containing the selected X-ray source numbers
; 		obtained by runing source_plot.pro
; osou_sel - vector containing the selected optical counterpart (GSC) numbers
; 		obtained by runing object_plot.pro
; xfile - name of the file containing the X-ray source list (i.e,
;		(rp*_src.fits)
; ofile - name of the file containing the GSC object list (i.e., gsc_30min.dat)
; val - containing the x, y and angular offsets
;	(in arcsec and in degree (anti-clockwise))
; xra,xdec,xperr - ra, dec (deg) and err bars (arcsec) of x-ray sources
;			if supplied, xfile and xsou_sel will not be used
; cat - cat=1 for cosmos and APM optical sources; cat=2 for gsc optical sources
; writen by wqd, May 4, 1994
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_offset,cra,cdec,xsou_sel,osou_sel,val'
print,',xfile=xfile,ofile=ofile,xra=xra,xdec=xdec,xperr=xperr,cat=cat'
return
endif
if n_elements(cat) eq 0 then cat=2
if n_elements(ofile) eq 0 then ofile='gsc_30min.dat'
if n_elements(xfile) eq 0 then xfile=!seq_no+'_src.fits'
if n_elements(xra) eq 0 then $
	source_posi,xra,xdec,xperr,infile=xfile,sou_sel=xsou_sel $
		,imcra=cra,imcdec=cdec
if cat eq 2 then begin
	gsc_info,ofile,opt_no,ora,odec,operr
endif else begin if cat eq 1 then begin
	object_info,opt_no,ora,odec,soufile=ofile
	operr=ora*0.+0.5 ;for APM catalogue as desribed by the Mike Irwin's 
	endif
endelse 
match,osou_sel,opt_no,c,sel,count=nsel
sel=sel(sort(c))
if nsel ne n_elements(osou_sel) then $
	stop,'no all optical sources are not found'
ora=ora(sel)
odec=odec(sel)
operr=operr(sel)
print,'xra,ora = ',xra,ora
print,'xdec,odec = ',xdec,odec
stop
;
trans_dist,cra,cdec,xra,xdec,xobs,yobs,/deg,/das ;dist in units of arcsec
trans_dist,cra,cdec,ora,odec,xopt,yopt,/deg,/das
wvec=1./(operr*operr+xperr*xperr)
chi=total(((xobs-xopt)*(xobs-xopt)+(yobs-yopt)*(yobs-yopt))*wvec)
print,'original Chi sq and ndf = ',chi,2*nsel

offset_fit,xobs,yobs,xopt,yopt,wvec,val
print,'Before correction: xobs,yobs (arcsec) = ',xobs,yobs
xobsn=val(0)+xobs-yobs*val(2)
yobsn=val(1)+yobs+xobs*val(2)
print,'absolute shifts from optical position (arcsec):'
print,sqrt((xobsn-xopt)*(xobsn-xopt)+(yobsn-yopt)*(yobsn-yopt))
print,'dispersions (arcsec):'
print,sqrt(1./wvec)
print,'After correction: xobs,yobs (arcsec)= ',xobsn,yobsn
chi=total(((xobsn-xopt)*(xobsn-xopt)+(yobsn-yopt)*(yobsn-yopt))*wvec)
print,'Chi sq and ndf = ',chi,2*nsel-3
val(2)=val(2)*180./!pi
print,'required X and Y (arcsec), and angular (degree) shifts of the X-ray image : ',val
stop
end