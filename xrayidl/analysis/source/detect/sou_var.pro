pro sou_var,soufile,backfile,gtifile,gtiext,lso,cra,cdec,emin,emax,tb,xmin,ymin,dim,block,sfrac=sfrac,psffile=psffile,slow=slow,rsfac=rsfac,outfile=outfile,probth=probth,tblimit=tblimit,append=append
;+
; create a set of diffuse (source-removed) background maps and place them in
; an array to be used for map_ratio.pro
;
; ls - count list
; tb - exposure map, used only as a filter
; xmin,ymin - lower left corner pixel position
; dim - dimension of the image
; 
; written by wqd, 6/8/2001
;-
if n_params() eq 0 then begin
print,'Calling procedure - map_back,ls,xmin,ymin,dim,bmin,bmax,tbo,tbso,ba'
print,',binfac=binfac,block=block,exam=exam'
return
endif

if N_elements(rsfac) eq 0 then rsfac=2.
if N_elements(tblimit) eq 0 then begin
	tblimit=0.5*max(tb)
	print,'using default tblimit = ',tblimit
endif

cbm=readfits(backfile,hdr)
sz=size(cbm)
source_info,sn,sra,sdec,souf=soufile,/deg,slow=slow

;get off-image center distance:
crval=sxpar(hdr,'crval*')
pixsize=sxpar(hdr,'cdelt2')*3600.
trans_dist,crval(0),crval(1),sra,sdec,xp,yp,/deg,pixsize=pixsize
loc=long(yp+sz(2)*0.5)*sz(1)+long(xp+sz(1)*0.5)
sel=where(tb(loc) lt tblimit,nsel)
if nsel ne 0 then begin
;	print,'remove sources ',sel
	remove,sel,loc,xp,yp,sra,sdec,sn
endif	
sfb=cbm(loc) ;this bb may be different from the choice in var_ks
;off aiming point:
trans_dist,cra,cdec,sra,sdec,xp,yp,/deg,pixsize=!size_pixel
oa=sqrt(xp^2+yp^2)*(!size_pixel/60.) ;off-axis distances
psf_params,oa,sr,perclimit=sfrac,psffile=psffile
rr=sr/block*3
tbs=source_sub_v(tb,crval(0),crval(1),sra,sdec,block=block,/deg,sradius=rr,subva=-1.)

;select off-source counts 
stop
list_image,lso,xmin,ymin,c,dim,block=block,emin=emin,emax=emax,filter=tbs,sel=sel
tref=lso(sel).time
tref=tref(sort(tref))
;on-source counts
list_image,lso,xmin,ymin,c,dim,block=block,emin=emin,emax=emax,filter=tbs,sel=sel,/rsel
;tv,bscale(c,0,2)
ls=lso(sel)
ls=ls(sort(ls.time))

;read from the event file the good time interval:
get_gti,gtifile,gtiext,gti,tlo,thi
var_ks,tref,ls,sn,xp,yp,(rr*block)*rsfac,tlo,thi,probv,block=block,sfb=sfb $
	,probth=probth
if n_elements(outfile) ne 0 then begin
	if keyword_set(append) then $
		openw,un,outfile,/get,/append else $
		openw,un,outfile,/get
	for k=1,n_elements(probv)-1 do begin
		if probv(k) lt probth then $ 
			printf,un,sn(k),probv(k), ' var' $
		else printf,un,sn(k),probv(k)
	endfor
	free_lun,un
endif
return
end