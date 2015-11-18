pro sou_del,infile,outfile,radius=radius
;-
; merge sources detected in overlapping images
; sources located within radius (default=1'.5) are merged with smallest off-axis
; detection kept in the list
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - sou_del,infile,outfile,radius=radius'
return
endif
if n_elements(radius) eq 0 then radius=1.5
source_info,sn,sra,sdec,sigma,cntr,xp,yp,/self,soufile=infile
ss=sort(sra)
sn=sn(ss) & sra=sra(ss) & sdec= sdec(ss) &sigma=sigma(ss)
cntr=cntr(ss) & xp=xp(ss) & yp=yp(ss)
nsource=n_elements(sn)
	sr=(radius*120.)^2
	cxp=xp-255.5
	cyp=yp-255.5
	ndel=0
	k=0
repeat begin
	trans_dist,sra(k),sdec(k),sra,sdec,xd,yd
	sdis=xd*xd+yd*yd
	c=where(sdis lt sr,nc)
	if nc gt 1 then begin
		offc=cxp(c)*cxp(c)+cyp(c)*cyp(c)
		dismin=min(offc,cmin)
		remove,cmin,c
		remove,c,sn,sra,sdec,sigma,cntr,cxp,cyp
		nsource=nsource-nc+1
		ndel=ndel+nc-1
	endif else k=k+1
endrep until k eq nsource-1
	xp=cxp+255.5
	yp=cyp+255.5
	
trans_degree,sra,sdec,ra_hour,ra_min,ra_sec,dec_deg,dec_min,dec_sec	
openw,out,outfile,/get_lun
sn=lindgen(nsource)
for k=0,(nsource-1) do begin
;	 print, sn(k), ra_hour(k),ra_min(k),ra_sec(k) $
	; ,dec_deg(k),dec_min(k),dec_sec(k), sigma(k), cntr(k), $
	; xp(k),yp(k),format='(I3, 2(2i4, f7.2), f9.2, f11.5,2I4)'
	 printf,out, sn(k),' |', ra_hour(k),ra_min(k) $
	,ra_sec(k),' |',dec_deg(k),dec_min(k),dec_sec(k),' |',sigma(k), $
	' |',cntr(k),' |', xp(k),' |',yp(k),' |' $
	,format='(I3,a2,2(2i4, f7.2,a2), f9.2,a2, f11.5,a2,2(I4,a2))'
	 print, sn(k),' |', ra_hour(k),ra_min(k) $
	,ra_sec(k),' |',dec_deg(k),dec_min(k),dec_sec(k),' |',sigma(k), $
	' |',cntr(k),' |', xp(k),' |',yp(k),' |' $
	,format='(I3,a2,2(2i4, f7.2,a2), f9.2,a2, f11.5,a2,2(I4,a2))'
endfor
print,ndel,' of sources are deleted'
free_lun,out
end