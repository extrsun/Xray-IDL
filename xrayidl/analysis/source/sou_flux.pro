pro sou_flux,nhim,hdr,slow=slow,flow=flow,blank=blank,scr_dif=scr_dif,crflux=crflux,opfile=opfile,soufile=soufile,outfile=outfile,fluxth=fluxth,fluxv=fluxv
;-
; calculate conversion using an HI image
; writen by WQD 5/26/96
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - sou_flux,nhim,hdr,slow=slow,flow=flow'
print,',blank=blank,scr_dif=scr_dif,crflux=crflux,opfile=opfile'
print,',soufile=soufile,outfile=outfile,fluxth=fluxth'
return
endif
if n_elements(opfile) eq 0 then opfile='~/rosatdata/shadow/vp_a0.18_4_7'
if n_elements(soufile) eq 0 then soufile='sou_47all'
if n_elements(scr_dif) eq 0. then scr_dif=1.
if n_elements(crflux) eq 0. then srflux=1
if n_elements(blank) eq 0 then blank=0. ;HI image with this value will not
					; be used.
sz=size(nhim)
if n_elements(slow) eq 0 then slow=0.
crval=sxpar(hdr,'crval*')
cra=crval(0) & cdec=crval(1)
xmid=sxpar(hdr,'crpix1')
ymid=sxpar(hdr,'crpix2')
xbin=abs(sxpar(hdr,'cdelt1'))*3600.
ybin=abs(sxpar(hdr,'cdelt2'))*3600.

if n_elements(sra) eq 0 then begin
	source_info,sn,sra,sdec,ston,cntr $
	,soufile=soufile,slow=slow,flow=flow
	sra=sra*(180./!pi)
	sdec=sdec*(180./!pi)
endif
trans_dist,cra,cdec,sra,sdec,xd,yd,/deg,/das
xx=xmid+xd/xbin
yy=ymid+yd/ybin
sel  = where( (xx le sz(1)-1) and (xx ge 0) and $
             (yy le sz(2)-1) and (yy ge 0) and nhim(xx,yy) ne blank, count)
if count ne 0 then begin
      	snh = interpolate(nhim,xx(sel),yy(sel))
      	sra=sra(sel)
	sdec=sdec(sel)
	cntr=cntr(sel)
	sn=sn(sel)
	ston=ston(sel)
endif else begin
   stop,'!!! No correspondence found between the images !!!'
endelse

; calculate the conversion 
readphoct,nhv,crv,opfile
nhv=nhv*1.e2 ;in units of 10^20
crv=crv/crv(0)
linterp,nhv,crv,snh,scr
flux=cntr/scr/scr_dif*crflux
if n_elements(outfile) eq 0 then outfile=soufile+'_flux'
	openw,un,outfile,/get_lun
        trans_degree,sra,sdec,ra_hour, $
         ra_min,ra_sec,dec_deg,dec_min,dec_sec,/deg

        for k=0,(count-1) do begin
	 if flux(k) ge fluxth then begin
         print, sn(k), ra_hour(k),ra_min(k),ra_sec(k) $
         ,dec_deg(k),dec_min(k),dec_sec(k), ston(k), cntr(k), $
         snh(k),flux(k),format='(I3, 2(2i4, f7.2), f9.2, 3f11.5)'

         printf,un, sn(k),' |', ra_hour(k),ra_min(k) $
        ,ra_sec(k),' |',dec_deg(k),dec_min(k),dec_sec(k),' |',ston(k), $
        ' |',cntr(k),' |', snh(k),' |', flux(k),' |' $
        ,format='(I3,a2,2(2i4, f7.2,a2), f9.2,a2, f11.5,a2,2(f11.5,a2))'
	 endif
        endfor
free_lun,un
fluxv=flux
stop
return
end