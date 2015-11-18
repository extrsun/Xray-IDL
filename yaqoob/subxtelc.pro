pro subxtelc,ncur,sname,bname,outname,time,terr,rate,brate,net,neterr
;modified mike corcoran's routine for producing background-subtracted
;xte light curves
if n_params(0) eq 0 then begin
 print,'subxtelc,ncur,sname,bname,outname,time,terr,rate,brate,net,neterr'
 print,'NCUR  - Number of lightcurves in FITS files'
 print,'SNAME - XTE FITS file containing source+bgd'
 print,'BNAME - XTE FITS file containing modeled bgd '
 print,'OUTNAME - Output QDP file for bgd-sub lightcurves '
 retall
end 
;
;
;
;
;
openw,1,outname
printf,1,'READ SERR 1 2'
printf,1,'SKIP SINGLE'
printf,1,'LA X Time (s)'
printf,1,'LA Y cts/s '
;tvlct,r,g,b,/get
;window,xs=800,ys=1000
;        set_xy,0,0,0,60
;        !mtitle=sname+' '+bname+' '+outname
                tab=readfits(sname,h,ext=1,/silent)
		time=fits_get(h,tab,'time')
		ntim=(size(time))(1)
		terr=fltarr(ntim)
		rate=fltarr(ntim,ncur)
		brate=fltarr(ntim,ncur)
		err=fltarr(ntim,ncur)
		berr=fltarr(ntim,ncur)
		net=fltarr(ntim,ncur)
		neterr=fltarr(ntim,ncur)
		fe=fltarr(ntim,ncur)
		bfe=fltarr(ntim,ncur)
for i=0l,ncur-1l do begin
print,'Doing Lightcurve No. ',i
                tab=readfits(sname,h,ext=1,/silent)
                t=fits_get(h,tab,'time')
		if i eq 0 then begin
                rate(0:ntim-1l,i)=fits_get(h,tab,'rate')
                err(0:ntim-1l,i)=fits_get(h,tab,'error')
                fe(0:ntim-1l,i)=fits_get(h,tab,'fracexp')
		endif
		if i gt 0 then begin
 		rate(0:ntim-1l,i)=fits_get(h,tab,'rate'+strtrim(i+1,2))
		err(0:ntim-1l,i)=fits_get(h,tab,'error'+strtrim(i+1,2))
		fe(0:ntim-1l,i)=fits_get(h,tab,'fracexp'+strtrim(i+1,2))
		endif
                tbin=sxpar(h,'timedel')
                tab=readfits(bname,h,ext=1,/silent)
                t=fits_get(h,tab,'time')
		if i eq 0 then begin
                brate(0:ntim-1l,i)=fits_get(h,tab,'rate')
                berr(0:ntim-1l,i)=fits_get(h,tab,'error')
                bfe(0:ntim-1l,i)=fits_get(h,tab,'fracexp')
		endif
		if i gt 0 then begin
		brate(0:ntim-1l,i)=fits_get(h,tab,'rate'+strtrim(i+1,2))
		berr(0:ntim-1l,i)=fits_get(h,tab,'error'+strtrim(i+1,2))
		bfe(0:ntim-1l,i)=fits_get(h,tab,'fracexp'+strtrim(i+1,2))
		endif
                btbin=sxpar(h,'timedel')
endfor
                net=rate-brate
                neterr=sqrt(err^2+berr^2)
;
;
;
;                exposure=total(tbin*fe)
;                grosscts=total(rate*tbin*fe)
;                mrate=grosscts/exposure
;                merr=sqrt(grosscts)/exposure
;
;
;
;                bexposure=total(btbin*bfe)
;                bcts=total(brate*bfe*btbin)
;                mbrate=bcts/exposure
;                mberr=sqrt(bcts)/exposure

;                mnet=mrate-mbrate
;                mneterr=sqrt(merr^2+mberr^2)

;                form='(a6,2x,f9.3,2x,f9.3,2x,f9.3,2x,f9.3,2x,f9.3,2x,f9.3)'
;                print,t,mrate,merr,mbrate,mberr,mnet,mneterr,form=form
;                printf,1,t,mrate,merr,mbrate,mberr,mnet,mneterr,form=form
		for i=0l,ncur-1l do begin
		for k=0l,ntim-1l do begin
		terr(k)=0.5*tbin*fe(k,i)
		printf,1,t(k),terr(k),net(k,i),neterr(k,i)
		endfor
		printf,1,'NO NO'
		endfor
close,1
end
