pro whathk,hkinfo,hkparam,gti,msk,mkdir=mkdir
;Author T. Yaqoob - March 1993 -> **
;modified Feb 1997 to automatically get mkf filenames
;from mkdir (or ../aux by default) if hkinfo is not defined
if n_params(0) eq 0 then begin
 print,'whathk,hkinfo,hkparam,gti,msk,mkdir=mkdir'
 print,'What are the ranges of the HK parameters in a photon list ?'
 retall
end
if n_elements(0) eq 0 then begin
ans=' '
read,' Input HK parameter name '
endif
if n_elements(mkdir) eq 0 then mkdir='../aux/'
if n_elements(hkinfo) gt 0 then begin
nmkf=hkinfo.nfiles
mknames=hkinfo.fname & mknames=mknames(0:nmkf-1)
endif else begin
 mknames=findfile(mkdir+'*.mkf',count=nmkf)
endelse
for k=0l,nmkf-1 do begin
 tab=readfits(mknames(k),h,ext=1)
 timtmp=tbget(h,tab,'TIME')
 hktmp=tbget(h,tab,hkparam)
 if k eq 0 then begin
  time=timtmp & hkval=hktmp
 endif
 if k gt 0 then begin
  time=[time,timtmp] & hkval=[hkval,hktmp]
 endif
endfor
nhk=(size(hkval))(1) &print,'Read ',nhk,' values of ',hkparam
msk=intarr(nhk)
ngti=n_elements(gti)/2
print,ngti,' GTIs ...'
for j=0l,ngti-1 do begin
 wm=intarr(nhk)
 wm=(time gt gti(j,0) and time lt gti(j,1))
 wmi=where(wm gt 0)
; print,gti(j,0),gti(j,1),min(time(wmi)),max(time(wmi))
;print,gti(j,1)-gti(j,0),max(time(wmi))-min(time(wmi))
 msk=msk+(time gt gti(j,0) and time lt gti(j,1))
endfor
wmsk=where((msk gt 0),nmsk)
print,nmsk,' values of HK params selected '
if nmsk eq 0 then begin
 print,' The data includes NO values of ',hkparam
endif
if nmsk gt 0 then hkval=hkval(wmsk)
if nmsk gt 0 then time=time(wmsk)
print,' MIN and MAX values of ',hkparam 
print,min(hkval),max(hkval)
window,0,xsize=1100,ysize=500
plot,time,hkval,/xst,/yst,psym=3
vmax=0.8*max(hkval)
for k=0,ngti-1 do begin
 vec=[vmax,vmax]
 oplot,gti(k,*),vec
endfor
return
end
