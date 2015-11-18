pro sisdetreg,geo
if n_params(0) eq 0 then begin
 print,'sisdetreg,geo'
 print,'Convert region relection parameters into SIS DET coords (mm)'
 retall
endif
x=fltarr(5) & y=fltarr(5)
x0=geo.cen1(0) & y0=geo.cen1(1)
dir='/satsuki/h1/yaqoob/siscal/'
screw=1 & rf=1 & units=0 & sis=geo.inst & ccd=geo.chp
sisdet,x0,y0,ccd,sis,units,rf,screw,xdet,ydet,dir=dir
print,' geo.cen1 = ',xdet,ydet,' (mm)'
x0=geo.cen2(0) & y0=geo.cen2(1)
sisdet,x0,y0,ccd,sis,units,rf,screw,xdet,ydet,dir=dir
print,' geo.cen2 = ',xdet,ydet,' (mm)'
x=geo.d1 & y=geo.d2 
dum=fltarr(5)
sisdet,x,dum,ccd,sis,units,rf,screw,xdet,ydet,dir=dir
print,'geo.d1 (mm) ',xdet
sisdet,y,dum,ccd,sis,units,rf,screw,xdet,ydet,dir=dir
print,'geo.d2 (mm) ',xdet
return
end
