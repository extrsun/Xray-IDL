pro jarfpars,geo,plist,smo,nocent=nocent
;Author T. Yaqoob - March 1993->** 
if n_params(0) eq 0 then begin
 print,'jarfpars,geo,plist,smo,nocent=nocent'
 print,'Take a GEO and photon list and output parameters for JBLDARF'
 print,'SMO	:smoothing factor for image to compute centroid'
 print,'	:< eq 1 for no smoothing'
 print,'NOCENT  :<= 0 don t compute centroid (useful for 4CCD)'
 retall
end
print,'** YOUR PHOTON LIST AND GEO MUST CARRY DETECTOR COORDS**'
x=plist.detx & y=plist.dety & pha=plist.pha
xran=minmax(x)-1.0 & yran=minmax(y)-1.0
dims=[(xran(1)-xran(0)+1),(yran(1)-yran(0)+1)]
XY2IMAGE,x,y,pha,xran,yran,dims,xbns,ybns,image,pimage
if nocent le 0 then begin
if smo gt 1 then centroid,smooth(image,smo),xcen,ycen else $
 centroid,image,xcen,ycen
xc=xbns(fix(xcen),2) & yc=ybns(fix(ycen),2)
endif
if nocent gt 0 then begin
 xc=geo.cen1(0) & yc=geo.cen1(1)
endif
if geo.inst le 1 then begin
 pix=0.027 & xicen=640.5 & yicen=640.5
endif
if geo.inst ge 2 and geo.inst le 3 then begin
 pix=0.25 & xicen=128.5 & yicen=128.5
endif
if geo.inst gt 3 then begin
 print,'This routine can only be used for ASCA SIS and GIS'
 goto, fin
endif
xcmm=(xc-xicen)*pix & ycmm=(yc-yicen)*pix
print,'SOURCE CENTROID : '
print,'Detector Pixels (xc,yc): ',xc,yc
print,' MM units       (xc,yc): ',xcmm,ycmm
print,' '
print,'Circle region center [geo.cen1]: ',geo.cen1(0),geo.cen1(1)
regx=(geo.cen1(0)-xicen)*pix & regy = (geo.cen1(1)-yicen)*pix
print,'Circle region centre in mm     : ',regx,regy
print,' '
print,' ** Cirle Radii ** '
print,'GEO.D1(0) [DET pixels]: ',geo.d1(0)
print,'GEO.D1(0) [mm]        : ',geo.d1(0)*pix       
print,'GEO.D2(0) [DET pixels]: ',geo.d2(0)
print,'GEO.D2(0) [mm]        : ',geo.d2(0)*pix
print,'GEO.D2(1) [DET pixels]: ',geo.d2(1)
print,'GEO.D2(1) [mm]       ]: ',geo.d2(1)*pix
fin: return
end
