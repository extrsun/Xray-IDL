pro ascaoff,plist,posdet,rf,inst,dir=dir,image,geo=geo,sm=sm,sx,sy,sr
if n_params(0) eq 0 then begin
 print,'ascaoff,plist,posdet,rf,inst,dir=dir,geo=geo,sm=sm,sx,sy,sr'
 print,'compute input to JBLDARF and distance from optical axis '
 print,'PLIST: input, photon list'
 print,'POSDET: (for GIS) = 0 for FLF, =1 for POW'
 print,'RF: (for SIS) = 0 if coords are truly RAW'
 print,'              = 1 if they were converted using raw2new'
 print,'INST: =0 (SIS0) =1 (SIS1) =2 (GIS2) =3 (GIS3)'
 print,'DIR: name of directory containing appropriate calibration file'
 print,'/sm: smooth image before finding centroid'
 print,'geo=geo: use x,y in geo for centroid'
 print,'sx sy sr : output-source centroid and distance from opt axis'
 retall
end
x=plist.x & y=plist.y
cmpxy = 1
if n_elements(geo) ne 0 then begin
  cmpxy = 0
  xcen = geo.cen1(0)
  ycen = geo.cen1(1)
  chp = geo.chp
  print,'Using (',strn(xcen),',',strn(ycen),') as centroid'
endif
;GIS
if inst ge 2 then begin
 gtgismap,posdet,inst,dir,gmap,cal,detx,dety,deltax,deltay,coeff
 gislinxy,coeff,cal,detx,dety,deltax,deltay,x,y,xlin,ylin,ip,jp
;convert to mm
 xdet=(xlin-128.5)*0.25 & ydet=(ylin-128.5)*0.25 
;parameters to make an image to find the centroid
 dims=[280,280]
xran=[-35.,35.] & yran=[-35.,35.] & dims=[280,280]
endif
if inst le 1 then begin
;SIS
 units=0 & screw=2
;warning the value of screw depends on exactly how the rotation angle
;is defined as it may include a 'flip'. For (s1)s0_alignment_ascalin.fits
;screw=1 but for sis0_telescope_def.fits screw =2 
 ccd=plist.ccd
 sisdet,x,y,ccd,inst,units,rf,screw,xdet,ydet,dir=dir
if not cmpxy then  sisdet,xcen,ycen,chp,inst,units,rf,screw,xc,yc,dir=dir
;units are already in mm
;parameters to make an image to find the centroid 
dims=[480,480]
xran=[-12.,12.] & yran=[-12.,12.] & dims=[480,480]
endif
xmean=total(xdet)/(size(xdet))(1)
ymean=total(ydet)/(size(ydet))(1)
print,' Mean X, Y of events ',xmean,ymean
;read,'Enter half width of image (mm) to compute centroid ',hfw
;xran=[xmean-hfw,xmean+hfw] & yran=[ymean-hfw,ymean+hfw]
;xrl=min(xdet) & xrh=max(xdet) & yrl=min(ydet) & yrh=max(ydet)
;xran=[xrl,xrh] & yran=[yrl,yrh]
if cmpxy then begin 
XY2IMAGE,xdet,ydet,ydet,xran,yran,dims,xbns,ybns,image
if n_elements(sm) gt 0 then image = smooth(image,3)
centroid,image,xcen,ycen
xc = xbns(fix(xcen-1),2) & yc = ybns(fix(ycen-1),2) 
endif
rad=sqrt((xdet-xmean)*(xdet-xmean)+(ydet-ymean)*(ydet-ymean))
;xmean=total(xdet)/(size(xdet))(1)
;ymean=total(ydet)/(size(xdet))(1)
;print,'Source centre in mm ',xmean,ymean
print,'Source centre in mm ',xc,yc
radmax=max(rad)
print,'Maximum radius from source centre (mm)',radmax
regx0=(max(xdet)+min(xdet))/2. 
regy0=(max(ydet)+min(ydet))/2. 
print,'Average X Y of region (mm) ',regx0,regy0
regradmm=(abs(max(xdet)-min(xdet))+abs(max(ydet)-min(ydet)))/4.
print,'Average region radius (mm) ',regradmm
;define the optical axes in these detector coords
;xopt=[0.6,-0.6,1.3,-2.3] & x0=xopt(inst)
;yopt=[2.2,-3.6,0.8,1.0] & y0=yopt(inst)
;below are new optical axes reflecting changes in jbldarf. 23/12/93
xopt=[0.6,-0.6,1.0,-2.41] & x0=xopt(inst)
yopt=[-2.2,3.6,0.49,1.36] & y0=yopt(inst)
plate_scale=0.98220003 
rad1=sqrt((xc-x0)*(xc-x0)+(yc-y0)*(yc-y0))
print,' Distance of centroid from optical axis (arcmin) ~ ',rad1*plate_scale
window,0,xsize=600,ysize=600
if inst le 1 then plot,xdet,ydet,xr=[-12.,12.],yr=[-12.,12.],psym=3
if inst ge 2 then plot,xdet,ydet,xr=[-32.,32.],yr=[-32.,32.],psym=3
drawcirc,radmax,xmean,ymean
oplot,[xc-15,xc+15],[yc,yc] & oplot,[xc,xc],[yc-15,yc+15]
sx=xc & sy=yc & sr=rad1*plate_scale
return
end
