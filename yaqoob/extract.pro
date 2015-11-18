pro extract,plist,x0,y0,ri,ro,inst,pos,dir=dir,slist
if n_params(0) eq 0 then begin
 print,'extract,plist,x0,y0,ri,ro,inst,pos,dir=dir,slist'
 print,'EXTRACT a photon list from an annulus in mm detector '
 print,' coords. The coord system is the same as that used in '
 print,' jbldarf. '
 print,' INPUTS : plist - input photon list '
 print,'        : x0, y0 - centre of extraction region '
 print,'        : ri, ro - inner and outer radius of region '
 print,'        : inst   - instrument 0,1,2,3 for S0,S1,S2,S3'
 print,'        : pos    - position detection method (GIS):flf=0,pow=1'
 print,'        : dir    - directory containing calibration files'
 print,' OUTPUT : slist  - selected photon list '
 retall
end
if n_elements(dir) eq 0 then begin
 dir=' '
 read,' Enter name of directory containing cal file (include slash)',dir
endif
if inst le 1 then begin
; SIS
 units=0 & rf = 1 & screw = 1 & sis=inst
 sisdet,plist.x,plist.y,plist.ccd,sis,units,rf,screw,xdet,ydet,dir=dir 
 xl = 12. & yl = 12.
endif
if inst ge 2 and inst le 3 then begin
;GIS
  GTGISMAP, pos, inst, dir, gmap,cal,dtx,dty,deltax,deltay,coeff
  GISLINXY,coeff,cal,dtx,dty,deltax,deltay,plist.x,plist.y,xlin,ylin,xlp,ylp
  xdet=(xlin-128.5)*0.25 & ydet=(ylin-128.5)*0.25
  xl = 35. & yl = 35.
endif
 radsq = (xdet-x0)*(xdet-x0)+(ydet-y0)*(ydet-y0)
 rad=fltarr((size(radsq))(1))
 rad(where(radsq gt 0.0)) = sqrt(radsq)  
 slist = plist(where((rad ge ri) and (rad lt ro)))
if inst le 1 then begin
 sisdet,slist.x,slist.y,plist.ccd,sis,units,rf,screw,sxdet,sydet,dir=dir
endif
if inst ge 2 and inst le 3 then begin
GTGISMAP, pos, inst, dir, gmap,cal,dtx,dty,deltax,deltay,coeff
GISLINXY,coeff,cal,dtx,dty,deltax,deltay,slist.x,slist.y,sxlin,sylin,xlp,ylp
sxdet=(sxlin-128.5)*0.25 & sydet=(sylin-128.5)*0.25
endif
 plot,xr=[-xl,xl],yr=[-yl,yl],sxdet,sydet,psym=3
return
end
