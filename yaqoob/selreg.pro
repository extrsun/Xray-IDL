pro selreg,det,oldgeo,geo,plist,slist,blist,byhand=byhand
;Author T. Yaqoob - March 1993->**
;Last updated 29/10/95
if n_params(0) eq 0 then begin
  print,'selreg,det,oldgeo,geo,plist,slist,blist,byhand=byhand'
  print,'select 1 or 2 regions from a photon list [blist optional]'
  print,'DET	:=0 selects on RAW coords '
  print,'	:=1 selects on DETECTOR coords '
  print,'	:=2 selects on SKY coords '
  print,'BYHAND :=0 for cursor, =1 for manual keyboard selection'
  retall
end
geo=oldgeo
if n_elements(byhand) eq 0 then byhand=0
;find out if we are actually carrying detector coords
if det gt 0 then begin
 tags=tag_names(plist) 
 wd=where((tags eq 'DET'),nwd)
 if nwd gt 0 then geo.det=det
 if det eq 1 then wdet=where((tags eq 'DETX'),nwdet) 
 if det eq 2 then wdet=where((tags eq 'SKYX'),nwdet) 
 if nwdet eq 0 then begin
  print,'Photon list has no DETECTOR or SKY coords - will select on RAW'
  det=0
 endif
endif 
print,'Photon list size on entry ',(size(plist))(1)
if det eq 0 then begin     
 px=plist.x & py=plist.y
endif
if det eq 1 then begin 
 px=plist.detx & py=plist.dety
endif
if det eq 2 then begin
 px=plist.skyx & py=plist.skyy
endif
if n_params(0) lt 6 then ibgd=0 else ibgd=1
if geo.inst le 1 and det eq 0 then begin 
window,0,xsize=864,ysize=848
plot,px,py,xr=[0,864],yr=[0,848],psym=3
endif
if geo.inst le 1 and det eq 1 then begin
window,0,xsize=900,ysize=900
plot,px,py,xr=[100,1100],yr=[100,1100],psym=3
endif
if geo.inst le 1 and det eq 2 then begin
window,0,xsize=900,ysize=900
plot,px,py,xr=[0,1300],yr=[0,1300],psym=3
endif
if geo.inst gt 1 and geo.inst le 3 then begin
window,0,xsize=900,ysize=900
plot,px,py,xr=[0,260],yr=[0,260],psym=3
endif
if geo.inst gt 3 then begin
yr=minmax(py) & xr=minmax(px)
window,0,xsize=800,ysize=800
plot,px,py,xr=xr,yr=yr,psym=3
endif
rtype=0
gtregion,rtype,byhand,geo,ibgd
geo.type=rtype
gtsbpl,det,geo,plist,slist,blist,ibgd,snone,bnone
;print,' snone & bnone ',snone,bnone
print,' Selected source events :',(size(slist))(1)
print,' Selected background events :',(size(blist))(1)
if geo.inst le 1 then begin
read,'Select SIS chip (0-3 and 4 = ALL)',chip
nwcs=0 & nwcb=0
wcs=where((slist.ccd eq chip),nwcs)
if ibgd gt 0 then wcb=where((blist.ccd eq chip),nwcb)
if nwcs eq 0 then print,'No src events in requested chip '$
 else slist=slist(wcs)
if nwcb eq 0 then print,'No bgd events in requested chip '$
 else blist=blist(wcb)
geo.chp=chip
print,' Selected source events :',(size(slist))(1)
print,' Selected background events :',(size(blist))(1)
endif
if ibgd eq 0 then begin
 print,'Resetting BGD parameters in GEO '
 geo.d2(0:4)=fltarr(5) & geo.s2(0:4)=fltarr(5)
endif
return
end
