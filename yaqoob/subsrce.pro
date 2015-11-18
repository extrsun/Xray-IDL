pro subsrce,geo,slist,sublist
if n_params(0) eq 0 then begin
  print,'SUBSRCE, GEO, SLIST, SUBLIST'
  print,' select a sub-source list from a restriced region of slist '
  print,' i.e. specify new radius '
  retall
endif
;first plot the image
mkimage,slist,simg
shade_surf,smooth(float(simg),3.)
np=(size(slist))(1)
print,' Number of events in original photon list ',np
x=slist.x & y=slist.y & x0 = geo.cen1(0) & y0=geo.cen1(1)
rad=(x-x0)*(x-x0)+(y-y0)*(y-y0)
print,' Radius of extraction of original region was ',geo.d1(0)
read,' Enter new radius ',r0 & r0sq=r0*r0
sublist=slist(where(rad le r0sq))
print,' Number of events in new photon list ',(size(sublist))(1)
;modify geo
geo.type=2 & geo.d1(0)=r0 & geo.s1(0)=r0sq
srce=geo.s1(0)-geo.d1(4)
bgd=geo.s2(0)-geo.s1(0)-geo.d2(4)
if srce gt 0.0 then geo.bscl=bgd/srce else geo.bscl = 0.0
print,' New background scale factor = ',geo.bscl
mkimage,sublist,simg
shade_surf,smooth(float(simg),3.)
return
end