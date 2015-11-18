pro gtsbpl,det,geo,plist,slist,blist,ibgd,snone,bnone
; take a photon list (plist), type of region selection (rtype)
; and parameters of the region selection (px and py) to create
; two new selection regions for src and bkgd (ssel and bsel)
if det eq 0 then begin
 x=plist.x & y=plist.y
endif
if det eq 1 then begin
 x=plist.detx & y=plist.dety
endif
if det eq 2 then begin
 x=plist.skyx & y=plist.skyy
endif
snone=0l & bnone=0l
r1sq=geo.s1(0) & r2sq=geo.s2(0) & r3sq=geo.s2(1)
xc1=geo.cen1(0) & yc1=geo.cen1(1) & xc2=geo.cen2(0) & yc2=geo.cen2(1)
rad1 = (x-xc1)*(x-xc1)+(y-yc1)*(y-yc1)
sloc=where((rad1 le r1sq),snone)
if snone gt 0 then slist=plist(sloc)
if ibgd gt 0 then begin
if (geo.type eq 1) then $
	bloc = where(((rad1 gt r1sq) and (rad1 le r2sq)),bnone)
if geo.type eq 2 then begin
	bloc=where(((rad1 ge r2sq) and (rad1 le r3sq)),bnone)
endif
if geo.type eq 3 then begin
	rad2=(x-xc2)*(x-xc2)+(y-yc2)*(y-yc2)
	bloc=where((rad2 le r2sq),bnone)
endif
if bnone gt 0 then blist=plist(bloc)
endif
return
end
