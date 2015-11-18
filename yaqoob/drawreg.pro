pro drawreg,geo,ibgd
;draw regions for source and background extraction
rtype=geo.type
drawcirc,geo.d1(0),geo.cen1(0),geo.cen1(1)
if rtype eq 1 and ibgd gt 0 then drawcirc,geo.d2(0),geo.cen1(0),geo.cen1(1)
if rtype eq 2 and ibgd gt 0 then begin
 drawcirc,geo.d2(0),geo.cen1(0),geo.cen1(1)
 drawcirc,geo.d2(1),geo.cen1(0),geo.cen1(1)
endif
if rtype eq 3 and ibgd gt 0 then drawcirc,geo.d2(0),geo.cen2(0),geo.cen2(1)
return
end
