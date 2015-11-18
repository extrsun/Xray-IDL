pro cmpbscl,geo
if n_params(0) eq 0 then begin
print,'cmpbscl,geo '
print,' compute area scale factor for background '
retall
end
if (geo.type eq 1) then begin
 xc1 = (geo.cen1)(0) & yc1=(geo.cen1)(1)
 r1sq=(geo.s1)(0) & r2sq=(geo.s2)(0)
 r1=(geo.d1)(0) & r2=(geo.d2)(0)
 xc2=xc1 & yc2=yc1
endif
if geo.type eq 2 then begin
 xc1=(geo.cen1)(0) & yc1=(geo.cen1)(1)
 r1sq=(geo.s1)(0) & r2sq=(geo.s2)(0) & r3sq=(geo.s2)(1)
 r1=(geo.d1)(0) & r2=(geo.d2)(0) & r3=geo.d2(1)
 xc2=xc1 & yc2=yc1
endif
if geo.type eq 3 then begin
 xc1=(geo.cen1)(0) & yc1=(geo.cen1)(1)
 xc2=(geo.cen2)(0) & yc2=(geo.cen2)(1)
 r1sq=(geo.s1)(0) & r2sq=(geo.s2)(0)
 r1=(geo.d1)(0) & r2=(geo.d2)(0)
endif
if (geo.type gt 3) then begin
 print,' Region type not recognized ** ABORTING ** '
 retall
end
instr=geo.inst & chip=geo.chp 
;given circle centre, source radius and bgd radius, compute
;ratio of area of backround annulus to source circle area
if instr ge 0 and instr le 1 then begin
chipbnd,instr,chip,xl,xu,yl,yu
print,'chip bounds: ',xl,xu,yl,yu
genrimg,xl,xu,yl,yu,xc1,yc1,img
src=float((size(where((img le r1sq) and (img ge 0))))(1))
genrimg,xl,xu,yl,yu,xc2,yc2,img
if geo.type eq 1 then $
bgd=float((size(where((img gt r1sq) and (img le r2sq))))(1))
if geo.type eq 2 then $
bgd=float((size(where((img gt r2sq) and (img le r3sq))))(1))
if geo.type eq 3 then $
bgd=float((size(where((img le r2sq) and (img ge 0))))(1))
src=src-geo.d1(4) -geo.s1(4) & bgd=bgd-geo.d2(4)-geo.s2(4)
print,' Hot pixel locations in src and bgd regions',$
geo.d1(4),geo.d2(4)
endif
if instr ge 2 and instr le 5 then begin
 src=geo.s1(0) -geo.s1(4) & bgd = geo.s2(0) - geo.s1(0) - geo.s2(4)
 if geo.type eq 1 then $
 bgd=geo.s2(0) - geo.s1(0) - geo.s2(4)
 if geo.type eq 2 then $
 bgd=geo.s2(1)-geo.s2(0)-geo.s2(4)
 if geo.type eq 3 then $
 bgd=geo.s2(0)-geo.s2(4)
endif
print,'src = ',src,' bgd = ',bgd
bscale=0.00
if src gt 0.0 then bscale = bgd/src
geo.bscl = bscale
print,' bkgd scaling factor bgd/src = ',geo.bscl
if geo.bscl gt 0.0 then print,' 1/geo.bscl = ',1./geo.bscl
return
end

