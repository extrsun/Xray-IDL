pro nhotpix,geo,nsrce,nbkgd,hpm,ibgd
;find number of hot pixels removed in source and bkgd regions
rtype=geo.type & instr=geo.inst & chip=geo.chp
chipbnd,instr,chip,xl,xu,yl,yu 
xdim = (size(hpm))(1) & ydim=(size(hpm))(2)
nsrce=0l & nbkgd=0l
if rtype eq 1 then begin
r1sq=geo.s1(0) & r2sq=geo.s2(0) 
xc=geo.cen1(0) & yc=geo.cen1(1)
sxysis,hpm,xyl & nxy=(size(xyl))(2) 
for k=0l,nxy-1 do begin
 x=xyl(0,k) & y=xyl(1,k)
if (x ge xl) and (x le xu) and (y ge yl) and (y le yu) then begin
 rr = (x-xc)*(x-xc)+(y-yc)*(y-yc)
 if (rr le r1sq) then nsrce=nsrce+1
 if (rr gt r1sq) and (rr le r2sq) and ibgd gt 0 then nbkgd=nbkgd+1
endif
endfor
;print,'1st method: nsrce & nbkgd ',nsrce,nbkgd
;nsrce=0l & nbkgd=0l
;for i=xl,xu do begin 
;	xsq= float(i)-xc & xsq=xsq*xsq
;  for j=yl,yu do begin
;	ysq=float(j)-yc & ysq=ysq*ysq
;	rad = xsq+ysq
;	if hpm(i,j) gt 0 then begin
;	  if (rad le r1sq) then nsrce=nsrce+1
;  if ((rad gt r1sq) and (rad le r2sq)) and (ibgd gt 0) then nbkgd=nbkgd+1
;	endif
;  endfor
;endfor
endif
return
end
