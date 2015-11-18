pro gtregion,rtype,byhand,geo,ibgd
; get source and background regions interactively from users
;the following is temporary ; use cursor input soon
px=fltarr(4)
py=fltarr(4)
rad=fltarr(5)
rsq=fltarr(5)
x0=fltarr(2)
y0=fltarr(2)
keyb=0
if rtype gt 3 then rtype =0
if rtype le 0 then begin
print, ' '
print,' Types of region available: '
print,' 1: concentric circles for source and background '
print,' 2: concentric circles for source and background (diff radii)'
print,' 3: separate circles for source and background '
lab1: read,' Enter Region Type ',rtype
geo.type=rtype
if (rtype gt 3) then  goto, lab1
endif
if ((rtype eq 1) and (keyb gt 0)) then begin
print,' Enter x y coord of centre of circles' & read,p1,p2
px(0) = p1 & py(0) = p2
print,' Enter radius of inner circle ' & read,p1 & p2 =0.0
px(1) = p1+px(0) & py(1) = p2+py(0)
if ibgd gt 0 then begin
print,' Enter radius of outer circle ' & read,p1 & p2 =0.0
px(2) = p1+px(0) & py(2) = p2+py(0)
endif
endif
if rtype eq 1 then begin 
 nrad=2 & ncen=1
endif
if rtype eq 2 then begin
 nrad=3 & ncen=1
endif
if rtype eq 3 then begin
 nrad=2 & ncen=2
endif
if ((rtype eq 1) and (keyb eq 0)) then begin
 if byhand eq 0 then begin 
 print,' Click cursor for centre of circles' & cursor,p1,p2,4  
 endif
 if byhand gt 0 then read,'Enter centre of circles (x,y)',p1,p2 
px(0) = p1 & py(0) = p2
x0(0)=p1 & y0(0)=p2
geo.cen1(0)=p1 & geo.cen1(1)=p2 
 if byhand eq 0 then begin
  print,' Click cursor for radius of inner circle ' & cursor,p1,p2,4
  px(1) = p1 & py(1) = p2
  rsq(0)=(px(1)-px(0))*(px(1)-px(0))+(py(1)-py(0))*(py(1)-py(0))
  if rsq(0) gt 0. then rad(0)=sqrt(rsq(0)) else rad(0) =0.
 endif
 if byhand gt 0 then begin 
	read,'Enter radius of inner circle ',rr1
	rad(0)=rr1 & rsq(0)=rr1*rr1
 endif
if rad(0) gt 0. then drawcirc,rad(0),px(0),py(0)
geo.d1(0)=rad(0) & geo.s1(0)=rsq(0)
if ibgd gt 0 then begin
geo.cen2(0)=geo.cen1(0) & geo.cen2(1)=geo.cen1(1)
if byhand eq 0 then begin
 print,' Click cursor for radius of outer circle ' & cursor,p1,p2,4
 px(2) = p1 & py(2) = p2
 rsq(1)=(px(2)-px(0))*(px(2)-px(0))+(py(2)-py(0))*(py(2)-py(0))
 if rsq(1) gt 0. then rad(1)=sqrt(rsq(1)) else rad(0) =0.
endif
if byhand gt 0 then begin
 read,'Enter  radius of outer circle ',rr2
 rad(1)=rr2 & rsq(1)=rr2*rr2
endif
if rad(1) gt 0. then drawcirc,rad(1),px(0),py(0)
geo.d2(0)=rad(1) & geo.s2(0)=rsq(1)
endif
endif
if rtype eq 2 and (keyb eq 0) then begin
if byhand eq 0 then begin
 print,'Click cursor for centre of circles ' & cursor,p1,p2,4
endif
if byhand gt 0 then read,'Enter centre of circles (x,y)',p1,p2
 px(0)=p1 & py(0) =p2 & x0(0)=p1 & y0(0)=p2
 geo.cen1(0)=p1 & geo.cen1(1)=p2 
if byhand eq 0 then begin
 print,'Click cursor for outer radius of srce circle ' & cursor,p1,p2,4
 px(1)=p1 & py(1)=p2
 rsq(0)=(px(1)-px(0))*(px(1)-px(0))+(py(1)-py(0))*(py(1)-py(0))
 if rsq(0) gt 0 then rad(0)=sqrt(rsq(0)) else rad(0)=0.0
endif
if byhand gt 0 then begin
 read,'Enter outer radius of srce circle ',rr1
 rad(0)=rr1 & rsq(0)=rr1*rr1
endif
if rad(0) gt 0. then drawcirc,rad(0),px(0),py(0)
geo.d1(0) =rad(0) & geo.s1(0)=rsq(0)
if ibgd gt 0 then begin
geo.cen2=geo.cen1
if byhand eq 0 then begin
 print,'Click cursor for inner radius of bgd circle ' & cursor,p1,p2,4
 px(2)=p1 & py(2)=p2
 rsq(1)=(px(2)-px(0))*(px(2)-px(0))+(py(2)-py(0))*(py(2)-py(0))
 if rsq(1) gt 0. then rad(1)=sqrt(rsq(1)) else rad(1) =0.0
endif
if byhand gt 0 then begin
 read,'Enter inner radius of bgd circle ',rr2
 rad(1)=rr2 & rsq(1)=rr2*rr2
endif
if rad(1) gt 0.0 then drawcirc,rad(1),px(0),py(0)
geo.d2(0)=rad(1) & geo.s2(0)=rsq(1)
if byhand eq 0 then begin
 print,'Click cursor for outer radius of bgd circle ' & cursor,p1,p2,4
 px(3)=p1 & py(3)=p2
 rsq(2)=(px(3)-px(0))*(px(3)-px(0))+(py(3)-py(0))*(py(3)-py(0))
 if rsq(2) gt 0. then rad(2)=sqrt(rsq(2)) else rad(2) = 0.0
endif
if byhand gt 0 then begin
 read,'Enter outer radius of bgd circle ',rr3
 rad(2)=rr3 & rsq(2)=rr3*rr3
endif
if rad(2) gt 0. then drawcirc,rad(2),px(0),py(0) 
geo.d2(1)=rad(2) & geo.s2(1)=rsq(2)
endif
endif
if rtype eq 3 then begin
if byhand eq 0 then begin
 print,'Click cursor for centre of source circle ' & cursor,p1,p2,4
endif
if byhand gt 0 then read,'Enter centre of source circle (x,y)',p1,p2 
 px(0)=p1 & py(0)=p2 & x0(0)=p1 & y0(0)=p2
 geo.cen1(0)=p1 & geo.cen1(1)=p2
if byhand eq 0 then begin
 print,'Click cursor for radius of source circle ' & cursor,p1,p2,4
 px(1)=p1 & py(1)=p2
 rsq(0)=(px(1)-px(0))*(px(1)-px(0))+(py(1)-py(0))*(py(1)-py(0))
 if rsq(0) gt 0. then rad(0) = sqrt(rsq(0)) else rad(0)=0.
endif
if byhand gt 0 then begin
 read,'Enter radius of source circle ',rr1
 rad(0)=rr1 & rsq(0)=rr1*rr1
endif
if rad(0) gt 0. then drawcirc,rad(0),px(0),py(0)
geo.d1(0)=rad(0) & geo.s1(0)=rsq(0)
if ibgd gt 0 then begin
 if byhand eq 0 then begin
  print,'Click cursor for centre of bgd circle ' & cursor,p1,p2,4
 endif 
 if byhand gt 0 then read,'Enter centre of bgd circle (x,y)',p1,p2
 px(2)=p1 & py(2)=p2 & x0(1)=p1 & y0(1)=p2
 geo.cen2(0)=p1 & geo.cen2(1)=p2
 if byhand eq 0 then begin
  print,'Click cursor for radius of bgd circle ' & cursor,p1,p2,4
  px(3)=p1 & py(3)=p2
  rsq(1)=(px(3)-px(2))*(px(3)-px(2))+(py(3)-py(2))*(py(3)-py(2))
  if rsq(1) gt 0. then rad(1)=sqrt(rsq(1)) else rad(1)=0.
 endif
 if byhand gt 0 then begin
  read,'Enter radius of bgd circle ',rr2
  rad(1)=rr2 & rsq(1)=rr2*rr2
 endif
if rad(1) gt 0. then drawcirc,rad(1),px(2),py(2)
geo.d2(0)=rad(1) & geo.s2(0)=rsq(1)
endif
endif
for k=0,ncen-1 do print,'Centre(s) ',x0(k),y0(k)
for k=0,nrad-1 do print,'Radii     ',rad(k)
return
end
