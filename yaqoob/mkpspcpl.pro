pro mkpspcpl,h,tab,plist
if n_params(0) eq 0 then begin
 print,' MKPSPCPL, h, tab, plist '
 print,' Make photon list structure for PSPC from events table '
 retall
endif
np=(size(tab))(2)
x=tbget(h,tab,'X') & x = x > 0l & x = x< 16384l
y=tbget(h,tab,'Y') & y = y > 0l & y = y< 16384l
dx=tbget(h,tab,'DX') & dx =dx > 0l & dx = dx < 8192l
dy=tbget(h,tab,'DY') & dy = dy > 0l & dy = dy< 8192l
pha=tbget(h,tab,'PHA') & pha = pha > 0 & pha = pha < 256
pi=tbget(h,tab,'PI') & pi=pi > 0 & pi = pi < 512
time=tbget(h,tab,'TIME')
row={pspceve,x:0l,y:0l,dx:0l,dy:0l,pha:0,pi:0,time:0.0D0}
plist=replicate(row,np)
if np eq 1 then begin
 x=x(0) & y=y(0) & dx=dx(0) & dy=dy(0) & pha=pha(0) & pi=pi(0) 
 time=time(0)
endif
plist.x=x & plist.y=y & plist.dx=dx & plist.dy=dy
plist.pha=pha & plist.pi=pi & plist.time=time
return
end
