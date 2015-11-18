pro gislist,det,hg,tabg,glist
;convert fits gis ph mode table to photon list structure
if n_params(0) eq 0 then begin
 print,'gislist,det,hg,tabg,glist'
 retall
end
np=(size(tabg))(2)
x=tbget(hg,tabg,'RAWX') 
x=x*(x ge 0)&x=x*(x le 256)
y=tbget(hg,tabg,'RAWY') 
y=y*(y ge 0)&y=y*(y le 256)
time=tbget(hg,tabg,'TIME')
pha=tbget(hg,tabg,'PHA')  & pha = pha*(pha ge 0) & pha =pha*(pha le 1024)
pi=tbget(hg,tabg,'PI') & pi = pi*(pi ge 0) & pi=pi*(pi le 2048)
rt=tbget(hg,tabg,'RISE_TIME')
if det eq 0 then begin
 row={gevent,x:0,y:0,pha:0,pi:0,time:0.0D0,rt:0}
endif
if det gt 0 then begin
 detx=tbget(hg,tabg,'DETX') & dety=tbget(hg,tabg,'DETY')
 skyx=tbget(hg,tabg,'X') & skyy=tbget(hg,tabg,'Y')
 row={fgevent,x:0,y:0,detx:0,dety:0,skyx:0,skyy:0,pha:0,pi:0,time:0.0D0,rt:0}
endif
glist=replicate(row,np)
if np eq 1 then begin
x=x(0) & y=y(0) & time=time(0) & pha=pha(0) & pi=pi(0) & rt=rt(0)
if det gt 0 then begin
 detx=detx(0)&dety=dety(0)&skyx=skyx(0)&skyy=skyy(0)
endif
endif
glist.x=x & glist.y=y & glist.time=time & glist.pha=pha & glist.rt=rt
glist.pi=pi
if det gt 0 then begin
 glist.detx=detx&glist.dety=dety&glist.skyx=skyx&glist.skyy=skyy
endif
return
end
