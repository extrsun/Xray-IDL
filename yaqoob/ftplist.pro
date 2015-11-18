pro ftplist,det,hf,tabf,flist
;convert fits faint mode table to photon list structure
np=(size(tabf))(2)
x=tbget(hf,tabf,'RAWX') & x = x*(x ge 0) & x = x*(x le 424)
y=tbget(hf,tabf,'RAWY') & y = y*(y ge 0) & y = y*(y le 421)
time=tbget(hf,tabf,'TIME')
phas=tbget(hf,tabf,'PHAS') 
ccd=tbget(hf,tabf,'CCDID')
dum=intarr(9)
if det eq 0 then begin
 row={fevent,x:0,y:0,phas:dum,time:0.0D0,ccd:0}
endif
if det gt 0 then begin
 detx=tbget(hf,tabf,'DETX') & dety=tbget(hf,tabf,'DETY')
 skyx=tbget(hf,tabf,'X') & skyy=tbget(hf,tabf,'Y')
 row={ffevent,x:0,y:0,detx:0,dety:0,skyx:0,skyy:0,phas:dum,time:0.0D0,ccd:0}
endif
flist=replicate(row,np)
if np eq 1 then begin
x=x(0) & y=y(0) & time=time(0) & ccd=ccd(0)
 if det gt 0 then begin
  detx=detx(0)&dety=dety(0)&skyx=skyx(0)&skyy=skyy(0)
 endif
endif
flist.x=x & flist.y=y & flist.time=time & flist.phas=phas 
flist.ccd=ccd
if det gt 0 then begin
 flist.detx=detx &flist.dety=dety& flist.skyx=skyx &flist.skyy=skyy
endif
return
end
