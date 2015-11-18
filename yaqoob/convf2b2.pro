pro convf2b2,det,split,echo,gmax,flist,clist,null
;convert faint mode photon list to bright mode
;inputs: split, gmax - split thresh and max grade number to keep (1-8)
;	flist - faint mode photon list
;outputs: clist - converted photon list
np=(size(flist))(1) & null = 0
phas=flist.phas & faint2b,phas,split,echo,pha,gd
if np le 1 then begin
 pha = pha(0) & gd=gd(0)
endif
pha = pha > 0 & pha = pha < 4096
if det eq 0 then row={sevent,x:0,y:0,pha:0,time:0.0D0,grade:0,ccd:0}
if det gt 0 then row={fsevent,x:0,y:0,detx:0,dety:0,skyx:0,skyy:0,$
pha:0,pi:0,time:0.0D0,grade:0,ccd:0}
clist=replicate(row,np)
clist.x=flist.x & clist.y=flist.y & clist.time=flist.time
pha=pha*(pha le 1024)+(512+pha/2)*((pha gt 1024) and (pha le 2048))$
+(1024+pha/4)*((pha gt 2048)and(pha le 4096))
clist.pha = pha & clist.grade=gd+1 & clist.ccd=flist.ccd
if det gt 0 then begin
 clist.detx=flist.detx & clist.dety=flist.dety
 clist.skyx=flist.skyx & clist.skyy=flist.skyy
 clist.pi=clist.pha
endif
if min(clist.grade) gt gmax then null = -1
if null ge 0 then clist=clist(where((clist.grade) le gmax))
return
end
