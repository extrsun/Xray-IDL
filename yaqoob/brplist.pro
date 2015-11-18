pro brplist,det,hb,tabb,plist
; Place event parameters from SIS bright mode file in plist
; INPUTS, hb,tabb
; OUTPUT, plist
if n_params(0) eq 0 then begin
print,'brplist,det,hb,tabb,plist'
print,'Place event parameters from SIS bright mode file in plist'
retall
end
nct=(size(tabb))(2) 
if det eq 0 then begin
 brtab2st,hb,tabb,x,y,pha,time,grade,ccd
 row={sevent,x:0,y:0,pha:0,time:0.0D0,grade:0,ccd:0}
endif
if det gt 0 then begin
 brtab2st2,hb,tabb,x,y,detx,dety,skyx,skyy,pha,pi,time,grade,ccd
 row={fsevent,x:0,y:0,detx:0,dety:0,skyx:0,skyy:0,pha:0,$
 pi:0,time:0.0D0,grade:0,ccd:0}
endif
plist=replicate(row,nct)
plist.x=x&plist.y=y&plist.pha=pha&plist.time=time&plist.grade=grade
plist.ccd=ccd
if det gt 0 then begin
 plist.detx=detx & plist.dety=dety & plist.skyx=skyx & plist.skyy=skyy
 plist.pi=pi
endif
return
end

