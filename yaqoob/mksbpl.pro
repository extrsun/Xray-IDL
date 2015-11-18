pro mksbpl,hb,tabb,plist
; Place event parameters from SIS bright mode file in plist
; INPUTS, hb,tabb
; OUTPUT, plist
if n_params(0) eq 0 then begin
print,'MKSBPL,hb,tabb,plist'
print,'Place event parameters from SIS bright mode file in plist'
retall
end
nct=(size(tabb))(2) & assigns,hb,tabb,x,y,pha,time,grade
row={sevent,x:0,y:0,pha:0,time:0.0D0,grade:0}
plist=replicate(row,nct)
plist.x=x&plist.y=y&plist.pha=pha&plist.time=time&plist.grade=grade
return
end

