pro mksfpl,hf,tabf,plist
; Place event parameters from SIS faint mode file in plist
; INPUTS, hf,tabf
; OUTPUT, plist
if n_params(0) eq 0 then begin
print,'MKSFPL,hf,tabf,plist'
print,'Place event parameters from SIS faint mode file in plist'
retall
end
nct=(size(tabf))(2) & assigns,hf,tabf,x,y,pha,time,grade
row={sevent,x:0,y:0,pha:0,time:0.0D0,grade:0}
plist=replicate(row,nct)
plist.x=x&plist.y=y&plist.time=time&plist.grade=grade
pha=pha*(pha le 1024)+(512+pha/2)*((pha gt 1024) and (pha le 2048))$
+(1024+pha/4)*((pha gt 2048)and(pha le 4096)) & plist.pha=pha
return
end

