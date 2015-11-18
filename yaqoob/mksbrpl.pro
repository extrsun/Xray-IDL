pro mksbrpl,h,tab,plist
; Place event parameters from SIS bright file in plist
; INPUTS, h,tab
; OUTPUT, plist
if n_params(0) eq 0 then begin
print,'MKSBRPL,h,tab,plist'
print,'Place event parameters from SIS bright file in plist'
retall
end
nct=(size(tab))(2) & assign,h,tab,x,y,pha,time,grade
row={xevent,x:0,y:0,pha:0,time:0.0D0,grade:0}
plist=replicate(row,nct)
plist.x=x&plist.y=y&plist.pha=pha&plist.time=time&plist.grade=grade
return
end

