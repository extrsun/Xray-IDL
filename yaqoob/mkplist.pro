pro mkplist,x,y,pha,time,grade,plist,nct=nct
if n_params(0) eq 0 then begin
print,'MKPLIST,x,y,pha,time,grade,plist,nct=nct
print,'Place event parameters in a structure plist'
retall
end
if n_elements(nct) eq 0 then read,'Enter event number: ',nct
row={xevent,x:0,y:0,pha:0,time:0.0D0,grade:0}
plist=replicate(row,nct)
plist.x=x&plist.y=y&plist.pha=pha&plist.time=time&plist.grade=grade
return
end

