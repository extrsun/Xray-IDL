pro sqsel,plist,slist,xb=xb,yb=yb
; Select a rectangular source region defined by the vectors xb and yb
if n_params(0) eq 0 then begin
print,SQSEL,plist,slist,xb=xb,yb=yb
print,'Select a rectang source region defined by the vectors xb and yb'
retall
end
x=plist.x & y=plist.y
chout=((x gt xb(0)) and (x le xb(1))) and ((y gt yb(0)) and (y le yb(1)))
slist=plist(where(chout))
return
end
