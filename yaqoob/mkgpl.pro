pro mkgpl,h,tab,plist
; Place selected event parameters from GIS fits file in plist
; INPUTS, h,tab
; OUTPUT, plist
if n_params(0) eq 0 then begin
print,'MKGPL,h,tab,plist'
print,'Place selected event parameters from GIS fits file in plist'
retall
end
nct=(size(tab))(2) & assigng,h,tab,x,y,pha,time,rtm
row={gevent,x:0,y:0,pha:0,time:0.0D0,rtm:0}
plist=replicate(row,nct)
plist.x=x&plist.y=y&plist.pha=pha&plist.time=time&plist.rtm=rtm
return
end

