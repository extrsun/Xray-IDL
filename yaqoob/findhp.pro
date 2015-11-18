pro findhp,plist,nhpm,hpm=hpm
;Remove hot pixels from SIS data
; INPUTS, plist,hpm(optional)
; OUTPUT, modified plist,nhpm =>new hot pxl mtrx
if n_params(0) eq 0 then begin
print,'FINDHP,plist,nhpm,hpm=hpm
print,'Remove hot pixels from SIS data'
retall
end
if n_elements(hpm) ne 0 then goto,fhp1
x=plist.x & y=plist.y & m=intarr(884,882) & n=(size(plist))(1)-1
x = x > 0 & x = x < 883 & y = y > 0 & y = y < 881
for i=0l,n do m(x(i),y(i))=m(x(i),y(i))+1
a=intarr(20) & for i=1,19 do a(i)=total(m ge i)
print,[transpose(indgen(19)+1),transpose(a(1:*))]
read,'Enter desired cutoff: ',n & ans=' '
hpm=m ge n & if total(hpm) eq 0.0 then goto,fhp2
fhp1:xysis,hpm,xyl & hpmask,x,y,xyl,mout & nhpm=hpm
read,'Ready to modify plist? y/(n): ',ans
if ans ne 'y' then goto,fhp2
plist=plist(where(mout))
fhp2:return
end


