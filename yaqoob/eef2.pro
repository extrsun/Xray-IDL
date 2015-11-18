function eef2,m,r,b
; Total counts within 'r' pxl rad off location 'b' in mtrx 'm'
sz=size(m) & r=float(r)
a=rebin((lindgen(sz(1))-b(0))^2,sz(1),sz(2))+ $
rebin(rotate((lindgen(sz(2))-b(1))^2,1),sz(1),sz(2))
c=a le r^2 & a=c*m
return,z=long(total(a))
; return,z=fix(a)  ; activate this line to get the matrix core
end
