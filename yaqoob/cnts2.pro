function cnts2,m,r,b
; Net counts and error within 'r' pxl rad off given loc in mtrx 'm'
; Backgrnd taken from perimeter 2*r from same location
sz=size(m) & r=float(r)
a=rebin((lindgen(sz(1))-b(0))^2,sz(1),sz(2))+ $
rebin(rotate((lindgen(sz(2))-b(1))^2,1),sz(1),sz(2))
c1=a le r^2 & a1=c1*m & c2=a le (2*r)^2 & a2=c2*m
d1=long(total(a1)) & d2=long(total(a2)) & d2=d2-d1
return,z=[(d1-d2/4.),sqrt(d1+d2/16.)]
end
