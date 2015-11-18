function eef1,m,r
; Total counts within 'r' pxl rad off peak count in mtrx 'm'
sz=size(m) & b=maxloc(m) & r=float(r)
a=rebin((lindgen(sz(1))-b(0))^2,sz(1),sz(2))+ $
rebin(rotate((lindgen(sz(2))-b(1))^2,1),sz(1),sz(2))
c=a le r^2 & a=c*m
return,z=long(total(a))
; return,z=fix(a)  ; activate this line to get the matrix core
end
