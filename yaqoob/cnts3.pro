function cnts3,m,r,b
; ; Net counts and error within 'r' pxl rad off given loc in mtrx 'm'
; Bgnd taken from great circle about ctr of view through source region
sz=size(m) & b=float(b) & r1=sqrt(total(([256,256]-b)^2))-1. & r2=r1+2.
a1=rebin((lindgen(sz(1))-b(0))^2,sz(1),sz(2))+ $
rebin(rotate((lindgen(sz(2))-b(1))^2,1),sz(1),sz(2))
a2=rebin((lindgen(sz(1))-256.)^2,sz(1),sz(2))+ $
rebin(rotate((lindgen(sz(2))-256.)^2,1),sz(1),sz(2))
c1=a1 le r^2 & as=c1*m & c2=(a2 ge r1^2) and (a2 lt r2^2)
c2=(c2-c1) eq 1 & ab=c2*m & c2=total(c2) & c1=total(c1) & db=long(total(ab))
ds=long(total(as)) & ns=ds-db*c1/c2 & db=sqrt(ds+db*(c1/c2)^2)
return,z=[ns,db]
end

 



