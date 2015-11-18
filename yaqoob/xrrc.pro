function xrrc,ga,e,oc,dc
; Comp X-ray refl coeff at phot enrgy 'e' incid at graz angl 'ga'(deg)
; 'oc' is a table of optical constants for the reflecting surface.
; 'dc' is a density correction (fractional density)
; 'ga' and 'e' may be constants or vectors.
; If both are vectors, they must have same dimension.
th=ga*0.0174533 & n=dc*interp2(e,oc) & n0=n(0,*) & n1=n(1,*)
if (size(n))(0) eq 1 then begin & n0=n0(0) & n1=n1(0) & endif
x=th^2-2.*n0 & r=sqrt(x^2+(2.*n1)^2)
a=.70711*sqrt(r+x) & b=.70711*sqrt(r-x)
z=((th-a)^2+b^2)/((th+a)^2+b^2)
if (size(th))(0) eq 0 and (size(e))(0) gt 0 then z=rotate(z,3)
return,z
end
