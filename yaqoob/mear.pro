function mear,e,m,oc
; Compute effect area at 'e' of mirror with matrix 'm'
; 'oc' are the optical constant data characteristic of surface.
; set 'e'= 0 for geometric area.
e=float(e) & se=size(e)  & rw=10.16 & ri=0.8  ; refl width and ineff fctr
th=m(4,*) & r=m(1,*) & d=sin(th*0.017453)
; expression below computes reflector support transmission
h=.2179+.12447*r-.011737*r^2+5.3677e-4*r^3-9.54223e-6*r^4
d=d*r*h & b=d & if se(0) eq 0 then begin & if e eq 0 then goto, mr1
b=(xrrc(th,e,oc,0.87))^2*d*ri & endif else begin
for i=0,se(1)-1 do begin
c=(xrrc(th,e(i),oc,0.87))^2*d*ri & b=[b,c] & endfor
b=b(1:se(1),*) & endelse
mr1: return,z=total(b,2)*2.*!pi*rw
end    

