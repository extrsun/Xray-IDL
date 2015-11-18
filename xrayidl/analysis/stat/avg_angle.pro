pro avg_angle,cc,tt,bb,cpx=cpx,cpy=cpy,anglem=anglem,dl=dl,dh=dh
if n_params() eq 0 then begin
print,'CALLING Sequnece - avg_angle,cc,tt,bb,cpx=cpx,cpy=cpy,anglem=anglem,dl=dl,dh=dh'
print,'anglem starts from the north and anticlockwise."
return
endif
sz=size(cc)
if n_elements(dl) eq 0. then dl=0.
if n_elements(dh) eq 0. then dh=min(sz(1),sz(2))*0.5
if n_elements(bb) ne 0 then ff=imdiv((cc-bb),tt) else ff=imdiv(cc,tt)
get_angle,dis,angle,sz(1),sz(2),cpx=cpx,cpy=cpy
angle=angle*(!pi/180.)
s=where(tt gt 0. and dis ge dl and dis le dh,nsel) 
if nsel eq 0 then stop,'no bins with exposure not equal to 0'
ca=total(ff(s)*cos(angle(s)))
sa=total(ff(s)*sin(angle(s)))
rr=sqrt(ca^2+sa^2 > 1.e-20)
ca=ca/rr
sa=sa/rr
anglem=asin(sa)*(180./!pi)
if ca lt 0. then anglem=180.-anglem
if sa lt 0. and ca ge 0. then anglem=360+anglem
print,'anglem = ',anglem
stop
return
end