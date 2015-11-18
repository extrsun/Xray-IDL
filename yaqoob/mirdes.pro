function mirdes,r1,r2,fl,rw,rt,vg
; This function generates foil mirror design parameters;  'r1' and 'r2' 
; are the innermost and outermost reflector radii in cm, 'fl' is the focal
; length in M, 'rw' and 'rt' are reflector width and thickness in cm;
; 'vg' is the gap between the two cone arrays.
; The output is a five column array consisting of the top and bottom 
; radii of primary and secondary reflectors and of the grazing angle 
; for each primary reflector in degrees.
rr1=fltarr(500) & rr2=rr1 & th=rr1 & f=vg/2.+4.*(fl*100.-(rw+vg)/2.)
th(0)=atan(r1/f) & rr1(0)=r1 & rr2(0)=r1+rw*sin(th(0))
i=0. & repeat begin & i=i+1 & rr1(i)=rr2(i-1)+rt & th(i)=atan(rr1(i)/f) 
rr2(i)=rr1(i)+rw*sin(th(i)) & endrep until rr2(i) ge r2 
br2=rr1-vg*tan(2.*th) & br1=br2-rw*sin(3.*th) & th=th*180./!pi
return,z=[rotate(rr2(0:i-1),1),rotate(rr1(0:i-1),1), $
rotate(br2(0:i-1),1),rotate(br1(0:i-1),1),rotate(th(0:i-1),1)]
end

