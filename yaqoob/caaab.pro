function caaab,a
b=float(a) & z=b(0:1,*) & b=b*!dtor 
z(0,*)=acos(cos(b(0,*))*cos(b(2,*))+sin(b(0,*))*sin(b(2,*))*cos(b(1,*)))
z(0,*)=(z(0,*)>1.e-7) & z(0,*)=(z(0,*)<3.141592)
sc=sin(b(1,*))*sin(b(2,*))/sin(z(0,*)) & sc=(sc>(-1.) ) & sc=(sc<1.)
cc=(cos(b(2,*))-cos(z(0,*))*cos(b(0,*)))/(sin(z(0,*))*sin(b(0,*)))
cc=(cc<1.) & cc=(cc>(-1.))
z(1,*)=(!pi-asin(sc))*((sc ge 0.) and (cc le 0.))
z(1,*)=z(1,*)+(!pi+asin(-sc))*((sc le 0.) and (cc le 0.))
z(1,*)=z(1,*)+(2.*!pi-acos(cc))*((sc le 0.) and (cc ge 0.))
z(1,*)=z(1,*)+acos(cc)*((sc ge 0.) and (cc ge 0.))
return,z*!radeg
end

