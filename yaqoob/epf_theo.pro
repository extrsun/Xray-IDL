pro epf_theo,mu,nt,x1,y1,tc1,rt1,x2,y2,tc2,rt2,f1,f2,f3,f4
if n_params(0) eq 0 then begin
 print,'epf_theo,mu,nt,x1,y1,tc1,rt1,x2,y2,tc2,rt2,f1,f2,f3,f4'
 print,'epf_theo,mu,dx,dy,nx,ny,y,yy,x0,fy,intgnd,qdpname=qdpname'
 print,'numerical computation of epf in the Guassian limit'
 retall
end
safe=0.98
;cf2=3.+2.*sqrt(2.)
;cf1=2.
cf1=1.
cf2=4.
t1=-atan((cf1-mu)/(cf2-mu))+1.5*!pi
;T2=atan(1.-(1./sqrt(1.+2.*mu)))
t2=t1+!pi
t3=!pi/4.
t4=-atan((cf2-mu)/(cf1-mu))+1.5*!pi
Print,'Theta lower integration limits /pi: ',t1/!pi, t3/!pi
Print,'Theta upper integration limits /pi: ',t2/!pi, t4/!pi
t1=safe*t1 
t2=safe*t2 
t3=safe*t3 
t4=safe*t4 
Dt1=(t2-t1)/float(nt-1)
Dt2=(t4-t3)/float(nt-1)
Tc1=t1+findgen(nt)*dt1
Tc2=t3+findgen(nt)*dt2
ct1=cos(tc1)
ct2=cos(tc2)
st1=sin(tc1)
st2=sin(tc2)
mfac=sqrt(2./mu)
f4=ct1-st1
f3=st1/f4
g4=ct2-st2
g3=st2/g4
f1=1.+mfac*f3
g1=1.+mfac*g3
f2=sqrt(1.+f1*f1)
g2=sqrt(1.+g1*g1)
rt1=(f1+f2)/f4
rt2=(g1-g2)/g4
x1=rt1*ct1 & y1=rt1*st1
x2=rt2*ct2 & y2=rt2*st2
intgnd1=0.5/(exp(rt1*rt1))
intgnd2=0.5/(exp(rt2*rt2))
;simpson's rule
intgnd1(nt-1l)=0.
intgnd2(nt-1l)=0.
ys1=0.5*dt1*(intgnd1(0)+intgnd1(nt-1l)+2.*total(intgnd1(1:nt-2l)))
ys2=0.5*dt2*(intgnd2(0)+intgnd2(nt-1l)+2.*total(intgnd2(1:nt-2l)))
print,'integrals (lower upper and total = ',ys1,ys2,ys1+ys2
print,'Theoretical limit for infinite MU :'
del=(!pi)/float(nt-1l)
u=findgen(nt)*del
v=exp(-2.91421/cos(u)/cos(u))+exp(-0.0857864/cos(u)/cos(u))
v(nt-1l)=0.
tv=0.25*del*(v(0)+v(nt-1l)+2.*total(v(1:nt-2l)))
quadmu,xmu,wmu
vv=exp(-2.91421/cos(xmu)/cos(xmu))+exp(-0.0857864/cos(xmu)/cos(xmu))
print,'Simson Rule'
print,tv
print,'Gauss quad'
print,total(vv*wmu)/2.
return
end
