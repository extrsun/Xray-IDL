pro eefnewbgd,xrteef,r0,rnorm,ctsnorm,normeef,bgd,neweef
if n_params(0) eq 0 then begin
 print,'eefnewbgd,xrteef,r0,rnorm,ctsnorm,normeef,bgd,neweef'
 retall
end
eef=xrteef
for k=0,3 do eef(*,k)=xrteef(*,k)/normeef(k)
neweef=eef
pi=3.14159265
npts=(size(xrteef))(1)
for k=0,3 do begin
 f1=pi*rnorm*rnorm*bgd/ctsnorm
 f2=pi*r0*r0*bgd/ctsnorm
 neweef(*,k)=eef(*,k)*(1.-f1)+ f2
endfor
return
end
