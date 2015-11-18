pro intsphesc,ntau,tmin,tmax,tau,escfac,dist,intg
if n_params(0) eq 0 then begin
 print,'intsphesc,ntau,tmin,tmax,tau,escfac,dist,intg'
 print,'Compute the escape probability of photons from a sphere'
 print,'as a function of total optical depth for a given photon '
 print,'distribution in the sphere '
 retall
end
nmu=200 & nx = 100
intg=fltarr(ntau,nx,nmu)
;x = r/R , fractional radial position of photon
;mu = cosine of angle between radial and scattering direction
;set up bins
delmu=(2./float(nmu)) & mul=-1.+delmu*findgen(nmu)      
muh = mul+delmu & mucen=0.5*(mul+muh)
delx = 1./float(nx) & xl=findgen(nx)*delx & xh=xl+delx
x=0.5*(xl+xh)
fr=fltarr(nx)  & escfac=fltarr(ntau)
tml=alog10(tmin) & tmh=alog10(tmax)
deltau=(tmh-tml)/float(ntau-1)
tau=10^(tml+deltau*findgen(ntau))
for i=0l,ntau-1l do begin
 if dist eq 0 then fr=fltarr(nx)+(3./4./!pi)
 if dist eq 1 then fr = tau(i)*exp(-x*tau(i))/4./!pi/x/x/(1.-exp(-tau(i)))
 for j=0l,nx-1 do begin
  d = -x(j)*mucen + sqrt(1.-x(j)*x(j)*(1.-mucen*mucen))
  intg(i,j,0:nmu-1)=exp(-tau(i)*d)*2.*!pi*x(j)*x(j)*delx*fr(j)*delmu
  muint = delmu*total(exp(-tau(i)*d))
  escfac(i) =escfac(i)+(2.*!pi*x(j)*x(j)*delx*fr(j)*muint)
 endfor
endfor
end 

