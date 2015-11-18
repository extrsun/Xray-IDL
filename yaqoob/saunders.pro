pro saunders,c,alp,sig,lgstr,lg60min,lg60max,a,b,h,lden,l60,lxcen,lxwid
if n_params(0) eq 0 then begin
 print,'saunders,c,alp,sig,lgstr,lg60min,lg60max,a,b,h,lden,l60,lxcen,iarg'
 print,'Integrate the 60um luminosity function for Saunders et al'
 print,'1990, MNRAS, 242, 318 '
 print,'**INPUTS** '
 print,'C ALP SIG LGSTR  - parameters from Saunders Table 3' 
 print,'i.e. norm, alpha, sigma, and log Lstar '
 print,'lg60min, lg60max - Lower and Upper log luminosity bounds for '
 print,'Integration '
 print,'A, B - The coefficients in the Lx vs L60 relation : ' 
 print,'i.e. lg Lx = b + a lg L60 . green etal 1992 give b=12.4,a=0.64'
 print,'H = Hubble constant in units of 100 km /s/Mpc'
 print,'**OUTPUT**LDEN 0.5-4.5 local luminosity density in 1e38 erg/s'
 retall
end
;lower and upper integration bounds for X-ray luminosity
lglxl = b + a*lg60min & lglxu = b + a*lg60max
print,'Log X-ray luminosity bounds: ',lglxl,lglxu
;set up X-ray luminosity bins 
npts=200
dellx = (lglxu-lglxl)/float(npts)
lglx1 = lglxl + findgen(npts)*dellx
lglx2 = lglx1 + dellx
lxcen = 10.^(0.5*(lglx1+lglx2)-38.)
lxwid = 10.^(lglx2-38.) - 10.^(lglx1-38.)
l60 = 10.^(((0.5*(lglx1+lglx2)-b)/a)-38.) 
lstar = (10^(lgstr-5.))*3.826/h/h
xl = l60/lstar
norm= c*h*h*h
xxl = alog10(1.+xl) & xxl=xxl*xxl
phix = norm* (xl^(1.-alp))*exp(-0.5*xxl/sig/sig)/a/alog(10.)
iarg = phix*lxwid
lden = total(iarg)
print,'Integrated 0.5-4.5 keV luminosity is [1e38 erg/s]',lden
return
end
 
