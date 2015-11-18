pro cl_para,bet,core,zz,lum,soc=soc,cf=cf,Ho=Ho,qo=qo,ctoe=ctoe,soe=soe $
,block=block,ro=ro,no=no,verb=verb,mpcam=mpcam,donly=donly,ratio=ratio $
,mradius=mradius,tt=tt,dl=dl
;+
; bet - beta value (or para(0) from the fit (mle_cl_main).
; core - core size (in arcsec).
; zz - the redshift of the cluster
; soc - the central count intensity (in units of counts/s pixel)
; 	Alternatively, 'so' may be estimated by providing the luminosity 
;	(10^44 ergs/s cm^2) of the cluster!
; ctoe - the count intensity to energy flux conversion 
;		(10^-12 ergs/(s cm^2) /cts/s)
; cf - the cooling function (actually n_p/n_e \Lambda) which can be obtained
;	in XSPEC, i.e., the ratio of flux/K, where K is the norm and the
;	flux (10^{-12} ergs/s cm^2) is in the band same as that used for 
;	obtaining so or lum.
; Ho, qo - Hubble constant (km s-1/Mpc) and the decelleration parameter.
; donly - only distance parameters are calculated
; ratio - the ratio of the mean surface brightness to the surface brightness 
;	at the petrosian radius
; mradius - the radius to calculate the gas mass (arcmin)
; tt - the temperature of the ICM for calculating the virial mass
; dl - luminosity distance of the target in units of Mpc.
;*example:
;cl_para,para(0),para(1),0.222,soc=3.3/1.3e4,ctoe=5.8/0.1,cf=5.8/3.4e-3
;lum =       30.2479 in units of 10^44 ergs/s cm^2
;Da(Mpc),     rc(arcmin),    ro(Mpc),    soe(e-12 ergs/s cm arcmin^2), no(cm^-3)
;      936.009     0.676355     0.184154      2.12012   0.00481829
;
; written by wqd, 7/16/96
;-
if n_params() lt 1 then begin
 print,'CALLING SEQUENCE - cl_para,bet,core,zz,lum,soc=soc,cf=cf,Ho=Ho,qo=qo'
print,',ctoe=ctoe,soe=soe,block=block,ro=ro,no=no,verb=verb,mpcam=mpcam'
print,',donly=donly,ratio=ratio,mradius=mradius,tt=tt,dl=dl'
 return
endif
if n_elements(verb) eq 0 then verb=1
if n_elements(Ho) eq 0 then Ho=70
if n_elements(qo) eq 0  then qo=0.1
if n_elements(block) eq 0 then block=10
parc=3.0856 ;the distance of a parcecs in units of 10^18 cm
if n_elements(Dl) eq 0 then $
  Dl=2.9979e5*(qo*zz+(qo-1)*(sqrt(1.+2*qo*zz)-1))/(qo^2*Ho) ;in Mpc
Da=Dl/(1.+zz)^2                 ;in Mpc
amtosa=!pi/(60.*180.) 
mpcam=1./(Da*amtosa)
if keyword_set(donly) ne 0 then begin
 print,'Da, DL, 4*pi*DL^2 = ',da, Dl,' Mpc',4*!pi*(parc*DL*1.e-3)^2,'e54 cm^2'
 print,'1 Mpc = ',mpcam,' arcmin'
 return
endif

rc=core/60. ;in arcmin
ro=rc*amtosa*Da ;in Mpc

if n_elements(soc) eq 0 then begin
	if n_elements(lum) eq 0 then begin
		print,'Either soe or lum is required.'
		return
	endif else $
		soe=1.e8*lum/(4*!pi*(Dl*parc)^2)*(3.*bet-1.5)/(!pi*rc^2)
       	;lum in units of 10^44 ergs/s cm^2 
	;soe of 10^-12 ergs/s cm^2 arcmin^2
endif else begin
	if n_elements(ctoe) eq 0 then begin
		print,'ctoe is needed!'
		return
	endif
	soe=soc*ctoe*(60./float(block)/!size_pixel)^2 
		;10^-12 ergs/s cm^2 arcmin^2
	lum=soe*(4*!pi*(Dl*parc)^2)/1.e8*(!pi*rc^2)/(3.*bet-1.5)
	if verb ne 0 then begin
            print,'lum = ', lum,' in units of 10^44 ergs/s cm^2'
            print,'lum can be very sensitive to the bet value, i.e. depending'
            print,'on flux far from the core region.'
        endif 
endelse
no=(1./(1.e10*parc)) $ ; 
 *(1.+zz)^4*soe*4/(amtosa^2*cf*ro)*gamma(3*bet)/gamma(3*bet-0.5)
no=sqrt(no)

if verb ne 0 then begin
print,'Da(Mpc),     rc(arcmin),    ro(Mpc),    soe(e-12 ergs/s cm arcmin^2), no(cm^-3)'
print,Da, rc, ro, soe, no
endif
if n_elements(ro) gt 1 then begin
	print,'ro and no and their 90\% limits:'
	avg_median,ro,sig=0.05,/pri
	avg_median,no,sig=0.05,/pri
endif

if n_elements(mradius) ne 0 then begin
	uu=(mradius/rc)^2
	uu=uu/(1.+uu)
	mg=2.27e3*no*ro^3*ibeta_real(1.5,1.5*(bet-1.),uu)
	print,'mg and its 90\% limits (in units of 10^14 solar mass):'
	if n_elements(mg) gt 1 then avg_median,mg,sig=0.05,/pri else $
          print,mg
endif
if n_elements(tt) ne 0 then begin
	print,'m_v (in units of 10^14 solar mass):'
	mv=1.15*bet*tt*ro*uu*(mradius/rc)
	avg_median,mv,sig=0.05,/pri
endif

if n_elements(ratio) ne 0 then begin
	petrosian,ro,bet,rp,ratio=ratio
	print,'Petrosian radius = '
	if n_elements(rp) gt 1 then avg_median,rp,sig=0.05,/pri else $
		print,rp
endif
return
end
