pro fit_acf_m,tail,device=device,acf_p=acf_p,block=block,startbin=startbin,endbin=endbin,gauss=gauss,ncomp=ncomp,acffile=acffile,iacffile=iacffile,area=area
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - fit_acf_m,tail,device=device,acf_p=acf_p,block=block'
print,'startbin=startbin,endbin=endbin,gauss=gauss,ncomp=,acffile='
return
endif
;
if n_elements(block) eq 0 then block=!block
if n_elements(device) eq 0 then device='x'
if n_elements(startbin) eq 0 then startbin=0.
if n_elements(area) eq 0 then area=914. ;14623 bins
set_plot,device
;
tail=strtrim(tail)
print,'get the ACF of the data file'
if n_elements(acffile) eq 0 then acffile=!data_dir+!seq_no+'_acf'+tail+'.dat'
read_acf,acffile,angle_d,acf_d,acferr_d,fluxm_d,fluxerr_d,nbin
angle_d=angle_d*(block/120.) ; in units of arcmin
xrange=[0.,max(angle_d)*1.1]
yrange=[min(acf_d-acferr_d),max(acf_d+acferr_d)*1.1]
plot,angle_d,acf_d,Ytitle='ACF',Xtitle='Angle (arcmin)' $
,yrange=yrange $ ;[-0.1,0.6] $ 
,xrange=xrange,psym=4,thick=2.
errplot,angle_d,acf_d-acferr_d,acf_d+acferr_d
;
print,'get the ACF of the exposure map'
if n_elements(iacffile) eq 0 then $
	iacffile=!data_dir+!seq_no+'_acfi'+tail+'.dat' 
read_acf,iacffile,angle_i,acf_i
;
print,'get the ACF of the PRF'
if n_elements(acf_p) eq 0 then begin
file_prf='~/rosat/analysis/cor/'+'prf_acf'+strtrim(tail)+'.dat'
read_acf,file_prf,angle_p,acf_p
endif
;
if n_elements(endbin) eq 0 then endbin=n_elements(angle_d) < n_elements(angle_i)
angle_d=angle_d(0:endbin-1)
nd_p=n_elements(acf_p) 
print,'the number of observations included are ', endbin
;
; prf acf beyond a distance is set equal to zero
if nd_p lt endbin then begin
	buf=acf_p
	acf_p=fltarr(endbin)
	acf_p(startbin:nd_p-1)=buf
endif
;
if n_elements(ncomp) eq 0 then ncomp=2 ;total number of component
comp=ncomp-1
print,'The number of components in the fits is ',ncomp
func=fltarr(endbin,ncomp)

if ncomp eq 2 then func(*,comp)=acf_i(startbin:endbin-1) 
if ncomp ne 0 or keyword_set(guass) ne 0 then oplot,angle_d,acf_i,line=1
comps=acf_i(startbin:endbin-1) ;the component is going to be subtracted


if keyword_set(gauss) eq 0 and ncomp lt 1 then  comps=comps*0. ;no actual subtraction

; component 1 is always the prf
func(*,0)=(1.+acf_i(startbin:endbin-1))*acf_p(startbin:endbin-1)
w=1./(acferr_d*acferr_d)
ndf=(n_elements(angle_d)-ncomp)
;

if keyword_set(gauss) ne 0 then begin
;--------------------------

; Gaussian component
ndf=ndf-1 ;due to the additional parameter of the dispersion
;A=fltarr(6)
print,'Give the interval of the Gaussian sigma and number of division'
read,smin,smax,ninter
;
if !debug eq 1 then stop
chimin=1.e10
;A(0)=1.
;A(1)=0.
disp=smin+(smax-smin)/float(ninter-1)*findgen(ninter)

; Now calculate the chi square gride
for k =0,(ninter-1) do begin
;	A(2)=disp(k)
	file=['cl5','cl8','cl10','cl20','cl40']
	cal_angle=[0.5,0.8,1.,2.,4.]
	dir='~/rosat/analysis/cor/'
	prf_acfoff,disp(k),tail,area,aoff,clacf,dir=dir,file=file $
		,cal_angle=cal_angle
	func(*,comp)=clacf
	coef=funcfitw(angle_d,acf_d,w,func,ncomp,yfit,yband,sigma,var,comp_sub=comps)
	print,'disp, chi, ndf = ',disp(k),sigma*ndf,ndf,coef(0),coef(1)
	if chimin gt sigma then begin 
		chimin=sigma & chik=k
	endif
endfor
print,'disp = ', disp(chik)
; 
; refit the data with the parameter of minimum chi square
;A(2)=disp(chik)
;gauss_funct,angle_d,a,f
prf_acfoff,disp(chik),tail,area,aoff,clacf,dir=dir,file=file $
	,cal_angle=cal_angle
func(*,comp)=clacf
endif
;
if !debug eq 1 then stop
coef=funcfitw(angle_d,acf_d,w,func,ncomp,yfit,yband,sigma,var,comp_sub=comps)
;
for k=0,(ncomp-1) do begin
	print,'coef = ', coef(k),' +- ',sqrt(var(k,k))
	oplot,angle_d,coef(k)*func(*,k),line=k+2
endfor
print,'var = ',var
print,'chi, ndf = ',sigma*ndf,ndf
print,'fluxm_d,fluxerr_d,nbin = ',fluxm_d,fluxerr_d,nbin
oplot,angle_d,yfit
;
if !debug eq 1 then stop
;
end