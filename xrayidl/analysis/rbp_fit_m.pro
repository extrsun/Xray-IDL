pro rbp_fit,xp,yp,image_c,dist,rbp,rbperr,yfit, $
rlimit=rlimit,dim=dim,block=block,chlow=chlow,chhigh=chhigh $
,binrat=binrat,offaxis=offaxis,spfil=spfil,
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -  rbp_fit,xp,yp,image_c,dist,rbp,rbperr,yfit, '
print,'rlimit=rlimit,dim=dim,block=block,chlow=chlow,chhigh=chhigh ,
print,',binrat=binrat,offaxis=offaxis'
retall
endif
;
;xp=256.46
;yp=257.95
if n_elements(offaxis) eq 0 then offaxis=0.
if n_elements(rlimit) eq 0 then rlimit=3.
if n_elements(dim) eq 0 then dim=361
if n_elements(block) eq 0 then block=2
if n_elements(chlow) eq 0 then chlow=12
if n_elements(chhigh) eq 0 then chhigh=29
if n_elements(binrat) eq 0 then binrat=1
;
emin=!group(chlow,0) 
emax=!group(chhigh,1)
print,'emin, emax = ', emin, emax
;
print,'get count image ...'
;
make_image,image_c,dim=dim,xc=xp,yc=yp,emin=emin,emax=emax $
,block=block,tmin=0
;
tv,bscale(image_c)
print,'to get the exact position of the source'
get_center,image_c,sxp,syp,radius=0.5,block=block,xc=xp,yc=yp,/psel
;
print,'the source pixel position is ',sxp,syp
print,'center the image on the source'
hdim=(dim-1.)*0.5
xp=xp+(sxp-hdim)*block/30.
yp=yp+(syp-hdim)*block/30.
make_image,image_c,dim=dim,xc=sxp,yc=syp,emin=emin,emax=emax $
,block=block,tmin=0
;
print,'get radial brigtness profile around the center...'
;
rbp,image_c,dist,rbp,rbperr,block=block,radiushi=rlimit,/plotoff
dir='/home/casa/wqd/rosat/analysis/spectral/'
filen='~/rosat/analysis/spectral/po17.dat'
;
if !proc eq 'PSPC' instr='P' else instr='H'
if n_elements(spfil) eq 0 then begin 
print,' get model spectral rate and group...'
;
modelrate,filen,chlow,chhigh,rate,group,dir=dir
spfil='NONE'
endif
inputs='instr='+instr+',spfil='+spfil+',sptyp=PHA,imgfil=NONE,proftab=NONE'+ $
	',block='+strtrim(block,2)+',binsiz='+strtrim(block*0.5)+',binrat='+ $
	strtrim(binrat,2)+',emin='+strtrim(emin,2)+',emax='+strtrim(emax,2) + $
	',pixel=BL'+',nbins='+strtrim(dim,2)

make_psf,inputs,offang,prof,psfim

;
print,'get point spread function ...'
binsize=0.5*block
calcpsfp,dim,binsize,offaxis,rate,offang,prof,psfim,group=group,binrat=binrat
;
if !debug eq 1 then stop,'before rbp'
rbp,psfim,dist,psf,block=block,radiushi=rlimit,/plotoff
;
; now do the fit
;
npoint=n_elements(dist)
func=fltarr(npoint,2)
func(*,0)=1.
func(*,1)=psf
coef=funcfitw(dist,rbp,(1./rbperr^2),func,2,yfit,yband,sigma,inv)
;
; plot
;
erase
rbp_plot,dist,rbp,rbperr,yfit
; print out parameters
;
print,'chi square = ',sigma,' with No. of degree freedom = ',npoint-2
print,'coefficient of the functions:'
for k=0,1 do begin
print,coef(k), ' +/- ',sqrt(inv(k,k))
endfor
end