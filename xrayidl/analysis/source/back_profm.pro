pro back_profm,rlo,rhi,rdivs,image,dist,flux,fluxlo,fluxhi,nbin, $
block=block,filter=filter,siglevel=siglevel,xp=xp,yp=yp,imb=imb

;---------------------------------------------------------------
if n_params() EQ 0 then begin
print,'CALL SEQUENCE -back_profm,rlo,rhi,rdivs,image,dist,flux,fluxlo,fluxhi'
print,',block=block,filter=filter,siglevel=siglevel'
retall
endif
;---------------------------------------------------------------
if n_elements(block) EQ 0 then block=!block
sz=size(image) 
szmin=(sz(1) < sz(2))
xc=(sz(1)-1)/2.
yc=(sz(2)-1)/2.
if n_elements(xp) ne 0 then xc=xc+xp/float(block) ;off image center 
if n_elements(yp) ne 0 then yc=yc+yp/float(block)

rlov=[0.,rlo+(rhi-rlo)/float(rdivs)*findgen(rdivs)]
rhiv=[rlov(1:*),rhi]
;
dist_circle,circle,szmin,xc,yc
;  good=where((circle ge rlo) and (circle LT rhi) and filter gt 0., n_bin)
good=where((circle LT rhi) and filter gt 0., n_bin)
if n_bin eq 0 then begin
	stop,'No data covered in the radius. Is the radius too small?'
endif

dis=circle(good)
vec_c=float(image(good))

ss=sort(dis)
dis=dis(ss)
vec_c=vec_c(ss)

flux=fltarr(rdivs+1)
fluxlo=flux
fluxhi=flux
dist=flux
nbin=flux
for n = 0,rdivs do begin
	tabinv,dis,rlov(n),indlo
	tabinv,dis,rhiv(n),indhi
	indhi=long(indhi)
	indlo=long(indlo)
	if indlo ne 0 then indlo=indlo+1 < indhi
	avg_median,vec_c(indlo:indhi),f,flo,fhi,siglevel=siglevel
	flux(n)=f
	fluxlo(n)=flo
	fluxhi(n)=fhi
	nbin(n)=n_elements(dis(indlo:indhi))
	dist(n)=total(dis(indlo:indhi))/nbin(n)
  endfor
if !debug eq 1 then stop
imb=image
good=where(circle LT rhi)
linterp,dist,flux,circle(good),fluxgood
imb(good)=fluxgood

ambin=60./(block*!size_pixel) 
sambin=ambin^2*1.e3
  
flux=flux*sambin
fluxlo=fluxlo*sambin
fluxhi=fluxhi*sambin
dist=dist/ambin

print,flux,fluxlo,fluxhi
;
stop
return
end 