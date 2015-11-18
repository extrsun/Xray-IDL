pro fudge_refl,npts,nang,npts2,spec,hspec,fname1=fname1,fname2=fname2
if n_elements(fname1) eq 0 then begin
 print,'fudge_refl,npts,nang,npts2,spec,hspec,fname1=fname1,fname2=fname2'
 retall
endif
;read in monte carlo data
rdsmonte,npts,nang,spec,fname=fname1
;and hrefl
rdhrefl,npts2,hspec,hname=fname2
eh=hspec(0:npts2-1,0:0,0:0) & hinci=hspec(0:npts2-1,0:0,1:1)
hrefl=hspec(0:npts2-1,1:1,1:1)
em=spec(0:npts-1,0:0,0:0) & sm=spec(0:npts-1,0:0,2:2)
sinci=spec(0:npts-1,0:0,1:1)
;photon index
gamma=alog(hinci(npts2-1)/hinci(0))/alog(eh(0)/eh(npts2-1))
print,' photon index ',gamma
ratio = (hinci(0)/sinci(0))*((eh(0)/em(0))^gamma)
print,' ratio = ',ratio
ymin=min(hinci) & ymin=ymin/100./ratio
ymax=max(hinci)/ratio
window,0
plot,eh,hinci/ratio,xtype=1,ytype=1,yr=[ymin,ymax]
oplot,em,sinci,psym=1
oplot,eh,(hrefl/ratio)
oplot,em,sm,psym=1
fint,eh,(hrefl/ratio),em,newhrefl
ecmp=em & smcmp=sm & shcmp=newhrefl
wgood=where((em  le 110. and em ge 0.1),ngood)
if ngood gt 0 then begin
 ecmp=ecmp(wgood) & smcmp=sm(wgood) & shcmp=newhrefl(wgood)
endif
agn: read,'Enter fudge factors: e0 exp roll(keV) and norm ',e0,fudge,norm
fac=fltarr((size(smcmp))(1))+1.0
gtfftn=where((ecmp gt e0),ngt)
if ngt gt 0 then fac(gtfftn)=exp((sqrt(e0)-sqrt(ecmp(gtfftn)))/fudge)
print,' NGT 15 and FAC '
print,ngt,minmax(fac)
shcmp=norm*fac*newhrefl(wgood)
;chi-square
del=(smcmp-shcmp)*(smcmp-shcmp)/smcmp/smcmp
chisq=total(del)/(size(smcmp))(1)
ferr=total(sqrt(del))/(size(smcmp))(1)
print,' chi-square and average fractional error ',chisq,ferr
window,1
plot,ecmp,smcmp*ecmp,xtype=1,ytype=1,xr=[4,120],psym=1
oplot,ecmp,shcmp*ecmp
window,2
plot,ecmp,sqrt(del),xtype=1 
;if en lt 15. then hrefl2(k)=spec else hrefl2(k)=spec*exp((15.-en)/40.) 
ans=' '
read,'Do you want to try fitting again? ',ans
if ans eq 'y' then goto, agn
return
end
