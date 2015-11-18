;=========================================================
; Main program: streak_removal (see memo_dsteak)
;
; commands here can be run by copying and pasting as well and the variables
; are seen in the main IDL session
;
; roll - observation roll angle adjusted (adding 90) to
;	make the streak vertical in the ccd image
;
;*Key outputs:
; ccdims - selected ccdim for calculating the 1-d flux distribution
; ccdbim - output background ccd image containing the streak flux
; bmap - the background in the sky coordinates
;
; written by wqd, Jan, 1, 2003
; fixed the rotation angle, wqd, July 5, 2004
;=========================================================
if n_elements(factor) eq 0 then factor=1. ;source radius factor
x=ls.x 
y=ls.y

;get a rough center location of the chip (which should not affect the
;result)
xm=fix(avg(minmax(x))) ;in the fortran format
ym=fix(avg(minmax(y)))
print,xm,ym   

;rotate the counts and construct the ccdim
;rot_xy,x,y,(roll+90),block=1,xpref=xm,ypref=ym
case !instr of
'aciss': rangle=roll-360
'epic': rangle=90.-roll
else : rangle=roll+90. ;e.g., acisi
endcase
rot_xy,x,y,rangle,block=1,xpref=xm,ypref=ym

minx=min(x) & maxx=max(x)
if !instr ne 'epic' then dy=16 ; to crudely account for chandra dithering at the edges
miny=min(y)+dy & maxy=max(y)-dy
;make sure that the image has an even pixel dimesion of integer block
block=long(block)
dimx=long(maxx-minx+1)/(2*block)*(2*block) 
dimy=long(maxy-miny+1)/(2*block)*(2*block) 
x1=0 & x2=dimx/block-1 & y1=0 & y2=dimy/block-1
imdimx=dimx/block & imdimy=dimy/block
window,xsize=imdimx,ysize=imdimy

list_image,0,xm-dimx/2,ym-dimy/2,ccdim,imdimx,imdimy,block=block,xp=x,yp=y $
           ,emin=bmin(blo),emax=bmax(bhi)

if n_elements(tbw) eq 0 then tbw=!tbw
sz=size(ta)
tim=fltarr(sz(1),sz(2),bhi-blo+1)
if (bhi-blo) gt 0 then begin
    for k=0, bhi-blo do tim(*,*,k)=ta(*,*,k)*tbw(k)
    tim=total(tim,3)/total(tbw(blo:bhi)) 
endif else   tim=ta(*,*,blo)
;reference pixel is at the center of the image
tv,bscale(ccdim,0,3)
;cast the exposure map into the ccd frame
;trans_loct,xm-!pref+0.5,ym-!pref+0.5,fra,fdec,nfra,nfdec,/deg,pixsize=!size_pixel
trans_loct,xm-!pref+0.5,ym-!pref+0.5,cra,cdec,nfra,nfdec,/deg,pixsize=!size_pixel
get_fitshead,ccdim,ch,imhdr,crval=[nfra,nfdec],del=!size_pixel*block/3600. 
;cast,expfname,ch,outa=tbo,roll=rangle ;,inh=exphdr
cast,'',ch,outa=tbo,roll=rangle,ina=tim,inh=hdr ;,inh=imh
;tbo=tt*0.+1. ;streak is not vignetted ;but image is vignetted and
;needs to be corrected. a slight overestimate effect on the streak intensity
tbo(where(tbo le 0.25*max(tbo)))=0.

;remove point-like sources
;tbs=source_sub_v(tbo,nfra,nfdec,slist.ra,slist.dec,sradius=slist.sradius/(!size_pixel*block),block=block,fac=factor,/deg,roll=rangle)
tbs=source_sub_v(tbo,nfra,nfdec,slist.ra,slist.dec,slist.cntr,block=block,fac=factor,/deg,roll=rangle,perclimit=perclimit)
cs=ccdim
cs(where(tbs le 0.))=0
tv,bscale(cs,0,3)
;----------------------------------------------------------------
if !debug eq 1 then stop,'Check the image, to see if additional region need to be removed here before selecting the region, using sexcl=defroi(imdimx,imdimy) !'
;sexcl=defroi(imdimx,imdimy) 
if n_elements(sexcl) ne 0 then begin
    cs(sexcl)=0 & tbs(sexcl)=0
    tv,bscale(cs,0,3)
endif
;----------------------------------------------------------------

if abs(newpexcl) eq 1  then begin
    print,'selecting the region:'
    pexcl=defroi(dimx/block,dimy/block)
endif else print,'using the existing pexcl. Or newpexcl needs to be reset!'
if newpexcl gt 0 then begin ;exclusion
    cs(pexcl)=0 & tbs(pexcl)=0      
endif else begin ;inclusion
    tmp=cs
    cs=cs*0
    cs(pexcl)=tmp(pexcl)
    tmp=tbs
    tbs=tbs*0
    tbs(pexcl)=tmp(pexcl)
endelse 
newpexcl=2*newpexcl/abs(newpexcl)

;get the adaptive flux distribution along the x-axis
;flux_1d_var,cs,tbs*0.,tbs,dis,flux,eflux,xmin=x1,xmax=x2,ymin=y1,yma=y2,block=block,im=ccdims,cton=fcton,dl,dh,lpam=block
flux_1d_var,cs,tbs*0.,tbs,dis,flux,eflux,xmin=x1,xmax=x2,ymin=y1,yma=y2,block=block,im=ccdims,cton=fcton,dl,dh,lpam=block,ebv=ebv
;dl=fix((dl+0.5)*block) & dh=fix((dh+0.5)*block-1)
dl=fix(dl) & dh=fix((dh)-1)
;dl(1:*)=dl(1:*)+1
;dis=dis*block
;dl=dl*block
;dh=dh*block
stop
ploterr,dis,flux,eflux,psym=3 ;,type=1 ;,hat=0.,errthick=0.5
errplot_y,flux,dl,dh,wid=0,errth=0.5

;exclude a local background to get bins with significant streak contributions
if n_elements(xrange) ne 2 then $
	ss=where(flux-median(flux) gt stonth*eflux,nss) $
 	else ss=where(dl ge xrange(0) and dh le xrange(1),nss)
xexcl=indgen(n_elements(flux))
if nss eq 0 then begin
    print,'Maximu deviation =',max(flux-median(flux))
    stop,'No deviation above stonth = ',stonth
endif
remove,ss,xexcl
mflux=median(flux(xexcl))
nflux=flux-mflux
oplot,dis,dis*0.+mflux

nflux=nflux(ss)
oplot,dis(ss),flux(ss),psym=5 ;selected bins

;place the streak flux into a background image:
ccdbim=tbo*0.
;for k=0,nss-1 do
;ccdbim(dl(ss(k))/block:dh(ss(k))/block,*)=tbo(dl(ss(k))/block:dh(ss(k))/block,*)*(nflux(k)*block^2)
for k=0,nss-1 do ccdbim(dl(ss(k))/block:dh(ss(k))/block,*)=nflux(k)
s=where(ccdbim gt 0. and tbs gt 0.)
ccdbim=ccdbim*(total(tbs(s)*ccdbim(s))/total(ccdbim(s))) 
                                ;normalized to the count rate and
                                ;correct for the overestimte due to
                                ;the exposure correction.
;streak image in units of cts/pixel
print,'total streak flux, mflux = ',total(nflux*(dh(ss)-dl(ss)+1))*dimy, mflux
;cast_r,exphdr,bmap,ch,ccdbim*bnorm,roll=-(roll+90),avg=0

;pixelout=sxpar(exphdr,'cdelt2') 
;pixelin=sxpar(ch,'cdelt2')
;if pixelout lt sqrt(2.)*pixelin then begin
;  cast,'',exphdr,outa=bmap,inh=ch,ina=ccdbim*bnorm,roll=-rangle
;  bmap=bmap*(pixelout/pixelin)^2
;endif else $
;  cast_r,exphdr,bmap,ch,ccdbim*bnorm,roll=-rangle,avg=0
if imblock gt block then $
;  cast_r,hdr,bmap,ch,ccdbim*bnorm,roll=-rangle,avg=1 $
;         else cast,'',hdr,outa=bmap,inh=ch,ina=ccdbim*bnorm,inroll=rangle,/noi
cast_r,hdr,bmap,ch,ccdbim,roll=-rangle,avg=1 $
         else cast,'',hdr,outa=bmap,inh=ch,ina=ccdbim,inroll=rangle,/noi
if nofile eq 0 then writefits,outfname,bmap,hdr else print,'No file output!'
if n_elements(nofile) eq 0 then nofile=0
rfim=(imdiv(ccdim-ccdbim*block^2,tbo))*(1.e3*(60./(!size_pixel*block))^2)
 ;in units of counts per arcmin^2
;cast,expfname,hdr,outa=tt,/noin
cast,'',hdr,outa=tt,ina=tim,inh=hdr,/noin
;bmap=tt*bmap*imblock^2
bmap=bmap*imblock^2
print,'total streak counts = ',total(bmap)
fim=imdiv(im-bmap,tt)*(1.e3*(60./(!size_pixel*imblock))^2)
 ;in units of counts per (10^3 s arcmin^2)
end
