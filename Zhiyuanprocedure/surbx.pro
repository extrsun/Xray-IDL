instr='aciss'
cenvset,instr

maindir='../'
evtroot='evt2file_new_clean'
block=1
pixsize=!size_pixel*block/60 ;in units of arcmin
tran=60.0/block/!size_pixel
trans=tran^2.0
ccdid=[7]
gradec=double([223.489988,3.544458])

ca=readfits('n5775_c.fits',mh)
ea=readfits('n5775_e.fits')
ba=readfits('n5775_b.fits')
;bsiga=readfits('n5775_bsig.fits')
bsiga=ba^2
taw=1./!tanorm
tbw=[0.401588,0.292407,0.188253,0.117751]

cut=1 ;for filtering the disk region
sz = size(ca)

pa=-55. ;position angle of the positive x-axis (major axis)
hsizep=2.; in unit of arcmin along positive x direction
hsizen=2.5; in unit of arcmin along negative x direction
width=1.5; in unit of arcmin along y direction
;width=0.6
;wcut=100./60*cos(86./180*!pi)
wcut=12./60

xp=(sz[1]-1)/2.
yp=(sz[2]-2)/2.

xmin=xp-hsizen*tran
xmax=xp+hsizep*tran
ymin=yp-width*tran
ymax=yp+width*tran
doff=xp/tran

loc2=lindgen(sz[1]*sz[2])
y = loc2/sz[1]
x=loc2 mod sz[2]
rot_xy,x,y,pa,xshift=xshift,yshift=yshift,block=block,xpref=xp*block,ypref=yp*block,/xyreal

nbins=long((hsizep+hsizen)/0.2)
vbins=(xmax-xmin)/(nbins)*findgen(nbins+1)+xmin

stonth4=[3,3,2,2]
;ctonth4=[12,10,10,12]
ctonth4=[0,0,0,0]
;fb=[0.001163,0.000475274,9.00610e-05,5.20055e-05]
fb=[0.001213,0.000504,9.00610e-05,5.20055e-05] ;sky background in units of count/s/arcmin^2
efb=fb*0.3

band=0
ctonth=ctonth4(band)
stonth=stonth4(band)
e_image=ea(*,*,band)*0.
vdist= findgen(nbins)*0.0
vcount= findgen(nbins)*0.0
vexpt= findgen(nbins)*0.0
vback= findgen(nbins)*0.0
vsig2= findgen(nbins)*0.0
vnorm= findgen(nbins)*0.0
vsback= findgen(nbins)*0.0
n_pix=findgen(nbins)*0.0

c_image=total(ca(*,*,0:1),3)
e_image=(ea(*,*,0)*taw(0)*tbw(0)+ea(*,*,1)*taw(1)*tbw(1))/(taw(0)*tbw(0)+taw(1)*tbw(1))
b_image=total(ba(*,*,0:1),3)
bsig_image=total(bsiga(*,*,0:1),3)
fcon=(fb(0)*taw(0)+fb(1)*taw(1))/(taw(0)*tbw(0)+taw(1)*tbw(1))*(tbw(0)+tbw(1))
efcon=(efb(0)*taw(0)+efb(1)*taw(1))/(taw(0)*tbw(0)+taw(1)*tbw(1))*(tbw(0)+tbw(1))

;c_image=total(ca(*,*,0:2),3)
;e_image=(ea(*,*,0)*taw(0)*tbw(0)+ea(*,*,1)*taw(1)*tbw(1)+ea(*,*,2)*taw(2)*tbw(2))/(taw(0)*tbw(0)+taw(1)*tbw(1)+taw(2)*tbw(2))
;b_image=total(ba(*,*,0:2),3)
;bsig_image=total(bsiga(*,*,0:2),3)
;fcon=(fb(0)*taw(0)+fb(1)*taw(1)+fb(2)*taw(2))/(taw(0)*tbw(0)+taw(1)*tbw(1)+taw(2)*tbw(2))*(tbw(0)+tbw(1)+tbw(2))
;efcon=(efb(0)*taw(0)+efb(1)*taw(1)+efb(2)*taw(2))/(taw(0)*tbw(0)+taw(1)*tbw(1)+taw(2)*tbw(2))*(tbw(0)+tbw(1)+tbw(2))

;bsig_image=bsiga(*,*,band)
;c_image=ca(*,*,band)
;b_image=ba(*,*,band)
;e_image=ea(*,*,band)
;fcon=fb(band)
;efcon=efb(band)

;;;;;;;;;;;;;;;
;;;;;The following calculates vertical intensity profiles according to a given S/N ratio.
;;;;;;;;;;;;;;;

for k=0, nbins-1 do begin
    pix=where(e_image gt 0 and y ge ymin and y lt ymax and x ge vbins(k) and x lt vbins(k+1),npix)
    if cut eq 1 then pix=where(e_image gt 0 and y ge ymin and y lt ymax and abs(y-yp) gt wcut*tran and x ge vbins(k) and x lt vbins(k+1),npix)
    n_pix(k)=npix
    vdist(k)=(vbins[k]+vbins[k+1])*0.5
    vcount(k)=float(total(c_image(pix)))
    vexpt(k)=float(total(e_image(pix)))
    vnorm(k)=float(total(bsig_image(pix)))
    vsback(k)=vexpt(k)*fcon/trans
    vback(k)=float(total(b_image(pix)))+vsback(k)
    vsig2(k)=vcount(k)+vnorm(k)+efcon/trans*vexpt(k)

endfor

surb_1d,vdist,vcount,vback,vexpt,flux,eflux,dist,dlow,dhigh,ctonth=ctonth,stonth=stonth,nounit=nounit,block=block,rsig2=vsig2
fn0=(vcount-vback)/vexpt
efn0=sqrt(vsig2)/vexpt

dist=dist-doff
print,[transpose(dist),transpose(flux),transpose(eflux)]
d0=dist
dlow0=dlow-doff
dhigh0=dhigh-doff
f0=flux*1e4
ef0=eflux*1e4

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;The following calculates half-light heights basing on the above vertical bins.
;;;;;;;;;;;;;;;;;;;;;;;;
v0=f0*0.0
vu0=v0*0.0
vd0=v0*0.0
ev0=f0*0.0
fac=0.5 ;fraction of enclosed emission
for k=0, n_elements(d0)-1 do begin
    hflux=0.0
    v=wcut
    while v lt width and hflux*(v-wcut)/(width-wcut) lt fac*(f0(k)-ef0(k))*1e-4 do begin
        pix=where(e_image gt 0 and y ge ymin and y lt ymax and abs(y-yp) gt wcut*tran and abs(y-yp) lt (v+0.1)*tran and (x-xp)/tran ge dlow0(k) and (x-xp)/tran lt dhigh0(k),npix)
        hflux=(total(c_image(pix))-total(b_image(pix)))/total(e_image(pix))*trans-fcon
        v=v+0.1
    endwhile
    vd0(k)=v-0.1
    while v lt width and hflux*(v-wcut)/(width-wcut) lt fac*f0(k)*1e-4 do begin
        pix=where(e_image gt 0 and y ge ymin and y lt ymax and abs(y-yp) gt wcut*tran and abs(y-yp) lt (v+0.1)*tran and (x-xp)/tran ge dlow0(k) and (x-xp)/tran lt dhigh0(k),npix)
        hflux=(total(c_image(pix))-total(b_image(pix)))/total(e_image(pix))*trans-fcon
        v=v+0.1
    endwhile
    v0(k)=v-0.1
    while v lt width and hflux*(v-wcut)/(width-wcut) lt fac*(f0(k)+ef0(k))*1e-4 do begin
        pix=where(e_image gt 0 and y ge ymin and y lt ymax and abs(y-yp) gt wcut*tran and abs(y-yp) lt (v+0.1)*tran and (x-xp)/tran ge dlow0(k) and (x-xp)/tran lt dhigh0(k),npix)
        hflux=(total(c_image(pix))-total(b_image(pix)))/total(e_image(pix))*trans-fcon
        v=v+0.1
    endwhile
    vu0(k)=v-0.1
endfor
;ev0=0.5*(vu0-vd0)
ev0=v0*ef0/f0
print,[transpose(d0),transpose(v0),transpose(ev0)]

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;The following repeats the above steps but for a different band
;;;;;;;;;;;;;;;;;;;;;;;;;
band=1
ctonth=ctonth4(band)
stonth=stonth4(band)
vdist= findgen(nbins)*0.0
vcount= findgen(nbins)*0.0
vexpt= findgen(nbins)*0.0
vback= findgen(nbins)*0.0
vsig2= findgen(nbins)*0.0
vnorm= findgen(nbins)*0.0
vsback= findgen(nbins)*0.0
n_pix=findgen(nbins)*0.0
bsig_image=bsiga(*,*,band)
c_image=ca(*,*,band)
b_image=ba(*,*,band)
e_image=ea(*,*,band)
fcon=fb(band)
efcon=efb(band)

for k=0, nbins-1 do begin
    pix=where(e_image gt 0 and y ge yp and y lt ymax and x ge vbins(k) and x lt vbins(k+1),npix)
    if cut eq 1 then pix=where(e_image gt 0 and y ge ymin and y lt ymax and abs(y-yp) gt wcut*tran and x ge vbins(k) and x lt vbins(k+1),npix)
    n_pix(k)=npix
    vdist(k)=(vbins[k]+vbins[k+1])*0.5
    vcount(k)=float(total(c_image(pix)))
    vexpt(k)=float(total(e_image(pix)))
    vnorm(k)=float(total(bsig_image(pix)))
    vsback(k)=vexpt(k)*fcon/trans
    vback(k)=float(total(b_image(pix)))+vsback(k)
    vsig2(k)=vcount(k)+vnorm(k)+efcon/trans*vexpt(k)
endfor

surb_1d,vdist,vcount,vback,vexpt,flux,eflux,dist,dlow,dhigh,ctonth=ctonth,stonth=stonth,nounit=nounit,block=block,rsig2=vsig2
fn1=(vcount-vback)/vexpt
efn1=sqrt(vsig2)/vexpt

dist=dist-doff
print,[transpose(dist),transpose(flux),transpose(eflux)]
d1=dist
dlow1=dlow-doff
dhigh1=dhigh-doff
f1=flux*1e4
ef1=eflux*1e4

v1=f1*0.0
vu1=v1*0.0
vd1=v1*0.0
ev1=f1*0.0

for k=0, n_elements(d1)-1 do begin
    hflux=0.0
    v=wcut
    while v lt width and hflux*(v-wcut)/(width-wcut) lt fac*(f1(k)-ef1(k))*1e-4 do begin
        pix=where(e_image gt 0 and y ge ymin and y lt ymax and abs(y-yp) ge wcut*tran and abs(y-yp) lt (v+0.1)*tran and (x-xp)/tran ge dlow1(k) and (x-xp)/tran lt dhigh1(k),npix)
        hflux=(total(c_image(pix))-total(b_image(pix)))/total(e_image(pix))*trans-fcon
        v=v+0.1
    endwhile
    vd1(k)=v-0.1
    while v lt width and hflux*(v-wcut)/(width-wcut) lt fac*f1(k)*1e-4 do begin
        pix=where(e_image gt 0 and y ge ymin and y lt ymax and abs(y-yp) ge wcut*tran and abs(y-yp) lt (v+0.1)*tran and (x-xp)/tran ge dlow1(k) and (x-xp)/tran lt dhigh1(k),npix)
        hflux=(total(c_image(pix))-total(b_image(pix)))/total(e_image(pix))*trans-fcon
        v=v+0.1
    endwhile
    v1(k)=v-0.1
    while v lt width and hflux*(v1(k)-wcut)/(width-wcut) lt fac*(f1(k)+ef1(k))*1e-4 do begin
        pix=where(e_image gt 0 and y ge ymin and y lt ymax and abs(y-yp) ge wcut*tran and abs(y-yp) lt (v+0.1)*tran and (x-xp)/tran ge dlow1(k) and (x-xp)/tran lt dhigh1(k),npix)
        hflux=(total(c_image(pix))-total(b_image(pix)))/total(e_image(pix))*trans-fcon
        v=v+0.1
    endwhile
    vu1(k)=v-0.1
endfor
;ev1=0.5*(vu1-vd1)
ev1=v1*ef1/f1
print,[transpose(d1),transpose(v1),transpose(ev1)]
;omap=readfits('../../mdata/N5775_HA_COLLINS.FITS',ohdr)
;crval=[sxpar(ohdr,'CRVAL1'),sxpar(ohdr,'CRVAL2')]
;crpix=[sxpar(ohdr,'CRPIX1'),sxpar(ohdr,'CRPIX2')]
;pixsize=sxpar(ohdr,'CDELT2')*60 ;pixel size in unit of arcmin
;trans_dist,crval(0),crval(1),gradec(0),gradec(1),xdis,ydis,pixsize=pixsize*60,/degree
;xp=crpix(0)+xdis
;yp=crpix(1)+ydis

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;Bin the Spitzer image in the same way as above
;;;;;;;;;;;;;;;;;;;;;;;;;;
;omap=readfits('../../mdata/N5775_Spitzer_I4.fits',ohdr)
;a=finite(omap)
;pix=where(a ne 1)
;omap(pix)=0
omap=readfits('N5775_Spitzer_I4_cal.fits',ohdr)

;pixsize=1./tran
crota=sxpar(ohdr,'CROTA2')
cast,'',mh,outa=new_omap,ina=omap,inh=ohdr,inroll=crota
omap=new_omap
osz=size(omap)

xmin=xp-hsizen/pixsize
xmax=xp+hsizep/pixsize
ymin=yp-wcut/pixsize
ymax=yp+wcut/pixsize

loc=lindgen(osz(1)*osz(2))
y = loc/long(osz(1))
x=loc mod osz(1)
rot_xy,x,y,pa,xshift=xshift,yshift=yshift,block=1.0,xpref=xp,ypref=yp,/xyreal

obins=n_elements(d0)
odlow=(dlow0+doff)*tran
odhigh=(dhigh0+doff)*tran
odist=d0
oflux= findgen(obins)*0.0

for k=0, obins-1 do begin
    pix=where(omap ne 0. and y ge ymin and y le ymax and x ge odlow(k) and x lt odhigh(k),npix)
    oflux(k)=mean(omap(pix))
    ;print,odist(k),npix
endfor
o0=oflux*0.737 ;aperture correction

a0=total(o0*f0/ef0^2)/total(o0^2/ef0^2)
del0=sqrt(n_elements(f0)*2.706/total(o0^2/ef0^2))

obins=n_elements(d1)
odlow=(dlow1+doff)*tran
odhigh=(dhigh1+doff)*tran
odist=d1
oflux= findgen(obins)*0.0

for k=0, obins-1 do begin
    pix=where(omap ne 0. and y ge ymin and y le ymax and x ge odlow(k) and x lt odhigh(k),npix)
    oflux(k)=mean(omap(pix))
    ;print,odist(k),npix
endfor
o1=oflux*0.737
a1=total(o1*f1/ef1^2)/total(o1^2/ef1^2)
del1=sqrt(n_elements(f1)*2.706/total(o1^2/ef1^2))

;;;;;;;;;;;;;;;;
;;;;Rank and plot the X-ray intensity-IR intensity pairs
;;;;;;;;;;;;;;;;
loadct_self,30
!y.style=1
tvlct,255*[0,1,1,0,0,1,1],255*[0,1,0,1,0,1,1],255*[0,1,0,0,1,0,1]
set_plot,'ps'
device,bits=8,/land,color=1,xsize=16.,ysize=16.,yoff=26,xoff=0,filename='surbx.ps'
!color=0
sh_plot,o0,f0,ef0,f0*0.0-100,xtit='Spitzer 8 micron Intensity (MJy/Str)',ytit='!6X-ray Intensity (10!U-4!N cts s!U-1!N arcmin!U-2!N)',xrange=[0,30],yrange=[0,nint(max(f0+ef0)*1.05/10)*10]
oplot,[0,o0,50],[0,o0,50]*a0

!color=2
oploterr,o1,f1,ef1,4
errplot_x,o1,f1-ef1,f1+ef1
oplot,[0,o1,50],[0,o1,50]*a1

device,/close
set_plot,'x'

;;;;;;;;;;;;;;;
;;;;;;Plot the X-ray half-light height-IR intensity pairs
set_plot,'ps'
device,bits=8,/land,color=1,xsize=16.,ysize=16.,yoff=26,xoff=0,filename='surbh.ps'
!color=0
sh_plot,o0,v0,ev0,f0*0.0-1,xtit='Spitzer 8 micron Intensity (MJy/Str)',ytit='!6X-ray Half-light Height (arcmin)',xrange=[0,30],yrange=[0,width]

!color=2
oploterr,o1,v1,ev1,4
errplot_x,o1,v1-ev1,v1+ev1

device,/close
set_plot,'x'

;;;;;;;;;;;;;;;;
;;;;;Calcuate the X-ray hardness ratio
;;;;;;;;;;;;;;;;
hr_vec_var,fn0,efn0,fn1,efn1,hr,hre,vdist,xhr,vbins(0:nbins-1),vbins(1:*),ston=2.,xhrlo=xhrlo,xhrhi=xhrhi
obins=n_elements(hr)
odlow=xhrlo
odhigh=xhrhi
odist=xhr

;hr=(fn1-fn0)/(fn1+fn0)
;hre=(efn0+efn1)*2*fn1/(fn1+fn0)^2
;obins=n_elements(hr)
;odlow=vbins(0:nbins-1)
;odhigh=vbins(1:*)
;odist=vdist

oflux= findgen(obins)*0.0

for k=0, obins-1 do begin
    pix=where(omap ne 0. and y ge ymin and y le ymax and x ge odlow(k) and x lt odhigh(k),npix)
    oflux(k)=mean(omap(pix))
    ;print,odist(k),npix
endfor
on=oflux*0.737

;;;;;;;;;;;;;;;;;
;;;;;;;;Plot the hardness ratio-IR intensity pairs
;;;;;;;;;;;;;;;;;

set_plot,'ps'
device,bits=8,/land,color=1,xsize=16.,ysize=16.,yoff=26,xoff=0,filename='hardness.ps'
!color=0
sh_plot,on,(hr+1)/(1-hr),hre,hr*0.0-10,xtit='Spitzer 8 micron Intensity (MJy/Str)',ytit='!6X-ray Hardness Ratio',xrange=[0,30],yrange=[0,1.5]

device,/close
set_plot,'x'

end
