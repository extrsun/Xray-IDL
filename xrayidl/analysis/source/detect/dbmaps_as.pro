pro im_fill_enlarge,fmo,tb,fac,cbm,frac=frac
;+
; to enlarge a flux image with the empty pixels first filled with 
; approximate values to avoid the artificial reduction of flux values 
; at the edges of the image during the expansion of the image, 
; called by dbmaps_as.pro
;
; fmo - the input flux map
; tb - exposure map used only for locating the unexposed regions
; fac - the images will be enlarged by this factor
; cbm - enlarged flux map
; frac - the lower flux fraction of the pixesl used to estimate the
; 	filling value (def 1/5.)
;
; written by wqd, 4/24/2002
;-
if n_elements(frac) eq 0 then frac=1./5
fm=fmo
ss=where(tb gt 0.,nbin)
fmv=fm(ss)
fmv=fmv(sort(fmv))
ss=where(tb le 0.,nss)
if nss ne 0 then fm(ss)=avg(fmv(0:nbin*frac))
cbm=image_comp(fm,fac)*(1./fac^2)
return
end

pro dbmaps_as,ls,xmin,ymin,block,dim,bmin,bmax,tb,tbs,ta,cbm,cbma,fra=fra,th1=th1,sbo=sbo,ncmin=ncmin,fstep=fstep,expminf=expminf
;+
; create a set of diffuse (source-removed) background maps
; to be used for map_ratio.pro
;
; ls - count list
; xmin,ymin - lower left corner pixel position
; block - image block factor
; dim - dimension of the image
; bmin, bmax - vectors containing lower and upper limits of the energy bands
;		in units of eV for Chandra data
; tb - exposure mask
; tbs - source region removed exposure mask
; ta - a stack of exposure maps in individual energy bands
; cbm - the output background map
; cbma - the output  background map in individual energy bands
; fra - if given, the images will be binned by this factor
; sb - background maps (e.g., streak background)
;
; th1 - parameters used by adp_gs_m.pro
; ncmin - the minimum number of pixels left for the smoothing to stop
;         used in adp_m.pro
; fstep - maximum fraction of pixels in each smoothing step used in
;         adp_m.pro
;
; written by wqd, April 24, 2002
;-
if n_params() eq 0 then begin
print,'Calling procedure - dbmaps_as,ls,xmin,ymin,block,dim,bmin,bmax'
print,',tb,tbs,ta,cbm,cbma,fra=fra,th1=th1,ncmin=ncmin,expminf=expminf'
print,',fstep=fstep'
return
endif
if n_elements(fra) eq 0 then fra=2
if n_elements(npo) eq 0 then npo=3.
if n_elements(npd) eq 0 then npd=0.1
if n_elements(th1) eq 0 then th1=10

sz=size(ta)
nb=sz(3)
ca=intarr(sz(1),sz(2),nb)

help,ca,/struct
for k=0,nb-1 do begin
	list_image,ls,xmin,ymin,cc,dim,block=block,emin=bmin(k),emax=bmax(k)
	print,dim,block
	help,cc,/struct
	ca(*,*,k)=cc
endfor
tas=ta

;remove source regions in the images:
if n_elements(expminf) eq 0 then expminf=0.1
ss=where(tbs gt expminf*max(tbs))
imagea_sel,ss,ca
imagea_sel,ss,tas
nsb=n_elements(sbo)
if nsb ne 0 then begin
    sb=sbo
    imagea_sel,ss,sb
endif

;rebin the images:
tbtemp=image_comp(tb,1./fra)
sta=fltarr(sz(1)/fra,sz(2)/fra,nb)
for k=0,nb-1 do sta(*,*,k)=image_comp(ta(*,*,k),1./fra)
ts=fltarr(sz(1)/fra,sz(2)/fra,nb)
for k=0,nb-1 do ts(*,*,k)=image_comp(tas(*,*,k),1./fra)
if nsb ne 0 then begin
    cs=fltarr(sz(1)/fra,sz(2)/fra,nb)
    for k=0,nb-1 do cs(*,*,k)=image_comp(ca(*,*,k)-sb(*,*,k),1./fra)*fra^2
endif else begin
    cs=intarr(sz(1)/fra,sz(2)/fra,nb)
    for k=0,nb-1 do cs(*,*,k)=image_comp(ca(*,*,k),1./fra)
endelse

;smooth the images:
;adp_gs_m,cb,imtemp,cbm,ims,npo=npo,npd=npd,th1=th1,filter=tbtemp,back=back
adp_m,cs,ts,fms,fmas,ftonth=th1,mexpt=mexpt,filter=tbtemp,gslo=0.5 $
  ,fstep=fstep,ncmin=ncmin
for k=0,nb-1 do fmas(*,*,k)=median(fmas(*,*,k),3) 
            ;to minimizing the effect of any remaining source effect
;enlarge the images: 
cbma=fltarr(sz(1),sz(2),nb)
if fra ne 1 then begin
    im_fill_enlarge,fms/mexpt,tbtemp,fra,cbm,frac=frac 
    cbm=cbm*tb
    endif else cbm=fms/mexpt*tb
if nsb ne 0 then cbm=cbm+total(sbo,3)
for k=0,nb-1 do begin
	if fra ne 1 then begin
            im_fill_enlarge,fmas(*,*,k)/mexpt,sta(*,*,k),fra,fm,frac=frac 
            cbma(*,*,k)=fm*ta(*,*,k)
        endif else cbma(*,*,k)=fmas(*,*,k)/mexpt*ta(*,*,k)
	if nsb ne 0 then cbma(*,*,k)=cbma(*,*,k)+sbo(*,*,k)
    endfor
return
end
