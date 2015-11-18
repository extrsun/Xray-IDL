pro adp_ewm,imao,back,expta,ewa,index=index,pimmid=pimmid,exptc=exptc,rewa=rewa,rindex=rindex,filter=filter,gslo=gslo,gstep=gstep,gshi=gshi,ftonth=ftonth,gsimsz=gsimsz,nimax=nimax,fstep=fstep,ncmin=ncmin,npfrac=npfrac,bmin=bmin,bmax=bmax
; filter - regions with values <=0 will not be calculated, but
;          source-removed may still be included (e.g., an exposure map)

;read the xspec band count rates:

if n_elements(ncmin) eq 0 then ncmin=0.01
if n_elements(fstep) eq 0 then fstep=0.02
if n_elements(nimax) eq 0 then nimax=1000
if n_elements(gstep) eq 0 then gstep=0.02
if n_elements(gslo) eq 0 then gslo=1.
if n_elements(npfrac) eq 0 then npfrac=0.05

if n_elements(ftonth) eq 0 then ftonth=[0.3,0.3,0.3,0.5]
sz=size(imao)
if sz(0) eq 2 then nima=1 else if sz(0) eq 3 then nima=sz(3) else begin
    print,'ima must be a 2-D or 3-D array!'
    return
endelse
if n_elements(gshi) eq 0 then gshi=min(sz(1:2))/2.
if n_elements(gsimsz) eq 0 then gsimsz=((sz(1) < sz(2))/8)*2+1 ;odd dim

ima=imao
;if the filter is supplied
nfilter=n_elements(filter)
if nfilter eq 0 then begin
    nsel=sz(1)*sz(2)
    sel=lindgen(nsel)
endif else begin
    sel=where(filter gt 0,nsel)
    imagea_sel,sel,ima
endelse

;if n_elements(filter) eq 0 then filter=expta
;sel=where(filter gt 0,nsel)
;if nsel ne sz(1)*sz(2) then imagea_sel,sel,ima

;define output arrays:
ewa=fltarr(sz(1),sz(2))
rewa=fltarr(sz(1),sz(2))
index=fltarr(sz(1),sz(2))
rindex=fltarr(sz(1),sz(2))
pimmid=fltarr(sz(1),sz(2))
exptc=fltarr(sz(1),sz(2))

;define minimum background level to suppress noise
mine=fltarr(nima)
for k=0,nima-1 do begin
    bim=back(*,*,k)
    mine(k)=0.1*avg(bim(sel)) 
     ;arbitrary systematic back error: 10% the background 
endfor

imac=ima*0.
imaec=imac
bmac=imac
ni=0
binsize=gslo
nselo=nsel
ncc=0
while ni lt nimax and binsize lt gshi do begin
    print,'ni, binsize,nsel = ',ni,binsize,nsel
     wlimg=psf_gaussian(nd=2,fwhm=binsize,np=gsimsz,/norm)
     exptac=convolve(expta,wlimg) 
     exptac2=convolve(expta,wlimg^2)
     for k=0,nima-1 do begin
         imac(*,*,k)=convolve(ima(*,*,k),wlimg) 
         imaec(*,*,k)=convolve(ima(*,*,k),wlimg^2)
         bmac(*,*,k)=convolve(back(*,*,k),wlimg) 
         imaec(*,*,k)=sqrt(exptac2*imdiv(imac(*,*,k),exptac)  > 1.e-10)
     endfor
     imac=(imac-bmac) > 1.e-20
     imlo=imac(*,*,0) & immid=imac(*,*,1) & imhi=imac(*,*,2)
     rimlo=imdiv(imaec(*,*,0),imlo)
     rimmid=imdiv(imaec(*,*,1),immid)
     rimhi=imdiv(imaec(*,*,2),imhi)
     ;find pixels with relative count errors smaller than the thresholds
     cc=where(rimlo(sel) lt ftonth(0) and rimmid(sel) lt ftonth(1) and $
             rimhi(sel) lt ftonth(2),nc) 
     missing=-999
     if nc ne 0 then begin
         selc=sel(cc)
         vlo=imlo(selc) & vmid=immid(selc) & vhi=imhi(selc)
         vhi_vlo=vhi/vlo 
         ;get the estimate of the power law index
         pindex=alog(vhi_vlo*(bmax(0)-bmin(0))/(bmax(2)-bmin(2))) $
                /alog((bmax(2)+bmin(2))/(bmax(0)+bmin(0)))
         pvmid=vmid*((bmax(1)+bmin(1))/(bmax(0)+bmin(0)))^pindex $
                  *((bmax(1)-bmin(1))/(bmax(0)-bmin(0)))
         ;get the EW and its relative error
         ew=(vmid/pvmid-1.)*(bmax(1)-bmin(1))
             ewa(selc)=ew 
             pimmid(selc)=pvmid/(bmax(1)-bmin(1))
             index(selc)=pindex
             exptc(selc)=exptac(selc)
             if nc eq nsel then goto,done
             remove,cc,sel
             if nc gt fstep*nsel and nc gt fstep*(nselo-nsel) then $
               gstep=gstep*0.5  ;current step is too large
             nsel=nsel-nc
     endif 
     if nc lt (ncmin*(nselo-nsel) > 1) then gstep=gstep*1.5
                       ;current step is too small
     ni=ni+1
     binsize=binsize*(1.+gstep)
     if !debug eq 1 then stop
     if nsel lt nselo*npfrac then goto,done
 endwhile
done:
;for the rest of the image, set to the current values
; and normalize the smoothed image value to avoid any intensity shift
index(sel)=-999
if !debug eq 2 then stop
return
end
