pro adp_ew,imao,back,expta,ewidth,ewa,mfile=mfile,index=index,pimmid=pimmid,exptc=exptc,rewa=rewa,rindex=rindex,filter=filter,gslo=gslo,gstep=gstep,gshi=gshi,ftonth=ftonth,gsimsz=gsimsz,nimax=nimax,fstep=fstep,ncmin=ncmin,npfrac=npfrac
;+
; construct an equivalent width (EW) map width the continuum
; subtraction based on the XSPEC power law prediction and lower and
; upper measured counts
;
;*INPUTS:
; imao, back, expta - count, background, and exposure arrays stacked
;                     together in three bands (lower, middle (line),
;                     upper).
; ewidth - the width of the middle (line) band in keV
; mfile - the xspec model file name 
; filter - regions with values <=0 will not be calculated, but
;          source-removed may still be included (e.g., an exposure map)
; gslo, gstep, gshi - initial, step, and largest smoothing gaussian
;                     size
; ftonth - signal to noise ratio vector for the three bands and the EW
;          calculation
;
;*OUTPUTS:
; ewa - equivalent width image
; rewa - error in ewa
; index - power law index and middle band continuum count maps
;         predicted from the upper to lower band ratio
; rindex - error in the power law index
; exptc - convolved exposure map
; 
; written by wqd, Feb. 2006
;-
if n_params() eq 0 then begin
print,'CALLIG SEQ - adp_ew,imao,back,expta,ewidth,ewa'
print,',index=index,pimmid=pimmid,exptc=exptc,rewa=rewa,rindex=rindex'
print,',filter=filter,gslo=gslo,gstep=gstep,gshi=gshi,ftonth=ftonth'
print,',gsimsz=gsimsz,nimax=nimax,fstep=fstep,ncmin=ncmin,mfile=mfile,npfrac=npfrac'
return
endif
;read the xspec band count rates:
if n_elements(mfile) eq 0 then mfile='gamma_count.dat'
getdatafromfile,mfile,dt
sz=size(dt)
mindex=reform(dt(0,*),sz(2))
m_hi_lo=reform(dt(3,*)/dt(1,*),sz(2)) ;high to low band ratio
m_mid_lo=reform(dt(2,*)/dt(1,*),sz(2)) ;meddle to low band ratio

if n_elements(ncmin) eq 0 then ncmin=0.01
if n_elements(fstep) eq 0 then fstep=0.02
if n_elements(nimax) eq 0 then nimax=1000
if n_elements(gstep) eq 0 then gstep=0.02
if n_elements(gslo) eq 0 then gslo=1.
if n_elements(npfrac) eq 0 then npfrac=0.01

if n_elements(ftonth) eq 0 then ftonth=[0.3,0.3,0.3,0.5]
sz=size(imao)
if sz(0) eq 2 then nima=1 else if sz(0) eq 3 then nima=sz(3) else begin
    print,'ima must be a 2-D or 3-D array!'
    return
endelse
;if n_elements(gshi) eq 0 then gshi=min(sz(1:2))/2.
if n_elements(gshi) eq 0 then gshi=min(sz(1:2))
if n_elements(gsimsz) eq 0 then gsimsz=((sz(1) < sz(2))/3)*2+1 ;odd dim

ima=imao
nfilter=n_elements(filter)
if nfilter eq 0 then begin
    nsel=sz(1)*sz(2)
    sel=lindgen(nsel)
endif else begin
    sel=where(filter gt 0,nsel)
    imagea_sel,sel,ima
endelse

;define output arrays:
ewa=fltarr(sz(1),sz(2))
rewa=fltarr(sz(1),sz(2))
index=fltarr(sz(1),sz(2))
rindex=fltarr(sz(1),sz(2))
pimmid=fltarr(sz(1),sz(2))
exptc=fltarr(sz(1),sz(2))

;define the minimum background level to suppress noise
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
missing=-999
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
     if nc ne 0 then begin
         selc=sel(cc)
         vlo=imlo(selc) & vmid=immid(selc) & vhi=imhi(selc)
         rvlo=rimlo(selc) & rvmid=rimmid(selc) & rvhi=rimhi(selc) 
         ;observed high to low band ratio and its relative error
         vhi_vlo=vhi/vlo & r_vhi_vlo=sqrt(rvlo^2+rvhi^2)
         ;get the estimate of the power law index
         linterp,m_hi_lo,mindex,vhi_vlo,pindex,mis=missing
         ;get the estimate of the predicted middle to high ratio 
         linterp,m_hi_lo,m_mid_lo,vhi_vlo,p_mid_lo,mis=missing
         ;the lower and upper errors of the ratio
         linterp,m_hi_lo,m_mid_lo,vhi_vlo*(1.-r_vhi_vlo),p_mid_lo_e1,mis=missing
         linterp,m_hi_lo,m_mid_lo,vhi_vlo*(1.+r_vhi_vlo),p_mid_lo_e2,mis=missing
         ;get the predicted middle band counts and error
         pvmid=vlo*p_mid_lo
         rpvmid=0.5*(p_mid_lo_e2-p_mid_lo_e1)/p_mid_lo
         ;get the EW and its relative error
         ew=(vmid/pvmid-1.)*ewidth
         rew=sqrt(rvmid^2+rpvmid^2)*ew
         ss=where(rew lt ftonth(3) and pindex gt 0 and  $
                  p_mid_lo ne missing,nss)
         if nss ne 0 then begin
             sels=selc(ss)
             ewa(sels)=ew(ss) & rewa(sels)=rew(ss)
             pimmid(sels)=pvmid(ss)
             index(sels)=pindex(ss) ;& rindex(sels)=prindex(ss)
             exptc(sels)=exptac(sels)
             if nss eq nsel then goto,done
             remove,cc(ss),sel
             if nss gt fstep*nsel and nss gt fstep*(nselo-nsel) then $
               gstep=gstep*0.5  ;current step is too large
             nsel=nsel-nss
         endif
     endif else nss=0
     if nss lt (ncmin*(nselo-nsel) > 1) then gstep=gstep*1.5
                       ;current step is too small
     ni=ni+1
     binsize=binsize*(1.+gstep)
     if !debug eq 1 then stop
;     if nsel lt nselo*npfrac then goto,done
 endwhile
done:
;for the rest of the image, set to the current values
;index(sel)=-999
if !debug eq 2 then stop
return
end
