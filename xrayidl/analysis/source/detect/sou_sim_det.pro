;+
; Main program for source map-detection in simulated background
; images. useful for estimating the fake detections due to 
; background fluctuations.
;*Requirement:
; sou_main should be run, probably with wavesd=0 and oldbmap=1 set,
; which requires the availability of the background map file
; *_cbma.fits.
;
; wqd, 11/4/03
;-
if n_elements(sprobth) eq 0 then sprobth=-7. ;detection threshold
if n_elements(probth_map) eq 0 then probth_map=-5.
if n_elements(nsim) eq 0 then nsim=10 ;number of desirable simulations
nss=intarr(nsim)
sprobv=[-999]
for k=0,nsim-1 do begin
    ci=poidev(cbm)
    scan_map,nra,ndec,ci,cbm,tb,xoff,yoff,block=block,sigma=s,cntr=c $
      ,thre=probth_map,sfrac=dsfrac,inr=inr,outf=soufile,psffile=psffile $
      ,fac_ssr=fac_ssr,sprob=sprob
    sel=where(sprob lt sprobth,nsel)
    sprobv=[sprobv,sprob]
    if nsel ne 0 then nss(k)=nsel
endfor 
sprobv=sprobv(1:*)
print,sprobv
print,'nss = ',nss
print,'Average number of detection is ',avg(nss),' for sprobth = ',sprobth
end
