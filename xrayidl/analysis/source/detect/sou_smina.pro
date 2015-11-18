pro sou_smina,tb,cbm,block,smina,impsf=impsf,asfrac=asfrac,probth=probth,blow=emin,bhigh=emax $
          ,instr=instr,psffile=psffile
;cbm must be the background count rate for the current tb with bin
;size = block
;create detection lower limit map:
if n_elements(asfrac) eq 0 then asfrac=0.9
if n_elements(probth) eq 0 then probth=-6.0d
sz=size(tb)
bin_sel=where(tb gt 0)      ;size may be changed in map_extract
if n_elements(impsf) ne 0 then core_size=impsf(bin_sel) else begin
    dist_circle,dis,sz(1),(sz(1)+1.)/2.,(sz(2)+1.)/2.
    dd=dis(bin_sel)*(block*!size_pixel/60.) ; in units of arcmin
    psf_params,dd,core_size,perclimit=asfrac,blow=emin,bhigh=emax $
      ,instr=instr,psffile=psffile
endelse
core_size=core_size/block
map_extract,core_size,tb,cbm,bin_sel=bin_sel,core_2=bs,core_1=exptv $
  ,core_bin=core_bin ;,crpix=crpix,/scale,asfrac=asfrac,nbin=nbin,block=block
exptv=exptv/core_bin
bs=bs*(core_size^2*!pi/core_bin)
poisson_inv,double(bs),1.-10^double(probth),slo ;,/interp
		;source detection lower limit in counts
slo=slo > (fix(2.5+sqrt(bs*10))+1)
;slo=slo > (2.5+sqrt(bs*10)) 
 ;slightly biased to small value to compensate the foolowing differences
smina=cbm*0.
smina(bin_sel)=((slo-bs)/exptv)/asfrac 
  ;this broad band rate coult be slightly different the sum of the
  ;rates in subbands as for source rates
end
