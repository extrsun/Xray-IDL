pro scan_batch_band,outfile=outfile,tss,exptail=exptail,blow=blow,bhigh=bhigh,radius=radius,souf=souf,threshold=threshold
;-
; detect sources in one individual pspc band
;+
if n_params() eq 0 then begin
print,'calling sequence - scan_batch_band,outfile=outfile,tss,exptail=exptail,blow=blow,bhigh=bhigh,threshold=threshold'
return
endif
if n_elements(threshold) eq 0 then threshold=threshold
if n_elements(exptail) eq 0 then exptail='all'
if n_elements(blow) eq 0 then blow=2
if n_elements(bhigh) eq 0 then bhigh=2
if n_elements(souf) eq 0 then souf=!seq_no+'_souratio.dat'
if n_elements(outfile) eq 0 then outfile=!seq_no+'_sou_'+strtrim(blow,2)+strtrim(bhigh,2)+'.dat'
get_image,tb,cb,dim=512,blow=blow,bhigh=bhigh,exptail=exptail ;,tbs,souf=souf
image_center,cra,cdec
tss=tb
;scan_v,cra,cdec,cb,tb,tbs,blow=blow,bhigh=bhigh,iterate=1,/append,threshold=3.;5,file=outfile,radius=radius
scan_v,cra,cdec,cb,tss,blow=blow,bhigh=bhigh,tss,iterate=2,/append, $
threshold=threshold,file=outfile,radius=radius
end

