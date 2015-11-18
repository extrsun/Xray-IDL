pro source_para,seq_no,x_source,y_source,cntr_source
if N_params() LT 3 then begin
	print,'CALLING SEQUENCE - source_para, seq_no,x_source,y_source,cntr_source'
	return
endif
fname=!data_dir+strtrim(seq_no,2)+'_src.fits'
rs_sources,fname,hdr,s
x_source=s.ra/!radeg
y_source=s.dec/!radeg
cntr_source=s.ccts
return
end