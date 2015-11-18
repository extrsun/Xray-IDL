pro source_read,struct,sn=seq_no,dir=dir
; get the source information from a PSPC file (i.e., rp123456_src.fits)
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - source_info,struct,[sn,dir]'
retall
endif
;
if n_elements(dir) eq 0 then dir=!data_dir
if n_elements(seq_no) eq 0 then seq_no=!seq_no
fname=dir+strtrim(seq_no,2)+'_src.fits'
rs_sources,fname,hdr,struct
;
return
end