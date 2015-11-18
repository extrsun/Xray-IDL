pro loadct_self,itab,file=file,dir=dir,NCOLORS = nc1, BOTTOM=botto
;-
; use loadct.pro to read to read my own color table. 
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - loadct_self,itab,file=file,dir=dir,NCOLORS = nc1, BOTTOM=botto'
return
endif
if n_elements(file) eq 0 then file='colors1_self.tbl'
if n_elements(dir) eq 0 then dir=!idl_dir+'/analysis/plot/'
loadct,itab,file=dir+file,NCOLORS = nc1, BOTTOM=botto
return
end