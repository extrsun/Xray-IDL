pro get_expsub,image_t,sn=seq_no,dir=dir,source_excl=sexcl,fracofradius=frac
;
; sexcl - an array containing the mplsx numbers of the sources exempted from
;         the subtraction
;
if n_params() eq 0 then begin
print,'CALL SEQUENCE - get_expsub,image_t,[sn,dir,source_excl(array),fracofradius]'
retall
endif
;
if N_elements(seq_no) eq 0 then seq_no=!seq_no else !seq_no=seq_no
if n_elements(dir) eq 0 then dir=!data_dir else !data_dir=dir
image_get,image_t,h,sn=seq_no+'_mex'
	crval=sxpar(h,'crval*')/!radeg ; convert degree into radian
	image_ra=crval(0)
	image_dec=crval(1)
       source_info,sid,source_ra,source_dec
nexcl=n_elements(sexcl)
if nexcl ne 0 then begin  
for i=0, nexcl-1 do begin
c=where(sid ne strtrim(sexcl(i)))
print,'source ',sid(where(sid eq strtrim(sexcl(i)))),' is not subtracted'
sid=sid(c)
source_ra=source_ra(c)
source_dec=source_dec(c)
endfor
endif
       image_t=source_sub(image_t,image_ra,image_dec,source_ra,source_dec $
               ,fracofradius=frac)
if !debug eq 1 then stop
return
end

