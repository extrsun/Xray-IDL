pro opac,nh,op,k,quad=quad,filter=filter,opfile=opfile
;** Modified by kachun 27 July 1993 to go with new 27 July 1993 version of
;**   f_ab.pro (formerly fit_ab.pro).
;** Modified by kachun 28 July 1993; added filter parameters which will
;**   contain the exposure map and be used in determining bins to inter-
;**   for if the /all keyword is set in the main program.
opext=['3_5','13_18','19_30']
if n_elements(opfile) eq 0 then opfile = 'phoct_dat.ray.'
if n_elements(filter) eq 0 then fil
file=opfile+opext(k)
openr,unit,file,/get_lun
a=''
for i=0,12 do begin
  readf,unit,a
endfor
nbin = 94
arr=fltarr(2,nbin)
readf,unit,arr
free_lun,unit

nhf=fltarr(nbin)
opf=fltarr(nbin)
nhf(0:*)=arr(0,*)
opf(0:*)=arr(1,*)
opf=opf/opf(0)

;** Select out non-zero elements to do interpolation:
if n_elements(filter) ne 0 then sel = where(filter gt 0.,n_sel) else begin
	n_sel=n_elements(nh)
	sel=lindgen(n_sel)
endelse
	nh_temp=nh(sel)
  if quad ne 0 then quadterp,nhf,opf,nh_temp,op_temp $
    else linterp,nhf,opf,nh_temp,op_temp
op=nh*0.
op(sel)=op_temp

return
end
