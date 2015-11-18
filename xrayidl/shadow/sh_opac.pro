pro sh_opac,nh,op,k,quad=quad,filter=filter,opfile=opfile,nolog=nolog $
,noim=noim,sel=sel
;-
; nolog - if set, the output op will be the diminution factor instead of
; the opacity
; k - the band number (0,1,2,3,4,5,or 6) 
;	for ['1_2','4_5','6_7','1_1','2_2','3_3','4_4','5_5']
; nh - input column density in units of 10^22 cm^-2
;
;** Modified by kachun 27 July 1993 to go with new 27 July 1993 version of
;**   f_ab.pro (formerly fit_ab.pro).
;** Modified by kachun 28 July 1993; added filter parameters which will
;**   contain the exposure map and be used in determining bins to inter-
;**   for if the /all keyword is set in the main program.
;** Modified by wqd and kachun 4 August 1993; shortened program, and
;**   added opfile parameter while taking away the all parameter and
;**   using only the filter parameter.
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - sh_opac,nh,op,k,quad=quad,filter=filter'
print,',opfile=opfile,nolog=nolog'
return
endif
;** Read in data files:
opext=['1_2','4_5','6_7','1_1','2_2','3_3','4_4','5_5']
if n_elements(quad) eq 0 then quad=0
if n_elements(opfile) eq 0 then opfile ='~wqd/rosatdata/shadow/phoct_dat.normal.'
file=opfile+opext(k) 

openr,unit,file,/get_lun
a=''
for i=0,12 do begin
  readf,unit,a
endfor

nhf=[-999.]
opf=nhf
a=0.
b=0.
while not eof(unit) do begin
	readf,unit,a,b
	nhf=[nhf,a]
	opf=[opf,b]
endwhile
free_lun,unit
nhf=nhf(1:*)
opf=opf(1:*)

opf=opf/opf(0)
n_size = n_elements(nh(0,*))

if n_elements(sel) eq 0 then begin
;** Select out non-zero elements to do interpolation if filter is set;
;** otherwise use all elements in nh parameter:
if n_elements(filter) ne 0 then begin
  sel = where(filter gt 0.,n_sel)
endif else begin
  n_sel = n_elements(nh)
  sel = lindgen(n_sel)
endelse
endif

;** Interpolate:
nh_temp = nh(sel)
if quad ne 0 then begin
;  print,'Beginning quadratic interpolation:'
  quadterp,nhf,opf,nh_temp,op_temp
endif else begin
;  print,'Beginning linear interpolation:'
  linterp,nhf,opf,nh_temp,op_temp
endelse
if keyword_set(nolog) eq 0 then op_temp = alog(op_temp)
if keyword_set(noim) eq 0 then begin
	op = nh*0.
	op(sel) = op_temp
endif else op=op_temp

return
end
