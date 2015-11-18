pro avg_median,data,data_m,data_me1,data_me2,siglevel=siglevel,pri=pri,sel=sel
;-
; To calculate the median value of the data and its 68\% confidence error
; bars. The data points should be large enough to get a meaningful result.
; writen by WQD 6/18/93
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - avg_median,data,data_m,data_me1,data_me2,siglevel=siglevel,pri=pri,sel=sel'
return
endif
if n_elements(sel) ne 0 then datas=data(sel) else datas=data
datas=datas(sort(datas))
ndata=n_elements(datas)
;if ndata lt 3 then begin
;	data_m=datas
;	data_me1=data_m
;	data_me2=data_m
;	print,'WARNING: there is less than 3 points in the data'
;endif else begin
if n_elements(siglevel) eq 0 then siglevel=0.16
	nh=ndata/2
	n16=long(siglevel*ndata)
	if nh*2 ne ndata then data_m=datas(nh) else $
		data_m=0.5*(datas(nh)+datas(nh-1))
	data_me1=datas(n16)
	data_me2=datas(ndata-n16-1)
if keyword_set(pri) ne 0 then $
	print,'data_m,data_me1,data_me2 = ',data_m,data_me1,data_me2
;endelse
end