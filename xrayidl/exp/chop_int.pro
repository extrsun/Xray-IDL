pro chop_int,listh,listh_err,time,dev=dev,ymax=ymax,ymin=ymin
;+
; chop off regions with global enhancements in the count histrogram
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -  chop_int,listh,time,dev=dev,ymax=ymax,ymin=ymin'
return
endif
;
if n_elements(dev) eq 0 then dev='x'
;
set_plot,dev
if n_elements(ymax) eq 0 then ymax=40
if n_elements(ymin) eq 0 then ymin=0.
plot,listh,yrange=[ymin,ymax]
;
read,'Give a massive value here for removing peaks, or type 0 for manually removing: ', level
if level ne 0. then sel_bad=where(listh gt level) else begin
  read,'Please give the pairs of the time intervals you want to input: ',npair
  if npair eq 0 then return
  nbino=n_elements(listh)
  sel_bad=lonarr(nbino)
  kk=0


  for k=0,npair-1 do begin
	cursor,x1,y,/data,3
	cursor,x2,y,/data,3
	nx1=nint(x1) > 0
	nx2=nint(x2) < nbino-1
	print,'nx1 , nx2 = ',nx1,nx2
	nbin=nx2-nx1+1
	sel_bad(kk:kk+nbin-1)=nx1+indgen(nbin)
	kk=kk+nbin
  endfor
  sel_bad=sel_bad(0:kk-1)
endelse
remove,sel_bad,time,listh,listh_err
print,n_elements(sel_bad),' bad bins have been chopped off'
;
plot,listh,yrange=[0,30.]

end