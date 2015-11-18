pro ellcl_val,para,xx,yy,cl
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - ellcl_val,para,xx,yy,cl'
return
endif
xo=para(0)
yo=para(1)
aas=para(2)^2
bbs=(1-para(3))^2*aas
index=-3.*para(4)+0.5
al=para(5)
f1=(cos(al)*(xx-xo)+sin(al)*(yy-yo))^2/aas
f2=(-sin(al)*(xx-xo)+cos(al)*(yy-yo))^2/bbs
cl=(1.+f1+f2)^index
return
end
;======================================
pro cal_ellcl,para,dim,cl,bpix=bpix,ipix=ipix
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - cal_ellcl,para,dim,cl,bpix=bpix,ipix=ipix'
print,'para=[xc,yc,a,e,beta,al]'
return
endif
loc=lindgen(long(dim)*dim)
xx=loc mod dim
yy=loc/dim
ellcl_val,para,xx,yy,cl
cl=reform(cl,dim,dim)
if n_elements(ipix) ne 0 then cl=cl*ipix
if n_elements(bpix) ne 0 then cl=cl+bpix
return
end
;======================================
