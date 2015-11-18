pro calc_feint,gam,amin,amax,qname=qname
if n_params(0) eq 0 then begin
 print,'calc_feint,gam,amin,amax,qname=qname'
 print,'Make a QDP file containing the value of the Fe K integral'
 print,'as a function of alpha (=Afe*NH) for several gammas'
 retall
endif
if n_elements(qname) eq 0 then begin
 qname=' '
 read,'Enter name of output QDP file',qname
endif 
npts=200
ngam=n_elements(gam)
dalph=(amax-amin)/float(npts-1)
alpha=fltarr(npts) & alpha=amin+dalph*findgen(npts)
e0=7.11 & nterm = 15
openw,1,qname
printf,1,'SKIP SINGLE'
printf,1,'LA X \ga (0.0012 * A\dFe\u * N\d21\u) '
printf,1,'LA Y Fe K integral '
printf,1,'LA T Gammas: ',gam
for j=0,ngam-1 do begin
 for k=0,npts-1 do begin
  feintegral,gam(j),e0,alpha(k),nterm,sum
  printf,1,alpha(k),sum
 endfor
 printf,1,'no no'
endfor
close,1
return
end
