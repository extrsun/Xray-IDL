pro hkcorhead,h,hout
if n_params(0) eq 0 then begin
 print,'hkcorhead,h,hout'
 print,'Fudge MKFILTER file header so that IDL can read it'
 retall
end
hout=h
str0=h(12)
if strmid(str0,11,1) eq 'E' then begin
 corr='1E'
 for k=0,125 do begin
  str = h(12+2*k)
  strput,str,corr,11
  hout(12+2*k)=str
 endfor
endif
return
end
