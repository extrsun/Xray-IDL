pro factorial,x,y
if n_params(0) eq 0 then begin
 print,'factorial,x,y'
 print,'returns double array y = factoral(integer array x)'
 retall
endif
;what is the maximum factorial we have to find?
mx=max(x)
;factorial array
fact=dblarr(mx+1)
fact(0)=1.d0
for k=1l,mx do begin
 fact(k)=double(k)*fact(k-1)
endfor
y=fact(x)
return
end
