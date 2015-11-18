pro rem_0s,a,b,c,d,e
;  Remove zeros from parameter vectors (same dimentions)
if n_params(0) eq 0 then begin
print,'REM_0S,a,b,c,d,c'
print,'Remove zeros from parameter vectors (same dimentions)'
retall
end
a=a(where(a)) & if n_params(0) eq 1 then goto,rend
b=b(where(b)) & if n_params(0) eq 2 then goto,rend
c=c(where(c)) & if n_params(0) eq 3 then goto,rend
d=d(where(d)) & if n_params(0) eq 4 then goto,rend
e=e(where(e))
rend: return
end

