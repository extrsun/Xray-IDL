pro maskpar,mask,a,b,c,d,e
if n_params(0) eq 0 then begin
print,'MASKPAR,mask,a,b,c,d,e
print,'Adjust obs parameters based on input mask'
retall
end
a=a*mask & if n_params(0) eq 2 then goto, mend
b=b*mask & if n_params(0) eq 3 then goto, mend
c=c*mask & if n_params(0) eq 4 then goto, mend
d=d*mask & if n_params(0) eq 5 then goto, mend
e=e*mask
mend: return
end

