pro chicrit,low,high,fname=fname
if n_params(0) eq 0 then begin
  print,'chicrit,low,high,fname=fname'
  print,'generate table of critical chi-square values for 0.10 0.05 & 0.01'
  print,' percent probability that the hypothesis is correct if chi-square'
  print,'exceeds this value '
  print,' start the table with LOW d.o.f. and end with HIGH d.o.f.'
  retall
end 
openw,1,fname
print,'		critical chi-square values '
print,'      DOF      0.10          0.05        0.01 '
printf,1,'		critical chi-square values '
printf,1,'    DOF      0.10          0.05        0.01 '
for k=low,high do begin
 dof=k & c1=chi_sqr(0.1,dof) & c2=chi_sqr(0.05,dof)&c3=chi_sqr(0.01,dof)
 printf,1,format='(I7,2X,3(F8.2,5X))',k,c1,c2,c3
 print,k,c1,c2,c3
endfor
close,1
return
end
