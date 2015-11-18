pro mkchitble,maxdof
if n_params(0) eq 0 then begin
  print,'mkchitble,maxdof'
  retall
end
;calculate delta chi^2 for various dof
;for 68% 90% 99% and 3 sigma(99.73%)
p=dblarr(4) & delc=p
p(0)=0.32 & p(1)=0.10 & p(2)=0.01 & p(3)=1.d0-0.9973
openw,1,'delchi.tab'
printf,1,' 			Delta chi-square values '
printf,1,' '
printf,1,'  DOF	       0.68            0.90           0.99	   3 sigma'
print,'  DOF	    0.68            0.90           0.99	   3 sigma'
printf,1,' '
for k=0,maxdof-1 do begin
 for j=0,3 do delc(j)=chi_sqr(p(j),(k+1))
 printf,1,format='(I4,5X,4(F10.3,5X))',k+1,delc(0),delc(1),delc(2),delc(3)
 print,format='(I4,5X,4(F10.3,5X))',k+1,delc(0),delc(1),delc(2),delc(3)
endfor
close,1
return
end
