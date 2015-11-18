pro chilc,data,err,chisq,pchi
if n_params(0) eq 0 then begin
 print,'chilc,data,err,chisq,pchi'
 print,'Given set of data point compute chi-sq against'
 print,'constant hypothesis and prob that this is obtained '
 print,'by chance.'
 retall
end
npts=(size(data))(1)
print,'# of data points : ',npts
t1=total(data*data/err/err)
t2=total(data/err/err)
t3=total(1./err/err)
kval=t2/t3
print,'Best-fitting constant is ',kval
chisq=t1-(2.*t2*t2/t3)+(t2*t2/t3)
print,'Chi-square = ',chisq
dof=npts-1l
pchi=chi_sqr1(chisq,dof) 
print,'Probability that this chi-sq or less is NOT obtained by chance ',pchi
return
end
