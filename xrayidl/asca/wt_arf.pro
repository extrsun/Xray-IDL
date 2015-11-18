pro wt_arf,lower, hiher, middl, arf,rspfil=rspfil,phafil=phafil,arffil=arffil

hist = strarr(50)
taskname = 'IDL wt_arf v0.0'
hist(1-1) = 'ARF created by '+taskname
hist(2-1) = 'from '+phafil
hist(3-1) = 'using '+rspfil
nhist=4

print,'Writing arf file '+arffil
instrum=2
waoaa=2.1
nenerg=n_elements(middl)
aa_wrtarf, arffil, nenerg, instrum, waoaa, lower, hiher, middl, arf, $
  hist, nhist, rspfil, status
;stop,'at the end of wt_arf'
return
end
