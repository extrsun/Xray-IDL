pro ascabin,inst,e1,bin1,e2,bin2,ein,einerr,ratin,ratinerr,eout,eouterr,ratout,ratouterr,iout
;Author T. Yaqoob
;DATE - sometime in 1995
if n_params(0) eq 0 then begin
print,'ascabin,inst,e1,bin1,e2,bin2,ein,einerr,ratin,ratinerr,eout,eouterr,ratout,ratouterr,iout'
 print,'ASCABIN - Take an array of a quantity versus energy for the '
 print,'ASCA SIS or GIS and bin it up to approximately the energy '
 print,'resolution of the detector. The binned values and errors will '
 print,'correspond to the weighted mean and weigthed errors resp. '
 print,'INPUTS - '
 print,'INST	- = 0 for SIS and =1 for GIS '
 print,'E1, BIN1    - Minimum bin width in band e1-e2 keV '
 print,'E2, BIN2    - Minimum bin width at > e2 keV '
 print,'EIN, EINERR : bin energy centres and half-widths ' 
 print,'RATIN, RATINERR : unbinned quantity and errors '
 print,'OUTPUTS - '
 print,'EOUT, EOUTERR : output bin energy centres and half-widths '
 print,'RATOUT, RATOUTERR: binned output and errors '
 print,'IOUT : number of output bins '
 retall
end
noutmx = 5000l
ni = (size(ein))(1)
eout=fltarr(noutmx) & eouterr=eout & ratout=eout & ratouterr=eout
i=0l
ewid=0.0
emin=1.e36
emax=0.0
for k=0l,ni-1l do begin
 el=ein(k)-einerr(k) & eh=ein(k)+einerr(k)
 emin=min([el,emin])
 emax=max([eh,emax])
 ewid=ewid + (eh-el)
 if inst eq 0 then eres = 0.0086*sqrt(16.+(0.12*ein(k)/0.00365))
 if inst eq 1 then eres = ein(k)*0.078*sqrt(5.9/ein(k))
 if inst eq 0 then begin
  if ein(k) gt e1 and ein(k) le e2 then eres=max([eres,bin1])
  if ein(k) gt e2 then eres=max([eres,bin2])
 endif
 ratout(i) = ratout(i)+ratin(k)/ratinerr(k)/ratinerr(k)
 ratouterr(i) = ratouterr(i) + (1./ratinerr(k)/ratinerr(k)) 
; print,k,i,emin,emax,ewid
 if ewid gt eres then begin
  eout(i)=0.5*(emax+emin)
  eouterr(i)=0.5*(emax-emin)
  ratout(i)=ratout(i)/ratouterr(i)
  ratouterr(i)=sqrt(1./ratouterr(i))
  emin=1.e36 & emax=0.0
  ewid=0.0
  i=i+1l
 endif
endfor
iout=i
print,'Total number of new bins = ',iout
return
end 
