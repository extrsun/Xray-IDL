pro spec_comb,r1,er1,r2,er2,rb,erb,fname=fname
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - spec_comb,r1,er1,r2,er2,rb,erb,fname=fname'
return
endif
ser1=er1*er1
ser2=er2*er2
erb=1./(1./ser1+1./ser2)
rb=(r1/ser1+r2/ser2)*erb
erb=sqrt(erb)
if n_elements(fname) eq 0 then fname=!data_dir+strtrim(!seq_no,2)+'_comb'
inputs='FNAME='+strtrim(fname,2)+',BKFIL=none'
make_pha,inputs,rb,erb,1.,1
end

