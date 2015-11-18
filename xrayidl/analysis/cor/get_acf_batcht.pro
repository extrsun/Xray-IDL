pro get_acf_batch,dim,slow=slow,flow=flow,factor=factor,tail=tail,exptail=exptail
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_acf_batch,dim,slow=slow,flow=flow,factor=factor,exptail=exptail'
return
endif
if n_elements(tail) eq 0 then tail='acf'


get_image,t,c,ts,dim=dim,blow=2,bhigh=3,slow=slow,flow=flow,/tonly,factor=factor,exptail=exptail 
get_acf,dim/2.,ts,ts,14,a,acf,acfe,outfile=!seq_no+'_'+tail+'i23.dat'
make_image,c,dim=dim,emin=42,emax=51,tfile=!seq_no+'_gtiall.dat'
get_acf,dim/2.,c,ts,14,a,acf,acfe,outfile=!seq_no+'_'+tail+'33.dat'

make_image,c,dim=dim,emin=20,emax=41,tfile=!seq_no+'_gtiall.dat'
get_acf,dim/2.,c,ts,14,a,acf,acfe,outfile=!seq_no+'_'+tail+'22.dat'

end
