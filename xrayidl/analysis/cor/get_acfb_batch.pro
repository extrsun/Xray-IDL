pro get_acfb_batch,dim,slow=slow,flow=flow,factor=factor,tail=tail
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_acf_batch,dim,slow=slow,flow=flow,factor=factor'
return
endif
if n_elements(tail) eq 0 then tail='acf'
get_image,t,c,ts,dim=dim,blow=6,bhigh=7,slow=slow,flow=flow,/tonly,factor=factor

;get_acf,dim/2.,ts,ts,14,a,acf,acfe,outfile=!seq_no+'_'+tail+'i67.dat'
make_image,c,dim=dim,emin=91,emax=201,tfile=!seq_no+'_gtiall.dat'
get_acf,dim/2.,c,ts,14,a,acf,acfe,outfile=!seq_no+'_'+tail+'67.dat'

get_image,t,c,ts,dim=dim,blow=4,bhigh=5,slow=slow,flow=flow,/tonly,factor=factor
;get_acf,dim/2.,ts,ts,14,a,acf,acfe,outfile=!seq_no+'_'+tail+'i45.dat'
make_image,c,dim=dim,emin=52,emax=90,tfile=!seq_no+'_gtiall.dat'
get_acf,dim/2.,c,ts,14,a,acf,acfe,outfile=!seq_no+'_'+tail+'45.dat'

get_image,t,c,ts,dim=dim,blow=2,bhigh=3,slow=slow,flow=flow,/tonly,factor=factor 
;get_acf,dim/2.,ts,ts,14,a,acf,acfe,outfile=!seq_no+'_'+tail+'i23.dat'
make_image,c,dim=dim,emin=20,emax=51,tfile=!seq_no+'_gtiall.dat'
get_acf,dim/2.,c,ts,14,a,acf,acfe,outfile=!seq_no+'_'+tail+'23.dat'

end
