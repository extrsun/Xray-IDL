pro acf_test,dim=dim,slow=slow,flow=flow,factor=factor
if n_elements(dim) eq 0 then dim=120
get_image,t,c,ts,dim=dim,blow=6,bhigh=7,slow=slow,flow=flow,/tonly,factor=factor
make_image,c,dim=dim,emin=132,emax=201,tfile=!seq_no+'_gtiall.dat'
get_acf,dim/2.,c,ts,14,a,acf,acfe,outfile=!seq_no+'_acf77.dat'

make_image,c,dim=dim,emin=91,emax=131,tfile=!seq_no+'_gtiall.dat'
get_acf,dim/2.,c,ts,14,a,acf,acfe,outfile=!seq_no+'_acf66.dat'

end
