pro get_acfi_batch,dim=dim
if n_elements(dim) eq 0 then dim=120
get_image,t,c,ts,dim=dim,blow=6,bhigh=7,/tonly
get_acf,dim/2.,ts,ts,14,a,acf,acfe,outfile=!seq_no+'_acfi67.dat'

get_image,t,c,ts,dim=dim,blow=4,bhigh=5,/tonly
get_acf,dim/2.,ts,ts,14,a,acf,acfe,outfile=!seq_no+'_acfi45.dat'

get_image,t,c,ts,dim=dim,blow=2,bhigh=3,/tonly
get_acf,dim/2.,ts,ts,14,a,acf,acfe,outfile=!seq_no+'_acfi23.dat'

end