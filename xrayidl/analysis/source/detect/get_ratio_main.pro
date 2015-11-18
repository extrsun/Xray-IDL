pro get_ratio_main,exptail=exptail,slow=slow,flow=flow,soufile=soufile,outfile=outfile
if n_elements(exptail) eq 0 then exptail='all'
if n_elements(slow) eq 0 then slow=4.
if n_elements(flow) eq 0 then  flow=0.
if n_elements(soufile) eq 0 then soufile=!seq_no+'_sou.dat'
get_image,ts,cs,tss,blow=1,bhigh=2,exptail=exptail,slow=slow,flow=flow,soufile=soufile
get_image,tm,cm,blow=4,bhigh=5,exptail=exptail,slow=slow,flow=flow,soufile=soufile
get_image,th,ch,blow=6,bhigh=7,exptail=exptail,slow=slow,flow=flow,soufile=souf
get_ratios,cs,ts,cm,tm,ch,th,tss,blow=1,bhigh=2,infile=soufile,outfile=outfile $
,slow=slow,flow=flow
end