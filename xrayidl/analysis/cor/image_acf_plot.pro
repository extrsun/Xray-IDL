pro ps=ps
if keyword_set(ps) ne 0 then set_plot,'ps'
head=!data_dir+!seq_no+'_acf'
tail='.dat'
fname=head+'47'+tail
read_acf,fname,a,acf,acfe
plot,a,acf,line=0,Xtitle='Angle (arcminutes)', Ytitle='ACF'
errplot,a,acf-acfe,acf+acfe
fname=head+'67'+tail
read_acf,fname,a,acf,acfe
oplot,a,acf,line=1
errplot,a,acf-acfe,acf+acfe
fname=head+'45'+tail
read_acf,fname,a,acf,acfe
oplot,a,acf,line=2
errplot,a,acf-acfe,acf+acfe
fname=head+'23'+tail
read_acf,fname,a,acf,acfe
oplot,a,acf,line=3
errplot,a,acf-acfe,acf+acfe
;
if keyword_set(ps) ne 0 then begin
device,/close
set_plot,'x'
endif
;
end