pro plotimg,x,y
if n_params(0) eq 0 then begin
 print,'plotimg,x,y'
 retall
endif
set_plot,'ps'
device,/landscape
plot,x,y,xrange=[0,256],yrange=[0,256],xtitle='X',ytitle='Y',$
title='NO DX DY AND NO RAN',pos=[0.217,0.1,0.7828,0.9],psym=3
device,/close
set_plot,'x'
return
end
