pro ploteric,xl,yl,gxl,gyl
if n_params(0) eq 0 then begin
 print,'ploteric,xl,yl,gxl,gyl'
 retall
end
window,1
plot,gxl,xl,xtitle='GISLIN X',ytitle='IDL X',psym=3
oplot,findgen(256)
window,2
plot,gyl,yl,xtitle='GISLIN Y',ytitle='IDL Y',psym=3
oplot,findgen(256)
set_plot,'ps'
device,/landscape
plot,gxl,xl,xtitle='GISLIN X',ytitle='IDL X',psym=3
oplot,findgen(256)
device,/close
set_plot,'x'
$mv idl.ps idlx_vs_gx.ps
set_plot,'ps'
device,/landscape
plot,gyl,yl,xtitle='GISLIN Y',ytitle='IDL Y',psym=3
oplot,findgen(256)
device,/close
set_plot,'x'
$mv idl.ps idly_vs_gy.ps
return
end
