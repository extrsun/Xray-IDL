pro hardim,image
set_plot,'ps'
device,/landscape
shade_surf,image
device,/close
set_plot,'x'
$lpr idl.ps
return
end
