pro printit,image
;
set_plot,'ps'
loadct,0
i=0
while total(strlen(findfile('temp'+strtrim(i,2)+'.ps'))) gt 0 do i=i+1
name='temp'+strtrim(i,2)+'.ps'
;
device,bits_per_pixel=8,file=name,xsize=18.,ysize=18.,/landscape
tv,image
device,/close
set_plot,'x'
;
spawn,'lpr -h -Pnecd '+name
return
end


