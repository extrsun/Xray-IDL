pro my_plotsrc, srclist=srclist, number=number, edgex=edgex, edgey=edgey, xshift=xshift, yshift=yshift, charsi=charsi, CHARTHICK=CHARTHICK

data=fltarr(3,number)
openr,lun,srclist,/get_lun
readf,lun,data
close,lun
free_lun,lun

for i=0,number-1 do begin
xyouts,(data[0,i]-edgex[0])/(edgex[1]-edgex[0])+xshift,(data[1,i]-edgey[0])/(edgey[1]-edgey[0])+yshift,string(fix(data[2,i])),/norm,charsi=charsi,CHARTHICK=CHARTHICK
print,string(fix(data[2,i])),(data[0,i]-edgex[0])/(edgex[1]-edgex[0])+xshift,(data[1,i]-edgey[0])/(edgey[1]-edgey[0])+yshift
endfor

end
