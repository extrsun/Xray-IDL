pro assigns,h,tab,xo,yo,pha,time,grade
; Parameter assignment to fits table columns
; Drop events with neg x or y or pha > 4000
if n_params(0) eq 0 then begin
print,'ASSIGNS,h,tab,x,y,pha,time,grade'
print,'Use FITS table to assign values to x, y, pha, time and grade'
retall
end
x=fits_get(h,tab,'rawx') & x = x > 0 & x = x < 424 
y=fits_get(h,tab,'rawy') & y = y > 0 & y = y < 421
pha=fits_get(h,tab,'pha') & pha=pha >0 & pha=pha*(pha le 4000) 
time=fits_get(h,tab,'time')
grade=fits_get(h,tab,'grade')+1
return
end
