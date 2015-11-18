pro assigng,h,tab,x,y,pha,time,rtm
; Parameter assignment to fits table columns
; Drop events with neg x or y or pha > 4000
if n_params(0) eq 0 then begin
print,'ASSIGN,h,tab,x,y,pha,time,rtm
print,'Use FITS table to assign values to x, y, pha, time and rs tm'
retall
end
x=fits_get(h,tab,'rawx') & x = x > 0 & x = x < 256 
y=fits_get(h,tab,'rawy') & y = y > 0 & y = y < 256
pha=fits_get(h,tab,'pha') & pha=pha >0 & pha=pha*(pha le 4000) 
time=fits_get(h,tab,'time')
rtm=fits_get(h,tab,'rise_time')+1
return
end
