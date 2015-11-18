pro brtab2st,h,tab,x,y,pha,time,grade,ccd
; Parameter assignment to fits table columns
; Drop events with neg x or y or pha > 4000
if n_params(0) eq 0 then begin
print,'ASSIGNS,h,tab,x,y,pha,time,grade,ccd'
print,'Use FITS table to assign values to x, y, pha, time and grade ccd'
retall
end
x=tbget(h,tab,'RAWX') & x = x*(x ge 0) & x=x*(x le 424) 
y=tbget(h,tab,'RAWY') & y = y*(y ge 0) & y=y*(y le 424)
pha=tbget(h,tab,'pha') & pha=pha >0 & pha=pha*(pha le 4000) 
time=tbget(h,tab,'time')
grade=tbget(h,tab,'grade')+1
ccd=tbget(h,tab,'ccdid')
return
end
