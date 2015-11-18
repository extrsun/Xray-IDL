pro brtab2st2,h,tab,x,y,detx,dety,skyx,skyy,pha,pi,time,grade,ccd
; Parameter assignment to fits table columns
; Drop events with neg x or y or pha > 4000
if n_params(0) eq 0 then begin
print,'brtab2st2,h,tab,x,y,detx,dety,skyx,skyy,pha,pi,time,grade,ccd'
print,'Use FITS table to assign values to x, y, pha, time and grade ccd'
retall
end
x=tbget(h,tab,'RAWX') & x = x*(x ge 0) & x=x*(x le 424) 
y=tbget(h,tab,'RAWY') & y = y*(y ge 0) & y=y*(y le 424)
detx=tbget(h,tab,'DETX') & dety=tbget(h,tab,'DETY')
skyx=tbget(h,tab,'X') & skyy=tbget(h,tab,'Y') 
pha=tbget(h,tab,'pha') & pha=pha >0 & pha=pha*(pha le 4000) 
pi=tbget(h,tab,'PI')
time=tbget(h,tab,'time')
grade=tbget(h,tab,'grade')+1
ccd=tbget(h,tab,'ccdid')
return
end
