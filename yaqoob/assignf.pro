pro assignf,h,tab,split,echo,x,y,pha,time,grade
; Parameter assignment to fits table columns
; Drop events with neg x or y or pha > 4096
if n_params(0) eq 0 then begin
print,'ASSIGNF,h,tab,split,x,y,pha,time,grade'
print,'Use FITS table to assign values to x, y, pha, time and grade'
retall
end
x=fits_get(h,tab,'x') & x = x > 0 & x = x < 883 
y=fits_get(h,tab,'y') & y = y > 0 & y = y < 881
phas=fits_get(h,tab,'PHAS') & faint2b,phas,split,echo,pha,grade
pha=pha >0 & pha=pha*(pha le 4096) 
time=fits_get(h,tab,'time')
grade=grade+1
return
end
