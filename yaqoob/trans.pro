pro trans,x0,y0,x1,y1,x2,y2,x3,y3,xd,yd
if n_params(0) eq 0 then begin
 print,'trans,x0,y0,x1,y1,x2,y2,x3,y3,xd,yd'
 retall
endif
x0 = x0 > 7 & x0 = x0 < 424
y0 = y0 > 2 & y0 = y0 < 421
x1 = x1 > 7 & x1 = x1 < 424
y1 = y1 > 2 & y1 = y1 < 421
x2 = x2 > 7 & x2 = x2 < 424
y2 = y2 > 2 & y2 = y2 < 421
x3 = x3 > 7 & x3 = x3 < 424
y3 = y3 > 2 & y3 = y3 < 421
x0d=633-x0 & y0d=1053-y0
x1d=1074-x1 & y1d=1053-y1
x2d=644+x2 & y2d=207+y2
x3d=203+x3 & y3d=207+y3
xd=[x0d,x1d,x2d,x3d]
yd=[y0d,y1d,y2d,y3d]
return
end
