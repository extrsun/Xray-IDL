pro rawxpand,rx,ry,x,y,sistyp,chip
if n_params(0) eq 0 then begin
	print,'RAWXPAND, RX,RY,X,Y,INSTR,CHIP
	print,' expand raw x and raw y using ccd id '
retall
endif
print,'RAWX RAWY: MINMAX',minmax(rx),minmax(ry)
rx=rx>1 & rx=rx<422 & ry=ry>1 & ry=ry<421
x=rx & y=ry
if sistyp eq '0' then begin
 wchip0=where(chip eq 0)
 wchip1=where(chip eq 1)
 wchip2=where(chip eq 2)
 if total(wchip0) gt 0.0 then y(wchip0)=849-y(wchip0)
 if total(wchip1) gt 0.0 then begin
  x(wchip1)=860-x(wchip1) & y(wchip1)=849-y(wchip1)
 endif
 if total(wchip2) gt 0.0 then x(wchip2)=x(wchip2)+439
endif
if sistyp eq '1' then begin
 wchip2 =where(chip eq 2)
 wchip0 =where(chip eq 0)
 wchip3 =where(chip eq 3)
 if total(wchip2) gt 0.0 then y(wchip2)=849-y(wchip2)
 if total(wchip3) gt 0.0 then begin 
  x(wchip3)=861-x(wchip3) & y(wchip3)=849-y(wchip3)
 endif
 if total(wchip0) gt 0.0 then x(wchip0)=x(wchip0)+440
endif
x=x-1 & y=y-1
end
