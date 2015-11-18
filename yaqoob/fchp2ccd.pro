pro fchp2ccd,inst,x,y,ccd
if n_params(0) eq 0 then begin
 print,'fchp2ccd,inst,x,y,ccd'
 print,'Given a set of x,y coords in mock 4-chip mode '
 print,'generate the correct CCD ID '
 print,'INST	:=0 for S0, =1 for S1'
 retall
end
np=(size(x))(1)
ccd=intarr(np)-1
s0x=intarr(4,2) & s0y=s0x
s1x=s0x & s1y=s0y
s0x(0,0:1)=[0,417] & s0y(0,0:1)=[423,842]
s0x(1,0:1)=[441,858] & s0y(1,0:1)=[423,842]
s0x(2,0:1)=[442,859] & s0y(2,0:1)=[0,419]
s0x(3,0:1)=[1,418] & s0y(3,0:1)=[0,419]
s1x(0,0:1)=[442,859] & s1y(0,0:1)=[0,419]
s1x(1,0:1)=[1,418] & s1y(1,0:1)=[0,419]
s1x(2,0:1)=[0,417] & s1y(2,0:1)=[423,842]
s1x(3,0:1)=[441,858] & s1y(3,0:1)=[423,842]
if inst eq 0 then begin
 for k=0,3 do begin
 wc=where((x ge s0x(k,0) and x le s0x(k,1) and y ge s0y(k,0) $
 and y le s0y(k,1)),nc)
 if nc gt 0 then ccd(wc) = intarr(nc)+k
 endfor
endif
if inst eq 1 then begin
 for k=0,3 do begin
 wc=where((x ge s1x(k,0) and x le s1x(k,1) and y ge s1y(k,0) $
 and y le s1y(k,1)),nc)
 if nc gt 0 then ccd(wc) = intarr(nc)+k
 endfor
endif
return
end
