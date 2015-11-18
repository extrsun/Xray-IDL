pro meancntr,plist,t1,t2,x0,y0
if n_params(0) eq 0 then begin
 print,'meancntr,plist,t1,t2,x0,y0'
 print,'return mean coords of photon list between times t1 and t2'
 retall
endif
x0=0. & y0=0.
np=(size(plist))(1) 
x=plist.x & y=plist.y & time=plist.time
wtm=where((time ge t1 and time lt t2),nwtm)
if nwtm gt 0 then begin
x0=total(x(wtm))/float(nwtm)
y0=total(y(wtm))/float(nwtm)
print,' t1 t2 x0 x0'
print,t1,t2,x0,y0
endif
return
end
