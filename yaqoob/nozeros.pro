pro nozeros,x,y,pha,time
if n_params(0) eq 0 then begin
print,'NOZEROS,x,y,pha,time'
print,'remove zeros from observation parameters'
retall
end
x=x(where(x)) & y=y(where(y)) & pha=pha(where(pha))
time=time(where(time))
return
end
