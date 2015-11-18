pro dmp8,filename,title,xb,yb,grid
if n_params(0) eq 0 then begin
 print,'dmp8,filename,title,xb,yb,grid'
 print,'dump a grid to ascii 8 rows wide'
 retall
end
if (size(grid))(1) ne 8 then begin
 print,'grid must be 8 wide'
 return
end
ny=(size(grid))(2)
openw,1,filename
printf,1,title
printf,1,'X lower bound, bin centre and Upper bound '
for i=0l,7 do begin
 printf,1,i+1,xb(i,0),0.5*(xb(i,0)+xb(i,1)),xb(i,1)
endfor
 printf,1,'Y lower bound, bin centre and Upper bound '
for i=0l,ny-1 do begin
 printf,1,i+1,yb(i,0),0.5*(yb(i,0)+yb(i,1)),yb(i,1)
endfor
for i=0l,ny-1l do begin
 printf,1,'y ('+strtrim(i+1,2)+'):'
 printf,1,format='(8(E8.2,1X))',grid(0,i),grid(1,i),grid(2,i),grid(3,i),$
 grid(4,i),grid(5,i),grid(6,i),grid(7,i)
endfor
close,1
return
end
