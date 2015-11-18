pro optxsflxalw,gridi1,gridi2,grido,tolerence=tolerence,ifl=ifl
if n_params(0) eq 0 then begin
 print,'optxsflxalw,gridi1,gridi2,grido,tolerence=tolerence,ifl=ifl'
 print,'Take input flux grids GRIDI1(nx1,ny1,4) and GRIDI2(nx2,ny2,4)' 
 print,'and make a grid GRIDI(nx2,ny2,nx1*ny1) made as follows:' 
 print,'For each gridi1(*,*,ifl) pick out fluxes in gridi2(*,*,ifl)'
 print,'which are equal within the TOLERENCE value (percent) '
 print,'and set grido(x2,y2,*) equal to 1, 0 otherwise'
 retall
end
nx1=(size(gridi1))(1)
ny1=(size(gridi1))(2)
nx2=(size(gridi2))(1)
ny2=(size(gridi2))(2)
grido=fltarr(nx2,ny2,nx1,ny1)
tmpgrd2=fltarr(nx2,ny2)
for i=0l,nx1-1l do begin
 for j=0l,ny1-1l do begin
  tmpgrd=abs((gridi2(*,*,ifl)-gridi1(i,j,ifl))/gridi1(i,j))
  w=where((tmpgrd le tolerence),nw)
  if nw gt 0 then tmpgrd2(w)=1.
  grido(0:nx2-1,0:ny2-1,i,j) = tmpgrd2
 endfor
endfor
return
end
