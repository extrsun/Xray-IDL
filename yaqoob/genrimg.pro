pro genrimg,xl,xu,yl,yu,xc,yc,img
 if n_params(0) eq 0 then begin
  print,'genrimg,xl,xu,yl,yu,xc,yc,img'
  print,'given the boundaries of a chip on the sis generate '
  print,'an image which has values equal to the distance from'
  print,'the point xc yc'
 retall
end
img=fltarr(864,848)-10.
xcsq = xc*xc & txc = 2.*xc & ycsq = yc*yc & tyc = 2.*yc
for i=xl,xu do begin
	x=float(i) & xsq=x*x
	xfac=txc*x
  for j=yl,yu do begin
	  y=float(j) & ysq=y*y
	  img(i,j)=xsq+ysq-xfac-tyc*y + xcsq + ycsq
  endfor
endfor
return
end
