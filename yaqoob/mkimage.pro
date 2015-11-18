pro mkimage,qlist,img,pimg
;Author T. Yaqoob - March 1993 ->**
;make an image from a photon list
if n_params(0) eq 0 then begin 
 print,' MKIMAGE,plist,image'
retall
end
x=qlist.x
y=qlist.y
xmin=min(x)
xmax=max(x)
ymin=min(y)
ymax=max(y)
pha=qlist.pha
nx=xmax-xmin+1
ny=ymax-ymin+1
img=lonarr(nx,ny)
pimg=lonarr(nx,ny)
for i=0l,(size(qlist))(1)-1 do begin
	xx=x(i)-xmin
	yy=y(i)-ymin
		img(xx,yy) = img(xx,yy)+1l
		pimg(xx,yy) =pimg(xx,yy)+pha(i)
endfor
end
