pro mkimage2,qlist,img,pimg,hucut,xyl,cts,avpha,apimg
;make an image from a photon list
if n_params(0) eq 0 then begin 
 print,' MKIMAGE,qlist,image,pimage,hucut,xyl,cts,avpha,apimg'
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
img=intarr(nx,ny)
pimg=intarr(nx,ny)
;hotlist = intarr(n_elements(qlist))
hotcut=0
for i=0,(size(qlist))(1)-1 do begin
	xx=x(i)-xmin
	yy=y(i)-ymin
		img(xx,yy) = img(xx,yy)+1
		pimg(xx,yy) =pimg(xx,yy)+pha(i)
endfor
print,'created images'
hpm = (img gt hotcut) and (img le hucut)
a=where(hpm)/float(nx) & b=fix((a-fix(a))*nx+.1)
xyl=[rotate(b,1),rotate(fix(a),1)] 
np=(size(x))(1)
;hout=intarr(np)
;print,'creating mask ...'
;for j=0l,np-1 do begin
;hout(j) = (total((x(j) eq (xyl(0,*)+xmin)) and (y(j) eq (xyl(1,*)+ymin)))) 
;endfor
;plist = qlist(where(hout))
;nl=(size(plist))(1) & print,' new photon list size',nl
nxy=(size(xyl))(2) & print,' number of occupied pixels ',nxy
cts=lonarr(nxy)
avpha=lonarr(nxy)
openw,2,'mkimage.qdp'
for i=0l,nxy-1 do begin
	cts(i) = img(xyl(0,i),xyl(1,i))
	pimg(xyl(0,i),xyl(1,i)) = pimg(xyl(0,i),xyl(1,i))/cts(i)
	avpha(i) = pimg(xyl(0,i),xyl(1,i))
	printf,2,cts(i),avpha(i)
endfor
close,2
mcts=max(cts)
mpha=max(avpha)
;create 2-d array of avpha and cts
apimg=intarr(mcts,64)
for k=1,mcts do begin
  yarr=avpha(where(cts eq k))
  apimg(k-1,*)=histogram(yarr,min=0,max=2047,binsize=32)
endfor
phlims=intarr(2,1000)
ans='c'
read,' select on mean pha from file (f) or cursor?',ans
if (strmid(ans,0,1) eq 'f') then begin
 fname=' '
 read,' Input file name containing min and max phas ',fname
 openr,1,fname
 for j=0,mcts-1 do begin
 readf,1,p1,p2
 phlims(0,j) = p1 & phlims(1,j) = p2
 print,phlims(0,j),phlims(1,j)
 endfor
 close,1
endif
if (strmid(ans,0,1) ne 'f') then begin
 fname=' '
 read,' Filename to put pha limits in ',fname
 openw,1,fname
 plot,cts,avpha,/xst,/yst,xrange=[0,mcts+1],yrange=[-100,mpha*1.1],psym=1
 for j=0,mcts-1 do begin
  print,' Hit cursor for lower and upper pha limits for cts = ',j+1
  cursor,xc,yc,3 & phlims(0,j) = yc
  cursor,xc,yc,3 & phlims(1,j) = yc
  print,phlims(0,j),phlims(1,j)
  endfor
close,1
forprint,text=fname,phlims(0,0:mcts-1),phlims(1,0:mcts-1)
endif
;now create a new xy list for filtered events
msk=intarr(np)
for i=0l,np-1 do begin
	xx=x(i)-xmin
	yy=y(i)-ymin
	iph=img(xx,yy)-1
	aph = pimg(xx,yy)
	msk(i) = (img(xx,yy) le mcts) and (img(xx,yy) gt hotcut)
msk(i) = (aph ge phlims(0,iph)) and (aph le phlims(1,iph))
endfor
qlist = qlist(where(msk))
end
