pro xy2image,x,y,pha,xran,yran,dims,xbns,ybns,img,pimg
;make an image from a photon list
if n_params(0) eq 0 then begin 
 print,' XY2IMAGE,x,y,pha,xran,yran,dims,xbns,ybns,image,pimage'
 print,' INPUTS: xran=[xmin,ymin] yran=[ymin,ymax] dims=[xdim,ydim]'
 print,' OUPUTS: xbns(*,3) ybns(*,3) [low,high and centre]'
 print,'       : img - IMAGE and pimg - total pulse height image '
retall
end
xmin=xran(0)
xmax=xran(1)
ymin=yran(0)
ymax=yran(1)
nx=dims(0) & ny=dims(1)
delx=(xmax-xmin)/float(nx) & dely=(ymax-ymin)/float(ny)
xbns=fltarr(nx,3) & ybns=fltarr(ny,3)
xl=fltarr(nx) & xh=xl & yl=fltarr(ny) & yh=yl & xcen=xl & ycen=yl
xbns(*,0)=xmin+findgen(nx)*delx & xbns(*,1)=xmin+delx*(1.+findgen(nx))
ybns(*,0)=ymin+findgen(ny)*dely  & ybns(*,1)=ymin+delx*(1.+findgen(ny))
xbns(*,2)=(xbns(*,0)+xbns(*,1))/2. & ybns(*,2)=(ybns(*,0)+ybns(*,1))/2.
img=lonarr(nx,ny)
pimg=lonarr(nx,ny)
nevents=0l
for i=0l,(size(x))(1)-1 do begin
 if x(i) ge xmin and x(i) le xmax and y(i) ge ymin and y(i) le ymax $
 then begin
	nevents=nevents+1l
	xx= fix((x(i)-xmin)/delx) & xx=min([nx-1,xx])
	yy= fix((y(i)-ymin)/dely) & yy=min([ny-1,yy])
;	xx = where((x(i) ge xbns(*,0) and x(i) lt xbns(*,1)),nxx)
;	if nxx le 0 then print,x,' out of range '
;	yy = where((y(i) ge ybns(*,0) and y(i) lt ybns(*,1)),nyy)
;	if nyy le 0 then print,y(i),' out of range 
;	if nxx gt 0 and nyy gt 0 then begin
		img(xx,yy) = img(xx,yy)+1l
		pimg(xx,yy) =pimg(xx,yy)+pha(i)
;        endif
 endif
endfor
print,' XY2IMAGE: Total number of events input : ',(size(x))(1)
print,' XY2IMAGE: Total number of events in image : ',nevents
end
