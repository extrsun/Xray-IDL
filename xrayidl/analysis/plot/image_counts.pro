pro image_counts,list,cor,xmin,ymin,xdim,ydim,det=det,xp=xp,yp=yp,psym=psym,symsize=symsize,color=color,thick=thick,detagname=detagname
;+
; plot a 2-D count distribution using a count list
;
; list - input count list
; detagname - calternative (e.g., detector) coordinates 
;	with the tagname = detagname+'x' and detagname+'y'
; cor - the corner coordinates from cont_grey
; xmin, ymin - the low left corner pixel coordinates
; dim - the dimension of the image
; det - if set, the detector coordinates will be used
;
; writen by wqd, Jan, 31, 1995
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - image_counts,list,cor,xmin,ymin,dim,det=det,xp=xp,yp=yp,psym=psym,symsize=symsize,color=color,thick=thick,detagname=detagname'
return
endif
 if n_elements (detagname) eq 0 then detagname=''
if n_elements(color) eq 0 then color=!d.n_colors-1
if n_elements(ydim) eq 0 then ydim=xdim
if n_elements(symsize) eq 0 then symsize=1
if n_elements(psym) eq 0 then psym=3
if n_elements(xp) ne 0 then begin
	x=xp & y=yp
endif else begin
	tagin=tag_names(list)
	match,tagin,strupcase(detagname+'x'),xtagn
	match,tagin,strupcase(detagname+'y'),ytagn
	x=list.(xtagn(0)) & y=list.(ytagn(0)) 
endelse
sz=size(x)

mxdim=xdim+1. ;inlcuding whole edge pixels
mydim=ydim+1. 
mxmin=xmin-0.5
mymin=ymin-0.5
mxmax=mxmin+mxdim
mymax=mymin+mydim
c=where(x ge mxmin and x le mxmax and y ge mymin and y le mymax,nc)
if nc eq 0 then begin
    print,'no counts included in the field!!!'
    return
endif
if sz(2) lt 4 then begin
  x=randomu(seed,nc)+x(c)-xmin & y=randomu(seed,nc)+y(c)-ymin
; the randomizing is to avoid that counts at same position simply piling up
;
endif  else  begin
    x=x(c)-mxmin & y=y(c)-mymin
endelse
xpixn=(cor(1)-cor(0))/float(mxdim)
ypixn=(cor(3)-cor(2))/float(mydim)

x=cor(0)+x*xpixn ;x is in the range of 0 to dim as in the figure
y=cor(2)+y*ypixn
plots,x,y,/normal,psym=psym,symsize=symsize,color=color,thick=thick
if !debug eq 3 then stop
return
end
