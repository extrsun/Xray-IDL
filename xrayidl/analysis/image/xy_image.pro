pro xy_image,x,y,xdim,ydim,image,xmin,ymin,block=block
;-
; construct image
; x,y - integer vectors containing IDL X and Y axis coordinates (i.e., the
;	lower left corner pixel = (0,0)
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - list_image,x,y,xdim,ydim,image,xmin,ymin,block=block'
return
endif

if n_elements(block) eq 0 then block=!block
if n_elements(xmin) eq 0 then xmin=0l
if n_elements(ymin) eq 0 then ymin=0l

image=intarr(xdim,ydim)
;
xmax=xmin+long(xdim)*block
ymax=ymin+long(ydim)*block
c=where(x ge xmin and x le xmax and y ge ymin and y le ymax)
x=(x(c)-xmin)/long(block) & y=(y(c)-ymin)/long(block)
elements=long(xdim)*y+x
print,'Creating image'
sz=size(x)  ; check to see if x is a scalar
case 1 of
  (sz(0) ne 0): begin        ; x is a vector
     h=histogram(elements)
     bin=lindgen(n_elements(h))+min(elements)
     image(bin)=h
     end
  else:    image = 0
endcase
if !debug eq 1 then stop
end 
