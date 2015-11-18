pro list_image_m,list,dim,image,xmin,ymin,block=block,filter=filter,det=det,sel=sel,loc=loc,xc=xc,yc=yc
;-
; cast a count list into an image 
;
; list - table list of counts, whose content does not need to match the image boundaries
; xmin, ymin - the SASS pixel coordinates of the lower left corner of the 
;		image. If not provided, xc and yc (or their def) will be
;		used.
; image - the output image
; dim - the dimension of the image
; filter - an image of the size of the image for excluding counts in regions
;	 with filter values = 0
; det - cast the image in the detector coordinates
; sel - the output address of the selected counts in the list
; loc - the output location of the selected counts in the image
;
; writen by wqd, 1994
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - list_image,list,dim,image,xmin,ymin,block=block'
print,',filter=filter,sel=,loc=loc,det=,xc=xc,yc=yc'
return
endif

if n_elements(block) eq 0 then block=!block
image=intarr(dim,dim)
;
if keyword_set(det) eq 0 then begin
	x=list.x & y=list.y
endif else begin
	block=30./1.8684151 ;use this fixed block to get pixel size=15"
	x=list.dx & y=list.dy
	print,'the scale of the image is ', 0.93420756*block,' arcsec'
endelse
if n_elements(xmin) eq 0 then begin
 if n_elements(xc) eq 0 then begin
	case !instr of 
	 'p': xc=7680 ; in units of 0.5"; wqd, Aug. 30, 94
	 'h': xc=4096
	 'a': xc=640
	endcase
 endif
 if n_elements(yc) eq 0 then yc=xc
 hdim=dim*block/2. ; now in units of 0".5
 xmin=nint(xc-hdim)+1 ;because list.x >=1
 ymin=nint(yc-hdim)+1 
endif
xmax=xmin+long(dim)*block
ymax=ymin+long(dim)*block

sel=where(x ge xmin and x le xmax and y ge ymin and y le ymax,nsel)
if nsel eq 0 then stop,'stop: no counts in the list is in the image!!!'
loc=long(dim)*(long(y(sel)-ymin)/long(block))+ $
		long(x(sel)-xmin)/long(block)
if n_elements(filter) ne 0 then begin
	c=where(filter(loc) eq 0.,nc)
	if nc ne 0 then remove,c,loc,sel
endif

print,'Creating image'
sz=size(loc)  ; check to see if x is a scalar
case 1 of
  (sz(0) ne 0): begin        ; x is a vector
     h=histogram(loc)
     bin=lindgen(n_elements(h))+min(loc)
     image(bin)=h
     end
  else:    image = 0
endcase
end 
