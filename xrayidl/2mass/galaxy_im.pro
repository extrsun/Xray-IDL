pro galaxy_im,sra,sdec,cra,cdec,dimx,dimy,im,pixsize=pixsize,block=block,loc=loc,filter=filter,val=val
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - galaxy_im,sra,sdec,cra,cdec,dimx,dimy,im'
print,',pixsize=pixsize,block=block,loc=loc,filter=filter,val=val'
return
endif
if n_elements(val) eq 0 then val=0
if n_elements(pixsize) eq 0 then pixsize=60.
if n_elements(block) eq 0 then block=1
trans_dist,cra,cdec,sra,sdec,xp,yp,/deg,pixsize=pixsize
hdimx=dimx*0.5
hdimy=dimy*0.5
xp=xp+hdimx
yp=yp+hdimy
if n_elements(filter) ne 0 then begin
	s=size(filter) & nxfilter=s(1)
	;compute Location indices.
	imLoc = long(xp/block) + long(yp/block) * long(nxfilter)
	c=where(filter(imloc) eq val,nc) 
	if nc ne 0 then begin
		xp=xp(c)
		yp=yp(c)
		loc=lindgen(long(dimx)*long(dimy))
		loc=loc(c)
	endif
endif
list_image,cra,0,0,im,dimx,dimy,block=block,xp=xp,yp=yp
return
end