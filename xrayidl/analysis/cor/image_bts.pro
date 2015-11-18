pro image_bts,seed,list,xmin,ymin,dim,image_c,nsim=nsim,block=block
if N_params() eq 0 then begin
print,'CALLING SEQUENCE - '
print,'image_bts,seed,list,xmin,ymin,dim,image_c,nsim=nsim,block=block'
return
endif
if n_elements(block) eq 0 then block=30
if n_elements(nsim) eq 0 then nsim=100

list_image,list,xmin,ymin,im,dim,block=block,loc=loc

image_c=bytarr(dim*dim,1+nsim)
For n=1,nsim do begin
     	bts,seed,loc,locsim
     	h=histogram(locsim)
     	bin=lindgen(n_elements(h))+min(locsim)
	image_c(bin,n)=h
endfor
image_c=reform(image_c,dim,dim,1+nsim)
image_c(*,*,0)=im
end