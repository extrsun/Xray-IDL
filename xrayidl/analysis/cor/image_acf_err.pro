pro image_acf_err,radius,list,xmin,ymin,filter,length,angle,acf,acferr,nbin, $
countm,countrms,outfile=outfile,keepedge=keepedge,dir=dir,selv=selv,nsim=nsim $
,block=block
if n_elements(block) eq 0 then block=10
sz=size(filter)
dim=sz(1) < sz(2)
list_image,list,xmin,ymin,image_c,dim,block=block,loc=loc

get_acf,radius,image_c,filter,length,angle,acf,nbin, $
countm,countrms,outfile='none',keepedge=keepedge,dir=dir,selv=selv

acferr=fltarr(length,nsim)

For n=0,nsim-1 do begin
     	bts,loc,locsim
     	h=histogram(loc)
     	bin=lindgen(n_elements(h))+min(loc)
	image_c=fltarr(dim,dim)
     	image_c(bin)=h
endfor

for k=0,length-1 do begin
	avg_median,acfe(k,*),acfm,acfe1,acfe2,siglevel=siglevel
	acfelo(k)=acfe1
	acfehi(k)=acfe2
	print,angle(k),acf(k),acfelo(k),acfehi(k)
endfor
end