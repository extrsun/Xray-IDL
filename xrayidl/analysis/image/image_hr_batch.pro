pro image_hr_batch,maxbox,lmin,f,fluxerr=fluxerr
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --image_av_batch ,blow,bhigh,maxbox,lmin,f'
print,',fluxerr=fluxerr'
return
endif
fname=strtrim(!seq_no,2)+'_im2.fits'
imh=image_comp(readfits(fname,h),0.25)
fname=strtrim(!seq_no,2)+'_im3.fits'
ims=image_comp(readfits(fname,h),0.25)
fname=strtrim(!seq_no,2)+'_mex.fits'
imt=image_comp(readfits(fname,h),0.25)
pixel_size=30/0.25
image_hr,maxbox,imh,ims,imt,imt,f,pixel_size=pixel_size,lmin=lmin,fluxerr=fluxerr
end
