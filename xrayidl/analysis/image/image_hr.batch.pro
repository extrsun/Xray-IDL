pro image_hr_batch,seq_no,maxbox,lmin,f,fluxerr=fluxerr
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --image_av_batch ,blow,bhigh,maxbox,lmin,f'
print,',fluxerr=fluxerr'
return
endif
fname='rp'+strtrim(seq_no,2)+'_im2.fits'
h=(readfits(fname,h),0.25)
fname='rp'+strtrim(seq_no,2)+'_im3.fits'
s=(readfits(fname,h),0.25)
fname='rp'+strtrim(seq_no,2)+'_mex.fits'
t=(readfits(fname,h),0.25)
pixel_size=30/0.25
image_hr,maxbox,h,s,t,f,pixel_size=pixel_size,lmin=lmin,fluxerr=fluxerr
end
