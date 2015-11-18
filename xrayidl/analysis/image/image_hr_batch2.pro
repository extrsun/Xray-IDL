pro image_hr_batch2,blow,bhigh,blow2,bhigh2,maxbox,lmin,f,fluxerr=fluxerr
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --image_hr_batch,blow,bhigh,blow2,bhigh2,maxbox,lmin,f'
print,',fluxerr=fluxerr'
return
endif
get_image,t,ims,blow=blow,bhigh=bhigh,dim=512,tfile=!seq_no+'_gtiall.dat' ;,slow=4,factor=2.
get_image,t,imh,blow=blow2,bhigh=bhigh2,dim=512,tfile=!seq_no+'_gtiall.dat' ;,slow=4,factor=2.
pixel_size=30/0.25
tts=t
;tts(where(ts le 0.))=0.
;c(where(ts le 0.))=0.
t=image_comp(t,0.25)
;tts=image_comp(tts,0.25)
ims=image_comp(ims,0.25)
imh=image_comp(imh,0.25)
image_hr,maxbox,imh,ims,t,t,f,pixel_size=pixel_size,lmin=lmin,fluxerr=fluxerr
end
