pro image_av_batch,blow,bhigh,maxbox,lmin,f,fluxerr=fluxerr,nosub=nosub,exptail=exptail
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --image_av_batch ,blow,bhigh,maxbox,lmin,f,exptail='
print,',fluxerr=fluxerr'
return
endif
if n_elements(exptail) eq 0 then exptail=''
if keyword_set(nosub) eq 0 then begin
get_image,t,c,blow=blow,bhigh=bhigh,dim=512,exptail=exptail,slow=4,factor=2.,ts
endif else begin
get_image,t,c,blow=blow,bhigh=bhigh,dim=512,exptail=exptail,slow=4,factor=2. 
ts=t
endelse
pixel_size=30/0.25
tts=t
tts(where(ts le 0.))=0.
c(where(ts le 0.))=0.
t=image_comp(t,0.25)
tts=image_comp(tts,0.25)
c=image_comp(c,0.25)
bixarea=(pixel_size*0.5/60)^2
pperbin=bixarea*0.00001 ;?
image_av,maxbox,c,t*pperbin,tts,f,pixel_size=pixel_size,lmin=lmin,fluxerr=fluxerr
end
