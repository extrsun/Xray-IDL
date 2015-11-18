pro get_conv,cluster,psf,conv,radius=radius
;
; get the convolution of the intrinsic cluster flux distribution with the
; PSF of the instrument
;
if N_params() eq 0 then begin
print,'CALLING SEQUENCE - get_conv,cluster,psf,conv,radius=radius'
return
endif 
;
szpsf=size(psf)
edge=(szpsf(1)-1)/2
szc=size(cluster)
dimc=szc(1)
dimconv=(dimc-2*edge)
conv=cluster*0.
if n_elements(radius) eq 0 then radius=(dimconv-1)/2.
;
dist_circle,dis,dimc,(dimc-1.)/2.,(dimc-1.)/2.
sel=where(dis le radius, nsel)
j=sel/dimc
i=sel mod dimc
jmin=j-edge
jmax=j+edge
imin=i-edge
imax=i+edge
for k = 0, nsel-1 do begin
	conv(imin(k):imax(k),jmin(k):jmax(k)) = $
	conv(imin(k):imax(k),jmin(k):jmax(k))+psf*cluster(i(k),j(k))
endfor
end
