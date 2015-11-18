function psf_chi, tt,binsize,npix,plotoff=plotoff
;+
if 2*(fix(npix)/2) eq npix then begin
	print,'change npix into an odd number'
endif
hnpix=fix(npix)/2
delfv = (findgen(hnpix) +1.)*(tt*!pi*binsize)
psf = (sin(delfv)/delfv)^2
psf=[reverse(psf),1.,psf]
psf=psf/total( psf )
if keyword_set(plotoff) eq 0 then plot,psf
return,psf
end
