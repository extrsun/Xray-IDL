PRO maxent, img_name, psf_name, Niter, maxent

; Make sure data is real-valued.
rawim= double( readfits(img_name) )
help, rawim
print, total(rawim)

tit=string(img_name,psf_name,f='(A," maxent deconvolved by ",A)')
function_2d,id,rawim, DATASET_NAME='rawim',WIDGET_TITLE=tit,/UNITY_ASPECT

; Normalize PSF.
psf=readfits(psf_name)
psf=psf / total(psf, /DOUBLE)

maxent     =0
multipliers=0.
psf_ft     =0

for ii=0,Niter-1 do begin
  max_entropy,rawim,psf,maxent,multipliers,FT_PSF=psf_ft
  if ((ii mod ceil(Niter/28.)) EQ 0) then $
    function_2d,id,maxent, DATASET_NAME=string(ii)
endfor
function_2d,id,maxent, DATASET_NAME=string(ii)
return
end
