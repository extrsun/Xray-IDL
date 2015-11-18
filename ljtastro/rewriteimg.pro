pro rewriteimg

file="/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/figures/paraimg/nochange/singleparaimg/PwrId_img.fits"
outputfilename="/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/work2/PwrId_Sub2.5.fits"
toolandtime='TOOL  :rewriteimg.pro-IDL,  2006-04-01   '
xrange=512
yrange=512
subvalue=2.5
savenumber=0


image=mrdfits(file,0,fitshead)
for i=0,xrange-1 do begin
for j=0,yrange-1 do begin
if image[i,j] ne 0 then begin
image[i,j]=image[i,j]-subvalue
endif
endfor
endfor

if savenumber then begin
 
 fxaddpar, fitshead, 'HISTORY', toolandtime
 mwrfits,image,outputfilename,fitshead

endif

window,1,retain=2
tvscl,image

end
