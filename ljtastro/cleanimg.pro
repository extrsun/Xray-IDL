pro cleanimg,imagename,threshold=threshold,outimg=outimg

image=mrdfits(imagename,0,head)

nx=n_elements(image[*,0])
ny=n_elements(image[0,*])
for i=0,nx-1 do begin
for j=0,ny-1 do begin
if image[i,j] le threshold[0] or image[i,j] ge threshold[1] then begin
image[i,j]=0.
endif
endfor
endfor

writefits,outimg,image,head

end
