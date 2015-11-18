pro image_comb,image_out,image0,image1,image2
nimage=n_params()-1
sz=size(image0)
image_out=fltarr(sz(1),sz(2),nimage)
image_out(*,*,0)=image0
image_out(*,*,1)=image1
if nimage eq 2 then return
image_out(*,*,2)=image2
return
end
