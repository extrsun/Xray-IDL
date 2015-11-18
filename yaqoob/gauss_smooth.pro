function gauss_smooth,im,sigma,center=center,kern=kern

	if n_params(0) eq 0 then begin
	  print,'smoothed_im = gaus_smooth(im,sigma)'
	  print,'Smooth im with a gaussian of width sigma'
	  return,0
	endif
        sz = size(im)
        if n_elements(center) eq 0 then center=1
	boxsz = fix(sigma*5+0.5)+1
	kern = fltarr(boxsz,boxsz)
;	norm = 1/(2*!pi*sigma^2)
	midl = (boxsz-1)/2.
	for x = 0,boxsz-1 do for y = 0,boxsz-1 do $
	  kern(x,y) = exp((-(x-midl)^2-(y-midl)^2)/(2*sigma^2))
	kern = kern/total(kern)
; Continue convolution to edge of image by adding boxsz buffer around im
        temp = fltarr(sz(1)+2*boxsz,sz(2)+2*boxsz)
        temp(boxsz:boxsz+sz(1)-1, boxsz:boxsz+sz(2)-1) = im
        temp = convol(temp,kern,center=center)
	return,temp(boxsz:boxsz+sz(1)-1, boxsz:boxsz+sz(2)-1)
	end

