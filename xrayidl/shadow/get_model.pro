pro get_model,nhd,filter,t_flx,nhback=nhback,etlow=etlow, $
  op_limit=op_limit,bin_size=bin_size,minnorm=minnorm,xflux=xflux
;** Subroutine for sh_600045 script to create the theoretical flux
;** map given the values for the two-component x-ray background
;** and the IRAS absorption image nhd.
nh_sub = nhd-nhback
opac,(minnorm*nh_sub)/100.,image_op,0,quad=0,filter=filter
sel = where(filter gt etlow and -image_op lt op_limit,nsel)
op_array = fltarr(32,32)
op_array(sel) = -image_op(sel)
 
nhsel = where(filter gt 0.)
n_size = 32
t_flx = fltarr(n_size,n_size)
t_flx(nhsel) = xflux(0) + xflux(1)*exp(-minnorm*op_array(nhsel))
t_flx = t_flx*(60./(bin_size*.5))^2
  ;** theoretical image.

return
end
