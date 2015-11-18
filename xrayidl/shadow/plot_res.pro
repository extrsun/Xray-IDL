pro plot_res,xflux,minnorm,nhd,npb,ba,et0,fsel,irh,binsize=binsize, $
  etlow=etlow,op_limit=op_limit,levels=levels,opfile=opfile
;** Plot residual between theoretical flux image (created by using the
;** fit from f_ab.pro) and the real flux image.
if n_elements(op_limit) eq 0 then op_limit = 1.e22
if n_elements(etlow) eq 0 then etlow = 800.
nh_sub = nhd-ba
opac,(minnorm*nh_sub)/100.,image_op,0,quad=0,filter = et0,opfile=opfile
if n_elements(fsel) eq 0 then begin
  sel = where(et0 gt etlow and image_op lt op_limit,nsel)
endif else sel = fsel
op_array = fltarr(32,32)
op_array(sel) = -image_op(sel)

image_av,10,npb,ba,et0,flx,bin_size=30*16
nhsel = where(et0 gt 0.)
flx(where(et0 le 0.)) = 0.
n_size = 32
t_flx = fltarr(n_size,n_size)
t_flx(nhsel) = xflux(0) + xflux(1)*exp(-minnorm*op_array(nhsel))
  ;** theoretical image.
r_flx = abs(flx - t_flx/(4.^2)) ;** residual image
cont_grey,nhd,irh,r_flx,type=1,levels=levels
print,'Minmax of residual image = ',minmax(r_flx(sel))
stop
return
end
