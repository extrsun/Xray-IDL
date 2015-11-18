PRO WVT_LIST_TO_IMAGE, image, list, x, y, dim=dim, mask=mask

IF NOT(keyword_set(dim)) THEN dim=[max(x)+1,max(y)+1]
npix=long(n_elements(x))
image=dblarr(dim[0],dim[1])
mask=intarr(dim[0],dim[1])

For i=0L,npix-1L DO BEGIN
  image(x[i],y[i])=list[i]
  mask(x[i],y[i])=1
Endfor

END
