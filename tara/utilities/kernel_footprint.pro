;;;===========================================================================
;;; $Id: kernel_footprint.pro 1764 2003-05-22 13:22:54Z patb $
;;; Routine to compute the 1-D indexes into a vector or image image for the 
;;; pixels that fall under the specified kernel.

;;; Initialize with 
;;;   kernel_footprint, this, /CREATE, IMAGE=image, RADII=indgen(10)
;;; or
;;;   kernel_footprint, this, /CREATE, IMAGE=image, KERNELS=kernels
;;; where "kernels" is a 3-D array of kernel images.

;;; To retrieve the indexes of the pixels and weights that fall under
;;; a specified kernel call like this:
;;;   kernel_footprint, this, xcen, ycen, kernel_index, $
;;;			pixel_list, kernel_values
;;; The kernel values are normalized to sum to 1.0

;;; Destroy heap variables with
;;;   kernel_footprint, this, /DESTROY
;;;===========================================================================

PRO kernel_footprint, this, CREATE=create, IMAGE=image, $
		      RADII=radii, KERNELS=kernels, $ 
		      xcen, ycen, kernel_index, pixel_list, kernel_values,$
		      DESTROY=destroy

if keyword_set(create) then begin
  image_xdim = (size( image, /DIMEN ))[0]
  if (size(image, /N_DIM) EQ 2) then begin
    image_ydim = (size( image, /DIMEN ))[1]
  endif else begin
    image_ydim = 1
  endelse

  if keyword_set(radii) then begin
    num_kernels = n_elements(radii)
    max_radius =  ceil(max(radii))
    kernel_dim =  1 + 2*max_radius

    if (image_ydim EQ 1) then begin
      distances  = shift( dist(kernel_dim,1), max_radius)

      kernels = fltarr(kernel_dim,1,num_kernels)
    endif else begin
      distances  = shift( dist(kernel_dim), max_radius, max_radius)

      kernels = fltarr(kernel_dim,kernel_dim,num_kernels)
    endelse

    for ii = 0,num_kernels-1 do begin
      kernels[*,*,ii] = (distances LE radii[ii])
    endfor

    kernel_footprint, this, /CREATE, IMAGE=image, KERNELS=kernels
    return
  endif
  

  ;; Examine the dimensions of the stack of square kernel arrays.
  kernel_xdim = (size(kernels, /DIM))[0]
  kernel_ydim = (size(kernels, /DIM))[1]
  
  kernel_xdim_half = (kernel_xdim-1)/2
  kernel_ydim_half = (kernel_ydim-1)/2

  if ((kernel_xdim MOD 2) NE 1) OR ((kernel_ydim MOD 2) NE 1) then $
    message, 'ERROR: dimensions of kernel image must be odd.'


  if (size(kernels, /N_DIM) EQ 3) then begin
    num_kernels = (size(kernels, /DIM))[2]
  endif else begin
    num_kernels = 1
  endelse  

  p_x_index     = ptrarr(num_kernels, /ALLOC)
  p_y_index     = ptrarr(num_kernels, /ALLOC)
  p_kernel_vals = ptrarr(num_kernels, /ALLOC)

  ;; Record the location of the members of each kernel as 
  ;; (delta_x, delta_y) offsets from the center pixel.
  index_center_pix = kernel_ydim_half*kernel_xdim + kernel_xdim_half 

  for ii = 0,num_kernels-1 do begin
    ;; Find the non-zero kernel pixels.
    kernel = kernels[*,*,ii]
    index_1d = where(kernel NE 0)

    ;; Put the central pixel first in the lists.
    s = where(index_1d EQ index_center_pix)
    temp        = index_1d[s]
    index_1d[s] = index_1d[0]
    index_1d[0] = temp

    *p_x_index  [ii] = (index_1d mod kernel_xdim) - kernel_xdim_half 
    *p_y_index  [ii] = (index_1d  /  kernel_xdim) - kernel_ydim_half
    *p_kernel_vals[ii] = kernel[index_1d]
  endfor

  ;; Save everything we'll need for later calls.
  this = {image_xdim:image_xdim, image_ydim:image_ydim, $
  	   p_x_index:p_x_index,   p_y_index:p_y_index, $
			      p_kernel_vals:p_kernel_vals}
  return
endif


if keyword_set(destroy) then begin
  ptr_free, this.p_x_index, this.p_y_index, this.p_kernel_vals
  return
endif


x_index = xcen + (*this.p_x_index  [kernel_index])
y_index = ycen + (*this.p_y_index  [kernel_index])

;; Find pixels that are in bounds of image.
s = where((0 LE x_index) AND (x_index LT this.image_xdim) AND $
	  (0 LE y_index) AND (y_index LT this.image_ydim))

pixel_list = y_index[s] * this.image_xdim + x_index[s]

if arg_present(kernel_values) then begin
  ;; Retrieve the corresponding kernel values and normalize to 1.0
  kernel_values = (*this.p_kernel_vals[kernel_index])[s]
  kernel_values = kernel_values / total(kernel_values)
endif
return
end


PRO test
image=bytarr(10,10)
vector = image[*,0]
radii = [0,1,2,4]
num_kernels      = n_elements(radii)
max_radius = 4
kernel_dim = 1 + 2*max_radius
kernels    = fltarr(kernel_dim,kernel_dim,num_kernels)

distances  = shift( dist(kernel_dim), max_radius, max_radius)

for ii = 0,num_kernels-1 do begin
  mask   = (distances LE radii[ii])
  
  kernels[*,*,ii] = mask * distances
endfor

kernels[max_radius,max_radius,*] = 0.1

for ii = 0,num_kernels-1 do begin
  print
  print, kernels[*,*,ii], F='(9F4.1)'
endfor
print

kernel_footprint, k2d, /CREATE, IMAGE=image, KERNELS=kernels
kernel_footprint, k1d, /CREATE, IMAGE=vector, KERNELS=kernels


kernel_footprint, k2d, 4, 3, 2, s, kernel_values
im=image
im[s]=kernel_values/min(kernel_values) & print,im & print

kernel_footprint, k2d, 0, 0, 0, s, kernel_values
im=image
im[s]=kernel_values/min(kernel_values) & print,im & print

kernel_footprint, k2d, 0, 0, 1, s, kernel_values
im=image
im[s]=kernel_values/min(kernel_values) & print,im & print

kernel_footprint, k2d, 0, 0, 2, s, kernel_values
im=image
im[s]=kernel_values/min(kernel_values) & print,im & print

kernel_footprint, k2d, 0, 9, 0, s, kernel_values
im=image
im[s]=kernel_values/min(kernel_values) & print,im & print

kernel_footprint, k2d, 0, 9, 1, s, kernel_values
im=image
im[s]=kernel_values/min(kernel_values) & print,im & print

kernel_footprint, k2d, 0, 9, 2, s, kernel_values
im=image
im[s]=kernel_values/min(kernel_values) & print,im & print

kernel_footprint, k2d, /DESTROY


print, '1D TEST CASES'
for ii = 0,num_kernels-1 do begin
  kernel_footprint, k1d, 0, 0, ii, s, kernel_values
  ve=vector
  ve[s]=kernel_values/min(kernel_values) & print,ve & print
endfor

im=fltarr(201,201)
kernel_footprint, k2d, /CREATE, IM=im, RAD=[90]
kernel_footprint, k2d, 100, 100, 0, s, kernel_values
im[s]=kernel_values & tvscl,im
kernel_footprint, k2d, /DESTROY


return
end
