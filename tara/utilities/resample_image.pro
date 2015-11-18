;==========================================================================
;;; Image Resampling Routine: $Id: resample_image.pro 2095 2004-11-10 11:15:26Z patb $
;;;
;;; We are given an image (xdim X ydim array) whose placement in an X/Y
;;; coordinate system is uniquely defined by the coordinates (X0,Y0) of the
;;; CENTER of pixel (0,0), and by the dimensions of the pixels,
;;; delta_x X delta_y.  We wish to resample this image to form another such
;;; image whose origin is at a different coordinate (X0',Y0'), whose pixels
;;; have a different size, delta_x' X delta_y', and whose dimensions are
;;; different, xdim' X ydim'.
;;; 
;;; We also return two vectors containing the indexes of pixels in the new
;;; array that fell outside the old array boundaries (bgrnd_index) and the
;;; indexes of pixels that fell inside the old array (fgrnd_index).  If
;;; there are no background pixels then bgrnd_index == -1.  If the old and
;;; new arrays don't overlap then fgrnd_index == -1.
;;; 
;;; If the keyword BACKGROUND_VALUE is supplied, then background pixels are
;;; set to that value.
;;; If the keyword SCREEN_NAN is supplied, then NaN values are flagged as
;;; background pixels.
;==========================================================================
PRO resample_image, x0,   y0,   delta_x,   delta_y,   array, $
		    x0_p, y0_p, delta_x_p, delta_y_p, $
		    xdim_p, ydim_p, array_p, COL=col, ROW=row, $
		    bgrnd_index, fgrnd_index, $
		    BACKGROUND_VALUE=background_value, SCREEN_NAN=screen_nan
		    

;; Compute the coordinates of the CENTERS of the pixels in the new array.
center_x = x0_p + (findgen(xdim_p) * delta_x_p)
center_y = y0_p + (findgen(ydim_p) * delta_y_p)


;; Compute the column & row indexes in the original array under which those 
;; center positions fall.
make_2d, round((center_x - x0) / delta_x), round((center_y - y0) / delta_y), $
	 col, row


;; Determine which of those (col,row) positions fall within the original
;; array and which fall outside.
dims        = size(array, /DIM)

is_fgrnd    = col GE 0 AND col LT dims[0] AND row GE 0 AND row LT dims[1]

fgrnd_index = where(is_fgrnd, COMPLEMENT=bgrnd_index, NCOMPLEMENT=bgrnd_count)


;; Resample the input array, after adjusting the col & row values 
;; of all the background pixels so they get the value array[0,0] 
if (bgrnd_count GT 0) then begin
  col[bgrnd_index] = 0
  row[bgrnd_index] = 0
endif
 
array_p = array[col,row]

;; If desired, mark NaN values are background.
if keyword_set(screen_nan) then begin
  nan_index = where(finite(array_p) EQ 0, nan_count)
  if (nan_count GT 0) then begin
    is_fgrnd[nan_index] = 0B
    fgrnd_index = where(is_fgrnd, COMPLEMENT=bgrnd_index, NCOMPLEMENT=bgrnd_count)
  endif
endif

;; Finally, if desired, set background pixels to the specified value.
if ((bgrnd_count GT 0) AND (n_elements(background_value) EQ 1)) then $
  array_p[bgrnd_index] = background_value


return
end


PRO resample_image_test

x0=10 & y0=10 & delta_x=1 & delta_y=1
array = lindgen(300,400)
x0_p=0 & y0_p=0 & delta_x_p=0.8 & delta_y_p=0.8 & xdim_p=300 & ydim_p=400

t=systime(1)
for i=0,9 do $
 resample_image, x0,   y0,   delta_x,   delta_y,   array, $
		    x0_p, y0_p, delta_x_p, delta_y_p, $
		    xdim_p, ydim_p, array_p, bgrnd_index, fgrnd_index, $
		    BACKGROUND_VALUE=0, /SCREEN_NAN

print, systime(1)-t
return
end
