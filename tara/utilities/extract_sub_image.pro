;+
;========================================================================
;;;
;;; FILE NAME:    $Id: extract_sub_image.pro 284 1997-04-14 16:11:34Z patb $
;;;
;;; DESCRIPTION:  Routine to extract a sub-image from a 2-D image,
;;;               returning both the extracted 2-D array and the 1-D
;;;               indexes (in the full array) of those elements. 
;;;
;;;               The parameters x_range and y_range are of the form [min,max].
;;;               The specified rectangular region may lie PARTIALLY outside
;;;               the boundaries of the image.
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1996, Pennsylvania State University
;;;
;;; NOTES:        
;;;
;-
;==========================================================================
FUNCTION extract_sub_image, image, x_range, y_range, indexes

min_x = x_range(0) &  max_x = x_range(1)
min_y = y_range(0) &  max_y = y_range(1)

S = size(image)
x_dim = S(1)  &  y_dim = S(2)

temp = lindgen( n_elements(image) )
x_coord = temp mod x_dim
y_coord = temp  /  x_dim
temp = 0

indexes = where( (x_coord GE min_x) AND (x_coord LE max_x) AND $
		 (y_coord GE min_y) AND (y_coord LE max_y), count )

if (count EQ 0) then message, 'NULL sub-image requested!'

x_dim_subimage = 1 + ((max_x < (x_dim-1)) - (0 > min_x))
y_dim_subimage = 1 + ((max_y < (y_dim-1)) - (0 > min_y))

return, reform( image( indexes ), x_dim_subimage, y_dim_subimage )
end

