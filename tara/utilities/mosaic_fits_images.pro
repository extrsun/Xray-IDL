;;; $Id: mosaic_fits_images.pro 3258 2008-11-18 18:18:38Z patb $
;;; Program to mosaic FITS images.
;;; Patrick Broos, 2001
;;;
;;; Use BLANKVAL to specify the pixel value to use outside the field.
;;;
;;; Use /USE_MAX_FOR_BLANKVAL to use the max pixel value in the mosaic for
;;; the off-field areas.
;;;
;;; Use NORMALIZATION to supply normalization values (e.g. exposure times).
;;;
;;; Use MASK_FILENAMES and optionally MASK_THRESHOLDS to specify a series of
;;; FITS images that are thresholded to define the field of view of each image.
;;;
;;; If no masks are supplied, you may specify with INTERP the type of 
;;; interpolation the HASTROM routine will use.

PRO mosaic_fits_images, image_filenames, mosaic_filename, $
			INTERP=interp, $
			BLANKVAL=blankval, USE_MAX_FOR_BLANKVAL=use_max_for_blankval,$
			NORMALIZATION=normalization, $
			MASK_FILENAMES=mask_filenames_p, $
			MASK_THRESHOLDS=mask_thresholds_p
    
print    
print, 'FYI, the tool "montage", available on the web, is a professional tool addressing the mosaic problem!'
print, 'http://hachi.ipac.caltech.edu:8080/montage/'
print

if (!version.release EQ '5.5') OR (!version.release EQ '5.6') then begin
  print, 'WARNING! This version of IDL contains a bug in poly_2d which prevents hastrom.pro from working correctly!'
endif

nullval = -1000
num_images = n_elements(image_filenames)

if (n_elements(interp) NE 1)          then interp=0
if (n_elements(blankval) NE 1)        then blankval=-1

mask_thresholds=replicate(0,num_images)
mask_filenames=replicate('',num_images)
if (n_elements(mask_thresholds_p) GT 0) then $
  mask_thresholds[0]=mask_thresholds_p

if (n_elements(mask_filenames_p ) GT 0) then begin
  mask_filenames [0]=mask_filenames_p

  interp=0
  print, 'HASTROM interpolation set to zero (nearest neighbor) since masks are supplied.'
endif

img_ptr  = ptrarr(num_images, /ALLOC)
head_ptr = ptrarr(num_images, /ALLOC)
astr_ptr = ptrarr(num_images, /ALLOC)

;; Read in images and apply optional masks.
for ii=0,num_images-1 do begin
  fn  = image_filenames[ii]
  img = readfits(fn, header)
  
  ;; Apply optional normalization.
  if keyword_set(normalization) then $
    img = img / float(normalization[ii])
  
  ;; Apply optional mask.
  if keyword_set(mask_filenames[ii]) then begin
    mask = readfits(mask_filenames[ii])
    
    img_size = size(img, /DIM)
    msk_size = size(mask, /DIM)
    if (img_size[0] NE msk_size[0]) OR (img_size[1] NE msk_size[1]) then $
      message, 'Image and mask do not match!'
    
    s = where(mask LE mask_thresholds[ii], count)
    if (count GT 0) then begin
      if (ii EQ 0) then img[s] = blankval $
      		   else img[s] = nullval
      print, count, ' pixels masked in ', fn
    endif
  endif
  
  *(img_ptr[ii])  = img
  *(head_ptr[ii]) = header
  
  extast, header, astrometry, success
  if (success EQ -1) then $
    message, 'No astrometry found in ', fn
    
  *(astr_ptr[ii]) = astrometry
endfor


;; Find the range of the slave images in the zero-based pixel coordinate 
;; system of the master, which is the first image.
head_master = *(head_ptr[0])
img_master  = *(img_ptr[0])
astr_master = *(astr_ptr[0])

xmin = 0  & ymin = 0
xmax = fxpar(head_master, 'NAXIS1')
ymax = fxpar(head_master, 'NAXIS2')

for ii=1,num_images-1 do begin
  ;; Convert LL corner to RA,DEC.
  xy2ad, 0, 0, *(astr_ptr[ii]), ra, dec
  
  ;; Convert RA,DEC to pixel system of master.
  ad2xy, ra, dec, astr_master, x, y
  print, 'LL corner:', x, y
  xmin = min([xmin,x])
  ymin = min([ymin,y])

  ;; Convert UR corner to RA,DEC.
  xy2ad, fxpar(*(head_ptr[ii]), 'NAXIS1') - 1, $
         fxpar(*(head_ptr[ii]), 'NAXIS2') - 1, *(astr_ptr[ii]), ra, dec
  
  ;; Convert RA,DEC to pixel system of master.
  ad2xy, ra, dec, astr_master, x, y
  print, 'UR corner:', x, y

  xmax = max([xmax,x])
  ymax = max([ymax,y])
endfor


;; Create a mosaic header that declares an image big enough to encompass 
;; everything.
print,'Range of the scene in master pixel coordinate system:'
print, 'X:', xmin, xmax
print, 'Y:', ymin, ymax

head_template = head_master

; Make the mosaic dimensions divisible by 4 in case we want to rebin.
fxaddpar, head_template, 'NAXIS1', 4*ceil((1+xmax-xmin)/4.)
fxaddpar, head_template, 'NAXIS2', 4*ceil((1+ymax-ymin)/4.)

fxaddpar, head_template, 'CRPIX1', fxpar(head_template,'CRPIX1') - xmin
fxaddpar, head_template, 'CRPIX2', fxpar(head_template,'CRPIX2') - ymin

;; Resample master image (first one) to start the mosaic, filling in zeros
;; outside the field.
print, 'Using HASTROM to resample images...'
hastrom, img_master, head_master, head_template, MISSING=blankval, INTERP=interp

;; Resample all the other images, marking the areas outside the field.
for ii=1,num_images-1 do begin
  hastrom, *(img_ptr[ii]), *(head_ptr[ii]), head_template, $
  	   MISSING=nullval, INTERP=interp
endfor


;; Combine the mosaic components.
print, 'Combining mosaic components...'
help, img_master
for ii=1,num_images-1 do begin
  help, *(img_ptr[ii])
  s=where(*(img_ptr[ii]) NE nullval, count)

  if (count EQ 0) then begin
    print, image_filenames[ii] + ' is completely off the field.'
  endif else begin
    img_master[s] = (*(img_ptr[ii]))[s]

    print, n_elements(*(img_ptr[ii]))-count, ' pixels off field in '+ image_filenames[ii]
  endelse
endfor


if keyword_set(use_max_for_blankval) then begin
  off_field = where(img_master EQ blankval, count, COMPLEMENT=on_field)
  maxval = max(img_master[on_field])
  if (count GT 0) then img_master[off_field] = maxval
  blankval = maxval
endif


;; For integer images, record BLANKVAL in the appropriate keyword.
if (fxpar(head_master, 'BITPIX') GT 0) then $
  fxaddpar, head_master, 'BLANK', blankval

;; Write out mosaic.
writefits, mosaic_filename, img_master, head_master

ptr_free, img_ptr, head_ptr, astr_ptr
return
end
