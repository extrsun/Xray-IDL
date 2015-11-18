;;; Convert HSV color model to RGB.
;;; $Id: tara_hsv2rgb.pro 1997 2004-04-29 12:49:47Z patb $
;;; Patrick Broos, 2004
;;;
;;; Input Parameters:
;;;   hue_norm: Normalized Hue in range [0..1]
;;;   sat_norm: Normalized Saturation in range [0..1]
;;;   value_data: Value (brightness) information, >=0
;;;   blue2red_hue: If TRUE Hue=0 is blue and Hue=1 is red.
;;;
;;; Output Parameters red_data, grn_data, blu_data have the same range as value_data

PRO tara_hsv2rgb, hue_norm, sat_norm, value_data, blue2red_hue, $
                  red_data, grn_data, blu_data

;; Scale hue to the range [min_hue ... 359,0, ... max_hue]
min_hue   = 320
max_hue   = 280
range_hue = (max_hue-min_hue+360) mod 360

hue_norm = 0.0 > hue_norm < 1.0
sat_norm = 0.0 > sat_norm < 1.0

  if (blue2red_hue EQ 0) then begin
    ; Increasing hue values
    hue_img  = min_hue + range_hue * hue_norm
  endif else begin
    ; Decreasing hue values
    hue_img  = max_hue - range_hue * hue_norm 
  endelse
  
  hue_img   = (hue_img + 360) mod 360
  hue_group = floor(hue_img/120)
  
  ;; Construct RGB data images which have the desired hue, saturation=1,
  ;;  and have a "value" equal to *axis2.image.
  ;; Equations derived from http://astronomy.swin.edu.au/~pbourke/colour/hsv/
  xdim = (size( hue_img, /DIMEN ))[0]
  ydim = (size( hue_img, /N_DIM ) EQ 2) ? (size( hue_img, /DIMEN ))[1] : 1
  red_data = reform(fltarr(xdim,ydim))
  grn_data = reform(fltarr(xdim,ydim))
  blu_data = reform(fltarr(xdim,ydim))

  ind = where(hue_group EQ 0, count)
  if (count GT 0) then begin
    temp = hue_img[ind] / 60.
    red_data[ind] = 2. - temp
    grn_data[ind] =      temp
  endif 
  
  ind = where(hue_group EQ 1, count)
  if (count GT 0) then begin
    temp = hue_img[ind] / 60.
    grn_data[ind] = 4. - temp
    blu_data[ind] = temp - 2.
  endif 
  
  ind = where(hue_group EQ 2, count)
  if (count GT 0) then begin
    temp = hue_img[ind] / 60.
    red_data[ind] = temp - 4.
    blu_data[ind] = 6. - temp
  endif
   
  red_data   = (1 - sat_norm + sat_norm * (red_data<1)) * value_data
  grn_data   = (1 - sat_norm + sat_norm * (grn_data<1)) * value_data
  blu_data   = (1 - sat_norm + sat_norm * (blu_data<1)) * value_data
  
return
end
