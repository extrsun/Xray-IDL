;;; Simple excerpt from true_color_image.pro to allow direct printing of RGB image.
;;; $Id: rgb_print.pro 1780 2003-06-18 15:27:48Z patb $
;;; Patrick Broos, 2003

PRO rgb_print, red_image, green_image, blue_image, filename, COLOR_MODEL=color_model, TVLOW=tvlow, TVHIGH=tvhigh

if NOT keyword_set(color_model) then color_model=0

if NOT keyword_set(tvlow)  then tvlow =[min(red_image), min(green_image), min(blue_image)]
if NOT keyword_set(tvhigh) then tvhigh=[max(red_image), max(green_image), max(blue_image)]


  ;; Express the value of each component image in terms of a number normalized
  ;; to the desired scaling range (0.0<--> tvlow, 1.0<-->tvhigh).
  red_norm = ((red_image   - tvlow[0]) / float(tvhigh[0] - tvlow[0])) > 0
  grn_norm = ((green_image - tvlow[1]) / float(tvhigh[1] - tvlow[1])) > 0
  blu_norm = ((blue_image  - tvlow[2]) / float(tvhigh[2] - tvlow[2])) > 0

  msg = string(tvlow[0], tvhigh[0], $
          f='("Red [",G10.4," to ",G10.4,"] ")' )
     
  msg = msg + string(tvlow[1], tvhigh[1], $
        f='("Green [",G10.4," to ",G10.4,"] ")' )
 
  msg = msg + string(tvlow[2], tvhigh[2], $
        f='("Blue [",G10.4," to ",G10.4,"]")' )
 print, msg

  ;; For each pixel, compute the brightest component.
  ;; Then, all pixels with a component greater than 1.0 need to be clipped in
  ;; such a way that their hue (ratio of components) is preserved.
  ;; Simply clipping each component at 1.0 is WRONG!  That leads to white
  ;; pixels in areas where red, green, & blue are NOT equal strength but simply
  ;; all three clipped.
  max_norm = red_norm > grn_norm > blu_norm
  s = where( max_norm GT 1.0, count )
  if (count GT 0) then begin
    print, count, ' pixels are saturated'
    red_norm[s] = red_norm[s] / max_norm[s]
    grn_norm[s] = grn_norm[s] / max_norm[s]
    blu_norm[s] = blu_norm[s] / max_norm[s]
  endif
 
  case color_model of
   ;Luminous Model
   0:
  
   ;; In the subtractive models when a component is strong (~1) we get that
   ;; color to "show through" white by suppressing the other two components.
   ;; When all 3 components are strong we get black, and when all 3 are
   ;; weak we get white.
   
   ; Subtract average of two components.  Bright Red, Green, Blue not possible.
   1: begin
    gb = grn_norm+blu_norm
    rb = red_norm+blu_norm
    rg = red_norm+grn_norm
    scale = 2.0
    red_norm = 1.0 - (gb)/scale
    grn_norm = 1.0 - (rb)/scale
    blu_norm = 1.0 - (rg)/scale
    end
   
   ; Subtract sum of two components normalized to stay inside unit cube.
   ; Some colors still not possible, e.g. yellow.
   2: begin
    gb = grn_norm+blu_norm
    rb = red_norm+blu_norm
    rg = red_norm+grn_norm
    scale = 1 > gb > rb > rg
    red_norm = 1.0 - (gb)/scale
    grn_norm = 1.0 - (rb)/scale
    blu_norm = 1.0 - (rg)/scale
    end
   
   ; Simply invert each component to get Cyan, Magenta, Yellow system.
   ; Not very intuitive, e.g. yellow result means ONE data component dominates.
   3: begin     
    red_norm = 1.0 - red_norm
    grn_norm = 1.0 - grn_norm
    blu_norm = 1.0 - blu_norm
    end
  endcase

  ;; Finally we scale up to rgb values in the range 0..255
  red_img = round( 255 * red_norm ) > 0
  grn_img = round( 255 * grn_norm ) > 0
  blu_img = round( 255 * blu_norm ) > 0


color_manager, /PS_TRUE, NCOLORS=ncolors, $
		     FILENAME=filename, ENCAPSULATED=0, PREVIEW=0, $
		     LANDSCAPE=0, BITS_PER_PIXEL=8

tv, fix([[[red_img]], [[grn_img]], [[blu_img]]] * (ncolors/256.0)), TRUE=3

device, /CLOSE
color_manager, /X_PSEUDO
return
end
