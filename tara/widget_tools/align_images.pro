;+
;========================================================================
;;;
;;; Align Images Widget: $Id: align_images.pro 1326 2001-01-29 15:36:44Z patb $
;;;
;;; Copyright (C) 2000, Pennsylvania State University
;;; Pat Broos (patb@astro.psu.edu)
;;;
;========================================================================
;;; DESCRIPTION:  
;;; This tool searches for images in all existing dataset_2d and 
;;; dataset_3d widgets, aligns them on a common grid, and allows the
;;; user to combine them, e.g. add, subtract, divide, spawn 
;;; true_color_image tool, etc.
;;;
;========================================================================
;;; INTERFACE TO CLIENT
;;;
;;; align_images
;-

;==========================================================================
;;; img_list is a vector of "image" structures.
;;; If crop==1 then result region is the intersection of images, 
;;; else result region is the union (pad with zeros).
;;; (x0,y0) are the coordinates of the CENTER of pixel (0,0).
;;; Pixels are of size delta_x X delta_y.
;;; array_ptrs is a vector of pointers to identically-sized 2D arrays.
PRO AlignImagesRegister, img_list, crop, $
			 array_ptrs, x0_p, y0_p, delta_x_p, delta_y_p

num_img = n_elements(img_list)

;; Use the smallest bin sizes for the registered images.
delta_x_p = float(min( img_list.delta_x ))
delta_y_p = float(min( img_list.delta_y ))


;; Compute the bounds of the region each image covers.
left_edges  = fltarr(num_img)
right_edges = fltarr(num_img)
bot_edges   = fltarr(num_img)
top_edges   = fltarr(num_img)
for ii=0,num_img-1 do begin
  img = img_list[ii]
  
  left_edges[ii]  = img.x0 - img.delta_x/2.0
  bot_edges [ii]  = img.y0 - img.delta_y/2.0
  
  xdim = (size(*(img.array), /DIM))[0]
  ydim = (size(*(img.array), /DIM))[1]
    
  right_edges[ii] = left_edges[ii] + xdim * img.delta_x
  top_edges  [ii] = bot_edges [ii] + ydim * img.delta_y
endfor


;; Choose the bounds of the desired region.
if (crop) then begin
  left_edge  = max( left_edges )
  bot_edge   = max( bot_edges )
  
  right_edge = min( right_edges )
  top_edge   = min( top_edges )
endif else begin
  left_edge  = min( left_edges )
  bot_edge   = min( bot_edges )
  
  right_edge = max( right_edges )
  top_edge   = max( top_edges )
endelse


;; Compute the specs for the array that cover the desired region with
;; the desired pixel size.
x0_p   = left_edge + delta_x_p/2.0
y0_p   = bot_edge  + delta_y_p/2.0

xdim_p = round((right_edge-left_edge)/delta_x_p) > 2
ydim_p = round(  (top_edge-bot_edge )/delta_y_p) > 2


;; Resample each input array to the new specs.
array_ptrs = ptrarr(num_img)
for ii=0,num_img-1 do begin
  img = img_list[ii]
  array = *img.array

  ; Remove NaN's.
  index = where(0 EQ finite(array), count)
  if (count GT 0) then begin
    array[index] = 0
  endif
  
  resample_image, img.x0, img.y0, img.delta_x,   img.delta_y,  array, $
		      x0_p,   y0_p,   delta_x_p,     delta_y_p, $
		  xdim_p, ydim_p, array_p, BACKGROUND_VALUE=0
  
  array_ptrs[ii] = ptr_new(array_p, /NO_COPY)
endfor
 
return
end


;==========================================================================
PRO AlignImagesEvent, event

COMMON align_images, f_2d

widget_control, /HOURGLASS
DestroyFlag = 0

;; Get the state structure. 
top_base = Event.handler
widget_control, top_base, GET_UVALUE=st

;; Process the event.
case Event.ID of

;--------------------------------------------------------------------------
; Algorithm
 (*st).mode: $
  begin
  label = ['A - B','A / B','A + B','Hue:A, Brightness:B','Red:A, Green:B, Blue:C']
  widget_control, (*st).mode_label, SET_VALUE=label[Event.index]
  end
  
;--------------------------------------------------------------------------
; Ok button
 (*st).ok_button: $
  begin
  mode  = widget_info( (*st).mode, /DROPLIST_SELECT )
  crop  = widget_info( (*st).crop, /DROPLIST_SELECT )

  img1 = (*st).images[widget_info( (*st).img1, /DROPLIST_SELECT )]
  img2 = (*st).images[widget_info( (*st).img2, /DROPLIST_SELECT )]
  img3 = (*st).images[widget_info( (*st).img3, /DROPLIST_SELECT )]
  
  print, img1.name, img2.name, img3.name
  
  ;; Handle each mode.
  case (mode) of
   ;-- Subtract --
   0:begin
     name = string( img1.name, img2.name, F='("{",A0,"} - {",A0,"}")' )
     AlignImagesRegister, [img1,img2], crop, $
     			  array_ptrs, x0, y0, delta_x, delta_y
     			  
     function_2d, f_2d,  (*array_ptrs[0] - *array_ptrs[1]),$
		  DATASET_NAME=name, $
     		  X0=x0, DELTA_X=delta_x, Y0=y0, DELTA_Y=delta_y, $
     		  XTITLE=(*st).xtitle, YTITLE=(*st).ytitle, $
     		  UNITY_ASPECT=(*st).unity_aspect
     end
     
   ;-- Divide --
   1:begin
     name = string( img1.name, img2.name, F='("{",A0,"} / {",A0,"}")' )
     AlignImagesRegister, [img1,img2], crop, $
     			  array_ptrs, x0, y0, delta_x, delta_y

     ratio = (*array_ptrs[0] / *array_ptrs[1])
     index = where(0 EQ finite(ratio), count)
     if (count GT 0) then begin
       ratio[index] = 0
     endif

     function_2d, f_2d,  ratio,$
		  DATASET_NAME=name, $
     		  X0=x0, DELTA_X=delta_x, Y0=y0, DELTA_Y=delta_y, $
     		  XTITLE=(*st).xtitle, YTITLE=(*st).ytitle, $
     		  UNITY_ASPECT=(*st).unity_aspect
     end

   ;-- Add --
   2:begin
     name = string( img1.name, img2.name, F='("{",A0,"} + {",A0,"}")' )
     AlignImagesRegister, [img1,img2], crop, $
     			  array_ptrs, x0, y0, delta_x, delta_y
     			  
     function_2d, f_2d,  (*array_ptrs[0] + *array_ptrs[1]),$
		  DATASET_NAME=name, $
     		  X0=x0, DELTA_X=delta_x, Y0=y0, DELTA_Y=delta_y, $
     		  XTITLE=(*st).xtitle, YTITLE=(*st).ytitle, $
     		  UNITY_ASPECT=(*st).unity_aspect
     end

   ;-- Hue/Brightness --
   3:begin
     AlignImagesRegister, [img1,img2], crop, $
     			  array_ptrs, x0, y0, delta_x, delta_y
     			  
     true_color_image, tci, *array_ptrs[0], *array_ptrs[1],  $
			DESCRIPTIONS=[img1.name,img2.name], $
			X0=x0, DELTA_X=delta_x, Y0=y0, DELTA_Y=delta_y, $
     		  	XTITLE=(*st).xtitle, YTITLE=(*st).ytitle, $
     		  	UNITY_ASPECT=(*st).unity_aspect
     end

   ;-- RGB --
   4:begin
     AlignImagesRegister, [img1,img2,img3], crop, $
     			  array_ptrs, x0, y0, delta_x, delta_y
     			  
     true_color_image, tci, *array_ptrs[0], *array_ptrs[1], *array_ptrs[2],  $
			DESCRIPTIONS=[img1.name,img2.name,img3.name], $
			X0=x0, DELTA_X=delta_x, Y0=y0, DELTA_Y=delta_y, $
     		  	XTITLE=(*st).xtitle, YTITLE=(*st).ytitle, $
     		  	UNITY_ASPECT=(*st).unity_aspect, /RGB
     end

  endcase
  
  ptr_free, array_ptrs

  DestroyFlag=1
  end
  

  ;--------------------------------------------------------------------------
; Cancel button
 (*st).cancel_button: DestroyFlag=1
  
 else: print, 'unknown event in dataset_2d'
endcase


if DestroyFlag then widget_control, top_base, /DESTROY

return
end


;==========================================================================
PRO align_images, XTITLE=xtitle, YTITLE=ytitle, UNITY_ASPECT=unity_aspect

image      = {name:'', array:ptr_new(), $
	      x0:0.0, delta_x:0.0, y0:0.0, delta_y:0.0}
images     = replicate( image, 100 )
num_images = 0

idlist = widget_info( /MANAGED )

;; See if there are any images (either the current frame if a 3-d density is
;; available, or a statistic map if available) in dataset_3d widgets.
ds_3d  = widget_info(idlist, FIND_BY_UNAME='dataset_3d' )

index = where( ds_3d NE 0, count )
for ii=0,count-1 do begin
  dataset_3d, ds_3d[index[ii]], GET_DATASETS=datasets
    
  sz = size(datasets, /STR)
  if (sz.TYPE EQ 8) then begin
    for jj=0,sz.N_ELEMENTS-1 do begin
      dataset = datasets[jj]
      if (dataset.stat_map.stale EQ 0) then begin
        image.name    = dataset.name + '(ds_3d)'
        image.array   = dataset.stat_map.samples
        image.x0      = dataset.stat_map.x0
        image.y0      = dataset.stat_map.y0
        image.delta_x = dataset.binning.delta_x
        image.delta_y = dataset.binning.delta_y
        images[num_images] = image
        num_images         = num_images + 1
      endif
    endfor
  endif
endfor


;; See if there are any images (2-D densities) in dataset_2d widgets.
ds_2d  = widget_info(idlist, FIND_BY_UNAME='dataset_2d' )

index = where( ds_2d NE 0, count )
for ii=0,count-1 do begin
  dataset_2d, ds_2d[index[ii]], GET_DATASETS=datasets
    
  sz = size(datasets, /STR)
  if (sz.TYPE EQ 8) then begin
    for jj=0,sz.N_ELEMENTS-1 do begin
      dataset = datasets[jj]
      if (dataset.density.stale EQ 0) then begin
        image.name    = dataset.name + '(ds_2d)'
        image.array   = dataset.density.samples
        image.x0      = dataset.density.x0
        image.y0      = dataset.density.y0
        image.delta_x = dataset.binning.delta_x
        image.delta_y = dataset.binning.delta_y
        images[num_images] = image
        num_images         = num_images + 1
      endif
    endfor
  endif
endfor


if (num_images EQ 0) then begin
  msg='No images are available.'
  dummy=dialog_message(msg, /INFO)
  return
endif else begin
  images = images[0:num_images-1] 
endelse

;; Build the widget.
center = ScreenCenter()
xoffset= center(0) - 400

top_base = widget_base( /BASE_ALIGN_CENTER, XOFFSET=xoffset, $
			/COLUMN, /SPACE, XPAD=0, YPAD=0, UNAME='align_images' )

up_base = widget_base( top_base, /ALIGN_LEFT, /ROW, $
			   SPACE=8, XPAD=0, YPAD=0 )

 modes = ['Subtract','Divide','Add','Hue/Brightness','RGB']
 mode = widget_droplist( up_base, VALUE=modes, TITLE='Algorithm:' )
 mode_label = widget_label( up_base, /DYNAMIC_RESIZE, VALUE='A - B' )
 

mid_base = widget_base( top_base, /ROW, $
			   SPACE=8, XPAD=0, YPAD=0, /FRAME )

 img1 = widget_droplist( mid_base, VALUE=images.name, TITLE='A:' )
 img2 = widget_droplist( mid_base, VALUE=images.name, TITLE='B:' )
 img3 = widget_droplist( mid_base, VALUE=images.name, TITLE='C:' )
 
modes = ['Pad to largest component','Crop to smallest component']
crop = widget_droplist( top_base, VALUE=modes, /ALIGN_LEFT )

low_base = widget_base( top_base, /ALIGN_RIGHT, /ROW, $
			   SPACE=10, XPAD=0, YPAD=0 )

 ok_button     = widget_button( low_base, VALUE='   OK   ' )
 cancel_button = widget_button( low_base, VALUE=' CANCEL ' )


state={	mode:mode, mode_label:mode_label, crop:crop, $
	img1:img1, img2:img2, img3:img3, $
	ok_button:ok_button, cancel_button:cancel_button, $
	
	images:images, $
	xtitle:'X', ytitle:'Y', unity_aspect:1 }
	
if (0 NE n_elements(xtitle))       then state.xtitle = xtitle
if (0 NE n_elements(ytitle))       then state.ytitle = ytitle
if (0 NE n_elements(unity_aspect)) then state.unity_aspect = unity_aspect

;; Save state structure.
widget_control, top_base, SET_UVALUE=ptr_new(state, /NO_COPY)

widget_control, top_base, /REALIZE
xmanager, 'align_images', top_base, /NO_BLOCK, /JUST_REG,$
          EVENT_HANDLER='AlignImagesEvent'


widget_control, img1, SET_DROPLIST_SELECT=0
widget_control, img2, SET_DROPLIST_SELECT=(1 mod num_images)
widget_control, img3, SET_DROPLIST_SELECT=(2 mod num_images)
widget_control, crop, SET_DROPLIST_SELECT=1

return
end
