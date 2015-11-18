;+
;========================================================================
;;;
;;; FILE NAME:    $Id: list_control.pro 1252 2000-10-31 15:43:08Z patb $
;;;
;;; DESCRIPTION:  General-purpose list manipulation utility
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1996, Pennsylvania State University
;;;
;;; NOTES:        USAGE EXAMPLE:
;;;
;;;               create the list
;;;               list_control, list, /CREATE 			
;;;
;;;               add 4 items to the list
;;;               list_control, list, data0, /APPEND		
;;;               list_control, list, data1, /APPEND
;;;               list_control, list, data2, /APPEND
;;;               list_control, list, data3, /APPEND
;;;
;;;               retrieve all items concatenated together
;;;               list_control, list, all_data, /CONCATENATE 
;;;
;;;               retrieve a specific item
;;;               list_control, list, data2, INDEX=2 		
;;;
;;;               destroy the list
;;;               list_control, list, /DESTROY 
;;;
;;;               The variable passed as APPEND will be undefined in the
;;;               caller upon return.
;;;
;;;               This utility only handles numeric data (byte, int, long,
;;;               float, double).  When CONCATENATE is set, the type of the
;;;               result is the most precise type found among the elements
;;;               of the list.
;;;
;;;               If you're going to retrieve the concatenated list, and
;;;               then immediately destroy the list, then doing it in one
;;;               call will save memory, e.g.
;;;               list_control, list, all_data, /CONCATENATE, /DESTROY.
;;;
;-
;========================================================================
PRO list_control, list, data, CREATE=create, DESTROY=destroy, $
		  APPEND=append, INDEX=index, CONCATENATE=concatenate

if keyword_set(create) then begin
  ;; Make an empty list (an undefined variable).
  list = 0  &  dum = temporary(list)
endif


if keyword_set(append) then begin
  new_item = ptr_new( data, /NO_COPY )

  if (0 EQ n_elements(list)) then begin
    list = [new_item]
  endif else begin
    list = [list, new_item]
  endelse
endif


if (n_elements(index) GT 0) then begin
  data = *(list[index])
endif


item_count = n_elements(list)

if keyword_set(concatenate) then begin

  ;; If there's only one item on the list, then we can retrieve it directly
  if (item_count EQ 1) then begin

     if keyword_set(destroy) then begin
       data = temporary( *(list[0]) )
     endif else begin
       data = *(list[0])
     endelse

  ;; If there's more than one item on the list, then we have to create a new
  ;; array big enough to hold everything and copy each item into it.
  endif else begin

    ;; Count how many total elements are on the list, and figure out the
    ;; datatype we're going to need to hold the concatenated list.
    total_elements  = 0L
    result_type     = 0
    width           = 1

    for ii = 0, item_count-1 do begin
      total_elements = total_elements + n_elements( *(list[ii]) )
      component_type = size((*(list[ii]))[0], /TYPE)
      if (component_type LE 5) $
        then result_type = max([result_type, component_type]) $
        else result_type = component_type
      
      if (size(*(list[ii]), /N_DIM) EQ 2) then $
        width = (size(*(list[ii]), /DIM))[0]
    endfor

    ;; Concatenate the items
    if (total_elements GT 0) then begin
      ;; We need to handle 1-D and 2-D elements separately.
      if (width GT 1) then begin
        data = make_array( width, total_elements/width, /NOZ, TYPE=result_type )
        if (n_elements(data) NE total_elements) then $
          message, 'Components of mixed widths??'
        
      endif else begin
        data = make_array( total_elements, /NOZ, TYPE=result_type )
      endelse
      
      start = 0

      for ii = 0, item_count-1 do begin
        if keyword_set(destroy) then item = temporary(*(list[ii])) $
        			else item = *(list[ii])
        			
        item_length = n_elements(item)
        
        if ((width GT 1) AND (item_length GT 1)) then begin
          data[start] = reform( item, item_length, /OVERWRITE )
        endif else begin
          data[start] = item
        endelse
        
        start       = start + item_length
      endfor
    endif else begin
      data  = 0  &  dummy = temporary(data)
    endelse
  endelse
endif


if (keyword_set(destroy) AND (item_count GT 0)) then ptr_free, list

return
end

