;;; $Id: combine_catalogs.pro 2103 2004-11-17 15:28:16Z patb $
;;; Tool to edit several source catalogs that are stored as FITS binary tables.
;;; The catalogs must include columns named X and Y.
;;; If the edited catalogs have the same number of entries then sources are 
;;; matched and the catalogs are saved in the same order.

;;; Example:  
;combine_catalogs,['central_1.sources','iarray_2.sources','fullfield_4.sources']


;==========================================================================
;;; Clean up after the widget.
;==========================================================================
PRO CombineCatalogsCleanup, top_base

widget_control, top_base, GET_UVALUE=st

;; Free the heap vars allocated locally.
ptr_free, (*st).catalogs
ptr_free, st
return
end


;==========================================================================
;;; Routine to update the widget's appearance
;==========================================================================
PRO RedrawCombineCatalogs, st

widget_control, /HOURGLASS

; Figure out which catalog is currently selected in dataset_2d.
cat_name = ''
dataset_2d, (*st).dataset_2d, GET_DATASETS=datasets
sz = size(datasets, /STR)
if (sz.TYPE EQ 8) then begin
  cat_name = datasets[0].name
  p = strpos(cat_name,', removed')
  if (p NE -1) then cat_name = strmid(cat_name,0,p)
endif


num_cats    = n_elements((*st).catalog_names)

psyms = [1,4,5,6,7]  &  num_syms = n_elements(psyms)

for ii=0,num_cats-1 do begin
  name = (*st).catalog_names[ii]
  
  cat  = (*st).catalogs[ii]
  
  index = where( (*cat).entry_ok EQ 1, count )
  if (count GT 0) then begin
    dataset_2d, (*st).dataset_2d, [(*cat)[index].x + (*st).xoffsets[ii]], $
    				  [(*cat)[index].y + (*st).yoffsets[ii]], $
    		DATASET_NAME=name, REDRAW=0, /UNITY_ASPECT,$
    		COLOR='white', PSYM=psyms[ii mod num_syms]
    good_name = name
  endif else dataset_2d, (*st).dataset_2d, DATASET_NAME=name, /DELETE, REDRAW=0
  
  name = (*st).catalog_names[ii]+', removed'

  index = where( (*cat).entry_ok EQ 0, count )
  if (count GT 0) then begin
    dataset_2d, (*st).dataset_2d, [(*cat)[index].x + (*st).xoffsets[ii]], $
    				  [(*cat)[index].y + (*st).yoffsets[ii]], $
    		DATASET_NAME=name, REDRAW=0, $
    		COLOR='red', PSYM=psyms[ii mod num_syms]
    good_name = name
  endif else dataset_2d, (*st).dataset_2d, DATASET_NAME=name, /DELETE, REDRAW=0
endfor

if (cat_name NE '') then good_name = cat_name
dataset_2d, (*st).dataset_2d, DATASET_NAME=good_name, /REDRAW

return
end


;==========================================================================
;;; Event Handler Function
;==========================================================================
FUNCTION CombineCatalogsEventFn, Event

widget_control, /HOURGLASS
new_event   = 0
DestroyFlag = 0
SaveCatalogs= 0

;; Get the state structure. 
top_base = Event.handler
widget_control, top_base, GET_UVALUE=st

;; Process the event.
case Event.ID of

;--------------------------------------------------------------------------
; Add/Remove Source events.
  (*st).add_remove_button: $
   begin
   ;; Get a mouse click.
   prompt='Middle-click on a source, or right-click to stop '
   plot_window, (*st).pw_id, GET_MOUSE=point, PROMPT=prompt
   
   while (point[2] NE 4) do begin
     ; Figure out which catalog is currently selected in dataset_2d.
     dataset_2d, (*st).dataset_2d, GET_DATASETS=datasets
     cat_name = datasets[0].name
     p = strpos(cat_name,', removed')
     if (p NE -1) then cat_name = strmid(cat_name,0,p)
     
     index = (where( cat_name EQ (*st).catalog_names, count ))[0]
     if (count EQ 0) then index = 0
     cat = (*st).catalogs[index]
     
     xpos = (*cat).x + (*st).xoffsets[index]
     ypos = (*cat).y + (*st).yoffsets[index]
   
      ; Find nearest source in the selected catalog.
     dum = min((point[0]-xpos)^2 + (point[1]-ypos)^2, imin)
   
     (*cat)[imin].entry_ok = ((*cat)[imin].entry_ok + 1) mod 2
   
     RedrawCombineCatalogs, st
     tvcrs, ((*cat).x)[imin], ((*cat).y)[imin], /DATA
     
     plot_window, (*st).pw_id, GET_MOUSE=point, PROMPT=prompt
   endwhile
   end

;--------------------------------------------------------------------------
; Print Source events.
  (*st).print_button: $
   begin
   tnums=[0,1,4,5,8,9,11,13,17,19,20]

   ;; Get a mouse click.
   prompt='Middle-click on a source, or right-click to stop '
   plot_window, (*st).pw_id, GET_MOUSE=point, PROMPT=prompt
   
   while (point[2] NE 4) do begin
     ; Figure out which catalog is currently selected in dataset_2d.
     dataset_2d, (*st).dataset_2d, GET_DATASETS=datasets
     cat_name = datasets[0].name
     p = strpos(cat_name,', removed')
     if (p NE -1) then cat_name = strmid(cat_name,0,p)
     
     index = (where(cat_name  EQ (*st).catalog_names, count ))[0]
     if (count EQ 0) then index = 0
     cat = (*st).catalogs[index]
   
     xpos = (*cat).x + (*st).xoffsets[index]
     ypos = (*cat).y + (*st).yoffsets[index]
   
      ; Find nearest source in the selected catalog.
     dum = min((point[0]-xpos)^2 + (point[1]-ypos)^2, imin)
   
     print_struct, (*cat)[imin], TNUMS=tnums, FORM=['g','8','5']

     tvcrs, ((*cat).x)[imin], ((*cat).y)[imin], /DATA
     
     plot_window, (*st).pw_id, GET_MOUSE=point, PROMPT=prompt
   endwhile
   end

;--------------------------------------------------------------------------
; Save catalogs.
  (*st).save_button: SaveCatalogs = 1

;--------------------------------------------------------------------------
; Exit events.
  (*st).done_button:  $
    begin
      result=dialog_message("Confirm:  Quit?",/CANCEL, $
			    DIALOG_PARENT=(*st).done_button)
      if (result EQ 'OK') then DestroyFlag = 1
    end

  else: print, 'unknown event in CombineCatalogsEventFn'
endcase


if SaveCatalogs then begin
  ;; Extract all catalogs that have good entries.
  num_cats = n_elements((*st).catalog_names)
  num_good = lonarr(num_cats)
  catalogs = ptrarr(num_cats)
  
  for ii=0,num_cats-1 do begin
    cat = (*st).catalogs[ii]
   
    index = where( (*cat).entry_ok EQ 1, count )
    num_good[ii] = count
    if (count GT 0) then begin
      catalogs[ii] =  ptr_new( (*cat)[index] )
    endif
  endfor
  
  index = where(num_good GT 0, num_cats)
  if (num_cats EQ 0) then print, 'No good entries any any catalog!' $
  else begin
    num_good      = num_good[index]
    catalogs      = catalogs[index]
    catalog_names = (*st).catalog_names[index]
    xoffsets      = (*st).xoffsets[index]
    yoffsets      = (*st).yoffsets[index]

    ;; If all trimmed catalogs are the same length then sort them so their
    ;; entries match.
    if ( min(num_good EQ num_good[0]) EQ 1 ) then begin
      print, 'Sorting catalogs to match entries before saving.'
      xref = (*catalogs[0]).x + xoffsets[0]
      yref = (*catalogs[0]).y + yoffsets[0]
      num_entries = num_good[0]
      
      for ii=1,num_cats-1 do begin
        max_dist = 0.0
        catalog = catalogs[ii]
        for num_sorted = 0,num_entries-2 do begin
          ; Extract the x,y values for the unsorted tail of the catalog.
          x = ((*catalog).x)[num_sorted:*] + xoffsets[ii]
          y = ((*catalog).y)[num_sorted:*] + yoffsets[ii]
          
          ; Find the entry that is closest to the current ref entry.
          temp = min((x - xref[num_sorted])^2 + (y - yref[num_sorted])^2, imin)
          max_dist = max_dist > sqrt(temp)
          
          ; Change imin from an index into the tail to an index into the
          ; catalog
          imin = imin + num_sorted
          
          ; Exchange that matching entry with the first entry in the unsorted
          ; tail of the catalog, which lengthens the sorted head by one and
          ; shortens the unsorted tail by one.
          matching_entry         = (*catalog)[imin]
          (*catalog)[imin]       = (*catalog)[num_sorted]
          (*catalog)[num_sorted] = matching_entry
        endfor ;num_sorted loop
        
        print, 'max matching distance:', max_dist
      endfor; catalog loop
    endif ; match catalogs
    
    ;; Save the catalogs to disk with the extension '.trimmed'
    for ii=0,num_cats-1 do begin
     cat_name  = catalog_names[ii]
     trim_name = cat_name+'.trimmed'
     
     hdr = headfits( cat_name )
     writefits, trim_name, '', hdr
       
     hdr = headfits( cat_name, EXTEN=1 )
     sxaddpar, hdr, 'HISTORY', 'trimmed by $RCSfile$, Version $Revision: 2103 $'
       
     mwrfits, (*catalogs[ii]), trim_name, hdr
     print, num_good[ii], trim_name, F='("Wrote ",I0," sources to ",A)'
   endfor  
  endelse ; good entries found
endif


if DestroyFlag then widget_control, (*st).parent, /DESTROY

return, new_event
end



;==========================================================================
PRO combine_catalogs, catalog_names, XOFFSETS=xoffsets, YOFFSETS=yoffsets,$
		      REMOVE_ALL=remove_all, WIDGET_TITLE=widget_title

if (0 EQ n_elements(widget_title))   then widget_title = 'Combine Catalogs'

;; Read the catalogs.
num_cats    = n_elements(catalog_names)
catalogs    = ptrarr(num_cats)
num_entries = lonarr(num_cats)

if (num_cats NE n_elements(xoffsets)) then xoffsets = fltarr(num_cats)
if (num_cats NE n_elements(yoffsets)) then yoffsets = fltarr(num_cats)

for ii=0,num_cats-1 do begin
  cat        = mrdfits(catalog_names[ii],1)
  cat_length = n_elements(cat)
  num_entries[ii] = cat_length
  
  if TAG_EXIST(cat, 'entry_ok') then begin
    catalogs[ii]    = ptr_new( cat )
  endif else begin
    cat_plus_entry = create_struct( cat[0], 'entry_ok', $
    				   (keyword_set(remove_all) EQ 0) )
    				 
    cat_plus       = replicate(cat_plus_entry, cat_length)
    for jj=0,cat_length-1 do begin
      struct_assign, cat[jj], cat_plus_entry, /NOZERO
      cat_plus[jj] = cat_plus_entry
    endfor
    catalogs[ii]    = ptr_new( cat_plus )
  endelse
endfor


;; Build the widget.
color_manager
center  = ScreenCenter()
xoffset = center(0) - 400
yoffset = center(1) - 250
parent = plot_window_topbase(TITLE=widget_title, XOFF=xoffset, YOFF=yoffset)

top_base = widget_base( parent, /BASE_ALIGN_CENTER, $
			EVENT_FUNC='CombineCatalogsEventFn', $
			KILL_NOTIFY='CombineCatalogsCleanup', $
			/COLUMN, /SPACE, XPAD=0, YPAD=0 )

upper_base = widget_base( top_base, /ALIGN_LEFT, /ROW, $
			   SPACE=8, XPAD=0, YPAD=0 )

 add_remove_button = widget_button( upper_base, VALUE='Add/Remove Sources' )
 print_button      = widget_button( upper_base, VALUE='Print Sources' )
 save_button       = widget_button( upper_base, VALUE='Save Trimmed Catalogs' )
 done_button       = widget_button( upper_base, VALUE='Exit' )

dataset_2d, dataset_2d, PARENT_WIDGET=top_base

;; Dive into the bowels of the dataset_2d object and grab the plot_window id.
widget_control, dataset_2d, GET_UVALUE=st


;; Build state structure.
state={ parent:parent, $
	add_remove_button:add_remove_button, print_button:print_button, $
	save_button:save_button, done_button:done_button, $
	dataset_2d:dataset_2d, pw_id:(*st).pw_id, $

	catalog_names:catalog_names, catalogs:catalogs, $
	num_entries:num_entries, $
	xoffsets:xoffsets, yoffsets:yoffsets $
       }


	  
;; Save state structure.
widget_control, top_base, SET_UVALUE=ptr_new(state, /NO_COPY)

widget_control, parent, /REALIZE
xmanager, 'CombineCatalogs', parent, EVENT_HANDLER='PlotWindowTopbaseEventFn', /NO_BLOCK

widget_control, top_base, GET_UVALUE=st
RedrawCombineCatalogs, st


return
end

