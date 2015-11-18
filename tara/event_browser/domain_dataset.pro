;+
;==========================================================================
;;;
;;; FILE NAME:    $Id: domain_dataset.pro 2954 2008-01-16 22:07:50Z patb $
;;;
;;; DESCRIPTION:  Domain Dataset Widget for X-ray Event Browser application
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1996, Pennsylvania State University
;;;
;;; NOTES:        See the document "X-ray Event Browser Software Design
;;;               Specification"
;;;
;-
;==========================================================================
;;; Widget Creation Routine
FUNCTION CreateDomainDataset, manager_widget, recover_flag, $
		GENERIC=generic, KEYWORDS2COLUMNS=keywords2columns

@eb_common

;; Create the widget hierarchy, storing widget ID's in the state structure.
;; The indenting below indicates the widget hierarchy.

top_base = widget_base( TITLE='Domain Dataset', /COLUMN, $
			/SPACE, /XPAD, /YPAD, GROUP=manager_widget)

;catfile_base = widget_base( top_base, /COL, /FRAME, /SPACE, /XPAD, /YPAD )
 catfile_base = widget_base( top_base, /COL, /SPACE, /XPAD, /YPAD )

  base = widget_base( catfile_base, /ROW, /SPACE, /XPAD, /YPAD )

   catfile_button = widget_button( base, VALUE='Input File' )

   catfile_label = widget_label( base, VALUE='no file selected', /DYNAMIC_RES )

  catfile_polling = widget_label( catfile_base, VALUE='', $
  				  /ALIGN_LEFT, /DYNAMIC_RESIZE )

 middle_base = widget_base( top_base, /ROW, /SPACE, /XPAD, /YPAD )

  cat_list_base = widget_base( middle_base, /COLUMN )

   cat_list_label = widget_label( cat_list_base, /ALIGN_CENTER, $
					VALUE='Catalog Contents' )

   cat_list = widget_list( cat_list_base, XSIZE=32, YSIZE=5 )

  list_controls_base = widget_base( middle_base, /COLUMN, /SPACE, /XPAD, /YPAD)


   add_button = widget_button( list_controls_base, VALUE='>>ADD>>' )

   clear_add_button = widget_button( list_controls_base, $
				     VALUE='>>CLEAR, then ADD>>' )

   add_all_button = widget_button( list_controls_base, VALUE='>>ADD ALL>>' )

   remove_button = widget_button( list_controls_base, VALUE='<<REMOVE<<' )

  domain_list_base = widget_base( middle_base, /COLUMN )

   domain_list_label = widget_label( domain_list_base, /ALIGN_CENTER, $
					VALUE='Domain Dataset' )

   domain_list = widget_list( domain_list_base, XSIZE=32, YSIZE=5 )

 status_label = widget_label( top_base, VALUE='STATUS:', /ALIGN_CENTER, $
 				/DYNAMIC_RESIZE )

 assemble_button = widget_button( top_base, VALUE='Assemble Domain Dataset' )

 item_polling = widget_label( top_base, VALUE=' ', /ALIGN_CENTER, $
 				/DYNAMIC_RESIZE )


 parameter_base = widget_base( top_base, /ROW, /ALIGN_CENTER )

  kywd_button = widget_button(parameter_base,VALUE='Keywords')


  ;; Derived, FITS, and parameter menus.
  names = GetPropertyNames(/DERIVED)
  num_items = n_elements(names)
  menu = replicate( {CW_PDMENU_S}, 1+num_items )
  menu.name = ['Derived Properties', names]
  menu[0].flags = 1
  menu[num_items].flags = 2
  derived_menu = cw_pdmenu(parameter_base, menu, /RETURN_NAME)

  names = GetParameterNames()
  num_items = n_elements(names)
  menu = replicate( {CW_PDMENU_S}, 1+num_items )
  menu.name = ['Analysis Parameters', names]
  menu[0].flags = 1
  menu[num_items].flags = 2
  parameter_menu = cw_pdmenu(parameter_base, menu, /RETURN_NAME)
  

;; Create the widget state structure.

max_cat_entries = 5000

state = { $
	; The id of the Manager Widget.
	manager_widget:manager_widget, generic:keyword_set(generic),$
	keywords2columns: keyword_set(keywords2columns) ? keywords2columns : '', $

	; IDs of widgets that generate events or have to be updated.

	top_base:top_base, $
	catfile_label:catfile_label, catfile_polling:catfile_polling, $
	catfile_button:catfile_button, $

	cat_list:cat_list, domain_list:domain_list, $
	add_button:add_button, clear_add_button:clear_add_button, $
	add_all_button:add_all_button, remove_button:remove_button, $
	kywd_button:kywd_button, $
	derived_menu:derived_menu, parameter_menu:parameter_menu, $

	status_label:status_label, $
	assemble_button:assemble_button, $
	item_polling:item_polling, $

	; Other state information.
	max_cat_entries:max_cat_entries, $
	cat_pathname:'', cat_dir:'', filter:'', $
	num_cat_items:0, selected_cat_item:-1, $
	cat_items:strarr(max_cat_entries), $

	num_domain_items:0, selected_domain_item:-1, $
	domain_items:strarr(max_cat_entries), $

	; Dataset status: (empty, assembled, edited)
	dataset_status:'empty' $
	}

;; If the /RECOVER keyword is used to start EB, then we need to look for
;; an existing domain dataset.
if (keyword_set(recover_flag)) then state.dataset_status = 'reduced'

;; Initialize the widget appearance.
UpdateDomainDatasetAppearance, state

;; Store the widget state structure.
widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state, /NO_COPY


;; Realize the widget and register with manager.
widget_control, top_base, /REALIZE

;; To help users, bring up the catalog selection dialog immediately.
new_event = { ID:catfile_button, TOP:top_base, HANDLER:top_base }
DomainDatasetEvent,new_event

xmanager, 'filter & display manager', top_base, $
	  EVENT_HANDLER='DomainDatasetEvent', /NO_BLOCK

return, top_base
end


;==========================================================================
PRO DomainDatasetEvent, event

;; Get the widget state structure.
top_base = event.handler
widget_control, widget_info(top_base, /CHILD), GET_UVALUE=state, /NO_COPY


;; Handle the event; branch first based on which widget generated it.
case event.id of

  ;------------------------------------------------------------------------
  ;; This label widget generates only timer events that are used when the 
  ;; catalog file is being polled to see if it has been extended.
  ;; The UVALUE is a flag (0 or 1) that indicates if polling should really
  ;; be done -- when a new catalog file is selected we can get a spurious
  ;; timer event left over from a previous catalog file that we being polled.
  state.catfile_polling: $
    begin
    widget_control, state.catfile_polling, GET_UVALUE=flag

    ; We must check to see if polling is enabled because we can sometimes
    ; get an extra event on this widget after we want to STOP polling!!!
    if (keyword_set(flag)) then begin

      widget_control, /HOURGLASS

      ;; Open the cat file can skip over the items we've already read.
      openr, unit, state.cat_pathname, /GET_LUN
      count = 0
      item = ''
      while (count LT state.num_cat_items AND (NOT EOF(unit))) do begin
        readf, unit, item
        count = count + 1
      endwhile

      ;; If there are no new items, then setup to poll again.
      if (EOF(unit)) then begin

        widget_control, state.catfile_polling, TIMER=30, SET_UVALUE=1

      ;; If there are new items, append them to the cat_list.
      endif else begin

        while (NOT EOF(unit)) do begin
          readf, unit, item
          state.cat_items(count) = item
          count = count + 1
        endwhile

        if (item EQ 'END') then begin
          state.cat_items(count-1) = ''
          state.num_cat_items = count-1

          widget_control, state.catfile_label,   SET_VALUE=state.cat_pathname
          widget_control, state.catfile_polling, SET_VALUE='', SET_UVALUE=0
        endif else begin
          state.num_cat_items = count
          widget_control, state.catfile_polling, TIMER=30, SET_UVALUE=1
        endelse

        ;; Update the cat_list widget and button sensitivity.
        UpdateDomainDatasetAppearance, state
      endelse

      free_lun, unit
    endif ;flag is set
    end

  ;------------------------------------------------------------------------
  state.catfile_button: $
  begin
  ;; Allow the user to choose a new catalog file.
  pathname = dialog_pickfile( GROUP=top_base, /MUST_EXIST, $
			      PATH=state.cat_dir, $
	TITLE='Select Input File (a catalog, index, or single FITS file)' )

  if (pathname NE '') then begin
    fdecomp, pathname, disk, dir, dummy, qual
    state.cat_pathname = pathname
    state.cat_dir      = dir
    state.filter       = '*.' + qual

    ;; Throw away the old cat_items.
    state.cat_items(*) = ''

    ;; First, open the file and figure out if it is a FITS file by looking 
    ;; for a first line  that starts with SIMPLE.
    openr, unit, state.cat_pathname, /GET_LUN
    stat = fstat(unit)
    first_line = bytarr( 8 < stat.size )
    readu, unit, first_line
    free_lun, unit

    fits_flag = (string(first_line) EQ 'SIMPLE  ')

    if (fits_flag) then begin
      ; This "cat" file is really a FITS file and we need to pretend that 
      ; we've read a cat file that had one entry followed by "END".
      if (state.cat_dir EQ '') then begin
            filename = pathname
      endif else begin
	    posn = strpos(pathname, state.cat_dir) + strlen(state.cat_dir)
            filename = strmid( pathname, posn, 1000 )
      endelse

      state.cat_items(0)  = filename
      state.num_cat_items = 1
      end_found = 1

    endif else begin
      ;; This should be an ASCII index file, so let's read it.

      openr, unit, state.cat_pathname, /GET_LUN

      end_found = 0
      count = 0
      item = ''
      while ((NOT end_found) AND (NOT EOF(unit))) do begin
        readf, unit, item
        item = strtrim( item )
      
        if (item EQ 'END') then begin
  	  end_found = 1
        endif else begin
          state.cat_items(count) = item
          count = count + 1
        endelse

      endwhile
      free_lun, unit
      state.num_cat_items = count
    endelse


    if (end_found) then begin
      widget_control, state.catfile_polling, SET_VALUE='', SET_UVALUE=0
    endif else begin
      widget_control, state.catfile_polling, SET_VALUE='polling enabled', $
					     TIMER=30, SET_UVALUE=1
    endelse

    widget_control, state.catfile_label, SET_VALUE=state.cat_pathname
    

    ;; Clear the domain dataset.

    state.domain_items(*)      = ''
    state.num_domain_items     =  0
    state.selected_domain_item = -1
    widget_control, state.domain_list,     SET_VALUE=''

    state.dataset_status = 'empty'

    ;; If there is one one catfile item, then save the user some steps by
    ;; automatically defining the domain item list and pressing the Assemble
    ;; button.
    if (state.num_cat_items EQ 1) then begin
      state.domain_items(0)      = state.cat_items(0)
      state.num_domain_items     = 1
      state.selected_domain_item = 0
      widget_control, state.domain_list, SET_VALUE=state.domain_items(0:0), $
					 SET_LIST_SELECT=0
      state.dataset_status = 'edited'

      widget_control, state.assemble_button, SEND_EVENT=$
	  {ID:state.assemble_button, TOP:state.assemble_button, HANDLER:0L}
    endif

    ;; Update the cat_list widget and button sensitivity.
    state.selected_cat_item = 0
    UpdateDomainDatasetAppearance, state
  endif
  end

  ;------------------------------------------------------------------------
  state.cat_list: $
    begin
    if (state.selected_cat_item NE -1) then begin
      state.selected_cat_item = event.index
    endif
    end

  ;------------------------------------------------------------------------
  state.domain_list: $
    begin
    if (state.selected_domain_item NE -1) then begin
      state.selected_domain_item = event.index

    endif
    end

  ;------------------------------------------------------------------------
  state.add_button: $
    begin
    ;; Add the selected item to the domain list if it's not already there.

    cat_selection = state.cat_items( state.selected_cat_item )
    index = where( state.domain_items EQ cat_selection, count )

    if (count EQ 0) then begin

      ; Update state data structures.
      state.domain_items(state.num_domain_items) = cat_selection
      state.num_domain_items = state.num_domain_items + 1

      ; Update widget appearance.
      names = state.domain_items(0:state.num_domain_items-1)

      names = state.domain_items(0:state.num_domain_items-1)
      state.selected_domain_item = 0
      widget_control, state.domain_list, SET_LIST_SELECT=0, SET_VALUE=names

      state.dataset_status = 'edited'

    endif 

    state.selected_cat_item = state.selected_cat_item + 1
    UpdateDomainDatasetAppearance, state
    end

  ;------------------------------------------------------------------------
  state.clear_add_button: $
    begin
    ;; Clear the domain dataset, then add the selected item.
    cat_selection = state.cat_items( state.selected_cat_item )

    state.domain_items(*)      = ''
    state.domain_items(0)      = cat_selection
    state.num_domain_items     =  1

    names = state.domain_items(0:state.num_domain_items-1)
    state.selected_domain_item = 0
    widget_control, state.domain_list, SET_LIST_SELECT=0, SET_VALUE=names

    state.dataset_status = 'edited'
    state.selected_cat_item = state.selected_cat_item + 1
    UpdateDomainDatasetAppearance, state
    end

  ;------------------------------------------------------------------------
  state.add_all_button: $
    begin
    ;; Clear the domain dataset, then add all the catalog items.
    num_items = state.num_cat_items

    state.num_domain_items     =  num_items
    state.domain_items(*)      = ''
    state.domain_items(0:num_items-1) = $
				state.cat_items(0:num_items-1)

    names = state.domain_items(0:state.num_domain_items-1)
    state.selected_domain_item = 0
    widget_control, state.domain_list, SET_LIST_SELECT=0, SET_VALUE=names

    state.dataset_status = 'edited'
    state.selected_cat_item = 0
    UpdateDomainDatasetAppearance, state
    end

  ;------------------------------------------------------------------------
  state.remove_button: $
    begin

    if (state.num_domain_items EQ 1) then begin
      ;; Clear the domain dataset.

      state.domain_items(*)      = ''
      state.num_domain_items     =  0
      state.selected_domain_item = -1
      widget_control, state.domain_list,     SET_VALUE=''

      state.dataset_status = 'empty'
      UpdateDomainDatasetAppearance, state

    endif else begin
      ;; Remove the selected item.

      selected_index = state.selected_domain_item
      last_index     = state.num_domain_items-1
      
      for ii = selected_index+1, last_index do $
        state.domain_items[ii-1] = state.domain_items[ii]
      
      state.domain_items[last_index] = ''

      state.num_domain_items = state.num_domain_items - 1

      ;; Redisplay widget.
      names = state.domain_items(0:state.num_domain_items-1)
      state.selected_domain_item = state.selected_domain_item MOD $
      				   state.num_domain_items
      widget_control, state.domain_list, SET_VALUE=names, $
      				SET_LIST_SELECT=state.selected_domain_item

      state.dataset_status = 'edited'
      UpdateDomainDatasetAppearance, state

    endelse
    end

  ;------------------------------------------------------------------------
  state.assemble_button: $
    begin
    widget_control, /HOURGLASS

    ;; Assemble the domain dataset by reading the domain files.
    open_item = 0
    
    widget_control, state.status_label, $
		    SET_VALUE='STATUS: assembling domain dataset ...'

    AssembleDomainDataset, state, $
    			   state.domain_items(0:state.num_domain_items-1), $
			   open_item

    state.dataset_status = 'assembled'
    UpdateDomainDatasetAppearance, state


    ;; If there is an open item, enable the polling widget.
    if (keyword_set(open_item)) then begin

      text = 'Polling open item ' + open_item.name
      widget_control, state.item_polling, SET_VALUE=text, $
					  SET_UVALUE=open_item, TIMER=30
	
    endif else begin

      widget_control, state.item_polling, SET_VALUE='', $
					  SET_UVALUE=0, /CLEAR_EVENTS
	
    endelse
    end

  ;------------------------------------------------------------------------
  ;; This label generates only timer events when we are polling an open item
  ;; that is an index file.
  ;; The UVALUE of state.item_polling serves two purposes -- as a flag to
  ;; indicate if polling is enabled and as a structure to hold information 
  ;; about the open item that is being polled for new data.  
  ;; If the UVALUE is 0 then polling is DISABLED.
  ;; If the UVALUE is not 0 it should be a structure.

  state.item_polling: $
    begin
    widget_control, state.item_polling, GET_UVALUE=open_item

    ; We must check to see if polling is enabled because we can sometimes
    ; get an extra event on this widget after we want to STOP polling!!!
    if (keyword_set(open_item)) then begin

      widget_control, /HOURGLASS

      widget_control, state.status_label, $
		      SET_VALUE='STATUS: checking for new data ...'

      ;; Try to append new data to the domain dataset.
      AssembleDomainDataset, state, 0, open_item 

      state.dataset_status = 'assembled'
      UpdateDomainDatasetAppearance, state

      ;; If the item is still open, re-enable the polling.
      if (keyword_set(open_item)) then begin

        widget_control, state.item_polling, SET_UVALUE=open_item, TIMER=30
	
      endif else begin

        widget_control, state.item_polling, SET_VALUE='', $
					    SET_UVALUE=0, /CLEAR_EVENTS
	
      endelse
    endif ;open_item NE 0
    end


  ;------------------------------------------------------------------------
  state.kywd_button: $
    begin
    xdisplayfile, '', GROUP=top_base, TEXT=GetPrimaryKywds(), $
		  TITLE='Primary Keywords'
    xdisplayfile, '', GROUP=top_base, TEXT=GetTableKywds(), $
		  TITLE='Binary Table Keywords'
    end

  ;------------------------------------------------------------------------
  state.derived_menu: $
   begin
   EditDerivedProperty, event.value, top_base
   new_event = { DOMAIN_SET_REVISED, ID:state.manager_widget, $
                 TOP:state.manager_widget, HANDLER:0L, $
                 NAME: 'domain_set_revised', COMPUTE_WDS:0, FILENAME:'' }
   widget_control, state.manager_widget, SEND_EVENT=new_event
   end

  ;------------------------------------------------------------------------
  state.parameter_menu: $
   begin
   EditParameter, event.value, top_base
   new_event = { DOMAIN_SET_REVISED, ID:state.manager_widget, $
                 TOP:state.manager_widget, HANDLER:0L, $
                 NAME: 'domain_set_revised', COMPUTE_WDS:0, FILENAME:'' }
   widget_control, state.manager_widget, SEND_EVENT=new_event
   end

  ;------------------------------------------------------------------------
  else: message, 'event not handled'
endcase


;; Save the state structure.
widget_control, widget_info(top_base, /CHILD), SET_UVALUE=state, /NO_COPY


return
end

;==========================================================================
PRO UpdateDomainDatasetAppearance, state

;; ADD, CLEAR, and ADD ALL buttons.
if (state.num_cat_items GT 0) then begin

  widget_control, state.add_button,       SENSITIVE=1
  widget_control, state.clear_add_button, SENSITIVE=1
  widget_control, state.add_all_button,   SENSITIVE=1

  names = state.cat_items(0:state.num_cat_items-1)
  state.selected_cat_item = state.selected_cat_item MOD state.num_cat_items

  widget_control, state.cat_list, SET_VALUE=names, $
  				  SET_LIST_SELECT=state.selected_cat_item 

endif else begin

  widget_control, state.add_button,       SENSITIVE=0
  widget_control, state.clear_add_button, SENSITIVE=0
  widget_control, state.add_all_button,   SENSITIVE=0

  state.selected_cat_item = -1
  widget_control, state.cat_list, SET_VALUE=''

endelse


;; REMOVE button
if (state.num_domain_items GT 0) then begin
  widget_control, state.remove_button, SENSITIVE=1
endif else begin
  widget_control, state.remove_button, SENSITIVE=0
endelse


;; ASSEMBLE button & status label
case state.dataset_status of
 'empty': $
  begin
    widget_control, state.status_label, $
		    SET_VALUE='STATUS: domain dataset is empty'
    widget_control, state.assemble_button, SENSITIVE=0
  end

 'assembled': $
  begin
    status = string( CountXrayData(), $
	f='("STATUS: domain dataset assembled, (",I0," xray events)")')
    widget_control, state.status_label, SET_VALUE=status
    widget_control, state.assemble_button, SENSITIVE=0
  end

 'reduced': $
  begin
    status = string( CountXrayData(), $
	f='("STATUS: domain dataset reduced, (",I0," xray events)")')
    widget_control, state.status_label, SET_VALUE=status
    widget_control, state.assemble_button, SENSITIVE=0
  end

 'edited': $
  begin
    widget_control, state.status_label, $
		    SET_VALUE='STATUS: editing domain dataset'
    widget_control, state.assemble_button, SENSITIVE=1

    ; Disable polling of open item.
    widget_control, state.item_polling, SET_VALUE='', $
					  SET_UVALUE=0, /CLEAR_EVENTS
  end
endcase

return
end


;==========================================================================
;;; Assemble the items named in the string array "item_list" into a
;;; domain dataset accessible by the function GetXrayProperty().

;;; Each item in item_list can be either a FITS filename OR an index 
;;; file containing the names of a bunch of FITS files.  These index files
;;; form a second level of cataloging.  A completed index file, just like
;;; a completed catalog file, should contain a line with the word "END".

;;; THE RULES FOR CATALOG AND INDEX FILE CONTENTS ARE AS FOLLOWS (I think):
;;; Catalog files (the top-level file that contains the items in "item_list")
;;; can contain either relative or absolute pathnames to either FITS or
;;; "index" files.
;;; Index files (the second-level of cataloging) must contain ONLY relative
;;; path names.

;;; On entry, if open_item is zero then a new domain_dataset is assembled
;;; using the list of items in item_list.  If one of the items is an index
;;; file (described above) and that item is still "open" (does not contain
;;; "END") then the parameter "open_item" is set to a structure describing 
;;; the open index file.

;;; On entry, if open_item is not zero, then it should be a structure returned 
;;; by a previous call to AssembleDomainDataset containing information about 
;;; an open item, i.e. an index file that is subject to extension.  
;;; In this case, if the index file has grown, then the new data is appended 
;;; to the domain dataset.

;==========================================================================
PRO AssembleDomainDataset, state, item_list, open_item

catalog_path = state.cat_dir
dataset_changed = 0

if (keyword_set(open_item)) then begin
  
  ;; Handle an open_item with an absolute pathname.
  fdecomp, open_item.name, disk, item_path, item_name, item_qual
  if ('' NE item_qual)               then item_name = item_name+ '.' +item_qual
  if (0 NE strpos( item_path, '/' )) then item_path = catalog_path + item_path

  ;; Skip over the files in the open item that have already been processed.
  openr, unit, item_path + item_name, /GET_LUN
  filename  = ''
  for ii=0, open_item.file_index-1 do readf, unit, filename

  ;; Compile a list of the new filenames found in the open item.
  file_list = strarr(state.max_cat_entries)
  num_files = 0
  end_found = 0
  while ((NOT end_found) AND (NOT EOF(unit))) do begin
    readf, unit, filename
    filename = strtrim( filename )

    if (filename EQ 'END') then begin
      end_found = 1
    endif else begin
      file_list(num_files) = item_path + filename
      num_files            = num_files + 1
      open_item.file_index = open_item.file_index + 1
    endelse
  endwhile

  free_lun, unit

  ;; If the item is no longer subject to extension, set open_item to zero.
  if (end_found) then open_item = 0

  ;; Append data to the domain dataset if the open item has grown.
  if (num_files GT 0) then begin
    ReadDomainDataset, file_list(0:num_files-1), GENERIC=state.generic, $
    		       KEYWORDS2COLUMNS=state.keywords2columns, $
		       /APPEND, DATASET_CHANGED=dataset_changed
  endif 
  
endif else begin
  ;; Create a new domain dataset from the list of items.

  ;; Compile a list of all the FITS files contained in all the items in
  ;; item_list.

  open_item = 0
  file_list = strarr(state.max_cat_entries)
  num_files = 0
  for ii=0, n_elements(item_list)-1 do begin
    
    ;; Handle catalog items with absolute pathnames.
    fdecomp, item_list(ii), disk, item_path, item_name, item_qual
    if ('' NE item_qual)               then item_name = item_name+'.'+item_qual
    if (0 NE strpos( item_path, '/' )) then item_path = catalog_path + item_path


    ;; First, open the item and figure out if it is a FITS file by looking  
    ;; for a first line that starts with SIMPLE.
    openr, unit, item_path + item_name, /GET_LUN
    stat = fstat(unit)
    first_line = bytarr( 8 < stat.size )
    readu, unit, first_line
    free_lun, unit

    fits_flag = (string(first_line) EQ 'SIMPLE  ') 

    if (fits_flag) then begin
      ; This is a FITS file, so add the item name to the file_list and go on.
      file_list(num_files) = item_path + item_name
      num_files            = num_files + 1
    endif else begin
     ; This is an "index" file, so read it's contents.
      openr, unit, item_path + item_name, /GET_LUN

      linecount = 0
      end_found = 0
      while ((NOT end_found) AND (NOT EOF(unit))) do begin
        line      = ''
	readf, unit, line

        if (line EQ 'END') then begin
          end_found = 1
        endif else begin
          file_list(num_files) = item_path + line
          num_files            = num_files + 1
          linecount            = linecount + 1
        endelse
      endwhile

      free_lun, unit

      ;; If the item is subject to extension, assign open_item.
      if (NOT end_found) then begin
        open_item = {OPEN_ITEM, name:item_list(ii), file_index:linecount}
      endif
    endelse

  endfor

  ;; Read the domain dataset files.
  if (num_files GT 0) then begin
    ReadDomainDataset, file_list(0:num_files-1), GENERIC=state.generic,$
    		       KEYWORDS2COLUMNS=state.keywords2columns, $
		       DATASET_CHANGED=dataset_changed
  endif else begin
    ;; Throw away the data in the domain dataset.
    InitializeDomainDataset
    dataset_changed = 1
  endelse
endelse

if (dataset_changed) then begin
  new_event = { DOMAIN_SET_REVISED, ID:state.manager_widget, $
                TOP:state.manager_widget, HANDLER:0L, $
                NAME: 'domain_set_revised', COMPUTE_WDS:1, $
		FILENAME:file_list[0] }
  widget_control, state.manager_widget, SEND_EVENT=new_event
endif

return
end


;==========================================================================
;;; Read the x-ray data in the FITS files in file_list into the domain
;;; dataset.  
;;; The output parameter dataset_changed is set to TRUE if this call
;;; changes the domain dataset.
;==========================================================================
PRO ReadDomainDataset, file_list, APPEND=append, $
	GENERIC=generic, KEYWORDS2COLUMNS=keywords2columns, $
	DATASET_CHANGED=dataset_changed

;;---------------------------------------------------------------------
;; We cannot APPEND if there are zero events in the existing domain dataset.
if (CountXrayData() EQ 0) then append = 0

;; Unless we're appending, let's throw away the existing domain dataset.
if (NOT keyword_set(append)) then begin
  InitializeDomainDataset
  dataset_changed = 1
endif

print
print, 'Counting events in domain dataset files ... '
dataset_changed = 0
num_files = n_elements(file_list)
error    = ''

;;---------------------------------------------------------------------
;; Count how many x-ray events are in the files and extract keywords.
num_xrays = 0

for ii=0, num_files-1 do begin
  ;; Open the file and read the primary header.
  fits_open, file_list(ii), fcb, /NO_ABORT, MESSAGE=error
  
  if (!ERR EQ -1) then goto, FITSERROR

  ;; Read the 'EVENTS' binary table header and count the rows.
  if keyword_set(generic) then begin
    fits_read, fcb, dummy, header, /NO_PDU, /HEADER_ONLY, EXTEN_NO=1, $
  	       /NO_ABORT, MESSAGE=error
  endif else begin
    fits_read, fcb, dummy, header, /NO_PDU, /HEADER_ONLY, EXTNAME='EVENTS', $
  	       /NO_ABORT, MESSAGE=error
  endelse
  
  if (!ERR EQ -1) then goto, FITSERROR
   
  count =  fxpar( header, 'NAXIS2' )
  num_xrays = num_xrays + count

  print, file_list(ii), count, f='(4x,A,": (",I0," events)")'
  
  ;; Close the file.
  fits_close, fcb
endfor ;; Loop over file_list.


;;---------------------------------------------------------------------
;; If there are no x-rays to read in, then we're done.
if (num_xrays EQ 0) then return


;;---------------------------------------------------------------------
;; Set up a list_control object for each FITS column.
tbinfo, header, tb_str
num_cols  = n_elements(tb_str.ttype)

col_name = tb_str.ttype
unit_name= tb_str.tunit

if keyword_set(keywords2columns) then begin
  num_keycols = n_elements(keywords2columns)
  ; Enlarge set of "columns" to include those derived from header keywords.
  col_name    = [col_name,  keywords2columns]
  unit_name   = [unit_name, replicate('',num_keycols)] 
endif else begin
  num_keycols = 0
endelse

dim_spec = strarr(num_cols + num_keycols)
tdims    = fxpar(header, 'TDIM*', COUNT=count)
if (count GT 0) then dim_spec[0] = tdims

prop_name= strupcase( col_name )

lists     = ptrarr(num_cols + num_keycols, /ALLOC)
for ii = 0, n_elements(lists)-1 do list_control, *lists[ii], /CREATE

;; If we're appending, then add the existing data to the lists (without copying).
if keyword_set(append) then begin
  for ii = 0, n_elements(lists)-1 do begin
    if (NOT GetProperty( prop_name[ii], DDS_DATA=dds_data )) then begin
      error = 'Cannot combine FITS files with different column names!'
      goto, FITSERROR
    endif
    
    list_control, *lists[ii], *dds_data, /APPEND
  endfor
endif


;;---------------------------------------------------------------------
;; Read each FITS event list.
for filenum=0, num_files-1 do begin
  ;; Open the file and read the primary header.
  fits_open, file_list(filenum), fcb, /NO_ABORT, MESSAGE=error
  
  if (!ERR EQ -1) then goto, FITSERROR
  
  ;; Read the 'EVENTS' binary table header and count the rows.
  print, 'reading file ... '
  if keyword_set(generic) then begin
    fits_read, fcb, table, header, /NO_PDU, EXTEN_NO=1, $
  	       /NO_ABORT, MESSAGE=error
  endif else begin
    fits_read, fcb, table, header, /NO_PDU, EXTNAME='EVENTS', $
  	       /NO_ABORT, MESSAGE=error
  endelse

  if (!ERR EQ -1) then goto, FITSERROR
  
  fits_close, fcb

  tbinfo, header, tb_str
   
  num_rows  = fxpar( header, 'NAXIS2' )
  
  if (num_rows GT 0) then begin
    ;; Extract the data for each column.
    for ii = 0, num_cols-1 do begin
      index = where( tb_str.ttype EQ col_name[ii], count )
      if (count EQ 0) then begin
        error = 'Cannot combine FITS files with different column names!'
        goto, FITSERROR
      endif
      
      data = tbget(tb_str, table, col_name[ii])
      case (tb_str.idltype)[ii] of
       2: if (size(data, /TYPE) GE 4) then data = fix(data)
       3: if (size(data, /TYPE) GE 4) then data = long(data)
       else:
      endcase

      list_control, *lists[ii], data, /APPEND
    endfor
    
    ;; Populate any columns derived from header keywords.
    ;; Try the binary table header first, then the primary header.
    for ii = 0, num_keycols-1 do begin
      value = fxpar(header, keywords2columns[ii], COUNT=count, COMMENT=comment)
      if (count EQ 0) then value = fxpar(fcb.HMAIN, keywords2columns[ii], COUNT=count, COMMENT=comment)
      
      if (count EQ 0) then print, keywords2columns[ii], file_list(filenum), $
      				  F='(%"WARNING: header keyword %s not found in %s")'
      
      if (size(value, /TNAME) EQ 'STRING') then begin
        str = value
        if NOT strnumber(str, value) then begin
          print, str, keywords2columns[ii], $
          	 F='(%"WARNING: cannot convert value ''%s'' in keyword %s to float")'
          value = 0
        endif
      endif
      
      list_control, *lists[num_cols+ii], replicate(value,num_rows), /APPEND
      unit_name[num_cols+ii] = comment
    endfor
  endif
  
  ;; Give up memory.
  table = 0

  ;; Save the primary header and table header keywords.
  ExtractPrimaryKywds, fcb.HMAIN, DUPLICATE=(filenum GT 0)
  ExtractTableKywds,   header
endfor


;;---------------------------------------------------------------------
;; Create or update the FITS properties that correspond to the FITS columns
;; we've read.
for ii = 0, n_elements(lists)-1 do begin    
  wcs_object, wcs, /INIT, HEADER=header, COLNUM=1+ii
  list_control, *lists[ii], data, /CONCATENATE, /DESTROY
  AddFitsProperty, prop_name[ii], col_name[ii], '', unit_name[ii], '', $
    		   dim_spec[ii], '', wcs, data
endfor

ptr_free, lists
dataset_changed = 1

;;---------------------------------------------------------------------
;; Set default values for various parameters.
SetParameterDefaults

return

FITSERROR:
    TimedMessage, msg, error, TITLE='ERROR READING EVENTS' 
    message, error, /INFO
    return
  
end


;==========================================================================
;; We need a procedure with the same name as this file to keep 
;; RESOLVE_ROUTINE happy.
PRO domain_dataset
return
end
