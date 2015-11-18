;;; Can some analysis parameters come from FITS keywords?
;;; Should the be written out as FITS keywords?
;+
;========================================================================
;;;
;;; FILE NAME:    $Id: eb_property.pro 4379 2012-10-30 20:18:54Z psb6 $
;;;
;;; DESCRIPTION: Routines to manipulate event properties
;;;		 for X-ray Event Browser application
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Copyright (C) 1998, Pennsylvania State University
;-
;========================================================================


;========================================================================
FORWARD_FUNCTION DerivedPropertyIsValid
FORWARD_FUNCTION GetPropertyNames

;========================================================================
FUNCTION GetEpoch, INCREMENT=increment

@eb_common

if keyword_set(increment) then epoch = epoch + 1

return, epoch
end


;========================================================================
PRO RemoveProperty, names

@eb_common

for ii = 0, n_elements(prop_list)-1 do begin
  prop_ptr = prop_list[ii]
  dum = where( (*prop_ptr).name EQ names, count )
  
  if (count GT 0) then begin
    ptr_free, (*prop_ptr).data
    ptr_free, prop_ptr
  endif
endfor

prop_list = prop_list[where( ptr_valid(prop_list) )]

return
end


;========================================================================
;;; Make sure the property list contains all the derived properties we support
;;; and does NOT contain any FITS properties.

PRO InitializeDomainDataset, recover_flag, VERBOSE=verbose, $
	GUI_ACTIVE=gui_active_kywd, UNDEFINE_PROPERTIES=undefine_properties

@eb_common

if (n_elements(verbose) EQ 0) then eb_verbose=1 else eb_verbose=verbose

if (n_elements(prop_list) EQ 0) then begin
  dds_size  = 0
  null_data = ptr_new( [0] )
  epoch     = 0L
  
  fxhmake, primary_kywds,    /INITIALIZE
  fxbhmake,  table_kywds, 0, /INITIALIZE
  keywords_epoch = 0L
  gui_active     = 1B
    
  ;; Create the parameter list.
  param_list = [ptr_new()]
  
  AddParameter, 'split_event_threshold', 'split event threshold',    3, 13
  AddParameter, 'time_offset', 'time offset',			     3, 0
  AddParameter, 'ev_per_pi', 'PI channel width (eV)',                3, 14.6
  AddParameter, 'ev_per_dn', 'eV/DN (linear DN-to-eV model)',        3, 1
  AddParameter, 'ev_dn_offset',	'eV offset (linear DN-to-eV model)', 3, 0
  
  ; Default the dn2ev filename to the ACIS file.
  result = routine_info( 'eb_property', /SOURCE )
  fdecomp, result.PATH, disk, dir, name, qual
  AddParameter, 'dn_ev_file', $
  	     'file containing amp-dependent ev_per_dn & ev_dn_offset values',$
		0, ''
		
  AddParameter, 'island_order', 'style for ordering pixels in event island', $
  		1, 'UNKNOWN', ALLOWED_VALUES=['UNKNOWN','ATICA', 'ASCA', 'ASC']

  AddParameter, 'x_dither_amplitude', 'x_dither_amplitude (ACIS pixels)',$
  		3, 10
  AddParameter, 'x_dither_period', 'x_dither_period (ACIS exposures)',$
  		3, 20
  AddParameter, 'x_dither_null', 'x_dither_null (ACIS exposure number)',$
  		3, 0
  AddParameter, 'y_dither_amplitude', 'y_dither_amplitude (ACIS pixels)',$
  		3, 10
  AddParameter, 'y_dither_period', 'y_dither_period (ACIS exposures)',$
  		3, 20
  AddParameter, 'y_dither_null', 'y_dither_null (ACIS exposure number)',$
  		3, 0



  param_list = param_list[1:*]
  

  ;; EB has just started, so create all the derived properties.  
  AddDerivedProperty, 'row_number'
  AddDerivedProperty, 'x_position'
  AddDerivedProperty, 'y_position'
  AddDerivedProperty, 'timestamp'
  AddDerivedProperty, 'pha'
  AddDerivedProperty, 'energy'
  AddDerivedProperty, 'wavelength'
  AddDerivedProperty, 'pi'
  AddDerivedProperty, 'grating_order'
  AddDerivedProperty, 'instrument_grade'
  AddDerivedProperty, 'asca_grade'
  AddDerivedProperty, 'ccd_id'
  AddDerivedProperty, 'amp_id'
  AddDerivedProperty, 'exposure'
  AddDerivedProperty, 'qualcode'
  AddDerivedProperty, 'island'
  AddDerivedProperty, 'center_pix'
  AddDerivedProperty, 'up_pix'
  AddDerivedProperty, 'down_pix'
  AddDerivedProperty, 'left_pix'
  AddDerivedProperty, 'right_pix'
  AddDerivedProperty, 'ul_pix'
  AddDerivedProperty, 'ur_pix'
  AddDerivedProperty, 'll_pix'
  AddDerivedProperty, 'lr_pix'
  AddDerivedProperty, 'generic1'
  AddDerivedProperty, 'generic2'
  AddDerivedProperty, 'generic3'
  AddDerivedProperty, 'generic4'
  AddDerivedProperty, 'generic5'
  AddDerivedProperty, 'generic6'
    
  
endif else if (NOT keyword_set(recover_flag)) then begin
  ;; Unless the recover_flag is set, throw away all the FITS property data
  ;; (freeing the pointers), the working data set indexes, the FITS keywords,
  ;; and the primary header data.
  RemoveProperty, GetPropertyNames( /FITS )
  dds_size   =0  &  wds_size = 0
  wds_indexes=[0] 
  fxhmake, primary_kywds, /INITIALIZE
  fxbhmake,  table_kywds, 0, /INITIALIZE
  keywords_epoch = 0L
  
  ;; Set all the epoch tags in derived properties to zero so they are stale.
  ;; Throw away the data.
  for ii = 0, n_elements(prop_list)-1 do begin
      (*prop_list[ii]).data_epoch  = 0
      (*prop_list[ii]).fresh_epoch = 0
      
      ;; Unless the previous data pointer was an alias, free it.
      if ((*prop_list[ii]).alias_t EQ 0) then ptr_free, (*prop_list[ii]).data

      if keyword_set(undefine_properties) then $
        dum = DerivedPropertyIsValid( *prop_list[ii], /SET_UNDEFINED )
  endfor
      
endif

if (n_elements(gui_active_kywd) EQ 1) then gui_active=gui_active_kywd
return
end


;========================================================================
;;; Create a new property structure, add it to prop_list, and return a 
;;; pointer to the new property.

PRO CreateProperty, name, prop_ptr

@eb_common

wcs_object, wcs, /INIT

prop = { EB_PROPERTY, $
 name: 		name, $ 
   ;a unique string
 col_name:	'', $
   ;FITS column name (TTYPE keyword)
 col_txt:	'', $
   ;comment on TTYPE keyword
 unit_name:	'', $
   ;units of property (TUNIT keyword)
 unit_txt:	'', $
   ;comment on TUNIT keyword
 dim_spec:	'', $
   ;dimension of property (TDIM keyword)
 dim_txt:	'', $
   ;comment on TDIM keyword
 wcs:		wcs, $
   ;World Coordinate system object
   
 def_code: 	0, $	
   ;0:	  This property corresponds to a FITS column.
   ;1..N: This property is derived from other ones using an algorithm 
   ;      coded by "def_code".
 alias_t:	0, $
   ;1:   This property is an alias for another, named in def_parameter.
   ;     The "data" pointer here points to a heap variable "owned" by
   ;     another property and should NOT be freed.
 def_txt:  	'', $
   ;a description of the definition to show the user
 def_parameter: '', $
 def_search_in_progress: 0, $
   ; 1: A search for a valid definition of this property is currently
   ;    in progress; do NOT initiate a duplicate search!
   
 data:          ptr_new( ), $
   ;pointer to the data, 1-D vector or 2-D array (k,num_events)
 data_epoch:		0L, $
   ;the epoch when *data was last computed
 fresh_epoch:		0L $
   ;the epoch when the definition AND data of this property were known to be
   ;fresh.  Note the distinction from data_epoch: data_epoch is when the
   ;data was last computed, fresh_epoch is when we last verified that the
   ;data is still fresh (doesn't need to be computed).
	}
prop_ptr = ptr_new(prop, /NO_COPY)

if (n_elements(prop_list) EQ 0) then prop_list = [prop_ptr] $
				else prop_list = [prop_list, prop_ptr]
return
end


;========================================================================
;;; The parameter "prop_name" should be upper case!

PRO AddFitsProperty, prop_name, col_name, col_txt, unit_name, unit_txt, $
		     dim_spec, dim_txt, wcs, data

@eb_common

;; Search for an existing property with the specified name.
found = 0
ii    = 0
while ((NOT found) AND (ii LT n_elements(prop_list))) do begin
  prop_ptr = prop_list[ii]
  if ((*prop_ptr).name EQ prop_name) then found = 1
  ii = ii + 1
endwhile

;; If necessary, create a new property.
if (NOT found) then begin
  CreateProperty, prop_name, prop_ptr
  (*prop_ptr).col_name  = col_name
  (*prop_ptr).col_txt   = col_txt
  (*prop_ptr).unit_name = unit_name
  (*prop_ptr).unit_txt  = unit_txt
  (*prop_ptr).dim_spec  = dim_spec
  (*prop_ptr).dim_txt   = dim_txt
  (*prop_ptr).wcs       = wcs
  (*prop_ptr).def_code  = 0
  (*prop_ptr).def_txt   = 'FITS column "' + col_name + '"'
endif

dds_size = (size(data,/DIM))[ size(data,/N_DIM)-1 ]

epoch = GetEpoch( /INCREMENT )
ptr_free, (*prop_ptr).data
(*prop_ptr).data        = ptr_new( data, /NO_COPY )
(*prop_ptr).data_epoch  = epoch
(*prop_ptr).fresh_epoch = epoch

return
end


;========================================================================
;;; Search for the named property, either a FITS property or a derived one,
;;; returning a pointer to it in "prop_ptr".  If the property is found and
;;; it has a valid definition, the value 1 is returned.
;;; If a derived property is found, but it's current definition is invalid,
;;; then a search is initiated for a valid one.  
;;; 
;;; The tricky part of searching for a new definition is that the set of
;;; all possible definitions for all the derived properties forms a graph
;;; which has loops in it.  As a simple example consider two properties
;;; "energy" and "wavelength" -- there might exist a definition of "energy"
;;; in terms of "wavelength" (for use when "wavelength" is supplied
;;; externally or calculated somehow) AND a definition of "wavelength" in
;;; terms of "energy" (for use when "energy" is available some other way).
;;; 
;;; Since this graph has loops, we have to be careful to avoid infinite
;;; recursion while searching for a property definition.  For example
;;; suppose we are searching for a definition of "energy" and we're testing
;;; the definition which involves "wavelength".  Well, we have to determine
;;; if "wavelength" has a valid definition (by calling this routine
;;; recursively).  Suppose the current definition of "wavelength" is
;;; invalid and a search is begun for a new one.  When we get to trying out
;;; the "wavelength" definition that involves "energy", we need to
;;; recognize that we're already in the process of trying to define
;;; "energy" and we must abandon that definition of "wavelength" as an
;;; option.  We do this by setting the def_search_in_progress property tag 
;;; while we're searching for a definition for the property.  During the
;;; searching process, which may induce searches for definitions of other
;;; properties, if we run into a property with def_search_in_progress set, 
;;; we know we've traversed a loop in the graph and need to prune our search.
;;; 
;;; If DEF_ONLY is set, the property's definition is checked, but its data
;;; is NOT recomputed if it is stale.
;;; 
;;; DDS_DATA returns a POINTER to the domain dataset data.
;;; WDS_DATA returns the working dataset data itself (NOT a pointer).
;;; ONE_DATUM returns the property value for ONE event, which may be a vector.
;;; LABEL returns a string suitable for labeling a plot axis.

FUNCTION GetProperty, name, prop_ptr, DEF_ONLY=def_only, $
		      DDS_DATA=dds_data, WDS_DATA=wds_data,$
		      ONE_DATUM=one_datum, LABEL=label, WCS=wcs

@eb_common

def_only = keyword_set(def_only)

;; Check for invalid keyword combinations.
if ((arg_present(dds_data) OR arg_present(wds_data) OR arg_present(one_datum))$
    AND def_only) then message, 'Invalid keyword combination.'

;; Search for the specified property name.
found = 0
ii    = 0
while ((NOT found) AND (ii LT n_elements(prop_list))) do begin
  prop_ptr = prop_list[ii]
  if ((*prop_ptr).name EQ name) then found = 1
  ii = ii + 1
endwhile


;; If property is derived, then validate its definition.
if (found AND ((*prop_ptr).def_code NE 0)) then begin
  ;; If we find that def_search_in_progress is set, then we know that this
  ;; property does not currently have a valid definition.  We do NOT want
  ;; to test the definition by calling DerivedPropertyIsValid() because
  ;; such a call may result in another call to GetProperty() for another
  ;; property that is also engaged in a definition search -- leading to an
  ;; infinite "ping pong" recursion like this:
  ;; GetProperty(A) -> DerivedPropertyIsValid(A) -> 
  ;; GetProperty(B) -> DerivedPropertyIsValid(B) -> GetProperty(A) -> ...
  
  if ((*prop_ptr).def_search_in_progress EQ 1) then begin
    found = 0
  endif else begin
    found = (1 EQ DerivedPropertyIsValid( *prop_ptr, DEF_ONLY=def_only ))
    
    ;; If the current property definition is NOT valid, then initiate a
    ;; search for a valid one, stopping when the first valid one is found
    ;; (valid EQ 1) or when we run out of definitions (valid EQ -1).
    ;; We know that a definition search for this property is NOT already in 
    ;; progress because we tested def_search_in_progress above.
    if (NOT found) then begin
;     print, 'Searching for definition of property ', (*prop_ptr).name
      def_code  =  0
      repeat begin
        def_code = def_code + 1
  
        ;; Set def_search_in_progress to indicate that we're in search mode.
        (*prop_ptr).def_search_in_progress = 1
        
        valid = DerivedPropertyIsValid( (*prop_ptr), DEF_ONLY=def_only, $
        				       DEF_CODE=def_code, /INIT_FITS)
      endrep until (valid NE 0)
      
      (*prop_ptr).def_search_in_progress = 0
      
      if (valid EQ 1) then begin
        if (eb_verbose GT 0) then print, (*prop_ptr).name, (*prop_ptr).def_txt, F='(A16," == ''",A,"''.")'
        found = 1
      endif else begin
        ;; All definition attempts have failed, so set the property's definition
        ;; to "undefined" and return failure.
        print, (*prop_ptr).name, F='("Property {",A,"} cannot be defined.")'
        dum = DerivedPropertyIsValid( (*prop_ptr), /SET_UNDEFINED )
        found = 0
      endelse
    endif ;(NOT found) 
  endelse ;def_search_in_progress EQ 0
endif ;derived property


;; Return the property components requested.
if (found AND (NOT def_only)) then begin
  dds_data = (*prop_ptr).data
  
  n_dimensions = size(*dds_data, /N_DIMEN)
  
  if arg_present(wds_data) then begin
    if (wds_size EQ dds_size) then begin
      ; If the DDS and WDS are the same, skip the indexing for efficiency.
      wds_data = *dds_data
    endif else begin
      ; In the usual case, we must index the DDS by wds_indexes.
      if (n_dimensions EQ 2) $
       then wds_data = (*dds_data)[*,wds_indexes] $
       else wds_data = (*dds_data)[  wds_indexes]
    endelse
  endif
  
  if (n_dimensions EQ 2) then begin
    this_prop_length = (size(*dds_data,/DIM))[1]
    one_datum        = (*dds_data)[*,0]
  endif else begin
    this_prop_length = (size(*dds_data,/DIM))[0]
    one_datum        = (*dds_data)[0]
  endelse
  
  ;; As a defensive move, let's verify that the number of property values
  ;; matches the number of events in the DDS.  
  if (dds_size NE this_prop_length) then $
    message, 'property '+name+' has the wrong number of entries.'

endif else begin
  dds_data = null_data
  wds_data = *null_data
  one_datum= (*null_data)[0]
endelse 

if (found AND arg_present(label)) then begin
  ttype = strtrim( (*prop_ptr).col_name,  2 )
  tunit = strtrim( (*prop_ptr).unit_name, 2 )
  if (tunit NE '' AND tunit NE 'none' AND tunit NE 'NONE') then begin
    label = string(ttype, tunit, f='(A," (",A,")")')
  endif else begin
    label = ttype
  endelse
endif else begin
  label = ''
endelse 

wcs = (*prop_ptr).wcs

return, found
end


;========================================================================
FUNCTION GetPropertyNames, FITS=fits, DERIVED=derived, $
			   DIM=dim, NUMERIC_ONLY=numeric_only

@eb_common

names = strarr( n_elements(prop_list) )
dims  = intarr( n_elements(prop_list) )
types = intarr( n_elements(prop_list) )

for ii = 0, n_elements(prop_list)-1 do begin
  prop_ptr = prop_list[ii]

  if (keyword_set(fits)    AND ((*prop_ptr).def_code EQ 0)) then $
    names[ii] = (*prop_ptr).name
    
  if (keyword_set(derived) AND ((*prop_ptr).def_code NE 0)) then $
    names[ii] = (*prop_ptr).name
  
  if ptr_valid((*prop_ptr).data) then begin
    dims [ii]  = size(*(*prop_ptr).data, /N_DIMEN)
    types[ii]  = size(*(*prop_ptr).data, /TYPE)
  endif
endfor

ok_to_return = (names NE '')
if keyword_set(dim) then ok_to_return = ok_to_return AND (dims EQ dim)

if keyword_set(numeric_only) then begin
  ok_to_return = ok_to_return AND (types NE 7)
endif

index = where( ok_to_return, count )
		    
if (count EQ 0) then return, '' $
		else return, names[index]
end


;========================================================================
PRO SetWdsMask, mask, MERGE=merge

@eb_common

if keyword_set(merge) then begin
  if (n_elements(mask) NE n_elements(wds_mask)) then $
    message, 'ERROR: Existing and extra mask vectors are not the same length!'
  wds_mask AND= mask 
endif else begin
  wds_mask    = mask
endelse

;; As a defensive move, let's verify that the mask length
;; matches the number of events in the DDS.  
if ((dds_size > 1) NE n_elements(mask)) then $
    message, 'mask & DDS sizes do not match!!'

wds_indexes = where( mask, wds_size )

if (wds_size EQ 0) then wds_indexes = [0]

return
end


;========================================================================
FUNCTION CountXrayData, WORKING_DATASET=working_dataset

@eb_common

if keyword_set(working_dataset) then return, wds_size $
else return, dds_size
end



;========================================================================
;==========================================================================
;; We need a procedure with the same name as this file to keep 
;; RESOLVE_ROUTINE happy.
PRO eb_property
return
end


