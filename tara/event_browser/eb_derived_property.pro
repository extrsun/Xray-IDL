;+
;========================================================================
;;;
;;; FILE NAME:    $Id: eb_derived_property.pro 2877 2007-11-01 16:05:27Z patb $
;;;
;;; DESCRIPTION: Routines to define and compute derived event properties
;;;		 for X-ray Event Browser application
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Copyright (C) 1998, Pennsylvania State University
;-
;========================================================================

PRO AddDerivedProperty, name

@eb_common

CreateProperty, name, prop_ptr

dum = DerivedPropertyIsValid( *prop_ptr, /SET_UNDEFINED )
return
end

;========================================================================
;;; Run a modal widget which allows the user to edit the property's 
;;; definition, FITS column name, units, etc.
PRO EditDerivedProperty, name, group_leader
@eb_common

widget_control, /HOURGLASS
;if (eb_verbose GT 0) then print, 'Searching for definitions of the property ' + name

;; Make sure the existing property definition is valid.
valid = GetProperty( name, prop_ptr, /DEF_ONLY )
    
if (valid NE 1) then begin
  TimedMessage, dummy, 'The property '+name+' cannot be defined.', LIFE=10 
endif else begin
  ;; Make a copy of the property to record the existing definition.
  existing_prop     = *prop_ptr
  
  ;; Create a list of all valid property definitions (EB_PROPERTY structures).
  defns = replicate( *prop_ptr, 200 )
   
      
  ;; Search for all the valid definitions of the selected property, starting
  ;; with def_code 1.
  num_valid = 0
  this_code =  0
  repeat begin
    this_code = this_code + 1
    
    ;; Set def_search_in_progress to indicate that we're in search mode.
    (*prop_ptr).def_search_in_progress = 1
    
    if (this_code EQ existing_prop.def_code) then begin
      defns[num_valid] = existing_prop
      select_index     = num_valid
      num_valid        = num_valid + 1
    endif else begin
      valid = DerivedPropertyIsValid( (*prop_ptr), /DEF_ONLY, $
    				      DEF_CODE=this_code, /INIT_FITS)
    
      if (valid EQ 1) then begin
        defns[num_valid] = *prop_ptr
        num_valid = num_valid + 1
      endif
    endelse
  endrep until (valid EQ -1)
  
  (*prop_ptr).def_search_in_progress = 0
  
  defns = defns[0:num_valid-1]
  
    
  ;; Create the widgets.
  top_base = widget_base(TITLE='Property '+name, /MODAL, $
  			 GROUP_LEADER=group_leader, /COLUMN)

  ; Make a droplist with the def descriptions, and set it to the current
  ; definition.
  definition = widget_droplist(top_base, VALUE=defns.def_txt, $
  				TITLE='property definition:')
  widget_control, definition, SET_DROPLIST_SELECT=select_index

  cname = cw_field(top_base, TITLE='FITS column name if saved:',  $
  		   VALUE=existing_prop.col_name)
  
  ctxt  = cw_field(top_base, TITLE='property description:',  $
  		   VALUE=existing_prop.col_txt)
  
  uname = cw_field(top_base, TITLE='units:',  $
  		   VALUE=existing_prop.unit_name)
  
  utxt  = cw_field(top_base, TITLE='comment on units:',  $
  		   VALUE=existing_prop.unit_txt)


  wcs_object, existing_prop.wcs, IS_NULL=is_null_k, $
	      CTYP=ctyp_k, CRVL=crvl_k, CRPX=crpx_k, CDLT=cdlt_k, /GET
		
  is_null = widget_droplist(top_base, TITLE='world coordinate system', $
  			    VALUE=['enable','disable'])
  widget_control, is_null, SET_DROPLIST_SELECT=is_null_k
  
  wcs_base = widget_base(top_base, /FRAME, /COLUMN)
  ctyp  = cw_field(wcs_base, TITLE='TCTYP',            VALUE=ctyp_k)
  crvl  = cw_field(wcs_base, TITLE='TCRVL', /FLOATING, VALUE=crvl_k)
  crpx  = cw_field(wcs_base, TITLE='TCRPX', /FLOATING, VALUE=crpx_k)
  cdlt  = cw_field(wcs_base, TITLE='TCDLT', /FLOATING, VALUE=cdlt_k)
  widget_control, wcs_base, SENSITIVE=(is_null_k EQ 0)
  
  done = widget_button(top_base, VALUE='Done')
  
  ;; Setup state structure.
  state={ defns:defns, prop_ptr:prop_ptr, $
	 definition:definition, done:done, $
	 cname:cname, ctxt:ctxt, uname:uname, utxt:utxt, $
	 is_null:is_null, wcs_base:wcs_base, $
	 ctyp:ctyp, crvl:crvl, crpx:crpx, cdlt:cdlt}
  
  ;; Save state structure.
  widget_control, top_base, SET_UVALUE=ptr_new(state, /NO_CO)

  ;; Realize the widget.
  widget_control, top_base, /REALIZE

  xmanager, 'EditDerivedProperty', top_base, GROUP_LEADER=group_leader, $
	    /NO_BLOCK, EVENT_HANDLER='EditDerivedPropertyEvent'
endelse

return
end


;========================================================================
PRO EditDerivedPropertyEvent, event

;; Get the state structure. 
top_base = Event.handler
widget_control, top_base, GET_UVALUE=st

case event.id of
 (*st).definition: $
  begin
  ;; Update the cw_field widgets.
  widget_control, (*st).cname, SET_VAL=((*st).defns[event.index]).col_name
  widget_control, (*st).ctxt,  SET_VAL=((*st).defns[event.index]).col_txt
  widget_control, (*st).uname, SET_VAL=((*st).defns[event.index]).unit_name
  widget_control, (*st).utxt,  SET_VAL=((*st).defns[event.index]).unit_txt

  wcs_object, ((*st).defns[event.index]).wcs, IS_NULL=is_null_k, $
	      CTYP=ctyp_k, CRVL=crvl_k, CRPX=crpx_k, CDLT=cdlt_k, /GET
  widget_control, (*st).is_null, SET_DROPLIST_SELECT=is_null_k
  widget_control, (*st).ctyp, SET_VAL=ctyp_k
  widget_control, (*st).crvl, SET_VAL=crvl_k
  widget_control, (*st).crpx, SET_VAL=crpx_k
  widget_control, (*st).cdlt, SET_VAL=cdlt_k
  widget_control, (*st).wcs_base, SENSITIVE=(is_null_k EQ 0)
  end
  
 (*st).is_null: widget_control, (*st).wcs_base, SENSITIVE=(event.index EQ 0)
  
 (*st).done: $
  begin
  prop_ptr = (*st).prop_ptr
  
  ;; Change the property definition and FITS tags.
  index = widget_info( (*st).definition, /DROPLIST_SELECT )
  dum = DerivedPropertyIsValid( (*prop_ptr), /DEF_ONLY, /INIT_FITS, $
  				DEF_CODE=((*st).defns[index]).def_code )
  
  widget_control, (*st).cname, GET_VAL=cname
  (*prop_ptr).col_name = cname
  
  widget_control, (*st).ctxt,  GET_VAL=ctxt 
  (*prop_ptr).col_txt  = ctxt
  
  widget_control, (*st).uname, GET_VAL=uname
  (*prop_ptr).unit_name = uname
  
  widget_control, (*st).utxt,  GET_VAL=utxt 
  (*prop_ptr).unit_txt  = utxt 
  
  is_null = widget_info( (*st).is_null, /DROPLIST_SELECT )
  wcs_object, wcs, /INIT
  if (is_null EQ 0) then begin
    widget_control, (*st).ctyp, GET_VAL=ctyp_k
    widget_control, (*st).crvl, GET_VAL=crvl_k
    widget_control, (*st).crpx, GET_VAL=crpx_k
    widget_control, (*st).cdlt, GET_VAL=cdlt_k
    wcs_object, wcs, CTYP=ctyp_k, CRVL=crvl_k, CRPX=crpx_k, CDLT=cdlt_k
  endif
  (*prop_ptr).wcs = wcs

  widget_control, top_base, /DESTROY
  ptr_free, st
  end
endcase

return
end


;========================================================================
;;; This procedure assigns various members of a property by inheriting
;;; members of another property (the default) or by explicit values passed
;;; by keyword.
PRO AssignPropMembers, prop, cprop, $
			COL_NAME=col_name, COL_TXT=col_txt, $
			UNIT_NAME=unit_name, UNIT_TXT=unit_txt, $
			DEF_TXT=def_txt

if (n_elements(col_name) GT 0) 	then prop.col_name  = col_name $
				else prop.col_name  = (*cprop).col_name

if (n_elements(col_txt) GT 0) 	then prop.col_txt   = col_txt $
				else prop.col_txt   = (*cprop).col_txt

if (n_elements(unit_name) GT 0) then prop.unit_name = unit_name $
				else prop.unit_name = (*cprop).unit_name

if (n_elements(unit_txt) GT 0) 	then prop.unit_txt  = unit_txt $
				else prop.unit_txt  = (*cprop).unit_txt

if (n_elements(def_txt) GT 0) 	then prop.def_txt   = def_txt $
				else prop.def_txt   = (*cprop).name

return
end


;========================================================================
;;; This procedure defines a property as an alias for another property.
PRO AssignPropAlias, prop, cprop

;; Unless the previous data pointer was an alias, free it.
if (prop.alias_t EQ 0) then ptr_free, prop.data
        
;; Set up an alias data pointer.
prop.alias_t       = 1
prop.def_parameter = (*cprop).name
prop.data          = (*cprop).data
prop.wcs           = (*cprop).wcs

;; Move time forward (/INCREMENT) and record the epoch when this data was
;; computed.
prop.data_epoch    = GetEpoch( /INCREMENT )
return
end



;========================================================================
;;; This procedure defines a property as a vector of computed values.
PRO AssignPropData, prop, data, WCS=wcs
@eb_common

;; Unless the previous data pointer was an alias, free it.
if (eb_verbose GT 0) then print, prop.name, F='(A16," calculated")'
if (prop.alias_t EQ 0) then ptr_free, prop.data
        
;; Create a pointer to the supplied data.
prop.alias_t       = 0
prop.def_parameter = ''
prop.data = ptr_new( data, /NO_COPY )

if (n_elements(wcs) EQ 0) then wcs_object, wcs, /INIT
prop.wcs = wcs

;; Move time forward (/INCREMENT) and record the epoch when this data was
;; computed.
prop.data_epoch    = GetEpoch( /INCREMENT )
return
end


;========================================================================
;;; This procedure does lots of different things related to maintaining the
;;; definition of derived properties and calculating data values for them.
;;; All of the domain-specific knowledge regarding how derived properties
;;; can be computed from FITS properties, and indeed from other derived
;;; properties, is represented by the code here.
;;; 
;;; When a client of the property database wishes to obtain data for a
;;; derived property, a call to this procedure (with no keywords set) is
;;; made to verify that the current definition of the property is still
;;; valid, i.e. that the properties and parameters needed by that
;;; definition are available, and to actually perform the calculations
;;; necessary.
;;; 
;;; If the current definition of the property is valid, then this procedure
;;; makes sure that the property's data is up to date
;;; and returns the value 1.  If the property definition is NOT valid, then 
;;; zero is returned.  
;;; If prop.def_code has a value which does not refer to any possible 
;;; definition of the property then -1 is returned.
;;; 
;;; If INIT_FITS is set then property tags such as col_name are initialized
;;; based on the current def_code.
;;; 
;;; If DEF_ONLY is set, the property's definition is checked, but its data
;;; is NOT recomputed if it is stale.
;;;
;;; If DEF_CODE is supplied, then the property's definition is changed
;;; at the beginning, it's data is marked as stale, and the epoch is
;;; incremented to notify other properties that the property database
;;; has changed.

FUNCTION DerivedPropertyIsValid, prop, SET_UNDEFINED=set_undefined, $
	 DEF_ONLY=def_only, INIT_FITS=init_fits, DEF_CODE=def_code_kywd
	 
@eb_common

if keyword_set(set_undefined) then begin
  prop.def_code    = 10000
  prop.data_epoch  =  0
  prop.fresh_epoch =  0
  dum = GetEpoch( /INCREMENT )
  return, -1
endif

def_only  = keyword_set(def_only)
init_fits = (keyword_set(init_fits) OR (prop.def_txt EQ ''))

if keyword_set(def_code_kywd) then begin
  prop.def_code    = def_code_kywd
  prop.data_epoch  =  0
  prop.fresh_epoch =  0
  dum = GetEpoch( /INCREMENT )
endif

now = GetEpoch()

;; If we've validated this property's definition and data in the current 
;; epoch, then there's no need to do it again.
if (prop.fresh_epoch EQ now) then return, 1

stale = 0
num_events = CountXrayData()
dum = GetParameter('island_order', DATA=island_order)

case prop.name of
 ;--------------------------------------------------------------------------
 'row_number': $
  begin
  if (prop.def_code EQ 1) then begin
    valid = 1
    if init_fits then AssignPropMembers, prop, $
			COL_NAME='ROW_NUM', COL_TXT='row number', $
			UNIT_NAME='none', UNIT_TXT='', $
			DEF_TXT='row number'
		
    AssignPropData, prop, lindgen(num_events)
           
  endif else return, -1
  end


 ;--------------------------------------------------------------------------
 'x_position': $
  begin
  names = ['X','DETX','TDETX','CHIPX','RAWX','RAY_X']
  case 1 of
  (prop.def_code LE 6): $
   begin
   valid = GetProperty(names[prop.def_code-1], cprop, DEF_ONLY=def_only)

   if valid then begin
     if init_fits then AssignPropMembers, prop, cprop
     
     stale = (prop.data_epoch LT (*cprop).data_epoch)
     if (stale AND (NOT def_only)) then AssignPropAlias, prop, cprop
   endif ;valid   
   end
   
  (prop.def_code LE 12): $
   begin
   valid = GetProperty(names[prop.def_code-7], cprop, ONE_DATUM=datum) AND $
   	   (size(datum,/TYPE) LE 3)

   if valid then begin
     if init_fits then AssignPropMembers, prop, cprop, $
     			DEF_TXT=(*cprop).name + ', randomized'
     
     stale = (prop.data_epoch LT (*cprop).data_epoch)
     if (stale AND (NOT def_only)) then $
         AssignPropData, prop, *(*cprop).data + random(num_events)-0.5
   endif ;valid      
   end

  (prop.def_code EQ 13): $
   begin
   valid = GetProperty('TDETX',    tdetx,    DEF_ONLY=def_only) AND $
      	   GetProperty('exposure', exposure, DEF_ONLY=def_only) AND $
      	   GetParameter('x_dither_amplitude', amp,    DATA=A)   AND $
      	   GetParameter('x_dither_period',    period, DATA=P)   AND $
	   GetParameter('x_dither_null',      null,   DATA=N)

   if valid then begin
     if init_fits then AssignPropMembers, prop, tdetx, COL_NAME='X', $
     			COL_TXT='TDETX, de-dithered by Event Browser', $
			DEF_TXT='TDETX + A sin(2PI(exposure-null)/period)'
     
     stale = (prop.data_epoch LT $
		max([(*tdetx).data_epoch, (*exposure).data_epoch, $
       	             (*amp).data_epoch, (*period).data_epoch, $
       	             (*null).data_epoch]))
       	             
     if (stale AND (NOT def_only)) then begin
         dither = *A * sin( 2 * !PI * (*(*exposure).data - *N) / *P )
         AssignPropData, prop, (*(*tdetx).data - dither)
     endif
   endif ;valid      
   end
      
   else: return, -1
  endcase
  end

 ;--------------------------------------------------------------------------
 'y_position': $
  begin
  names = ['Y','DETY','TDETY','CHIPY','RAWY','RAY_Y']
  case 1 of
  (prop.def_code LE 6): $
   begin
   valid = GetProperty(names[prop.def_code-1], cprop, DEF_ONLY=def_only)

   if valid then begin
     if init_fits then AssignPropMembers, prop, cprop
     
     stale = (prop.data_epoch LT (*cprop).data_epoch)
     if (stale AND (NOT def_only)) then AssignPropAlias, prop, cprop
   endif ;valid   
   end
   
  (prop.def_code LE 12): $
   begin
   valid = GetProperty(names[prop.def_code-7], cprop, ONE_DATUM=datum) AND $
   	   (size(datum,/TYPE) LE 3)

   if valid then begin
     if init_fits then AssignPropMembers, prop, cprop, $
     			DEF_TXT=(*cprop).name + ', randomized'
     
     stale = (prop.data_epoch LT (*cprop).data_epoch)
     if (stale AND (NOT def_only)) then $
         AssignPropData, prop, *(*cprop).data + random(num_events)-0.5
   endif ;valid      
   end

  (prop.def_code EQ 13): $
   begin
   valid = GetProperty('TDETY',    tdety,    DEF_ONLY=def_only) AND $
      	   GetProperty('exposure', exposure, DEF_ONLY=def_only) AND $
      	   GetParameter('y_dither_amplitude', amp,    DATA=A)   AND $
      	   GetParameter('y_dither_period',    period, DATA=P)   AND $
	   GetParameter('y_dither_null',      null,   DATA=N)

   if valid then begin
     if init_fits then AssignPropMembers, prop, tdety, COL_NAME='Y', $
     			COL_TXT='TDETY, de-dithered by Event Browser', $
			DEF_TXT='-[TDETY + A sin(2PI(exposure-null)/period)]'
     
     stale = (prop.data_epoch LT $
		max([(*tdety).data_epoch, (*exposure).data_epoch, $
       	             (*amp).data_epoch, (*period).data_epoch, $
       	             (*null).data_epoch]))
       	             
     if (stale AND (NOT def_only)) then begin
         dither = *A * sin( 2 * !PI * (*(*exposure).data - *N) / *P )
         AssignPropData, prop, -(*(*tdety).data - dither)
     endif 
   endif ;valid      
   end
      
   else: return, -1
  endcase
  end


 ;--------------------------------------------------------------------------
 'timestamp': $
  begin
  case prop.def_code of
   ;; See if the components needed by this algorithm are available.
   1: $ 
   begin
   valid = GetProperty('TIME', time, DEF_ONLY=def_only)
   
   if valid then begin
     if init_fits then AssignPropMembers, prop, time, $
     			DEF_TXT='TIME - min(TIME)'
     
     stale = (prop.data_epoch LT (*time).data_epoch)
     if (stale AND (NOT def_only)) then begin
       min_time = min(*(*time).data)
       if (eb_verbose GT 0) then print, 'Minimum TIME value is ', min_time
       AssignPropData, prop, *(*time).data - min_time
     endif
   endif ;valid      
   end
           
   2: $ 
   begin
   valid = GetProperty('TIME', time, DEF_ONLY=def_only)
   
   if valid then begin
     if init_fits then AssignPropMembers, prop, time
     
     stale = (prop.data_epoch LT (*time).data_epoch)
     if (stale AND (NOT def_only)) then begin
         if (size(*(*time).data, /TNAME) EQ 'DOUBLE') then begin
           AssignPropData, prop, (*(*time).data+0)
         endif else begin
           AssignPropAlias, prop, time
         endelse        
     endif
   endif ;valid      
   end

   3: $ 
   begin
   valid = GetProperty('TIME', time, DEF_ONLY=def_only) AND $
   	   GetParameter('time_offset', time_offset)
             
   if valid then begin
     if init_fits then AssignPropMembers, prop, time, $
     			DEF_TXT='TIME - {time_offset}'
     
     stale = prop.data_epoch LT $
       	       max([(*time).data_epoch, (*time_offset).data_epoch])
     if (stale AND (NOT def_only)) then $
       AssignPropData, prop, *(*time).data - *(*time_offset).data
   endif ;valid      
   end
           
   else: return, -1
  endcase
  end

 ;--------------------------------------------------------------------------
 'island': $
  begin
  case prop.def_code of
   ;; See if the components needed by this algorithm are available.
   1: $ ;PHAS - BIAS - OVRCLK
   begin
   valid = GetProperty('PHAS',   phas, ONE_DATUM=datum) AND $
   	   GetProperty('BIAS',   bias, DEF_ONLY=def_only) AND $
   	   GetProperty('OVRCLK', ovck, DEF_ONLY=def_only)
   
   if valid then begin
     if init_fits then AssignPropMembers, prop, phas, $
     			DEF_TXT='PHAS - BIAS - OVRCLK'
     
     stale= prop.data_epoch LT $
              max([(*phas).data_epoch,(*bias).data_epoch,(*ovck).data_epoch])
     if (stale AND (NOT def_only)) then begin
         ;; Generally, the PHAS and BIAS columns are (9,N) and the OVRCLK
         ;; column is (1,N), so we need to expand the overclock to 2-D.
         width = n_elements(datum)
         
         if (width GT 1) then begin
           overclock = replicate(1,width) # *(*ovck).data
           AssignPropData, prop, *(*phas).data - *(*bias).data - overclock
         endif else begin
           AssignPropData, prop, *(*phas).data - *(*bias).data - *(*ovck).data
         endelse
         
         ; Pass on any TDIM information that describes PHAS order.
         prop.dim_spec   = (*phas).dim_spec
     endif
   endif ;valid      
   end
        
   2: $ ;PHAS
   begin
   valid = GetProperty('PHAS',   phas, DEF_ONLY=def_only) AND $
   	   (NOT GetProperty('BIAS',   /DEF_ONLY)) AND $
   	   (NOT GetProperty('OVRCLK', /DEF_ONLY))

   if valid then begin
     if init_fits then AssignPropMembers, prop, phas
     
     stale = (prop.data_epoch LT (*phas).data_epoch)
     if (stale AND (NOT def_only)) then begin
       AssignPropAlias, prop, phas
         
       ; Pass on any TDIM information that describes PHAS order.
       prop.dim_spec   = (*phas).dim_spec
     endif
   endif ;valid   
   end
   
   else: return, -1
  endcase
  end

 ;--------------------------------------------------------------------------
;;; Below the properties up_pix, down_pix, ... are extracted from the property
;;; "island".  For def_codes 1 below, the format of the nine (or 25) pixels
;;; in "island" is assumed to be ASCA format.
;;; The vector island(*,n) represents a 3x3 neighborhood around event n.
;;; The pixels are stored in the ASCA order shown below which was provided in
;;; a private communication with Ken Ebisawa.
;;; I cannot find an ASCA document that says this, although some MIT ACIS 
;;; documents state this format.
;;;                  ^
;;;                | 6 7 8
;;;   CHIPX=ccdRow | 4 0 5
;;;                | 1 2 3
;;;                |----------->
;;;                 CHIPX=ccdColumn
;;; 
;;; ACIS lab and ACIS-2chip islands generated by MIT are oriented with respect
;;; to the READX axis instead of the CHIPX axis in the picture above -- thus 
;;; events from amps B & D appear flipped left to right.  
;;; Alternate property definitions below will attempt to detect MIT-created  
;;; files and reorient neighborhoods from amps B & D to match the picture above.
;;;
;;; Continuous clocking mode results in a three-element island.
;;; Alternate property definitions below will handle this.
 ;--------------------------------------------------------------------------
 'center_pix': $
  begin
  ; We need a 2-D island property.
  case prop.def_code of
   1: valid = GetProperty('island', cprop, ONE_DATUM=datum) AND $
   	      (n_elements(datum) GT 1) AND $
   	      ((*island_order EQ 'ATICA') OR (*island_order EQ 'ASCA'))

   ; ASC modes
   2: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) EQ  9) AND (*island_order EQ 'ASC')
   3: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) EQ 25) AND (*island_order EQ 'ASC')
   else: return, -1
  endcase
    
  if valid then begin
    if init_fits then begin
      case prop.def_code of
       1: def_txt = 'island[0]'
       2: def_txt = 'island[4]'
       3: def_txt = 'island[12]'
      endcase
      AssignPropMembers, prop, cprop, $
			COL_NAME='CENTRPIX', COL_TXT='pixel at (CHIPX,CHIPY)', $
			DEF_TXT=def_txt
    endif
    
    stale = (prop.data_epoch LT (*cprop).data_epoch)
    if (stale AND (NOT def_only)) then begin
      case prop.def_code of
         1: AssignPropData, prop, reform(/OV,(*(*cprop).data)[ 0,*])
         2: AssignPropData, prop, reform(/OV,(*(*cprop).data)[ 4,*])
         3: AssignPropData, prop, reform(/OV,(*(*cprop).data)[12,*])
      endcase
    endif    
  endif ;valid   
  end

 ;--------------------------------------------------------------------------
 'up_pix': $
  begin
  case prop.def_code of
   ; We need 3x3 or 5x5 mode.
   1: valid = GetProperty('island', cprop, ONE_DATUM=datum) AND $
   	      (n_elements(datum) GE 9) AND $
   	      ((*island_order EQ 'ATICA') OR (*island_order EQ 'ASCA'))

   ; ASC modes
   2: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) EQ  9) AND (*island_order EQ 'ASC')
   3: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) EQ 25) AND (*island_order EQ 'ASC')
   else: return, -1
  endcase
    
  if valid then begin
    if init_fits then begin
      case prop.def_code of
       1: def_txt = 'island[7]'
       2: def_txt = 'island[7]'
       3: def_txt = 'island[17]'
      endcase
      AssignPropMembers, prop, cprop, $
			COL_NAME='UPPIX', COL_TXT='pixel at (CHIPX,CHIPY+1)', $
			DEF_TXT=def_txt
    endif
    
    stale = (prop.data_epoch LT (*cprop).data_epoch)
    if (stale AND (NOT def_only)) then begin
      case prop.def_code of
         1: AssignPropData, prop, reform(/OV,(*(*cprop).data)[ 7,*])
         2: AssignPropData, prop, reform(/OV,(*(*cprop).data)[ 7,*])
         3: AssignPropData, prop, reform(/OV,(*(*cprop).data)[17,*])
      endcase
    endif    
  endif ;valid   
  end

 ;--------------------------------------------------------------------------
 'down_pix': $
  begin
  case prop.def_code of
   ; We need 3x3 or 5x5 mode.
   1: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) GE 9) AND $
   	      ((*island_order EQ 'ATICA') OR (*island_order EQ 'ASCA'))

   ; ASC modes
   2: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) EQ  9) AND (*island_order EQ 'ASC')
   3: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) EQ 25) AND (*island_order EQ 'ASC')
   else: return, -1
  endcase
  
  if valid then begin
    if init_fits then begin
      case prop.def_code of
       1: def_txt = 'island[2]'
       2: def_txt = 'island[1]'
       3: def_txt = 'island[7]'
      endcase
      AssignPropMembers, prop, cprop, $
			COL_NAME='DOWNPIX', COL_TXT='pixel at (CHIPX,CHIPY-1)', $
			DEF_TXT=def_txt
    endif
    
    stale = (prop.data_epoch LT (*cprop).data_epoch)
    if (stale AND (NOT def_only)) then begin
      case prop.def_code of
         1: AssignPropData, prop, reform(/OV,(*(*cprop).data)[2,*])
         2: AssignPropData, prop, reform(/OV,(*(*cprop).data)[1,*])
         3: AssignPropData, prop, reform(/OV,(*(*cprop).data)[7,*])
      endcase
    endif    
  endif ;valid   
  end

 ;--------------------------------------------------------------------------
 'left_pix': $
  begin
  case prop.def_code of
   ; ATICA/MIT 3x3 or 5x5 mode.
   1: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) GE 9) AND (*island_order EQ 'ATICA')
   2: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
              GetProperty('amp_id', amp_id, DEF_ONLY=def_only) AND $
   	      (n_elements(datum) GE 9) AND (*island_order EQ 'ASCA')

   ; 1x3 mode.
   3: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) EQ 3) AND (*island_order EQ 'ATICA')
   
   ; ASC modes
   4: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) EQ  9) AND (*island_order EQ 'ASC')
   5: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) EQ 25) AND (*island_order EQ 'ASC')
   else: return, -1
  endcase
  
  if valid then begin
    if init_fits then begin
      col_txt = 'pixel at (CHIPX-1,CHIPY)'
      case prop.def_code of
       1: def_txt = 'island[4]'
       2: begin
          col_txt = 'pixel at (READX+-1,READY)'
          def_txt = 'island[4|5]'
          end
       3: def_txt = 'island[1]'
       4: def_txt = 'island[3]'
       5: def_txt = 'island[11]'
      endcase
      AssignPropMembers, prop, cprop, $
			COL_NAME='LEFTPIX', COL_TXT=col_txt, DEF_TXT=def_txt
    endif
    
    stale = (prop.data_epoch LT (*cprop).data_epoch)
    if (stale AND (NOT def_only)) then begin
      case prop.def_code of
         1: AssignPropData, prop, reform(/OV,(*(*cprop).data)[ 4,*])
         2: begin
            data  = (*(*cprop).data)[4,*]
            index = where(*(*amp_id).data EQ 1 OR *(*amp_id).data EQ 3, count)
            if (count GT 0) then data[index] = (*(*cprop).data)[5,index]
            AssignPropData, prop, reform(/OV, data)
            end
         3: AssignPropData, prop, reform(/OV,(*(*cprop).data)[ 1,*])
         4: AssignPropData, prop, reform(/OV,(*(*cprop).data)[ 3,*])
         5: AssignPropData, prop, reform(/OV,(*(*cprop).data)[11,*])
      endcase
    endif    
  endif ;valid   
  end

 ;--------------------------------------------------------------------------
 'right_pix': $
  begin
  case prop.def_code of
   ; ATICA/MIT 3x3 or 5x5 mode.
   1: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) GE 9) AND (*island_order EQ 'ATICA')
   2: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
              GetProperty('amp_id', amp_id, DEF_ONLY=def_only) AND $
   	      (n_elements(datum) GE 9) AND (*island_order EQ 'ASCA')

   ; 1x3 mode.
   3: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) EQ 3) AND (*island_order EQ 'ATICA')
   
   ; ASC modes
   4: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) EQ  9) AND (*island_order EQ 'ASC')
   5: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) EQ 25) AND (*island_order EQ 'ASC')
   else: return, -1
  endcase
  
  if valid then begin
    if init_fits then begin
      col_txt = 'pixel at (CHIPX+1,CHIPY)'
      case prop.def_code of
       1: def_txt = 'island[5]'
       2: begin
          col_txt = 'pixel at (READX+-1,READY)'
          def_txt = 'island[5|4]'
          end
       3: def_txt = 'island[2]'
       4: def_txt = 'island[5]'
       5: def_txt = 'island[13]'
      endcase
      AssignPropMembers, prop, cprop, $
			COL_NAME='RIGHTPIX', COL_TXT=col_txt, DEF_TXT=def_txt
    endif
    
    stale = (prop.data_epoch LT (*cprop).data_epoch)
    if (stale AND (NOT def_only)) then begin
      case prop.def_code of
         1: AssignPropData, prop, reform(/OV,(*(*cprop).data)[ 5,*])
         2: begin
            data  = (*(*cprop).data)[5,*]
            index = where(*(*amp_id).data EQ 1 OR *(*amp_id).data EQ 3, count)
            if (count GT 0) then data[index] = (*(*cprop).data)[4,index]
            AssignPropData, prop, reform(/OV, data)
            end
         3: AssignPropData, prop, reform(/OV,(*(*cprop).data)[ 2,*])
         4: AssignPropData, prop, reform(/OV,(*(*cprop).data)[ 5,*])
         5: AssignPropData, prop, reform(/OV,(*(*cprop).data)[13,*])
      endcase
    endif    
  endif ;valid   
  end

 ;--------------------------------------------------------------------------
 'ul_pix': $
  begin
  case prop.def_code of
   ; ATICA/MIT 3x3 or 5x5 mode.
   1: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) GE 9) AND (*island_order EQ 'ATICA')
   2: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
              GetProperty('amp_id', amp_id, DEF_ONLY=def_only) AND $
   	      (n_elements(datum) GE 9) AND (*island_order EQ 'ASCA')

   ; ASC modes
   3: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) EQ  9) AND (*island_order EQ 'ASC')
   4: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) EQ 25) AND (*island_order EQ 'ASC')
   else: return, -1
  endcase
  
  if valid then begin
    if init_fits then begin
      col_txt = 'pixel at (CHIPX-1,CHIPY+1)'
      case prop.def_code of
       1: def_txt = 'island[6]'
       2: begin
          col_txt = 'pixel at (READX+-1,READY+1)'
          def_txt = 'island[6|8]'
          end
       3: def_txt = 'island[6]'
       4: def_txt = 'island[16]'
      endcase
      AssignPropMembers, prop, cprop, $
			COL_NAME='ULPIX', COL_TXT=col_txt, DEF_TXT=def_txt
    endif
    
    stale = (prop.data_epoch LT (*cprop).data_epoch)
    if (stale AND (NOT def_only)) then begin
      case prop.def_code of
         1: AssignPropData, prop, reform(/OV,(*(*cprop).data)[ 6,*])
         2: begin
            data  = (*(*cprop).data)[6,*]
            index = where(*(*amp_id).data EQ 1 OR *(*amp_id).data EQ 3, count)
            if (count GT 0) then data[index] = (*(*cprop).data)[8,index]
            AssignPropData, prop, reform(/OV, data)
            end
         3: AssignPropData, prop, reform(/OV,(*(*cprop).data)[ 6,*])
         4: AssignPropData, prop, reform(/OV,(*(*cprop).data)[16,*])
      endcase
    endif    
  endif ;valid   
  end

 ;--------------------------------------------------------------------------
 'ur_pix': $
  begin
  case prop.def_code of
   ; ATICA/MIT 3x3 or 5x5 mode.
   1: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) GE 9) AND (*island_order EQ 'ATICA')
   2: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
              GetProperty('amp_id', amp_id, DEF_ONLY=def_only) AND $
   	      (n_elements(datum) GE 9) AND (*island_order EQ 'ASCA')

   ; ASC modes
   3: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) EQ  9) AND (*island_order EQ 'ASC')
   4: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) EQ 25) AND (*island_order EQ 'ASC')
   else: return, -1
  endcase
  
  if valid then begin
    if init_fits then begin
      col_txt = 'pixel at (CHIPX+1,CHIPY+1)'
      case prop.def_code of
       1: def_txt = 'island[8]'
       2: begin
          col_txt = 'pixel at (READX+-1,READY+1)'
          def_txt = 'island[8|6]'
          end
       3: def_txt = 'island[8]'
       4: def_txt = 'island[18]'
      endcase
      AssignPropMembers, prop, cprop, $
			COL_NAME='URPIX', COL_TXT=col_txt, DEF_TXT=def_txt
    endif
    
    stale = (prop.data_epoch LT (*cprop).data_epoch)
    if (stale AND (NOT def_only)) then begin
      case prop.def_code of
         1: AssignPropData, prop, reform(/OV,(*(*cprop).data)[ 8,*])
         2: begin
            data  = (*(*cprop).data)[8,*]
            index = where(*(*amp_id).data EQ 1 OR *(*amp_id).data EQ 3, count)
            if (count GT 0) then data[index] = (*(*cprop).data)[6,index]
            AssignPropData, prop, reform(/OV, data)
            end
         3: AssignPropData, prop, reform(/OV,(*(*cprop).data)[ 8,*])
         4: AssignPropData, prop, reform(/OV,(*(*cprop).data)[18,*])
      endcase
    endif    
  endif ;valid   
  end


 ;--------------------------------------------------------------------------
 'll_pix': $
  begin
  case prop.def_code of
   ; ATICA/MIT 3x3 or 5x5 mode.
   1: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) GE 9) AND (*island_order EQ 'ATICA')
   2: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
              GetProperty('amp_id', amp_id, DEF_ONLY=def_only) AND $
   	      (n_elements(datum) GE 9) AND (*island_order EQ 'ASCA')

   ; ASC modes
   3: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) EQ  9) AND (*island_order EQ 'ASC')
   4: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) EQ 25) AND (*island_order EQ 'ASC')
   else: return, -1
  endcase
  
  if valid then begin
    if init_fits then begin
      col_txt = 'pixel at (CHIPX-1,CHIPY-1)'
      case prop.def_code of
       1: def_txt = 'island[1]'
       2: begin
          col_txt = 'pixel at (READX+-1,READY-1)'
          def_txt = 'island[1|3]'
          end
       3: def_txt = 'island[0]'
       4: def_txt = 'island[6]'
      endcase
      AssignPropMembers, prop, cprop, $
			COL_NAME='LLPIX', COL_TXT=col_txt, DEF_TXT=def_txt
    endif
    
    stale = (prop.data_epoch LT (*cprop).data_epoch)
    if (stale AND (NOT def_only)) then begin
      case prop.def_code of
         1: AssignPropData, prop, reform(/OV,(*(*cprop).data)[1,*])
         2: begin
            data  = (*(*cprop).data)[1,*]
            index = where(*(*amp_id).data EQ 1 OR *(*amp_id).data EQ 3, count)
            if (count GT 0) then data[index] = (*(*cprop).data)[3,index]
            AssignPropData, prop, reform(/OV, data)
            end
         3: AssignPropData, prop, reform(/OV,(*(*cprop).data)[0,*])
         4: AssignPropData, prop, reform(/OV,(*(*cprop).data)[6,*])
      endcase
    endif    
  endif ;valid   
  end


 ;--------------------------------------------------------------------------
 'lr_pix': $
  begin
  case prop.def_code of
   ; ATICA/MIT 3x3 or 5x5 mode.
   1: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) GE 9) AND (*island_order EQ 'ATICA')
   2: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
              GetProperty('amp_id', amp_id, DEF_ONLY=def_only) AND $
   	      (n_elements(datum) GE 9) AND (*island_order EQ 'ASCA')

   ; ASC modes
   3: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) EQ  9) AND (*island_order EQ 'ASC')
   4: valid = GetProperty('island', cprop, ONE_DATUM=datum)  AND $
   	      (n_elements(datum) EQ 25) AND (*island_order EQ 'ASC')
   else: return, -1
  endcase
  
  if valid then begin
    if init_fits then begin
      col_txt = 'pixel at (CHIPX+1,CHIPY-1)'
      case prop.def_code of
       1: def_txt = 'island[3]'
       2: begin
          col_txt = 'pixel at (READX+-1,READY-1)'
          def_txt = 'island[3|1]'
          end
       3: def_txt = 'island[2]'
       4: def_txt = 'island[8]'
      endcase
      AssignPropMembers, prop, cprop, $
			COL_NAME='LRPIX', COL_TXT=col_txt, DEF_TXT=def_txt
    endif
    
    stale = (prop.data_epoch LT (*cprop).data_epoch)
    if (stale AND (NOT def_only)) then begin
      case prop.def_code of
         1: AssignPropData, prop, reform(/OV,(*(*cprop).data)[3,*])
         2: begin
            data  = (*(*cprop).data)[3,*]
            index = where(*(*amp_id).data EQ 1 OR *(*amp_id).data EQ 3, count)
            if (count GT 0) then data[index] = (*(*cprop).data)[1,index]
            AssignPropData, prop, reform(/OV, data)
            end
         3: AssignPropData, prop, reform(/OV,(*(*cprop).data)[2,*])
         4: AssignPropData, prop, reform(/OV,(*(*cprop).data)[8,*])
      endcase
    endif    
  endif ;valid   
  end


 ;--------------------------------------------------------------------------
;;; The algorithm for computing an event amplitude and grade is listed
;;; in the ACIS Software Requirements Specification (Nov 95), sections
;;; 3.2.2.3.14 and 3.2.2.3.16 and in the IP&CL Notes (Rev B), section 4.7.
 'pha': $
  begin
  case prop.def_code of
   ;; See if the components needed by this algorithm are available.
   1: $ 
   begin
   valid = GetProperty('PHA', pha, DEF_ONLY=def_only)
   
   if valid then begin
     if init_fits then AssignPropMembers, prop, pha
     
     stale = (prop.data_epoch LT (*pha).data_epoch)
     if (stale AND (NOT def_only)) then AssignPropAlias, prop, pha
   endif ;valid   
   end
   
   2: $ 
   begin
   valid = GetProperty('center_pix',center_pix,DEF_ONLY=def_only) AND $
	   GetProperty('up_pix',      up_pix,  DEF_ONLY=def_only) AND $
   	   GetProperty('down_pix',   down_pix, DEF_ONLY=def_only) AND $
   	   GetProperty('left_pix',   left_pix, DEF_ONLY=def_only) AND $
   	   GetProperty('right_pix', right_pix, DEF_ONLY=def_only) AND $
   	   GetProperty('ul_pix',       ul_pix, DEF_ONLY=def_only) AND $
   	   GetProperty('ur_pix',       ur_pix, DEF_ONLY=def_only) AND $
   	   GetProperty('ll_pix',       ll_pix, DEF_ONLY=def_only) AND $
   	   GetProperty('lr_pix',       lr_pix, DEF_ONLY=def_only) AND $
   	   GetParameter('split_event_threshold', spth)
   
   if valid then begin
     if init_fits then AssignPropMembers, prop, $
			COL_NAME='PHA', COL_TXT='event amplitude', $
			UNIT_NAME='DN', UNIT_TXT='', $
         DEF_TXT='f( 9 island pixels, {split_event_threshold} )'
     
     stale= prop.data_epoch LT  $
              max([(*spth).data_epoch,    (*center_pix).data_epoch, $
                   (*up_pix).data_epoch,  (*down_pix).data_epoch, $
                   (*left_pix).data_epoch,(*right_pix).data_epoch, $
                   (*ul_pix).data_epoch,  (*ur_pix).data_epoch, $
                   (*ll_pix).data_epoch,  (*lr_pix).data_epoch ])
     if (stale AND (NOT def_only)) then begin
         st = *(*spth).data
         if (eb_verbose GT 0) then print, 'split_event_threshold is ', st
         
         ;; Start with the central pixel.
         pha = *(*center_pix).data
         
         ;; Figure out which up, down, left, & right pixels are above ST.
         up_mask    = (*(*up_pix).data    GE st)
         down_mask  = (*(*down_pix).data  GE st)
         left_mask  = (*(*left_pix).data  GE st)
         right_mask = (*(*right_pix).data GE st)
         
         ;; Add in the corner pixels that are above ST, under the ACIS
         ;; restriction that a corner pixel counts only if one of its
         ;; neighbors also is above the ST.
         mask = (*(*ll_pix).data GE st) AND (down_mask OR left_mask)
         pha = temporary(pha) + *(*ll_pix).data * temporary(mask)
         
         mask = (*(*lr_pix).data GE st) AND (down_mask OR right_mask)
         pha = temporary(pha) + *(*lr_pix).data * temporary(mask)
         
         mask = (*(*ul_pix).data GE st) AND (up_mask   OR left_mask)
         pha = temporary(pha) + *(*ul_pix).data * temporary(mask)
         
         mask = (*(*ur_pix).data GE st) AND (up_mask   OR right_mask)
         pha = temporary(pha) + *(*ur_pix).data * temporary(mask)
         
         ;; Finally add in the up, down, left & right pixels that are above ST.
         pha = temporary(pha) + *(*up_pix).data    * temporary(up_mask)
         pha = temporary(pha) + *(*down_pix).data  * temporary(down_mask)
         pha = temporary(pha) + *(*left_pix).data  * temporary(left_mask)
         pha = temporary(pha) + *(*right_pix).data * temporary(right_mask)
         
         AssignPropData, prop, pha
     endif
   endif ;valid      
   end
                      
   3: $ 
   begin
   valid = GetProperty('center_pix',center_pix, DEF_ONLY=def_only) AND $
	   GetProperty('left_pix',  left_pix,   DEF_ONLY=def_only) AND $
   	   GetProperty('right_pix', right_pix,  DEF_ONLY=def_only) AND $
	   (NOT GetProperty('up_pix', /DEF_ONLY)) AND $
   	   GetParameter('split_event_threshold', spth)
   
   if valid then begin
     if init_fits then AssignPropMembers, prop, $
			COL_NAME='PHA', COL_TXT='event amplitude', $
			UNIT_NAME='DN', UNIT_TXT='', $
         DEF_TXT='f( 3 island pixels, {split_event_threshold} )'
    
     stale= prop.data_epoch LT  $
              max([(*spth).data_epoch,     (*center_pix).data_epoch, $
                   (*left_pix).data_epoch, (*right_pix).data_epoch])
     if (stale AND (NOT def_only)) then begin
         st = *(*spth).data
         if (eb_verbose GT 0) then print, 'split_event_threshold is ', st

         ;; Start with the central pixel.
         pha = *(*center_pix).data

         ;; Add in the left & right pixels that are above ST.
         pha = temporary(pha) + *(*left_pix).data  * (*(*left_pix).data  GE st)
         pha = temporary(pha) + *(*right_pix).data * (*(*right_pix).data GE st)

         AssignPropData, prop, pha
     endif
   endif ;valid      
   end
           
   else: return, -1
  endcase
  end

 ;--------------------------------------------------------------------------
 'energy': $
  begin
  case prop.def_code of
   ;; See if the components needed by this algorithm are available.
   1: $ 
   begin
   valid = GetProperty('ENERGY', energy, DEF_ONLY=def_only)
   
   if valid then begin
     if init_fits then AssignPropMembers, prop, energy
     
     stale = (prop.data_epoch LT (*energy).data_epoch)
     if (stale AND (NOT def_only)) then AssignPropAlias, prop, energy
   endif ;valid   
   end
   
   2: $ 
   begin
   valid = GetProperty('PI', pi, DEF_ONLY=def_only) AND $
   	   GetParameter('ev_per_dn',    ev_per_dn)    AND $
   	   GetParameter('ev_dn_offset', ev_dn_offset)
   
   if valid then begin
     if init_fits then AssignPropMembers, prop, $
			COL_NAME='ENERGY', COL_TXT='event energy', $
			UNIT_NAME='eV', UNIT_TXT='', $
		DEF_TXT='f( PI, {ev_per_dn}, {ev_dn_offset} )'
     
     stale= prop.data_epoch LT $
       	      max([(*ev_per_dn).data_epoch, (*ev_dn_offset).data_epoch, $
       	           (*pi).data_epoch])
       
     if (stale AND (NOT def_only)) then begin
         if (eb_verbose GT 0) then print, 'ev_per_dn is ',    *(*ev_per_dn).data
         if (eb_verbose GT 0) then print, 'ev_dn_offset is ', *(*ev_dn_offset).data
          
         ; Apply the linear DN/EV conversion model.
         ; We will dither the DN amplitudes over [-0.5,0.5] to make eV 
         ; values that are not bunched up.
         dither = random(n_elements(*(*pi).data)) - 0.5
         
         energy = *(*ev_per_dn).data * (dither + *(*pi).data) +  $
         	  *(*ev_dn_offset).data
         AssignPropData, prop, energy
     endif
   endif ;valid      
   end

   3: $ 
   begin
   valid = GetProperty('pha',    pha,    DEF_ONLY=def_only) AND $
	   GetProperty('ccd_id', ccd_id, DEF_ONLY=def_only) AND $
   	   GetProperty('amp_id', amp_id, DEF_ONLY=def_only) AND $
   	   GetParameter('dn_ev_file', dn_ev_file)
   
   if valid then begin
     ;; Make sure dn2ev file exists.
     if (*(*dn_ev_file).data EQ '') then begin
       valid = 0
     endif else begin
       dum=findfile(*(*dn_ev_file).data,COUNT=count)
       valid = (count EQ 1)
       if (valid EQ 0) then begin
	 print, 'WARNING! dn_ev_file "'+*(*dn_ev_file).data+'" not found! Trying again ...'
	 help, dum, count

         dum=findfile(*(*dn_ev_file).data,COUNT=count)
         valid = (count EQ 1)
         if (valid EQ 0) then $
	   print, 'WARNING! dn_ev_file "'+*(*dn_ev_file).data+'" not found!'
       endif
     endelse
   endif
   
   if valid then begin
     if init_fits then AssignPropMembers, prop, $
			COL_NAME='ENERGY', COL_TXT='event energy', $
			UNIT_NAME='eV', UNIT_TXT='', $
		DEF_TXT='f( pha, ccd_id, amp_id, {dn_ev_file} )'
    
     stale= prop.data_epoch LT  $
              max([(*dn_ev_file).data_epoch, (*pha).data_epoch, $
                   (*ccd_id).data_epoch,     (*amp_id).data_epoch])
       
     if (stale AND (NOT def_only)) then begin
         if (eb_verbose GT 0) then print, 'dn_ev_file is ', *(*dn_ev_file).data
         
         table  = (read_ascii( *(*dn_ev_file).data, COMMENT=';' )).(0)
	 ccd_entry = byte( table[0,*] )
	 amp_entry = byte( table[1,*] )
	 evpdn_value     = table[2,*]
	 evdn_offset     = table[3,*]

	 max_ccd_entry = max(ccd_entry)
	 max_amp_entry = max(amp_entry)

	 if ( max(*(*ccd_id).data) GT max_ccd_entry OR $
	      max(*(*amp_id).data) GT max_amp_entry ) then begin

	   valid = 0
	   msg=['Could not find DN/EV conversion for all CCD/AMP combinations.',$
	        'The property '+name+' cannot be defined.']
	   TimedMessage, dummy, msg, LIFE=10 
	 endif else begin
	   ; Make 2-D tables for the gains and offsets where one dimension 
	   ; represents CCD_IDs and the other represents AMP_IDs.
	   gains   = fltarr( max_ccd_entry+1, max_amp_entry+1 ) 
	   offsets = fltarr( max_ccd_entry+1, max_amp_entry+1 )

	   gains  ( ccd_entry, amp_entry ) = evpdn_value
	   offsets( ccd_entry, amp_entry ) = evdn_offset
  
           ; For each event, apply the correct linear DN/EV conversion model.
           ; We will dither the DN amplitudes over [-0.5,0.5] to make eV 
           ; values that are not bunched up.
           gain   = gains  [*(*ccd_id).data, *(*amp_id).data]
           offset = offsets[*(*ccd_id).data, *(*amp_id).data]
           dither = random(n_elements(*(*pha).data)) - 0.5
           
	   energy = (dither + *(*pha).data) * gain + offset
           AssignPropData, prop, energy
         endelse
     endif
   endif ;valid      
   end
           
   4: $ 
   begin
   valid = GetProperty('pha', pha, DEF_ONLY=def_only) AND $
   	   GetParameter('ev_per_dn',    ev_per_dn)    AND $
   	   GetParameter('ev_dn_offset', ev_dn_offset)
   
   if valid then begin
     if init_fits then AssignPropMembers, prop, $
			COL_NAME='ENERGY', COL_TXT='event energy', $
			UNIT_NAME='eV', UNIT_TXT='', $
		DEF_TXT='f( pha, {ev_per_dn}, {ev_dn_offset} )'
 
     stale= prop.data_epoch LT $
       	      max([(*ev_per_dn).data_epoch, (*ev_dn_offset).data_epoch, $
       	           (*pha).data_epoch])
       
     if (stale AND (NOT def_only)) then begin
         if (eb_verbose GT 0) then print, 'ev_per_dn is ',    *(*ev_per_dn).data
         if (eb_verbose GT 0) then print, 'ev_dn_offset is ', *(*ev_dn_offset).data
          
         ; Apply the linear DN/EV conversion model.
         ; We will dither the DN amplitudes over [-0.5,0.5] to make eV 
         ; values that are not bunched up.
         dither = random(n_elements(*(*pha).data)) - 0.5
         
         energy = *(*ev_per_dn).data * (dither + *(*pha).data) +  $
         	  *(*ev_dn_offset).data
         AssignPropData, prop, energy
     endif
   endif ;valid      
   end
           
   ;;; Conversion from eV to wavelength (in angstroms) is:
   ;;;  wavelength (angstrom) = 1.24E4 / energy (eV)
   5: $ 
   begin
   valid = GetProperty('wavelength', wavelength, DEF_ONLY=def_only)
   
   if valid then begin
     if init_fits then AssignPropMembers, prop, $
			COL_NAME='ENERGY', COL_TXT='event energy', $
			UNIT_NAME='eV', UNIT_TXT='', $
		DEF_TXT='f( wavelength )'
		
     stale= prop.data_epoch LT (*wavelength).data_epoch
             
     if (stale AND (NOT def_only)) then begin
         AssignPropData, prop, 1.24E4 * (*(*wavelength).data)
     endif
   endif ;valid      
   end

   else: return, -1
  endcase
  end

 ;--------------------------------------------------------------------------
 'wavelength': $
  begin
  case prop.def_code of
   ;; See if the components needed by this algorithm are available.
   1: $ 
   begin
   valid = GetProperty('TG_LAM', tg_lam, DEF_ONLY=def_only)
   
   if valid then begin
     if init_fits then AssignPropMembers, prop, tg_lam
     
     stale = (prop.data_epoch LT (*tg_lam).data_epoch)
     if (stale AND (NOT def_only)) then AssignPropAlias, prop, tg_lam
   endif ;valid   
   end
   
   ;;; Conversion from eV to wavelength (in angstroms) is:
   ;;;  wavelength (angstrom) = 1.24E4 / energy (eV)
   2: $ 
   begin
   valid = GetProperty('energy', energy, DEF_ONLY=def_only)
   
   if valid then begin
     if init_fits then AssignPropMembers, prop, $
			COL_NAME='LAMBDA', COL_TXT='event wavelength', $
			UNIT_NAME='angstrom', UNIT_TXT='', $
			DEF_TXT='f( energy )'
		
     stale= prop.data_epoch LT (*energy).data_epoch
             
     if (stale AND (NOT def_only)) then begin
         AssignPropData, prop, 1.24E4 / *(*energy).data
     endif
   endif ;valid      
   end
                      
   else: return, -1
  endcase
  end


 ;--------------------------------------------------------------------------
 'pi': $
  begin
  case prop.def_code of
   ;; See if the components needed by this algorithm are available.
   1: $ 
   begin
   valid = GetProperty('PI', pi, DEF_ONLY=def_only)
   
   if valid then begin
     if init_fits then AssignPropMembers, prop, pi
     
     stale = (prop.data_epoch LT (*pi).data_epoch)
     if (stale AND (NOT def_only)) then AssignPropAlias, prop, pi
   endif ;valid   
   end
   
   ;;; Conversion from eV to PI (a 1-based integer channel system) is:
   ;;;  pi = ceil( energy/ev_per_pi )
   2: $ 
   begin
   valid = GetProperty('energy', energy, DEF_ONLY=def_only) AND $
   	   GetParameter('ev_per_pi', ev_per_pi)   
   	    
   if valid then begin
     col_txt=string(*(*ev_per_pi).data, F='("ceil( energy/",F5.2," )")')
     if init_fits then AssignPropMembers, prop, $
			COL_NAME='PI', COL_TXT=col_txt, $
			UNIT_NAME='none', UNIT_TXT='', $
			DEF_TXT='f( energy, {ev_per_pi} )'
		
     stale= prop.data_epoch LT $
       	      max([(*ev_per_pi).data_epoch, (*energy).data_epoch])
       	                
     if (stale AND (NOT def_only)) then begin
         if (eb_verbose GT 0) then print, 'ev_per_pi is ',    *(*ev_per_pi).data
                   
         AssignPropData, prop, fix(ceil( *(*energy).data / *(*ev_per_pi).data ))
     endif
   endif ;valid      
   end
                      
   else: return, -1
  endcase
  end


 ;--------------------------------------------------------------------------
 'grating_order': $
  begin
  case prop.def_code of
   ;; See if the components needed by this algorithm are available.
   1: $ 
   begin
   valid = 0
   end
           
   else: return, -1
  endcase
  end

 ;--------------------------------------------------------------------------
 'instrument_grade': $
  begin
  case prop.def_code of
   ;; See if the components needed by this algorithm are available.
   1: $ 
   begin
   valid = GetProperty('FLTGRADE', fltgrade, DEF_ONLY=def_only)
   
   if valid then begin
     if init_fits then AssignPropMembers, prop, fltgrade
     
     stale = (prop.data_epoch LT (*fltgrade).data_epoch)
     if (stale AND (NOT def_only)) then AssignPropAlias, prop, fltgrade
   endif ;valid   
   end

   2: $ 
   begin
   valid = GetProperty('up_pix',      up_pix,  DEF_ONLY=def_only) AND $
   	   GetProperty('down_pix',   down_pix, DEF_ONLY=def_only) AND $
   	   GetProperty('left_pix',   left_pix, DEF_ONLY=def_only) AND $
   	   GetProperty('right_pix', right_pix, DEF_ONLY=def_only) AND $
   	   GetProperty('ul_pix',       ul_pix, DEF_ONLY=def_only) AND $
   	   GetProperty('ur_pix',       ur_pix, DEF_ONLY=def_only) AND $
   	   GetProperty('ll_pix',       ll_pix, DEF_ONLY=def_only) AND $
   	   GetProperty('lr_pix',       lr_pix, DEF_ONLY=def_only) AND $
   	   GetParameter('split_event_threshold', spth)
   
   if valid then begin
     if init_fits then AssignPropMembers, prop, $
			COL_NAME='FLTGRADE', COL_TXT='ACIS grade', $
			UNIT_NAME='none', UNIT_TXT='', $
	DEF_TXT='f( 8 neighbor pixels, {split_event_threshold} )'
 
     stale= prop.data_epoch LT  $
              max([(*spth).data_epoch, $
                   (*up_pix).data_epoch,  (*down_pix).data_epoch, $
                   (*left_pix).data_epoch,(*right_pix).data_epoch, $
                   (*ul_pix).data_epoch,  (*ur_pix).data_epoch, $
                   (*ll_pix).data_epoch,  (*lr_pix).data_epoch ])
       
     if (stale AND (NOT def_only)) then begin
         st = *(*spth).data
         if (eb_verbose GT 0) then print, 'split_event_threshold is ', st
	 grade =  '01'XB * (*(*ll_pix).data    GE st) + $
	 	  '02'XB * (*(*down_pix).data  GE st) + $
	 	  '04'XB * (*(*lr_pix).data    GE st) + $
	 	  '08'XB * (*(*left_pix).data  GE st) + $
	 	  '10'XB * (*(*right_pix).data GE st) + $
	 	  '20'XB * (*(*ul_pix).data    GE st) + $
	 	  '40'XB * (*(*up_pix).data    GE st) + $
	 	  '80'XB * (*(*ur_pix).data    GE st)
	
         AssignPropData, prop, grade
     endif
   endif ;valid      
   end
           
   3: $ 
   begin
   valid = GetProperty('left_pix',  left_pix,  DEF_ONLY=def_only) AND $
   	   GetProperty('right_pix', right_pix, DEF_ONLY=def_only) AND $
	   (NOT GetProperty('up_pix', /DEF_ONLY)) AND $
   	   GetParameter('split_event_threshold', spth)
   
   if valid then begin
     if init_fits then AssignPropMembers, prop, $
			COL_NAME='FLTGRADE', COL_TXT='ACIS grade', $
			UNIT_NAME='none', UNIT_TXT='', $
	DEF_TXT='f( 2 neighbor pixels, {split_event_threshold} )'
 
     stale= prop.data_epoch LT  $
              max([(*spth).data_epoch, $
                   (*left_pix).data_epoch, (*right_pix).data_epoch])
       
     if (stale AND (NOT def_only)) then begin
         st = *(*spth).data
         if (eb_verbose GT 0) then print, 'split_event_threshold is ', st
         
	 grade =  '08'XB * (*(*left_pix).data  GE st) + $
	 	  '10'XB * (*(*right_pix).data GE st)	
         AssignPropData, prop, grade
     endif
   endif ;valid      
   end
           
   else: return, -1
  endcase
  end

 ;--------------------------------------------------------------------------
 'asca_grade': $
  begin
  case prop.def_code of
   ;; See if the components needed by this algorithm are available.
   1: $ 
   begin
   valid = GetProperty('GRADE', grade, DEF_ONLY=def_only)
   
   if valid then begin
     if init_fits then AssignPropMembers, prop, grade
     
     stale = (prop.data_epoch LT (*grade).data_epoch)
     if (stale AND (NOT def_only)) then AssignPropAlias, prop, grade
   endif ;valid   
   end

   2: $ 
   begin
   valid = GetProperty('instrument_grade', grade, DEF_ONLY=def_only)
           
   if valid then begin
     if init_fits then AssignPropMembers, prop, $
			COL_NAME='GRADE', COL_TXT='ASCA grade', $
			UNIT_NAME='none', UNIT_TXT='', $
			DEF_TXT='f( instrument_grade, amp_id )'
    
     stale= (prop.data_epoch LT (*grade).data_epoch)
     
     amp_available = GetProperty('amp_id', amp_id, DEF_ONLY=def_only)
     if amp_available then begin
       stale = stale OR (prop.data_epoch LT (*amp_id).data_epoch)
     endif 
       
     if (stale AND (NOT def_only)) then begin
	 ;; table[0,*] is for amps 0 & 2, table[1,*] is for amps 1 & 3
	 table = replicate(7B, 2, 256)
	 
	 table[ *, [0] ] = 0
	 table[ *, [1,4,5,32,33,36,37,128,129,132,133,160,161,164,165] ] = 1
	 table[ *, [2,34,64,65,68,69,130,162] ] = 2
	 
	 table[ 0, [8,12,136,140] ] = 3
	 table[ 1, [8,12,136,140] ] = 4
	 table[ 0, [16,17,48,49]  ] = 4
	 table[ 1, [16,17,48,49]  ] = 3
	 
	 table[ *, [3,6,9,13,20,21,35,38,40,44,52,53,96,97,100,101,131,134,137,$
	 	    141,144,145,163,166,168,172,176,177,192,193,196,197] ] = 5
	 table[ *, [10,11,18,22,50,54,72,76,80,81,104,108,138,139,208,209] ]=6
	 
	 if amp_available then begin
	   table_column = (*(*amp_id).data EQ 1) OR (*(*amp_id).data EQ 3)
	 endif else begin
	   table_column = 0
	 endelse
	 
         AssignPropData, prop, table[ table_column, *(*grade).data ]
     endif
   endif ;valid      
   end
           
   else: return, -1
  endcase
  end

 ;--------------------------------------------------------------------------
 'ccd_id': $
  begin
  case prop.def_code of
   ;; See if the components needed by this algorithm are available.
   1: $ 
   begin
   valid = GetProperty('CCD_ID', ccd_id, DEF_ONLY=def_only)
   
   if valid then begin
     if init_fits then AssignPropMembers, prop, ccd_id
     
     stale = (prop.data_epoch LT (*ccd_id).data_epoch)
     if (stale AND (NOT def_only)) then AssignPropAlias, prop, ccd_id
   endif ;valid   
   end

   2: $
   begin
   kywd_name = 'CCD_ID'
   ccd_id = fxpar( GetPrimaryKywds(EPOCH=kywd_epoch), kywd_name, $
      		   COMMENT=comment, COUNT=count )
   valid  = (count EQ 1)
      
   if valid then begin
     if init_fits then AssignPropMembers, prop, $
			COL_NAME='CCD_ID', COL_TXT=comment, $
			UNIT_NAME='none', UNIT_TXT='', $
			DEF_TXT=kywd_name + ' keyword'
			
     stale= prop.data_epoch LT kywd_epoch

     if (stale AND (NOT def_only)) then begin
	
         AssignPropData, prop, replicate(ccd_id, num_events)
     endif
   endif ;valid      
   end

   else: return, -1
  endcase
  end

 ;--------------------------------------------------------------------------
 'amp_id': $
  begin
  names = ['AMP_ID','CCDNODE','NODE_ID']
  case 1 of
  (prop.def_code LE 3): $
   begin
   valid = GetProperty(names[prop.def_code-1], cprop, DEF_ONLY=def_only)

   if valid then begin
     if init_fits then AssignPropMembers, prop, cprop
     
     stale = (prop.data_epoch LT (*cprop).data_epoch)
     if (stale AND (NOT def_only)) then AssignPropAlias, prop, cprop
   endif ;valid   
   end
   
  (prop.def_code LE 4): $
   begin
   valid = GetProperty('CHIPX', chipx, DEF_ONLY=def_only)

   if valid then begin
     if init_fits then AssignPropMembers, prop, $
			COL_NAME='AMP_ID', $
			COL_TXT='amp computed from CHIPX assuming 4 amps', $
			UNIT_NAME='none', UNIT_TXT='', $
			DEF_TXT='amp computed from CHIPX assuming 4 amps'
     
     stale = (prop.data_epoch LT (*chipx).data_epoch)
     if (stale AND (NOT def_only)) then $
         AssignPropData, prop, fix(*(*chipx).data - 1) / 256
   endif ;valid      
   end
   
   else: return, -1
  endcase
  end


 ;--------------------------------------------------------------------------
 'exposure': $
  begin
  names = ['EXPOSURE','EXPNO','FRAME','CCDFRAME']
  case 1 of
  (prop.def_code LE 4): $
   begin
   valid = GetProperty(names[prop.def_code-1], cprop, DEF_ONLY=def_only)

   if valid then begin
     if init_fits then AssignPropMembers, prop, cprop
     
     stale = (prop.data_epoch LT (*cprop).data_epoch)
     if (stale AND (NOT def_only)) then AssignPropAlias, prop, cprop
   endif ;valid   
   end  
   
   else: return, -1
  endcase
  end


 ;--------------------------------------------------------------------------
;;; If a QUALCODE FITS column is not available and the event list is NOT
;;; bias corrected, i.e. the columns BIAS and PHAS are available, then 
;;; the QUALCODE (quality code) vector is computed using the following 
;;; ACIS-specific rules:
;;; 
;;; * Events with any BIAS values >= 4094 have bit 0 in QUALCODE set, i.e. 
;;;   QUALCODE = 1.  For ACIS data, the value 4094 == BIAS_BAD == parity error, 
;;;   and the value 4095 == PIXEL_BAD == pixel in bad-pixel map.
;;;
;;; * Events with any BIAS values of zero (no bias recovered by ATICA)
;;;   have bit 1 in QUALCODE set, i.e. QUALCODE = 2. 
;;;
;;; * Events whose center pixel (PHAS[0]) has the values 4095 (saturated 
;;;   A/D converter) have bit 2 in QUALCODE set, i.e. QUALCODE = 4.
;;;
 'qualcode': $
  begin
  case prop.def_code of
   ;; See if the components needed by this algorithm are available.
   1: $ 
   begin
   valid = GetProperty('STATUS', status, ONE_DATUM=datum)
   
   if valid then begin
     if init_fits then AssignPropMembers, prop, status
     
     stale = (prop.data_epoch LT (*status).data_epoch)
     if (stale AND (NOT def_only)) then begin
         width = n_elements(datum)
         
         case (width) of
          1: AssignPropAlias, prop, status
          
           ; The ASC's STATUS column is read as a 2xN byte array.  
           ; We want to cast to an integer N-vector.
          2: AssignPropData, prop, fix( *(*status).data, 0, num_events )
          
           ; The ASC's STATUS column is read as a 2xN byte array.  
           ; We want to cast to an long N-vector.
          4: AssignPropData, prop, long( *(*status).data, 0, num_events )
          
          else: valid = 0
         endcase
     endif
   endif ;valid   
   end

   2: $ 
   begin
   valid = GetProperty('QUALCODE', qualcode, DEF_ONLY=def_only)
   
   if valid then begin
     if init_fits then AssignPropMembers, prop, qualcode
     
     stale = (prop.data_epoch LT (*qualcode).data_epoch)
     if (stale AND (NOT def_only)) then $
       AssignPropAlias, prop, qualcode
   endif ;valid   
   end

   3: $ 
   begin
   valid = GetProperty('BIAS', bias, DEF_ONLY=def_only) AND $
   	   GetProperty('PHAS', phas, DEF_ONLY=def_only)
   
   if valid then begin
     if init_fits then AssignPropMembers, prop, $
			COL_NAME='QUALCODE', COL_TXT='quality flags', $
			UNIT_NAME='none', UNIT_TXT='', $
			DEF_TXT='(BIAS >= 4094); (BIAS == 0); (PHAS == 4095)'
		
     stale= prop.data_epoch LT max([(*phas).data_epoch,(*bias).data_epoch])
             
     if (stale AND (NOT def_only)) then begin
         ; Compute bits 0, 1, & 2 of QUALCODE vector.
         bad_pixel_map = (*(*bias).data GE 4094)
         large_bias    = ( total( bad_pixel_map, 1 ) GT 0 )
         bad_pixel_map = (*(*bias).data EQ 0)
         zero_bias     = ( total( bad_pixel_map, 1 ) GT 0 ) 
         bad_phas      = ((*(*phas).data)[0,*] EQ 4095)
         quality_code  = large_bias OR ishft(zero_bias,1) OR ishft(bad_phas,2) 

         AssignPropData, prop, quality_code
     endif
   endif ;valid      
   end
           
   else: return, -1
  endcase
  end

 ;--------------------------------------------------------------------------
 'generic5': $
  begin
  names = ['down_pix', GetPropertyNames( /FITS ), 'qualcode', 'grating_order',$
	   'll_pix','lr_pix',$
	   'left_pix','center_pix','right_pix',$
	   'ul_pix','up_pix','ur_pix','pi','row_number']
  if (prop.def_code LE n_elements(names)) then begin
    cprop_name = names[prop.def_code-1]
    valid = GetProperty(cprop_name, cprop, DEF_ONLY=def_only)
    if valid then begin
      if init_fits then AssignPropMembers, prop, cprop, $
      			DEF_TXT='alias for ' + (*cprop).name
      
      stale = (prop.data_epoch LT (*cprop).data_epoch)
          
      if (stale AND (NOT def_only)) then begin
        valid = GetProperty(cprop_name, cprop, ONE_DATUM=datum) AND $
   	        (n_elements(datum) EQ 1)

        if (valid) then AssignPropAlias, prop, cprop
      endif
    endif ;valid 
  
  endif else if (prop.def_code EQ (n_elements(names)+1)) then begin
    catch, error_code
    if (error_code NE 0) then begin
      valid = 0
    endif else begin
      eb_custom_property5, prop, def_only, init_fits, valid
    endelse
    catch, /CANCEL

  endif else return, -1
  end

 ;--------------------------------------------------------------------------
 'generic3': $
  begin
  names = ['left_pix', GetPropertyNames( /FITS ), 'qualcode', 'grating_order',$
	   'll_pix','down_pix','lr_pix',$
	   'center_pix','right_pix',$
	   'ul_pix','up_pix','ur_pix','pi','row_number']
  if (prop.def_code LE n_elements(names)) then begin
    cprop_name = names[prop.def_code-1]
    valid = GetProperty(cprop_name, cprop, DEF_ONLY=def_only)
    if valid then begin
      if init_fits then AssignPropMembers, prop, cprop, $
      			DEF_TXT='alias for ' + (*cprop).name
      
      stale = (prop.data_epoch LT (*cprop).data_epoch)
          
      if (stale AND (NOT def_only)) then begin
        valid = GetProperty(cprop_name, cprop, ONE_DATUM=datum) AND $
   	        (n_elements(datum) EQ 1)

        if (valid) then AssignPropAlias, prop, cprop
      endif
    endif ;valid 
  
  endif else if (prop.def_code EQ (n_elements(names)+1)) then begin
    catch, error_code
    if (error_code NE 0) then begin
      valid = 0
    endif else begin
      eb_custom_property3, prop, def_only, init_fits, valid
    endelse
    catch, /CANCEL

  endif else return, -1
  end

 ;--------------------------------------------------------------------------
 'generic1': $
  begin
  names = ['center_pix', GetPropertyNames( /FITS ), 'qualcode', 'grating_order',$
	   'll_pix','down_pix','lr_pix',$
	   'left_pix','right_pix',$
	   'ul_pix','up_pix','ur_pix','pi','row_number']
  if (prop.def_code LE n_elements(names)) then begin
    cprop_name = names[prop.def_code-1]
    valid = GetProperty(cprop_name, cprop, DEF_ONLY=def_only)
    if valid then begin
      if init_fits then AssignPropMembers, prop, cprop, $
      			DEF_TXT='alias for ' + (*cprop).name
      
      stale = (prop.data_epoch LT (*cprop).data_epoch)
          
      if (stale AND (NOT def_only)) then begin
        valid = GetProperty(cprop_name, cprop, ONE_DATUM=datum) AND $
   	        (n_elements(datum) EQ 1)

        if (valid) then AssignPropAlias, prop, cprop
      endif
    endif ;valid 
  
  endif else if (prop.def_code EQ (n_elements(names)+1)) then begin
    catch, error_code
    if (error_code NE 0) then begin
      valid = 0
    endif else begin
      eb_custom_property1, prop, def_only, init_fits, valid
    endelse
    catch, /CANCEL

  endif else return, -1
  end

 ;--------------------------------------------------------------------------
 'generic2': $
  begin
  names = ['right_pix', GetPropertyNames( /FITS ), 'qualcode', 'grating_order',$
	   'll_pix','down_pix','lr_pix',$
	   'left_pix','center_pix',$
	   'ul_pix','up_pix','ur_pix','pi','row_number']
  if (prop.def_code LE n_elements(names)) then begin
    cprop_name = names[prop.def_code-1]
    valid = GetProperty(cprop_name, cprop, DEF_ONLY=def_only)
    if valid then begin
      if init_fits then AssignPropMembers, prop, cprop, $
      			DEF_TXT='alias for ' + (*cprop).name
      
      stale = (prop.data_epoch LT (*cprop).data_epoch)
          
      if (stale AND (NOT def_only)) then begin
        valid = GetProperty(cprop_name, cprop, ONE_DATUM=datum) AND $
   	        (n_elements(datum) EQ 1)

        if (valid) then AssignPropAlias, prop, cprop
      endif
    endif ;valid 
  
  endif else if (prop.def_code EQ (n_elements(names)+1)) then begin
    catch, error_code
    if (error_code NE 0) then begin
      valid = 0
    endif else begin
      eb_custom_property2, prop, def_only, init_fits, valid
    endelse
    catch, /CANCEL

  endif else return, -1
  end

 ;--------------------------------------------------------------------------
 'generic4': $
  begin
  names = ['up_pix', GetPropertyNames( /FITS ), 'qualcode', 'grating_order',$
	   'll_pix','down_pix','lr_pix',$
	   'left_pix','center_pix','right_pix',$
	   'ul_pix','ur_pix','pi','row_number']
  if (prop.def_code LE n_elements(names)) then begin
    cprop_name = names[prop.def_code-1]
    valid = GetProperty(cprop_name, cprop, DEF_ONLY=def_only)
    if valid then begin
      if init_fits then AssignPropMembers, prop, cprop, $
      			DEF_TXT='alias for ' + (*cprop).name
      
      stale = (prop.data_epoch LT (*cprop).data_epoch)
          
      if (stale AND (NOT def_only)) then begin
        valid = GetProperty(cprop_name, cprop, ONE_DATUM=datum) AND $
   	        (n_elements(datum) EQ 1)

        if (valid) then AssignPropAlias, prop, cprop
      endif
    endif ;valid 
  
  endif else if (prop.def_code EQ (n_elements(names)+1)) then begin
    catch, error_code
    if (error_code NE 0) then begin
      valid = 0
    endif else begin
      eb_custom_property4, prop, def_only, init_fits, valid
    endelse
    catch, /CANCEL

  endif else return, -1
  end

 ;--------------------------------------------------------------------------
 'generic6': $
  begin
  names = [ GetPropertyNames( /FITS ), 'qualcode', 'grating_order',$
	   'll_pix','down_pix','lr_pix',$
	   'left_pix','center_pix','right_pix',$
	   'ul_pix','up_pix','ur_pix','pi','row_number']
  if (prop.def_code LE n_elements(names)) then begin
    cprop_name = names[prop.def_code-1]
    valid = GetProperty(cprop_name, cprop, DEF_ONLY=def_only)
    if valid then begin
      if init_fits then AssignPropMembers, prop, cprop, $
      			DEF_TXT='alias for ' + (*cprop).name
      
      stale = (prop.data_epoch LT (*cprop).data_epoch)
          
      if (stale AND (NOT def_only)) then begin
        valid = GetProperty(cprop_name, cprop, ONE_DATUM=datum) AND $
   	        (n_elements(datum) EQ 1)

        if (valid) then AssignPropAlias, prop, cprop
      endif
    endif ;valid 
  
  endif else if (prop.def_code EQ (n_elements(names)+1)) then begin
    catch, error_code
    if (error_code NE 0) then begin
      valid = 0
    endif else begin
      eb_custom_property6, prop, def_only, init_fits, valid
    endelse
    catch, /CANCEL

  endif else return, -1
  end



 else: message, 'No definitions for property ' + name
endcase


;; If the property defn was valid and def_only was FALSE, then in the code
;; above we tested the property's value for staleness, recomputing it if
;; it was stale.  
;; So, we know that in the current epoch both the property definition was
;; valid and the property data was fresh (not stale).  We record the current
;; epoch in "fresh_epoch"so that in future calls here we may be able to avoid 
;; the cost of testing the definition and staleness again.
if (valid AND (NOT def_only)) then prop.fresh_epoch = GetEpoch()

return, valid
end

;==========================================================================
;; We need a procedure with the same name as this file to keep 
;; RESOLVE_ROUTINE happy.
PRO eb_derived_property
return
end
