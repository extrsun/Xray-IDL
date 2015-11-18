;+
;========================================================================
;;;
;;; FILE NAME:    $Id: eb_parameter.pro 2906 2007-11-23 22:22:14Z patb $
;;;
;;; DESCRIPTION: Routines to manipulate analysis parameters
;;;		 for X-ray Event Browser application
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Copyright (C) 1998, Pennsylvania State University
;-
;========================================================================
FORWARD_FUNCTION GetParameter
FORWARD_FUNCTION GetEpoch
FORWARD_FUNCTION GetProperty
;==========================================================================
PRO AddParameter, name, def_txt, def_code, value, ALLOWED_VALUES=allowed_values

@eb_common

param = {EB_PARAMETER, $
 name: 		name, $ 
   ;a unique string
 def_code:	def_code, $
   ;0: filename
   ;1: string
   ;2: integer
   ;3: float
 def_txt:  	def_txt, $
   ;a description of the definition to show the user
 allowed_values:ptr_new(), $
   ;(optional) for string parameters, the allowed values
 data:          ptr_new(value), $
   ;pointer to the parameter data
 data_epoch:		0L $
   ;the epoch when *data was last edited
   }
   
if keyword_set(allowed_values) then $
  param.allowed_values = ptr_new(allowed_values)

param_list = [param_list, ptr_new(param, /NO_COPY)]
return
end

;========================================================================
;;; Run a modal widget which allows the user to edit the parameter's value.
PRO EditParameter, name, group_leader

;; Retrieve the parameter.
if (GetParameter( name, param_ptr ) EQ 0) then $
  message, 'Parameter not found.'
  
case (*param_ptr).def_code of
 0: $ ;filename
  begin
  result = routine_info( 'eb_parameter', /SOURCE )
  fdecomp, result.PATH, disk, dir, name, qual
  fname = dialog_pickfile( GROUP=group_leader, /MUST_EXIST, $
			   TITLE='Select a '+(*param_ptr).def_txt, $
			   PATH=dir(0),  FILTER='*.dn2ev', $
			   FILE=*(*param_ptr).data )
  if (fname NE '') then *(*param_ptr).data = fname
  end
  
 1: $ ;string
  begin
  if ptr_valid((*param_ptr).allowed_values) then begin
    index = (where(*(*param_ptr).allowed_values EQ *(*param_ptr).data))[0]
    list  = string(*(*param_ptr).allowed_values, F='(100(A,:,"|"))')
    
    f1 = '0, DROPLIST,' + list +', SET_VALUE=' + string(index) + $
  	', LABEL_LEFT=' + (*param_ptr).def_txt+', TAG=value'

    ok = '2, BUTTON, OK, QUIT'
    r = cw_form( [f1,ok], /COLUMN, TITLE='Parameter '+(*param_ptr).name )
    *(*param_ptr).data = (*(*param_ptr).allowed_values)[r.value] 
  endif else begin
    f1 = '0, TEXT,' + *(*param_ptr).data + $
  	', LABEL_LEFT=' + (*param_ptr).def_txt+', WIDTH=16, TAG=value'
    ok = '2, BUTTON, OK, QUIT'
    r = cw_form( [f1,ok], /COLUMN, TITLE='Parameter '+(*param_ptr).name )
    *(*param_ptr).data = r.value
  endelse
  end
  
 2: $ ;integer
  begin
  f1 = '0, INTEGER,' + string(*(*param_ptr).data) + $
	', LABEL_LEFT=' + (*param_ptr).def_txt+', WIDTH=12, TAG=value'
  ok = '2, BUTTON, OK, QUIT'
  r = cw_form( [f1,ok], /COLUMN, TITLE='Parameter '+(*param_ptr).name )
  *(*param_ptr).data = r.value
  end
  
 3: $ ;double
  begin
  f1 = '0, TEXT,' + string(*(*param_ptr).data) + $
	', LABEL_LEFT=' + (*param_ptr).def_txt+', WIDTH=16, TAG=value'
  ok = '2, BUTTON, OK, QUIT'
  r = cw_form( [f1,ok], /COLUMN, TITLE='Parameter '+(*param_ptr).name )
  
  ;; If the number is an integer, then convert it to integer type since
  ;; that may speed up algorithms which use this parameter.
  value = double(r.value)
  if (value EQ fix(value)) then begin
    *(*param_ptr).data = fix(value)
  endif else if (value EQ long(value)) then begin
    *(*param_ptr).data = long(value)
  endif else begin
    *(*param_ptr).data = value
  endelse
  end
endcase

(*param_ptr).data_epoch = GetEpoch( /INCREMENT )
return
end


;========================================================================
;;; Search for the named parameter,
;;; returning a pointer to it in "param_ptr".  If the parameter is found and
;;; it has a valid definition, the value 1 is returned.
;;; 
;;; DATA returns a POINTER to the parameter data.

FUNCTION GetParameter, name, param_ptr, DATA=data

@eb_common

;; Search for the specified parameter name.
found = 0
ii    = 0
while ((NOT found) AND (ii LT n_elements(param_list))) do begin
  param_ptr = param_list[ii]
  if ((*param_ptr).name EQ name) then found = 1
  ii = ii + 1
endwhile

if found then begin
  data = (*param_ptr).data
endif else begin
  data = null_data
endelse

return, found
end


;==========================================================================
FUNCTION GetParameterNames

@eb_common

names = strarr( n_elements(param_list) )
for ii = 0, n_elements(param_list)-1 do $
  names[ii] = (*param_list[ii]).name

return, names
end


;==========================================================================
PRO SetParameterDefaults

@eb_common

;--------------------------------------------------------------------------
;; Figure out the format of the event island column (e.g. PHAS).
dum = GetParameter('island_order', island_order)
*(*island_order).data = 'UNKNOWN'

; First, look for TDIM keyword on PHAS column.
if GetProperty('island', island) then begin
  if (strpos( (*island).dim_spec, '(3,3)' ) GE 0) OR $
     (strpos( (*island).dim_spec, '(5,5)' ) GE 0) then begin
    *(*island_order).data = 'ASC'
  endif
endif

; Second, look for ORIGIN keyword.
if (*(*island_order).data EQ 'UNKNOWN') then begin
  origin = fxpar( primary_kywds, 'ORIGIN', COUNT=count )
  if (count GT 0) then begin
    ; The space after MIT is VITAL since ATICA writes ORIGIN="MIT/PSU".
    if (strpos(origin, 'MIT ') GE 0) then *(*island_order).data = 'ASCA'
     
    if (strpos(origin, 'ASC ') GE 0) then *(*island_order).data = 'ASC'
    if (strpos(origin, 'CXC ') GE 0) then *(*island_order).data = 'ASC'
  endif  
endif

; Third, look for CREATOR keyword.
if (*(*island_order).data EQ 'UNKNOWN') then begin
  creator = fxpar( primary_kywds, 'CREATOR', COUNT=count )
  if (count GT 0) then begin
    if (strpos(creator, 'PRAXIS') GE 0) then *(*island_order).data = 'ATICA' 
    if (strpos(creator, 'ATICA')  GE 0) then *(*island_order).data = 'ATICA' 
    if (strpos(creator, 'PASS1')  GE 0) then *(*island_order).data = 'ATICA' 
    
    ; This case is to handle XRCF Post Production files, which were written
    ; by an old version of Event Browser and have ORIGIN = 'Event Browser'
    if (strpos(creator, 'Event Browser') GE 0) then $
    					  *(*island_order).data = 'ATICA'
  endif  
endif

 
msg = 'Parameter "island_order" assumed to be ' + *(*island_order).data
print, msg
if (gui_active) then begin
  if (*(*island_order).data EQ 'UNKNOWN') then begin
;   TimedMessage, msg_id, msg, TITLE='WARNING!!!' 
  endif else begin
;   TimedMessage, msg_id, msg, TITLE='Parameter Defaults', LIFE=30 
  endelse
endif

return
end


;==========================================================================
;;; Extract important keywords from a FITS header and store in the database.
;==========================================================================
PRO ExtractPrimaryKywds, header, DUPLICATE=duplicate

@eb_common

if (n_elements(header) EQ 1) then header = [header]

if (NOT keyword_set(duplicate)) then begin
  ;; This is the first header we've read, so just store the whole thing.
  primary_kywds = header
  
  ;; Remove some keywords
  sxdelpar, primary_kywds, ['CHECKSUM','DATASUM']
                 
endif else begin
  ;; This is not the first header, so we must merge the contents of this
  ;; one with whatever has already been stored.
  ;; At this point, all we're going to do is sum up certain keywords.
  names = ['NFRAMES', 'ONTIME']
  for ii=0, n_elements(names)-1 do begin
    value = fxpar( primary_kywds,  names(ii), COUNT=count1, COMMENT=comment )
    delta_value = fxpar( header, names(ii), COUNT=count2 )
    if (count1 GT 0 AND count2 GT 0) then begin
      fxaddpar, primary_kywds, names(ii), value + delta_value, comment
    endif
  endfor  
endelse

keywords_epoch = GetEpoch( /INCREMENT )
return
end


PRO ExtractTableKywds, header, ALL=all

@eb_common

if (n_elements(header) EQ 1) then header = [header]

table_kywds = header

;; Remove some checksum kywds & the "numbered" kywds that describe columns.
sxdelpar, table_kywds, ['CHECKSUM','DATASUM']

tfields = fxpar(header, 'TFIELDS')
if (tfields GT 0) && ~keyword_set(all) then begin
  kywds = ['TFORM','TTYPE','TDIM','TUNIT','TSCAL','TZERO',        $
           'TNULL','TDISP','TLMIN','TLMAX','TDESC','TROTA',        $
           'TCTYP','TCRVL','TCRPX','TCDLT','TCUNI'] 
  for ii=1,tfields do sxdelpar, table_kywds, kywds + strtrim(ii,2)
endif

keywords_epoch = GetEpoch( /INCREMENT )
return
end

;==========================================================================
;;; Add keywords to primary or table FITS headers in the database.
;==========================================================================
PRO AddPrimaryKywd, name, value, comment, _EXTRA=extra

@eb_common

fxaddpar, primary_kywds, name, value, comment, _EXTRA=extra
return
end

PRO AddTableKywd, name, value, comment, _EXTRA=extra

@eb_common

if (n_elements(comment) EQ 0) then comment = ''

fxaddpar, table_kywds, name, value, comment, _EXTRA=extra
return
end


PRO AddPrimaryAndTableKywd, name, value, comment, _EXTRA=extra
if (n_elements(comment) EQ 0) then comment = ''
AddPrimaryKywd, name, value, comment, _EXTRA=extra
AddTableKywd,   name, value, comment, _EXTRA=extra
return
end


;==========================================================================
;;; Retrieve FITS header keywords we've stored.
;==========================================================================
FUNCTION GetPrimaryKywds, EPOCH=epoch_parameter

@eb_common

epoch_parameter = keywords_epoch
return, primary_kywds
end


FUNCTION GetTableKywds, EPOCH=epoch_parameter

@eb_common

epoch_parameter = keywords_epoch
return, table_kywds
end


;==========================================================================
;; We need a procedure with the same name as this file to keep 
;; RESOLVE_ROUTINE happy.
PRO eb_parameter
return
end

