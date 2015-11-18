;; A custom definition of the Event Browser property "generic6" as a
;; 'sharpness' metric, i.e. the center pixel value divided by pha.

PRO eb_custom_property6, prop, def_only, init_fits, valid

;; This property definition is valid only if the components needed to 
;; compute it are available.
valid = GetProperty('center_pix', center_pix, DEF_ONLY=def_only) AND $
   	GetProperty('pha',        pha,        DEF_ONLY=def_only)

if valid then begin
  ;; If requested, assign FITS related property attributes:
  ;; COL_NAME & COL_TXT are the value and comment for kywd TTYPEn;
  ;; UNIT_NAME & UNIT_TXT are the value and comment for kywd TUNITn;
  ;; DEF_TXT is a description of this property definition shown to the user.
  if init_fits then AssignPropMembers, prop, $
			COL_NAME='SHARPNESS', COL_TXT='center_pix / pha', $
			UNIT_NAME='none', UNIT_TXT='', DEF_TXT='center_pix / pha'
  
  ;; The data values of this property are stale (must be recomputed) if their
  ;; data_epoch (timestamp) is less than any of the components used to compute
  ;; it.
  stale= prop.data_epoch LT max([(*center_pix).data_epoch,(*pha).data_epoch])

  if (stale AND (NOT def_only)) then begin
    ;; If the data are stale, and the call to this routine was not merely to
    ;; check if this property has a valid definitions (def_only==1), then
    ;; recompute and store the property values.
    AssignPropData, prop, *(*center_pix).data / float(*(*pha).data)
  endif
endif ;valid

;; Return valid, the flag indicating if this defintion of property 'generic6' 
;; is valid (i.e. computable).
return
end
