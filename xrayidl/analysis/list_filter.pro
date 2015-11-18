pro list_filter,list,sel,filter,xmin,ymin,block
;+
; select events from an event list based on a filter image
;
;*INPUTS:
; list - event list 
;
;*OUTPUTS:
; sel - event index in mlist selected with the projected filter value > 0 
;
;*OPTIONAL Inputs:
; xmin, ymin, block - the filter lower left coordinates in a system
;                     same as that defined for list. If not provided,
;                     default image coordinates are defined by
;                     sou_det_params, as used for exposure
;                     maps of individual observations.
; 
; written by wqd July 5, 2007
;-
if n_params() eq 0 then begin
print,'CALL SEQUENCE - list_filter,list,sel,filter,xmin,ymin,block'
return
endif
if n_elements(xmin) eq 0 then begin
    instr=!instr
    sou_det_params,instr,dim,block
    xmin=!pref+xoff-dim*block/2 ;low left pixel of the subimage
    ymin=!pref+yoff-dim*block/2
endif else begin
    sz=size(filter)
    dim=[sz(1),sz(2)]
    if n_elements(ymin) eq 0 then begin
        print,'ymin is needed for the filter!!'
        return
    endif 
    if n_elements(block) eq 0 then begin
        print,'block is needed for the filter!!'
        return
    endif 
endelse
list_image,list,xmin,ymin,cb,dim,block=block $
	   	,filter=filter,sel=sel,nsel=nsel
if nsel eq 0 then print,'no counts selected!!!'
return
end
 
