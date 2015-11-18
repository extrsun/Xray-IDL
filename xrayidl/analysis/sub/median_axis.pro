;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME
;	median_axis
;
;*PURPOSE:
;	obtain a median filtered image around one axis
;
;*CALLING SEQUENCE:
;	median_axis,inimage,outimage,width,axis=axis
;
;*PARAMETERS:
; INPUTS:
;	width - the dimension of the median filter
;	inimage - input image

; OUTPUTS
;	outimage - the median filter image
;
;*PROCEDURE:
;
;*RESTRICTIONS:
;
;*NOTES:
;
;*MODIFICATION HISTORY:
;	writen Sept 1 1993 (WQD)
;
;-
;---------------------------------------------------------------------------
pro median_axis,inimage,outimage,width,axis=axis
;
; width --- the maximum bin number in one side of the average
;------------------------------------------------------------------------
if n_params() eq 0 then begin
print,'CALL SEQUENCE - median_axis,inimage,outimage,width,axis=axis'
return
endif
;
if n_elements(axis) eq 0 then axis=1
sz=size(inimage)
xdim=sz(1)
ydim=sz(2)
;
if axis eq 1 then begin
	dim=sz(2)
	outimage=inimage
endif else  begin 
	dim=sz(1)
	outimage=transpose(inimage)
endelse
;---------------------------------------------------
for j=0,(dim-1) do begin 
	if width eq 0 then outimage(*,j)=outimage(*,j)-median(outimage(*,j)) $
	else outimage(*,j)=median(outimage(*,j),width)
endfor
if axis eq 2 then outimage=transpose(outimage)
if !debug eq 1 then stop
;
return
end

