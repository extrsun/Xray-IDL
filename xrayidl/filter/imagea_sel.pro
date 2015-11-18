pro imagea_sel,sel,ima,iman,append=append
;+
; sel common 2-D elements in a 3-D image stack.
; sel -  index of selected pixels
; ima - input original image stack, if iman is not given, will be
;       replaced by selected pixels in each image
; iman - if given, the selected pixels in ima will be placed.
; append - if given, the output selected pixel values will be appended
;          in iman; otherwise will be replaced
;
; written by wqd, May 28, 2003
;-
np=n_params ()
if np eq 0 then begin
print,'Calling SEQ - imagea_sel,sel,ima,iman,append=append'
return
endif

sza=size(ima)
if sza(0) eq 2 then nima=1 else if sza(0) eq 3 then nima=sza(3) else begin
    print,'ima must be a 2-D or 3-D array!'
    return
endelse
imt=fltarr(sza(1),sza(2))
if !debug eq 2 then stop
if keyword_set(append) eq 0 and np gt 2 then $
  iman=ima-ima                  ;just keep the same type
for k=0,nima-1 do begin
    imst=ima(*,*,k)
    imt(sel)=imst(sel)
    if np gt 2 then begin
        if keyword_set(append) then iman(*,*,k)=iman(*,*,k)+imt else $
            iman(*,*,k)=imt
    endif else ima(*,*,k)=imt ;keep only the pixels selected
    imt=imt*0
endfor
return
end
