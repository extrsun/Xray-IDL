pro imsel,sel,ima,iman
;+
; sel common 2-D elements in a 3-D image stack and put them into a 2-D
; array, different from imagea_sel
; sel -  index of selected pixels
; ima - input original image stack, if iman is not given, will be
;       replaced by selected pixels in each image
; iman - output 2-D array containing the selected pixels
;
; written by wqd, April 5, 2006
;-
np=n_params ()
if np eq 0 then begin
print,'Calling SEQ - imsel,sel,ima,iman'
return
endif

sza=size(ima)
if sza(0) eq 3 then nima=sza(3) else begin
    print,'ima must be a 2-D or 3-D array!'
    return
endelse
iman=make_array(n_elements(sel),nima,type=sza(4))
for k=0,nima-1 do begin
    imst=ima(*,*,k)
    iman(*,k)=imst(sel)
endfor
return
end
