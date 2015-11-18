pro total_3d,ima,totalv,sel=sel
;+
; calculate the total values of subimages for a 3-D array
; 
; ima - input 3-D array
; totalv - output total vectors
; sel - selected image index of subimages 
;
; written by wqd, June 18, 2003
;-
if n_params() eq 0 then begin
print,'Calling procedure - total_3d,ima,totalv,sel=sel'
return
endif

sz=size(ima)
if sz(0) ne 3 then begin
    print,'the imput image needs to 3-D!'
    return
endif
nsel=n_elements(sel)

totalv=fltarr(sz(3))
for k=0,sz(3)-1 do begin
    im_temp=ima(*,*,k)
    if nsel ne 0 then totalv(k)=total(im_temp(sel)) $
      else totalv(k)=total(im_temp)
endfor
return
end
