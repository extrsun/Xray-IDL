pro image_compa,imagea,nimagea,frac
;+
; compress an image array by an integer fraction 
; imagea - input image array
; nimagea - output image array with the first two dimensions shricked
;           by  a factor of frac
; 
; written by wqd, Aug. 8, 2005
;-
if n_params() lt 3 then begin
print,'image_compa,imagea,nimagea,frac'
return
endif
sz=size(imagea)
if sz(0) eq 2 then begin
    nimagea=image_comp(imagea,frac)
    return
endif

sz(1)=sz(1)*frac
sz(2)=sz(2)*frac
nsz=n_elements(sz)
case sz(nsz-2) of
    2: nimagea=intarr(sz(1),sz(2),sz(3))
    3: nimagea=lonarr(sz(1),sz(2),sz(3))
    else: nimagea=fltarr(sz(1),sz(2),sz(3))
endcase

for k=0,sz(3)-1 do begin
    nimagea(*,*,k) =image_comp(imagea(*,*,k),frac)
endfor

return
end
