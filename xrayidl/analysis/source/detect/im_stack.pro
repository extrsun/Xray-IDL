pro im_stack,arr1,arr2,arrs
;+
; stack two image arrays
;
; arr1, arr2 - two image arrays (i.e., 2- or 3-D data cube)
; arrs - stacked image array
;
; written by wqd, April, 24, 2002
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - im_stack,arr1,arr2,arrs'
return
endif

sz=size(arr1)
if sz(0) eq 2 then n1=1 else n1=sz(3)
sz=size(arr2)
if sz(0) eq 2 then n2=1 else n2=sz(3)
arrs=fltarr(sz(1),sz(2),n1+n2)
for k=0,n1-1 do arrs(*,*,k)=arr1(*,*,k)
for k=n1,n1+n2-1 do arrs(*,*,k)=arr2(*,*,k-n1)
return
end