pro binary_search,arr,x,index
;+
;
; NAME:
;    BINARY_SEARCH
;
; CALLING SEQUENCE:
;    binary_search,arr,x,index
;
; PURPOSE:
;    Perform a binary search on arr, an input array, for the closest match
;    to x.
;
;-

if n_params() eq 0 then begin
  print,'syntax- binary_search,arr,x,index'
  return
endif

n=n_elements(arr)
if (x lt arr(0)) or (x gt arr(n-1)) then begin
	index=-1
	return
endif
down=-1
up=n
while up-down gt 1 do begin
	mid=down+(up-down)/2
	if x ge arr(mid) then begin
		down=mid
	endif else begin
		up=mid
	endelse
endwhile
index=down
return
end	
