pro accum,vec,avec
;+
; get an accumulative vector
; 
; vec - input vector 
; avec - output vector 
;
; written by wqd, 5/30/2001
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - accum,vec,avec'
return
endif
avec=vec
for n=1L,n_elements(vec)-1L do avec(n)=avec(n-1)+vec(n)
return
end
