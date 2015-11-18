pro int_opacity,nh,op,int_op
;-
; integrated contribution along a line of sight
; output:
; int_op -- tsum of the opacity curve with a dimension smaller than that of
; 		nh by 1
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - int_opacity,nh,op,int_op'
return
endif
;
np=n_elements(nh)
int_op=fltarr(np-1)
for k=1,np-1 do begin
	int_op(k-1)=tsum(nh,op,0,k) ;op=0 at k=0
endfor
end