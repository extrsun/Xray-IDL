pro index_conv,indexo,dim,indexv,choice=choice
;+
; convert an index vector in an array into indexes in individual dimensions
; of the array or vice versa
;
; indexo - vector containing the index vector of the array
; dim - a vector containing number of elements in individual dimensions
; indexv - an array of dimension of n_elements(dim) * n_elements(indexo)
;		containing the indexes in individual dimensions
; choice - def=0 from array index to dimensional indexes
; 		otherwise, do the reverse
;
; writen by wqd, Aug. 16, 1993
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - index_conv,indexo,dim,indexv,choice=choice'
return
endif
;
if n_elements(choice) eq 0 then choice=0
if choice ne 0 then begin
	sz=size(indexv)
	indexo=transpose(indexv(0,*))
	frac=1L
	for k=1,n_elements(dim)-1 do begin
		frac=frac*dim(k-1)
		indexo=indexo+indexv(k,*)*frac
	endfor
stop
endif else begin
	ndim=n_elements(dim)
	sz=n_elements(indexo)
	index=indexo
	indexv=lonarr(ndim,sz)
	for k=0,ndim-2 do begin
		frac=dim(k)
		indexv(k,*)=index mod frac
		index=index/frac
	endfor
	indexv(ndim-1,*)=index
endelse
end
