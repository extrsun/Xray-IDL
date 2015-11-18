pro struct_col_add,struct,newcols,colname,colsample,structn,struct_temp=struct_temp
;+
; add columns of data into an existing structure
;
; struct - old structure
; colname - vector containing column names
; colsample - sample of the data types in the columns (for the definition)
; newcols - array containing the new columns of values: the first index should
;   match the rows of the old structure; the second index (the number of 
;   columns) should match the number of elements in both colname colsample 
; structn - new structure
; struct_temp - template of the output structure, which may be reused in
; 	subsequent calls (without colsample entry) to genenerate 
;	non-conflicting structures (allowing for concatenation)
;
;*Sample 
; struct_col_add,struct,newcols,['CNTRB1','CNTRB2'],[0.0,0.0],structn
; 
; written by wqd, April, 25, 2002
;-
nparams=n_params()
if nparams eq 0 then begin
print,'Calling Seq. - struct_col_add,struct,newcols,colname,colsample,structn,struct_temp=struct_temp'
return
endif
ntag=n_tags(struct(0))
if n_elements(struct_temp) eq 0 or n_elements(colsample) then begin
 ncol=n_elements(colname)
 struct_temp=struct(0)
 for k=0,ncol-1 do $
	struct_temp=create_struct(struct_temp,colname(k),colsample(k))
endif else ncol=n_tags(struct_temp)-ntag
nrow=n_elements(struct)
struct_int=replicate(struct_temp(0),nrow)
for kk=0,ntag-1 do struct_int(*).(kk)=struct(*).(kk)
for k=0,ncol-1 do struct_int(*).(ntag+k)=newcols(*,k)
if nparams lt 5 then struct=struct_int else structn=struct_int
if !debug eq 2 then stop
return
end