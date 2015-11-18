pro xysis,mtrx,xylist
; Tabulate the XY coordinates of non-zero sites of a SIS matrix
; INPUT
; 884x882 matrix containing 1 or 0
; OUTPUT
; XY list of the non-zero events
if n_params(0) eq 0 then begin
print,'XYSIS,mtrx,xylist'
print,'Tabulate the XY coordinates of non-zero sites of a SIS matrix'
retall
end
a=where(mtrx)/884. & b=fix((a-fix(a))*884+.1)
xylist=[rotate(b,1),rotate(fix(a),1)]
return
end
