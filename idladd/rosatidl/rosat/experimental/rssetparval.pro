;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;       rssetparval
;
; procedure to change a parameter value
;
;       RSSETPARVAL,st,names,PAR
;
; INPUT:
;	st - new parameter value (string)  'name=value'
;	names - string array of parameter names
; INPUT/OUTPUT:
;	par - string array of values
;
; HISTORY
;       taken from GHRS routine setparval written by D. Lindler (05 Oct 1991)
;-
;-------------------------------------------------------------------------------
pro rssetparval,st,names,par
;
npar = n_params(0)
if (npar eq 0) then begin
  print,'RSSETPARVAL, st, names, PAR'
  retall
endif
;
val=st
name=gettok(val,'=')
name=strupcase(name)
if (strlen(val) eq 0) then begin
  print,'Invalid parameter specification:',st
  !err=-1
  return
endif
;
; search for name in names
;
n=size(names) & n=n(1)
ipos = -1
for i=0,n-1 do if (name eq strtrim(names(i),2)) then ipos = i
;
if (ipos lt 0) then begin
  print,name,' is not a valid parameter name'
  !err=-1
endif else begin
  par(ipos)=val
  !err=1
endelse
;
return
end             ;pro rssetparval
