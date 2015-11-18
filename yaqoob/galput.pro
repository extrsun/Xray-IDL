pro galput,source,srcnames,paramname,value
if n_params(0) eq 0 then begin
 print,'galput,source,srcnames,paramname,value'
 print,'Put VALUE into the array with name PARAMNAME for ' 
 print,'object called SOURCE '
 retall
end
galindex,srcnames,source,index
paramname(index)=value
return
end
