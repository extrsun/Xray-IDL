pro galget,source,srcnames,paramname,value
if n_params(0) eq 0 then begin
 print,'galget,source,srcnames,paramname,value'
 print,'Get VALUE from the array with name PARAMNAME for ' 
 print,'object called SOURCE '
 retall
end
galindex,srcnames,source,index
value=paramname(index)
print,'VALUE = ',value
return
end
