pro absis,dname=dname,fname=fname,tname=tname
;print,n_params(0)
;print,n_elements(fname)
i=1
if i eq 1 then begin
	print, ' i = ',i
end

if (n_elements(fname) eq 0) then begin
	fname=' '
	print, 'entered the if statement'
	read,' Enter name of file containing data filenames',fname
endif
return
end
