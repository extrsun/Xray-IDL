pro readrsp,file,a,nchan=nchan
; Read an ASC II response file skipping the header
openu,1,file
point_lun,1,566
a=fltarr(3,nchan)
readf,1,a
close,1
return
end

