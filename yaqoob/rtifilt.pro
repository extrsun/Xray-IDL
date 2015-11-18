pro rtifilt,plist,dir=dir,rfile=rfile 
if n_params(0) eq 0 then begin
 print,'rtifilt,plist,dir=dir,rfile=rfile'
 print,'Filter a GIS photon list based on minimum and maximum RTI'
 print,'range defined in the calibration file dir+rfile'
 print,'Current values: '
 print,'dir - /FTP/caldb/data/asca/gis/bcf/ '
 print,'rfile - rti_gis_1024_040693.fits '
 retall
end
if n_elements(dir) eq 0 then begin
 dir=' '
 read,'Enter name of directory containing calibration file ',dir
endif
if n_elements(rfile) eq 0 then begin
 rfile=' '
 read,'Enter actual name of RTI calibration file ',rfile
endif
;read the cal file
fname=dir+rfile
tableget,fname,ch,'chan',1,xte=1
tableget,fname,rtmin,'rti_min',1,xte=1
tableget,fname,rtmax,'rti_max',1,xte=1
np=(size(plist))(1)
print,'Number of events on entry: ',np
msk=lonarr(np)
pi=plist.pi
rt=plist.rt
for k=0l,np-1 do begin
if rt(k) ge rtmin(pi(k)) and rt(k) le rtmax(pi(k)) then msk(k)=1 $
 else msk(k)=0
endfor
wmsk=where((msk eq 1),nmsk)
if nmsk gt 0 then begin
 print,'Number of events remaining after RTI filter will be: ',nmsk
 ans=' '
 read,'Do you want to apply the filter (y/n)? ',ans
 if ans eq 'y' then begin
  plist=plist(wmsk)
  npd=(size(plist))(1)
  print,'Number of events on exit: ',npd
 endif
endif
return
end 
