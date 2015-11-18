pro selchip,chip,geo,plist,plist_new
;Author T. Yaqoob - March 1993->**
if n_params(0) eq 0 then begin
 print,'selchip,chip,geo,plist,plist_new'
 print,'select photon list by CCD ID and create new GEO'
 retall
end
if chip lt 0 then read,'Enter valid CCD ID >0',chip  
if chip gt 3 then read,'Enter valid CCD ID <4',chip 
print,'Size of photon list on entry ',(size(plist))(1)
geo.chp=chip
wchp=where((plist.ccd eq chip),nsel)
if nsel eq 0 then begin
 print,'No events with CCD ID = ',chip
 retall
endif
plist_new=plist(wchp)
print,'Size of selected photon list ',(size(plist_new))(1)
return
end
