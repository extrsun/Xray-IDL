pro rejgrad,plist,a,b,c,d,e,f,g
;Author T. Yaqoob - March 1993->**
if n_params(0) eq 0 then begin
 print,'rejgrad,plist,0,1,2,3,4,5,6'
 print,'Reject events from an SIS photon list by grade where grade '
 print,'starts from 0. Note this definition is different to that '
 print,'defined by READASCA and the values in the photon list '
 retall
end
wg=where((tag_names(plist) eq 'GRADE'),nwg)
if nwg eq 0 then begin
 print,' ** Your photon list contains no GRADE tag ** '
 print,'  -  ABORTED - '
 return
end
np=(size(plist))(1)
print,' Statistics on Entry :'
for k=1,7 do begin
 we=where((plist.grade eq k),nwe)
 print,nwe,' events with grade ',k-1
endfor 
print,'Total number of events on Entry ',np
print,'** Changes to your photon list will be permenant ** '
ans='n' & read,'Do you want to go ahead (y/n)? ',ans
if strmid(ans,0,1) ne 'y' then begin
 print,' OK ' & return
end
if n_elements(a) gt 0 then plist=plist(where(plist.grade ne (a+1)))
if n_elements(b) gt 0 then plist=plist(where(plist.grade ne (b+1)))
if n_elements(c) gt 0 then plist=plist(where(plist.grade ne (c+1)))
if n_elements(d) gt 0 then plist=plist(where(plist.grade ne (d+1)))
if n_elements(e) gt 0 then plist=plist(where(plist.grade ne (e+1)))
if n_elements(f) gt 0 then plist=plist(where(plist.grade ne (f+1)))
if n_elements(g) gt 0 then plist=plist(where(plist.grade ne (g+1)))
print,' Statistics on Exit :'
for k=1,7 do begin
 we=where((plist.grade eq k),nwe)
 print,nwe,' events with grade ',k-1  
endfor
print,'Total number of events on Exit ',(size(plist))(1)
retall
end
