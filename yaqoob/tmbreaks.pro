pro tmbreaks,plist,tbeg,tend
; Find the beginning and end of good data intervals
a=plist.time & n=(size(plist))(1) & a=a(sort(a)) & b=([a,0]-[0,a])(1:*)
b=b(0:n_elements(b)-2) & tbeg=[a(0),a(where(b gt 1000.)+1)]
tend=[a(where(b gt 1000.)),a(n-1)]
return
end
