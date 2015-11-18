pro chipsel,plist,chlist,chip=chip
if n_params(0) eq 0 then begin
print,'CHIPSEL,plist,chlist,chip=chip'
print,'Limits events to those belonging to a single chip (0,1,2,3)'
retall
end
if (n_elements(chip) eq 0) then chip=4 
x=plist.x & y=plist.y
if chip eq 0 then begin
chout=(x le 419) and ((y gt 427) and (y le 847))
endif
if chip eq 1 then begin
chout=((x gt 445) and (x le 883)) and ((y gt 427) and (y le 847))
endif
if chip eq 2 then begin
chout=((x gt 445) and (x le 883)) and (y le 421)
endif
if chip eq 3 then begin
chout=(x le 419) and (y le 421)
endif
chlist=plist(where(chout)) & print,total(chout)
return
end

