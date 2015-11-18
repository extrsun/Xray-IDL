pro topgti,sgtifiles,topname=topname
;Author T. Yaqoob - March 1993->**
if n_params(0) eq 0 then begin
 print,'topgti,sgtifiles,topname=topname'
 print,'Replace Top GTI filename in the array GTIFILES with'
 print,'the file TOPNAME '
 retall
end
nfiles=n_elements(sgtifiles)
sgtifiles(nfiles-1)=topname
print,'New GTI filename list: '
forprint,sgtifiles
return
end
