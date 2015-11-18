function datxfer2,name
; Designed to read data into IDL
; Data format: two columns of numbers with no header
a=[-1]&b=[-1]
openr,un,name,/get_lun
while not eof(un) do begin
readf,un,n1,n2
a=[a,n1]&b=[b,n2]
endwhile
z=transpose([[a],[b]]) & z=z(0:1,1:*)
return,z
end

