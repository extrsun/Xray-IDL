function datxfer,name,n,la
; Designed to transfer n columns of data into IDL
; la=0 -> no label;  la=1,2...m -> 1,2....m lines of label
str=' ' & d=double(replicate(-1.,n)) & c=d & openr,un,name,/get_lun
if la eq 0 then goto,fn1 else begin
for i=1,la do begin & readf,un,str         ; read the label
endfor & endelse
fn1: while not eof(un) do begin
readf,un,c & d=[[d],[c]] 
endwhile 
return,d=d(0:n-1,1:*)
end     

