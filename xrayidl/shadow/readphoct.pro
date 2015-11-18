pro readphoct,col1,col2,file
openr,un,file,/get_lun
text=''
for i=1,13 do readf,un,text
c1=0.
c2=0.
col1=[-999]
col2=[-999]
while not eof(un) do begin
readf,un,c1,c2
col1=[col1,c1]
col2=[col2,c2]
endwhile
col1=col1(1:*)
col2=col2(1:*)
free_lun,un
return
end