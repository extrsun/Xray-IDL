pro sourcid_tv,xp,yp,sourceid
;
if n_params() eq 0 then begin
print,'CALL SEQUENCE - sourcid_tv,xp,yp,sourceid'
retall
endif
;
if n_elements(block) eq 0 then block=!block

print,'Press the left button to get the info about the closest source'
print,'Press the right button to select the source and  exit'
;
while (!err ne 4) do begin
cursor,x,y,/device,/down
dist=(x-xp)*(x-xp)+(y-yp)*(y-yp)
mindis=min(dist,id)
if mindis ne 0. then mindis=sqrt(mindis)
print,'The source (mplsx) id = ',source_id(id),'; distance = ',mindis, ' pixels;' 
print,'xp = ',xp(id);' and yp = ',yp(id)
print,''
endwhile
;
print,'The source ',source_id(id),' is selected'
sourceid=id
end