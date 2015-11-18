pro tmask,tm,int,msk
;  Produce a mask for selecting data in good time intervals
;  'INT' is a two-column array of good time intervals
if n_params(0) eq 0 then begin
print,'TMASK,tm,int,msk'
print,'Produce a mask for selecting data in good time intervals'
retall
end
msk=intarr(n_elements(tm))
for i=0,(size(int))(2)-1 do $ 
msk=msk+(tm ge int(0,i)) and (tm lt int(1,i))
return
end



