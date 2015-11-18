pro glimpse,plist,Dn=Dn,xyr=xyr
; Steps through a photon list to look for problems or 'other things'
; INPUT => 'plist'; dn (optional)
if n_params(0) eq 0 then begin
print,'GLIMPSE,plist,dn=dn,xyr=xyr'
print,'Go through a photon list in small steps'
retall
end
if n_elements(dn) eq 0 then dn=200 & i=0
if n_elements(xyr) eq 0 then xyr=[0,900,0,900]
n=fix(n_elements(plist)/dn) & nbeg=lindgen(n)*dn & nend=nbeg+dn
ans=' ' & read,'Auto-advance? y/(n) : ',ans & if ans eq 'y' then goto,gl2
print,'Press ''n'' for next, ''b'' to go back 1, ''s'' to stop'
gl1:if i ge n then goto,gl10
plot,(plist.x)(nbeg(i):nend(i)),(plist.y)(nbeg(i):nend(i)),psym=3, $
xr=xyr(0:1),yr=xyr(2:3),/xst,/yst
repeat begin & aa=get_kbrd(1)
endrep until (aa eq 'n') or (aa eq 's') or (aa eq 'b')
i=i+1 & if aa eq 'n' then goto,gl1 & if aa eq 's' then goto, gl10
if i eq 0 then goto,gl1 & i=i-2 & goto,gl1
gl2:read,'Type waiting between displays in s: ',dt
gl3:if i ge n then goto,gl10
plot,(plist.x)(nbeg(i):nend(i)),(plist.y)(nbeg(i):nend(i)),psym=3, $
xr=xyr(0:1),yr=xyr(2:3),/xst,/yst
wait,dt & i=i+1 & goto,gl3
gl10:return
end

