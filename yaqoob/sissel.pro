pro sissel,x,y,slist,blist,npxs,npxb,sr=sr,br=br
; Use to select source and background files from sis event list
; or to find source centroids via the cursor, i.e. 'select,x,y'
; 'sr' and 'br' are in pixel coordinates.
; If no backgrnd file is desired set br=0.
if n_params(0) eq 0 then begin
print,'SISSEL,x,y,slist,blist,npxs,npxb,sr=sr,br=br'
retall
end
if n_params(0) eq 2 then begin
; Just print source coordinates using the cursor
plot,x,y,psym=3,/xst,/yst & print,'Use cursor to mark center'
print,'Press any key for next source, ''s'' to stop'
repeat begin
cursor,xc,yc,1 & print,xc,yc
aa=get_kbrd(1) & endrep until aa eq 's'
retall
end
plot,plist.x,plist.y,psym=3,/xst,/yst & print,'Use cursor to mark center'
cursor,xc,yc,1
ang=findgen(361)*!pi/180. & cosa=cos(ang) & sina=sin(ang)
oplot,xc+sr*cosa,yc+sr*sina
if (br gt 0.) then oplot,xc+br*cosa,yc+br*sina
if (br gt 0.) then oplot,xc+br*cosa,yc+br*sina
print,'Type ''c'' to continue or ''s'' to select new regions'
repeat begin & kb=get_kbrd(1) & endrep until (kb eq 'c') or (kb eq 's')
if (kb eq 's') then goto,sel5
rad=sqrt((x-xc)^2+(y-yc)^2)
isel=where(rad le sr) & xs=x(isel) & ys=y(isel) & npxs=!pi*sr*sr
isel=where((rad gt sr) and (rad le br)) & npxb=!pi*br*br 
if (br gt 0.) then blist=plist(isel)
;help,slist,blist,npxs,npxb
netscnt=n_elements(slist)-n_elements(blist)*npxs/npxb
err=sqrt(n_elements(slist)+n_elements(blist)*(npxs/npxb)^2)
print,'         Source coordinates, net src cnts and err'
print,xc,yc,netscnt,err
sel5:return
end



