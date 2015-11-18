pro cursor_cor,cor,xout,yout
xout=[-990.]
yout=[-990.]
LOOP:
if keyword_set(normal) eq 1 then cursor,xpix,ypix,3,/normal,down=down ;/change ;,/down
if !ERR eq 4 then goto,done
print,xpix,ypix
xout=[xout,xpix]
yout=[yout,ypix]
goto,LOOP
done:
xout=(xout(1:*)-cor(0))/(cor(1)-cor(0))
yout=(yout(1:*)-cor(2))/(cor(3)-cor(2))
print,''
if n_elements(xout) gt 0 then $
for n=0,n_elements(xout)-1 do print,xout(n),yout(n)
return
end