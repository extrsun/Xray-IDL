pro cursor_cor,cor,xout,yout,device=device
;-
; get cordinates of cursors in the picture coodinates which are independent
; of devices
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - cursor_cor,cor,xout,yout'
return
endif
if keyword_set(device) eq 0 then normal=1
xout=[-990.]
yout=[-990.]
LOOP:
  cursor,xpix,ypix,3,normal=normal,device=device,down=down ;/change ;,/down
if !ERR eq 4 then goto,done
print,xpix,ypix
xout=[xout,xpix]
yout=[yout,ypix]
goto,LOOP
done:
if keyword_set(device) eq 0 then begin
	xout=(xout(1:*)-cor(0))/(cor(1)-cor(0))
	yout=(yout(1:*)-cor(2))/(cor(3)-cor(2))
endif else begin 
	xout=xout(1:*)
	yout=yout(1:*)
endelse

print,''
if n_elements(xout) gt 0 then $
for n=0,n_elements(xout)-1 do print,xout(n),yout(n)
return
end