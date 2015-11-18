PRO SETX,colors
if n_elements(colors) eq 0 then colors=249
;
     window,0,re=2,xsize=512,ysize=512,colors=colors
;
!PROMPT='X_Wind>'
SET_PLOT,'x'
;
END

