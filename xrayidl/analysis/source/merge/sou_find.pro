pro sou_find,cor,hdr,slist,srav,sdecv,psym=psym,s_c=s_c,symsize=symsize,glac=glac
;+
; find source information (e.g., coordinates) by clicking on the screen
;-
if n_params() eq 0 then begin
	print,'Calling Seq. - sou_find,cor,hdr,slist,srav,sdecv,psym=psym,s_c=s_c'
	print,',symsize=symsize,glac=glac'
	return
endif
sou_struct_out,slist,stext
if keyword_set(glac) then glactc,slist.ra,slist.dec,2000,sra,sdec,1,/deg $
else begin
	sra=slist.ra &sdec=slist.dec
endelse 
srav=[-0.0d]
sdecv=[-0.0d]
print,''
print,'Left key -> select sources; Right -> exit'
again:
pause
print,!err
case !err of 
	1: begin
	   cursor_posi,cor,hdr,sras,sdecs,/noloop
	   trans_dist,sras,sdecs,sra,sdec,xp,yp,/das,/deg,angle=ang
	   ang=min(ang,k)
	   print,stext(k)
	   print,ang,sra(k),',',sdec(k)
	   source_plot,'',cor,hdr,psym=psym,s_c=s_c $
	   ,sra=sra(k),sdec=sdec(k),symsize=symsize
		if !debug eq 2 then stop
	   srav=[srav,sra(k)]
	   sdecv=[sdecv,sdec(k)]
	   end
	4: goto, out
endcase
goto, again
out:
srav=srav(1:*)
sdecv=sdecv(1:*)
for k=0,n_elements(srav)-1 do print,k,srav(k),sdecv(k)
return
end