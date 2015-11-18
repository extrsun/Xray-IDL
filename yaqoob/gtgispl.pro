pro gtgispl,rtype,px,py,glist,slist,blist
; take a photon list (glist), type of region selection (rtype)
; and parameters of the region selection (px and py) to create
; two new selection regions for src and bkgd (ssel and bsel)
x=glist.x
y=glist.y
if (rtype eq 1) then begin
	r1sq = (px(1)-px(0))*(px(1)-px(0))+(py(1)-py(0))*(py(1)-py(0))
	r2sq = (px(2)-px(0))*(px(2)-px(0))+(py(2)-py(0))*(py(2)-py(0)) 
	rad = (x-px(0))*(x-px(0))+(y-py(0))*(y-py(0))
	slist = glist(where(rad le r1sq))  
	blist = glist(where((rad gt r1sq) and (rad le r2sq)))
	print,'size slist in gtgispl',size(slist)
	print,'size blist in gtgispl',size(blist) 
endif
return
end
