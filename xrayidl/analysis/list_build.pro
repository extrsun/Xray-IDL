pro list_build,tab,hdr,tagv,plist,xx=xx,yy=yy,pi=pi,time=time,dxx=dxx,dyy=dyy
;+
; convert the table data of events into a list.
;
;*INPUTS:
; tab - tab from direct reading of a fits binary table
; hdr - the header of the table
; tagv - the tags (as used in the table) of the columns that need to be included
;	the output list
;
;*OUTPUTS:
;  plist - structure containing information of individual counts
;
;*OPTIONS:
; to be used when data need to be reformated (e.g., the event file from MIDES) so 
; both tab and hdr will not be used.
; xx, yy, pi, time - columns of data to be included: x and y pixel coordinates, 
;		P channel, and count arrival time. Minimum set
; dxx, dyy - expaned input: detector x and y pixel coordinates
;
; written by wqd, Aug 17, 2000
;-
; 
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - list_build,tab,hdr,tagv,plist,xx=xx,yy=yy,pi=pi,time=time,dxx=dxx,dyy=dyy'
return
endif

if n_elements(xx) eq 0 then begin
	sz=size(tab) 
	nct=sz(2)
endif else nct=n_elements(xx)

if n_elements(tagv) gt 4 or n_elements(dxx) ne 0 then begin
row = {xevent,x:0.,y:0.,pi:0L,time:0.0D0,dx:0L,dy:0L}
plist = replicate(row,nct)
if n_elements(xx) eq 0 then  plist.x =   tbget(hdr,tab,tagv(0)) else plist.x=xx
if n_elements(yy) eq 0 then  plist.y =   tbget(hdr,tab,tagv(1)) else plist.y=yy
if n_elements(pi) eq 0 then  plist.pi =  tbget(hdr,tab,tagv(2)) else plist.pi=pi
if n_elements(time) eq 0 then plist.time =tbget(hdr,tab,tagv(3))  else plist.time=time
if n_elements(dxx) eq 0 then plist.dx =  tbget(hdr,tab,tagv(4))  else plist.dx=dxx
if n_elements(dyy) eq 0 then plist.dy =  tbget(hdr,tab,tagv(5)) else plist.dy=dyy
endif else begin
row = {xeve,x:0.,y:0.,pi:0L,time:0.0D0}
if n_elements(xx) eq 0 then  plist.x =   tbget(hdr,tab,tagv(0)) else plist.x=xx       
if n_elements(yy) eq 0 then  plist.y =   tbget(hdr,tab,tagv(1)) else plist.y=yy       
if n_elements(pi) eq 0 then  plist.pi =  tbget(hdr,tab,tagv(2)) else plist.pi=pi      
if n_elements(time) eq 0 then plist.time =tbget(hdr,tab,tagv(3))  else plist.time=time
endelse
return
end
