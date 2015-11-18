if n_elements(sarea) ne 0 then begin
a=ts*0. & a(sarea)=ts(sarea) & ts=a ;selected region for the background counts
a=tb*0. & a(sarea)=tb(sarea) & tb=a ;selected region for all counts
endif 

;select counts within the central box:
;clist=list(where(abs(list.chipx-512.5) lt 470 and $
;	abs(list.chipy-512.5) le 470,nc)) ;470 is chosen to avoid any effect
					;due to the sqrt(2)x16" dithering.
;flag the reference background counts:
;list_image,clist,xmin,ymin,a,dim,block=block,filter=ts,sel=bsel 
list_image,list,xmin,ymin,a,dim,block=block,filter=ts,sel=bsel 
nc=n_elements(list)
bcid=bytarr(nc)
bcid(bsel)=1

if n_elements(slow) eq 0 then slow=4 ;source signal-to-noise ratio lower limit 
ssel=where(sl.snr gt slow and tb(loc) gt 0.,nssel) 
if nssel eq 0 then begin
    print,'No source with the S/N threshold > ',slow
    return
endif 
vsr=rr*rfac
srr=vsr^2
rsel=[-999]
for k=0,nssel-1 do begin
    kk=ssel(k)
	;choose only those counts for this source:
	ss=where(((list.x-nxp(kk))^2+(list.y-nyp(kk))^2) le srr(kk),nss)
        if nss ne 0 then s=where(abs(list(ss).chipx-512.5) gt 489 or $
                 abs(list(ss).chipy-512.5) gt 489,ns)
;489 is chosen to avoid any effect due to the sqrt(2)x16" dithering.
        if nss eq 0 or ns ne 0 then rsel=[rsel,k]
    endfor
if n_elements(rsel) ne 1 then remove,rsel(1:*),ssel
nssel=n_elements(ssel)
print,'A total of ',nssel,' sources are selected!'
end
