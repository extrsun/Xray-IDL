pro scan_batch,tss,tb=tb,dim=dim,radius=radius,slow=slow,factor=factor,exptail=exptail,iterate=iterate,subfirst=subfirst
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -- scan_batch,tss,tb=tb,dim=dim,radius=radius'
print,',slow=slow,factor=factor,exptail=exptail,iterate=iterate,subfirst=subfirst'
return
endif
if n_elements(exptail) eq 0 then exptail='all'
if n_elements(dim) eq 0 then dim=512
;if n_elements(radius) eq 0 then radius=224
if n_elements(slow) eq 0 then slow=3.5
if n_elements(factor) eq 0 then factor=1.5
if n_elements(iterate) eq 0 then iterate=1
;!data_dir='/data1/wqd/archives/'+!seq_no+'/'
if n_elements(tb) eq 0 then begin
if keyword_set(subfirst) ne 0 then begin
get_image,t1,c1,dim=dim,blow=4,bhigh=5,exptail=exptail,ts1,slow=slow,factor=factor
get_image,t2,c2,dim=dim,blow=6,bhigh=7,exptail=exptail,ts1,slow=slow,factor=factor
endif else begin
get_image,t1,c1,dim=dim,blow=4,bhigh=5,exptail=exptail;,ts,slow=slow,factor=factor
get_image,t2,c2,dim=dim,blow=6,bhigh=7,exptail=exptail;,ts,slow=slow,factor=factor
endelse
t=fltarr(dim,dim,2)
c=intarr(dim,dim,2)
t(*,*,0)=t1
t(*,*,1)=t2
c(*,*,0)=c1
c(*,*,1)=c2
ts=t
if keyword_set(subfirst) ne  0 then  ts(where(ts1 le 0.))=0.
exp_broad,40.,c,t,ts,tb
sel=where(ts le 0.)
tb(sel)=ts(sel)
cb=c1+c2
endif else make_image,cb,dim=dim,emin=52,emax=201,exptail=exptail
image_center,cra,cdec
blow=4
bhigh=7
;scan_v,cra,cdec,cb,tb,tss,radius=radius,blow=blow,bhigh=bhigh,iterate=1,threshold=3.5,/append
if keyword_set(subfirst) eq 0 then begin
	scan_v,cra,cdec,cb,tb,tss,radius=radius,blow=blow,bhigh=bhigh,iterate=1,threshold=4.5 
endif else tss=ts
;scan_v,cra,cdec,cb,tss,tss,radius=radius,blow=blow,bhigh=bhigh,iterate=iterate,/append,threshold=4.0
scan_v,cra,cdec,cb,tss,tss,radius=radius,blow=blow,bhigh=bhigh,iterate=iterate,/append,threshold=3.5
scan_v,cra,cdec,cb,tss,tss,radius=radius,blow=blow,bhigh=bhigh,iterate=iterate,/append,threshold=3.5
end

