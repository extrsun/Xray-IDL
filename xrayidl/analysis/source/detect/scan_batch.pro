pro scan_batch,tbs,tb=tb,dim=dim,radius=radius,slow=slow,flow=flow,factor=factor,exptail=exptail,iterate=iterate,subfirst=subfirst,ratio=ratio,anal=anal,frac=frac,file=file,append=append,block=block
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -- scan_batch,tbs,tb=tb,dim=dim,radius=radius'
print,',slow=slow,flow=flow,factor=factor,exptail=exptail,iterate=iterate,subfirst=subfirst,ratio=ratio,anal=anal,frac=frac,file=file'
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
	 get_image,tm,cm,dim=dim,blow=4,bhigh=5,exptail=exptail,tsm,slow=slow,factor=factor,flow=flow,block=block
 	 get_image,th,ch,dim=dim,blow=6,bhigh=7,exptail=exptail,tsm,slow=slow,factor=factor,flow=flow,block=block
	endif else begin
 	 get_image,tm,cm,dim=dim,blow=4,bhigh=5,exptail=exptail;,ts,slow=slow,factor=factor,block=block
 	 get_image,th,ch,dim=dim,blow=6,bhigh=7,exptail=exptail;,ts,slow=slow,factor=factor,block=block
	endelse
	t=fltarr(dim,dim,2)
	c=intarr(dim,dim,2)
	t(*,*,0)=tm
	t(*,*,1)=th
	c(*,*,0)=cm
	c(*,*,1)=ch
	ts=t
	if keyword_set(subfirst) ne  0 then  ts(where(tsm le 0.))=0.
	exp_broad,40.,c,t,ts,tb
	tb(where(ts le 0.))=ts(where(ts le 0.))
	cb=cm+ch
endif else begin
	tfile=!seq_no+'_gti'+exptail+'.dat'
	make_image,cb,dim=dim,emin=52,emax=201,tfile=tfile
endelse
image_center,cra,cdec
blow=4
bhigh=7
if keyword_set(subfirst) eq 0 then begin
	scan_v,cra,cdec,cb,tb,tbs,radius=radius,blow=blow,bhigh=bhigh,iterate=1,frac=frac,threshold=4.5,file=file,append=append
endif else tbs=tb
scan_v,cra,cdec,cb,tbs,tbs,radius=radius,blow=blow,bhigh=bhigh,iterate=iterate,frac=frac,file=file,/append,threshold=4.0
scan_v,cra,cdec,cb,tbs,tbs,radius=radius,blow=blow,bhigh=bhigh,iterate=2,/append,frac=frac,file=file,threshold=3.

soufile=!seq_no+'_sou'
if keyword_set(anal) ne 0 then begin
	get_bimage,cb,tb,tbs,slow=slow,exptail=exptail,factor=factor
  anal_v,cra,cdec,cb,tb,tbs,tbs,radius=50,blow=blow,bhigh=bhigh,threshold=3.5, $
	infile=soufile+'.dat',outfile=soufile+'anal.dat'
endif

if keyword_set(ratio) ne 0 then begin
get_image,ts,cs,tbs,blow=2,bhigh=2,exptail=exptail,slow=slow $
	,soufile=soufile+'anal.dat',block=block
get_ratios,cs,ts,cm,tm,ch,th,tbs,blow=2,bhigh=2,infile=soufile+'anal.dat', $
	outfile=soufile+'ratio.dat',slow=slow
endif

end

