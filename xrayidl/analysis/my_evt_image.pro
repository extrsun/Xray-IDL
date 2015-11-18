pro my_evt_image,evtroot,instr,carr,ta,ba=ba,tbim=tbim,list=list,hdr=hdr $
,cra=cra,cdec=cdec,lhdr=lhdr,gradec=gradec,imblock=imblock,imdim=imdim $
,imccd=imccd,datadir=datadir,bv=bv,nofits=nofits,noexp=noexp,nomap=nomap $
,xmin=xmin,ymin=ymin,bmin=bmin,bmax=bmax $
,bfname=bfname,bmaponly=bmaponly,bimfroot=bimfroot,dfname=dfname,dca=dca $
,dmaponly=dmaponly,row=row,inlist=inlist,paronly=paronly,mbmin=mbmin,mbmax=mbmax,mblock=mblock,tbw=tbw,expt=expt,expfhead=expfhead,tanorm=tanorm
;+
; Name:
;  evt_image
; PURPOSE:
; Master program to construct images in various bands
; with the option to output an event structure that
; can be used to reconstruct different images
;
;*INPUTS:
; evtroot - event file root name
; instr - instrument name (e.g., 'aciss' or 'aciss')
;
;*Optional Inputs:
; gradec - Two element vector contains the RA and Dec of the output image center (deg)
;		def=the axis center
; imblock, imdim - the image pixel block and dimension (which can be
;                  2-element vector)
;		(def = the output from sou_det_params)
; imccd - chosen ccd of the output image (def = the output from sou_det_params)
; datadir - the directory in which the event file and exposure maps are located
;		(def='./')
; bv - bands (def = [1,2,3,4])
; nofits - if set, no fits file output 
; noexp - no exposure map ouput
; nomap - no map (count and exposure) output
; dmaponly - if set, no other map output
; bfname - vector contains the background event file names (including the path)
; bmaponly - if set, only the background maps are produced.
; bimfroot - the root name of the output background image 
;           files (def = 'b_')
; dfname - file name of the smoothed diffuse count map, usually in sou
;          dir
; row - definition of the output evt list
; inlist - input evt list. if provided, this list will be used,
;          instead of the file (useful for a modified list, e.g.,
;          diffuse X-rays)
; paronly - if set, only image parameters and headers are calculated
; mbmin, mbmax, mblock - if given, replacing bmin, bmax and block from sou_det_params
;*Outputs:
; carr - output count images
; ta - output exposure images
; list - event structure
; hdr - image fits header
; cra, cdec - RA and Dec of the image center
; lhdr - the header of the list file
; xmin, ymin - the lower left corner pixel value of the image to be used by
;	list_image
; bmin,bmax - lower and upper energy boundaries of the bands
; ba - output background images
; dca - output array of the casted diffuse background from source
;       detection. Units=counts per output image bin
; expfhead - the head of the exposure map fits files (e.g.,
;             def=evtroot+'_i')
;
; writen by WQD 1/17/2002
; background image construction included by wqd, 5/9/03
; modified by wqd, 3/27/05 to accormodate the XMM-Newton EPIC data
; add keyword expfhead for reading special exposure maps. wqd, 7/9/07
;
;-
if n_params() eq 0 then begin
print,'evt_image,evtroot,instr,carr,ta,ba=ba,tbim=tbim,list=list,hdr=hdr'
print,',cra=cra,cdec=cdec,lhdr=lhdr,gradec=gradec,imblock=imblock,imdim=imdim'
print,',imccd=imccd,datadir=datadir,bv=bv,nofits=nofits,noexp=noexp,nomap=nomap'
print,',xmin=xmin,ymin=ymin,bmin=bmin,bmax=bmax'
print,',bfname=bfname,bmaponly=bmaponly,bimfroot=bimfroot,dfname=dfname,dca=dca'
print,',dmaponly=dmaponly,row=row,inlist=inlist,paronly=paronly,mbmin=mbmin,mbmax=mbmax,mblock=mblock,tbw=tbw,expt=expt,expfhead=expfhead'
return
endif
;set the instrument environment:
cenvset,instr

;setup the source detection parameters:
sou_det_params,instr,dim,block,ccd,bmin,bmax,dsfrac,asfrac,psffile,bpsffile,ftail,aimoff,subfac

if n_elements(mbmin) gt 1 then bmin=mbmin                  ;set mbmin= scalar if not used
if n_elements(mbmax) gt 1 then bmax=mbmax 
if n_elements(mblock) ne 0 then if mblock ne 0 then block=mblock

;get the data file parameters
if n_elements(datadir) eq 0 then datadir=''
fname=datadir+evtroot+'.fits'
file_params,fname,lhdr,cra,cdec,expt,nra,ndec,xoff,yoff,roll,aimoff=aimoff
if !instr eq 'epic' then tagin=['X','Y','PI','CCDNR']
        
; define image parameters
if n_elements(imblock) eq 0 then imblock=block
del=!size_pixel*imblock/3600. ;pixel size in units of deg
if n_elements(imdim) eq 0 then imdim=[0,0]+dim else imdim=[0,0]+imdim
if n_elements(imccd) eq 0 then imccd=ccd
if n_elements(gradec) ne 0 then begin
 trans_dist,cra,cdec,gradec(0),gradec(1),xoff,yoff,/deg,pixsize=!size_pixel 
 endif else gradec=[cra,cdec]

xmin=!pref+xoff-float(imdim(0))*imblock/2. ;low left pixel of the subimage
ymin=!pref+yoff-float(imdim(1))*imblock/2.

cpx=(imdim(0)+1.)*0.5-xoff/imblock
cpy=(imdim(1)+1.)*0.5-yoff/imblock

nb=n_elements(bv)
if nb eq 0 then begin
    nb=n_elements(bmin)
    bv=indgen(nb)+1
endif else begin
    bmin=bmin(bv-1)
    bmax=bmax(bv-1)
endelse 

get_fitshead,'',hdr,del=del,crval=[cra,cdec],equi=2000,cpx=cpx,cpy=cpy $
      ,dim=imdim,type=4 

if keyword_set(paronly) then return ;just for parameters
;====================================================
print,'parameters here=',keyword_set(bmaponly),n_elements(noexp),keyword_set(nomap),keyword_set(dmaponly),n_elements(expfhead)
if keyword_set(bmaponly) eq 0 then begin
;the reference center remains the same even if gradec is given
    if n_elements(noexp) eq 0 and keyword_set(nomap) eq 0 and $
      keyword_set(dmaponly) eq 0 then begin
        if n_elements(expfhead) eq 0 then expfhead=evtroot+'_i'
print,'here tanorm=',tanorm
      map_exp,expt,tbim,ta,fhead=expfhead,mapdir=datadir,bv=bv,hdr=hdr,tbw=tbw,tanorm=tanorm
  endif 

;get the count list
    if n_elements(inlist) ne 0 then list=inlist else begin
        if n_elements(row) eq 0 then $
          ;row={x:0L,y:0L,energy:0,ccd_id:0,detx:0L,dety:0L,time:0.0d}
          row={x:0.,y:0.,energy:0,ccd_id:0}
    endelse 
      list_xray,fname,list,emin=min(bmin),emax=max(bmax),ccd=imccd,row=row,tagin=tagin
  endif
;==================================================
;generate a subimage in the source detection field
if n_elements(nomap) ne 0 then return
;smoothed diffuse images as used in source detection
dca=fltarr(imdim(0),imdim(1),nb)
if n_elements(dfname) ne 0 then begin
    odca=readfits(dfname,dhdr)
    for k=0,nb-1 do begin
        cast,'',hdr,ina=odca(*,*,bv(k)-1),inh=dhdr,outa=im,/psum
        dca(*,*,k)=im 
    endfor
endif

if keyword_set(dmaponly) then return
;background list and exposure normalization
nbfname=n_elements(bfname)
if nbfname ne 0 then begin
 if n_elements(bimfroot) eq 0 then bimfroot='b_' ;evtroot+'_b'
 brow={x:0.,y:0.,energy:0,ccd_id:0}
 ncol=lonarr(nbfname)
 bnorm=fltarr(nbfname)
 tblist=brow
 for kk=0,nbfname-1 do begin
     list_xray,bfname(kk),blist,emin=emin,emax=emax,ccd=imccd,row=brow,nct=nct,tagin=tagin
     fhdr=headfits(bfname(kk),ext=1)
     bexpt=sxpar(fhdr,'livetime')
     if bexpt eq 0 then bexpt=sxpar(fhdr,'exposure')
     if nct ne 0 then begin
     	if bexpt eq 0 then stop,'the background file exposure = 0! need a fix.'
     	tblist=[tblist,blist]
     endif
     ncol(kk)=nct
     bnorm(kk)=expt/bexpt
 endfor
 tblist=tblist(1:*)
endif

carr=intarr(imdim(0),imdim(1),nb)
ba=fltarr(imdim(0),imdim(1),nb)
for k=0,nb-1 do begin
 if n_elements(bfname) ne 0 then begin
	nc=0
        bb=0
	for kk=0,nbfname-1 do begin
	  if ncol(kk) ne 0 then begin
              list_image,tblist(nc:nc+ncol(kk)-1),xmin,ymin,imarr,imdim(0) $
                ,imdim(1),block=imblock,emin=bmin(k),emax=bmax(k),nsel=nsel
		if nsel ne 0 then bb=bb+bnorm(kk)*imarr
                nc=nc+ncol(kk)
          endif 
        endfor
        if n_elements(nofits) eq 0 and nc ne 0 then $
		writefits,bimfroot+strtrim(k+1,2)+'.fits',bb,hdr
        ba(*,*,k)=bb
    endif
 if keyword_set(bmaponly) eq 0 then begin
     list_image,list,xmin,ymin,imarr,imdim(0),imdim(1),block=imblock $
       ,emin=bmin(k),emax=bmax(k)
     if n_elements(nofits) eq 0 then begin
          writefits,'c_'+strtrim(k+1,2)+'.fits',imarr,hdr
          if n_elements(noexp) eq 0 then $
            writefits,'t_'+strtrim(k+1,2)+'.fits',ta(*,*,k),hdr 
    endif 
    carr(*,*,k)=imarr
endif
endfor 
if keyword_set(bmaponly) eq 0 and n_elements(nofits) eq 0 then $
  writefits,'c_b.fits',total(carr,3),hdr
if !debug eq 3 then stop
return
end
