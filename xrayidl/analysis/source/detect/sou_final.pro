;sou_final
; calculate hardness ratios of selected sources and generate output table and
; plots
;
;*Outputs:
;evtfname_map*_hr - Source list in fits format
;evtfname_map*_hr.tex -  Source list in a (La)TeX format, 
;	including hardness ratios
;evtfname_map*_hr?.ps - hardness ratio plots
; ...
;================================
 ; in case that sou_final is runned without sou_main in the session:
if n_elements(noplot) eq 0 then noplot=0
if n_elements(soufile) eq 0 then begin
    if !instr eq 'epic' then soufile='pn_map70BSH' else $
      soufile='evt2file_new_clean_map70BSH'
endif
if n_elements(evtfname) eq 0 then begin
    if !instr eq 'epic' then evtfname='pn' else $
      evtfname='evt2file_new_clean'
endif
if n_elements(probth) eq 0 then probth=-6 ;for single observation
;if n_elements(slist) eq 0 then $
sou_fits_info,soufile,slist,/all,probth=probth,radec_off=radec_off
if n_elements(sexcl) ne 0 then  remove,sexcl-1,slist
nslist=n_elements(slist)
slist.sn=indgen(nslist)+1

;if  n_elements(vpfname) eq 0 then $
sou_det_params,instr,dim,block,ccd,bmin,bmax,dsfrac,asfrac,psffile,bpsffile,ftail,aimoff,subfac,vpfname=vpfname,vrfname=vrfname,tabfile=tabfile,dhrch=dhrch,bname=bname
if n_elements(nvrfname) ne 0 then vrfname=nvrfname
if n_elements(nvpfname) ne 0 then vpfname=nvpfname

if n_elements(mbmin) gt 1 then bmin=mbmin ;set mbmin= scalar if not used
if n_elements(mbmax) gt 1 then bmax=mbmax 
if n_elements(mblock) ne 0 then if mblock ne 0 then block=mblock
;change the default block

if n_elements(sbmin) ne 0 then if total(sbmin) ne 0 then bmin=sbmin
if n_elements(sbmax) ne 0 then if total(sbmax) ne 0 then bmax=sbmax

;Hardness ratio analysis:
; with option for an output of the final source list into a nice LaTex format
; only sources with slow > hrslow are calculated
hrslow=4. ;signal-to-noise of the broad band count rate for calculating HRs
if n_elements(hrth) eq 0 then hrth=0.2 ; threshold of HR error for hardcopy
;output in the table and plots.
;hr_out,slist,hr,hr2,hre,hr2e,outf=soufile+'_out',hrslow=4,probth=probth
if n_elements(mfile) eq 0 then mfile=0
if n_elements(dhrch) eq 0 then dhrch=0
if mfile ne 0 then begin ;indicating that this is a merged source list with hr
    print,'Hardness ratios presented in the source structure are used!'
    hr=slist.hr &  hre=slist.hre 
    hr1=slist.hr1 &  hr1e=slist.hr1e 
    hr2=slist.hr2 &  hr2e=slist.hr2e 
endif else $
      hr_cal,slist,slist,hr,hre,hr2,hr2e,hr1,hr1e,hrslow=hrslow $
             ,probth=probth,nband=nband,dhrch=dhrch ;,soufile=soufile
;hr_out,slist,slist,hr,hre,hr1,hr1e,hr2,hr2e,outf=soufile+'_hr',texf=soufile+'_hr.tex',hrslow=hrslow,probth=probth,nband=nband
;if dhrch eq 2 then
;tabfile='$PUBDIR/tab_chandra/tab_source_4b_3hr.tex'

hr_out,slist,outf=soufile+'_hr',texf=soufile+'_hr.tex',nband=nband,hrth=hrth,dig=dig,npsyserr=npsyserr,tabfile=tabfile,/silent,dhrch=dhrch,apsyserr=apsyserr
sn=slist.sn

if mfile eq 0 then begin
    if  n_elements(evtfname) eq 0 then evtfname='evt2file_new_clean'
    if  n_elements(evtfdir) eq 0 then evtfdir='../xdata/'
    if n_elements(cra) eq 0 then begin
        fname=evtfdir+evtfname+'.fits'
        file_params,fname,hdr,cra,cdec,expt,nra,ndec,xoff,yoff,roll,aimoff=aimoff
    endif
endif
;========================================
;Hardness ratios:
s2=where(hre lt hrth and hr2e lt hrth,ns2)
print,ns2,' sources are selected for the HR2 plot (s2): '
print,s2+1
if nband eq 4 then s1=where(hre lt hrth and hr1e lt hrth,ns1)
print,ns1,' are selected for the HR1 plot (s1): '
print,s1+1

if n_elements(h2yr) ne 0 then yrange2=h2yr else yrange2=[-1.,1.]
if n_elements(h2xr) ne 0 then xrange2=h2xr else xrange2=[-1.,1.]
if n_elements(h1xr) ne 0 then xrange1=h1xr else xrange1=[-1.,1.]
if n_elements(h1yr) ne 0 then yrange1=h1yr else yrange1=[-0.5,1.]

if noplot eq 0 then begin
	;plot color-color diagram:
	!x.style=1
	!y.style=1
	if n_elements(mpv) eq 0 then mpv=[1,2,3]
	;if n_elements(mrv) eq 0 then mrv=[0.1,0.3,1,3,10]
	if n_elements(mrv) eq 0 then begin
            if instr eq 'aciss' then mrv=[0.2,1,1.5,2] else mrv=[0.3,1,2,4]
        endif 
	if n_elements(mnv) eq 0 then mnv=[1.,10,20,40,100,300] 
                              ;mnv=[1.,10,30,100,300]
	if dhrch eq 2 then begin
        plot_xy,hr(s2),hr(s2)-hre(s2),hr(s2)+hre(s2),hr2(s2),hr2(s2)-hr2e(s2) $
	 ,hr2(s2)+hr2e(s2),xrang=xrange2,yr=yrange2,xt='HR',yt='HR2'
	hr_model,vpfname,/noer,thick=3,mpv,mnv,hrch=2,dhrch=dhrch
	hr_model,vrfname,/noer,mrv,mnv,hrch=2,dhrch=dhrch
	xyouts,hr(s2)-0.15,hr2(s2)+0.05,sn(s2)
        endif
;	if nband eq 4 then begin
	if dhrch eq 0 then begin
            plot_xy,hr(s1),hr(s1)-hre(s1),hr(s1)+hre(s1) $
              ,hr1(s1),hr1(s1)-hr1e(s1),hr1(s1)+hr1e(s1),yrang=yrange1 $
	    	,xr=xrange1,xt='HR',yt='HR1'
	    hr_model,vpfname,/noer,thick=3,mpv,mnv,hrch=1,pch=0,dhrch=dhrch
	    hr_model,vrfname,/noer,mrv,mnv,hrch=1,pch=0,dhrch=dhrch
	    xyouts,hr(s1)-0.1,hr1(s1)+0.01,sn(s1)
	endif
    endif
;-----------
 if mfile eq 0 then sou_fits_reg,hdr,soufile+'_hr.reg',slist,fac=subfac

if n_elements(plotout) eq 0 then plotout=1
if plotout ne 0 then  begin
	set_plot,'ps'
	device,bits=8,/land,color=0,xsize=18.,ysiz=18,yoff=22,xoff=4
	cs=0.7
	device,filename=soufile+'_hr2.ps'
	plot_xy,hr(s2),hr(s2)-hre(s2),hr(s2)+hre(s2),hr2(s2),hr2(s2)-hr2e(s2) $
	 ,hr2(s2)+hr2e(s2),xrang=xrange2,yr=yrange2,xt='HR',yt='HR2'
	hr_model,vpfname,/noer,thick=3,mpv,mnv,hrch=2,dhrch=dhrch
	hr_model,vrfname,/noer,mrv,mnv,hrch=2,dhrch=dhrch
	xyouts,hr(s2)-0.15,hr2(s2)+0.05,sn(s2)
	device,/close
	print,soufile+'_hr2.ps is created for the color-color diagram!'
	if nband eq 4 then begin
            device,filename=soufile+'_hr1.ps'
		plot_xy,hr(s1),hr(s1)-hre(s1),hr(s1)+hre(s1) $
                  ,hr1(s1),hr1(s1)-hr1e(s1),hr1(s1)+hr1e(s1),yrang=yrange1 $
			,xr=xrange1,yt='HR1',xt='HR'
		hr_model,vpfname,/noer,thick=3,mpv,mnv,hrch=1,pch=0,dhrch=dhrch
		hr_model,vrfname,/noer,mrv,mnv,hrch=1,pch=0,dhrch=dhrch
		xyouts,hr(s1)-0.15,hr1(s1)+0.01,sn(s1)
            device,/close
            print,soufile+'_hr1.ps is created for the color-color diagram!'
	endif
	;========================================
        if n_elements(cmb) ne 0 then begin 
            ;cmb - broad band intensity map from sou_main
	device,filename=soufile+'_cm.ps'
	; Overplay of source positions on the broadband image:
        ncmb=cmb-imdiv(total(cbma,3),tb)*norm ;subtract the smooth backg
        writefits,soufile+'_cm.fits',ncmb,hh
        if !d.name eq 'X' then color=!d.n_colors-1 else color=0
        cont_grey,ncmb,hh,/noc,cor=cor,greymin=greymin,greymax=greymax*40 $
		,greyl=2,/def,/ps,barf=0.,mr=0,/noim,thick=2,f_c=color
        image_counts,list,cor,xmin,ymin,block*long(dim),block*long(dim),symsize=1,color=color

;	cont_grey,ncmb,hh,/noc,cor=cor,greymin=greymin,greymax=greymax $
;		,greyl=2,/full,/def,barf=0,f_c=-1,/ps
        plot_reg,cor,hh,soufile+'_hr.reg',color=color ;,factor=subfac
        source_plot,'',cor,hh,psym=6.,sym=0.5 $
          ,/sou_no,sra=slist.ra,sdec=slist.dec,sn=slist.sn $
         ,nssh=slist.sn,xsh=(!size_pixel*1.3)*slist.sradius $
         ,ysh=(!size_pixel*1.3)*slist.sradius,s_c=color
        device,/close
        endif
        if n_elements(cmbc) ne 0 then begin 
;cmb - broad band intensity map from sou_main
	device,filename=soufile+'_cmc.ps'
        if n_elements(mcdim) eq 0 then mcdim=cdim ;final central plot dim
        if cdim gt mcdim then begin
            mbcmbc=image_cut(bcmbc,mcdim/2,/pix,/rec,imcoff=mcimcoff)
            coff=(cdim-mcdim)/2+mcimcoff
            get_fitshead,intarr(mcdim,mcdim),mcmbch,cmbch,cdim=coff
            mxmine=xmine+coff(0)*cblock
            mymine=ymine+coff(1)*cblock
        endif else begin
            mcmbch=cmbch
            mbcmbc=bcmbc
            mxmine=xmine
            mymine=ymine
        endelse 
        cont_grey,mbcmbc,mcmbch,/noc,cor=cor,greymin=greymin,greymax=greymax*40 $
		,greyl=2,/def,/ps,barf=0.,mr=0,/noim,thick=3,f_c=color
        image_counts,list,cor,mxmine,mymine,cblock*long(mcdim),cblock*long(mcdim),symsize=1,color=color
        plot_reg,cor,mcmbch,soufile+'_hr.reg',color=color ;,factor=subfac
        source_plot,'',cor,mcmbch,psym=6.,sym=0.7 $
          ,/sou_no,sra=slist.ra,sdec=slist.dec,sn=slist.sn $
         ,s_c=color,nssh=slist.sn,xsh=(!size_pixel*0.8)*slist.sradius $
         ,ysh=(!size_pixel*1.3)*slist.sradius
;	cont_grey,mbcmbc,mcmbch,/noc,cor=cor,greymin=greymin,greymax=greymax*40 $
;		,greyl=2,/full,/def,barf=0,f_c=-1,/ps
;        plot_reg,cor,heh,soufile+'_hr.reg',color=!d.n_colors-1 ;,factor=subfac
;	source_plot,soufile+'_hr',cor,heh,slow=0,probth=probth,psym=6,sym=2,/fits
        device,/close
        endif

	;========================================
	set_plot,'x'
        if n_elements(tb) ne 0 then begin
;	get the source-removed mask:
        if mfile eq 0 then $
          tbs=source_sub_v(tb,nra,ndec,slist.ra,slist.dec,slist.cntrb $
	,block=block $
;        ,fac=subfac,/deg,perc=asfrac,psffile=psffile $
;        ,fac=subfac,/deg,sradius=slist.sradius/(block*!size_pixel) $
        ,fac=subfac,/deg,sradius=slist.sradius $
          ,cra=cra,cdec=cdec)
        endif
endif 
; if mfile eq 0 then sou_fits_reg,hdr,soufile+'_hr.reg',slist,fac=subfac
print,'sou_final.ps is created!'
	
end
