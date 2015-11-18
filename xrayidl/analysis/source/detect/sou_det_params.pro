pro sou_det_params,instr,dim,block,ccd,bmin,bmax,dsfrac,asfrac,psffile $
,bpsffile,ftail,aimoff,subfac,vpfname=vpfname,vrfname=vrfname,dhrch=dhrch,tabfile=tabfile,bname=bname
;,cfac_ssr=cfac_ssr,cdsfrac=cdsfrac,fac_ssr=fac,cblock=cblock,cdim=cdim
;+
; Parameter file for program sou_main. 
;
; This file needs to be copied to qthe
; directory where sou_main is to be excecuted.
;
;*Inputs:
; instr - instrument type: 'acisi' or 'aciss'
;*Outputs:
; block - block of the image pixels 
; ccd - selected ccd numbers  
; bmin, bmax - the lower and upper energy limits of the sub-broad bands 
;		which should be optimized according to absorption
; tbw - weights that are needed for constructing broad-band exposure map
; 	(spectral model-dependent, but nearly instrumentally independent)
; vpfname,vrfname - file names for predicted count rate of the 
;	powerlaw and Raymond models as function of T(index) or N_H
;
; itot - conversion from the units of cm^2 s to s, which can be produced 
; 		by map_exp.pro: !debug=2, print,t1/expt
; dhrch - the choice for the HR definition (the same for bands=3): 
;         for dhrch=0 (def), HR=(c(2:3)-c(1))/(c(2:3)+c(1)); for dhrch=1, 
;         HR=(c(2:3)-c(0:1))/(c(2:3)+c(0:1))
;; for dhrch=2, HR=(c(2)-c(0:1))/(c(2)+c(0:1))
; tabfile - the latex header and tail of the source table.
; cblock - block for the central detection
; cdim - dimension of the central detection
; cfac_ssr - detection fraction of the radius for the central
;            detection
; cdsfrac - dsfrac in the central region

; Other parameters are going to be read into sou_main for source detection
; 
; written by wqd, 7/8/2001 
;-
if n_params() eq 0 then begin
print,'Calling Seq. - sou_det_params,instr,dim,block,ccd,bmin,bmax'
print,',dsfrac,asfrac,psffile,bpsffile,ftail,aimoff,subfac,vpfname=vpfname,vrfname=vrfname,dhrch=dhrch,tabfile=tabfile'
return
end
;source detection map dimension
dim=1024 
;------------------------------------------------
;fraction for source count encircled radius
dsfrac=0.7 ;for source detection (scan_map)
ftail=strtrim(nint(dsfrac*100),2) ;file tail
;asfrac=0.9 ;for source analysis (ml_anal, source_sub_v, sou_ratio)
;subfac=1.5 ;changed from 2 to 1.5 on May 18, 2005 
asfrac=0.7 & subfac=2 ;to revised on July 17, 2007
;source subtraction radius factor in units of the energy-encircled
;radius

;fac_ssr=1.
;cblock=1 & cdim=1024 & cfac_ssr=0.5 & cdsfrac=0.5
;------------------------------------------------
if n_elements(dhrch) eq 0 then dhrch=0
tabfile='$PUBDIR/tab_chandra/tab_source_4b_hr'+strtrim(dhrch,2)
;source detection map bin size in units of data pixel
case instr of
	'epic': begin ;for NH=0.03
		block=80
		ccd=indgen(12)+1
		;bmin=[0.2,0.5,2.,4.5]
         	;bmax=[0.5,2.,4.5,7.5]
		bmin=[0.5,1.,2.,4.5]
         	bmax=[1.,2.,4.5,7.5]
		tbw=[0.257134,0.469862,0.179586,0.0934174] ;weight by the arf
		;produced in gcs:~/script_chandra/weights/acisi_fakarf.xcm
		;see /net/xray/pub/cal_scripts/memo_weights
		;bname=['0.2_0.5','0.5_2.','2._4.5','4.5_7.5']
                bname=['0.5_1.','1._2.','2._4.5','4.5_7.5']
		itot=[1,1,1,1]*1.
                dim=512
                dsfrac=0.4
;                asfrac=0.7
;                subfac=1.
                asfrac=0.4
                subfac=2.
                tabfile=tabfile+'_xmm.tex'
                cblock=0 ;no separate central detection
	end
	'acisi': begin ;for NH=0.03
		block=3 
		ccd=[0,1,2,3]
		bmin=[0.5,1.,2.,4.]
         	bmax=[1.,2.,4.,8.]
;		tbw=[0.401588,0.292407,0.188253,0.117751]
		tbw=[0.257134,0.469862,0.179586,0.0934174] ;weight by the arf
		;produced in gcs:~/script_chandra/weights/acisi_fakarf.xcm
		;see /net/xray/pub/cal_scripts/memo_weights
		bname=['0.5_1.','1._2.','2._4.','4._8.']
		itot=[211.77343,546.56173,309.31124,247.49074]
                tabfile=tabfile+'.tex'
	end
	'acisi_low': begin ;for N_H=10
		block=3 
		ccd=[0,1,2,3]
;		bmin=[1.,3.,5.]
;         	bmax=[3.,5.,8.]
;		defsysv,'!tbw',[0.0981564,0.421947,0.479896]
;		bname=['1._3.','3._5.','5._8.']
		bmin=[1.,2.5,4.,6.]
         	bmax=[2.5,4.,6.,9.]
		;tbw=[0.0323830,0.238315,0.373177,0.356125]
		tbw=[0.0389159,0.336057,0.485153,0.139874] ;weight by the arf
		bname=['1._2.5','2.5_4.','4._6.','6._9.']
		itot=[314.59781,355.00261,313.76682,96.879970]
                tabfile=tabfile+'.tex'
		end
	'aciss': begin ;for NH=0.03
		block=2 
		ccd=[6,7,8]
		bmin=[0.3,0.7,1.5,3.] 
		bmax=[0.7,1.5,3.,7.]
		;tbw=[0.405147,0.296608,0.172852,0.125393]
		tbw=[0.283934,0.423025,0.197086,0.096] ;weight by the arf
		bname=['0.3_0.7','0.7_1.5','1.5_3.','3._7.']
		itot=[300.03708,620.58543,521.84206,326.40636]
                tabfile=tabfile+'.tex'
		end
    endcase
defsysv,'!tbw',tbw
defsysv,'!itot',itot
bmin=bmin*1.e3
bmax=bmax*1.e3

; model count rate directory:
if n_elements(tabdir) eq 0 then tabdir='$PUBDIR/tab_chandra/'
vpfname=tabdir+strtrim(instr,2)+'_vp_a1.0e'+bname
vrfname=tabdir+strtrim(instr,2)+'_vr_a1.0e'+bname

;offset of the image center from the aiming point of the observation:
aimoff=[0,0] ;in units of data pixel
;------------------------------------------
;define the PSF file, which should be consistent with the energy range used 
psffile=!cdir+'psf_po0.7_'+strtrim(instr,2)+'_0.dat' 

; psf files for individual bands:
bpsffile=!cdir+'psf_po0.7_'+strtrim(instr,2)+'_'+['1','2','3','4']+'.dat' ;default

;Alternative psf file can be produced, using 
;psf_bb,1.5,7,a,outf='psf_po0.7_aciss_4.dat',instr='aciss',frac 
return
end
