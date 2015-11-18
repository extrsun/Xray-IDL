pro psf_params,dis,rs,cntr,perclimit=perclimit,psffile=psffile,gfrac=gfrac,cntrth=cntrth,blow=blow,bhigh=bhigh,spfile=spfile,instr=instr,uarcsec=uarcsec,rso=rso,choice=choice
;+
; get the on-source radius based the encircled energy radii of 
; the PSF or the size of the Gaussian component.
;
; dis - off-axis distances (arcmin) of  sources
; rs - output source radii (in units of data pixels)
; psffile -  the file name of the PSF file (including the directory)
;		needed for PSPC sources
; perclimit - scalar or vector containing the fraction(s) of the PSF energy 
; 		for calculating rs
; blow,bhigh - lower and upper limits of the energy band for calculating
;	the energy-encircled radius 
; spfile - spectral file for calculating
;	the energy-encircled radius 
; uarcsec - if set, rs is in units of arcsec
; rso - if given, only the cntr dependent part will be calculated and will
;	be added into rs as output
; choice - if =1 (def), use the energy dependent PSF (Jerius, D. et
;          al. 2000, SPIE, 4012, 17); otherwise, use the Feigelson, E. D. et al. (2002, ApJ, 574, 258).
; The PSF file can be produced by psf_frac.pro in ~/rosatshell
; 
; 
; writen by wqd, June 2, 2001
;-
if n_params() eq 0 then begin
print,'psf_params,dis,rs,cntr,perclimit=perclimit,psffile=psffile,gfrac=gfrac,cntrth=cntrth,blow=blow,bhigh=bhigh,spfile=spfile,instr=instr,uarcsec=uarcsec,rso=rso'
return
endif
if n_elements(instr) eq 0 then instr=!instr
if n_elements(perclimit) eq 0 then perclimit=0.9
;-------------------------------------------------------
;xmm-newton block
;if strupcase(strtrim(instr,2)) eq 'EPIC' then begin
;    if n_elements(cntr) ne 0 then $ ;including cntr scaling
;      perc=1.-(cntrth/(cntr < 0.05) < (1.-perclimit)) $
;           else perc=perclimit
;       ;scale rs in the range chntrth < cntr < 0.05
;    rs_cntr=(-10.552728*alog(1.0-perc)+3.7677256*tan(perc*!pi/2.0))/50.
;endif 
;-------------------------------------------------------
if n_elements(cntr) ne 0 then begin
	if n_elements(cntrth) eq 0 then cntrth=0.01
	rs_cntr=(1.+1*alog10(cntr/cntrth > 1.))
;	if n_elements(cntrth) eq 0 then cntrth=0.05
;	rs_cntr=(1.+5*alog(cntr/cntrth > 1.))
;	if n_elements(cntrth) eq 0 then cntrth=0.01
;	rs_cntr=(1.+4*alog(cntr/cntrth > 1.))
endif else rs_cntr=1
if n_elements(rso) ne 0 then begin
	rs=rso*rs_cntr
if !debug eq 2 then stop
	return
endif
;-------------------------------------------------------
;xmm-newton block
if strupcase(strtrim(instr,2)) eq 'EPIC' then begin
    rs=50.2888+1.13513*dis+0.0148872*dis^2 ;90% off-axis PSF radius
;        if perclimit lt 0. then begin
;            perc=1.-(cntrth/(cntr < 0.05) < (1.+perclimit))
;            ;scale rs in the range chntrth < cntr < 0.05
;            rs=rs*(-10.552728*alog(1.0-perc)+3.7677256*tan(perc*!pi/2.0))/50.
;        endif else $
         rs=rs/50.2888*(-10.552728*alog(1.0-perclimit)+3.7677256*tan(perclimit*!pi/2.0))
          ;on-axis radius as a function of encircled energy 
;    case perclimit of
;        0.7: rs=rs*0.35   ; crude approximation 
;        0.5: rs=rs*0.2
;        else: if perclimit ne 0.9 then stop,'no XMM PSF implemented!!!'
;    ;from Kate's fit to the 90% source radius, using scalereg.pro
;  endcase 
endif else begin
;-------------------------------------------------------
if n_elements(choice) eq 0 then choice=1
if choice eq 1 then begin 
    if n_elements(spfile) eq 0 then $
	spfile=!cdir+strtrim(instr,2)+'_po0.7.dat'
    if n_elements(blow) eq 0 then begin
	print,'assuming blow = !blow'
	blow=!blow
    endif
    if n_elements(bhigh) eq 0 then begin
	print,'assuming bhigh = !bhigh'
	bhigh = !bhigh
    endif
    case perclimit of
        0.5: psf_coef,blow*1.e-3,bhigh*1.e-3,coef,spfile=spfile,instr=instr,perc=perclimit
        0.9: psf_coef,blow*1.e-3,bhigh*1.e-3,coef,spfile=spfile,instr=instr,perc=perclimit
        else: begin
            psf_coef,blow*1.e-3,bhigh*1.e-3,coef5,spfile=spfile,instr=instr,perc=0.5
            psf_coef,blow*1.e-3,bhigh*1.e-3,coef9,spfile=spfile,instr=instr,perc=0.9
            coef=coef5+(coef9-coef5)/0.4*(perclimit-0.5)
        end
    endcase
;    rs=coef(0)+coef(1)*((dis < 15.)*0.1)^2    
    rs=coef(0)+coef(1)*((dis)*0.1)^2
    print,'coef1,coef2 = ',coef
endif else begin
    coef95=[2.05,-0.55,0.18]
    coef05=[0.43,-0.10,0.05]
    coef=coef05+(coef95-coef05)/0.45*(perclimit-0.5)
;    rs=coef(0)+coef(1)*(dis < 15)+coef(2)*(dis < 15.)^2
    rs=coef(0)+coef(1)*(dis)+coef(2)*(dis)^2
endelse 
;-------------------------------------------------------
;xmm-newton block
endelse
;-------------------------------------------------------
if not keyword_set(uarcsec) then rs=rs/!size_pixel

if n_elements(cntr) ne 0 then rs=rs*rs_cntr 

if !debug eq 2 then stop
return
end
