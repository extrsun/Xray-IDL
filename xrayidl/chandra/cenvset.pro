pro cenvset,instr,cdir=cdir,blow=blow,bhigh=bhigh,spfile=spfile
;+
; define Chandra system instrument parameters for Chandra data
;
; instr - instrument name ('acisi', 'hrc', or 'aciss')
; cdir - local calibration file directory (def=''$PUBDIR/cal_chandra/')
; blow, bhigh - lower and upper energy limits of the broad band (eV)
; 	for calculating the PSF.
; spfile - the spectral waiting file for calculating the PSF
;	(check psf_params.pro to see if it is actually used)
; tanorm - exposure normalization (in all 4 bands) 
; 	that is muliplied to the instrument maps to get the exposure
; 	maps and was calcualted using the old map_exp.pro, which is 
; now renamed as exp_map_test.pro. To use the later, .run map_exp_test 
; (wqd, Sept 9, 2003). The normalization is to the pre-ice-blocking
; decay of the arf was dicovered. So the exposure map created this way
; will account for this decay (ie., the on-axis exposure value of the
; first band can be substantially smaller the livetime of a later observation. 
;
;written by wqd, 6/4/2001
; include !tanorm for map_exp.pro, wqd, Sept 9, 2003
;-
;
;sample to call for epic data:
;cenvset,'epic',cdir='/home/kwhitaker/ben/',blow=300,bhigh=10000
;

if n_params() eq 0 then begin
print,'CALLING SEQUENCE - cenvset,instr,cdir=cdir,blow=blow,bhigh=bhigh,spfile=spfile'
return
endif
On_error,2    
!instr=instr
defsysv,'!tabdir','$PUBDIR/tab_chandra/' 

if n_elements(cdir) eq 0 then $
	defsysv,'!cdir','$PUBDIR/cal_chandra/' $
	else defsysv,'!cdir',cdir
case !instr of
'sxis': begin ;Suzaku XIS
        defsysv,'!size_pixel',1.0422
        defsysv,'!pref',768.5
	defsysv,'!tanorm',[1,1,1,1] ;the instrument map = exposure map
	if n_elements(blow) eq 0 then blow=200
	if n_elements(bhigh) eq 0 then bhigh=7500
        end
'epic': begin
        defsysv,'!size_pixel',0.05
        defsysv,'!pref',25921.0d
	defsysv,'!tanorm',[1,1,1,1] ;the instrument map = exposure map
	if n_elements(blow) eq 0 then blow=200
	if n_elements(bhigh) eq 0 then bhigh=7500
        end
'hrc': begin
	defsysv,'!size_pixel',0.1318  
	defsysv,'!pref',16384.5d
	end
'acisi': begin
	defsysv,'!size_pixel',0.492 
	;changed on 7/22/2001 from 0.492
	;;changed back on 11/21/2002 from 0.49
	defsysv,'!pref',4096.5
	defsysv,'!tanorm',[0.00909,0.002388,0.003272,0.005165]
;	defsysv,'!tanorm',[0.004718,0.00183,0.003230,0.004036]
	if n_elements(blow) eq 0 then blow=500
	if n_elements(bhigh) eq 0 then bhigh=8000
	end
'acisi_low': begin
	defsysv,'!size_pixel',0.492 
	;changed on 7/22/2001 from 0.492
	;;changed back on 11/21/2002 from 0.49
	defsysv,'!pref',4096.5
	defsysv,'!tanorm',[0.003177,0.002815,0.0031845,0.010307217]
	if n_elements(blow) eq 0 then blow=1000
	if n_elements(bhigh) eq 0 then bhigh=8000
	end
'aciss': begin
	defsysv,'!size_pixel',0.492 
	defsysv,'!pref',4096.5
	defsysv,'!tanorm',[0.003333,0.00161,0.001916,0.00306]
	if n_elements(blow) eq 0 then blow=300
	if n_elements(bhigh) eq 0 then bhigh=7000
	end
endcase

defsysv,'!blow',blow
defsysv,'!bhigh',bhigh
if n_elements(spfile) eq 0 then spfile=!cdir+strtrim(instr,2)+'_po0.7.dat'
defsysv,'!spfile',spfile
print,'!instr, !blow, !bhigh, !spfile = ',!instr,!blow,!bhigh,' ',!spfile

return
end
