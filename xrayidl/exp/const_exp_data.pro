;
;
;	PROGRAM : CONST_EXP_DATA
;	AUTHOR  : jeffrey a. mendenhall
;	DATE    : 4/23/92
;
;	This program creates appropriate ATTITUDE and LIVTIME
;	arrays called by CONST_EXP_MAPS when generating 
;	energy dependent exposure maps for ROSAT PSPC analysis.
;	Output ATTITUDE and LIVTIME data files are also created 
;	for FORTRAN users.
;
;

pro	const_exp_data,attitude,livtime

	casfile=''
	evrfile=''
	atfile=''
	evrout=''
	obi=strarr(10)
	obi=['1','2','3','4','5','6','7','8','9,','10']


;	get initial information regarding file names.

;	print,''
;	print,'Enter name of observation *.cas file:'
;	read,casfile
	casfile=!data_dir+strtrim(!seq_no)+'_cas.fits'
;	print,'Enter name of observation *evr file:'
;	read,evrfile
	evrfile=!data_dir+strtrim(!seq_no)+'_evr.fits'
	print,'Enter number of obis for this observation:'
	read,num
;	print,'Enter name of the output ATTITUDE file:'
;	read,atfile
	atfile=strtrim(!seq_no)+'_evrout.data'
;	print,'Enter name of the output LIVTIME file:'
;	read,evrout
	evrout=strtrim(!seq_no)+'_evrout.dat'
	print,''

;	read *cas and *evr files.

	print,'Reading *cas and *evr files...'
	dfitsrd,casfile,'temp_cas',NOPROMPT='noprompt'
	dfitsrd,evrfile,'temp_evr',NOPROMPT='noprompt'

;       open files.
 
        print,'Opening output files...'
        openw,10,atfile
        openw,30,evrout
 
;	create attitude file.

	print,'Creating attitude data file...'
	time=dblarr(100000)
	roll=dblarr(100000) 
	xoff=intarr(100000) 
        yoff=intarr(100000)
	i=0
	b=0
	while i lt num do begin
		print,'obi',i+1
		tbread,'temp_cas'+'_'+obi(i),h,tab
	 	temp=tbget(h,tab,1)
		s=size(temp)
		time(b:b+s(1)-1.)=tbget(h,tab,1)
		roll(b:b+s(1)-1.)=tbget(h,tab,3)
		xoff(b:b+s(1)-1.)=tbget(h,tab,4)
		yoff(b:b+s(1)-1.)=tbget(h,tab,5)
		b=b+s(1)
		i=i+1
	end
	
	k=0
	k=long(k)
	attitude=fltarr(5,b)
	attitude(0,*)=indgen(b)
	attitude(1,*)=time(0:b-1)
	attitude(2,*)=xoff(0:b-1)
	attitude(3,*)=yoff(0:b-1)
	attitude(4,*)=roll(0:b-1)
	while k lt b do begin
                printf,10,k+1,time(k),xoff(k),yoff(k),roll(k)
		k=k+1
        end

;	free array memory

	time=0
	xoff=0
	yoff=0
	roll=0
	
	
;	create livtime.

	print,'Creating livtime and eventrates data files...'
        time=dblarr(100000) 
	aer=intarr(100000)
	aex=fltarr(100000)
	a1ll=intarr(100000)
	i=0
	b=0
        while i lt num do begin
		print,'obi',i+1
                tbread,'temp_evr'+'_'+obi(i),h,tab
		temp=tbget(h,tab,1)
		s=size(temp)
                time(b:b+s(1)-1)=tbget(h,tab,1)
		aer(b:b+s(1)-1)=tbget(h,tab,7)
		aex(b:b+s(1)-1)=tbget(h,tab,3)
		a1ll(b:b+s(1)-1)=tbget(h,tab,4)
		b=b+s(1)
		i=i+1
	end
	
	k=0
	k=long(k)
	livtime=fltarr(5,b)
	livtime(0,*)=indgen(b)
	livtime(1,*)=time(0:b-1)
	livtime(2,*)=aex(0:b-1)
	livtime(3,*)=aer(0:b-1)
	livtime(4,*)=a1ll(0:b-1)
	while k lt b do begin
                printf,30,k+1,time(k),aex(k),aer(k),a1ll(k)
                k=k+1
        end


;	free array memory

	time=0
	aex=0
	aer=0
	a1ll=0

	
;	close files and end.

	print,'Closing files - normal termination.'
	close,10
	close,30

	return
	end
