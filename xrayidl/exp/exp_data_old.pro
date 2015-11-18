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
;	modified to use new fits reader so that no ST files are created
;       and to use default filenames for the inputs. Aug 31 1992 (WQD)
;       A major bug is fixed. The output arrays ATTITUDE AND LIVTIME shoudl
;	not be converted to float arrays. Sept 3 1992 (WQD)

pro	exp_data,attitude,livtime,num

;	get initial information regarding file names.

	casfile=!data_dir+strtrim(!seq_no)+'.cas'
	evrfile=!data_dir+strtrim(!seq_no)+'.evr'
;	print,'Enter name of the output ATTITUDE file:'
;	read,atfile
;	atfile=strtrim(!seq_no)+'_evrout.data'
;	print,'Enter name of the output LIVTIME file:'
;	read,evrout
;	evrout=strtrim(!seq_no)+'_evrout.dat'
;        print,'Opening output files...'
;        openw,10,atfile
;        openw,30,evrout

;	create attitude file.

	print,'Creating attitude data file...'

	time=lonarr(100000)
	roll=lonarr(100000) 
	xoff=intarr(100000) 
        yoff=intarr(100000)

	i=0
	b=0
	while i lt num do begin
		print,'obi',i+1
		tab=readfits(casfile,h,ext=(i+1))
		s=size(tab)
		time(b:b+s(2)-1)=fits_get(h,tab,'IT1_CAS') ;lon 
		roll(b:b+s(2)-1)=fits_get(h,tab,'IRO_CAS') ;lon
		xoff(b:b+s(2)-1)=fits_get(h,tab,'IXN_CAS') ;int
		yoff(b:b+s(2)-1)=fits_get(h,tab,'IYN_CAS') ;int
		b=b+s(2)
		i=i+1
	end

	k=0L
	attitude=fltarr(5,b) ;lost precision in time array
	attitude=lonarr(5,b)
	attitude(0,*)=indgen(b)
	attitude(1,*)=time(0:b-1)
	attitude(2,*)=xoff(0:b-1)
	attitude(3,*)=yoff(0:b-1)
	attitude(4,*)=roll(0:b-1)
;	while k lt b do begin
;                printf,10,k+1,time(k),xoff(k),yoff(k),roll(k)
;		k=k+1
;        end

;	free array memory

	time=0
	xoff=0
	yoff=0
	roll=0
	
	
;	create livtime.

	print,'Creating livtime and eventrates data files...'

        time=lonarr(100000) 
	aer=lonarr(100000)
	aex=lonarr(100000)
	a1ll=lonarr(100000)

	i=0
	b=0
        while i lt num do begin
		print,'obi',i+1
		tab=readfits(evrfile,h,ext=i+1)
		s=size(tab)
                time(b:b+s(2)-1)=fits_get(h,tab,'ITI_EVR') ;long
		aer(b:b+s(2)-1)=fits_get(h,tab,'IAX_EVR')  ;long
		aex(b:b+s(2)-1)=fits_get(h,tab,'IQE_EVR')  ;long
		a1ll(b:b+s(2)-1)=fits_get(h,tab,'IA1_EVR') ;long
		b=b+s(2)
		i=i+1
	end
	
	k=0L
;	livtime=fltarr(5,b) ;cause errors in the conversion
	livtime=lonarr(5,b)
	livtime(0,*)=indgen(b)
	livtime(1,*)=time(0:b-1)
	livtime(2,*)=aex(0:b-1)
	livtime(3,*)=aer(0:b-1)
	livtime(4,*)=a1ll(0:b-1)
;	while k lt b do begin
;                printf,30,k+1,time(k),aex(k),aer(k),a1ll(k)
;                k=k+1
;        end


;	free array memory

	time=0
	aex=0
	aer=0
	a1ll=0

	
;	close files and end.

;	print,'Closing files - normal termination.'
;	close,10
;	close,30

	return
	end
