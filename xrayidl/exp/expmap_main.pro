; program expmap_main
print,'This is the main program to obtain an energy dependent exposure map'
print,' for a pointed ROSAT PSPC observation. You can run the procedure'
print,' MAKE_EXPMAP directly once you have known what you have to do first.'
print,''
print,'Now make sure that you have done the following things correctly:'
print,'(You may want to define the system variables in your ROSAT IDL setup)'
print,''
print,'1. You have defined a system variable !seq_no containing the sequence'
print,'   number of the observation. Its value can be changed by using'
print,"!seq_no='rp123456';"
print,'2. You have defined a system variable !data_dir containing the default'
print,"   data directory, e.g., !data_dir='/home/casa/wqd/rosat/data/';"
print,'3. The default data directory contains the following files:'
print,'   (* represents a sequece number, e.g., rp123456)'
print,'     1. *.CAS (ASPECT INFO)'                        
print,'     2. *.EVR (EVENT RATE INFO)'
print,'     3. *.SO (SPLIT ORBIT INFO).'
print,'    4. *.FITS (SGTI TIME INTERVALS)'
print,''
stop,'If everything is ok, please type .c to continue'
print,''
;
; obtain VG time intervals
	print,"The VG code ranges are assumed to be the following:"
	seslow=0 & seshigh=3
	estlow=0 & esthigh=3
	print,"seslow,seshigh = ",seslow,seshigh
	print,"estlow,esthigh = ",estlow,esthigh
	stop,"If these values are ok, type .c to continue"
	sel_vg,seslow,seshigh,estlow,esthigh
        PRINT,''
	tfile=tfile=!seq_no+'_vg0101.dat'
	stop,'Select time interval using tfile = ',tfile, ' OK?'
	sel_int,listh,time,tfile=tfile

;       Select and read instrument map
band=0
        PRINT, "Enter a band range, by typying '1,7' for example"
        PRINT, '1 => CH 8-19'
        PRINT, '2 => CH 20-41'
        PRINT, '3 => CH 42-51'
        PRINT, '4 => CH 52-69'
        PRINT, '5 => CH 70-90'
        PRINT, '6 => CH 91-131'
        PRINT, '7 => CH 132-201'
	bandlow=0
	bandhigh=0
        READ, bandlow,bandhigh
        PRINT,''

	print,'Running make_expmap ...'
	print,''
tail=strtrim(seslow,2)+strtrim(seshigh,2)+strtrim(estlow,2)+strtrim(esthigh,2)
tfile=!seq_no+'_vg'+tail+'.dat'
make_expmap,bandlow,bandhigh,tfile=tfile
end