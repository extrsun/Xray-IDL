;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;		make_continent
;*CLASS:
;
;*CATEGORY:
;
;*PURPOSE:
;	Create the CONTINENT.DAT file on any computer (ie. UNIX or DOS)
;
;*CALLING SEQUENCE:
;	make_continent, dummy
;
;*PARAMETERS:
;	dummy - none
;
;*FILES USED:
;	zaux:continent.txt as input.
;
;*NOTES:
;	Routine creates CONTINENT.DAT (which can be run via routine
;	continent.pro. Note: For GHRS, you place this file into ZAUX path.
;
;*MODIFICATION HISTORY:
;	21-may-1991	DJL   - how Don figured it out...I'll never know.
;-
;-------------------------------------------------------------------------------
pro make_continent, dummy

openr,iunit, getlog('zaux') + 'continent.txt',/get_lun
openw,ounit, 'continent.dat',/get_lun
n=0
a=fltarr(4)
while (not (eof(iunit))) do begin
	readf,iunit,n,a
	b=fltarr(n/2) & c=fltarr(n/2)
	readf,iunit,b
	readf,iunit,c
	forwrt,ounit,n,a
	forwrt,ounit,b,c
end

free_lun,iunit
free_lun,ounit
return
end
