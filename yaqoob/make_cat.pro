pro make_cat,dirpath,flist,INSTR=instr,DATAMODE=datamode,BIT_RATE=bit_rate
; Creates list of selected ASCA files
; Inputs: flist (output filename),dirpath (directory of ASCA data)
;	  Selection criteria example:
;	INSTR='S1',DATAMODE='B',BIT_RATE='M'
; Outputs: file containing list
; Bugs: Fails with large directories (ls in unix fails)
; Currently requires bit rate when specifying datamode
; Currently excludes HK files

	n_p = N_PARAMS(0)
;	IF n_p lt 2 THEN BEGIN
	  PRINT,'make_cat,path,flist(,INSTR=instr,DATAMODE=datamode,BIT_RATE=bit_rate)'
;	  RETURN
;	ENDIF
	if n_elements(pathname) eq 0 then begin
	 dirpath=' '
	 read,'Input directory name',dirpath
	endif
	if n_elements(flist) eq 0 then begin
	 flist='dummy.txt'
	 flist=' '
	 read,'Filename for output ',flist
	endif
	if n_elements(instr) eq 0 then begin
	 instr=' '
	 read,'Input instrument name ',instr
	endif
	if n_elements(datamode) eq 0 then begin
	 datamode=' '
	 read,' Input datamode ',datamode
	endif
	if n_elements(bit_rate) eq 0 then begin
	 bit_rate = ' '
	 read,' Input bit rate ',bit_rate
	endif
	fspec = 'ft*'
	IF N_ELEMENTS(INSTR) THEN BEGIN
	  instr = STRUPCASE(instr)
	  instr = STRMID(instr,0,1)+STRMID(instr,STRLEN(instr)-1,1)
	  fspec = fspec+instr+'*'
	ENDIF
	IF N_ELEMENTS(DATAMODE) and not N_ELEMENTS(BIT_RATE) THEN BEGIN
	  PRINT,'You must specify a bit rate when specifying datamode'
	  RETURN
	ENDIF 
	IF N_ELEMENTS(DATAMODE) THEN IF STRUPCASE(STRMID(datamode,0,1)) eq 'B' $
	  THEN fspec = fspec+'2' ELSE fspec = fspec+'1'
	IF N_ELEMENTS(BIT_RATE) THEN fspec = fspec + $
	  STRUPCASE(STRMID(bit_rate,0,1))+'*'
	fspec = fspec + '.fits'
	PRINT,'Getting files '+dirpath+fspec
	lst = FINDFILE(dirpath+fspec,COUNT=n)
; Remove HK files
	IF n gt 0 THEN BEGIN
	  a = INDGEN(n)
 	  FOR i=0,n-1 DO IF STRPOS(lst(i),'HK') ge 0 THEN a(i) = -1
	  IF TOTAL(a) gt -n THEN lst = lst(WHERE(a ge 0,n)) ELSE n=0
	ENDIF
;	print,'Found ',n,' files'
	IF n gt 0 THEN BEGIN
	  OPENW,lun,flist,/GET_LUN
	  FOR i=0,n-1 DO begin 
	   printf,lun,lst(i)
;	   print,lst(i)
	  endfor
	  FREE_LUN,lun
	ENDIF
	RETURN
	END
