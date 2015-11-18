Function DSS_Image, RA, Declination, Header = Header, File = File, $
	Dimensions = Dimensions, J2000 = J2000, B1950 = B1950, Help = Help
;+
;Module:
;	DSS_Image
;Author:
;	Jim Pendleton, GRO/OSSE NU
;	5/27/94
;
;Purpose:
; 	Given a field center and dimensions in arcminutes, read an image from
;	a STScI Digital Sky Survey CD, optionally saving it to a FITS file.
;Input variables:
;	RA is the Right Ascension in decimal hours of the center of the
;		image to be generated.
;	Declination is the image center's declination, in degrees.
;
;	The default for these parameters is Epoch 2000 coordinates.  This can
;	modified with the /B1950 keyword parameter.
;Optional input parameters:
;	Dimensions is an array of two elements giving the extents of the image
;		to be created, in units of arcminutes.  The default is to
;		create an image 30 arcminutes to a side, i.e.,
;		Dimensions = [30., 30.].
;	File is a filename in which the FITS version of the image is to be
;		stored, along with its header information.
;Optional output parameters:
;	Header is a string array containing the FITS header information
;		associated with the image.
;Optional input keywords:
;	/J2000 indicates that RA and Declination are in Epoch 2000 coordinates.
;		This is the default.
;	/B1950 indicates that RA and Declination are in Epoch 1950 coordinates.
;	These two parameters are mutually exclusive.
;Return value:
;	The return value of the function is a two-dimensional image array
;	centered on or near the input RA and Declination coordinates.
;Restrictions:
;	This routine will work under both VMS and Unix, though apparently
;	one needs superuser privileges on Unix pieces of junk in order to
;	mount the CD.
;
;	You need to have the UIT IDL routines in your path somewhere, for
;	the READFITS function.
;
;	You must have write access to the directory from which you run this
;	function.
;
;	The symbol or alias "getimage" is expected to be defined and point to
;	either GETIMAGE.COM (on VMS) or the script file "getimage" on Unix
;	systems.
;Side effects:
;	A temporary file is created in SYS$SCRATCH (VMS) or /tmp (Unix)
;	to store the input coordinates, etc., for the GETIMAGE exectuable.
;
;	A temporary FITS file is created if image information is found on
;	the CD somewhere.  If the FILE parameter is not specified, this
;	file is deleted, otherwise it's renamed to the value of FILE.
;Calling sequence
;	Image = DSS_Image(RA, Declination [, Header = Header] $
;		[, File = File] [, Dimensions = Dimensions] [, /J2000] $
;		[, /B1950] [/Help])
;-
On_Error, 2
If (Keyword_Set(Help)) then Begin
	Doc_Library, 'DSS_Image'
	Return, 0
EndIf
;
; Set up keyword parameters for those that were and were not passed.
;
If (Keyword_Set(B1950)) then Begin
	If (Keyword_Set(J2000)) then Begin
		Message, '/B1950 and /J2000 are mutually exclusive', /Traceback
	EndIf
	AddQualifier = 'b'
EndIf Else Begin
	AddQualifier = 'j'
EndElse
If (N_elements(Dimensions) ne 2) then Begin
	Dimensions = [30., 30.]
EndIf
;
; Set up a temporary file root name.
;
IFile = 'tmp1'
;
; Check if the DSS_DEV environment variable has already been set.  We expect
; this on my version on VMS.  This allows us to prompt the user to mount
; a specific device.
;
DSS_Dev = GetEnv('DSS_DEV')
;
; Set up a temporary file name to contain the input line to the Getimage
; executable image.
;
If (!Version.OS eq 'vms') then Begin
	TmpFileName = 'Sys$Scratch:DSSITmpScript.TMP'
EndIf Else Begin
	Spawn, 'ls -F ~ | grep cdrom/', SpawnOut
	FoundIt = Where(SpawnOut eq 'cdrom/', NFoundIt)
	If (NFoundIt eq 0) then Begin
		Message, 'A ~/cdrom directory must be present as a mount ' + $
			'point for the DSS CD.', /Traceback
	EndIf
	TmpFileName = '/tmp/dssitmpscript.tmp'
EndElse
Command = 'getimage -cez' + AddQualifier + ' -i '+ TmpFileName
OpenW, TmpFile, TmpFileName, /Get_LUN
;
; Convert the decimal hours and degrees to the DD/HH MM SS.SS format
; expected by the GetImage executable.
;
RHH = Fix(RA)
RMM = Fix((RA - RHH)*60.)
RSec = (RA - RHH - RMM/60.)*60.
DD = Fix(Declination)
DMM = Fix((Abs(Declination) - Abs(DD))*60.)
DSec = (Abs(Declination) - Abs(DD) - DMM/60.)*60.
;
; Write the input line to the temporary file and close the file.
;
PrintF, TmpFile, String(IFile, Format = '(A4)') + ' ' + $
	String(RHH, Format = '(I2.2)') + ' ' + $
	String(RMM, Format = '(I2.2)') + ' ' + $
	StrTrim(String(RSec), 2) + ' ' + $
	StrTrim(DD, 2) + ' ' + $
	StrTrim(DMM, 2) + ' ' + $
	StrTrim(String(DSec), 2) + ' ' + $
	String(Dimensions(0)) + ' ' + String(Dimensions(1))
Free_LUN, TmpFile
;
; We might be successful on our first attempt to read the disk, so 
; we use a Repeat-Until.
;
Bad = -1
Repeat Begin
;
; Send off the GetImage execution.
;
	Spawn, Command, Output
;
; If the output from the command contains a "could not locate" string,
; then apparently it didn't find the correct disk in the CD reader.
;
	Bad = Max(Where(StrPos(Output, 'Could not locate') ne -1))
	If (Bad(0) ne -1) then Begin
;
; Extract the number of the disk the program was looking for.
;
		Disk = StrMid(Output(Bad), StrPos(Output(Bad), 'disc') + 5, 2)
		If (StrPos(Disk, '!') ne -1) then Begin
			Disk = StrMid(Disk, 0, StrLen(Disk) - 1)
		EndIf
;
; Tell the user to mount the appropriate disk in the CD reader.
;
		Print, 'Please mount Digitied Sky Survey disk # ' + Disk + $
			' in CD drive ' + DSS_Dev + '.'
		Print, "After inserting the disk, type the letter 'm' " + $
			'followed by <Return>.'
;
; Execute the GetImage program again, this time minus the "z" qualifier used
; earlier.
;
		Command = 'getimage -ce' + AddQualifier + ' -i ' + TmpFileName
	EndIf
EndRep Until Bad(0) eq -1
;
; Hey, it looks like something may have worked, so we nuke the temporary
; script file.
;
If (!Version.OS eq 'vms') then Begin
	Spawn, 'Delete/NoLog/NoConfirm ' + TmpFileName + ';*'
EndIf Else Begin
	Spawn, 'rm ' + TmpFileName
EndElse
;
; Check for a "created" string in the output from the command.
;
Okay = Max(Where(StrPos(Output, ' created.') ne -1))
If (Okay eq -1) then Begin
;
; Oh well.  It didn't work afer all.  Print out the output from the
; GetImage program and let the user figure out the problem.
;
	Print, Output
	Message, 'Error of some sort...take a look', /Traceback
EndIf
;
; Now we want to extract the FITS file name created by the routine.
; The Unix and VMS differ slightly because of lack of carriage returns
; in the Unix version.
;
LastLine = Output(Okay)
If (!Version.OS ne 'vms') then Begin
	LastLine = StrMid(LastLine, StrPos(LastLine, '...') + 3, $
		StrLen(LastLine))
EndIf
FName = StrMid(LastLine, 0, StrPos(LastLine, ' '))
;
; Call the UIT IDL routine READFITS to get the image into memory, along with
; the FITS header.
;
Image = ReadFits(FName, Header)
;
; If the user specified an output file, rename the temporary FITS file to
; this file name, otherwise delete the FITS file.
;
If (N_elements(File) ne 0) then Begin
	If (!Version.OS eq 'vms') then Begin
		Spawn, 'Rename/NoLog/NoConfirm ' + FName + ' ' + File
	EndIf Else Begin
		Spawn, 'mv ' + FName + ' ' + File
	EndElse
EndIf Else Begin
	If (!Version.OS eq 'vms') then Begin
		Spawn, 'Delete/NoLog/NoConfirm ' + FName + ';*', /NoWait
	EndIf Else Begin
		Spawn, 'rm ' + FName
	EndElse
EndElse
;If (!Version.OS eq 'vms') then Begin
;	Print, 'Please reset the CD reader by first logging off the current process, then'
;	Print, 'from a process with privs, executing the command : '
;	Print, '	$ mcr dua1:[pendleton.sysmgr]clrref dennis$dkb100'
;EndIf
Return, Image
End
