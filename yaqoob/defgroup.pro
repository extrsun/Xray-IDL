;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       defgroup
;
;*PURPOSE:
; A procedure to define channel groupings for make_spec and make_pha
;
;*CALLING SEQUENCE:
;       defgroup,group,ngroup,option,rate=rate,mincts=mincts
;
;*PARAMETERS:
; INPUTS:
;   OPTION   String variable which tells program how to define the groups
;            If not defined, user will be prompted.
;            Allowed options are:
;               'T'     Enter channel bounds by typing
;               'C'                          using cursor
;               'F'     Use the full channel resolution
;               'D'     Use the default channel groupings
;               'R'     The count rates in each group will be above
;                       a specified minimum (keyword RATE must be defined)
;
;*OPTIONAL INPUTS:
;   RATE     Vector containing a counting rate spectrum (counts per channel)
;   MINCTS   Minimum desired counts per output channel group (ignored unless
;            RATE is defined)
;
; OUTPUTS:
;   GROUP    2d array giving beginning and ending channel boundaries
;            (default = Rosat 34 channel grouping)
;   NGROUP   Number of grouped channels
;
;*NOTES:
;   Program will call chan2energy to give the approximate energies of the
;      pha channels at full resolution. The user will then be prompted to
;      enter the lower and upper bounds for each grouping. Type q or Q
;      to exit the program.
;   
;   The user has the option of typing in the channel bounds, or of setting 
;      these in sequence using the cursor.
;   If keyword RATE is defined, then the user also has the option of 
;      specifying a minimum number of counts per channel
;
;*RESTRICTIONS:
;   Assumes Rosat PSPC (for now).
;
;*SUBROUTINES CALLED:
;   CHAN2ENERGY
;
;*MODIFICATION HISTORY:
;    written 07 Feb 1993 by G.A. Reichert
;-
;-------------------------------------------------------------------------------
pro defgroup,group,ngroup,option,rate=rate,mincts=mincts
;
if (n_params(0) eq 0) then begin
   print,'DEFGROUP, GROUP, NGROUP, option (nullstr), rate=rate, mincts=mincts'
   retall
endif
if (n_elements(mincts) eq 0) then mincts = 0     ;start by assuming zero
if (n_elements(option) eq 0) then option = ''    ;user will be prompted
;
; First define approximate energies for full channel resolution
;
nfull = 256
grfull = intarr(nfull,2)
grfull(0,0) = indgen(nfull)
grfull(0,1) = grfull(*,0)
chan2energy,ecen,edel,group=grfull
;
; Ask user to select option
;
if (option eq '') then begin
  print,'    '
  print,' You may define the channel groupings by: '
  print,'     Typing.                                           Enter T.'
  print,'     Using the cursor.                                 Enter C.'
  print,'     Using the full channel resolution.                Enter F.'
  print,'     Using the default channel groupings.              Enter D.'
  if (n_elements(rate) ne 0) then begin
    print,'     Requiring a minimum number of counts per channel. Enter R.'
    print,'        The current assumed minimum = ',mincts
  endif
  print,'    '
  read,' Enter option  > ',option
endif
optuc = strupcase(option)
;
case 1 of
(optuc eq 'F'): begin
   ngroup = nfull
   group = grfull + 1         ;channel bounds should start from 1
end              ;using full channel resolution.
;
(optuc eq 'D'): begin
   group = !group
   sz = size(group)
   ngroup = sz(1)
end              ;using default channel groupings
;
(optuc eq 'T'): begin
;
; Now print them out so that the user can see them. Just show the channels
;   when the nominal energy changes by tol
;
   tol = 0.05
   isel=[0]                         ;always print the first channel
   elower = ecen(0)
   for nn=1,nfull-2 do begin
     diff = ecen(nn) - elower
     if (diff ge tol) then begin
       isel = [isel,nn]             ;print this channel as well
       elower = ecen(nn)
     endif
   endfor
   isel = [isel,nfull-1]            ;always print the last channel
   nsel = n_elements(isel)
;
   print,'    '
   print,' Here are the approximate central energies to help you.'
   print,'    '
   for nn=0,nsel-1,12 do begin
     n2 = (nn+11) < (nsel-1)
     print,form='$(12i6)',isel(nn:n2)+1
     print,form='$(12f6.3)',ecen(isel(nn:n2))
     print,'   '
   endfor
;
; Now ask the user to type in the channel bounds. User only needs to
;   type in the lower channel number for each group in turn, and then the
;   upper bound for the last group.
;
   lower = [-1]
   upper = [-1]
   print,' Now define the desired channel groupings (type Q to quit): '
   ans = ''
   read,' Enter channel lower bound of first group > ',ans
   ans = strupcase(ans)
   ngroup = 0
   if (ans ne 'Q') then num = fix(ans)
   while ( (ans ne 'Q') and (ngroup lt nfull) ) do begin
     lower = [lower,num]
     read,' Enter channel lower bound of next group > ',ans
     ans = strupcase(ans)
     if (ans ne 'Q') then begin
       num = fix(ans)
       upper = [upper,num-1]
       ngroup = ngroup + 1
       print,' Group ',ngroup,' : ',lower(ngroup),upper(ngroup),$
             ecen(lower(ngroup)-1)-edel(lower(ngroup)-1)/2.,$
             ecen(upper(ngroup)-1)+edel(upper(ngroup)-1)/2.,$
             form='$(a7,i6,a3,2i6,6x,2f6.3)'
     endif                
   endwhile
   if (ngroup gt 0) then begin
     read,' Enter channel upper bound of last group > ',ans
     upper = [upper,fix(ans)<nfull]
     ngroup = ngroup + 1
     print,' Group ',ngroup,' : ',lower(ngroup),upper(ngroup),$
           ecen(lower(ngroup)-1)-edel(lower(ngroup)-1)/2.,$
           ecen(upper(ngroup)-1)+edel(upper(ngroup)-1)/2.,$
           form='$(a7,i6,a3,2i6,6x,2f6.3)'
;
     group = intarr(ngroup,2)
     group(0,0) = lower(1:ngroup)
     group(0,1) = upper(1:ngroup)
   endif
end              ;defining channel boundaries by typing
;
(optuc eq 'C'): begin
;
;first, plot the energy channel boundaries at full resolution
;
   plot,grfull(0,0:1),ecen(0)+[0,0],$
        xran=[1,nfull],yran=[min(ecen-edel/2.),max(ecen+edel/2.)],$
        xtit='Channel Number',ytit='Channel Energy'
   for nn=1,nfull-1 do oplot,grfull(nn,0:1),ecen(nn)+[0,0]
   for nn=0,nfull-1 do oplot,grfull(nn,0:1),ecen(nn)-edel(nn)/2.+[0,0]
   for nn=0,nfull-1 do oplot,grfull(nn,0:1),ecen(nn)+edel(nn)/2.+[0,0]
;
   devname = !d.name                          
   cond1 = 120                              ;X (for tektronix)
   cond2 = 88                               ;x (for tektronix)
   if (devname eq 'X') then begin
     if (!version.os eq 'vms') then cond1 = 4 $    ;right cursor button
     else cond1 = 2                          ;if Sun, use middle button
   endif
;
   lower = [-1]
   upper = [-1]
   print,' Select the lower channel bound of each group in turn'
   if (!version.os eq 'vms') then $
      print,' Hit X (Tektronix) or right cursor button (X) to stop' $
      else print,' Hit middle cursor button to stop' 
   print,'  '
   cursor,csel,esel,1
   err = !err
   ngroup = 0
;
   num = nint(csel)
   while ( (err ne cond1) and (err ne cond2) ) do begin    ;not X or x
     lower = [lower,num]
     cursor,csel,esel,1
     err = !err
     if ( (err ne cond1) and (err ne cond2) ) then begin    ;not X or x
       num = nint(csel)
       upper = [upper,num-1]
       ngroup = ngroup + 1
     endif                
   endwhile
   if (ngroup gt 0) then begin
     print,' Select the upper channel bound of last group'
     cursor,csel,esel,1
     err = !err
     upper = [upper,nint(csel)<nfull]
     ngroup = ngroup + 1
;
     for nn=1,ngroup do begin
       xpl = [lower(nn),upper(nn),upper(nn),lower(nn),lower(nn)]
       ipl = [lower(nn)-1,lower(nn)-1,upper(nn)-1,upper(nn)-1,lower(nn)-1]
       ypl = ecen(ipl) +edel(ipl)*[-1.,-1.,1.,1.,-1.]/2.
       oplot,xpl,ypl
     endfor
;
     group = intarr(ngroup,2)
     group(0,0) = lower(1:ngroup)
     group(0,1) = upper(1:ngroup)
   endif
end                ;defining channel boundaries with cursor
;
(optuc eq 'R'): begin
   nrate = n_elements(rate)
   if (nrate gt 0) then begin
     if (mincts eq 0) then $
        read,' Enter desired minimum number of counts per group > ',mincts
;
     lower = [-1]
     upper = [-1]
     totcts = 0
     ngroup = 0
     ncount = 0
     while (ncount lt nrate) do begin
       lower = [lower,ncount+1]
       while ( (totcts lt mincts) and (ncount lt nrate) ) do begin
         totcts = totcts + rate(ncount)
         ncount = ncount + 1
       endwhile
       upper = [upper,ncount]
       ngroup = ngroup + 1
;
       print,' Group ',ngroup,' : ',lower(ngroup),upper(ngroup),$
             totcts,form='$(a7,i6,a3,2i6,6x,i6)'
;
       totcts = 0
     endwhile
     if (ngroup gt 0) then begin
       group = intarr(ngroup,2)
       group(0,0) = lower(1:ngroup)
       group(0,1) = upper(1:ngroup)
     endif
   endif else print,' Keyword RATE must be defined. Try again.'
end                ;requiring a minimum number of counts per group
;
else: print,'Option ',optuc,' not allowed. Try again.'
endcase
;
return
end                ;pro defgroup
