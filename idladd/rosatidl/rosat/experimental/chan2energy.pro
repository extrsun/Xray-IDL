;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       chan2energy
;
;*PURPOSE:
; Given an arbitray grouping of (Rosat PSPC) pha channels, calculate the
;   central energies of the channels
;
;*CALLING SEQUENCE:
;       chan2energy,ecen,edel,group=group
;
;*PARAMETERS:
; INPUTS:
;       group   - Array containing pha channel boundaries 
;                 Specifies the start and stop pha channels which are 
;                 included in each spectral bin
;                 If not set, !group is used as a default
;
; OUTPUTS:
;       ecen    - Bin central energies (keV)
;       edel    - Bin energy widths
;
;*PROCEDURE:
;
;*RESTRICTIONS:
;       So far this works only for the Rosat PSPC, and assumes the central
;       energies for the 256 pha channels as given by the standard
;       response file xanadu:[spectral.rsp.rosat]pspb_mar11.rsp
;
;*NOTES:
;
;*SUBROUTINES CALLED:
;
;*MODIFICATION HISTORY:
;    written 06-24-92 (GAR)
;-
;-------------------------------------------------------------------------------
pro chan2energy,ecen,edel,group=group
;
if (n_params(0) eq 0) then begin
   print,' CHAN2ENERGY, ECEN, EDEL, [group=group (def = !group)]'
   retall
end
if (n_elements(group) eq 0) then egroup = !group $    ;use !group as default
   else egroup = group
;
zaux = 'ZAUX'
get_lun,un
openr,un,getlog(ZAUX)+'pspcb_mar11.ebds'
dum = ''
pos = -1
while (pos lt 0) do begin          ;find number of pha channels
  readf,un,dum
  pos = strpos(dum,' No. of pha channels')
endwhile
try = gettok(dum,':')
nchan = fix(dum)
estart = fltarr(nchan)             ;start & stop energies of pha channels
estop = estart
;
pos = -1
while (pos lt 0) do begin          ;find last header line before entries
  readf,un,dum 
  pos = strpos(dum,'Lower and upper channel energies')
endwhile
;
ict = 0
while (not eof(un)) do begin
  readf,un,num,e1,e2
  estart(ict) = e1
  estop(ict) = e2
  ict = ict + 1
endwhile
close,un & free_lun,un
;
ebeg = estart(egroup(*,0))          ;start energies of grouped channels
eend = estop(egroup(*,1))           ;stop      "                 "
ecen = (ebeg + eend)/2.
edel = eend - ebeg
;
return
end          ;pro chan2energy
