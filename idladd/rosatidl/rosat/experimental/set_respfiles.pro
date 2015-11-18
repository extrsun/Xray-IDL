;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;       set_respfiles
;
;*PURPOSE:
; A procedure to define a system variable containing the default response
;    matrices to be used in make_pha
;
;*CALLING SEQUENCE:
;       set_respfiles,respfiles,nresp
;
;*PARAMETERS:
; INPUTS:
;   none
;
; OUTPUTS:
;   RESPFILES   a string variable containing the names of the default 
;               response matrices appropriate for the particular operating
;               system
;   NRESP       the number of response matrices
;
;*RESTRICTIONS:
;   RESPFILES will contain the names of the response matrix files for either
;     the Vax or Unix clusters at the GSFC HEASARC. These are:
;
;    Vax cluster:
;         xanadu:[cal.rosat.pspc]pspcb_mar11.rsp
;         xanadu:[cal.rosat.rsp]pspcc_mar11.rsp
;         $scratch:[turner]pspcb_93jan12.rsp      (this is a test matrix)
;         xanadu:[cal.rosat.pspc.old_crap]pspcb_35_300.rsp (fudged)
;         xanadu:[cal.rosat.pspc.old_crap]pspcb_35_1_300.rsp (fudged more)
;         xanadu:[old_spectral.rsp.rosat]pspc_mar03.rsp
;         xanadu:[old_spectral.rsp.rosat]pspcb.rsp
;         xanadu:[old_spectral.rsp.rosat]pspc_bor.rsp
;
;    Unix cluster:
;         /xanadu/spectral/rosat/pspc/pspcb_mar11.rsp
;         /xanadu/spectral/rosat/pspc/pspcc_mar11.rsp
;         /home/ros10/turner/pspcb_93jan12.rsp
;         /xanadu/spectral/rosat/pspc/pspcb_35_300.rsp
;         /xanadu/spectral/rosat/pspc/pspcb_35_1_300.rsp
;         /xanadu/spectral/rosat/pspc/pspc_mar03.rsp
;         /xanadu/spectral/rosat/pspc/gh.rsp
;
;  If you are running these programs at a different institution, then you
;     may wish to change these options
;
;*SUBROUTINES CALLED:
;
;*MODIFICATION HISTORY:
;    written 19 Jan 1993 by G.A. Reichert
;-
;-------------------------------------------------------------------------------
pro set_respfiles,respfiles,nresp
;
if (n_params(0) eq 0) then begin
   print,'SET_RESPFILES, RESPFILES, NRESP'
   retall
end
;
if (!version.os eq 'vms') then $
   respfiles = ['xanadu:[cal.rosat.pspc]pspcb_mar11.rsp',$
                'xanadu:[cal.rosat.rsp]pspcc_mar11.rsp',$
                '$scratch:[turner]pspcb_93jan12.rsp',$
                'xanadu:[cal.rosat.pspc.old_crap]pspcb_35_300.rsp',$
                'xanadu:[cal.rosat.pspc.old_crap]pspcb_35_1_300.rsp',$
                'xanadu:[old_spectral.rsp.rosat]pspc_mar03.rsp',$
                'xanadu:[old_spectral.rsp.rosat]pspcb.rsp',$
                'xanadu:[old_spectral.rsp.rosat]pspc_bor.rsp'] else $
   respfiles = ['/xanadu/spectral/rosat/pspc/pspcb_mar11.rsp',$
                '/xanadu/spectral/rosat/pspc/pspcc_mar11.rsp',$
                '/home/ros10/turner/pspcb_93jan12.rsp',$
                '/xanadu/spectral/rosat/pspc/pspcb_35_300.rsp',$
                '/xanadu/spectral/rosat/pspc/pspcb_35_1_300.rsp',$
                '/xanadu/spectral/rosat/pspc/pspc_mar03.rsp',$
                '/xanadu/spectral/rosat/pspc/gh.rsp'] 
nresp = n_elements(respfiles)
;
return
end                ;pro set_respfiles
