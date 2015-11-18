pro plot_sbp,dist,flux,fluxe,device=device,back=back
if n_elements(device) eq 0 then device='x'
set_plot,device
 plot,dist,flux*1.e4,xtitle='Distance (arcmin)',ytitle='SBP (10!E-4 !Ncts/s arcmin!E2!N)',charsize=2.,position=[0.2,0.2,0.9,0.9],thick=2
errplot,dist,(flux-fluxe)*1.e4,(flux+fluxe)*1.e4
if n_elements(back) ne 0 then oplot,[fix(min(dist)),fix(max(dist)+1.)] $
	,[back,back]*1.e4
if device ne 'x' then begin
	device,/close
	set_plot,'x'
endif
end
