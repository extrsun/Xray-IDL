;;; Routine to grab energy vector from Event Browser and compute the strength
;;; of the main photopeak.

PRO counts_in_peak

z=GetProperty( 'energy', WDS_DATA=energy )

estimate_peak, energy, mean, sigma, report

;; Count # datapoints in +-3 sigma range around peak.
z = where( ((mean-3*sigma) LE energy) AND ( energy LE (mean+3*sigma)), $
	   counts_in_peak )

;; Look up other information from EB.
nom_energy      = fxpar( GetPrimaryKywds(), 'ENERGY' )
num_photons     = fxpar( GetPrimaryKywds(), 'PHOTONS' )
num_events      = CountXrayData()
num_good_events = CountXrayData(/WORKING)


;; Show results.
report=string(nom_energy,num_photons,num_events,num_good_events,counts_in_peak,mean,sigma, f='(F6.3,1x,I6,1x,I6,1x,I6,1x,I6,1x,F7.1,1x,F5.1)')

print, 'Energy PHOTONS, #events, #g02346 events, # within 3-sigma, mean, sigma'
print, report
tara_clipboard, POST=report

return
end
