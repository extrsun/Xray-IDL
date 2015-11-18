;; $Id: estimate_peak.pro 1201 2000-09-08 12:29:31Z patb $
;; Routine to estimate the mean and sigma of a dataset whose distribution is
;; approximately gaussian.  To avoid contamination by non-gaussian wings, 
;; we (iteratively) fit a gaussian only to the central +- 1sigma core.

PRO estimate_peak, data, mean, sigma, report

;; Estimate mean and sigma of main peak.
meanclip, data, mean, sigma, CONVERGE_NUM=0.005
print, mean, sigma

;; Perform fit over range +-1 sigma around peak.
index = where( ((mean-sigma) LE data) AND ( data LE (mean+sigma)), count )
data_ptr = ptr_new( data[index] )

pars = {gauss: {amplitude:float(count), mean:mean, sigma:sigma, $
		freeze_amp:0, freeze_mean:0, freeze_sigma:0} }

lkh = ml_gaussian( PARAMETERS=pars, DATA_PTR=data_ptr, $
			  X0=0, DELTA_X=1, DISTRIBUTION=indgen(10),$
			  REPORT=report )
;print, report

;; Repeat, using revised estimate of mean and sigma.
mean  = pars.gauss.mean
sigma = pars.gauss.sigma

index = where( ((mean-sigma) LE data) AND ( data LE (mean+sigma)), count )
*data_ptr = data[index]

lkh = ml_gaussian( PARAMETERS=pars, DATA_PTR=data_ptr, $
			  X0=0, DELTA_X=1, DISTRIBUTION=indgen(10),$
			  REPORT=report )
;print, report

return
end
