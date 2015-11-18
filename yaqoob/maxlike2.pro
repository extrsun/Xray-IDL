pro maxlike2,blur,psf,iter,fftp,reblur,init=init,niter=niter, $
        scale=scale,sigma=sigma,chimin=chimin,convar=convar,stall=stall, $
        display=display,region=region,header=header,outfile=outfile
;+
;                       maxlike2
;
; Computes the maximum likelyhood deconvolution.
;
; CALLING SEQUENCE:
;       maxlike2,blur,psf,iter,fftp,reblur,/init,niter=N,
;               scale=R,sigma=R,chimin=R,convar=R,stall=R,display=S,region=V,
;               header=A,outfile=S
;
; INPUTS:
;       blur - unrestored raw image
;       psf - point spread function (normalized to total = 1.0)
;
; INPUTS/OUTPUTS:
;       iter - restored image.  if undefined, set to a scalar, or keyword INIT
;               is specified it will be initialized with all pixels having the
;               average flux value of BLUR
;
; OPTIONAL INPUTS/OUTPUTS:
;       fftp - the fft of the psf.  If not defined or if INIT is specified then
;               then it will be an output parameter.
;       reblur - The convolution of iter with the PSF.  If not defined or
;               INIT is specified, it will be computed by the routine for the
;               input ITER.  Upon output it will the the convolution of the
;               output ITER with the PSF.
;
; OPTIONAL KEYWORD PARAMETER INPUTS:
;
;       /INIT - specifies that X and FFTP should be initialized. This should
;               only be used on the first call.
;       NITER=n - specifies the maximum number of iterations.
;               Default = 50
;       SCALE=r - specifies the number of Poisson counts per flux unit.
;               This is used by the Chi-sqaure termination test.  If set
;               to 0.0 then only white noise is assumed. (default = 1.0)
;       SIGMA=r - standard deviation of the white noise.  This is used by
;               the Chi-squared termination test. (default = 0.0)
;       CHIMIN=r - Chi-squared value required for termination.  Default=1.0
;               If set to 0.0 this test is disabled.
;       CONVAR=r - Co-variance of the white noise for the modifed Lucy algorithm
.
;               (see method).  (Default = 0.0)
;       STALL=r - If the chi-squared changes by less than STALL, the iterations
;               are assumed to be stalled and processing is terminated.
;               (default = 0.001).  If set to 0.0 this test is disabled.
;       DISPLAY - IDL command to display the ITER.
;               examples:
;                        DISPLAY = 'tvscl,alog(iter>0.01<100.0)'
;                        DISPLAY = 'contour,iter>0'
;               if not supplied then the iteration is not displayed
;       REGION - region of image to use for the chi-squared test given as
;               a 4 element vector [starting sample, ending sample, staring
;               line, ending line].  Default is to use the entire image.
;       HEADER - input/output FITS header.  If supplied it will be updated
;               with restoration history.
;       OUFILE - output FITS file.  If supplied it will be write result
;               of convolution to a FITS file with this name.
;
; METHOD:
;       Let:
;               b = unrestored raw input image
;               p = point spread function
;               x = previous iteration (each pixel is initialized to the
;                       average value in b for the 0th iteration)
;               C(q,t) = the convolution of q and t
;               pr = 180 degree rotation of p
;               cx = previous iteration convolved with the psf, C(x,p)
;       Each iteration is computed by:
;
;       CASE 1:  CONVAR = 0.0,  SCALE>0  (standard Lucy algorithm)
;
;               x = x * C( b/cx,pr)
;
;       CASE 2:  CONVAR>0, SCALE>0  (Modified Lucy alogorithm to include
;                                       white (Gaussian Noise) in addition to
;                                       Poisson noise
;
;               x = x * C( (b+CONVAR)/(cx+CONVAR) , pr)
;
;       CASE 3: SCALE = 0           White (Gaussian) noise only
;
;               x = x + C(b-cx,pr)
;
; EXAMPLES:
;
;       Straight Lucy alogorithm with 7.5 counts per unit flux (WFPC).
;       Default termination (maximum of 50 iterations)
;
;               MAXLIKE2,blur, psf, result, /init, scale=7.5
;
;       Do 80 iterations of the standard lucy algorithm with no
;       tests for termination.  Follow it with 40 more.
;
;               result = 0      ;set to scalar, (same as using /INIT)
;               MAXLIKE2,blur,psf,result,chimin=0,stall=0,niter=80
;               MAXLIKE2,blur,psf,result,chimin=0,stall=0,niter=40
;
;       Lucy modified to include Poisson noise and additive Gaussian
;       noise with a variance of 4.0.  Results are displayed on the
;       image display after each iteration.
;
;               MAXLIKE2,blur,psf,result,/init,convar=4.0,sigma=2.0, $
;                               display='tvscl,iter<100'
;
;-
;-------------------------------------------------------------------------------
        if n_params(0) lt 1 then begin
            print,'CALLING SEQUENCE: maxlike2,blur,psf,iter,[ fftp,reblur]'
            print,'Optional keyword inputs: init, niter, scale, sigma, chimin,'
            print,'                         convar, stall, display, region,'
            print,'                         header, outfile'
            return
        end
;
; set default parameters
;
        if n_elements(init) eq 0 then init = 0
        if n_elements(niter) eq 0 then niter = 50
        if n_elements(scale) eq 0 then scale = 1.0
        if n_elements(sigma) eq 0 then sigma = 0.0
        if n_elements(chimin) eq 0 then chimin = 1.0
        if n_elements(convar) eq 0 then convar = 0.0
        if n_elements(stall) eq 0 then stall = 0.001
        if n_elements(display) eq 0 then display = ''
;
; compute FFT of the PSF if required.
;
        if (init ne 0) or (n_elements(fftp) lt 2) then $
                        psf_fft,blur,psf,fftp
        cfftp = conj(fftp)                      ;conjugate of fftp
;
; initialize iter if required:
;
        s = size(blur) & ns=s(1) & nl=s(2) & npixels = ns*nl
        if (init ne 0) or (n_elements(iter) le 1) then begin
                ave = total(blur)/npixels
                iter = replicate(ave,ns,nl)
                reblur = iter
        end
;
; Compute REBLUR if required.
;
        if n_elements(reblur) le 1 then $
                reblur = float(fft( fft(iter,-1)*fftp, 1))*npixels
;
; Compute input chisq.
;
        compute_chisq,blur,reblur,sigma,scale,chisqin,region=region
        print,'INPUT CHISQ per degree of freedom = ',chisqin
;
; loop on iterations
;
        for i = 1,niter do begin
                if scale gt 0.0 then begin
;
; Poisson + white
;
                        if convar gt 0 then begin
                                ratio = (blur + convar)/(reblur + convar)
                            end else begin              ;straight LUCY
                                good = where(reblur gt 1e-10)
                                ratio = FLTARR(ns,nl)
                                ratio(good) = (blur(good)/reblur(good))
                        end
                        iter = iter* $
                               (float(fft(fft(ratio,-1)*cfftp,1))*npixels)>1e-10
                   end else begin
;
; white noise only
;
                        diff = blur-reblur
                        iter = iter+(float(fft(fft(diff,-1)*cfftp,1))*npixels) $
                                                                        >0.0
                end
;
; display results
;
                if display ne '' then istat = execute(display)
;
; recompute reblur and chisq
;
                reblur = float(fft( fft(iter,-1)*fftp, 1))*npixels
                compute_chisq,blur,reblur,sigma,scale,chisq,region=region
                print,'Iteration '+strtrim(i,2)+ $
                        ' CHISQ per degree of freedom = ',chisq
;
; should we terminate
;
                if chisq le chimin then begin
                        message = 'Chi-squared value achieved'
                        goto,done
                end

                if abs(chisq-chisqin) lt stall then begin
                        message='Change in chi-squared less then '+ $
                                strtrim(stall,2)
                        goto,done
                end
                chisqin = chisq
        end; for i
done:
        niter = i<niter
        if n_elements(message) lt 1 then $
                         message = 'Max. number of iterations = '+ $
                         strtrim(niter,2)+' reached'
        message = 'TERMINATION: '+message
        print,message
;
        if n_elements(header) gt 0 then begin
              sxaddhist,'MAXLIKE2: '+strtrim(niter,2)+' iterations',header
              sxaddhist,'Chi-squared = '+strtrim(chisq,2)+' for SCALE='+ $
                      strtrim(scale,2)+'  SIGMA='+strtrim(sigma,2),header
              sxaddhist,message,header
        end
;
;        print, 'FITS FILE: '+ outfile
;
        if n_elements(outfile) gt 0 then begin
             print, 'WRITE RESULT TO FITS FILE: '+ outfile
             result = long(iter*1000)
             mkhdr, header, result
             writefits, outfile, result, header
        end
        return
;
end
