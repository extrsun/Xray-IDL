;=============================================================================
; $Id: match_xy.pro 4456 2013-04-11 13:34:57Z psb6 $
; Catalog Matching Program 
; Patrick Broos, November 1993, December 2005

; The input structure array cat_slave must contain the following tags/fields:
;   ID: (optional) a unique long integer identifier
;   X,Y: independent position coordinates.  These do not have to be Cartesian.
;   X_ERR, Y_ERR: standard deviations for X & Y
; A known offset error in cat_slave can be specified via XSHIFT_SLAVE,YSHIFT_SLAVE.
;
;
; The input/output data structure match_state holds three types of information. 
; (1) a master catalog consisting of at least these fields:
;   ID:  long integer identifier
;   X,Y: independent position coordinates.  These do not have to be Cartesian.
;   X_ERR, Y_ERR: standard deviations for X & Y
;
; The match_state is initialized and a master catalog is established via this call:
;       match_xy, match_state, cat_master, cat_name, /INIT
;
; (2) a copy of all the catalogs we've processed.  Note that any XSHIFT_SLAVE,YSHIFT_SLAVE values that are passed in are ADDED to the X & Y columns of these catalogs.
;
; (3) match information consisting of a Primary Match (PM) structure array and a Secondary Match (SM) structure array.  
;The PM contains an entry for each source in (1):
;   IDm, IDs: a pair of matching ID tags from the master & slave catalogs
;   deltaX, deltaY: offsets between matching entries (master - slave)
;   rank: see discussion below
;   type: {0: isolated source, 1: successful Primary Match, 2: Secondary match, 3: failed Primary match, }
;
; Every entry "a1" in catalog 1 has a unique Primary Match: the entry "a2" in catalog 2 that has the highest match rank.  The Primary Match <a1,a2> is "unsuccessful" if there exists another Primary Match involving either a1 or a2 ,<a1,b2> or <b2,a2>, which has a higher rank.  In other words, Primary Matches are geedy -- once <a1,a2> has been accepted as a successful match then neither a1 nor a2 can participate in another successful primary match.  We're trying to build a self-consistent hypothesis for the correspondence between the two catalogs.  Thus, if we hypothesize that a1 & a2 are the same object then it makes no sense to also hypothesize that a1 & b2 are the same object, even if a1 is the closest object to b2.  Rather it makes more sense to hypothesize that b2 has no match.;
;
; The SM contains a variable number of entries recording secondary matches between (1) and the slave catalog:


; The significance_threshold parameter specifies the formal "significance" required for a match, i.e. our decision criterion is to REJECT match candidates that fall in a region of parameter space containing an area (probability) of (1-significance_threshold parameter).
; The largest significance_threshold that can be handled seems to be 
;    significance_threshold=(1D - 1D-16)
; which corresponds to z_acceptable = 8.

; If the output parameter UNION_CAT is supplied then the master catalog is extended to include anything in cat_slave that is not matched.  For matched sources the position is taken from cat_slave if its more accurate.

; Some references on the catalog matching problem:
; http://sundog.stsci.edu/first/APM/HTML/node5.html


; By default, the primary match is chosen as the maximum likelihood counterpart, considering only positions.
; An alternate expression to chose among all the plausible matches can be supplied via MATCH_RANK_EXPRESSION, assuming that:
;   * the plausible slave catalog entries are stored in a variable named "plausible_cat_slave"
;   * the master catalog entry is stored in cat_master[ii]
; 
; For example, to choose the brightest slave candidate use something like:
;   MATCH_RANK_EXPRESSION='-plausible_cat_slave.KMAG'
; For example, to choose the slave candidate with the closest K magnitude, use something like:
;   MATCH_RANK_EXPRESSION='-abs(plausible_cat_slave.KMAG - cat_master[ii].KMAG)'


;=============================================================================
PRO match_xy_types, isolated_type, successful_primary_type, secondary_type, failed_primary_type, init_type
isolated_type           = 0
failed_primary_type     = 1
successful_primary_type = 2
secondary_type          = 3
init_type               = 4
return
end


PRO match_xy, match_state, cat_slave_p, cat_name, significance_threshold, $
	            XSHIFT_SLAVE=xshift_slave, YSHIFT_SLAVE=yshift_slave, $
              INIT=init, QUIET=quiet, $
              MATCH_RANK_EXPRESSION=match_rank_expression, $
              UNION_CAT=union_cat, UNION_REG=union_reg, USE_SLAVE_AS_TEMPLATE=use_slave_as_template, $
              PREFER_SLAVE=prefer_slave, PREFER_MASTER=prefer_master, $
              NUM_SUCCESSFUL_PRIMARY=num_successful_primary, NUM_WITH_SECONDARY=num_with_secondary, _EXTRA=extra_params
            
COMMON match_xy, idex, idey, idx, idy, idxe, idye, idsx, idsy, idsxy, idr1, idr2, idxdx, idydy

creator_string = "match_xy, version " +strmid("$Rev:: 4456  $",7,5) +strmid("$Date: 2013-04-11 09:34:57 -0400 (Thu, 11 Apr 2013) $", 6, 11)
if ~keyword_set(quiet) then print, creator_string, F='(%"\n\n%s")'
if ~keyword_set(quiet) then print, systime()


print, cat_name, F="(%'\n========================\nProcessing catalog %s:')"


match_xy_types, isolated_type, successful_primary_type, secondary_type, failed_primary_type, init_type

if (n_elements(xshift_slave) EQ 1) then xshift_slave=xshift_slave[0] else xshift_slave=0.
if (n_elements(yshift_slave) EQ 1) then yshift_slave=yshift_slave[0] else yshift_slave=0.

Nslave     = n_elements(cat_slave_p)

f_nan    = !VALUES.F_NAN

;; Make sure the catalog has an ID column.
if tag_exist(/TOP_LEVEL, cat_slave_p,'ID') then begin
  ; Check the existing ID column for problems.
  id_type = size(cat_slave_p.ID, /TNAME) 
  if (id_type NE 'INT') and (id_type NE 'LONG') then begin
    print, 'ERROR (match_xy): column "ID" in catalog '+cat_name+' must be INT or LONG type!'
    retall
  endif
  
  if (n_elements(uniq(cat_slave_p.ID,sort(cat_slave_p.ID))) NE Nslave) then begin
    print, 'ERROR (match_xy): column "ID" in catalog '+cat_name+' must be unique!'
    retall
  endif
    
  cat_slave = cat_slave_p
  
endif else begin
  ; Add an ID column to the catalog.
  cat_slave    = replicate(create_struct('ID', 0L, cat_slave_p[0]), Nslave)
  struct_assign, cat_slave_p, cat_slave
  cat_slave.ID = 1+lindgen(Nslave)
endelse

LABEL = tag_exist(/TOP_LEVEL, cat_slave,'LABEL') ? cat_slave.LABEL : '# '+strtrim(cat_slave.ID,2)

;; Make sure all the positions and errors are good numbers.
ind = where((finite(cat_slave.X) AND finite(cat_slave.Y) AND finite(cat_slave.X_ERR) AND finite(cat_slave.Y_ERR)) EQ 0, count)
if (count GT 0) then begin
  print, 'ERROR (match_xy): the following sources in catalog '+cat_name+' have some bad X,Y,X_ERR,Y_ERR values:'
  forprint, LABEL, cat_slave.X, cat_slave.Y, cat_slave.X_ERR, cat_slave.Y_ERR, SUBSET=ind
  print, 'Match was aborted!'
  retall
endif                                                                 

ind = where(((cat_slave.X_ERR GT 0) AND (cat_slave.Y_ERR) GT 0) EQ 0, count)
if (count GT 0) then begin
  print, 'WARNING (match_xy): the following sources in catalog '+cat_name+' have non-positive X_ERR,Y_ERR values:'
  forprint, LABEL, cat_slave.X, cat_slave.Y, cat_slave.X_ERR, cat_slave.Y_ERR, SUBSET=ind
  print, 'You may have read your catalog incorrectly, or your catalog may be missing some error information.'
endif

if ~keyword_set(quiet) && (Nslave GT 1) then begin
  dataset_1d, idex, cat_slave.X_ERR, DATASET=cat_name, BINSIZE=0.05, NORMALIZE_DENSITY=2, XTIT='X_ERR [pixels]'
  dataset_1d, idey, cat_slave.Y_ERR, DATASET=cat_name, BINSIZE=0.05, NORMALIZE_DENSITY=2, XTIT='Y_ERR [pixels]'
endif



; Apply offsets to a COPY of the slave catalog so we don't change it in the caller.
cat_slave.X = cat_slave.X + xshift_slave
cat_slave.Y = cat_slave.Y + yshift_slave

distance_shifted = sqrt(xshift_slave^2 + yshift_slave^2)
if (distance_shifted GT 0) then begin
  print, cat_name, distance_shifted, F='(%"Moving catalog %s a distance of %0.2f pixels.")'
endif

if keyword_set(init) then begin
  ;; Create a brand new match_state and copy the positions from the supplied catalog.
  Ncat = 1

  match_state = { ID:cat_slave.ID, LABEL:LABEL, X:cat_slave.X, Y:cat_slave.Y, X_ERR:cat_slave.X_ERR, Y_ERR:cat_slave.Y_ERR, cat_names:strarr(Ncat), sig_thresholds:dblarr(Ncat), catalogs:ptrarr(Ncat), match_primary:ptrarr(Ncat), match_secondary:ptrarr(Ncat) } 
endif else begin
  ; Disallow a slave catalog named the same as the master.
  if (cat_name EQ match_state.cat_names[0]) then begin
    print, 'ERROR (match_xy): the name of this slave catalog, '+cat_name+', conflicts with the name of the master catalog.'
    retall
  endif
endelse

Nmaster    = n_elements(match_state.X)
Nsecondary = 0L
num_with_secondary = 0L

dum = {PMATCH, IDm:0L, IDs:0L, deltaX:f_nan, deltaY:f_nan, rank:f_nan, type:isolated_type}
dum = {SMATCH, IDm:0L, IDs:0L, deltaX:f_nan, deltaY:f_nan, rank:f_nan }
match_primary   = replicate({PMATCH},Nmaster)
match_secondary = replicate({SMATCH},Nmaster)


if keyword_set(init) then begin
  ; Match slave_cat to itself with type set to init_type, then return.
  match_primary.IDm  = match_state.ID
  match_primary.IDs  = match_state.ID
  match_primary.type = init_type
  
  ptr_free, match_state.catalogs[0], match_state.match_primary[0]

  match_state.sig_thresholds[0] = keyword_set(significance_threshold) ? significance_threshold : 0
  match_state.cat_names     [0] = cat_name
  match_state.catalogs      [0] = ptr_new(cat_slave)
  match_state.match_primary [0] = ptr_new(match_primary)

  print, cat_name, F="(%'\nAdopted catalog %s as the MASTER.')"

  union_cat = cat_slave

  return
endif

print, cat_name, match_state.cat_names[0], significance_threshold, F="(%'\nMatching the slave catalog %s to the master catalog %s (significance_threshold = %0.3f)')"

;; Pull out some vectors from match_state and cat_slave structures to speed up execution.
cat_master     = *match_state.catalogs[0]
match_state_ID =  match_state.ID
  cat_slave_ID =    cat_slave.ID

Xs     = cat_slave.X
Ys     = cat_slave.Y
sig_Xs = cat_slave.X_ERR
sig_Ys = cat_slave.Y_ERR
var_Xs = sig_Xs^2
var_Ys = sig_Ys^2


; Initialize match_primary to be isolated sources from match_state.
match_primary.IDm  = match_state_ID
match_primary.type = isolated_type




;; Let the "null hypothesis" H0 be that these two position measurements are from a single source located at an unknown true position (Xtrue,Ytrue).  We assume we have 4 independent random variables, two X positions and two Y positions.  We need to choose a scheme for rejecting H0, i.e. for declaring that two catalog entries just do not plausibly match.  This means dividing the joint distribution into "acceptance" and "rejection" regions.

;; IF WE KNEW (Xtrue,Ytrue) then we could compute a confidence region on the 4-D jointly Gaussian distribution where the 4 axes are independent.  A simple region would be a "rectangle" in 4-D with an area equal to the significance desired; we would require that EACH measurement fall within a simple 1-D confidence interval for that axis. 

;; BUT, we DO NOT know (Xtrue, Ytrue).  Estimating Xtrue,Ytrue from the data is not appropriate since it will always be "near" the two observed positions.   Thus the more straighforward approach is to examine the offsets between the two observations directly.  Consider two new random variables representing the offset between the observed X & Y positions:
;; deltaX = abs(Xm - Xs)
;; deltaY = abs(Ym - Ys)
;; Under the null hypothesis, each of these is a difference between two measurements of the same object.  Thus, deltaX and deltaY are distributed as zero-mean Normals.  The variance of deltaX is equal to the sum of the two variances. The joint distributuion of these is a 2-D Gaussian.  An natural boundary of the confidence region would be a contour of the distribution, i.e. an ellipse.  All we would need to do is determine the value of the distribution at the appropriate contour, and then if the likelihood of the data exceeds that threshold then the match is accepted.   Unfortunately I cannot find publication of a clear derivation of the relationship between such a "confidence ellipse" (even in 2D) and the integrated probability enclosed.
;;  
;; Thus instead I'm going to use a confidence region that's a rectangle in this 2-D space.  In other words I'll require that each of the deltaX,deltaY offsets fall within a simple 1-D confidence interval for that axis.  If each 1-D confidence interval has a significance (area under the marginal 1-D PDF) of S then the significance of the corresponding confidence region in 2D (area under the 2-D PDF) will be S^2.  So for example to achieve a significance of 0.99 in 2D we require a significance of sqrt(0.99) = 0.99^(0.5) = 0.994987 for each 1-D interval.
;;
;; It is convenient to scale our deltaX and deltaY values so that they are in units of standard deviations, i.e. to "standardize" them or to compute the "standard score" aka "z-score".  Then, our confidence region becomes a square on the standard 2-D normal distribution, equivalent to a symmetric interval on the standard 1-D normal distribution.  Thus, our computation is distilled down to finding an integration limit "z_acceptable" such that the integral of the standard 1-D normal distribution over [-z_acceptable:+z_acceptable] is equal to sqrt(significance_threshold).
;;
;; The area in the two leftover tails of the distribution is simply (1-sqrt(significance_threshold)), and thus the area of the upper tail is simply (1-sqrt(significance_threshold)) / 2.  That expression is relevant because the IDL function gauss_cvf() will return the left-hand boundary of the upper tail of the standard Gaussian that has a specified area.  Thus, we can compute z_acceptable via:
;;;
;;;     z_acceptable = gauss_cvf((1 - sqrt(significance_threshold)) / 2.0 )


;; 
;;
;; BEWARE.  One may be tempted to use an alternate approach, namely to compute for EACH 4-tuple the integral of the 4D PDF within the rectangular region (in 4D) bounded by that 4-tuple, and then compare that area to a significance threshold (e.g. 0.99).  In this approach the rectangles can have wild shapes, depending on the data values, as opposed to the "compact" shape of the fixed "acceptance region" used in the first approach. Maybe there's some correspondence between the two approaches, but the second one seems wrong to me.

significance_threshold = double(significance_threshold)

z_acceptable = gauss_cvf((1 - sqrt(significance_threshold)) / 2.0 )
;help, z_acceptable

report_frac = [.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0]
t0 = systime(1)
;; Loop through primary catalog computing rank of every match that's judged to be plausible.
for ii=0L,Nmaster-1 do begin
;  if ((ii MOD 1000) EQ 0) then print, ii, ii / (systime(1)-t0)

  if (float(ii)/Nmaster GT report_frac[0]) then begin
    if ~keyword_set(quiet) then print, report_frac[0]*100, F='(%"Processed %d%% of master sources.")'
    report_frac = shift(report_frac,-1)
  endif
  
  IDm    = match_state.ID[ii]
  Xm     = match_state.X[ii]
  Ym     = match_state.Y[ii]
  
  sig_Xm = match_state.X_ERR[ii]
  sig_Ym = match_state.Y_ERR[ii]
  
  var_Xm = sig_Xm^2
  var_Ym = sig_Ym^2
  
  ; Compute best estimate of true position if H0 is true.  See Bevington section 4.1
  ; This is not actually used in the matching algorithm.
  ;Xtrue = (Xm*var_Xs + Xs*var_Xm) / (var_Xs + var_Xm)
  ;Ytrue = (Ym*var_Ys + Ys*var_Ym) / (var_Ys + var_Ym)

  ; Find the data points (source pairs) that fall in the "acceptance region" we've established.
  ; To get acceptable performance with large catalogs we're going to need to code this to minimize calculations ...
  
  ; Find the subset of slave sources that are acceptibly close to this master on the X-axis by
  ; normalizing the deltaX random variable, representing offsets between the two measurements, by the appropriate
  ; "sigma" and then comparing to a threshold.
  Zx = abs(Xm - Xs)/sqrt(var_Xm + var_Xs)  ; the "standard score" or "z-score" for DeltaX 
  
  one_axis_good_index = where(Zx LT z_acceptable, Ngood)
  if (Ngood EQ 0) then continue
  
  ; Similarly, normalize and threshold deltaY for the slave sources with acceptable X offsets..
  Zx = Zx[one_axis_good_index]
  Zy = abs(Ym - Ys[one_axis_good_index])/sqrt(var_Ym + var_Ys[one_axis_good_index]) ; the "standard score" or "z-score" for DeltaY 

  ind                 = where(Zy LT z_acceptable, Ngood)
  if (Ngood EQ 0) then continue

  Zx = Zx[ind]
  Zy = Zy[ind]

  two_axis_good_index = one_axis_good_index[ind]

  ; In the code below the vectors named rank, deltaX, deltaY, and IDs have Ngood elements.
  plausible_cat_slave = cat_slave[two_axis_good_index]
  IDs = plausible_cat_slave.ID
  
  ; We now have a set of matches that are all judged to be acceptable, i.e. to not be too extremely improbable.  However later we are going to have to rank these proposed matches in order to choose which to accept.  I think the most obvious metric to use for this ranking is the "likelihood", i.e. the height of the 4D jointly Gaussian PDF evaluated at the data 4-tuple.  Consider for a moment how this ranking method will behave when we have a pair of sources exactly coincident, i.e. the 4-tuple of data lies at the peak of the 4D PDF.  If all the measurements are very accurate (small sigma) then the PDF will be narrow and the peak will be high, producing a high rank.  On the other hand if the measurements are very inaccurate the PDF will be broad and the peak will be low, producing a low rank.  That seems to match ones intuition for a well-behaved ranking scheme.
  
  ; The likelihood of the X offset (Xm-Xs) is K*exp(-0.5*Zx^2).
  ; The likelihood of the Y offset (Ym-Ys) is K*exp(-0.5*Zy^2).
  ; Thus the likelihood of the match is the product of these two likelihoods: K*exp(-0.5*(Zx^2 + Zy^2)).
  ; It's convenient to use the log of the likelihood as the ranking metric:
  ;   log likelihood = log K  - 0.5*(Zx^2 + Zy^2)
  ; Since relative likelihood is all we care about, we ignore the constant log K.
  position_rank = -0.5*(Zx^2 + Zy^2)
  rank          = position_rank
  
  if keyword_set(match_rank_expression) then begin
    if NOT execute('rank = ' + match_rank_expression) then begin
      print, match_rank_expression, F='(%"MATCH_RANK_EXPRESSION (%s) could not be evaluated.")'
      help, /st, plausible_cat_slave, cat_master[ii]
      stop
    endif
  endif

  ; Compute position offsets which are reported in match structures (for use in estimating coordinate system transformations).
  ; We compute master - slave to get the same sign on these offsets as is neede for XSHIFT/YSHIFT inputs.
  deltaX = match_state.X[ii] - Xs[two_axis_good_index] 
  deltaY = match_state.Y[ii] - Ys[two_axis_good_index] 
  
  if (Ngood EQ 1) then begin
    ; There's only one possible match, the Primary Match.
    primary_ind = 0
  endif else begin
    ; The best one is the Primary Match.
    rank_maximum = max(rank, primary_ind)
    
    ; The rest are secondary matches.
    flag = replicate(1B, Ngood)
    flag[primary_ind] = 0
    secondary_ind = where(flag)
    
    if keyword_set(match_rank_expression) then begin
      dum = max(position_rank, max_position_rank_ind)
      if (max_position_rank_ind NE primary_ind) then $
        print, ii, rank_maximum, strjoin(string(rank[secondary_ind],F='(F0.2)'),' '), F='(%"Src %d: adopted match with likelihood %0.2f (better than %s) instead of most-likely match")'
    endif
    
    ; Allocate more space in all_records when necessary.
    NtoAdd = Ngood-1
    if ((Nsecondary+NtoAdd) GT n_elements(match_secondary)) then begin
      print, f='(%"extending match_secondary array...")'

      match_secondary = [match_secondary,replicate({SMATCH}, NtoAdd > 4000)]
    endif

    ; Save the secondary matches.    
    new_records = replicate({SMATCH}, NtoAdd)
    new_records.IDm        = IDm
    new_records.IDs        = IDs      [secondary_ind]
    new_records.deltaX     = deltaX   [secondary_ind]
    new_records.deltaY     = deltaY   [secondary_ind]
    new_records.rank       = rank     [secondary_ind]
    
    match_secondary[Nsecondary] = new_records
                    Nsecondary +=  NtoAdd
                    
    num_with_secondary++
  endelse
  
  ; Save the Primary Match, marking it as "successful" for now.
  match_primary[ii] = {PMATCH, IDm, IDs[primary_ind], deltaX[primary_ind], deltaY[primary_ind], rank[primary_ind], type:successful_primary_type}
endfor ;ii


;; Consider success of Primary Matches in order of rank.
; The strategy here is that when a source from list 1, P1, is judged to match
; a source from list 2, P2, then all other potential matches involving P1 and P2
; are DISABLED.  
; In other words a given source can NOT participate in multiple matches.
; We do however record "secondary matches" so one can tell if a reported match is
; unique/ambiguous.
;
; The potential downside of this approach is that a spurious match involving 
; source P1 will remove P1 from consideration in subsequent matches, possibly  
; preventing its true match from being recorded.  We minimize this problem by 
; processing matches sorted by their match distance, on the assumption that
; close matches are more reliable than larger ones.

sort_ind = reverse(sort(match_primary.rank))

for ii=0L,Nmaster-1 do begin
  match = match_primary[sort_ind[ii]]
  
  ; If this match is successful then all others involving this slave entry are NOT successful.
  if (match.type EQ successful_primary_type) then begin
  
    fail = where((match_primary.type EQ successful_primary_type) AND $
                 (match_primary.IDs  EQ match.IDs), count)
                 
    if (count EQ 0) then message, 'Bug found!'
    
    if (count GT 1) then begin
      ; Remove the current match from the fail list.
      fail = fail[ where(fail NE sort_ind[ii]) ]
      match_primary[fail].type = failed_primary_type

      if ~keyword_set(quiet) then begin
        print, match.IDm, match.IDs, F='(%"Successful primary match (%d,%d) forced the failure of these primary matches:")'
        forprint, match_primary[fail].IDm, match_primary[fail].IDs
      endif
    endif

  endif ;(successful_primary_type)
endfor ;ii


;; Store everything in match_state.  Overwrite any existing entry for this catalog.
ind = where(match_state.cat_names EQ cat_name, count)
if (count EQ 0) then begin
  ; Look for an empty spot.
  ind = where(match_state.cat_names EQ '', count)
endif

if (count EQ 0) then begin
  ; Need to enlarge match_state.
  Ncat = 1+n_elements(match_state.cat_names)
  ind  = Ncat-1
  temp_cat = match_state
  fa = fltarr(Nmaster)

  match_state = { ID:lonarr(Nmaster), LABEL:strarr(Nmaster), X:fa, Y:fa, X_ERR:fa, Y_ERR:fa, cat_names:strarr(Ncat), sig_thresholds:dblarr(Ncat), catalogs:ptrarr(Ncat), match_primary:ptrarr(Ncat), match_secondary:ptrarr(Ncat) } 
  
  struct_assign, temp_cat, match_state
endif

ptr_free, match_state.catalogs[ind], match_state.match_primary[ind], match_state.match_secondary[ind]

match_state.sig_thresholds[ind] = significance_threshold
match_state.cat_names     [ind] = cat_name
match_state.catalogs      [ind] = ptr_new(cat_slave)
match_state.match_primary [ind] = ptr_new(match_primary)
if (Nsecondary GT 0) then $
  match_state.match_secondary[ind] = ptr_new(match_secondary[0:Nsecondary-1])


;; Form the union of the master cat and cat_slave if desired.
if arg_present(union_cat) then begin
  
  ; Use the specified catalog as the template for the union.
  ; Create a union_cat entry with all fields nulled.
  null_union_record = keyword_set(use_slave_as_template) ? cat_slave[0] : cat_master[0]
  
 ;struct_assign, {zzz:0}, null_union_record
  null_union_record = null_structure(null_union_record)

  union_cat = replicate(null_union_record, Nmaster)

  ; Start with the master catalog.
  struct_assign, cat_master, union_cat
  
  label_available = tag_exist(/TOP_LEVEL, cat_master,'LABEL') && tag_exist(/TOP_LEVEL, cat_slave,'LABEL') 
  
  slave_unmatched_flag = replicate(1B, Nslave)
  
  ; Examine the matches.
  good = where(match_primary.type EQ successful_primary_type, Ngood)
  if (Ngood GT 0) then begin
    for ii=0L,Ngood-1 do begin
      match = match_primary[good[ii]]
      
      ind_m  = where(match_state_ID EQ match.IDm)
      ind_s  = where(  cat_slave_ID EQ match.IDs)
      master = union_cat[ind_m]
      slave  = cat_slave[ind_s]

      if (ind_m[0] NE good[ii]) then message, 'Order is suspect!'
      
      ; Mark the slave entry as matched.
      slave_unmatched_flag[ind_s] = 0
      
      ; Choose which position is to be carried forward.

      ; Create a union_cat entry with all fields nulled.
      this_union_record = null_union_record
      
      if keyword_set(prefer_master) then begin
      endif else if keyword_set(prefer_slave) then begin
        ; Use the slave position.
        if label_available then print, master.LABEL, slave.LABEL, F="(%'%s -> %s')" 
        struct_assign, slave, this_union_record
        union_cat[ind_m] = this_union_record
      endif else begin
        ; Update the master position if the slave is better.
        if ((slave.X_ERR^2+slave.Y_ERR^2) LT (master.X_ERR^2+master.Y_ERR^2)) then begin
          ; Replace the master entry in the union with the slave entry.
          if label_available then print, master.LABEL, slave.LABEL, F="(%'%s -> %s')" 
          struct_assign, slave, this_union_record
          union_cat[ind_m] = this_union_record
        endif
      endelse
    endfor ;ii
  endif
  
  ; Append any unmatched cat_slave entries to union_cat.
  extra = where(slave_unmatched_flag, Nextra)
  if (Nextra GT 0) then begin
    ; Enlarge union_cat with enough null rows to hold extra slave sources.
    temp         = temporary(union_cat)
    union_cat    = replicate(null_union_record, Nmaster+Nextra)
    union_cat[0] = temp
    
    ; Copy the extra slave sources into union_cat.
    for ii=0L,Nextra-1 do begin
      this_union_record = null_union_record
      struct_assign, cat_slave[extra[ii]], this_union_record
      union_cat[Nmaster+ii] = this_union_record
    endfor ;ii
  endif
  
  ; We have to regenerate unique ID tags.
  ; Even if the catalog was not expanded, an entry could have been replaced by the slave.
  union_cat.ID = lindgen(Nmaster+Nextra)

  print, Nextra, Nmaster+Nextra, F="(%'Appended %d sources to form union catalog with %d sources.')"  
     
  if keyword_set(union_reg) then begin
    print, 'Union catalog is depicted in the ds9 region file '+union_reg
    catalog_ds9_interface, union_cat, union_reg, /WRITE_REGFILE, _STRICT_EXTRA=extra_params
  endif
endif ;/UNION
    
    

; We do NOT report a tally of secondary matches here because in match_xy_analyze a few of them will not be counted because the slave source is participating in a primary match to another master source. 
num_successful_primary = total(match_primary.type EQ successful_primary_type, /INT)
print, num_successful_primary, round((100.0*num_successful_primary)/Nmaster), match_state.cat_names[0], cat_name, num_successful_primary-num_with_secondary, num_with_secondary,  F='(%"\n %d (%d%%) %s sources have primary matches to %s (%d unique and %d accompanied by secondary matches).\n")'   

return
end ; match_xy





;=============================================================================
;;; The input match_state is from match_xy routine.
;;;
;;; The optional boolean vector ANALYSIS_MASK must have the same number of elements 
;;; as the master catalog; only those source where ANALYSIS_MASK is true will
;;; participate in the offset analysis.
;;; 
;;; /CUSTOM1 enables some extra plots useful for ACIS work..
;;; Supplying ARCSEC_PER_PIXEL value makes plots use arcsecond units.
;;;
;;; The output composite_cat is a catalog with columns from all the catalogs in match_state.
;;; The output isolated_flag is a boolean vector identifying the sources with no counterparts in any slave catalog. 
;;;
;;; The region file produced includes labels for all the master sources, and all the slave sources involved in matches.
;;; Specify /LABEL_SLAVE_UNUSED to also produce labels for unused slave sources.
;;; Specify /SKIP_REGIONS to skip production of the region file.
;=============================================================================

PRO match_xy_analyze, match_state, ANALYSIS_MASK=analysis_mask, $
                      CUSTOM1=custom1, ARCSEC_PER_PIXEL=arcsec_per_pixel, QUIET=quiet_p, PLOT=plot, ARROW_PLOT=arrow_plot, $
                      SKIP_REGIONS=skip_regions, $
                      LABEL_SLAVE_UNUSED=label_slave_unused, $
                      ASTROMETRY=astrometry, RETAIN_DS9=retain_ds9, $
                        
                      composite_cat, isolated_flag, OUTPUT_DIR=output_dir, $
                      
                      XSHIFT_MEDIAN=xshift_median, YSHIFT_MEDIAN=yshift_median, $
                      
                      XSHIFT_MEAN      =xshift_mean      , YSHIFT_MEAN      =yshift_mean      , $
                      ERROR_XSHIFT_MEAN=xshift_mean_error, ERROR_YSHIFT_MEAN=yshift_mean_error, $

                      XSHIFT_WEIGHTED      =xshift_weighted      , YSHIFT_WEIGHTED      =yshift_weighted      , $
                      ERROR_XSHIFT_WEIGHTED=xshift_weighted_error, ERROR_YSHIFT_WEIGHTED=yshift_weighted_error

COMMON match_xy

creator_string = "match_xy_analyze, version " +strmid("$Rev:: 4456  $",7,5) +strmid("$Date: 2013-04-11 09:34:57 -0400 (Thu, 11 Apr 2013) $", 6, 11)
;print, creator_string, F='(%"\n\n%s")'
;print, systime()

match_xy_types, isolated_type, successful_primary_type, secondary_type, failed_primary_type, init_type

f_nan    = !VALUES.F_NAN

Nmaster  = n_elements(match_state.X)
Ncat     = n_elements(match_state.catalogs)
master_name = match_state.cat_names[0]

if (n_elements(analysis_mask) EQ 0) then begin
  analysis_mask = replicate(1B, Nmaster)
endif 
if (n_elements(analysis_mask) NE Nmaster) then begin
  print, 'ERROR (match_xy_analyze): ANALYSIS_MASK has the wrong number of elements!'
  return
endif

if ~keyword_set(skip_regions) then skip_regions=0

case n_elements(quiet_p) of
  0:      quiet = replicate(      0, Ncat)
  1:      quiet = replicate(quiet_p, Ncat)
  Ncat-1: quiet =        [0,quiet_p]
  else:   quiet = replicate(      0, Ncat)
endcase

case n_elements(plot) of
  0:      show_plot = replicate(   1, Ncat)
  1:      show_plot = replicate(plot, Ncat)
  Ncat-1: show_plot =        [0,plot]
  else:   show_plot = replicate(   1, Ncat)
endcase

if ~keyword_set(arrow_plot) then arrow_plot=0


output_dir = keyword_set(output_dir) ? output_dir+'/' : ''

;--------------------------------------------------------------------------
;; Build a null-filled composite catalog to hold data from all matches.
composite_record = {ID:0L, X:f_nan, Y:f_nan, X_ERR:f_nan, Y_ERR:f_nan}
tag_offset = 5

for jj=0L,Ncat-1 do begin
  cat_name  =  match_state.cat_names[jj]
  cat_slave = *match_state.catalogs [jj]

  ; Create a structure matching the slave catalog but with all fields nulled.
  null_slave_record = cat_slave[0]
  
 ;struct_assign, {zzz:0}, null_slave_record
  null_slave_record = null_structure(null_slave_record)
  
  ; Prepend fields from the match record.
  cat_record        = create_struct('IsNull',1B, 'deltaX',f_nan, 'deltaY',f_nan, 'deltaX_error',f_nan, 'deltaY_error',f_nan, 'rank',f_nan, 'type',isolated_type, 'num_SM',0L, null_slave_record)
  
  composite_record  = create_struct(composite_record, cat_name, cat_record)
endfor ;jj

composite_cat = replicate(composite_record, Nmaster)

;--------------------------------------------------------------------------
;; Populate composite_cat with data for the master catalog, which by convention is stored as the 0th "slave catalog".
composite_cat.ID    = match_state.ID
composite_cat.X     = match_state.X
composite_cat.Y     = match_state.Y
composite_cat.X_ERR = match_state.X_ERR
composite_cat.Y_ERR = match_state.Y_ERR

cat_master = *match_state.catalogs[0]

for ii=0L,Nmaster-1 do begin
  record = composite_cat[ii].(tag_offset+0)
  
  struct_assign, cat_master[ii], record
  
  composite_cat[ii].(tag_offset+0) = record
endfor

composite_cat.(tag_offset+0).IsNull       = 0
composite_cat.(tag_offset+0).deltaX       = 0
composite_cat.(tag_offset+0).deltaY       = 0
composite_cat.(tag_offset+0).deltaX_error = 0  
composite_cat.(tag_offset+0).deltaY_error = 0  
composite_cat.(tag_offset+0).rank         = 0
composite_cat.(tag_offset+0).type         = init_type
      
      
;--------------------------------------------------------------------------
;; These keep track of the type of match each master entry was involved in across all slave catalogs.  Thus ...
;;   master_successful_t[ii] is true if master ii had a successful match in ANY slave catalog.
;;   master_failed_t    [ii] is true if master ii had a failed     match in ANY slave catalog.
master_successful_t = replicate(0B,Nmaster)
master_failed_t     = replicate(0B,Nmaster)

;; Count the number of secondary matches each master source has in each slave catalog.
num_SM      = intarr(Ncat,Nmaster)

;; Figure out the name of the actual parameter corresponding to the formal parameter "composite_cat".
;; We will use this name to construct names for the region files we are creating.
composite_name = strlowcase((routine_names(composite_cat, ARG_NAME=(-1)))[0])
if (composite_name EQ '') then composite_name = 'composite_cat'

ind_wrap = indgen(Ncat) mod 6
symbol_list = (['diamond','cross','circle'      ,'box'    ,'X'     ,'arrow'])[ind_wrap]
color_list  = (['cyan'   ,'red'  ,'DodgerBlue'  ,'magenta','yellow','white'])[ind_wrap]


;--------------------------------------------------------------------------
;; Open a ds9 region file summarizing the results.
regfile1 = output_dir + composite_name+'.reg'
regfile2 = output_dir + composite_name+'_pm.reg'

if ~skip_regions then begin
  openw, regunit1, regfile1, /GET_LUN
  ;openw, regunit2, regfile2, /GET_LUN
  printf, regunit1, "# Region file format: DS9 version 3.0"
  printf, regunit1, 'global width=1 font="helvetica 12 normal"'
  ;printf, regunit2, "# Region file format: DS9 version 3.0"
  
  comment1 = "# " + strjoin(match_state.cat_names+' ('+symbol_list+')'  ,', ') + "; successful primary match (green), failed primary match (red), secondary match (magenta), isolated master source (cyan diamond), unused slave source (blue)"
  printf, regunit1, comment1
  printf, regunit1
  printf, regunit1, min(match_state.X), max(match_state.Y), comment1, F='(%"text %f %f # text={%s} color=red")'
  
  
  ;comment2 = "# " + strjoin(match_state.cat_names+' ('+color_list+' '+symbol_list+')'  ,', ') + '; matches are green'
  ;printf, regunit2, comment2
  ;printf, regunit2
  ;printf, regunit2, min(match_state.X), max(match_state.Y), comment2, F='(%"text %f %f # text={%s} color=red")'
endif else begin
  regunit1 = -1
  regunit2 = -1
endelse


;--------------------------------------------------------------------------
;; Process each of the "slave" catalogs.

;; For coding convenience the master catalog is replicated in both 
;; {match_state.ID, match_state.X/Y, match_state.X_ERR/Y_ERR} and 
;; in *match_state.catalogs[0] (where the matches appear as type "init_type"). 
;;
;; The loop below does two jobs: 
;; 1. recording what sort of matches each slave source was involved in, using the slave_successful_t, slave_failed_t, and slave_secondary_t boolean vectors
;; 2. adding slave catalog entries to composite_cat.

;; On the first iteration (jj EQ 0) the "slave catalog" is a copy of the master catalog; job #1 does not apply, but we want to do job #2 in order to get the master catalog entries into composite_cat.

for jj=1L,Ncat-1 do begin
  z_acceptable = gauss_cvf((1 - sqrt(match_state.sig_thresholds[jj])) / 2.0 )

  cat_name  =  match_state.cat_names[jj]
  cat_slave = *match_state.catalogs [jj]
  Nslave    = n_elements(cat_slave)
  
  slave_label  = tag_exist(/TOP_LEVEL, cat_slave,'LABEL') ? cat_slave.LABEL : '# '+strtrim(cat_slave.ID,2)

  ; We need to keep track of the types of match each slave source is involved in, so that it can later be displayed correctly.   We'd like a slave source involved in a successful match to be green even if it is also a failed or secondary match for another primary source.
  slave_successful_t = bytarr(Nslave)
  slave_failed_t     = bytarr(Nslave)
  slave_secondary_t  = bytarr(Nslave)

  ; Write error circles for slave sources to region file.
;  !TEXTUNIT = regunit1
;  if ~skip_regions then forprint, TEXTOUT=5, /NoComm, cat_slave.X, cat_slave.Y, 0.5*(cat_slave.X_ERR + cat_slave.Y_ERR),  F='(%"circle %f %f %f # tag={error circle} color=Black")'

  ;; Pull out some vectors from match_state and cat_slave structures to speed up execution.
  match_state_ID = match_state.ID
    cat_slave_ID =   cat_slave.ID
  
  
  ;--------------------------------------------------------------------------
  ;; Process the Primary Match table.  
  match_primary = *match_state.match_primary[jj]
  
  t0 = systime(1)
  report_frac = [99,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0]
  for ii=0L,Nmaster-1 do begin
  
    if (float(ii)/Nmaster GT report_frac[0]) then begin
      if ~quiet[jj] then print, cat_name, report_frac[0]*100, F='(%"Processed %s matches for %d%% of master sources.")'
      report_frac = shift(report_frac,-1)
    endif
    
    match  = match_primary[ii]
    
    save_slave_entry = 0
    
    ind_m  = where(match_state_ID EQ match.IDm)
    ind_s  = where(  cat_slave_ID EQ match.IDs, count)

    if (count GT 0) then begin
      deltaX_error = sqrt(match_state.X_ERR[ind_m]^2 + cat_slave[ind_s].X_ERR^2)
      deltaY_error = sqrt(match_state.Y_ERR[ind_m]^2 + cat_slave[ind_s].Y_ERR^2)
    endif else begin
      deltaX_error = !VALUES.F_NAN
      deltaY_error = !VALUES.F_NAN      
    endelse

    if (ind_m[0] NE ii) then message, 'Order is suspect!'


    if (match.type EQ isolated_type) then begin
      ; There is no slave match to save or depict in ds9.
      save_slave_entry = 0
      
    endif else if (match.type EQ init_type) then begin
      ; This should be the jj==0 case (the first slave catalog) where the "slave catalog" is a copy of the master catalog.  We want to copy the entry data to the composite catalog, but no ds9 region should be constructed.
      save_slave_entry = 1
      
    endif else begin
      ; Successful and failed PMs here.
      save_slave_entry = 1
      
      if (match.type EQ successful_primary_type) then begin
        ; Successful PM.
        master_successful_t[ind_m] = 1
        slave_successful_t [ind_s] = 1

        color     = 'green'
        line_tag  = 'primary match, '+cat_name+', good'
        thick     = 1
        
      endif else begin
        ; Failed PM.
        master_failed_t[ind_m] = 1 
        slave_failed_t [ind_s] = 1
        
        color     = 'red'
        line_tag  = 'primary match, '+cat_name+', failed'
        thick     = 1
      endelse

      ; In reg file #1 write a line segment connecting master and slave to depict the primary match.
      ; Extend the line to the point where the match would have failed (i.e. to the edge of the match rectangle surrounding the master) to give some visual indication of significance of the match.
      max_deltaX_for_match = z_acceptable * deltaX_error
      max_deltaY_for_match = z_acceptable * deltaY_error
      line_scaling         = min(/NAN, abs([(max_deltaX_for_match/match.deltaX),(max_deltaY_for_match/match.deltaY)]))
      
      if (line_scaling GT 1000) || ~finite(line_scaling) then begin
        ; The match is very close. 
        ; The master and slave entries might very well be identical, if the slave was earlier used to construct the master catalog.
        ; If that's the case, then we don't want any line graphic at all.
      endif else begin
        ; Scale the match line to the desired length.
        x0 = match_state.x[ind_m]
        y0 = match_state.y[ind_m]
        
        x1 = x0 - line_scaling*match.deltaX
        y1 = y0 - line_scaling*match.deltaY
        if ~skip_regions then printf, regunit1, x0, y0, x1, y1, line_tag, color, thick, F='(%"line %f %f %f %f # tag={%s} color=%s width=%d")'
      endelse
      
    endelse  ; Successful and failed PMs
    
    if save_slave_entry then begin
      ; Write slave entry to composite catalog plus fields from match record.
      record = composite_cat[ind_m].(tag_offset+jj)
      struct_assign, cat_slave[ind_s], record
      
      record.IsNull       = 0
      record.deltaX       = match.deltaX
      record.deltaY       = match.deltaY
      record.deltaX_error = deltaX_error  
      record.deltaY_error = deltaY_error  
      record.rank         = match.rank
      record.type         = match.type
      
      composite_cat[ind_m].(tag_offset+jj) = record
    endif
  endfor ;ii  loop over master sources

  ; The purpose of the jj==0 loop is simply to populate composite_cat with the master sources, so abort that loop here.
  if (jj EQ 0) then continue
  
  ;--------------------------------------------------------------------------
  ; Process the Secondary Match table.
  if ptr_valid(match_state.match_secondary[jj]) then begin
    match_secondary = *match_state.match_secondary[jj]
    
    for ii=0L,n_elements(match_secondary)-1 do begin
      match  = match_secondary[ii]
      ind_m  = where(match_state_ID EQ match.IDm)
      ind_s  = where(  cat_slave_ID EQ match.IDs)
      
      
      ; We *display* every secondary match.  
      slave_secondary_t [ind_s] = 1
      
      ; However when *counting* secondary matches (in num_SM) for the master source ind_m, we ignore cases where the slave entry is part of a successful match to another master, since that slave is really not available to form a match with source ind_m.
      if ~slave_successful_t[ind_s] then num_SM[jj,ind_m]++
      
      deltaX_error = sqrt(match_state.X_ERR[ind_m]^2 + cat_slave[ind_s].X_ERR^2)
      deltaY_error = sqrt(match_state.Y_ERR[ind_m]^2 + cat_slave[ind_s].Y_ERR^2)

      
      ; In reg file #1 write a line segment connecting master and slave to depict the secondary match.
      ; Extend the line to the point where the match would have failed (i.e. to the edge of the match rectangle surrounding the master) to give some visual indication of significance of the match.
      color = 'Purple'
      max_deltaX_for_match = z_acceptable * deltaX_error
      max_deltaY_for_match = z_acceptable * deltaY_error
      line_scaling         = min(/NAN, abs([(max_deltaX_for_match/match.deltaX),(max_deltaY_for_match/match.deltaY)]))
      
      if (line_scaling GT 1000) || ~finite(line_scaling) then begin
        ; The match is very close. 
        ; The master and slave entries might very well be identical, if the slave was earlier used to construct the master catalog.
        ; If that's the case, then we don't want any line graphic at all.
      endif else begin
        ; Scale the match line to the desired length.
        x0 = match_state.x[ind_m]
        y0 = match_state.y[ind_m]
        
        x1 = x0 - line_scaling*match.deltaX
        y1 = y0 - line_scaling*match.deltaY
        if ~skip_regions then printf, regunit1, x0, y0, x1, y1, 'secondary match, '+cat_name, color, F='(%"line %f %f %f %f # tag={%s} color=%s")'
      endelse
    endfor ;ii loop over secondary matches
    
    composite_cat.(tag_offset+jj).num_SM = reform(num_SM[jj,*])
    
    
    ; Report the likelihood ratio between the primary and secondary matches for each successful_primary match that has secondary matches.
    if ~quiet[jj] then begin
      ind = where(num_SM[jj,*] GT 0, count)
      for ii=0L, count-1 do begin
        ; Identify this master source and look up it's likelihood.
        ind_m = ind[ii]
        
        ; We cannot use the vector master_successful_t in the test below because it spans multiple slave catalogs.
        if (match_primary[ind_m].type NE successful_primary_type) then continue
  
        ind_s_primary  = where(  cat_slave_ID EQ match_primary[ind_m].IDs, count)
        
        rank_m = match_primary[ind_m].rank
        IDm    = match_primary[ind_m].IDm
        
        print, match_state.ID[ind_m], match_state.X[ind_m], match_state.Y[ind_m], num_SM[jj,ind_m], F='(%"\nMaster source %d at (%0.1f,%0.1f) has %d secondary matches.\n   position       ratio of primary to secondary position likelihood\n--------------------------------------")'
        
        ; Find the entries in match_secondary corresponding to this master source.
        ind_match_secondary = where(match_secondary.IDm EQ IDm)
        
        for kk=0L, n_elements(ind_match_secondary)-1 do begin
          match  = match_secondary[ind_match_secondary[kk]]
          
          ind_s_secondary  = where(  cat_slave_ID EQ match.IDs)
          
          if slave_successful_t[ind_s_secondary] then continue
          
          photometry_report = ''  
          if tag_exist(/TOP_LEVEL, cat_slave, 'UKIDSS') && tag_exist(/TOP_LEVEL, cat_slave, 'TWOMASS') then $
            photometry_report = string( min(/NAN, [cat_slave[ind_s_primary  ].UKIDSS.K, cat_slave[ind_s_primary  ].TWOMASS.K_M]), $
                                        min(/NAN, [cat_slave[ind_s_secondary].UKIDSS.K, cat_slave[ind_s_secondary].TWOMASS.K_M]), $
                                        F='(%"  (K=%4.1f vs %4.1f)")' )
          
          print, round(cat_slave[ind_s_secondary].X), round(cat_slave[ind_s_secondary].Y), exp( rank_m - match.rank ), photometry_report, F='(%"  (%d,%d)  %7.1f %s")'
        endfor ; kk loop
      
      endfor ; ii loop reporting likelihood ratios for secondary matches
    endif ; ~quiet[jj]
  endif ;ptr_valid()
  

  ; Report the number of matches.  
  if ~quiet[jj] then begin
    num_successful_primary = total(match_primary.type EQ successful_primary_type, /INT)
    
    num_with_secondary = total( (num_SM[jj,*] GT 0), /INT)
    
    print, num_successful_primary, round((100.0*num_successful_primary)/Nmaster), master_name, cat_name, num_successful_primary-num_with_secondary, num_with_secondary, F='(%"\n %d (%d%%) %s sources have primary matches to %s (%d unique and %d accompanied by secondary matches).")'   
    
  endif

  
  ;--------------------------------------------------------------------------
  ;; Write ds9 regions for all the slave entries, properly color coded.
  
  ; If involved in any successful match, the position symbol should be green.
  ind = where(slave_successful_t, count)
  if (count GT 0) then begin
    slave_failed_t   [ind] = 0 
    slave_secondary_t[ind] = 0   
    
    color      = 'green'
    source_tag = 'position, '+cat_name+', matched'
    format = string(symbol_list[jj], source_tag, color, F='(%"(\%\"%s  point \%f \%f # tag={%s} text={\%s} color=%s\")")')
    
    !TEXTUNIT = regunit1
    if ~skip_regions then forprint, SUBSET=ind, TEXTOUT=5, /NoComm, cat_slave.x, cat_slave.y, slave_label, F=format
    
    ; In reg file #2 write the catalog-specific symbol to record the successful PM.
;   !TEXTUNIT = regunit2
;   if ~skip_regions then forprint, SUBSET=ind, TEXTOUT=5, /NoComm, cat_slave.x, cat_slave.y, F=format
  endif
  
  
  ; If involved in any failed match, the position symbol should be red.
  ind = where(slave_failed_t, count)
  if (count GT 0) then begin
    slave_secondary_t[ind] = 0   
    
    color      = 'red'
    source_tag = 'position, '+cat_name+', matched'
    format = string(symbol_list[jj], source_tag, color, F='(%"(\%\"%s  point \%f \%f # tag={%s} text={\%s} color=%s\")")')
    
    !TEXTUNIT = regunit1
    if ~skip_regions then forprint, SUBSET=ind, TEXTOUT=5, /NoComm, cat_slave.x, cat_slave.y, slave_label, F=format
  endif
  
  
  ; If involved in any secondary match, the position symbol should be Purple
  ind = where(slave_secondary_t, count)
  if (count GT 0) then begin
    color      = 'Purple'
    source_tag = 'position, '+cat_name+', matched'
    format = string(symbol_list[jj], source_tag, color, F='(%"(\%\"%s  point \%f \%f # tag={%s} text={\%s} color=%s\")")')
    
    !TEXTUNIT = regunit1
    if ~skip_regions then forprint, SUBSET=ind, TEXTOUT=5, /NoComm, cat_slave.x, cat_slave.y, slave_label, F=format
  endif
  
  
  ; Write position symbols for unused slave entries.
  ind = where(~(slave_successful_t OR slave_failed_t OR slave_secondary_t), count)
  if (count GT 0) then begin
    color      = 'DodgerBlue'
    source_tag = 'position, '+cat_name+', unused'
    format = string(symbol_list[jj], source_tag, color, F='(%"(\%\"%s  point \%f \%f # tag={%s} text={\%s} color=%s\")")')
    
    !TEXTUNIT = regunit1
    if ~skip_regions then forprint, SUBSET=ind, TEXTOUT=5, /NoComm, cat_slave.x, cat_slave.y, slave_label, F=format
  endif
  
endfor ;jj loop over each slave catalog


;--------------------------------------------------------------------------
;; Write ds9 regions for all the master entries, properly color coded to designate isolated, failed PM, successful PM.
; Since ds9 center-justifies text, we must offset the labels for readability, say by 0.2% of the field.
margin = ((max(match_state.x) - min(match_state.x)) > (max(match_state.y) - min(match_state.y))) / 500.

master_label  = tag_exist(/TOP_LEVEL, match_state,'LABEL') ? match_state.LABEL : '# '+strtrim(match_state.ID,2)


; If PM failed in ANY catalog then we want to report it.
ind = where(master_failed_t, count)
if (count GT 0) && (total(quiet) EQ 0) then begin
  ; We have to "give up" !TEXTUNIT to prevent the forprint (to STDOUT) below from closing the file unit currently stored in !TEXTUNIT.
  !TEXTUNIT = 0
  print, count, F='(%"\nThese %d master catalog entries produced \"failed primary matches\" (the best counterpart was assigned to another master source):")'
  forprint, SUBSET=ind, match_state.ID, match_state.x, match_state.y, F='(%"%d  %0.1f  %0.1f ")'
endif


; If involved in any successful match, the position symbol should be green.
ind = where(master_successful_t, count)
if (count GT 0) then begin
  master_failed_t   [ind] = 0 
  
  color      = 'green'
  
  suffix            = replicate(', matched, not analyzed', Nmaster)
  ind_analysis_mask = where(analysis_mask, count_analysis_mask)
  if (count_analysis_mask GT 0) then $
    suffix[ind_analysis_mask] = ', matched'
  source_tag = 'position, '+master_name+suffix  
  
  format = string(symbol_list[0], color, F='(%"(\%\"%s  point \%f \%f # tag={\%s} text={\%s} color=%s\")")')
  
  !TEXTUNIT = regunit1
  if ~skip_regions then forprint, SUBSET=ind, TEXTOUT=5, /NoComm, match_state.x, match_state.y, source_tag, master_label, F=format
endif


; If involved in any failed match, the position symbol should be red
ind = where(master_failed_t, count)
if (count GT 0) then begin
  
  color      = 'red'
  source_tag = 'position, '+master_name+', failed'  
  
  format = string(symbol_list[0], source_tag, color, F='(%"(\%\"%s  point \%f \%f # tag={%s} text={\%s} color=%s\")")')
  
  !TEXTUNIT = regunit1
  if ~skip_regions then forprint, SUBSET=ind, TEXTOUT=5, /NoComm, match_state.x, match_state.y, master_label, F=format
endif


; For return to the caller, create a boolean vector identifying the sources with no counterparts in any slave catalog.
; Mark these sources with the master-cat symbol, with a graphic representing the zone where a counterpart with zero position error would have matched.
; The zone is determined by the LARGEST significance_threshold that was specified for any match.

isolated_flag = ~(master_failed_t OR master_successful_t)

largest_sig_threshold = max(match_state.sig_thresholds)
largest_z_acceptable  = gauss_cvf((1 - sqrt(largest_sig_threshold)) / 2.0 )

ind = where(isolated_flag, count)
if (count GT 0) then begin

  format = string(color_list[0], F='(%"(\%\"box \%f \%f \%f \%f 0 # tag={match domain} color=%s\")")')
  
  !TEXTUNIT = regunit1
  if (~skip_regions && (largest_sig_threshold GT 0)) then forprint, SUBSET=ind, TEXTOUT=5, /NoComm, match_state.x, match_state.y, 2*(largest_z_acceptable * match_state.X_ERR), 2*(largest_z_acceptable * match_state.Y_ERR), F=format
  
  color      = color_list[0]
  source_tag = 'position, '+master_name+', isolated'
  format = string(symbol_list[0], source_tag, color, F='(%"(\%\"%s  point \%f \%f # tag={%s} text={\%s} color=%s\")")')
  
  !TEXTUNIT = regunit1
  if ~skip_regions then forprint, SUBSET=ind, TEXTOUT=5, /NoComm, match_state.x, match_state.y, master_label, F=format
endif

if ~skip_regions then free_lun, regunit1
;if ~skip_regions then free_lun, regunit2


if (min(quiet) EQ 0) && ~skip_regions then begin
  print
  print, regfile1, F="(%'The region file %s shows details of matching using these symbols and colors:')"
  
  print, comment1
  print
;  print, 'The region file ', regfile2, ' shows each catalog with a different symbol with matches marked in green:'
;  print, comment2
;  print
endif


if keyword_set(astrometry) && ~skip_regions then begin
  repeat begin
    session_name = string(random()*1E4, F='(I4.4)')
    temproot = 'match_xy' + session_name +'.noindex/'
    temproot = filepath(temproot, /TMP)
  endrep until (NOT file_test(temproot))
  file_mkdir, temproot
  tempdir = temproot

  ;; Write a temporary FITS image carrying the astrometry information defining the tangent plane.
  ;; We have found that ds9 can make astrometry conversion mistakes if the regions extend way beyond the footprint of the dummy image we make below.
  temp_image_fn = tempdir + 'temp.img'
  
  blank_image = bytarr(floor(max(composite_cat.X)),floor(max(composite_cat.Y)))
  mkhdr, blank_image_hdr, blank_image, /IMAGE
  
  ;; We are using the xy2ad.pro and ad2xy.pro programs elsewhere in the match_xy package to convert between celestial and "X/Y" coordinates.
  ;; Because that code thinks of X/Y coordinates as 0-based indexes into an IDL array, the astrometry structure passed has been offset by one pixel.
  ;; To produce a set of correct FITS astrometry keywords, we must undo that offset below.
  fits_astrometry = astrometry
  fits_astrometry.CRPIX--
  
  putast, blank_image_hdr, fits_astrometry, CD_TYPE=1  ;CD_TYPE=1 ensures CDELT keywords carry plate scale   
  
  writefits, temp_image_fn, blank_image, blank_image_hdr
  
  ; Load the temp image and region file into ds9.
  print, 'Please WAIT while ds9 converts regions to celestial coordinates ...'
  cmd = string(session_name, temp_image_fn, regfile1, F='(%"ds9 -xpa local -title ''%s'' ''%s'' -regions %s &")')
  spawn, cmd
  my_ds9 = "DS9:"+session_name

  ; Wait for ds9 to register with XPA.
  ; Starting in CIAO 4.0, xpaaccess uses the exit code to return its result.
  ; Thus we can no longer allow run_command to interpret a non-zero exit code as failure.
  repeat begin
    spawn, string(my_ds9, F='(%"xpaaccess ''%s''")'), result
    if (result[0] EQ 'yes') then break
    print, 'waiting for ds9 to come up...'
    wait,2
  endrep until (0)
  
  cmd = [ string(my_ds9,           F='(%"xpaset -p ''%s'' regions system wcs")')        ,$
          string(my_ds9,           F='(%"xpaset -p ''%s'' regions skyformat degrees")') ,$  
          string(my_ds9, regfile1, F='(%"xpaset -p ''%s'' regions save %s")') ]
  
  for ii=0,2 do begin
    spawn, EXIT_STATUS=status, cmd[ii], result, /STDERR
  
    if (status NE 0) then begin
      print, result
      print, regfile1, F='(%"\nCommunication with ds9 has been lost.  Save the regions to %s in celestial coordinates, then press return.'
      s = ''
      read,s
      break
    endif
  endfor ;ii
        
  if ~keyword_set(retain_ds9) then spawn, string(my_ds9,           F='(%"xpaset -p ''%s'' exit")')
endif


;--------------------------------------------------------------------------
;; Finally, show some match statistics to help the user choose catalog offsets.
offset_units = (keyword_set(arcsec_per_pixel) ? '[arcsec]' : '[pixel]')

for jj=0L,Ncat-1 do begin
    
  z_acceptable = gauss_cvf((1 - sqrt(match_state.sig_thresholds[jj])) / 2.0 )
  
  cat_name  =  match_state.cat_names[jj]
  
  match_name=  master_name+' / '+cat_name
  
  composite_cat_section = composite_cat.(tag_offset+jj)

  mask = (composite_cat_section.type EQ successful_primary_type) AND analysis_mask  
  
  good_ind = where( mask, Nanalysis, COMPLEMENT=bad_ind, NCOMPLEMENT=Nignore )

  if (jj EQ 0) then begin
  endif else if (Nanalysis EQ 0) then begin
    if ~quiet[jj] then print, cat_name, F='(%"\nWARNING! The slave catalog %s has no matches that can estimate an offset from the master catalog!")'
    xshift_median         = f_nan
    yshift_median         = f_nan
    xshift_mean           = f_nan
    Yshift_mean           = f_nan
    xshift_mean_error     = f_nan
    yshift_mean_error     = f_nan
    xshift_weighted       = f_nan
    yshift_weighted       = f_nan
    xshift_weighted_error = f_nan
    yshift_weighted_error = f_nan
  endif else begin
    if ~quiet[jj] then print,   match_name, F='(%"\n--------------------------------------------------------------------------\nSummary of %s MATCHES")'
    
    ;; Examine the deltaX,deltaY offsets.
    deltaX = composite_cat_section.deltaX
    deltaY = composite_cat_section.deltaY
    
    deltaX_error = composite_cat_section.deltaX_error
    deltaY_error = composite_cat_section.deltaY_error
    
    ; The data for the catalog entries we do NOT want to analyze are set to NaN, rather than filtered away, so that the observer can read off indexes in dataset_2d.
    if (Nignore GT 0) then begin
      deltaX      [bad_ind] = f_nan
      deltaY      [bad_ind] = f_nan
      deltaX_error[bad_ind] = f_nan
      deltaY_error[bad_ind] = f_nan
    endif
    
    deltaX_title = 'Xmaster - Xslave '+ offset_units
    deltaY_title = 'Ymaster - Yslave '+ offset_units
    radius_title =   'match distance '+ offset_units
    
    if keyword_set(arcsec_per_pixel) then begin
      deltaX       *= arcsec_per_pixel  
      deltaY       *= arcsec_per_pixel  
      deltaX_error *= arcsec_per_pixel  
      deltaY_error *= arcsec_per_pixel  
    endif
    
    radius = sqrt(deltaX^2 + deltaY^2)
    if ~quiet[jj] then print, radius_title, median([radius]             ), F='(%"\nMedian %s: %0.3f")'
    if ~quiet[jj] then print, radius_title, mean  (radius, /DOUBLE, /NAN), F='(%"\nMean   %s: %0.3f")'

    if ~quiet[jj] then print, z_acceptable, F='(%"\nThe Z limit used for matching was %0.2f.  ( \"significance\" = (1D - 2*gauss_pdf(Z))^2 )")'
      
    if show_plot[jj] && (Nanalysis GT 1) then begin
      dataset_1d, idx, deltaX,  DATASET=match_name, BINSIZE=0.05, NORMALIZE_DENSITY=2, XTIT=deltaX_title
      dataset_1d, idy, deltaY,  DATASET=match_name, BINSIZE=0.05, NORMALIZE_DENSITY=2, XTIT=deltaY_title
      dataset_1d, idr2, radius, DATASET=match_name, BINSIZE=0.05, NORMALIZE_DENSITY=2, XTIT=radius_title
      
      dataset_1d, idsx, abs(deltaX) / deltaX_error, DATASET=match_name, BINSIZE=0.1, NORMALIZE_DENSITY=2, XTIT='|Xoffset|/Xsigma'
      dataset_1d, idsy, abs(deltaY) / deltaY_error, DATASET=match_name, BINSIZE=0.1, NORMALIZE_DENSITY=2, XTIT='|Yoffset|/Ysigma'
      sample_of_standard_Z = abs(random((100*Nanalysis)>10000, /NORMAL)) / 1.0
      dataset_1d, idsx, sample_of_standard_Z      , DATASET='Gaussian offsets', BINSIZE=0.05
      dataset_1d, idsy, sample_of_standard_Z      , DATASET='Gaussian offsets', BINSIZE=0.05
      
      z_enclosing_match = (abs(deltaX) / deltaX_error) > (abs(deltaY) / deltaY_error)
      dataset_1d, idsxy, z_enclosing_match, DATASET=match_name, BINSIZE=0.1, NORMALIZE_DENSITY=2, XTIT='Z limit required for match (|Xoffset|/Xsigma > |Yoffset|/Ysigma)'
;      
;      significance_threshold_required = (1D - 2*gauss_pdf(z_enclosing_match))^2
;      
;      dataset_1d, idsxy, alog10(significance_threshold_required), DATASET=match_name, XTIT='log of significance_threshold required for match'
      

     ;dataset_2d, idxe, abs(deltaX) / deltaX_error, deltaX_error, NAN_VALUES=[0,0], DATASET=match_name, XTIT='|Xoffset|/Xsigma', YTIT='Xsigma', PSYM=1
     ;dataset_2d, idye, abs(deltaY) / deltaY_error, deltaY_error, NAN_VALUES=[0,0], DATASET=match_name, XTIT='|Yoffset|/Ysigma', YTIT='Ysigma', PSYM=1
    endif
    
    ;; ------------------------------------------------------------------------
    ; It's important to make the partvelvec plot have unity aspect.
    ; Estimate the plot region size in device units.
    xrange = minmax(composite_cat.X)
    yrange = minmax(composite_cat.Y)
    xlen_est = !D.X_SIZE - !D.X_CH_SIZE * total( !X.margin )
    ylen_est = !D.Y_SIZE - !D.Y_CH_SIZE * total( !Y.margin )
  
    ; Enlarge the axis ranges to center desired region and have 1-1 aspect.
    pixel_size = max( [(xrange[1] - xrange[0]) / xlen_est, $
                       (yrange[1] - yrange[0]) / ylen_est] )
                
    xrange = ((xrange[0]+xrange[1]) / 2.) + $
                        pixel_size * xlen_est * [-0.5,0.5]
    
    yrange = ((yrange[0]+yrange[1]) / 2.) + $
                        pixel_size * ylen_est * [-0.5,0.5]

   ;rescale = (xrange[1]-xrange[0]) / median(deltaX[good_ind])
    if show_plot[jj] && (Nanalysis GT 1) then begin
      if keyword_set(arrow_plot) then begin
        window, jj, xsize=1800,ysize=1400
        
        ; Let's plot only 1000 points to avoid clutter.
        num_to_plot = 1000
        plot_ind = (Nanalysis LT num_to_plot) ? good_ind : congrid(good_ind, num_to_plot)
              
        partvelvec, deltaX[plot_ind], deltaY[plot_ind], composite_cat[plot_ind].X, composite_cat[plot_ind].Y, TITLE='Offsets (magnified) for the analyzed '+match_name+' matches', XTIT='X [pixel]', YTIT='Y [pixel]', XRANGE=xrange, YRANGE=yrange, XSTYLE=1, YSTYLE=1, LENGTH=0.12
      endif ; keyword_set(arrow_plot)
      
      dataset_2d, idxdx, (composite_cat_section.X)[good_ind], deltaX[good_ind], DATASET=match_name, PSYM=3, XTIT='X [pixel]', YTIT=deltaX_title
      dataset_2d, idydy, (composite_cat_section.Y)[good_ind], deltaY[good_ind], DATASET=match_name, PSYM=3, XTIT='Y [pixel]', YTIT=deltaY_title
    endif

;   print, Nanalysis, cat_name, master_name, F='(%"\nThe observed offsets (for the specified %d matches) from the slave catalog \'%s\' to the master catalog \`%s\` have been plotted.\n")'
                      

    ;; -----------------------------------------------------------------------
    ;; Make several estimates of the offset between the catalogs' astrometric frames.
    good_deltaX       = deltaX      [good_ind]
    good_deltaY       = deltaY      [good_ind]
    good_deltaX_error = deltaX_error[good_ind]
    good_deltaY_error = deltaY_error[good_ind]
        
    
    if ~quiet[jj] then print, cat_name, master_name, cat_name, F='(%"\nYou might improve the alignment between the slave catalog \`%s\` and the master catalog \`%s\` \nby adding the offsets suggested below to the XSHIFT/YSHIFT parameters of \`%s\`.")'
    
    ; Estimate the astrometric shift by our own routine for estimating uncertainties on the median.
    median_with_ci, good_deltaX, xshift_median, limit_lower, limit_upper, actual_confidence_level
    xshift_median_error = 0.5*(limit_upper-limit_lower)
    
    median_with_ci, good_deltaY, yshift_median, limit_lower, limit_upper, actual_confidence_level
    yshift_median_error = 0.5*(limit_upper-limit_lower)
    
    if ~quiet[jj] then print, xshift_median, yshift_median, offset_units, xshift_median_error, yshift_median_error, Nanalysis, Nanalysis, F='(%"  median       : XSHIFT= %7.3f, YSHIFT= %7.3f %s; 1-sigma errors: %0.3f %0.3f ; # %d, %d matches")'

    
    ; Estimate the astrometric shift by a sigma-clipped mean routine in the Astro Library.
    resistant_mean, good_deltaX, 3.0, xshift_mean, xshift_mean_error, num_rejectedX, GOODVEC=good_ind
    good_deltaX       = good_deltaX      [good_ind]
    good_deltaX_error = good_deltaX_error[good_ind]
    
    resistant_mean, good_deltaY, 3.0, yshift_mean, yshift_mean_error, num_rejectedY, GOODVEC=good_ind
    good_deltaY       = good_deltaY      [good_ind]
    good_deltaY_error = good_deltaY_error[good_ind]

    if ~quiet[jj] then print, xshift_mean, yshift_mean, offset_units, xshift_mean_error, yshift_mean_error, Nanalysis-num_rejectedX, Nanalysis-num_rejectedY, F='(%"  robust   mean: XSHIFT= %7.3f, YSHIFT= %7.3f %s; 1-sigma errors: %0.3f %0.3f ; # %d, %d matches ")'

    
    ; Estimate the astrometric shift by a weighted average of the match offsets that survived the sigma-clipping above.
    ; Wikipedia (http://en.wikipedia.org/wiki/Weighted_mean) says that the ML estimator corresponds to weighting each data point (in our case, an offset calculated from one match) by the inverse of its variance.
    ; Bevington concurs.
    weight                = 1. / good_deltaX_error^2                      ; Bevington 5-6
    weight_total          = total(/DOUBLE, weight) 
    xshift_weighted       = total(/DOUBLE, weight*good_deltaX) / weight_total
    xshift_weighted_error =                             sqrt(1 / weight_total) ; Bevington 5-9
    
    weight                = 1. / good_deltaY_error^2                      ; Bevington 5-6
    weight_total          = total(/DOUBLE, weight) 
    yshift_weighted       = total(/DOUBLE, weight*good_deltaY) / weight_total
    yshift_weighted_error =                             sqrt(1 / weight_total) ; Bevington 5-9
    
    if ~quiet[jj] then print, xshift_weighted, yshift_weighted, offset_units, xshift_weighted_error, yshift_weighted_error, Nanalysis-num_rejectedX, Nanalysis-num_rejectedY, F='(%"  weighted mean: XSHIFT= %7.3f, YSHIFT= %7.3f %s; 1-sigma errors: %0.3f %0.3f ; # %d, %d matches ")'
        
    
    if show_plot[jj] && keyword_set(custom1) then begin
      theta  = (composite_cat.acis.theta)
      dataset_2d, idr1, theta, radius, PSYM=1, DATASET=match_name, XTIT='theta [arcmin]', YTIT=radius_title
    endif
  endelse
endfor ;jj

return
end ; match_xy_analyze




;=============================================================================
; Estimate various performance rates of the matching algorithm via Monte Carlo simulations.
;
; This code is performing two separate MC simulations.

; 1. One simulation estimates the false-positive (and true-negative) rate under the assumption that NO actual counterpart exists.

; 2. The other simulation estimates the false-positive and false-negative (and true-positive) rates under the assumption that a counterpart (fake) does exist.
;
; See Appendix of Broos et al. 2007 paper on M17, and Broos et al. 2011 paper on Carina.
;
; The match_state input is the data structure created by a match_xy session already run.
; Since that session may include multiple slave catalogs, the cat_name input specifies which slave should be analyzed.
; Nsim is the number of Monte Carlo simulations desired.
;
; THE MASTER CATALOG MUST BE TRIMMED TO JUST INSIDE THE FIELD OF VIEW OF THE SLAVE CATALOG IN ORDER FOR THE STATISTICS CALCULATED HERE TO MAKE ANY SENSE!
;=============================================================================
PRO match_xy_simulate, match_state, cat_name, Nsim, _EXTRA=extra_params, results

COMMON match_xy_simulate, associated_correct_match, associated_incorrect_match, associated_false_negative, isolated_false_positive, isolated_true_negative, id1, id2

creator_string = "match_xy_simulate, version " +strmid("$Rev:: 4456  $",7,5) +strmid("$Date: 2013-04-11 09:34:57 -0400 (Thu, 11 Apr 2013) $", 6, 11)
print, creator_string, F='(%"\n\n%s")'
print, systime()

print, F='(%"\n\n=============================================================================")'
print, F='(%"SIMULATING MATCH OUTCOMES\n=============================================================================\n")'

match_xy_types, isolated_type, successful_primary_type, secondary_type, failed_primary_type, init_type

Nmaster     = n_elements(match_state.X)
master_name = match_state.cat_names[0]

;; Find this catalog in match_state.
cat_ind = (where(match_state.cat_names EQ cat_name, count))[0]
if (count EQ 0) then begin
  print, 'ERROR (match_xy_simulate): catalog not found.'
  return
endif

significance_threshold =   match_state.sig_thresholds[cat_ind]
cat_slave              = *(match_state.catalogs      [cat_ind])
match_primary          = *(match_state.match_primary [cat_ind])

num_matches = total(/INT, match_primary.type EQ successful_primary_type)


;; Pull out some vectors from match_state and cat_slave structures to speed up execution.
  cat_slave_ID =   cat_slave.ID
match_state_ID = match_state.ID


;; Create parent populations of X and Y position uncertainties to be used later for generating fake slave sources.
;; The set of slave sources participating in nominal matches seems like a reasonable sample from which to draw position uncertainties for fake sources. 
slave_was_matched = replicate(0B, n_elements(cat_slave))
for ii=0L,Nmaster-1 do begin
  if (match_primary[ii].type EQ successful_primary_type) then begin
    slave_was_matched[ where(cat_slave_ID EQ match_primary[ii].IDs) ] = 1B
  endif
endfor

X_err_population = cat_slave[where(slave_was_matched)].X_ERR
Y_err_population = cat_slave[where(slave_was_matched)].Y_ERR

;dataset_1d, id1, X_err_population
;dataset_1d, id1, Y_err_population


;; Choose a minimum catalog offset sufficient to randomize the phase between master and slave.
min_offset = 4 * (max(abs(match_primary.DELTAX)) > max(abs(match_primary.DELTAY)))

;; Generate random catalog offsets.
theta  = 2*!PI*random(Nsim)
radius = (min_offset + min_offset*random(Nsim))
;info, radius

xshift_slave = radius*sin(theta)
yshift_slave = radius*cos(theta)

dataset_2d, id2, xshift_slave, yshift_slave, PSYM=1, XTIT='random X offset (pixels)', YTIT='random Y offset (pixels)'


;; SIMULATION LOOP
associated_correct_match   = lonarr(Nsim)
associated_incorrect_match = lonarr(Nsim)
associated_false_negative  = lonarr(Nsim)
isolated_false_positive    = lonarr(Nsim)
isolated_true_negative     = lonarr(Nsim)

for ii=0L,Nsim-1 do begin
  ;=============================================================================
  ;; Simulate the "isolated population"---master sources for which no counterpart exists.
  ;; We simply shift one catalog to generate "random" positions within the other catalog,
  ;  run the matcher, and count "true negative" and "false positive" outcomes.
  sim_cat_name = 'isolated_'+cat_name
  match_xy, match_state, cat_slave, sim_cat_name, significance_threshold, $
	    XSHIFT_SLAVE=xshift_slave[ii], YSHIFT_SLAVE=yshift_slave[ii], /QUIET

  ;; Find this catalog in match_state.
  sim_cat_ind = (where(match_state.cat_names EQ sim_cat_name, count))[0]
  if (count EQ 0) then begin
    print, 'ERROR (match_xy_simulate): catalog not found.'
    return
  endif

  ;; Tablulate interesting classes of match results.
  bkg_false_positive= bytarr(Nmaster) ; Primary match is spurious (bkg source)
  bkg_true_negative = bytarr(Nmaster) ; ~bkg_false_positive

  match_primary = *(match_state.match_primary [sim_cat_ind])
  for jj=0L,Nmaster-1 do begin
    match = match_primary[jj]
    if (match.type EQ successful_primary_type) then bkg_false_positive[jj] = 1 $
                                               else bkg_true_negative [jj] = 1 ; No match found.
  endfor ;jj
  



  ;=============================================================================
  ;; Simulate the "associated population"---master sources for which a counterpart does exist.
  ;; We construct a fake slave source for each master, positioned with random offsets 
  ;;from the master appropriate for the declared position errors in both catalogs.
  
  ; Create a structure matching the slave catalog but with all fields nulled.
  null_slave_record = cat_slave[0]
  struct_assign, {zzz:0}, null_slave_record
  
  cat_fake = replicate(null_slave_record, Nmaster)
  
  ; Assign an ID for each fake slave entry that is not found in the real slave catalog and 
  ; that is easy to associate with the corresponding master source ID.  
  cat_fake.ID = -(match_state.ID)
  
  ; Generate random counterpart position errors similar to the real slave sources.
  N = n_elements(X_err_population)
  ind = (N-1) < floor(N*random(Nmaster))
  cat_fake.X_ERR = X_err_population[ind]
  cat_fake.Y_ERR = Y_err_population[ind]
  
  ; Generate random counterpart offsets from master positions.
  ; The offset random variables are Normal with variance equal to 
  ; the sum of the variances of master and slave (as assumed in match_xy routine).
  deltaX = sqrt(match_state.X_ERR^2 + cat_fake.X_ERR^2) * random(Nmaster, /NORMAL)
  deltaY = sqrt(match_state.Y_ERR^2 + cat_fake.Y_ERR^2) * random(Nmaster, /NORMAL)

  ; Their positions include the negative of the catalog offset so that
  ; they will end up matching the master positions.
  cat_fake.X = match_state.X - xshift_slave[ii] + deltaX
  cat_fake.Y = match_state.Y - Yshift_slave[ii] + deltaY
  
  
  ;; Perform the match using the slave catalog augmented with the fake sources generate above.
  ;; The total number of slave sources in this match will be slightly larger than in the original slave catalog, but
  ;; we expect this to be insignificant.
  sim_cat_name = 'associated_'+cat_name
  match_xy, match_state, [cat_slave,cat_fake], sim_cat_name, significance_threshold, $
	    XSHIFT_SLAVE=xshift_slave[ii], YSHIFT_SLAVE=yshift_slave[ii], /QUIET
	  
  ;; Find this catalog in match_state.
  sim_cat_ind = (where(match_state.cat_names EQ sim_cat_name, count))[0]
  if (count EQ 0) then begin
    print, 'ERROR (match_xy_simulate): catalog not found.'
    return
  endif

  ;; Tablulate interesting classes of match results.
  cp_correct_match  = bytarr(Nmaster)  ; Fake counterpart is primary match.
  cp_wrong_match    = bytarr(Nmaster)  ; Bkg source is primary match.
  cp_false_negative = bytarr(Nmaster)  ; No match found.

  match_primary = *(match_state.match_primary [sim_cat_ind])
  for jj=0L,Nmaster-1 do begin
    match = match_primary[jj]
    if (match.type EQ successful_primary_type) then begin
      ind_m = where(match_state_ID EQ match.IDm)
      if (match.IDm EQ -(match.IDs)) then begin
        ; Primary match was correct, i.e. to the fake counterpart.
        cp_correct_match[ind_m] = 1 
      endif else begin
        ; Primary match was not correct, i.e. to a background source.
        cp_wrong_match  [ind_m] = 1
      endelse
    endif else cp_false_negative[jj] = 1 ; No match found.
  endfor ;jj

 
  ; Test the flags for obvious inconsistencies.
  test = (bkg_false_positive AND bkg_true_negative)
  if (total(test) GT 0) then message, 'BUG!'

  test = (cp_correct_match AND cp_wrong_match) OR (cp_wrong_match AND cp_false_negative) OR (cp_false_negative AND cp_correct_match)
  if (total(test) GT 0) then message, 'BUG!'
  
  test = (cp_correct_match OR cp_wrong_match OR cp_false_negative) EQ 0
  if (total(test) GT 0) then message, 'BUG!'

  associated_correct_match  [ii] = total(cp_correct_match,  /INTEGER)
  associated_incorrect_match[ii] = total(cp_wrong_match,    /INTEGER)
  associated_false_negative [ii] = total(cp_false_negative, /INTEGER)
  isolated_false_positive   [ii] = total(bkg_false_positive,/INTEGER)
  isolated_true_negative    [ii] = total(bkg_true_negative ,/INTEGER)
; isolated_true_negative [ii] = Nmaster - isolated_false_positive[ii]
endfor ;ii sim loop


;; Test the tabulations for obvious problems.
if (total((associated_correct_match+associated_incorrect_match+associated_false_negative) NE Nmaster) NE 0) then message, 'BUG!'

;info, associated_correct_match
;info, associated_incorrect_match
;info, associated_false_negative
;info, isolated_true_negative
;info, isolated_false_positive

associated_correct_match   = mean(associated_correct_match  )
associated_incorrect_match = mean(associated_incorrect_match    )
associated_false_negative  = mean(associated_false_negative )
isolated_true_negative     = mean(isolated_true_negative )
isolated_false_positive    = mean(isolated_false_positive)


print,                             F='(%"\n\nOUTCOMES OF SIMULATIONS")'
print,                             F='(%"  Associated Population")'
print, associated_correct_match  , F='(%"    Correct Matches  : %7.1f")'  
print, associated_incorrect_match, F='(%"    Incorrect Matches: %7.1f")'  
print, associated_false_negative , F='(%"    False Negative   : %7.1f")'  
print,                             F='(%"                       ---------")'
print, Nmaster                   , F='(%"                       %5d")'
print,                             F='(%"  Isolated Population ")'  
print, isolated_true_negative    , F='(%"    True Negative    : %7.1f")'  
print, isolated_false_positive   , F='(%"    False Positive   : %7.1f")'  
print,                             F='(%"                       ---------")'
print, Nmaster                   , F='(%"                       %5d")'


;=============================================================================
;; Estimate the fraction of master sources that have true associations---a critical parameter for correctly estimating the frequency of the 5 possible match outcomes.

; See Appendix of Broos et al. 2007 paper on M17, and Broos et al. 2011 paper on Carina.
;  # unmatched            = "false negatives" for associated population + "true negatives" for isolated population
; (Nmaster - num_matches) = associated_fraction*associated_false_negative + (1-associated_fraction)*isolated_true_negative

associated_fraction =(((Nmaster - num_matches)   - isolated_true_negative) /  $
                      (associated_false_negative - isolated_true_negative)  ) > 0.0

; Display and save the outcomes frequences, expressed as percentages.
associated_correct_match   = 100./Nmaster*(  associated_fraction)*associated_correct_match  
associated_incorrect_match = 100./Nmaster*(  associated_fraction)*associated_incorrect_match
associated_false_negative  = 100./Nmaster*(  associated_fraction)*associated_false_negative 
isolated_true_negative     = 100./Nmaster*(1-associated_fraction)*isolated_true_negative 
isolated_false_positive    = 100./Nmaster*(1-associated_fraction)*isolated_false_positive

print,                                     F='(%"\nPREDICTED OUTCOMES FOR OBSERVATIONS")'
print, round(100*(  associated_fraction)), F='(%"  Associated Population      (%2d%%)")'
print, round(associated_correct_match   ), F='(%"    Correct Matches    : %2d%%")'  
print, round(associated_incorrect_match ), F='(%"    Incorrect Matches  : %2d%%")'  
print, round(associated_false_negative  ), F='(%"    False Negative     : %2d%%")'  
print, round(100*(1-associated_fraction)), F='(%"  Isolated Population        (%2d%%)")'  
print, round(isolated_true_negative     ), F='(%"    True Negative      : %2d%%")'  
print, round(isolated_false_positive    ), F='(%"    False Positive     : %2d%%")'  
print,                                     F='(%"                       ---------")'
print,                                     F='(%"                        100%%\n\n\n")'
                    
results = { cat_name                  :cat_name                  ,$
            Nmaster                   :Nmaster                   ,$
            Nsim                      :Nsim                      ,$
            significance_threshold    :significance_threshold    ,$
            associated_fraction       :associated_fraction       ,$
            associated_correct_match  :associated_correct_match  ,$
            associated_incorrect_match:associated_incorrect_match,$
            associated_false_negative :associated_false_negative ,$
            isolated_true_negative    :isolated_true_negative    ,$
            isolated_false_positive   :isolated_false_positive   $
          }
save, results, FILE='match_xy_simulate.sav'
return
end  ; match_xy_simulate


;=============================================================================

; Smaller values of SIGNIFICANCE_THRESHOLD force matches to be more certain.

; An initial rough offset for observation_cat can be specified via INITIAL_OBS_XSHIFT.,INITIAL_OBS_YSHIFT.
; This is helpful in cases where the two catalogs are initially far out of alignment.

; The final recommended offset for observation_cat is returned in OBS_XSHIFT, OBS_YSHIFT

;=============================================================================
PRO match_xy_estimate_offset, observation_cat, OBS_NAME=obs_name, $
	                            INITIAL_OBS_XSHIFT=initial_obs_xshift, INITIAL_OBS_YSHIFT=initial_obs_yshift, $
	            
	                            reference_cat  , REF_NAME=ref_name, SIGNIFICANCE_THRESHOLD=significance_threshold, $
              
                 	                  OBS_XSHIFT=obs_xshift      ,       OBS_YSHIFT=obs_yshift, $
                 	            ERROR_OBS_XSHIFT=obs_xshift_error, ERROR_OBS_YSHIFT=obs_yshift_error,$
                 	            ASTROMETRY=astrometry, _EXTRA=extra_params

creator_string = "match_xy_estimate_offset, version " +strmid("$Rev:: 4456  $",7,5) +strmid("$Date: 2013-04-11 09:34:57 -0400 (Thu, 11 Apr 2013) $", 6, 11)
print, creator_string, F='(%"\n\n%s")'
print, systime()

if ~keyword_set(obs_name) then obs_name = 'OBS'
if ~keyword_set(ref_name) then ref_name = 'REF'

if (n_elements(initial_obs_xshift) EQ 1) then initial_obs_xshift=initial_obs_xshift[0] else initial_obs_xshift=0.
if (n_elements(initial_obs_yshift) EQ 1) then initial_obs_yshift=initial_obs_yshift[0] else initial_obs_yshift=0.

f_nan            = !VALUES.F_NAN
obs_xshift       = f_nan
obs_yshift       = f_nan
obs_xshift_error = f_nan
obs_yshift_error = f_nan

xshift_total = 0.0
yshift_total = 0.0

; For this astrometric work, we want high-quality matches rather than numerous matches.
if ~keyword_set(significance_threshold) then significance_threshold=0.68

; Peform an initial match.  We will (confusingly) define the observation catalog to be the 
; "master" in this run, even though it's the one with the wrong astrometry, as a
; trick to improve runtime since the observation will typically have fewer sources
; than the reference catalog.  We'll make sure to keep this straight
; when applying the offsets later.
match_xy, hyper_cat, observation_cat, obs_name, /QUIET, /INIT, XSHIFT=initial_obs_xshift, YSHIFT=initial_obs_yshift
match_xy, hyper_cat, reference_cat  , ref_name, /QUIET, significance_threshold


; Construct a composite catalog, obs_ref, containing all the observed sources
; plus information from the matching reference sources.
;print, F="(%'\n========================\nANALYSIS OF ALL MATCHES:')"
match_xy_analyze, hyper_cat, obs_ref, ARCSEC_PER_PIXEL=0, /QUIET, PLOT=0, /SKIP_REGIONS, _EXTRA=extra_params 

; Analyze the matches in order to recommend a shift to be applied to the slave
; catalog, which is confusingly the reference catalog here!
; The parameter ANALYSIS_MASK should be 1 (true) for high-quality sources that 
; should be included in the offset analysis.
;print, F="(%'\n=============================\nANALYSIS OF FIDUCIAL MATCHES:')"
match_xy_analyze, hyper_cat, obs_ref, ARCSEC_PER_PIXEL=0, ARROW_PLOT=1, isolated_flag, _EXTRA=extra_params, $
  ANALYSIS_MASK=((obs_ref.(5).NOT_FIDUCIAL EQ 0) AND (obs_ref.(6).NOT_FIDUCIAL EQ 0)), $
  XSHIFT_WEIGHTED=xshift_extra, YSHIFT_WEIGHTED=yshift_extra, ERROR_XSHIFT_WEIGHTED=xshift_weighted_error, ERROR_YSHIFT_WEIGHTED=yshift_weighted_error

if ~finite(xshift_weighted_error) || ~finite(yshift_weighted_error) then return

xshift_total += xshift_extra
yshift_total += yshift_extra

if (abs(xshift_extra) GT 0.01) || (abs(yshift_extra) GT 0.01) then begin
  ; Repeat the matching with those offsets supplied via the XSHIFT/YSHIFT
  ; parameters. The matcher will ADD those offsets to the slave catalog before
  ; performing the match. The number of matches may change a little, especially if
  ; the offsets between the two catalogs were large.
  match_xy, hyper_cat, reference_cat  , ref_name, /QUIET, significance_threshold, XSHIFT=xshift_total, YSHIFT=yshift_total
  
  ; Re-analyze the offsets of the matched high-quality sources to determine how much more shifting is required.
  ;print, F="(%'\n========================\nANALYSIS OF ALL MATCHES:')"
  match_xy_analyze, hyper_cat, obs_ref, ARCSEC_PER_PIXEL=0, /QUIET, PLOT=0, /SKIP_REGIONS, _EXTRA=extra_params 
  
  print, F="(%'\n=============================\nANALYSIS OF FIDUCIAL MATCHES:')"
  match_xy_analyze, hyper_cat, obs_ref, ARCSEC_PER_PIXEL=0, ARROW_PLOT=1, isolated_flag, _EXTRA=extra_params, $
    ANALYSIS_MASK=((obs_ref.(5).NOT_FIDUCIAL EQ 0) AND (obs_ref.(6).NOT_FIDUCIAL EQ 0)), $
    XSHIFT_WEIGHTED=xshift_extra, YSHIFT_WEIGHTED=yshift_extra, ERROR_XSHIFT_WEIGHTED=xshift_weighted_error, ERROR_YSHIFT_WEIGHTED=yshift_weighted_error
  
  if ~finite(xshift_weighted_error) || ~finite(yshift_weighted_error) then return
  
  xshift_total += xshift_extra
  yshift_total += yshift_extra


  if (abs(xshift_extra) GT 0.01) || (abs(yshift_extra) GT 0.01) then begin
    ; Repeat the matching with the revised shifts, and analyze one last time to report how much residual shift remains.
    match_xy, hyper_cat, reference_cat  , ref_name, /QUIET, significance_threshold, XSHIFT=xshift_total, YSHIFT=yshift_total
    ;print, F="(%'\n========================\nANALYSIS OF ALL MATCHES:')"
    
    match_xy_analyze, hyper_cat, obs_ref, ARCSEC_PER_PIXEL=0, /QUIET, PLOT=0, /SKIP_REGIONS, _EXTRA=extra_params
    print, F="(%'\n=============================\nANALYSIS OF FIDUCIAL MATCHES:')"
    match_xy_analyze, hyper_cat, obs_ref, ARCSEC_PER_PIXEL=0, ARROW_PLOT=1, isolated_flag, _EXTRA=extra_params, $
      ANALYSIS_MASK=((obs_ref.(5).NOT_FIDUCIAL EQ 0) AND (obs_ref.(6).NOT_FIDUCIAL EQ 0)), $
      ERROR_XSHIFT_WEIGHTED=xshift_weighted_error, ERROR_YSHIFT_WEIGHTED=yshift_weighted_error

    if ~finite(xshift_weighted_error) || ~finite(yshift_weighted_error) then return
  endif
endif

if keyword_set(astrometry) then begin
  ; Build a region file in celestial coordinates.
    match_xy_analyze, hyper_cat, obs_ref, ARCSEC_PER_PIXEL=0, ARROW_PLOT=0, isolated_flag, _EXTRA=extra_params, $
      ANALYSIS_MASK=((obs_ref.(5).NOT_FIDUCIAL EQ 0) AND (obs_ref.(6).NOT_FIDUCIAL EQ 0)), ASTROMETRY=astrometry
endif

; Report the brightest unmatched sources.
if tag_exist(/TOP_LEVEL, obs_ref.(5),'NET_CNTS') then begin
  ind = where(isolated_flag AND (obs_ref.(5).NOT_FIDUCIAL EQ 0), count)
  if (count GT 0) then begin
    print, F='(%"\nBelow are the brightest unmatched sources.\n  ID  NET_CNTS     (X,Y)")'
    sort_ind = reverse(ind[sort((obs_ref.(5).NET_CNTS)[ind])])
    forprint, SUBSET=sort_ind[0:(20<count)-1], obs_ref.(5).ID, obs_ref.(5).NET_CNTS, round(obs_ref.(5).X), round(obs_ref.(5).Y), F='(%"%4d   %4d  (%d,%d) ")'
  endif
endif

; Save the session.
save, FILE='obs_ref.sav'
print, F='(%"Match data structures saved to obs_ref.sav.")'

;; Report the actual shift that should be supplied to the CIAO tool wcs_update.
obs_xshift       = initial_obs_xshift - xshift_total
obs_yshift       = initial_obs_yshift - yshift_total
obs_xshift_error = xshift_weighted_error
obs_yshift_error = yshift_weighted_error


; Figure out the name of the actual parameter corresponding to the formal parameter "observation_cat".
observation_cat_name = strlowcase((routine_names(observation_cat, ARG_NAME=(-1)))[0])
if (observation_cat_name EQ '') then observation_cat_name = 'the first catalog'


print, observation_cat_name, obs_xshift, xshift_weighted_error, obs_yshift, yshift_weighted_error, F='(%"\n\nAn analysis of the offsets between matching sources suggests that ''%s'' should be shifted by \n    DELTAX= %0.3f (+-%0.3f), DELTAY= %0.3f (+-%0.3f) pixels.\n")'
print, 'Our estimate of the residual offset that will remain is shown in the summary above.'
return
end  ; match_xy_estimate_offset



;=============================================================================
; Estimate the offset among a set of catalogs, displaying results in a matrix.
;
; This is similar to the tool ae_interObsID_astrometry

; catalog_ptr is an array of pointers to a set of match_xy catalogs that are defined in the same coordinate system
;=============================================================================


PRO match_xy_offset_matrix, cat_name, cat_comment, catalog_ptr, REF_CATALOG=ref_cat, $
                            ASTROMETRY=astrometry, SIGNIFICANCE_THRESHOLD=significance_threshold, $
                            obs_obs_xshift, obs_obs_yshift, obs_obs_xshift_error, obs_obs_yshift_error, $
                            obs_ref_xshift, obs_ref_yshift, obs_ref_xshift_error, obs_ref_yshift_error

; Using a moderate match criterion by default.
if ~keyword_set(significance_threshold) then significance_threshold=0.90

num_cats = n_elements(cat_name)


;=============================================================================
ref_cat_supplied = keyword_set(ref_cat)
if ref_cat_supplied then begin
  refname = (routine_names(ref_cat, ARG_NAME=(-1)))[0]
  
  obs_ref_xshift       = replicate(!VALUES.F_NAN, num_cats)
  obs_ref_yshift       = replicate(!VALUES.F_NAN, num_cats)
  
  obs_ref_xshift_error = replicate(!VALUES.F_NAN, num_cats)
  obs_ref_yshift_error = replicate(!VALUES.F_NAN, num_cats)
  
  for ii = 0,num_cats-1 do begin
    print, cat_name[ii], refname, F="(%'\n\n\n======================== ALIGNMENT BETWEEN %s and %s  ========================\n')"
    
    ; Evaluate the alignment of the catalogs,.
    match_xy_estimate_offset, *(catalog_ptr[ii]), OBS_NAME=cat_name[ii], $
                                         ref_cat, REF_NAME=refname, SIGNIFICANCE_THRESHOLD=significance_threshold, $
            OBS_XSHIFT=this_xshift      ,       OBS_YSHIFT=this_yshift, $
      ERROR_OBS_XSHIFT=this_xshift_error, ERROR_OBS_YSHIFT=this_yshift_error, ARROW_PLOT=0, ASTROMETRY=astrometry
      
    obs_ref_xshift      [ii] = this_xshift
    obs_ref_yshift      [ii] = this_yshift
    
    obs_ref_xshift_error[ii] = this_xshift_error
    obs_ref_yshift_error[ii] = this_yshift_error
    
    file_move, 'obs_ref.reg', strlowcase(refname)+'_'+cat_name[ii]+'.reg', /OVERWRITE
  endfor ; ii
endif ; reference catalog supplied


;=============================================================================
;; Define matrices to hold astrometric offset estimates between pairs of ObsIDs.
obs_obs_xshift       = replicate(!VALUES.F_NAN, num_cats, num_cats)
obs_obs_yshift       = replicate(!VALUES.F_NAN, num_cats, num_cats)

obs_obs_xshift_error = replicate(!VALUES.F_NAN, num_cats, num_cats)
obs_obs_yshift_error = replicate(!VALUES.F_NAN, num_cats, num_cats)

print, F="(%'\n\n\n======================== ALIGNMENT BETWEEN PAIRS OF ObsIDs  ========================\n')"

print, F="(%'\Estimates for the astrometric offset between each pair of ObsIDs are reported below.')"

exposure = fltarr(num_cats)

for ii = 0,num_cats-1 do begin
  for jj = ii+1, num_cats-1 do begin
    ; Evaluate the alignment of the catalogs.
    ; Compute the offset FROM ObsJJ TO ObsII.
    match_xy_estimate_offset, *(catalog_ptr[jj]), REF_NAME=cat_name[jj], $
                              *(catalog_ptr[ii]), OBS_NAME=cat_name[ii], SIGNIFICANCE_THRESHOLD=significance_threshold, $
            OBS_XSHIFT=this_xshift      ,       OBS_YSHIFT=this_yshift, $
      ERROR_OBS_XSHIFT=this_xshift_error, ERROR_OBS_YSHIFT=this_yshift_error, ARROW_PLOT=0
     
    obs_obs_xshift      [ii,jj] =  this_xshift
    obs_obs_xshift      [jj,ii] = -this_xshift
    obs_obs_xshift_error[jj,ii] =  this_xshift_error
    obs_obs_xshift_error[ii,jj] =  this_xshift_error
                                        
    obs_obs_yshift      [ii,jj] =  this_yshift
    obs_obs_yshift      [jj,ii] = -this_yshift
    obs_obs_yshift_error[ii,jj] =  this_yshift_error
    obs_obs_yshift_error[jj,ii] =  this_yshift_error
  endfor ; jj
endfor ; ii



;=============================================================================
;; Build a report.
if ref_cat_supplied then begin
  ; Plot direct estimates of shift wrt reference cat, and two-hop estimates.
  for ii = 0,num_cats-1 do begin
  id = 0L
  function_1d, id, PSYM=7, DATASET=cat_name[ii]+'->'+refname, $
           [obs_ref_xshift      [ii]], $
           [obs_ref_yshift      [ii]], $
    X_ERROR=obs_ref_xshift_error[ii], $
    Y_ERROR=obs_ref_yshift_error[ii], TITLE=cat_name[ii], XTIT='XSHIFT (skypix)', YTIT='YSHIFT (skypix)', PLOT_WINDOW_OPTIONS='/FORCE_UNITY_ASPECT'
    
  for jj = 0,num_cats-1 do begin
      if (jj EQ ii) then continue
      ; Two-hop offsets are added; uncertainties are added in quadrature.
      function_1d, id, PSYM=6, DATASET=cat_name[ii]+'->'+cat_name[jj]+'->'+refname, $
                     [obs_obs_xshift     [jj,ii]   + obs_ref_xshift      [jj]]  , $
                     [obs_obs_yshift     [jj,ii]   + obs_ref_yshift      [jj]]  , $
        X_ERROR=sqrt(obs_obs_xshift_error[jj,ii]^2 + obs_ref_xshift_error[jj]^2), $
        Y_ERROR=sqrt(obs_obs_yshift_error[jj,ii]^2 + obs_ref_yshift_error[jj]^2)
    endfor ; jj
  endfor; ii


  obs_ref_xshift_report = string(obs_ref_xshift, F='(%"%7.3f")') + string(abs(obs_ref_xshift)/obs_ref_xshift_error, F='(%" S=%-4.1f")') 
  obs_ref_yshift_report = string(obs_ref_yshift, F='(%"%7.3f")') + string(abs(obs_ref_yshift)/obs_ref_yshift_error, F='(%" S=%-4.1f")') 
  refname_formatted     = string(refname, F='(%"     %s")')
endif else begin
  obs_ref_xshift_report = strarr(num_cats)
  obs_ref_yshift_report = strarr(num_cats)
  refname_formatted     = ''
endelse


obs_obs_xshift_report = reform(string(obs_obs_xshift, F='(%"%7.3f")') + string(abs(obs_obs_xshift)/obs_obs_xshift_error, F='(%" S=%-4.1f")'), num_cats, num_cats)
obs_obs_yshift_report = reform(string(obs_obs_yshift, F='(%"%7.3f")') + string(abs(obs_obs_yshift)/obs_obs_yshift_error, F='(%" S=%-4.1f")'), num_cats, num_cats)

cat_comment_formatted = string(cat_comment, F='(%"%14s")')
cat_name_formatted    = string(cat_name   , F='(%"%14s")')
                                                                                    
; Remove the entries where no estimate was made.
ind = where( strmatch(obs_obs_xshift_report      , '*NaN S=NaN*'), count)
if (count GT 0) then  obs_obs_xshift_report[ind] = '        -     '
ind = where( strmatch(obs_obs_yshift_report      , '*NaN S=NaN*'), count)
if (count GT 0) then  obs_obs_yshift_report[ind] = '        -     '

print, F="(%'\n\n\n======================== ALIGNMENT SUMMARY ========================')"
print, F='(%"\nXSHIFTS [skypix]")'
for jj = 0,num_cats-1 do print, 'shift ', cat_name_formatted[jj], ' by ', obs_obs_xshift_report[*,jj], obs_ref_xshift_report[jj], F='(50A)'
print, 'to align with       ',cat_name_formatted, refname_formatted, F='(50A)'
print, '                    ',cat_comment_formatted                , F='(50A)'

print, F='(%"\nYSHIFTS [skypix]")'
for jj = 0,num_cats-1 do print, 'shift ', cat_name_formatted[jj], ' by ', obs_obs_yshift_report[*,jj], obs_ref_yshift_report[jj], F='(50A)'
print, 'to align with       ',cat_name_formatted, refname_formatted, F='(50A)'
print, '                    ',cat_comment_formatted                , F='(50A)'
print


return
end ; match_xy_offset_matrix




PRO test2, significance_threshold

dim=100
x=10*indgen(dim)
y=x
make_2d,x,y
Nentries = n_elements(x)
x=reform(x,Nentries)
y=reform(y,Nentries)

entry = {ID:0L, X:0., Y:0., X_ERR:0., Y_ERR:0. }
cat = replicate(entry,Nentries)
cat.ID = 1+indgen(Nentries)

X_ERR = 0.01
Y_ERR = 0.05
cat.X  = x + X_ERR*random(Nentries, /NORMAL)
cat.Y  = y + Y_ERR*random(Nentries, /NORMAL)
cat.X_ERR = X_ERR
cat.Y_ERR = Y_ERR

match_xy, match_state, cat, 'one', /INIT

X_ERR = 0.5
Y_ERR = 0.1
cat.X  = x + X_ERR*random(Nentries, /NORMAL)
cat.Y  = y + Y_ERR*random(Nentries, /NORMAL)
cat.X_ERR = X_ERR
cat.Y_ERR = Y_ERR

match_xy, match_state, cat, 'two', significance_threshold

 
return
end



PRO test1, cat1, cat2, match_primary, match_secondary

record = {source, ID:0L, X:0.0, Y:0.0, X_ERR:1.0, Y_ERR:1.0}
cat2 = replicate({source},12)
x=indgen(4)
y=indgen(3)
make_2d,x,y
cat2.x=reform(x+1,12)
cat2.y=reform(y+1,12)
cat2.id=indgen(12)
cat2.X_ERR=0.5
cat2.Y_ERR=1

plot,cat2.x,cat2.y,line=0,psym=4,xrange=[0,4],yrange=[0,4]

cat1 = replicate({source},3)
cat1.x=[2.5,2.5,2.5]
cat1.y=[1.5,2,2.5]
cat1.id=indgen(3)
cat1.X_ERR=1
cat1.Y_ERR=0.5
oplot,cat1.x,cat1.y,line=0,psym=2

significance_threshold=0.1
match_xy,         cat1, cat2, significance_threshold, match_primary, match_secondary

match_xy_analyze, cat1, cat2, significance_threshold, match_primary, match_secondary
return
end





;=============================================================================
; Interactive Editing of Catalogs
;
; It's often useful to be able to prune catalogs interactively, e.g. while displayed
; on top of data in ds9.  
; The two routines below allow you to represent a catalog as a ds9 region file, 
; prune it in ds9, then read it back into IDL and apply that pruning to the 
; actual catalog.

; You are  ONLY allowed to DELETE regions in ds9.  You may not add or move regions!
;
; At the end of each region the string IDTAG is written, followed by the ID of that source so that it can be later found within the catalog.
;
;=============================================================================
PRO catalog_ds9_interface, cat, reg_filename, $
                           WRITE_REGFILE=write_regfile, OMIT_LABELS=omit_labels, COLOR_LIST=color_list, $
                           PRUNE_CATALOG=prune_catalog

catalog_name_available = tag_exist(/TOP_LEVEL, cat,'CATALOG_NAME') 
label_available        = tag_exist(/TOP_LEVEL, cat,'LABEL') 
fiducial_available     = tag_exist(/TOP_LEVEL, cat,'NOT_FIDUCIAL') 

if keyword_set(write_regfile) then begin
  
  openw, regunit1, reg_filename, /GET_LUN
  printf, regunit1, "# Region file format: DS9 version 3.0"
  printf, regunit1, 'global move=0 width=1 font="helvetica 12 normal"'

  Nentries = n_elements(cat)
  is_fits_region = tag_exist(/TOP_LEVEL, cat,'SHAPE') && tag_exist(/TOP_LEVEL, cat,'R') && tag_exist(/TOP_LEVEL, cat,'ROTANG') 
  
  if catalog_name_available then begin
    catalog_name = strtrim(cat.CATALOG_NAME,2)
    catalog_tag  = 'tag={'+catalog_name+'}'
    
    ; Try to assign different colors to each catalog name, based on frequency of the name.
    if ~keyword_set(color_list) then $
      color_list =  ['red','green','cyan','magenta','yellow','DodgerBlue','Cornsilk','Goldenrod','Chocolate','DarkSalmon', 'Tan','Peru','Sienna','Salmon', 'SandyBrown','DarkGoldenrod','Brown','IndianRed','SlateBlue']

    num_colors = n_elements(color_list)
    colors     = replicate(color_list[num_colors-1],Nentries)
  
    name_list  = catalog_name[uniq(catalog_name, sort(catalog_name))]
    num_names  = n_elements(name_list)
    name_count = lonarr(num_names)
    for ii=0, num_names-1 do begin
      name_count[ii] = total(/INT, strmatch(catalog_name, name_list[ii]))
    endfor
    name_list = name_list[reverse(sort(name_count))]
    print,  F="(%'\n    COLOR  CATALOG\n    --------------')"
    for ii=0, num_names-1 do begin
      this_color = color_list[ii < (num_colors-1)]
      colors[where(strmatch(catalog_name, name_list[ii]))] = this_color
      print, this_color, name_list[ii], F="(%'%9s  %s')"
    endfor

  endif else begin
    colors      = replicate('green',Nentries)
    catalog_tag = replicate('',Nentries)
  endelse ; ~catalog_name_available
  
  
  fiducial_tag = replicate('',Nentries)
  if fiducial_available then begin
    ind = where(cat.NOT_FIDUCIAL, count)
    if (count GT 0) then fiducial_tag[ind] = 'tag={not fiducial}'
  endif 
  

  if keyword_set(omit_labels) then label=strarr(Nentries) $
  else begin
    if label_available then label = string(strtrim(cat.LABEL,2),  F='(%" text={%s} ")') $
    else                    label = string(        cat.ID      ,  F='(%" text={%d} ")')
  endelse
  
  for ii=0L,Nentries-1 do begin
    s=cat[ii]
        
    if is_fits_region then begin
      ; Silly wavdetect can have ellipse radii that are NaN or zero, which can be invisible in ds9!
      ; The max() calls below take care of both.
      printf, regunit1, s.x, s.y, max([0.5, s.r[0]]), max([0.5, s.r[1]]), s.rotang, catalog_tag[ii], fiducial_tag[ii], colors[ii], label[ii], s.ID, $
        F='(%"ellipse %f %f %f %f %f # %s %s color=%s %s IDTAG%d")'
    endif else begin
      printf, regunit1, s.x, s.y, catalog_tag[ii], fiducial_tag[ii], colors[ii], label[ii], s.ID, $
        F='(%"cross point %f %f # %s %s color=%s %s IDTAG%d")'
    endelse
  endfor ;ii
  
  free_lun, regunit1
  print, 'Regions written to '+reg_filename
endif ; keyword_set(write_regfile)


if keyword_set(prune_catalog) then begin
  ; Read the lines from the region file.
  num_lines = file_lines(reg_filename)
  lines = strarr(num_lines)  
  openr, regunit1, reg_filename, /GET_LUN
  readf, regunit1, lines
  free_lun, regunit1
  
  ; Find the lines with and without IDTAG fields.
  retain_ID = reform( (stregex(lines,'IDTAG([0-9]+)',/SUB,/EXT))[1,*] )
  
  region_ind = where(retain_ID NE '', COMPLEMENT=ignored_ind, NCOMPLEMENT=num_ignored)
  
  retain_ID = long(retain_ID[region_ind])
  
  ; Report any interesting lines in the region file that we're ignoring.
  if (num_ignored GT 0) then begin
    lines = lines[ignored_ind]
    ind = where(stregex(/BOOLEAN, lines, '(^#)|(^global)|(^physical)') EQ 0, count)
    if (count GT 0) then begin
      print, count, F="(%'\nWARNING!  The following %d lines in the region file were ignored.\nREMEMBER that you are not allowed to ADD detections via this interface!')"
      forprint, lines, SUBSET=ind
      print, '-----------------------------------------------------------------------------'
    endif
  endif
  
  
  Nentries = n_elements(cat)
  retain_row = bytarr(Nentries)
  cat_ID = cat.ID
  for ii=0,n_elements(retain_ID)-1 do begin
    ind = where(cat_ID EQ retain_ID[ii], count)
    if (count EQ 0) then begin
      print, 'ERROR: region file contained ID value not found in catalog:', retain_ID[ii]
      retall
    endif
    
    if (count GT 1) then begin
      print, 'ERROR: catalog has duplicated ID value: ', retain_ID[ii]
      retall
    endif
    
    if retain_row[ind] then begin
      print, 'WARNING: region file has duplicated ID value: ', retain_ID[ii]
    endif

    retain_row[ind] = 1
  endfor ;ii
    
  retain_ind = where(retain_row, COMPLEMENT=prune_ind, NCOMPLEMENT=prune_count)
  print, prune_count, F='(%"Pruned %d sources:")'
  if (prune_count GT 0) then begin
    if catalog_name_available then catalog_name = strtrim(cat.CATALOG_NAME,2) $
    else                           catalog_name = replicate('', Nentries)
  
    if label_available then label = string(strtrim(cat.LABEL,2),  F='(%" label: %s ")') $
    else                    label = string(        cat.ID      ,  F='(%" ID: %d ")')
  
    forprint, SUBSET=prune_ind, label, catalog_name
  
    prune_catalog = cat[ prune_ind]
    cat           = cat[retain_ind]
  endif
  
endif ; keyword_set(prune_catalog)

return
end ; catalog_ds9_interface


;=============================================================================
; Function to build an AstroLib "astrometry structure" relating WCS and Chandra SKY coordinates,
; as defined by the "X" and "Y" columns of an ACIS event list.


;=============================================================================
FUNCTION get_astrometry_from_eventlist, eventlist_fn

;; Read event file header to define ACIS sky coordinate system.
theader = headfits(eventlist_fn, EXT=1, ERRMSG=error )
if (keyword_set(error)) then message, 'ERROR reading ' + eventlist_fn

; Build astrometic structure from data header.
    fxbfind, theader, 'TTYPE', dum1, TTYPE, dum2, 'null'
    fxbfind, theader, 'TCTYP', dum1, TCTYP, dum2, 'null'
    fxbfind, theader, 'TCRVL', dum1, TCRVL, dum2, 0.0D
    fxbfind, theader, 'TCRPX', dum1, TCRPX, dum2, 0.0D
    fxbfind, theader, 'TCDLT', dum1, TCDLT, dum2, 0.0D
    colnames = strlowcase( strtrim(TTYPE,2) )
    x_ind    = where(strlowcase(colnames) EQ 'x')
    y_ind    = where(strlowcase(colnames) EQ 'y')

    ;; The xy2ad.pro and ad2xy.pro programs we are using in the match_xy package to convert between celestial and "X/Y" coordinates think of the X/Y coordinates as 0-based image array indexes.
    ;; However, when a Chandra event list is defining the tangent plane, we would like the X/Y coordinates in match_xy to correspond to the PHYSICAL "SKY" system defined by Chandra.
    ;; The requires adding one to the CRPIX astrometry keywords below.

    make_astr, event2wcs_astr, DELTA=  TCDLT[[x_ind,y_ind]], CTYPE=TCTYP[[x_ind,y_ind]], $
                               CRPIX=1+TCRPX[[x_ind,y_ind]], CRVAL=TCRVL[[x_ind,y_ind]]

return, event2wcs_astr
end




;=============================================================================
; Tool to reproject an existing match_xy catalog onto a new tangent plane.

; The existing catalog must have RA and DEC columns.

; The event2wcs_astr input provides a definition for the new sky coordinate system.
;=============================================================================

PRO match_xy_reproject_cat, cat, event2wcs_astr

ad2xy, cat.RA, cat.DEC, event2wcs_astr, x, y 
cat.X  = X
cat.Y  = Y

return
end


;=============================================================================
; Function to build a catalog from vectors (id, ra, dec, ra_err, dec_err).

; Input id is a unique long integer identifier for each source, or a scalar 0 to generate sequence numbers.

; Inputs ra_err, dec_err are 1-sigma single-axis position uncertainties, in arcseconds.

; The event2wcs_astr input provides a definition for the sky coordinate system.
;=============================================================================
FUNCTION build_generic_cat, ra, dec, ra_err, dec_err, event2wcs_astr, ID=id, LABEL=label, NAME=cat_name

Nentries = n_elements(ra)
if ~keyword_set(id) then id = 1+lindgen(Nentries)

if ~keyword_set(label) then label = '# '+strtrim(id,2)

;; Convert celestial to the ACIS tangent plane.
ad2xy, ra, dec, event2wcs_astr, x, y  

;; Assign position errors in the tangent plane coordinates.
arcsec_per_pixel = abs(event2wcs_astr.CDELT[0] * event2wcs_astr.CD[0,0])*3600.0
X_ERR =  ra_err / arcsec_per_pixel
Y_ERR = dec_err / arcsec_per_pixel

not_fiducial = bytarr(Nentries)

CATALOG_NAME = keyword_set(cat_name) ? cat_name : 'generic_cat' 

;; Build a suitable structure.
tag_names   = ['ID','LABEL','X','Y','X_ERR','Y_ERR','RA','DEC','NOT_FIDUCIAL','CATALOG_NAME']
tag_formats = 'J,A,F,F,F,F,D,D,B,A'                 

; "L" types in readcol must be "J" types in create_struct!
create_struct, cat, '', tag_names, repchr(tag_formats,'L','J'), DIMEN=Nentries

;; Populate the structure.
cmd = 'cat.'+tag_names+'='+tag_names

for ii=0L,n_tags(cat)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor

help, cat, /st
print, Nentries, ' sources created.'
return, cat
end


;=============================================================================
; Function to convert an array of IDL structures to a match_xy catalog.

; Supply the catalog name via the NAME input---to be used in ds9 region tags.

; Supply strings containing IDL expressions for the position errors, in units of arcseconds, via RA_ERROR_EXPRESSION and DEC_ERROR_EXPRESSION.  In those expressions, the catalog structure array is named "structure".

; Supply strings containing IDL expressions for the RA and DEC coordinates, in units of degrees, via RA_EXPRESSION and DEC_EXPRESSION.  In those expressions, the catalog structure array is named "structure".  
; The default is structure.RA and structure.DEC.

; The event2wcs_astr input provides a definition for the sky coordinate system.
;=============================================================================
FUNCTION build_structure_cat, structure, event2wcs_astr, $
           RA_EXPRESSION      =ra_expression      , DEC_EXPRESSION      =dec_expression, $
           RA_ERROR_EXPRESSION=ra_error_expression, DEC_ERROR_EXPRESSION=dec_error_expression, LABEL=label, NAME=cat_name


Nentries = n_elements(structure)

; Find celestial coordinates in the table.
if ~keyword_set( ra_expression) then  ra_expression = 'structure.RA'
if ~keyword_set(dec_expression) then dec_expression = 'structure.DEC'
if NOT execute('RA =double('+ ra_expression+')') then message,  'RA_EXPRESSION cannot be evaluated.'
if NOT execute('DEC=double('+dec_expression+')') then message, 'DEC_EXPRESSION cannot be evaluated.'

;; Convert celestial positions to the ACIS tangent plane.
ad2xy, ra, dec, event2wcs_astr, x, y 

;; Assign position errors in the tangent plane coordinates.
if NOT execute('X_ERR='+ ra_error_expression) then message,  'RA_ERROR_EXPRESSION cannot be evaluated.'
if NOT execute('Y_ERR='+dec_error_expression) then message, 'DEC_ERROR_EXPRESSION cannot be evaluated.'

arcsec_per_pixel = abs(event2wcs_astr.CDELT[0] * event2wcs_astr.CD[0,0])*3600.0
X_ERR /= arcsec_per_pixel
Y_ERR /= arcsec_per_pixel


;; Evaluate if the reported position is suspect.
not_fiducial = bytarr(Nentries)
      
CATALOG_NAME = keyword_set(cat_name) ? cat_name : 'structure_cat' 


;; Build a suitable structure.
;; Omit an "ID" tag in case the FITS table already has a column with that name.
tag_names   = ['X','Y','X_ERR','Y_ERR','NOT_FIDUCIAL','CATALOG_NAME']

entry = create_struct(structure[0], 'X',0., 'Y',0., 'X_ERR',0., 'Y_ERR',0., 'NOT_FIDUCIAL',0B, 'CATALOG_NAME','')

if keyword_set(label) then begin
  tag_names = [tag_names, 'LABEL']
  entry = create_struct(entry[0], 'LABEL','')
endif

cat   = replicate(entry, Nentries)                                                                      

;; Populate the structure.
for ii=0L,n_tags(structure)-1 do cat.(ii) = structure.(ii)

cmd = 'cat.'+tag_names+'='+tag_names

for ii=0L,n_elements(cmd)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor


help, cat, /st
;print, Nentries, (100.0 * total(/INT, not_fiducial))/Nentries, F='(%"%d sources read; we judge %0.1f%% to be not useful as astrometric standards.")'

return, cat
end



;=============================================================================
; Example code to read an ASCII catalog that has sexagesimal (J2000) celestial coordinates 
; stored in six columns: RAhour, RAmin, RAsec, Decdeg, Decmin, Decsec
; BE SURE THAT ALL HEADER LINES BEGIN WITH THE COMMENT CHARACTER "\".

; Modify as needed to accomodate whatever other columns

; The event2wcs_astr input provides a definition for the sky coordinate system.
;=============================================================================
FUNCTION build_sexagesimal_cat, catalog_fn, event2wcs_astr, NAME=cat_name

column_names = ['name','p1','p2','p3','p4','p5','p6']

column_formats = 'A,D,D,D,D,D,D'

;; Replace the "null" strings with NaN.
spawn, string(catalog_fn, 'temp.txt', F='(%"sed ''s/null/NaN/g'' %s > %s")')
cmd = string(strjoin(column_names,','), column_formats, F='(%"readcol, ''temp.txt'', %s, F=''%s'' ")') + ', COMMENT="\"' 
print, cmd
dum=execute(cmd)
file_delete, 'temp.txt'

Nentries = n_elements(p1)
id = 1+lindgen(Nentries)

LABEL = name

; Convert from sexagesimal to decimal.
ra  = tenv(p1,p2,p3)*15.D
dec = tenv(p4,p5,p6)

;; Convert celestial to the ACIS tangent plane.


ad2xy, ra, dec, event2wcs_astr, x, y  

;; Assign position errors in the tangent plane coordinates.
 ra_err = 0.5 ; arcsec
dec_err = 0.5 ; arcsec
arcsec_per_pixel = abs(event2wcs_astr.CDELT[0] * event2wcs_astr.CD[0,0])*3600.0
X_ERR =  ra_err / arcsec_per_pixel
Y_ERR = dec_err / arcsec_per_pixel

not_fiducial = bytarr(Nentries)

CATALOG_NAME = keyword_set(cat_name) ? cat_name : 'sexagesimal_cat' 

;; Build a suitable structure.
tag_names   = ['ID','LABEL','X','Y','X_ERR','Y_ERR','NOT_FIDUCIAL','CATALOG_NAME',column_names]
tag_formats = 'J,A,F,F,F,F,B,A,'                              +column_formats

; "L" types in readcol must be "J" types in create_struct!
create_struct, cat, '', tag_names, repchr(tag_formats,'L','J'), DIMEN=Nentries

;; Populate the structure.
cmd = 'cat.'+tag_names+'='+tag_names

for ii=0L,n_tags(cat)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor

help, cat, /st
print, Nentries, ' sources created.'
return, cat
end

      
;=============================================================================
; Function to read a catalog represented as a ds9 region file with celestial coordinates.
; Sources can be represented by circle or ellipse regions; coordinates must be in decimal degrees.
; The event2wcs_astr input provides a definition for the tangent plane coordinate system.
;=============================================================================
FUNCTION build_ds9_cat, region_filename, event2wcs_astr, NAME=cat_name
             
  ; Read the lines from the region file.
  num_lines    = file_lines(region_filename)
  lines        = strarr(num_lines)  
  openr, regunit1, region_filename, /GET_LUN
  readf, regunit1, lines
  free_lun, regunit1
  
  ra   = dblarr(num_lines)
  dec  = dblarr(num_lines)
  region_type = strarr(num_lines)
  
  ; Parse "ellipse" regions for RA & DEC values.
  ; This regular expression should work for positive or negative DEC, 
  ; space or comma separation between parameters, 
  ; and with or without () in the region specification.
  result = stregex(lines,'ellipse[^0-9]*([0-9]+\.[0-9]+)[^-0-9]*(-*[0-9]+\.[0-9]+)',/SUB,/EXT)
  tmp_ra  = double(reform(result[1,*]))
  tmp_dec = double(reform(result[2,*]))
  ind = where(tmp_ra NE 0 AND tmp_dec NE 0, count)
  if (count GT 0) then begin
    ra  [ind] = tmp_ra [ind]
    dec [ind] = tmp_dec[ind]
    region_type[ind] = 'ellipse'
  endif

    ; Parse "circle" regions for RA & DEC values.
  result = stregex(lines,'circle[^0-9]*([0-9]+\.[0-9]+)[^-0-9]*(-*[0-9]+\.[0-9]+)',/SUB,/EXT)
  tmp_ra  = double(reform(result[1,*]))
  tmp_dec = double(reform(result[2,*]))
  ind = where(tmp_ra NE 0 AND tmp_dec NE 0, count)
  if (count GT 0) then begin
    ra  [ind] = tmp_ra [ind]
    dec [ind] = tmp_dec[ind]
    region_type[ind] = 'circle'
  endif

  ind = where(ra NE 0 AND dec NE 0, count)
  if (count EQ 0) then begin
    print, 'ERROR (build_ds9_cat): no source coordinates could be parsed'
    return, 0
  endif

  ra  = ra  [ind]
  dec = dec [ind]
  region_type= region_type[ind]
;forprint, ra,dec

Nentries = n_elements(ra)
id = 1+lindgen(Nentries)


;; Convert celestial to the ACIS tangent plane.


ad2xy, ra, dec, event2wcs_astr, x, y  

;; Assign position errors in the tangent plane coordinates.
;; We arbitrarily assign a 1" error.
arcsec_per_pixel = abs(event2wcs_astr.CDELT[0] * event2wcs_astr.CD[0,0])*3600.0
X_ERR = 1.0 / arcsec_per_pixel
Y_ERR = X_ERR

CATALOG_NAME = keyword_set(cat_name) ? cat_name : 'source_ds9' 

;; Build a suitable structure.
tag_names   = ['ID','X','Y','X_ERR','Y_ERR','RA','DEC','REGION_TYPE','CATALOG_NAME']
tag_formats = 'J,F,F,F,F,D,D,A,A'                 

; "L" types in readcol must be "J" types in create_struct!
create_struct, cat, '', tag_names, repchr(tag_formats,'L','J'), DIMEN=Nentries

;; Populate the structure.
cmd = 'cat.'+tag_names+'='+tag_names

for ii=0L,n_tags(cat)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor

help, cat, /st
print, Nentries, ' sources read.'
return, cat
end


;=============================================================================
; Function to read a standard wavdetect FITS catalog
; The optional event2wcs_astr input provides a new definition for the sky coordinate system.
;
; Weak and off-axis sources are flagged as NOT_FIDUCIAL.
;
; If no sources, zero is returned instead of a structure.
;=============================================================================
FUNCTION build_wavdetect_cat, catalog_fn, event2wcs_astr, NAME=cat_name, QUIET=quiet

;; Read FITS catalog.
cat = mrdfits(catalog_fn, 1, /SILENT)

; If the FITS table is empty, mrdfits will return a scalar zero.
if ~keyword_set(cat) then return, 0

Nentries = n_elements(cat)

arcsec_per_chandra_pixel = 0.492  ; arcsec per Chandra skypix

if keyword_set(event2wcs_astr) then begin
  arcsec_per_pixel = abs(event2wcs_astr.CDELT[0] * event2wcs_astr.CD[0,0])*3600.0
  
  ;; Convert celestial coordinates to a new tangent plane.
  print, 'Reprojecting X/Y coordinates to new tangent plane.'

  ad2xy, cat.ra, cat.dec, event2wcs_astr, x, y  
  cat.X  = X
  cat.Y  = Y
                                                                                                                            
  ; The new tangent plane could define a different pixel size, so convert the wavdetect position errors from Chandra pixels to the pixels the observer specified in event2wcs_astr.
  cat.X_ERR *= (arcsec_per_chandra_pixel / arcsec_per_pixel)
  cat.Y_ERR *= (arcsec_per_chandra_pixel / arcsec_per_pixel)
  
endif else arcsec_per_pixel = arcsec_per_chandra_pixel

;; The (X_ERR,Y_ERR) from wavdetect are merely statistical errors for the mean event position within the extraction region.  
;; Systematic errors are not represented.
;; These errors can get unrealistically small which causes us to miss matches that are clearly legitimate.
;; We choose to arbitrarily add in 0.2" error to wavdetect positions to help deal with this.
;; The choice of this value DIFFERS from the systematic error we assume in build_AE_cat (which was estimated from empirical match distances seen between ACIS and HST sources).  
;; We're sticking with the 0.2" value here because we have a long history of using it in matches involving wavdetect, and because it's reasonable to think that wavdetect positions have lower quality than AE positions.
systematic_error  = 0.2  ; arcsec
systematic_error /=  arcsec_per_pixel
cat.X_ERR = sqrt(cat.X_ERR^2 + systematic_error^2)
cat.Y_ERR = sqrt(cat.Y_ERR^2 + systematic_error^2)

print, systematic_error, F='(%"A \"systematic\" uncertainty of %0.2f arcsec has been added in quadrature to the position uncertainties reported by wavdetect.")'


if keyword_set(cat_name) then begin
  if (total(strmatch(tag_names(cat),'CATALOG_NAME')) EQ 0) then begin
    ; We need to add a CATALOG_NAME column to the catalog.
    temp_cat = replicate(create_struct(cat[0], 'CATALOG_NAME', cat_name), Nentries)
    struct_assign, cat, temp_cat
    cat = temp_cat
    cat.CATALOG_NAME = cat_name
  endif else print, 'WARNING!  The catalog already has a CATALOG_NAME tag; ignoring your NAME keyword.'
endif ;keyword_set(cat_name)



;; Flag sources that would not make good astrometric fiducials.
;; Since some wavdetect errors seem to come out to be zero, we
;; also use a threshold on NET_COUNTS to be sure to omit very weak sources.
;; Since wavdetect positions probably have a systematic error off-axis we 
;; arbitrarily use only ACIS sources less than 5' off-axis.
temp_cat = replicate(create_struct(cat[0], 'NOT_FIDUCIAL', 0B), Nentries)
struct_assign, cat, temp_cat
cat = temp_cat
theta = sqrt((cat.X - 4096)^2 + (cat.Y - 4096)^2) * arcsec_per_pixel / 60. ; arcmin
cat.NOT_FIDUCIAL = (cat.NET_COUNTS LE 10) OR (THETA GT 5)

if ~keyword_set(quiet) then begin
  help, cat, /st
  print, Nentries, (100.0 * total(/INT, cat.not_fiducial))/Nentries, F='(%"%d sources read; we judge %0.1f%% to be not useful as astrometric standards.")'
endif
return, cat
end


;=============================================================================
; Function to build a catalog from the FITS table produced by ACIS Extract 
; in the COLLATED_FILENAME stage.
; The event2wcs_astr input provides a definition for the sky coordinate system.
;=============================================================================
FUNCTION build_AE_cat, collated_fn, event2wcs_astr, USE_MEAN_DATA_POSITIONS=use_mean_data_positions

tb = mrdfits(collated_fn, 1, /SILENT)

Nentries = n_elements(tb)

if keyword_set(use_mean_data_positions) then begin
  ; mean position of extracted events, for whatever set of ObsIDs were used in the merge that has been collated
  ra  = tb.RA_DATA  
  dec = tb.DEC_DATA
endif else begin
  ; position used in AE's catalog
  ra  = tag_exist(/TOP_LEVEL, tb,'RA' ) ? tb.RA  : tb.RAdeg ; AE collation, from COLLATE stage or from ae_flatten_collation
  dec = tag_exist(/TOP_LEVEL, tb,'DEC') ? tb.DEC : tb.DEdeg
endelse

;; Convert celestial positions to the ACIS tangent plane.
ad2xy, ra, dec, event2wcs_astr, x, y  



;; Retrieve position uncertainties in units of arcseconds.
if            tag_exist(/TOP_LEVEL, tb,'XPosErr' ) then begin
  ; Flattened AE table (modern, from ae_flatten_collation). 
  X_ERR = tb.XPosErr
  Y_ERR = tb.YPosErr
endif else if tag_exist(/TOP_LEVEL, tb,'ERX_DATA') then begin
  ; AE collation (modern, from COLLATE stage).
  X_ERR = tb.ERX_DATA
  Y_ERR = tb.ERY_DATA
endif else if tag_exist(/TOP_LEVEL, tb,'PosErr') then begin
  ; Flattened AE table (older, from ae_flatten_collation). 
  ; Older AE versions saved only an "error circle", defined as PosErr = sqrt( XPosErr^2 + YPosErr^2 ).
  ; If we assume the two errors are the same, then XPosErr=YPosErr= PosErr/sqrt(2)
  print, 'Position uncertainty obtained from PosErr column.'
  X_ERR = tb.PosErr/sqrt(2)
  Y_ERR = tb.PosErr/sqrt(2)
endif else begin
  ; AE collation (older, from COLLATE stage).
  ; Older AE versions saved only an "error circle", defined as ERR_DATA = sqrt( ERX_DATA^2 + ERY_DATA^2 ).
  ; If we assume the two errors are the same, then ERX_DATA=ERY_DATA= ERR_DATA/sqrt(2)
  print, 'Position uncertainty obtained from ERR_DATA column.'
  X_ERR = tb.ERR_DATA/sqrt(2)
  Y_ERR = tb.ERR_DATA/sqrt(2)
endelse

;; Inflate reported position errors to model systematic effects.
; The uncertainties estimated by AE are merely statistical errors for the mean event position within the extraction region.  
; Systematic errors (e.g. the extraction region placed in the wrong location) are not represented.
; These AE errors can get unrealistically small (e.g. 0.07 skypix for a 120 count source on-axis) which causes us to miss matches that are clearly legitimate.
; We choose to arbitrarily add error to ACIS positions to deal with this.

; The choice of this value was informed by the empirical match distances seen between ACIS and HST sources in NGC3603 (April 2011).
; Specifically, the systematic error value below produces cumulative distributions for Xoffset/Xsigma and Yoffset/Ysigma than contain ~68% of the matches in the interval [-1, 1].
systematic_error = 0.08  ; arcsec

arcsec_per_pixel = abs(event2wcs_astr.CDELT[0] * event2wcs_astr.CD[0,0])*3600.0

print, systematic_error, F='(%"A \"systematic\" uncertainty of %0.2f arcsec has been added in quadrature to the position uncertainties reported by AE.")'

;; Convert position errors from arcseconds to pixels in the tangent plane coordinates.
X_ERR = sqrt(X_ERR^2 + systematic_error^2) / arcsec_per_pixel  ; pixels defined by event2wcs_astr
Y_ERR = sqrt(Y_ERR^2 + systematic_error^2) / arcsec_per_pixel  ; pixels defined by event2wcs_astr


;; Evaluate if the reported position is suspect.
not_fiducial = bytarr(Nentries)

;; Build a suitable structure.
tag_names   = ['ID','X','Y','X_ERR','Y_ERR','NOT_FIDUCIAL']

entry = create_struct(tb[0], 'ID',0L, 'X',0., 'Y',0., 'X_ERR',0., 'Y_ERR',0., 'NOT_FIDUCIAL',0B)
cat   = replicate(entry, Nentries)

;; Populate the structure.
for ii=0L,n_tags(tb)-1 do cat.(ii) = tb.(ii)

ID = 1+lindgen(Nentries)

cmd = 'cat.'+tag_names+'='+tag_names

for ii=0L,n_elements(cmd)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor


;help, cat, /st
print, Nentries, ' sources read.'
return, cat
end


;=============================================================================
; Function to read a standard NOMAD catalog in FITS format obtained through Vizier.
; The event2wcs_astr input provides a definition for the sky coordinate system.
;=============================================================================
FUNCTION build_NOMAD_cat, catalog_fn, event2wcs_astr

tb = mrdfits(catalog_fn, 1, /SILENT)

Nentries = n_elements(tb)

;; Convert celestial positions to the ACIS tangent plane.
;; We're using measured positions, rather than those adjusted for proper motion by Vizier (which might be very noisy).
ad2xy, tb.RAJ2000, tb.DEJ2000, event2wcs_astr, x, y   


;; Assign position errors in the tangent plane coordinates.
;; The catalog errors are in units of mas.
arcsec_per_pixel = abs(event2wcs_astr.CDELT[0] * event2wcs_astr.CD[0,0])*3600.0
X_ERR = (tb.E_RAJ2000/1000.) / arcsec_per_pixel
Y_ERR = (tb.E_DEJ2000/1000.) / arcsec_per_pixel

; It's hard to believe, but all the 2MASS sources in NOMAD report zero for the position error.
; Thus, we're going to assign the median 2MASS position error that we found in the M17 field (0.16 skypix = 0.08")
; when errors are missing in NOMAD.
default_err = 0.08 ;arcsec
ind = where(X_ERR EQ 0, count)
if (count GT 0) then begin
  X_ERR[ind] = default_err / arcsec_per_pixel
  print, count, default_err, F='(%"%d missing X-position errors were assumed to be %0.2f arcsec")'
endif
ind = where(Y_ERR EQ 0, count)
if (count GT 0) then begin
  Y_ERR[ind] = default_err / arcsec_per_pixel
  print, count, default_err, F='(%"%d missing Y-position errors were assumed to be %0.2f arcsec")'
endif



;; Evaluate the complex criteria which seem to be involved in deciding if the reported position is suspect.
temp = tb.Xflags
;temp[where(temp EQ '')] = '0'
Xflags = lonarr(Nentries)
reads, temp, Xflags, F='(Z)'


;; The NOMAD documentation at Vizier and email with Dr. Levine on the NOMAD team indicate
;; that we should be able to simply use the column "R" ("recommended astrometric standard") 
;; in the catalog to decide if a source is clean or not. 
;; However, only a few percent of the NOMAD sources seem to be "recommended".
;; Thus, we choose to roll our own quality metric based on our interpretation of the NOMAD flags.

; Bit flags in Xflags column, shown in hex.
;  00001 = UBBIT   : Fails Blaise's test for USNO-B1.0 star
;  00002 = TMBIT   : Fails Roc's test for clean 2MASS star
;  00004 = YB6     : Included in YB6 catalog (Y)
;  00008 = 2MASS   : Included in 2MASS catalog (M)
;  00010 = TYBIT   : Astrometry comes from Tycho2 catalog (T)
;  00020 = XRBIT   : Alternative correlations for same (RA,Dec)
;  00040 = ITMBIT  : Alternative correlations for same 2MASS ID
;  00080 = IUCBIT  : Alternative correlations for same UCAC-2 ID
;  00100 = ITYBIT  : Alternative correlations for same Tycho2 ID
;  00200 = OMAGBIT : Blue magnitude from O (not J) plate (o)
;  00400 = EMAGBIT : Red magnitude from E (not F) plate (e)
;  00800 = TMONLY  : Object found only in 2MASS catalog (M)
;  01000 = HIPAST  : Ast from Hipparcos catalog (H)
;  02000 = SPIKE   : USNO-B1.0 diffraction spike bit set
;  04000 = TYCONF  : Tycho2 confusion flag set
;  08000 = BSCONF  : Bright star has nearby faint source
;  10000 = BSART
  
UBBIT  = (Xflags AND '00001'X ) NE 0
TMBIT  = (Xflags AND '00002'X ) NE 0
XRBIT  = (Xflags AND '00020'X ) NE 0
ITMBIT = (Xflags AND '00040'X ) NE 0
IUCBIT = (Xflags AND '00080'X ) NE 0
ITYBIT = (Xflags AND '00100'X ) NE 0
TYCONF = (Xflags AND '04000'X ) NE 0
SPIKE  = (Xflags AND '02000'X ) NE 0
TYCONF = (Xflags AND '04000'X ) NE 0
BSCONF = (Xflags AND '08000'X ) NE 0
BSART  = (Xflags AND '10000'X ) NE 0

; Values of the single-character column "r" indicating the origin of the reported position.
;      B = USNO-B1.0 (Monet et al., Cat. I/284)
;      C = UCAC2 (Zacharias et al., Cat. I/289)
;      M = 2MASS catalog of point sources (Cutri et al., Cat. II/246)
;      Y = YB6 Catalog (USNO, unpublished)
;      T = Tycho-2 Catalog (Hog et al., 2000, Cat. I/259)
;      H = Hipparcos catalog (Table I/239/hip_main)
;      o = Palomar-I blue (O) plate (for Bmag)
;      e = Palomar-I red  (E) plate (for Rmag)

origin_of_position = strtrim(tb.R,2)

; Historically, NOMAD catalogs from Vizier could end up with the column named "R" holding the "recommended flag", rather than the "origin of position" flag that we want. 
; We check for this mistake below.
if (min(strlen(origin_of_position)) NE 1) || $
   (max(strlen(origin_of_position)) NE 1) || $
   (total(/INT, ~stregex(/BOOL, origin_of_position, '[BCMYTHoe]')) NE 0) then begin
 
  ; If we don't know the origin of the adopted position, then we should not apply mission-specific cleaning criteria.
  fromUSNOB = replicate(0B, Nentries)
  from2MASS = replicate(0B, Nentries)
  fromTYCHO = replicate(0B, Nentries)
  print
  print, 'WARNING!  This NOMAD catalog is missing the "source of the position" column.  Our cleaning algorithm will be somewhat LESS aggressive than it should be (UBBIT, SPIKE, TMBIT, TYCONF criteria are ignored)!'
endif else begin
  fromUSNOB = (origin_of_position EQ 'B')      
  from2MASS = (origin_of_position EQ 'M')      
  fromTYCHO = (origin_of_position EQ 'T')      
endelse

num_reported = 0

; 2011 Sept
; Leisa and I chose an 0.25" position error threshold for "fiducial" sources after looking at the distribution of position errors over the Carina field.  On that field, 11.6% of sources fail this criterion.
position_error_theshold  = 0.25               ; arcsec
not_fiducial = (X_ERR GT position_error_theshold/arcsec_per_pixel) OR (Y_ERR GT position_error_theshold/arcsec_per_pixel)
num_not_fiducial = total(/INT, not_fiducial)
print, num_not_fiducial - num_reported, position_error_theshold, F='(%"\n%d have position errors larger than %0.2f arcsec.")'
num_reported = num_not_fiducial

not_fiducial OR= ITMBIT OR IUCBIT OR ITYBIT
num_not_fiducial = total(/INT, not_fiducial)
print, num_not_fiducial - num_reported, F='(%"%d more have ITMBIT OR IUCBIT OR ITYBIT.")'
num_reported = num_not_fiducial

not_fiducial OR= BSCONF OR BSART OR XRBIT
num_not_fiducial = total(/INT, not_fiducial)
print, num_not_fiducial - num_reported, F='(%"%d more have BSCONF OR BSART OR XRBIT.")'
num_reported = num_not_fiducial

not_fiducial OR= (fromUSNOB AND (UBBIT or SPIKE))
num_not_fiducial = total(/INT, not_fiducial)
print, num_not_fiducial - num_reported, F='(%"%d more have UBBIT or SPIKE.")'
num_reported = num_not_fiducial

not_fiducial OR= (from2MASS AND TMBIT)
num_not_fiducial = total(/INT, not_fiducial)
print, num_not_fiducial - num_reported, F='(%"%d more have TMBIT.")'
num_reported = num_not_fiducial

not_fiducial OR= (fromTYCHO AND TYCONF)
num_not_fiducial = total(/INT, not_fiducial)
print, num_not_fiducial - num_reported, F='(%"%d more have TYCONF.")'
num_reported = num_not_fiducial

CATALOG_NAME = 'NOMAD'

LABEL = tb.NOMAD1

;; Build a suitable structure.
tag_names   = ['ID','LABEL','X','Y','X_ERR','Y_ERR','NOT_FIDUCIAL','CATALOG_NAME']

entry = create_struct(tb[0], 'ID',0L, 'LABEL','', 'X',0., 'Y',0., 'X_ERR',0., 'Y_ERR',0., 'NOT_FIDUCIAL',0B, 'CATALOG_NAME','')
cat   = replicate(entry, Nentries)                                                                      

;; Populate the structure.
for ii=0L,n_tags(tb)-1 do cat.(ii) = tb.(ii)


ID = 1+lindgen(Nentries)

cmd = 'cat.'+tag_names+'='+tag_names

for ii=0L,n_elements(cmd)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor


;help, cat, /st
print, Nentries, (100.0 * total(/INT, not_fiducial))/Nentries, F='(%"%d sources read; we judge %0.1f%% to be not useful as astrometric standards.")'

return, cat
end



;=============================================================================
; Function to read any  FITS format catalog, e.g. from VizieR.

; WARNING!  Prior to ~2010 December, Vizier omitted TNULL keywords to their FITS format catalogs, making it impossible to identify null cells in a table.  As of 2011 April, Vizier catalogs with TNULL keywords are still not read properly by mrdfits() in the AstroLib; mrdfits() foolishly converts all null cells to zeros!!!!

; Supply the catalog name via the NAME input---to be used in ds9 region tags.

; Supply strings containing IDL expressions for the position errors, in units of arcseconds, via RA_ERROR_EXPRESSION and DEC_ERROR_EXPRESSION.  In those expressions, the catalog structure array is named "tb".

; Supply strings containing IDL expressions for the RA and DEC coordinates, in units of degrees, via RA_EXPRESSION and DEC_EXPRESSION.  In those expressions, the catalog structure array is named "tb".  
; The default is tb._RAJ2000 and tb._DEJ2000, suitable for VizieR catalogs.

; If /IGNORE_ID specified, any ID column in catalog is overwritten.

; The event2wcs_astr input provides a definition for the sky coordinate system.
;=============================================================================
FUNCTION build_FITS_cat, catalog_fn, event2wcs_astr, $
           RA_EXPRESSION      =ra_expression      , DEC_EXPRESSION      =dec_expression, $
           RA_ERROR_EXPRESSION=ra_error_expression, DEC_ERROR_EXPRESSION=dec_error_expression, NAME=cat_name, $
           IGNORE_ID=ignore_id

tb = mrdfits(catalog_fn, 1, /SILENT)

Nentries = n_elements(tb)
id = 1+lindgen(Nentries)

; Find celestial coordinates in the table.
if ~keyword_set( ra_expression) then  ra_expression = 'tb._RAJ2000'
if ~keyword_set(dec_expression) then dec_expression = 'tb._DEJ2000'
if NOT execute('RA=' + ra_expression) then message,  'RA_EXPRESSION cannot be evaluated.'
if NOT execute('DEC='+dec_expression) then message, 'DEC_EXPRESSION cannot be evaluated.'

;; Convert celestial positions to the ACIS tangent plane.
ad2xy, ra, dec, event2wcs_astr, x, y  

;; Assign position errors in the tangent plane coordinates.
if NOT execute('X_ERR='+ ra_error_expression) then message,  'RA_ERROR_EXPRESSION cannot be evaluated.'
if NOT execute('Y_ERR='+dec_error_expression) then message, 'DEC_ERROR_EXPRESSION cannot be evaluated.'

arcsec_per_pixel = abs(event2wcs_astr.CDELT[0] * event2wcs_astr.CD[0,0])*3600.0
X_ERR /= arcsec_per_pixel
Y_ERR /= arcsec_per_pixel


;; Evaluate if the reported position is suspect.
not_fiducial = bytarr(Nentries)
      
CATALOG_NAME = keyword_set(cat_name) ? cat_name : 'vizier_cat' 


;; Build a suitable structure.
tag_names   = ['X','Y','X_ERR','Y_ERR','NOT_FIDUCIAL','CATALOG_NAME']

entry = create_struct(tb[0], 'NOT_FIDUCIAL',0B, 'CATALOG_NAME','')
if ~tag_exist(/TOP_LEVEL, entry,'X'    ) then entry = create_struct(entry, 'X'    ,0., 'Y'    ,0.)
if ~tag_exist(/TOP_LEVEL, entry,'X_ERR') then entry = create_struct(entry, 'X_ERR',0., 'Y_ERR',0.)

; If desired, ignore any ID column in catalog and generate unique IDs here.
if keyword_set(ignore_id) then begin
  tag_names =[tag_names,'ID']
  if ~tag_exist(/TOP_LEVEL, entry,  'ID') then entry = create_struct(entry, 'ID',0L)
endif


cat   = replicate(entry, Nentries)                                                                      

;; Populate the structure.
for ii=0L,n_tags(tb)-1 do cat.(ii) = tb.(ii)

cmd = 'cat.'+tag_names+'='+tag_names

for ii=0L,n_elements(cmd)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor


help, cat, /st
print, Nentries, (100.0 * total(/INT, not_fiducial))/Nentries, F='(%"%d sources read; we judge %0.1f%% to be not useful as astrometric standards.")'

return, cat
end



;=============================================================================
; Function to read a standard 2MASS catalog in ASCII format obtained through "Gator" at
; http://irsa.ipac.caltech.edu/applications/Gator/
; HEADER LINES MUST BEGIN WITH THE COMMENT CHARACTER "\" or "|".

; The 2MASS columns are described at http://www.ipac.caltech.edu/2mass/releases/allsky/doc/sec2_2a.html

; The event2wcs_astr input provides a definition for the sky coordinate system.
;
; If /CIT is specified, photometry is converted to the CIT color system.
; http://www.astro.caltech.edu/~jmc/2mass/v3/transformations/
; http://www.ipac.caltech.edu/2mass/releases/allsky/doc/sec6_4b.html

;=============================================================================
FUNCTION build_2mass_cat, catalog_fn, event2wcs_astr, CIT=calculate_cit

; The 2MASS catalog has lots of columns; be sure those in your catalog match those listed below.
column_names = ['ra','dec','err_maj','err_min','err_ang','designation',$
                'j_m','j_cmsig','j_msigcom','j_snr',$
                'h_m','h_cmsig','h_msigcom','h_snr',$
                'k_m','k_cmsig','k_msigcom','k_snr',$
                'ph_qual','rd_flg','bl_flg','cc_flg','ndet','gal_contam','mp_flg',$
               ;'dist','angle',
                'j_h','h_k','j_k']

column_formats = 'D,D,D,D,I,A,'+$
                 'D,D,D,D,'+$
                 'D,D,D,D,'+$
                 'D,D,D,D,'+$
                 'A,A,A,A,A,I,I,'+$
                ;'F,F,'+$
                 'D,D,D'

;; Replace the "null" strings with NaN.
spawn, string(catalog_fn, 'temp.txt', F='(%"sed -e ''s/null/NaN/g'' -e ''s/ - /NaN/g'' %s > %s")')

cmd = "readcol, 'temp.txt'," + strjoin(column_names,',') + ', F=column_formats, COMMENT="\", STRINGSKIP="|" '
print, cmd
dum=execute(cmd)
file_delete, 'temp.txt'

;forprint, ra,dec

Nentries = n_elements(ra)
id = 1+lindgen(Nentries)


;; Convert celestial to the ACIS tangent plane.
ad2xy, ra, dec, event2wcs_astr, x, y  

;; Assign position errors in the tangent plane coordinates.
;; The err_maj and err_min columns are error ellipse axes in arcseconds which we choose to average.
arcsec_per_pixel = abs(event2wcs_astr.CDELT[0] * event2wcs_astr.CD[0,0])*3600.0
X_ERR = ((err_maj+err_min)/2) / arcsec_per_pixel
Y_ERR = X_ERR

;; Transform 2mass photometry to CIT
;; Code derived by Kosta in /bulk/tecumseh1/patb/projects/M17/data/Broos2007/counterparts/Kosta_analysis/calculate_avlbolmass_2mass.pro
JH_cit = 0.92*(j_m-h_m) + 0.043
HK_cit = (h_m-k_m) - 0.034
JK_cit = 0.936*(j_m-k_m) + 0.0187

K_cit = k_m + 0.019 - 0.000936*(j_m-k_m)
J_cit = k_m + 0.935*(j_m-k_m) + 0.0187
H_cit = k_cit + (h_m-k_m) - 0.034

not_fiducial = bytarr(Nentries)

CATALOG_NAME = 'TWOMASS'

;; Build a suitable structure.
if keyword_set(calculate_cit) then begin
  tag_names   = ['ID','X','Y','X_ERR','Y_ERR', 'NOT_FIDUCIAL','CATALOG_NAME', 'J_cit', 'H_cit', 'K_cit', 'JH_cit', 'HK_cit', 'JK_cit', column_names]
  tag_formats = 'J,F,F,F,F,B,A,F,F,F,F,F,F,' +column_formats
endif else begin
  tag_names   = ['ID','X','Y','X_ERR','Y_ERR', 'NOT_FIDUCIAL','CATALOG_NAME',  column_names]
  tag_formats = 'J,F,F,F,F,B,A,'             +column_formats
endelse

; "L" types in readcol must be "J" types in create_struct!
create_struct, cat, '', tag_names, repchr(tag_formats,'L','J'), DIMEN=Nentries

;; Populate the structure.
cmd = 'cat.'+tag_names+'='+tag_names

for ii=0L,n_tags(cat)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor

help, cat, /st
print, Nentries, ' sources read.'
return, cat
end




;=============================================================================
; Function to read a GLIMPSE catalog in ASCII format obtained through "Gator" at
; http://irsa.ipac.caltech.edu/applications/Gator/
; NOTE that the catalog columns assumed here are NOT the standards ones in Gator
; because readcol.pro can accept only 25 columns.
; BE SURE THAT ALL HEADER LINES BEGIN WITH THE COMMENT CHARACTER "\".

;;; See documents at http://www.astro.wisc.edu/glimpse/docs.html
; e.g. 
; http://www.astro.wisc.edu/glimpse/glimpse1_dataprod_v2.0.pdf
; and
; http://www.astro.wisc.edu/glimpse/GQA-master.pdf  

; The event2wcs_astr input provides a definition for the sky coordinate system.
;=============================================================================
FUNCTION build_glimpse_cat, catalog_fn, event2wcs_astr

column_names = ['designation','ra','dec','dra','ddec','csf',$
                'mag_J','dJ_m',$
                'mag_H','dH_m',$
                'mag_Ks','dKs_m',$
                'mag3_6','d3_6m',$
                'mag4_5','d4_5m',$
                'mag5_8','d5_8m',$
                'mag8_0','d8_0m',$
                'sqf_3_6','sqf_4_5','sqf_5_8','sqf_8_0']

column_formats = 'A,D,D,F,F,I,'+$
                 'F,F,'+$
                 'F,F,'+$
                 'F,F,'+$                     
                 'F,F,'+$
                 'F,F,'+$
                 'F,F,'+$
                 'F,F,'+$
                 'I,I,I,I'

;; Replace the "null" strings with NaN.
spawn, string(catalog_fn, 'temp.txt', F='(%"sed ''s/null/NaN/g'' %s > %s")')
cmd = string(strjoin(column_names,','), column_formats, F='(%"readcol, ''temp.txt'', %s, F=''X,%s'' ")') + ', COMMENT="\"' 
print, cmd
dum=execute(cmd)
file_delete, 'temp.txt'


;; Convert "null" codes in real-valued columns to NaN.
print, 'NEED TO MODIFY CODE: Convert "null" codes in real-valued columns to NaN.'


;forprint, designation,ra,dec

Nentries = n_elements(ra)
id = 1+lindgen(Nentries)

;; Convert celestial to the ACIS tangent plane.

ad2xy, ra, dec, event2wcs_astr, x, y  


;; Assign position errors in the tangent plane coordinates.
;; The catalog errors are in units of arcsec, but are WORTHLESS!
arcsec_per_pixel = abs(event2wcs_astr.CDELT[0] * event2wcs_astr.CD[0,0])*3600.0
X_ERR = 0.15 / arcsec_per_pixel
Y_ERR = 0.15 / arcsec_per_pixel

print,  F='(%"\nWARNING!  The position uncertainties reported by GLIMPSE are NOT the 1-sigma quantities that match_xy requires.\n'+$
          'A more realistic position uncertainty of 0.15 arcsec has been assigned to all sources.\n\n'+$
          'You should match GLIMPSE to an astrometric standard,\n  then estimate the GLIMPSE uncertainties from the empirical offsets of matched sources,\n  then update the X_ERR and Y_ERR columns (in units of \"pixels\"),\n  and then perform your science matching.\n")'

not_fiducial = bytarr(Nentries)
CATALOG_NAME = 'GLIMPSE'

;; Build a suitable structure.
tag_names   = ['ID','X','Y','X_ERR','Y_ERR','NOT_FIDUCIAL','CATALOG_NAME',column_names]
tag_formats = 'J,F,F,F,F,B,A,'                                           +column_formats

; "L" types in readcol must be "J" types in create_struct!
create_struct, cat, '', tag_names, repchr(tag_formats,'L','J'), DIMEN=Nentries

;; Populate the structure.
cmd = 'cat.'+tag_names+'='+tag_names

for ii=0L,n_tags(cat)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor

help, cat, /st
print, Nentries, ' sources read.'
return, cat
end


;=============================================================================
; Function to read a Spitzer catalog that is stored as a structure array in an IDL savefile.

; Supply the catalog name via the NAME input---to be used in ds9 region tags.

; The event2wcs_astr input provides a definition for the sky coordinate system.
;=============================================================================
FUNCTION build_spitzer_cat, catalog_fn, event2wcs_astr, NAME=cat_name

restore, catalog_fn, /RELAXED_STRUCTURE_ASSIGNMENT

; Matt names the catalog variable "g"; Mike names it 'ls_cat' or "ls_ar'.
if (n_elements(g) GT 0) then begin
  tb = temporary(g)
endif else if (n_elements(ls_cat) GT 0) then begin
  tb = temporary(ls_cat)
endif else if (n_elements(ls_ar ) GT 0) then begin
  tb = temporary(ls_ar)
endif else begin
  print, 'ERROR: Spitzer catalog does not contain the expected variable name ...'
  retall
endelse



;; Convert "null" codes in real-valued columns to NaN.
print, F='(%"\nConverting ad-hoc \"null\" values in real-valued columns to NaN ...")'
f_nan    = !VALUES.F_NAN

temp = tb.mag
ind  = where(temp GT 99, count)
if (count GT 0) then begin
  temp[ind] = f_nan
  tb.mag    = temp 
endif

temp = tb.dmag
ind  = where(temp GT 99, count)
if (count GT 0) then begin
  temp[ind] = f_nan
  tb.dmag   = temp 
endif

temp = tb.rmsmag
ind  = where(temp GT 9, count)
if (count GT 0) then begin
  temp[ind] = f_nan
  tb.rmsmag = temp 
endif

temp = tb.f
ind  = where(temp LT -999, count)
if (count GT 0) then begin
  temp[ind] = f_nan
  tb.f      = temp 
endif

temp = tb.df
ind  = where(temp LT -999, count)
if (count GT 0) then begin
  temp[ind] = f_nan
  tb.df     = temp 
endif

temp = tb.rmsf
ind  = where(temp LT -999, count)
if (count GT 0) then begin
  temp[ind] = f_nan
  tb.rmsf   = temp 
endif

temp = tb.sky
ind  = where(temp LT -999, count)
if (count GT 0) then begin
  temp[ind] = f_nan
  tb.sky    = temp 
endif

temp = tb.sn
ind  = where(temp LT -9, count)
if (count GT 0) then begin
  temp[ind] = f_nan
  tb.sn     = temp 
endif

temp = tb.srcdens
ind  = where(temp LT -9, count)
if (count GT 0) then begin
  temp[ind] = f_nan
  tb.srcdens= temp 
endif



Nentries = n_elements(tb)


;; Convert celestial positions to the ACIS tangent plane.
ad2xy, tb.RA, tb.DEC, event2wcs_astr, x, y  

;; Assign position errors in the tangent plane coordinates.
;; The catalog errors are in units of arcsec, but are WORTHLESS!
arcsec_per_pixel = abs(event2wcs_astr.CDELT[0] * event2wcs_astr.CD[0,0])*3600.0
X_ERR = 0.15 / arcsec_per_pixel
Y_ERR = 0.15 / arcsec_per_pixel

print,  F='(%"\nWARNING!  The position uncertainties reported by GLIMPSE are NOT the 1-sigma quantities that match_xy requires.\n'+$
          'A more realistic position uncertainty of 0.15 arcsec has been assigned to all sources.\n\n'+$
          'You should match GLIMPSE to an astrometric standard,\n  then estimate the GLIMPSE uncertainties from the empirical offsets of matched sources,\n  then update the X_ERR and Y_ERR columns (in units of \"pixels\"),\n  and then perform your science matching.\n")'


;; Evaluate if the reported position is suspect.
not_fiducial = bytarr(Nentries)
      
CATALOG_NAME = keyword_set(cat_name) ? cat_name : 'Spitzer' 


;; Build a suitable structure.
tag_names   = ['X','Y','X_ERR','Y_ERR','NOT_FIDUCIAL','CATALOG_NAME']

entry = create_struct(tb[0], 'X',0., 'Y',0., 'X_ERR',0., 'Y_ERR',0., 'NOT_FIDUCIAL',0B, 'CATALOG_NAME','')
cat   = replicate(entry, Nentries)                                                                      

;; Populate the structure.
for ii=0L,n_tags(tb)-1 do cat.(ii) = tb.(ii)

cmd = 'cat.'+tag_names+'='+tag_names

for ii=0L,n_elements(cmd)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor


help, cat, /st
print, Nentries,  F='(%"%d sources read.")'

return, cat
end



;=============================================================================
; Function to read an MSX catalog in ASCII format obtained through "Gator" at
; http://irsa.ipac.caltech.edu/applications/Gator/
; NOTE that the catalog columns assumed here are NOT the standards ones in Gator
; because readcol.pro can accept only 25 columns.
; BE SURE THAT ALL HEADER LINES BEGIN WITH THE COMMENT CHARACTER "\".

;;; See http://irsa.ipac.caltech.edu/applications/Gator/GatorAid/MSX/readme.html for formats.
; The event2wcs_astr input provides a definition for the sky coordinate system.
;=============================================================================
FUNCTION build_msx_cat, catalog_fn, event2wcs_astr

column_names = ['name','ra','dec','ePos','xPos','posPA', $
                'B1','q_B1','e_B1',$
                'B2','q_B2','e_B2',$
                'A','q_A','e_A',$
                'C','q_C','e_C',$
                'D','q_D','e_D',$
                'E','q_E','e_E' ]

column_formats = 'A,D,D,F,F,F,'+$
                 'F,I,F,'+$
                 'F,I,F,'+$
                 'F,I,F,'+$
                 'F,I,F,'+$
                 'F,I,F,'+$
                 'F,I,F'

;; Replace the "null" strings with NaN.
spawn, string(catalog_fn, 'temp.txt', F='(%"sed ''s/-99.0/NaN/g'' %s > %s")')
cmd = string(strjoin(column_names,','), column_formats, F='(%"readcol, ''temp.txt'', %s, F=''%s'' ")') + ', COMMENT="\"' 
print, cmd
dum=execute(cmd)
file_delete, 'temp.txt'

;forprint, designation,ra,dec

Nentries = n_elements(ra)
id = 1+lindgen(Nentries)

;; Convert celestial to the ACIS tangent plane.

ad2xy, ra, dec, event2wcs_astr, x, y  


;; Assign position errors in the tangent plane coordinates.
;; The catalog errors are in units of arcsec.
;; I'm just averaging the in-scan and cross-scan uncertainties, rather than trying
;; to project them onto celestial coordinates.
arcsec_per_pixel = abs(event2wcs_astr.CDELT[0] * event2wcs_astr.CD[0,0])*3600.0
X_ERR = mean([ePos,xPos]) / arcsec_per_pixel
Y_ERR = X_ERR

CATALOG_NAME = 'MSX'

;; Build a suitable structure.
tag_names   = ['ID','X','Y','X_ERR','Y_ERR','CATALOG_NAME',column_names]
tag_formats = 'J,F,F,F,F,A,'                   +column_formats

; "L" types in readcol must be "J" types in create_struct!
create_struct, cat, '', tag_names, repchr(tag_formats,'L','J'), DIMEN=Nentries

;; Populate the structure.
cmd = 'cat.'+tag_names+'='+tag_names

for ii=0L,n_tags(cat)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor

help, cat, /st
print, Nentries, ' sources read.'
return, cat
end


;=============================================================================
; Function to read a Galactic Plane Survey "gpsSource" UKIDSS catalog in FITS format obtained via http://surveys.roe.ac.uk:8080/wsa/region_form.jsp

; The number of result rows written to files is limited and depends on how many columns have been requested i.e.
;   maximum rows written to file = nint(15000 / no. columns) x 1000
; To avoid hard limits on the catalog size, the following subset of the table columns is requested, via the "Select" field:
;   SOURCEID,CUEVENTID,FRAMESETID,RA,DEC,SIGRA,SIGDEC,EPOCH,MURA,MUDEC,SIGMURA,SIGMUDEC,CHI2,NFRAMES,CX,CY,CZ,HTMID,L,B,LAMBDA,ETA,PRIORSEC,JMHPNT,JMHPNTERR,HMK_1PNT,HMK_1PNTERR,H2_1MK_1PNT,H2_1MK_1PNTERR,JMHEXT,JMHEXTERR,HMK_1EXT,HMK_1EXTERR,H2_1MK_1EXT,H2_1MK_1EXTERR,MERGEDCLASSSTAT,MERGEDCLASS,PSTAR,PGALAXY,PNOISE,PSATURATED,JHALLMAG,JHALLMAGERR,JPETROMAG,JPETROMAGERR,JPSFMAG,JPSFMAGERR,JSERMAG2D,JSERMAG2DERR,JAPERMAG3,JAPERMAG3ERR,JAPERMAG4,JAPERMAG4ERR,JAPERMAG6,JAPERMAG6ERR,JGAUSIG,JELL,JPA,JERRBITS,JDEBLEND,JCLASS,JCLASSSTAT,JPPERRBITS,JSEQNUM,JOBJID,JXI,JETA,HHALLMAG,HHALLMAGERR,HPETROMAG,HPETROMAGERR,HPSFMAG,HPSFMAGERR,HSERMAG2D,HSERMAG2DERR,HAPERMAG3,HAPERMAG3ERR,HAPERMAG4,HAPERMAG4ERR,HAPERMAG6,HAPERMAG6ERR,HGAUSIG,HELL,HPA,HERRBITS,HDEBLEND,HCLASS,HCLASSSTAT,HPPERRBITS,HSEQNUM,HOBJID,HXI,HETA,K_1HALLMAG,K_1HALLMAGERR,K_1PETROMAG,K_1PETROMAGERR,K_1PSFMAG,K_1PSFMAGERR,K_1SERMAG2D,K_1SERMAG2DERR,K_1APERMAG3,K_1APERMAG3ERR,K_1APERMAG4,K_1APERMAG4ERR,K_1APERMAG6,K_1APERMAG6ERR,K_1GAUSIG,K_1ELL,K_1PA,K_1ERRBITS,K_1DEBLEND,K_1CLASS,K_1CLASSSTAT,K_1PPERRBITS,K_1SEQNUM,K_1OBJID,K_1XI,K_1ETA
;
; UKIDS column defintions can be found at http://surveys.roe.ac.uk/wsa/www/wsa_browser.html
; The table type (e.g. gpsSource) returned by the query is shown in the FITS header.

; The event2wcs_astr input provides a definition for the sky coordinate system.
;=============================================================================
FUNCTION build_UKIDSS_cat, catalog_fn, event2wcs_astr

tb = mrdfits(catalog_fn, 1, /SILENT)

; Convert celestial coordinates from radians to degrees.
tb.RA  *= !RADEG
tb.DEC *= !RADEG

; This source database has not been cleaned much.  
; Following the example in Appendix 3 of Lucas et al., MNRAS 391, 136-163 (2008), 
; we are going to remove here these really serious problems:
;   - detections classified as noise
;   - secondary detections of the same source
PriOrSec   = strtrim(tb.PriOrSec,2)
framesetID = strtrim(tb.framesetID,2)
ind = where((tb.mergedClass NE 0) AND ((PriOrSec EQ '0') OR (PriOrSec EQ framesetID)))
tb = tb[ind]

Nentries = n_elements(tb)

;; Convert celestial positions to the ACIS tangent plane.
ad2xy, tb.RA, tb.DEC, event2wcs_astr, x, y  


;; Assign position errors in the tangent plane coordinates.
;; The catalog errors are in units of degrees.
arcsec_per_pixel = abs(event2wcs_astr.CDELT[0] * event2wcs_astr.CD[0,0])*3600.0
X_ERR = 0 > ((tb.sigRA *3600.) / arcsec_per_pixel) ; skypix
Y_ERR = 0 > ((tb.sigDec*3600.) / arcsec_per_pixel) ; skypix

; In Data Release 2 position errors are missing, so we'll assign something arbitrarily.
;
;Dye et al. 2006 (MNRAS 372, 1227) discusses the astrometric calibration of UKIDSS (for the early data release). They reported an internal astrometric rms of 50-100mas and in comparison to 2MASS (on which the astrometric solution was based) ~80mas for individual stars.
; What does this mean, exactly??

default_err = 0.1 ;arcsec
ind = where(X_ERR EQ 0, count)
if (count GT 0) then begin
  X_ERR[ind] = default_err / arcsec_per_pixel
  print, count, default_err, F='(%"%d missing X-position errors were assumed to be %0.2f arcsec")'
endif
ind = where(Y_ERR EQ 0, count)
if (count GT 0) then begin
  Y_ERR[ind] = default_err / arcsec_per_pixel
  print, count, default_err, F='(%"%d missing Y-position errors were assumed to be %0.2f arcsec")'
endif


;; Evaluate if the reported position is suspect.
;; Some important flag bit definitions are at http://surveys.roe.ac.uk/wsa/ppErrBits.html
not_fiducial = bytarr(Nentries)

CATALOG_NAME = 'UKIDSS'


;; Build a suitable structure.
tag_names   = ['ID','X','Y','X_ERR','Y_ERR','NOT_FIDUCIAL','CATALOG_NAME']

entry = create_struct(tb[0], 'ID',0L, 'X',0., 'Y',0., 'X_ERR',0., 'Y_ERR',0., 'NOT_FIDUCIAL',0B, 'CATALOG_NAME','')
cat   = replicate(entry, Nentries)                                                                      

;; Populate the structure.
for ii=0L,n_tags(tb)-1 do cat.(ii) = tb.(ii)


ID = 1+lindgen(Nentries)

cmd = 'cat.'+tag_names+'='+tag_names

for ii=0L,n_elements(cmd)-1 do begin
  if NOT execute(cmd[ii]) then message, 'cmd failed'
endfor


help, cat, /st
print, Nentries, (100.0 * total(/INT, not_fiducial))/Nentries, F='(%"%d sources read; we judge %0.1f%% to be not useful as astrometric standards.")'

return, cat
end

