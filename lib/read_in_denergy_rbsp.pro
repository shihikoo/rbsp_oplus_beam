; ---------------------------------------------------------------
; Purpose: caluclate denergy with given energy range. The output
; denergy is FWHM. energy +/- denergy is the full expression.
; The energy and denergy are from the eqipment settings
; -------------------------------------------------------------
pro read_in_denergy_rbsp, epcut, epcut_denergy
  
  common SHARE1, ENERGY_BINS, DENERGY_BINS, PA_BINS, ERROR_MESSAGE

  energy = ENERGY_BINS
  denergy = DENERGY_BINS

  n_time = n_elements(epcut)
  epcut_denergy = dblarr(n_time)
  epcut_denergy[*] = !values.f_nan
  index = where(finite(epcut), ct)

  if ct gt 0 then begin
    for iepcut = 0, n_elements(index) - 1 do begin
      ind_energy_bin = where(round(energy) eq round(epcut[index[iepcut]]))
      epcut_denergy[index[iepcut]] = denergy[ind_energy_bin]
    endfor
  endif

  ; RETURN, epcut_denergy
end