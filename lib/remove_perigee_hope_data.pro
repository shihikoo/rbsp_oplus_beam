; PROCEDURE: remove_perigee_hope_data
;
; PURPOSE:remove perigee data for HOPE data since those data are too close to the Earth and they don't have the same energy bins as the rest of the times
;
; INPUT:
; var -> variable name - Array of strings or numbers. The '*'
; is also accepted.
; enspec_name -> the energy spectra variable name to extract periogee data from
; KEYWORD:
; keep_v ->
; error_message ->
; CREATED by: J. Liao
;
; LAST MODIFIED: 06/29/23
;

pro remove_perigee_hope_data, vars, enspec_var, keep_v = keep_v
  

  common SHARE1, ENERGY_BINS, DENERGY_BINS, PA_BINS, ERROR_MESSAGE

  ; ------------------------------------------------------------------------------------------------------
  ; Find the names of the variables to be averaged, and the check all variables are found
  ; ------------------------------------------------------------------------------------------------------
  tplot_names, vars, names = var_names

  if ~keyword_set(var_names) then begin
    error_message = var_names + ' not found'
    RETURN
  endif

  if var_names[0] eq '' then begin
    error_message = var_names + ' not found'
    RETURN
  endif

  tplot_names, enspec_var, names = enspec_name

  if enspec_name eq '' then begin
    error_message = enspec_var + ' not found'
    RETURN
  endif

  ; ------------------------------------------------------------------------------------------------------
  ; main process
  ; ------------------------------------------------------------------------------------------------------
  get_data, enspec_name, data = data
  if ~keyword_set(data.y) or ~keyword_set(data.v) then begin
    error_message = enspec_name + ' data.y or data.v is not found'
    RETURN
  endif
  index = where(data.v[*, 71] le 20, ct)
  if ct gt 0 then begin
    enbins = data.v[index[0], *]
  endif else begin
    enbins = ENERGY_BINS
  endelse

  index = where(data.v[*, 71] gt 20, ct)
  ; --- Loop through all variables ---
  if ct gt 0 then begin
    for iv = 0, n_elements(var_names) - 1 do begin
      var_name = var_names[iv]
      ; --- Extract data information from tplot variable    ---
      get_data, var_name, data = data, dlim = dlim, lim = lim

      if ~keyword_set(data.y) or ~keyword_set(data.v) then begin
        error_message = var_name + ' data.y or data.v is not found'
        continue
      endif

      data.y[index, *] = !values.f_nan

      if ~keyword_set(keep_v) and (size(data.v))[0] eq 2 then begin
        data.v[index, *] = transpose(CMREPLICATE(enbins, ct))
      endif

      ; save data back the original varnames and with the all data structure

      store_data, var_name, data = data, dlim = dlim, lim = lim
    endfor
  endif
end