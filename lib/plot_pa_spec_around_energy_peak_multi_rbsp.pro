; ---------------------------------------------------------------
; Purpose: Plot pitch angle around with given energy range and store pitch angle peak into tplot strings
;
; Inputs:  sc, specie, units_name, epcut_name, erange_name
; Keywords: pa_range, pa_name, average_time, n_pa_bins, END_time
;
; ---------------------------------------------------------------

pro plot_pa_spec_around_energy_peak_multi_rbsp, sc, specie, units_name, epcut_name, erange_name, pa_range = pa_range, pa_name = pa_name, average_time = average_time, time_avg = time_avg

  common SHARE1, ENERGY_BINS, DENERGY_BINS, PA_BINS, ERROR_MESSAGE

  ; ----------------------------------------------------------
  ; check keywords and set the energybins range to nrange
  ; ---------------------------------------------------------
  ; energy bins
  energy_bins_defined = ENERGY_BINS
  ; pitch angle bins
  pa_bins_defined = PA_BINS
  n_pa_bins = n_elements(pa_bins_defined)

  ; This units is for pitch angle tlot names of rbsp
  case units_name of
    'DIFF FLUX': units_str = 'DIFFFLUX'
    ; 'Counts':units_str = 'COUNTS'
    'EFLUX': units_str = 'EFLUX'
  endcase
  ; This units is for pitch angle tlot names of rbsp
  case specie of
    '3': sp_str = '3'
    '0': sp_str = '0'
  endcase

  case sc of
    '1': sc_str = 'A'
    '2': sc_str = 'B'
  endcase

;  if ~keyword_set(average_time) then average_time = 300 ; default average time: 300s

  ; ---------------------------------------------------------
  ; Get energy peak and energy range
  ; ---------------------------------------------------------
  n_time = n_elements(time_avg)

  get_data, epcut_name, data = data
  ; time_ep = data.x
  energy_peak = data.y
  if n_elements(energy_peak[*, 0]) ne n_time then stop

  energybins = data.energybins
  nenergybins = n_elements(energybins)

  get_data, erange_name, data = data
  energy_range = data.y

  ; -- get the full timespan
  get_timespan, interval
  ; start_time = interval[0]
  ; end_time = interval[1]

  ; -----------------------------------------------------------------
  ; plot pa in the energy range and store pa specta data
  ; -----------------------------------------------------------------
  time_pa = time_avg
  flux_pa = dblarr(n_time, n_pa_bins, nenergybins)
  pa_pa = dblarr(n_time, n_pa_bins, nenergybins)

  flux_pa[*] = !values.f_nan
  pa_pa[*] = !values.f_nan

  ; plot pa with energy_range.
  ; May need to check no magnetic field error n_mag_error = 0
  for jjj = 0, n_time - 1 do begin
    time = time_avg[jjj] ; start_time + jjj * average_time
    if total(energy_range[jjj, *], /nan) gt 0 then begin
      for kkk = 0, nenergybins - 1 do begin
        energy_range_kkk = [energy_range(jjj, kkk), energy_range(jjj, kkk + nenergybins)]

        if total(energy_range_kkk, /nan) gt 0 then begin
          timespan, time - average_time / 2, average_time, /seconds

          index_low = (sort(abs(energy_bins_defined - min(energy_range_kkk))))[0]
          index_high = (sort(abs(energy_bins_defined - max(energy_range_kkk))))[0]
          ; To avoid overlapping between beams observed at the same time but at different energy, we choose to plot pitch angle around the mid-point of a energy bin, not the low/high edge
          energy = [energy_bins_defined[index_low], energy_bins_defined[index_high]]

          plot_rbsp_hope_l3_paspec, sc, specie, units_name, energy_range = energy, energy_units_in_flux = 'eV', eph_data = 0, aux_data = 0

          e_min = strcompress(string(energy(0), format = '(i5.5)'), /remove_all)
          e_max = strcompress(string(energy(1), format = '(i5.5)'), /remove_all)

          s_pa_name = 'RBSP' + sc_str + '_HOPE_l3_paspec_sp' + sp_str + '_' + units_str + '_' + e_min + '_' + e_max

          tplot_names, s_pa_name, names = names
          if names[0] ne '' then begin
            time_trim_tplot_variable, s_pa_name, time - average_time / 2, time + average_time / 2, keep_v = 1
            get_data, s_pa_name, data = s_pa, dlim = dlim, lim = lim

            ; s_time_pa = s_pa.x
            s_flux_pa = s_pa.y
            s_pa_pa = s_pa.v

            ; A check point to verify the number of pitch angle bins is  the same as defined. At this moment we don't know the data details
            if n_elements(s_flux_pa[0, *]) ne n_pa_bins then stop

            ; average flux data over average_time and save them into arrays
            if units_name eq 'DIFF FLUX' then y_data = total(s_flux_pa, 1, /nan) / n_elements(s_flux_pa) ; MEAN(s_flux_pa, DIM=1 ,/Nan)
            if units_name eq 'EFLUX' then y_data = total(s_flux_pa, 1, /nan)

            flux_pa[jjj, *, kkk] = y_data
            pa_pa[jjj, *, kkk] = pa_bins_defined
            if ~array_equal(reform(s_pa_pa), pa_bins_defined) then stop

            ; plot,  pa_bins_defined, y_data
            store_data, delete = s_pa_name
          endif else pa_pa[jjj, *, kkk] = pa_bins_defined
        endif else  pa_pa[jjj, *, kkk] = pa_bins_defined
      endfor
    endif
  endfor

  ; if pitch angle range is set
  if keyword_set(pa_range) then begin
    pitch_angle = s_pa_pa[0, *]
    index = where(pitch_angle lt pa_range(0) or pitch_angle gt pa_range(1), ct)
    if ct gt 0 then flux_pa[*, index, *] = !values.f_nan
  endif

  ; store pitch angle tplot
  str = {x: time_pa, y: flux_pa, v: pa_pa}
  store_data, pa_name, data = str, dlim = dlim, lim = lim
  options, pa_name, 'ytitle', 'PA!CEN Peak'
  zlim, pa_name, 0.1, 100

  ; set the timespan as before
  timespan, interval[0], interval[1] - interval[0], /seconds

  ; for k = 0, nenergybins-1 do store_data, pa_name+'_'+string(k,format='(i2.2)'), data = {x:time_pa, y:reform(flux_pa[*,*,k]), v:reform(pa_pa[*,*,k])} , dlim =dlim , lim = lim
  ; for k = 0, nenergybins -1 do options, pa_name+'_'+string(k,format='(i2.2)'), 'ytitle', 'pa!C'+string(energy_bins_defined[k])
  ; stop
end