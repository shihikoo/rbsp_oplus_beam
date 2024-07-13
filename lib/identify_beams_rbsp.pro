; -----------------------------------------------------------------------
; Purpose: Identify O+ beam
; Description: This is the main part of the O+ beam program:
; = > 1. clean up the low counts(eflux) data from averaged energy spectra
; = > 2. find energy peak from filted energy spectra
; = > 3. plot pitch angle around the them
; = > 4. find the pitch angle peak
; = > 5. filter the beam out by cleaning up the uncontineous pitch angle
;
; Input:   flux_name              : tplot name of diff flux - energy spectrum
; counts_name            : tplot name of counts - energy spectrum
; t_s                    : start of the time
; t_e                    : end of the time
; average_time           : the average time
; peak_pa_range          : peak pitch angle range matching flux_name
; bx_name                : magentic field x component
; x_gse_name             : tplot name of the empemeris x component
; z_gsm_name             :  tplot name of the empemeris z component in gsm
; low_count_line        : statistics threshold in counts
; plot_low_count_filter
; low_count_filtername
; Created by Jing Liao
; Created on 06/28/2023
; -----------------------------------------------------------------------

pro identify_beams_rbsp, sat, specie, flux_name, average_time, time_avg, region_name $
  , t_s = t_s, t_e = t_e $
  , peak_pa_range = peak_pa_range $
  , low_count_line = low_count_line, pa_count_line = pa_count_line, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor $
  , epcut_name = epcut_name, erange_name = erange_name $
  , pa_name = pa_name, pa_eflux_name = pa_eflux_name $
    ; , pa_h_name = pa_h_name, pa_eflux_h_name = pa_eflux_h_name $
  , pap_name = pap_name, pap_beam_name = pap_beam_name, pap_range_name = pap_range_name, int_flux_name = int_flux_name $
  , epcut_beam_name = epcut_beam_name, erange_beam_name = erange_beam_name $
  , diff_en = diff_en, diff_pa = diff_pa, multi_peak = multi_peak, remove_bidirectional_pa = remove_bidirectional_pa
  

  ; ----------------------------------------
  ; Keywords handling
  ; ---------------------------------------
  if ~keyword_set(t_s) or ~keyword_set(t_e) then begin
    get_timespan, interval
    t_s = interval[0]
    t_e = interval[1]
  endif

  if ~keyword_set(low_count_line) then low_count_line = 800.
  if ~keyword_set(pa_count_line) then pa_count_line = low_count_line / 16.
  if ~keyword_set(flux_threshold) then flux_threshold = [0, 0, 0]
  if ~keyword_set(def_pap_factor) then def_pap_factor = [1, 1, 1]

  ; IF ~KEYWORD_SET(pa_range) THEN pa_range = [0,180]
  if ~keyword_set(peak_pa_range) then peak_pa_range = [0, 180]

  ; -------------------------------------------------------------------
  ; find the evergy range and energy peak from average energy spectra
  ; -------------------------------------------------------------------
  if keyword_set(multi_peak) then begin
    tplot_names, erange_name, names = names
    if ~keyword_set(names) then find_energy_range_from_enspec_multi, flux_name, epcut_name, erange_name, min_erange_bins = 1, round_energybins = 0

    ; plot pitch angle in 'DIFF FLUX' with routine plot_pa_spec_around_energy_peak
    tplot_names, pa_name, names = names
    if ~keyword_set(names) then plot_pa_spec_around_energy_peak_multi_rbsp, sat, specie, 'DIFF FLUX', epcut_name, erange_name, pa_name = pa_name, average_time = average_time, time_avg = time_avg

    ; plot pitch angle for IN 'EFLUX' unit
    tplot_names, pa_eflux_name, names = names
    if ~keyword_set(names) then plot_pa_spec_around_energy_peak_multi_rbsp, sat, specie, 'EFLUX', epcut_name, erange_name, pa_name = pa_eflux_name, average_time = average_time, time_avg = time_avg

    ; plot H+ pitch angle in 'DIFF FLUX' with routine plot_pa_spec_around_energy_peak
    ; tplot_names, pa_h_name, names = names
    ; IF ~KEYWORD_SET(names)  THEN plot_pa_spec_around_energy_peak_multi_rbsp, sat, 0, 'DIFF FLUX', epcut_name, erange_name, pa_name = pa_h_name, average_time = average_time,  time_avg = time_avg

    ; plot H+ pitch angle for IN 'EFLUX' unit
    ; tplot_names, pa_eflux_h_name, names = names
    ; IF ~KEYWORD_SET(names) THEN  plot_pa_spec_around_energy_peak_multi_rbsp, sat, 0, 'EFLUX', epcut_name, erange_name, pa_name = pa_eflux_h_name, average_time = average_time,  time_avg = time_avg

    ; find pitch angle peak
    tplot_names, pap_name, names = names
    if ~keyword_set(names) then find_pa_peak_multi, pa_eflux_name, pa_name, pap_name, region_name, peak_pa_range = peak_pa_range, pa_count_line = pa_count_line, flux_threshold = flux_threshold, remove_bidirectional_pa = remove_bidirectional_pa, def_pap_factor = def_pap_factor

    ; filter pitch angle peak to beams by requiring continuity and close energy range
    tplot_names, pap_beam_name, names = names
    if ~keyword_set(names) then filter_beams_multi, pap_name, epcut_name, erange_name, pap_beam_name, epcut_beam_name, erange_beam_name, diff_en = diff_en, diff_pa = diff_pa

    ; find the pitch angle range around the pitch angle
    tplot_names, pap_range_name, names = names
    if ~keyword_set(names) then find_pa_range_multi, pa_name, pap_beam_name, flux_name, pap_range_name, int_flux_name

    ;-----------------------------------------
; for non multiple energy range routines
;--------------------------------------------     
  endif else begin
    tplot_names, erange_name, names = names
    if ~keyword_set(names) then begin
      find_energy_range_from_enspec, flux_name, epcut_name, erange_name
    endif
    ; plot pitch angle in 'DIFF FLUX' with routine 
    tplot_names, pa_name, names = names
    if ~keyword_set(names) then plot_pa_spec_around_energy_peak_rbsp, sat, specie, 'DIFF FLUX' $
      , epcut_name, erange_name, pa_name = pa_name $
      , average_time = average_time, time_avg = time_avg

    ; plot pitch angle for IN 'EFLUX' unit
    tplot_names, pa_eflux_name, names = names
    IF ~KEYWORD_SET(names) THEN  plot_pa_spec_around_energy_peak_rbsp, sat, specie, 'EFLUX', epcut_name, erange_name, pa_name = pa_eflux_name  , average_time = average_time,  time_avg = time_avg

    ; plot H+ pitch angle in 'DIFF FLUX' with routine plot_pa_spec_around_energy_peak
    ; tplot_names, pa_h_name, names = names
    ; IF ~KEYWORD_SET(names)  THEN plot_pa_spec_around_energy_peak_rbsp, sat, 0, 'DIFF FLUX'  , epcut_name, erange_name, pa_name = pa_h_name, average_time = average_time,  time_avg = time_avg

    ; plot H+ pitch angle for IN 'EFLUX' unit
    ; tplot_names, pa_eflux_h_name, names = names
    ; IF ~KEYWORD_SET(names) THEN  plot_pa_spec_around_energy_peak_rbsp, sat, 0, 'EFLUX'  , epcut_name, erange_name, pa_name = pa_eflux_h_name, average_time = average_time, time_avg = time_avg

    ; find pitch angle peak
    tplot_names, pap_name, names = names
    if ~keyword_set(names) then find_pa_peak, pa_eflux_name, pa_name, pap_name, region_name, peak_pa_range = peak_pa_range $
    , pa_count_line = pa_count_line, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor

    ; filter pitch angle peak to beams by requiring continuity and close energy range
    tplot_names, pap_beam_name, names = names
    if ~keyword_set(names) then filter_beams, pap_name, epcut_name, erange_name, pap_beam_name, epcut_beam_name, erange_beam_name, diff_en = diff_en, diff_pa = diff_pa
  endelse
end