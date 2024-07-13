; ------------------------------------------------------------------------------
; Purpose: Identify O+ beam using RBSP data
;
; Keywords: sc           : rbsp probe no. if not set the default is 1
; sp           :
; t_s          :
; t_e          :
; log_filename = log_filename, $
; average_time : in seconds , if not set the default is 5 min
; ps_plot      : plot the tplot in ps ,
; idl_plot     : plot the tplot in idl window,
; save_data    : save data into csv file
; store_tplot  : store data into .tplot
; beam_recalc  :
; output_path = output_path, $
; low_count_line = low_count_line, $
; plot_low_count_filter =  plot_low_count_filter, $
; pa_count_line = pa_count_line, $
; displaytime = displaytime, $
; plot_all_region = plot_all_region, $
; flux_threshold = flux_threshold, $
; def_pap_factor = def_pap_factor, $
; diff_en = diff_en, $
; diff_pa = diff_pa, $
; dispersion_list = dispersion_list, $
; subtraction = subtraction, $
; , multi_peak = multi_peak $
; , remove_bidirectional_pa = remove_bidirectional_pa
;
; Output: Depends on Keywords settings
; There will also be two .log files
;
; Written by Jing Liao  06/20/2023
; -------------------------------------------------------------------------------

pro find_o_beam_rbsp, sc = sc, $
  sp = sp, $
  t_s = t_s, $
  t_e = t_e, $
  log_filename = log_filename, $
  average_time = average_time, $
  ps_plot = ps_plot, $
  idl_plot = idl_plot, $
  save_data = save_data, $
  store_tplot = store_tplot, $
  beam_recalc = beam_recalc, $
  output_path = output_path, $
  tplot_path = tplot_path, $
  data_path = data_path, $
  plot_path = plot_path, $
  low_count_line = low_count_line, $
  plot_low_count_filter = plot_low_count_filter, $
  pa_count_line = pa_count_line, $
  displaytime = displaytime, $
  plot_all_region = plot_all_region, $
  flux_threshold = flux_threshold, $
  def_pap_factor = def_pap_factor, $
  diff_en = diff_en, $
  diff_pa = diff_pa, $
  dispersion_list = dispersion_list, $
  subtraction = subtraction $
  , multi_peak = multi_peak $
  , remove_bidirectional_pa = remove_bidirectional_pa $
  , dispersion_continuous_time = dispersion_continuous_time $
  , maximum_dispersion_energy_difference = maximum_dispersion_energy_difference
  
  common SHARE1, ENERGY_BINS, DENERGY_BINS, PA_BINS, ERROR_MESSAGE

  ; -----------------------------------------------------
  ; Check keywords
  ; ---------------------------------------------------
  running_time_s = systime(/seconds)
  if n_elements(sc) eq 0 then sc = 1
  if sc eq 1 then sc_str = 'A'
  if sc eq 2 then sc_str = 'B'

  if n_elements(sp) eq 0 then sp = 3
  sp_str = strcompress(sp,/REMOVE_ALL)

  if n_elements(average_time) eq 0 then average_time = 5 * 60 ; in seconds
  ; at_str = strcompress(round(average_time), /remove_all)
  average_time = float(average_time)

  if n_elements(flux_threshold) eq 0 then flux_threshold = [0, 0, 0]
  if n_elements(def_pap_factor) eq 0 then def_pap_factor = [1, 1, 1]

  ; 800 is for eflux low count
  if n_elements(low_count_line) eq 0 then low_count_line = 800

  ; 16 is number of pitch angular bins
  if n_elements(pa_count_line) eq 0  then pa_count_line = low_count_line / 16.

  if n_elements(maximum_dispersion_energy_difference) eq 0 then maximum_dispersion_energy_difference = 3
  if n_elements(dispersion_continuous_time)  eq 0 then dispersion_continuous_time = 30. * 60

  dispersion_folder = plot_path + '/obeam_day/dispersion/'
  SPAWN, 'mkdir -p ' + dispersion_folder

  ; --------------------------------------------------------------------
  ; Settings
  ; --------------------------------------------------------------------
  bmodel = 'T89Q' ; ; 'OP77Q', 'T89Q' 'T89D', 'TS04D'
  ; bin_size_pa =            ; HPCA: 11.25, codif:22.5
  ; n_pa_bins = 11

  full_energy_range = [0, 51767]
  full_energy_bin_range = [0, 71]

  full_pitch_angle_range = [0, 180]
  para_pitch_angle_range = [0, 60]
  anti_pitch_angle_range = [120, 180]

  full_angle_range = [[-90., 90.], [0., 360.]]

  parallel_pa_range = [0, 60]
  ; perpendicular_pa_range = [60, 120]
  antiparallel_pa_range = [120, 180]

  ; -------------------------------------------------------------------------
  ; Delete all the string stored data in order to make sure the program can run correctly
  ; --------------------------------------------------------------------------
  tplot_names, names = names
  store_data, delete = names
  ERROR_MESSAGE = ''
  ; -------------------------------------------------------------------------
  ; Load all the tplot_names for the routine
  ; -------------------------------------------------------------------------
  all_tplot_names = load_tplot_names_rbsp(sc_str, sp_str,bmodel, parallel_pa_range, antiparallel_pa_range)
  
  ; ------------------------------------------------------------------------
  ; Get the time interval from timespan
  ; --------------------------------------------retur----------------------------
  if ~keyword_set(t_s) or ~keyword_set(t_e) then begin
    get_timespan, interval
    t_s = interval[0]
    t_e = interval[1]
  endif

  t_dt = t_e - t_s
  ts = time_string(t_s)
  te = time_string(t_e)
  date_s = extract_date_string(ts)
  time_s = extract_time_string(ts)
  date_e = extract_date_string(te)
  time_e = extract_time_string(te)

  data_filename = tplot_path + 'o_beam_' + date_s + '_' + time_s + '_to_' + date_e + '_' + time_e

  ; -- We adjust the time to include two average_time before and after the original time range to ensure we capture all the edge beams of the time
  adjusted_t_s = t_s - average_time * 2
  adjusted_t_e = t_e + average_time * 2
  adjusted_t_dt = adjusted_t_e - adjusted_t_s

  timespan, adjusted_t_s, adjusted_t_dt, /seconds

  time_avg = indgen(round(adjusted_t_dt / average_time)) * average_time + adjusted_t_s + average_time / 2

  n_avg = n_elements(time_avg)
  ; ------------------------------------------------------------------------
  ; If beam_recalc is not set, then read the tplot varialbes of beam identification. Restore the tplot variables. For flag ct_beam:
  ; 0  => beam_recalc is set
  ; -1 => the tplot file is not found
  ; -----------------------------------------------------------------------
  beam_found = 0
  if ~keyword_set(beam_recalc) then begin
    print, FINDFILE(data_filename + '.tplot.gz', count = ct_tplot_gz)
    if ct_tplot_gz then spawn, 'gzip -df ' + data_filename + '.tplot.gz'
    print, FINDFILE(data_filename + '.tplot', count = ct_tplot)
    if ct_tplot gt 0 then begin
      tplot_restore, filenames = data_filename + '.tplot'
      spawn, 'gzip -9f ' + data_filename + '.tplot'

      tplot_names, all_tplot_names.pap_beam_combine_name, names = names
      if names[0] ne '' then begin
        get_data, names[0], data = data
        if array_equal(time_avg, data.x) then begin 
              beam_found = 1
              write_text_to_file, log_filename, ts + ' TO ' + te + '-------- Found O+ Beam------', /APPEND
        endif else begin 
          tplot_names, names = names
          store_data, delete = names
        endelse 
      endif 
    endif 
  endif
;----
tplot_names, '*HPCA*', names=names
if keyword_set(names) then begin
  store_data, delete = names
endif 

  ; -------------------------------------------------------------
  ; Load the tplot varibles
  ; -------------------------------------------------------------
  
  ; -- Load ephemeris --
  ; resolution 1 min
  tplot_names, all_tplot_names.y_gsm_name, names = names
  if ~keyword_set(names) then begin
    plot_rbsp_magephem, sc, bfield_model = bmodel, all = 0, variable = 'Rgsm'
    plot_rbsp_magephem, sc, bfield_model = bmodel, all = 0, variable = 'Rgse'
    plot_rbsp_magephem, sc, bfield_model = bmodel, all = 0, variable = 'CDMAG_MLT'
    plot_rbsp_magephem, sc, bfield_model = bmodel, all = 0, variable = 'Lsimple'
    plot_rbsp_magephem, sc, bfield_model = bmodel, all = 0, variable = 'InvLat'
    plot_rbsp_magephem, sc, bfield_model = bmodel, all = 0, variable = 'CDMAG_R'

    average_mlt_tplot_variable_with_given_time, all_tplot_names.mlt_name, average_time, time_avg
    average_tplot_variable_with_given_time, all_tplot_names.ephemeris_names, average_time, time_avg

    ; extract x,y,z ephemeris data for tplot purpose
    get_data, all_tplot_names.gse_name, data = data
    store_data, all_tplot_names.x_gse_name, data = {x: data.x, y: data.y[*, 0]}
    store_data, all_tplot_names.y_gse_name, data = {x: data.x, y: data.y[*, 1]}
    store_data, all_tplot_names.z_gse_name, data = {x: data.x, y: data.y[*, 2]}
    get_data, all_tplot_names.gsm_name, data = data
    store_data, all_tplot_names.x_gsm_name, data = {x: data.x, y: data.y[*, 0]}
    store_data, all_tplot_names.y_gsm_name, data = {x: data.x, y: data.y[*, 1]}
    store_data, all_tplot_names.z_gsm_name, data = {x: data.x, y: data.y[*, 2]}
  endif

  ; -- Load Magnetic field--
  ; resolution 4 s
  tplot_names, all_tplot_names.mag_names, names = names
  if ~keyword_set(names) then begin
    plot_rbsp_emfisis_mag, sc, 'GSM'

    tplot_names, all_tplot_names.mag_names, names = names
    if ~keyword_set(names) then begin
      ERROR_MESSAGE = 'No B field data'
      log_error, store_tplot, data_filename, log_filename, t_s, t_e
      RETURN
    endif 

    average_tplot_variable_with_given_time, all_tplot_names.mag_names, average_time, time_avg
  endif 

  ; -- Load H+ and O+ moments--
  ; resolution 23 s
  tplot_names, all_tplot_names.o1_velocity_perp_name, names = names
  if ~keyword_set(names) then begin
    plot_hope_3dmom, [sc, sc, sc, sc, sc, sc], [0, 3, 0, 3, 0, 3], ['D', 'D', 'P', 'P', 'V', 'V'], full_angle_range, full_energy_range
    average_tplot_variable_with_given_time, all_tplot_names.moments_names, average_time, time_avg

    calculate_para_velocity, all_tplot_names.h1_velocity_name, all_tplot_names.mag_name, all_tplot_names.h1_velocity_par_name, all_tplot_names.h1_velocity_perp_name
    calculate_para_velocity, all_tplot_names.o1_velocity_name, all_tplot_names.mag_name, all_tplot_names.o1_velocity_par_name, all_tplot_names.o1_velocity_perp_name
  endif

  ; ------------------------------------------------------------------------------
  ; -- Validate and calculate total pressure & beta --
  tplot_names, all_tplot_names.beta_name, names = names1
  tplot_names, all_tplot_names.density_ratio_name, names = names2
  if ~keyword_set(names1) or ~keyword_set(names2) then begin
    calculate_plasma_beta, all_tplot_names.h1_pressure_name, all_tplot_names.o1_pressure_name, all_tplot_names.mag_pressure_name, all_tplot_names.o1_density_name, all_tplot_names.h1_density_name, all_tplot_names.beta_name, all_tplot_names.p_total_name, all_tplot_names.density_ratio_name

    if ERROR_MESSAGE ne '' then begin
      log_error, store_tplot, data_filename, log_filename, t_s, t_e
      RETURN
    endif
  endif

  ; ---------------------------------------------------------------------
  ; Identify different regions and save in tplot var 'location'
  ; ------------------------------------------------------------------------
  tplot_names, all_tplot_names.region_name, names = names
  ; IF ~KEYWORD_SET(names) THEN $
  identify_regions, all_tplot_names.h1_density_name, all_tplot_names.h1_velocity_t_name, all_tplot_names.x_gse_name, all_tplot_names.y_gse_name, all_tplot_names.z_gsm_name, all_tplot_names.beta_name, all_tplot_names.l_name, all_tplot_names.region_name
  ; endif

  if ~keyword_set(plot_all_region) then begin
    ; Because region is stored as numeric numbers and the '1','2','3' digit stores
    ; magnetosphere region identification, 1 => within magnetosphere, 0 =>
    ; not. Here we take the modulo of 10. to retrive the '1','2','3' digit.
    index = where((r_data(all_tplot_names.region_name, /y) mod 10.) gt 0, ct_magnetosphere)

    ; ct_magnetosphere = TOTAL(magnetosphere_region,/nan)
    if ct_magnetosphere eq 0 then begin
      ERROR_MESSAGE = 'No magnetosphere data'
      log_error, store_tplot, data_filename, log_filename, t_s, t_e
      RETURN
    endif
  endif

tplot_names,'RBSP2*',names=names
store_data, delete=names
; tplot_names, 'RBSPB*', names=names
; store_data, delete=names

; plot_o_beam_day_rbsp, store=1,time_start = '2015-06-23', time_end='2015-06-24', ps=1,sc=2

  ; ----------------------------------------------------------------
  ; Load enegy spectra
  ; ----------------------------------------------------------------
  ; resolution 23 s
  ; -- Load H+ energy spectra --
  tplot_names, all_tplot_names.diffflux_h1_name, names = names
  if ~keyword_set(names) then plot_rbsp_hope_l3_red_enspec, sc, 0, 'DIFF FLUX', energy_units_in_flux = 'eV', pa_range = full_pitch_angle_range, aux_data = 0, eph_data = 0

  tplot_names, all_tplot_names.eflux_h1_name, names = names
  if ~keyword_set(names) then plot_rbsp_hope_l3_red_enspec, sc, 0, 'EFLUX', energy_units_in_flux = 'eV', pa_range = full_pitch_angle_range, aux_data = 0, eph_data = 0

  ; -- Load H+ pitch angle spectra
  tplot_names, all_tplot_names.diffflux_h1_pa_name, names = names
  if ~keyword_set(names) then plot_rbsp_hope_l3_paspec, sc, 0, 'DIFF FLUX', energy_bins_range = full_energy_bin_range, energy_units_in_flux = 'eV', eph_data = 0, aux_data = 0

  tplot_names, all_tplot_names.eflux_h1_pa_name, names = names
  if ~keyword_set(names) then plot_rbsp_hope_l3_paspec, sc, 0, 'EFLUX', energy_range = full_energy_range, energy_units_in_flux = 'eV', eph_data = 0, aux_data = 0

  ; -- Load O+ energy spectra --
  tplot_names, all_tplot_names.diffflux_o1_name, names = names
  if ~keyword_set(names) then plot_rbsp_hope_l3_red_enspec, sc, sp, 'DIFF FLUX', energy_units_in_flux = 'eV', pa_range = full_pitch_angle_range, aux_data = 0, eph_data = 0

  tplot_names, all_tplot_names.eflux_o1_name, names = names
  if ~keyword_set(names) then plot_rbsp_hope_l3_red_enspec, sc, sp, 'EFLUX', energy_units_in_flux = 'eV', pa_range = full_pitch_angle_range, aux_data = 0, eph_data = 0

  ; -- Load O+ energy spectra - parallel --
  tplot_names, all_tplot_names.diffflux_ion_parallel_name, names = names
  if ~keyword_set(names) then plot_rbsp_hope_l3_red_enspec, sc, sp, 'DIFF FLUX', energy_units_in_flux = 'eV', pa_range = para_pitch_angle_range, aux_data = 0, eph_data = 0

  tplot_names, all_tplot_names.eflux_ion_parallel_name, names = names
  if ~keyword_set(names) then plot_rbsp_hope_l3_red_enspec, sc, sp, 'EFLUX', energy_units_in_flux = 'eV', pa_range = para_pitch_angle_range, aux_data = 0, eph_data = 0

  ; -- Load O+ energy spectra - anti-parallel --
  tplot_names, all_tplot_names.diffflux_ion_antiparallel_name, names = names
  if ~keyword_set(names) then plot_rbsp_hope_l3_red_enspec, sc, sp, 'DIFF FLUX', energy_units_in_flux = 'eV', pa_range = anti_pitch_angle_range, aux_data = 0, eph_data = 0

  tplot_names, all_tplot_names.eflux_ion_antiparallel_name, names = names
  if ~keyword_set(names) then plot_rbsp_hope_l3_red_enspec, sc, sp, 'EFLUX', energy_units_in_flux = 'eV', pa_range = anti_pitch_angle_range, aux_data = 0, eph_data = 0

  ; -- Load O+ pitch angle spectra
  tplot_names, all_tplot_names.diffflux_o1_pa_name, names = names
  if ~keyword_set(names) then plot_rbsp_hope_l3_paspec, sc, sp, 'DIFF FLUX', energy_range = full_energy_range, energy_units_in_flux = 'eV', eph_data = 0, aux_data = 0

  tplot_names, all_tplot_names.eflux_o1_pa_name, names = names
  if ~keyword_set(names) then plot_rbsp_hope_l3_paspec, sc, sp, 'EFLUX', energy_range = full_energy_range, energy_units_in_flux = 'eV', eph_data = 0, aux_data = 0

  tplot_names, all_tplot_names.diffflux_h1_name, names = names
  if ~keyword_set(names) then begin
    ERROR_MESSAGE = 'hope data missing'
    log_error, store_tplot, data_filename, log_filename, t_s, t_e
    RETURN
  endif

  ; remove perigee hope data (energy bins are different)
  get_data, all_tplot_names.diffflux_h1_name, data = data
  index = where(data.v[*, 71] gt 20, ct)
  if ct gt 0 then begin
    remove_perigee_hope_data, [all_tplot_names.diffflux_h1_name, all_tplot_names.eflux_h1_name, all_tplot_names.diffflux_o1_name, all_tplot_names.eflux_o1_name, all_tplot_names.diffflux_h1_pa_name, all_tplot_names.eflux_h1_pa_name, all_tplot_names.diffflux_o1_pa_name, all_tplot_names.eflux_o1_pa_name, all_tplot_names.diffflux_ion_parallel_name, all_tplot_names.eflux_ion_parallel_name, all_tplot_names.diffflux_ion_antiparallel_name, all_tplot_names.eflux_ion_antiparallel_name], all_tplot_names.diffflux_h1_name

    index = where(data.v[*, 71] le 20, ct)
    if ct eq 0 then begin
      ERROR_MESSAGE = 'No non perigee data'
      log_error, store_tplot, data_filename, log_filename, t_s, t_e
      RETURN
    endif
  endif

  ; average energy spectra and pitch angle spectra data
  get_data, all_tplot_names.eflux_o1_name, data = data
  if data.x[1] - data.x[0] ne average_time then begin
    average_tplot_variable_with_given_time, [all_tplot_names.diffflux_h1_name, all_tplot_names.eflux_h1_name, all_tplot_names.diffflux_o1_name, all_tplot_names.eflux_o1_name], average_time, time_avg

    average_tplot_variable_with_given_time, [all_tplot_names.diffflux_h1_pa_name, all_tplot_names.eflux_h1_pa_name, all_tplot_names.diffflux_o1_pa_name, all_tplot_names.eflux_o1_pa_name], average_time, time_avg, keep_v = 1
  endif

  ; ------------------------------------------------------------------------------------------------------
  ; preprocess energy spectra
  ; ---------------------------------------------------------------------------------------------------------
  
  tplot_names, all_tplot_names.diffflux_ion_parallel_name + '_Original', names = names
  if ~keyword_set(names) then begin
    preprocess_enspec, [sc], [sp], all_tplot_names.diffflux_ion_parallel_name, all_tplot_names.eflux_ion_parallel_name, average_time, time_avg, all_tplot_names.region_name, t_s = adjusted_t_s, t_e = adjusted_t_e, plot_low_count_filter = plot_low_count_filter, plot_path = plot_path

    preprocess_enspec, [sc], [sp], all_tplot_names.diffflux_ion_antiparallel_name, all_tplot_names.eflux_ion_antiparallel_name, average_time, time_avg, all_tplot_names.region_name, t_s = adjusted_t_s, t_e = adjusted_t_e, plot_low_count_filter = plot_low_count_filter, plot_path = plot_path

    if ERROR_MESSAGE ne '' then begin
      log_error, store_tplot, data_filename, log_filename, t_s, t_e
      return
    endif
  endif

  ; ------------------------------------------------------------------------------
  ; Energy spectra subtraction
  ; ------------------------------------------------------------------------------
  para_enspec_name = all_tplot_names.diffflux_ion_parallel_name
  anti_enspec_name = all_tplot_names.diffflux_ion_antiparallel_name

  if keyword_set(subtraction) then begin
    tplot_names, all_tplot_names.diffflux_ion_parallel_subtracted_name, names = names
    if names[0] eq '' then energy_spectra_subtraction, all_tplot_names.diffflux_ion_parallel_name, all_tplot_names.diffflux_ion_antiparallel_name, all_tplot_names.diffflux_ion_parallel_subtracted_name

    tplot_names, all_tplot_names.diffflux_ion_antiparallel_subtracted_name, names = names
    if names[0] eq '' then energy_spectra_subtraction, all_tplot_names.diffflux_ion_antiparallel_name, all_tplot_names.diffflux_ion_parallel_name, all_tplot_names.diffflux_ion_antiparallel_subtracted_name

    para_enspec_name = all_tplot_names.diffflux_ion_parallel_subtracted_name
    anti_enspec_name = all_tplot_names.diffflux_ion_antiparallel_subtracted_name
  endif

  ; ------------------------------------------------------------------------------
  ; Identify O+ beam for different directions or pitch angle ranges
  ; -------------------------------------------------------------------------------
  ; parallel

  tplot_names, all_tplot_names.parallel_epcut_beam_name, names = names
  if ~keyword_set(names[0]) then $
    identify_beams_rbsp, [sc], [sp], para_enspec_name, average_time, time_avg, all_tplot_names.region_name, t_s = adjusted_t_s, t_e = adjusted_t_e, peak_pa_range = parallel_pa_range, low_count_line = low_count_line, pa_count_line = pa_count_line, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor, erange_name = all_tplot_names.parallel_erange_name, epcut_name = all_tplot_names.parallel_epcut_name, pa_name = all_tplot_names.parallel_pa_name, pa_eflux_name = all_tplot_names.parallel_pa_eflux_name, pap_name = all_tplot_names.parallel_pap_name, pap_beam_name = all_tplot_names.parallel_pap_beam_name, pap_range_name = all_tplot_names.parallel_pap_range_name, int_flux_name = all_tplot_names.int_diffflux_ion_parallel_subtracted_name, epcut_beam_name = all_tplot_names.parallel_epcut_beam_name, erange_beam_name = all_tplot_names.parallel_erange_beam_name, diff_en = diff_en, diff_pa = diff_pa, multi_peak = multi_peak, remove_bidirectional_pa = remove_bidirectional_pa

  ; antiparallel
  tplot_names, all_tplot_names.antiparallel_epcut_beam_name, names = names
  if ~keyword_set(names) then $
    identify_beams_rbsp, [sc], [sp], anti_enspec_name , average_time, time_avg  , all_tplot_names.region_name, t_s = adjusted_t_s, t_e = adjusted_t_e, peak_pa_range = antiparallel_pa_range, low_count_line = low_count_line, pa_count_line = pa_count_line, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor, erange_name = all_tplot_names.antiparallel_erange_name, epcut_name = all_tplot_names.antiparallel_epcut_name, pa_name = all_tplot_names.antiparallel_pa_name, pa_eflux_name = all_tplot_names.antiparallel_pa_eflux_name, pap_name = all_tplot_names.antiparallel_pap_name, pap_beam_name = all_tplot_names.antiparallel_pap_beam_name, pap_range_name = all_tplot_names.antiparallel_pap_range_name, int_flux_name = all_tplot_names.int_diffflux_ion_antiparallel_subtracted_name, epcut_beam_name = all_tplot_names.antiparallel_epcut_beam_name, erange_beam_name = all_tplot_names.antiparallel_erange_beam_name, diff_en = diff_en, diff_pa = diff_pa, multi_peak = multi_peak, remove_bidirectional_pa = remove_bidirectional_pa

  ; -- combine beam results --
  tplot_names, all_tplot_names.pap_beam_combine_name, names = names
  if ~keyword_set(names) then combine_beam, all_tplot_names, start_time = adjusted_t_s, end_time = adjusted_t_e, average_time = average_time

    ; -- write to log that beam is found --
    write_text_to_file, log_filename, ts + ' TO ' + te + '-------- Found O+ Beam------ ' + string((systime(/seconds) - running_time_s) / 60.) + ' minitues used', /append
    
  ; -----------------------------------------------------------------------
  ; Calculate Moments for the beam and save them into tplot
  ; ----------------------------------------------------------------------
  ; tplot_names, all_tplot_names.parallel_temperature_name, names = names
  ; if ~keyword_set(names) then $
  ; calculate_moments_rbsp_for_beam, sc, sp, average_time, all_tplot_names.parallel_erange_beam_name, all_tplot_names.parallel_pap_beam_name, all_tplot_names.parallel_pap_range_name, all_tplot_names.parallel_density_name, all_tplot_names.parallel_pressure_name, all_tplot_names.parallel_velocity_name, all_tplot_names.parallel_temperature_name
 
  ; tplot_names, all_tplot_names.antiparallel_temperature_name, names = names
  ; if ~keyword_set(names) then $
  ; calculate_moments_rbsp_for_beam, sc, sp, average_time, all_tplot_names.antiparallel_erange_beam_name,all_tplot_names.antiparallel_pap_beam_name, all_tplot_names.antiparallel_pap_range_name, all_tplot_names.antiparallel_density_name, all_tplot_names.antiparallel_pressure_name,all_tplot_names.antiparallel_velocity_name, all_tplot_names.antiparallel_temperature_name

  ; -----------------------------------------------------------------------
  ; Calculate denergy and inverse velocity for the beam and save them into tplot
  ; ----------------------------------------------------------------------

  tplot_names, all_tplot_names.parallel_epcut_beam_denergy_name, names = names
  if ~keyword_set(names) then calculate_denergy_multi_rbsp, all_tplot_names.diffflux_ion_parallel_name, all_tplot_names.parallel_epcut_beam_name, all_tplot_names.parallel_epcut_beam_denergy_name
 
  tplot_names, all_tplot_names.antiparallel_epcut_beam_denergy_name, names = names
  if ~keyword_set(names) then calculate_denergy_multi_rbsp, all_tplot_names.diffflux_ion_antiparallel_name, all_tplot_names.antiparallel_epcut_beam_name, all_tplot_names.antiparallel_epcut_beam_denergy_name

  tplot_names, all_tplot_names.parallel_beam_inverse_v_name, names = names
  if ~keyword_set(names) then calculate_inverse_velocity, sp, all_tplot_names.parallel_epcut_beam_name, all_tplot_names.parallel_epcut_beam_denergy_name, all_tplot_names.parallel_beam_inverse_v_name

  tplot_names, all_tplot_names.antiparallel_beam_inverse_v_name, names = names
  if ~keyword_set(names) then calculate_inverse_velocity, sp, all_tplot_names.antiparallel_epcut_beam_name, all_tplot_names.antiparallel_epcut_beam_denergy_name, all_tplot_names.antiparallel_beam_inverse_v_name

  ; ----------------------------------------------------------------------------------
  ; Calculate convective electric field from Vperp of H+ and O+ and magnetic field
  ; ------------------------------------------------------------------------------------
  tplot_names, all_tplot_names.electric_field_h_name, names = names
  if ~keyword_set(names) then calculate_e_field_from_vperp, all_tplot_names.h1_velocity_perp_name, all_tplot_names.bt_name, all_tplot_names.electric_field_h_name

  tplot_names, all_tplot_names.electric_field_o_name, names = names
  if ~keyword_set(names) then calculate_e_field_from_vperp, all_tplot_names.o1_velocity_perp_name, all_tplot_names.bt_name, all_tplot_names.electric_field_o_name

  ; -----------------------------------------------------------------------
  ; Load solar wind data from OMNI
  ; ----------------------------------------------------------------------
  tplot_names, all_tplot_names.parallel_sw_p_name, names = names
  if ~keyword_set(names) then begin
    read_omni, all = 1, hr = 1

    calculate_solarwind_delayed, all_tplot_names.parallel_epcut_beam_name, all_tplot_names.dist_name, [all_tplot_names.imf_bx_name, all_tplot_names.imf_by_gsm_name, all_tplot_names.imf_bz_gsm_name, all_tplot_names.sw_v_name, all_tplot_names.sw_p_name, all_tplot_names.sw_n_name, all_tplot_names.sw_t_name, all_tplot_names.sw_mack_number_name], [all_tplot_names.parallel_imf_bx_name, all_tplot_names.parallel_imf_by_gsm_name, all_tplot_names.parallel_imf_bz_gsm_name, all_tplot_names.parallel_sw_v_name, all_tplot_names.parallel_sw_p_name, all_tplot_names.parallel_sw_n_name, all_tplot_names.parallel_sw_t_name, all_tplot_names.parallel_sw_mack_number_name]

    calculate_solarwind_delayed, all_tplot_names.antiparallel_epcut_beam_name, all_tplot_names.dist_name, [all_tplot_names.imf_bx_name, all_tplot_names.imf_by_gsm_name, all_tplot_names.imf_bz_gsm_name, all_tplot_names.sw_v_name, all_tplot_names.sw_p_name, all_tplot_names.sw_n_name, all_tplot_names.sw_t_name, all_tplot_names.sw_mack_number_name], [all_tplot_names.antiparallel_imf_bx_name, all_tplot_names.antiparallel_imf_by_gsm_name, all_tplot_names.antiparallel_imf_bz_gsm_name, all_tplot_names.antiparallel_sw_v_name, all_tplot_names.antiparallel_sw_p_name, all_tplot_names.antiparallel_sw_n_name, all_tplot_names.antiparallel_sw_t_name, all_tplot_names.antiparallel_sw_mack_number_name]

    average_tplot_variable_with_given_time, all_tplot_names.omni_tplot_names, average_time, time_avg
  endif

  tplot_names, all_tplot_names.parallel_sw_p_name+'_1h', names = names
  if ~keyword_set(names) then begin
    read_omni, all = 1, hr = 1

    calculate_solarwind_delayed_1h, all_tplot_names.parallel_epcut_beam_name, all_tplot_names.dist_name, [all_tplot_names.imf_bx_name, all_tplot_names.imf_by_gsm_name, all_tplot_names.imf_bz_gsm_name, all_tplot_names.sw_v_name, all_tplot_names.sw_p_name, all_tplot_names.sw_n_name, all_tplot_names.sw_t_name, all_tplot_names.sw_mack_number_name], [all_tplot_names.parallel_imf_bx_name, all_tplot_names.parallel_imf_by_gsm_name, all_tplot_names.parallel_imf_bz_gsm_name, all_tplot_names.parallel_sw_v_name, all_tplot_names.parallel_sw_p_name, all_tplot_names.parallel_sw_n_name, all_tplot_names.parallel_sw_t_name, all_tplot_names.parallel_sw_mack_number_name]+'_1h'

    calculate_solarwind_delayed_1h, all_tplot_names.antiparallel_epcut_beam_name, all_tplot_names.dist_name, [all_tplot_names.imf_bx_name, all_tplot_names.imf_by_gsm_name, all_tplot_names.imf_bz_gsm_name, all_tplot_names.sw_v_name, all_tplot_names.sw_p_name, all_tplot_names.sw_n_name, all_tplot_names.sw_t_name, all_tplot_names.sw_mack_number_name], [all_tplot_names.antiparallel_imf_bx_name, all_tplot_names.antiparallel_imf_by_gsm_name, all_tplot_names.antiparallel_imf_bz_gsm_name, all_tplot_names.antiparallel_sw_v_name, all_tplot_names.antiparallel_sw_p_name, all_tplot_names.antiparallel_sw_n_name, all_tplot_names.antiparallel_sw_t_name, all_tplot_names.antiparallel_sw_mack_number_name]+'_1h'

    average_tplot_variable_with_given_time, all_tplot_names.omni_tplot_names, average_time, time_avg
  endif
  
  ; -----------------------------------------------------------------------
  ; interplate kp data (since kp is 1 hour data with some gaps, and
  ; store it into the orignal tplot name
  ; ----------------------------------------------------------------------
  tplot_names, all_tplot_names.kp_name, names = names
  if ~keyword_set(names) then begin
    read_omni, all = 1

    get_data, all_tplot_names.kp_name, data = data, dlim = dlim, lim = lim
    data_kp = interpol(data.y, data.x, time_avg, /nan) / 10.
    store_data, all_tplot_names.kp_name, data = {x: time_avg, y: data_kp, dlim: dlim, lim: lim}

    get_data, all_tplot_names.f107_name, data = data, dlim = dlim, lim = lim
    data_f107 = interpol(data.y, data.x, time_avg, /nan)
    store_data, all_tplot_names.f107_name, data = {x: time_avg, y: data_f107, dlim: dlim, lim: lim}
  endif

  ; ------------------------------------------------------------------------
  ; Load storm data phase data and store phase flag into tplot variables
  ; ------------------------------------------------------------------------
  tplot_names, all_tplot_names.storm_phase_tplot_name, names = names
  if ~keyword_set(names) then find_storm_phase, time_avg, storm_phase_filename = 'data/storm_phase_list.csv', storm_phase_tplot_name = all_tplot_names.storm_phase_tplot_name
  ; -----------------------------------------------------------------------
  ; Load substorm data, and store substorm flag into tplot variables
  ; ----------------------------------------------------------------------
  tplot_names, all_tplot_names.substorm_phase_tplot_name, names = names
  if ~keyword_set(names) then find_substorm_phase, time_avg, substorm_phase_filename = 'data/substorm_list_2016_2017.csv', substorm_phase_tplot_name = all_tplot_names.substorm_phase_tplot_name

    ; ---------------------------------------------------------------------
  ; Identify dispersion
  ; ---------------------------------------------------------------------
  tplot_names, all_tplot_names.parallel_dispersion_n_name, names = names
;  if ~keyword_set(names) then $
  identify_dispersions_multi, average_time, 'PARA' $
    , all_tplot_names.parallel_epcut_beam_name $
    , all_tplot_names.parallel_dispersion_name $
    , all_tplot_names.parallel_beam_inverse_v_name $
    , all_tplot_names.parallel_dispersion_inverse_v_name $
    , all_tplot_names.parallel_dispersion_estimated_distance_name $
    , all_tplot_names.parallel_dispersion_estimated_distance_error_name $
    , all_tplot_names.parallel_dispersion_inverse_v_fitting_name $
    , all_tplot_names.parallel_dispersion_inverse_v_fitting_chisq_name $
    , all_tplot_names.parallel_dispersion_inverse_v_fitting_status_name $
    , all_tplot_names.parallel_dispersion_inverse_v_fitting_dof_name $
    , all_tplot_names.parallel_dispersion_n_name $
    , all_tplot_names.parallel_epcut_name $
    , ps_plot = ps_plot, idl_plot = idl_plot, dispersion_folder = dispersion_folder $
    , dispersion_list = dispersion_list, dispersion_inverse_v_fitting_rsquare_name = all_tplot_names.parallel_dispersion_inverse_v_fitting_rsquare_name, maximum_dispersion_energy_difference = maximum_dispersion_energy_difference, continuous_time = dispersion_continuous_time, reverse_v = 1

  tplot_names, all_tplot_names.antiparallel_dispersion_n_name, names = names
;  if ~keyword_set(names) then $
  identify_dispersions_multi, average_time, 'ANTI' $
    , all_tplot_names.antiparallel_epcut_beam_name $
    , all_tplot_names.antiparallel_dispersion_name $
    , all_tplot_names.antiparallel_beam_inverse_v_name $
    , all_tplot_names.antiparallel_dispersion_inverse_v_name $
    , all_tplot_names.antiparallel_dispersion_estimated_distance_name $
    , all_tplot_names.antiparallel_dispersion_estimated_distance_error_name $
    , all_tplot_names.antiparallel_dispersion_inverse_v_fitting_name $
    , all_tplot_names.antiparallel_dispersion_inverse_v_fitting_chisq_name $
    , all_tplot_names.antiparallel_dispersion_inverse_v_fitting_status_name $
    , all_tplot_names.antiparallel_dispersion_inverse_v_fitting_dof_name $
    , all_tplot_names.antiparallel_dispersion_n_name $
    , all_tplot_names.antiparallel_epcut_name $
    , ps_plot = ps_plot, idl_plot = idl_plot, dispersion_folder = dispersion_folder $
    , dispersion_list = dispersion_list, dispersion_inverse_v_fitting_rsquare_name = all_tplot_names.antiparallel_dispersion_inverse_v_fitting_rsquare_name, maximum_dispersion_energy_difference = maximum_dispersion_energy_difference, continuous_time = dispersion_continuous_time, reverse_v = 1
    
  ; ------------- -------------------------------------------------------
  ; Save tplot varialbes
  ; ---------------------------------------------------------------------
  if keyword_set(store_tplot) then begin
    tplot_save, filename = data_filename
    spawn, 'gzip -9f ' + data_filename + '.tplot'
  endif

  ; -------------------------------------------------------------
  ; Change the time to the original time from the adjusted time
  ; ------------------------------------------------------------
  timespan, t_s, t_dt, /seconds
  ; --------------------------------------------------------------
  ; Overview plots
  ; --------------------------------------------------------------
  
  if keyword_set(ps_plot) or keyword_set(idl_plot) then begin
    if keyword_set(multi_peak) then make_o_beam_tplots_multi_rbsp, sc_str, sp_str, t_s, t_e, t_dt, plot_path, all_tplot_names, displaytime = displaytime, ps = ps_plot, idl_plot = idl_plot else make_o_beam_tplots_rbsp, sc_str, t_s, t_e, t_dt, plot_path, all_tplot_names, displaytime = displaytime, ps = ps_plot, idl_plot = idl_plot
    
  endif

  ; --------------------------------------
  ; Save the data
  ; --------------------------------------
  if keyword_set(save_data) then begin
    ; time_trim_tplot_variable, '*', t_s, t_e
    if keyword_set(multi_peak) then save_o_beam_data_multi_rbsp, date_s, date_e, data_path, all_tplot_names, t_s, t_e else save_o_beam_data_rbsp, date_s, date_e, data_path, all_tplot_names
  endif
  ; ----------------------------------------------------------------
  ; Print, running_time_s
  ; -----------------------------------------------------------------
  print, string((systime(/seconds) - running_time_s) / 60.) + ' minitues used'
  
  close, /all
end