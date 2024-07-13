; ------------------------------------------------------------------------------
; Purpose: Identify O+ beam over the given time period.
; Routine divide the given period into calculation time period, and run the find O+ beam routine for each time period.
;
; Keywords:
; time_start : time for averaging data, if not set the default is 5 min
; time_end   : plot the result plot in idl_window
; stop       : plot the result plot in ps files,
; beam_recalc: do not load tplot data, but re-calcularte everything
; store_tplot: store tplot data
; ps_plot         : plot ps plots
; save_data  : save data to csv file
; idl_plot   : plot idl plots
; diff_e     : beam filter energy difference tolerance
; diff_pa    : beam filter pitch angle difference tolerance
; time_duration
; subtraction
; flux_threshold,
; def_pap_factor,
; average_time,
; multi_peak
; remove_bidirectional_pa,
; display_time
;
; Output: Depends on Keywords settings. The possible output includes
; data, tplot file and plots. There will also be two .log files
;
; Written by Jing Liao  06/20/2023
; -------------------------------------------------------------------------------

pro plot_o_beam_day_rbsp, sc = sc, sp = sp, time_start = time_start, time_end = time_end, stop = stop, beam_recalc = beam_recalc, store_tplot = store_tplot, ps_plot = ps_plot, save_data = save_data, idl_plot = idl_plot, diff_en = diff_en, diff_pa = diff_pa, time_duration = time_duration, subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor, average_time = average_time, multi_peak = multi_peak, remove_bidirectional_pa = remove_bidirectional_pa, display_time = display_time, low_count_line = low_count_line, plot_all_region = plot_all_region, plot_low_count_filter = plot_low_count_filter
  
  common SHARE1, ENERGY_BINS, DENERGY_BINS, PA_BINS, ERROR_MESSAGE

  ENERGY_BINS = [51767.680, 44428.695, 38130.121, 32724.498, 28085.268, 24103.668, 20686.559, 17753.877, 15236.896, 13076.798, 11222.936, 9631.8994, 8266.4062, 7094.5161, 6088.7222, 5225.5273, 4484.7422, 3848.9187, 3303.2842, 2834.9644, 2433.0547, 2088.1287, 1792.0959, 1538.0620, 1319.9771, 1132.8461, 972.23694, 834.42133, 716.16302, 614.57758, 527.48431, 452.70224, 388.54303, 333.45901, 286.18381, 245.59184, 210.76859, 180.86984, 155.26245, 133.24290, 114.31875, 98.138245, 84.208954, 72.319794, 62.048695, 53.254951, 45.727501, 39.184952, 33.627300, 28.913851, 24.763199, 21.245699, 18.290998, 15.688050, 13.436850, 11.537399, 9.9193497, 8.5123501, 7.3163996, 6.2611499, 5.3465996, 4.6431003, 3.9396000, 3.3767998, 2.9546998, 2.5326002, 2.1808500, 1.8290999, 1.5476999, 1.3366499, 1.1959500, 0.98490000]

  DENERGY_BINS = [3882.5762, 3332.1523, 2859.7593, 2454.3374, 2106.3953, 1807.7751, 1551.4919, 1331.5408, 1142.7672, 980.75989, 841.72021, 722.39246, 619.98047, 532.08875, 456.65417, 391.91458, 336.35568, 288.66891, 247.74632, 212.62233, 182.47911, 156.60965, 134.40720, 115.35465, 98.998283, 84.963455, 72.917770, 62.581600, 53.712231, 46.093319, 39.561325, 33.952671, 29.140728, 25.009428, 21.463787, 18.419390, 15.807645, 13.565239, 11.644684, 9.9932184, 8.5739069, 7.3603687, 6.3156719, 5.4239845, 4.6536522, 3.9941216, 3.4295628, 2.9388714, 2.5220475, 2.1685388, 1.8572400, 1.5934275, 1.3718250, 1.1766038, 1.0077637, 0.86530501, 0.74395126, 0.63842630, 0.54873002, 0.46958625, 0.40099499, 0.34823254, 0.29547000, 0.25325999, 0.22160248, 0.18994503, 0.16356376, 0.13718250, 0.11607750, 0.10024875, 0.089696258, 0.073867500]

  ; RBSP_ENERGY_BINS_2 = [51767.703,46489.328,41749.023, 37492.117,33669.395,30236.381,27153.359,24384.686,21898.328,19665.545,17660.434,15859.675,14242.563,12790.324,11486.172,10314.999,9263.2520,8318.7490,7470.5308,6708.8667,6024.7617,5410.4888,4858.7993, 4363.3794,3918.4988,3518.9165, 3160.1245,2837.9172,2548.5645,2288.6936,2055.3440,1845.7736, 1657.5909,1488.5337,1336.7919, 1200.4530,1078.0458,968.15631,869.45416,780.81366,701.17810,629.70306,565.47461,507.78720,456.008,409.50775,367.78928,330.29340,296.59473,266.34464,239.19043,214.77837,192.89964,173.27206,155.61374,139.71494,125.50434,112.70101,101.16339,90.892395,81.606140,73.304695,65.777328,59.093830, 53.043938,47.626949,42.772804,38.411125,34.541893,31.024385,27.858553,24.974258]

  PA_BINS = [4.5000000, 18.000000, 36.000000, 54.000000, 72.000000, 90.000000, 108.00000, 126.00000, 144.00000, 162.00000, 175.50000]

  ERROR_MESSAGE = ''
  ; ---------------------------------------------------------------
  ; Handle keyword
  ; --------------------------------------------------------------
  if n_elements(sc) eq 0 then sc = 1 ; set the satallite number
  if sc eq 1 then sc_str = 'A'
  if sc eq 2 then sc_str = 'B'
  if sc eq 'A' or sc eq 'B' then sc_str = sc

  if n_elements(sp) eq 0 then sp = 3 ; set the species, 0: H+, 3: O+

  ; keyword set for threshold and the string for setting up the output directory
  if n_elements(flux_threshold) eq 0 then flux_threshold = [0.5, 0.75, 1] ; [0,0,0] ;[0.1, 0.15, 0.2]
  if array_equal(flux_threshold, [0, 0, 0]) then flux_threshold_str = '' else flux_threshold_str = '_flux' + string(flux_threshold[0], format = '(f4.2)') + string(flux_threshold[1], format = '(f4.2)') + string(flux_threshold[2], format = '(f4.2)')

  if n_elements(def_pap_factor) eq 0 then def_pap_factor = [3, 2, 1.1] ; [1,1,1] ;[1.7, 1.4, 1.1]
  if array_equal(def_pap_factor, [1, 1, 1]) then def_pap_factor_str = '' else def_pap_factor_str = '_pap' + string(def_pap_factor[0], format = '(f3.1)') + string(def_pap_factor[1], format = '(f3.1)') + string(def_pap_factor[2], format = '(f3.1)')
  ; time range keyword set
  if n_elements(time_start) eq 0 then time_start = '2013-01-01/00:00:00'

  if keyword_set(time_duration) and keyword_set(time_end) then begin
    print, 'Cannot have time_end and time_duration at the same time.'
    stop
  endif else if (~keyword_set(time_end)) and not (keyword_set(time_duration)) then begin
    time_end = '2020-01-01/00:00:00'
  endif else if ~keyword_set(time_end) and keyword_set(time_duration) then time_end = time_string(time_double(time_start) + time_duration * 24. * 3600.) ; second

  ; The methods that used to improve beam identifcations. The settings set to 1 are those have shown improvements.
  if ~keyword_set(subtraction) then subtraction = 1
  if ~keyword_set(multi_peak) then multi_peak = 1
  if ~keyword_set(remove_bidirectional_pa) then remove_bidirectional_pa = 1

  if ~keyword_set(low_count_line) then low_count_line = 2000
  low_count_line_str = '_lowcount' + strcompress(low_count_line, /remove_all)
  
  if ~keyword_set(diff_pa) then diff_pa = 1
  if ~keyword_set(diff_en) then diff_en = 4

  ; ------------------------------------------------------------------
  ; Settings for running process
  ; -----------------------------------------------------------------
  dt = time_double(time_end) - time_double(time_start)

  calc_time = 24. * 60. * 60. < dt ; in seconds
  if ~keyword_set(display_time) then display_time = 6. * 60 * 60 < dt ; in seconds
  if ~keyword_set(average_time) then average_time = 5 * 60 ; in seconds

  ; ----------------------------------------------------
  ; Set up folders and log filenames
  ; -----------------------------------------------------
  output_path = 'output'
  output_path = output_path + '_' + strcompress(average_time, /remove_all) + 'sec'

  if keyword_set(multi_peak) then output_path = output_path + '_multi'
  if keyword_set(diff_pa) then output_path = output_path + '_pa' + strcompress(diff_pa, /remove_all)
  if keyword_set(diff_en) then output_path = output_path + '_en' + strcompress(diff_en, /remove_all)
  if keyword_set(subtraction) then output_path = output_path + '_subtraction'

  if keyword_set(remove_bidirectional_pa) then output_path = output_path + '_removebi'
  
  output_path = output_path + low_count_line_str
  if flux_threshold_str ne '' then output_path = output_path + flux_threshold_str
  if def_pap_factor_str ne '' then output_path = output_path + def_pap_factor_str

  output_path = output_path + '/'
  
  tplot_path = output_path + 'tplot_daily/'

  scsp_str = 'sc' + strcompress(sc, /remove_all) + '_sp' + strcompress(sp, /remove_all)

  log_path = output_path + 'log/'
  data_path = output_path +'data/' + scsp_str+'/'
  plot_path =  output_path + 'plots/' + scsp_str+'/'

  spawn, 'mkdir -p '+ tplot_path
  spawn, 'mkdir -p '+ data_path
  spawn, 'mkdir -p '+ plot_path
  spawn, 'mkdir -p '+ log_path

  ; Write [START] in log files
  log_filename = log_path + scsp_str + strmid(time_start, 0, 4) + '_log.txt'
  write_text_to_file, log_filename, '[START]', /append
  ; ---------------------------------------------------
  ; Set the loop as requested
  ; ---------------------------------------------------
  ts = time_double(time_start)
  te = time_double(time_end)
  ntime = ceil((te - ts) / calc_time)
  ; ------------------------------------------------------------
  for i = 0l, ntime - 1 do begin
    ; Timespan over each calculation time
    t_s = ts + i * calc_time
    t_e = ts + i * calc_time + calc_time
    ; identify O+ beam
    find_o_beam_rbsp, sc = sc $
    , sp = sp $
    , t_s = t_s $
    , t_e = t_e $
    , log_filename = log_filename $
    , average_time = average_time $
    , ps_plot = ps_plot $
    , idl_plot = idl_plot $
    , save_data = save_data $
    , store_tplot = store_tplot $
    , beam_recalc = beam_recalc $
    , output_path = output_path $
    , tplot_path = tplot_path $
    , data_path = data_path $
    , plot_path = plot_path $
    , low_count_line = low_count_line $
    , plot_low_count_filter = plot_low_count_filter $
    , displaytime = display_time $
    , plot_all_region = plot_all_region $
    , flux_threshold = flux_threshold $
    , def_pap_factor = def_pap_factor $
    , diff_en = diff_en $
    , diff_pa = diff_pa $
    , subtraction = subtraction $
    , multi_peak = multi_peak $
    , remove_bidirectional_pa = remove_bidirectional_pa

    if keyword_set(stop) then stop
  endfor
  ; write [END] in the logs
  write_text_to_file, log_filename, '[END]', /append
  print, time_start, time_end
end