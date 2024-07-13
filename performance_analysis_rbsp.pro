pro performance_analysis_rbsp, read_from_dat = read_from_dat, store_tplot = store_tplot
  
  avoid_compression_time = 1
  ; ------------------------------
  ; Read in data
  ; ------------------------------

  ; Read test data
  test_data_filename = 'data/outflow_test_set.csv'
  test_data = read_csv(test_data_filename)
  test_datetime = test_data.field02
  test_date = strmid(test_datetime, 0, 10)
  test_date = test_date[uniq(test_date)]
  human_flag = test_data.field04
  test_hemi = test_data.field11
  test_hemi = test_hemi eq 'north'

  ; --------------------------------------------------------------------------
  ; read in daily data from csv files into structure data
  ; ---------------------------------------------------------------------------
  time_start = '2016-01-01/00:00:00'
  time_end = '2020-12-31/23:59:59'

  ; main_path = 'output_substraction/'
  main_path = 'output_2min_subtraction/'
  data_path = main_path + 'data/'
  if keyword_set(avoid_compression_time) then begin
    plot_path = main_path + 'plots_avoid_compression/'
    tplot_path = main_path + 'tplot_map_avoid_compression/'
  endif else begin
    plot_path = main_path + 'plots/'
    tplot_path = main_path + 'tplot_map/'
  endelse

  data = read_daily_data(time_start, time_end, tplot_path, data_path, read_from_dat = read_from_dat, store_tplot = store_tplot, avoid_compression_time = avoid_compression_time)
  datetime = data.time
  flag_para = data.flag_para
  flag_anti = data.flag_anti
  xgsm = data.gsm_x
  zgsm = data.gsm_z
  bx_gsm = data.bx_gsm

  indexes = []
  ; Compare data arrays
  for i_test_datetime = 0, n_elements(test_datetime) - 1 do begin
    this_test_datetime = test_datetime[i_test_datetime]
    index = where(data.time eq time_double(this_test_datetime), ct)
    if ct gt 0 then indexes = [indexes, index]
  endfor

  n_time = n_elements(indexes)
  datetime = data.time[indexes]
  flag_para = data.flag_para[indexes]
  flag_anti = data.flag_anti[indexes]
  xgsm = data.gsm_x[indexes]
  zgsm = data.gsm_z[indexes]
  bx_gsm = data.bx_gsm[indexes]

  ; north
  hemi = (xgsm ge -1 and zgsm ge 0) or (xgsm lt -1 and bx_gsm ge 0)

  flag = intarr(n_time)
  index = where(hemi eq 1, ct)
  if ct gt 0 then flag[index] = flag_anti[index]

  index = where(hemi eq 0, ct)
  if ct gt 0 then flag[index] = flag_para[index]

  ; Calculate performance
  print, 1 - total(abs(flag - abs(human_flag))) / n_time

  stop
end