;-------------------------------------------------------------------------------
; Purpose: Create maps of streaming O+ with given conditions
; Desctiption: Process is written for RBSP HOPE O+ data
; Inputs:  
; Keywords: ps_plot, read_from_dat, store_tplot
;
; Created by Jing Liao
; Created on 04/13/2021
;-------------------------------------------------------------------------------

PRO sort_o_beam_map_rbsp, ps_plot = ps_plot, read_from_dat = read_from_dat, store_tplot = store_tplot, time_start = time_start, time_end = time_end, stop = stop $
                         , avoid_compression_time = avoid_compression_time $
                         , avoid_2019 = avoid_2019 $
                         , subtraction = subtraction $
                         , reduced= reduced $
                         , sliced_with_input_range = sliced_with_input_range, non_sort_map = non_sort_map, sort_hemi_map = sort_hemi_map, sort_dist_map = sort_dist_map, sort_kp_map = sort_kp_map, sort_bx=sort_bx $
                         , plot_2d = plot_2d $
                         , plot_slice = plot_slice $
                         , direction_set = direction_set $
                         , storm_phase_set=storm_phase_set $
                         , substorm_phase_set= substorm_phase_set $
                         , region_map_set= region_map_set $
                         , point_plot = point_plot $
                         , property_map_set = property_map_set $
                         , property_map_type_set= property_map_type_set $
                         , events_map= events_map $
                         , coor_set = coor_set $
                         , grid= grid  ,slice_grid=slice_grid $
                         , energy_set= energy_set $
                         , imfBz_set= imfBz_set $
                         , imfBy_set=imfBy_set  $
                         , swP_set= swP_set $
                         , swV_set=swV_set
;----------------------------------------------------------------------
; Time settings
;------------------------------------------------------------------------
  IF ~KEYWORD_SET(time_start) THEN time_start = '2016-01-01/00:00:00' 
  IF ~KEYWORD_SET(time_end) THEN time_end = '2020-12-31/23:59:59'

 ; main_path = 'output_2min_multi_subtraction_flux0.100.150.20_pap3.01.81.2'
  main_path = 'output_2min/'
  IF KEYWORD_SET(subtraction) THEN main_path = 'output_2min_subtraction/'
  IF KEYWORD_SET(reduced) THEN main_path = 'output_2min_reduced/' 
  
  data_path = main_path + 'data/'
  if keyword_set(avoid_compression_time)  then begin 
     plot_path = main_path + 'plots_avoid_compression/'
     tplot_path = main_path + 'tplot_map_avoid_compression/'
  endif else if keyword_set(avoid_2019) then begin 
        plot_path = main_path + 'plots_avoid_2019/'
     tplot_path = main_path + 'tplot_map_avoid_2019/'
  endif else  begin 
     plot_path = main_path + 'plots/'
     tplot_path = main_path + 'tplot_map/'
  endelse 
  
;--------------------------------------------------------------------------
; read in daily data from csv files into structure data
;---------------------------------------------------------------------------
  data = read_daily_data(time_start, time_end, tplot_path, data_path, read_from_dat = read_from_dat, store_tplot=store_tplot, avoid_compression_time = avoid_compression_time, avoid_2019 = avoid_2019)
;  data = calculate_daily_data(data)

  IF KEYWORD_SET(stop) THEN stop
  
;----------------------------------------------------------------------------
; non_sort plots 
;----------------------------------------------------------------------------
  IF KEYWORD_SET(non_sort_map) THEN BEGIN
     sort_path = plot_path +'non_sort_map/'
     sort_flag = 1
     sort_title = ''
     make_o_beam_map, data, header $
                      , sort_flag = sort_flag, sort_title = sort_title $
                      , ps_plot = ps_plot, plot_path = sort_path $
                      , grid = grid, slice_grid = slice_grid $
                      , point_plot = point_plot, events_map = events_map $
                      , make_table=make_table $
                      , plot_2d= plot_2d, plot_slice = plot_slice $
                      , region_map_set = region_map_set $
                      , direction_set = direction_set $
                      , storm_phase_set = storm_phase_set $
                      , substorm_phase_set = substorm_phase_set $
                      , coor_set = coor_set $
                      , property_map_set = property_map_set $
                      , property_map_type_set = property_map_type_set $
                      , energy_set = energy_set $
                      , imfBz_set = imfBz_set $
                      , imfBy_set = imfBy_set $
                      , swP_set = swP_set $
                      , swV_set = swV_set
  ENDIF 

  IF KEYWORD_SET(sort_kp_map) THEN BEGIN
     kp_set = ['lt_2', 'ge_2']
     kp_title_set = ['lt_2', 'ge_2']
     FOR iset = 0, N_ELEMENTS(kp_set)-1 DO BEGIN
        this_range_str = kp_set[iset]
        this_title_set = kp_title_set[iset]
        sort_title = 'Kp' + this_title_set
        sort_path = plot_path +'kp_sort_map/kp_'+ this_range_str + '/'
        IF iset EQ 0 THEN sort_flag = 1.0 * (data.kp LT 2) ELSE sort_flag = 1.0 * (data.kp GE 2)
        
        index = WHERE(sort_flag EQ 0, ct)
        IF ct GT 0 THEN sort_flag[index] = !VALUES.F_NAN

        make_o_beam_map, data, header $
                         , sort_flag = sort_flag, sort_title = sort_title $
                         , ps_plot = ps_plot, plot_path = sort_path $
                         , grid = grid, slice_grid = slice_grid $
                         , point_plot = point_plot, events_map = events_map $
                         , make_table=make_table $
                         , plot_2d= plot_2d, plot_slice = plot_slice $
                         , region_map_set = region_map_set $
                         , direction_set = direction_set $
                         , storm_phase_set = storm_phase_set $
                         , substorm_phase_set = substorm_phase_set $
                         , coor_set = coor_set $
                         , property_map_set = property_map_set $
                         , property_map_type_set = property_map_type_set $
                         , energy_set = energy_set $
                         , imfBz_set = imfBz_set $
                         , imfBy_set = imfBy_set $
                         , swP_set = swP_set $
                         , swV_set = swV_set
     ENDFOR 
  ENDIF 

  IF KEYWORD_SET(sort_hemi_map) THEN BEGIN
     coor_set = ['X_GSM', 'Y_GSM', 'Z_GSM']
     plot_2d = 1
     plot_slice = 0
     
     bx_set = ['south', 'north']
     bx_title_set = ['south', 'north']
     FOR iset = 0, N_ELEMENTS(bx_set)-1 DO BEGIN
        this_range_str = bx_set[iset]
        this_title_set = bx_title_set[iset]
        sort_title =  this_title_set
        sort_path = plot_path +'hemi_sort_map/hemi_'+ this_range_str + '/'
        IF iset EQ 0 THEN sort_flag = 1.0 * ((data.GSM_X le -1 and data.bx_gsm le 0) or (data.GSM_X gt -1 and data.gsm_z le 0)) ELSE sort_flag = 1.0 * ((data.GSM_X le -1 and data.bx_gsm gt 0) or (data.GSM_X gt -1 and data.gsm_z gt 0)) 
        
        index = WHERE(sort_flag EQ 0, ct)
        IF ct GT 0 THEN sort_flag[index] = !VALUES.F_NAN
        
        make_o_beam_map, data, header $
                         , sort_flag = sort_flag, sort_title = sort_title $
                         , ps_plot = ps_plot, plot_path = sort_path $
                         , grid = grid, slice_grid = slice_grid $
                         , point_plot = point_plot, events_map = events_map $
                         , make_table=make_table $
                         , plot_2d= plot_2d, plot_slice = plot_slice $
                         , region_map_set = region_map_set $
                         , direction_set = direction_set $
                         , storm_phase_set = storm_phase_set $
                         , substorm_phase_set = substorm_phase_set $
                         , coor_set = coor_set $
                         , property_map_set = property_map_set $
                         , property_map_type_set = property_map_type_set $
                         , energy_set = energy_set $
                         , imfBz_set = imfBz_set $
                         , imfBy_set = imfBy_set $
                         , swP_set = swP_set $
                         , swV_set = swV_set 
     ENDFOR 
  ENDIF 


  IF KEYWORD_SET(sort_bx_map) THEN BEGIN
     bx_set = ['le15', 'gt15']
     bx_title_set = ['le15', 'gt15']
     FOR iset = 0, N_ELEMENTS(bx_set)-1 DO BEGIN
        this_range_str = bx_set[iset]
        this_title_set = bx_title_set[iset]
        sort_title =  this_title_set
        sort_path = plot_path +'bx_sort_map/bx_'+ this_range_str + '/'
        IF iset EQ 0 THEN sort_flag = 1.0 * (ABS(data.bx_gsm) le 15) ELSE sort_flag = 1.0 * (ABS(data.bx_gsm) gt 15)
        
        index = WHERE(sort_flag EQ 0, ct)
        IF ct GT 0 THEN sort_flag[index] = !VALUES.F_NAN
        
        make_o_beam_map, data, header $
                         , sort_flag = sort_flag, sort_title = sort_title $
                         , ps_plot = ps_plot, plot_path = sort_path $
                         , grid = grid, slice_grid = slice_grid $
                         , point_plot = point_plot, events_map = events_map $
                         , make_table=make_table $
                         , plot_2d= plot_2d, plot_slice = plot_slice $
                         , region_map_set = region_map_set $
                         , direction_set = direction_set $
                         , storm_phase_set = storm_phase_set $
                         , substorm_phase_set = substorm_phase_set $
                         , coor_set = coor_set $
                         , property_map_set = property_map_set $
                         , property_map_type_set = property_map_type_set $
                         , energy_set = energy_set $
                         , imfBz_set = imfBz_set $
                         , imfBy_set = imfBy_set $
                         , swP_set = swP_set $
                         , swV_set = swV_set 
     ENDFOR 
  ENDIF

  IF KEYWORD_SET(sort_dist_map) THEN BEGIN
     dist_set = ['gt7']
     dist_title_set = ['gt7']
     FOR iset = 0, N_ELEMENTS(dist_set)-1 DO BEGIN
        this_range_str = dist_set[iset]
        this_title_set = dist_title_set[iset]
        sort_title =  this_title_set
        sort_path = plot_path +'dist_sort_map/dist_'+ this_range_str + '/'
        IF iset EQ 0 THEN sort_flag = 1.0 * (ABS(data.dist) gt 7) ELSE sort_flag = 1.0
        
        index = WHERE(sort_flag EQ 0, ct)
        IF ct GT 0 THEN sort_flag[index] = !VALUES.F_NAN
        
        make_o_beam_map, data, header $
                         , sort_flag = sort_flag, sort_title = sort_title $
                         , ps_plot = ps_plot, plot_path = sort_path $
                         , grid = grid, slice_grid = slice_grid $
                         , point_plot = point_plot, events_map = events_map $
                         , make_table=make_table $
                         , plot_2d= plot_2d, plot_slice = plot_slice $
                         , region_map_set = region_map_set $
                         , direction_set = direction_set $
                         , storm_phase_set = storm_phase_set $
                         , substorm_phase_set = substorm_phase_set $
                         , coor_set = coor_set $
                         , property_map_set = property_map_set $
                         , property_map_type_set = property_map_type_set $
                         , energy_set = energy_set $
                         , imfBz_set = imfBz_set $
                         , imfBy_set = imfBy_set $
                         , swP_set = swP_set $
                         , swV_set = swV_set 
     ENDFOR 
  ENDIF

  IF KEYWORD_SET(sliced_with_input_range) THEN BEGIN
     plot_2d = 0
     plot_slice = 1
;     coor_set_set = [['X_GSM', 'Z_GSM', 'Y_GSM'],['X_GSM', 'Z_GSM', 'Y_GSM'],['X_GSM', 'Z_GSM', 'Y_GSM'], ['Y_GSM', 'Z_GSM', 'X_GSM'], ['Y_GSM', 'Z_GSM', 'X_GSM']]
;     slice_grid_set = [15.,10.,15,15,10]
;     input_range_set = [[-20.,5.],[-5.,5.],[5.,20.], [-30.,15.],[-15.,5.]]
     coor_set_set = [['Y_GSM', 'Z_GSM', 'X_GSM']]
     slice_grid_set = [20.]
     input_range_set = [[-30.,-10.]]
     
     FOR iset = 0, N_ELEMENTS(slice_grid_set)-1 DO BEGIN
        coor_set = coor_set_set[*,iset]
        slice_grid = slice_grid_set[iset]
        range_input = input_range_set[*,iset]
        
        sort_title =  ''
        sort_path = plot_path +'non_sort_map/'
        
        make_o_beam_map, data, header $
                         , sort_flag = sort_flag, sort_title = sort_title $
                         , ps_plot = ps_plot, plot_path = sort_path $
                         , grid = grid, slice_grid = slice_grid $
                         , point_plot = point_plot, events_map = events_map $
                         , make_table=make_table $
                         , plot_2d= plot_2d, plot_slice = plot_slice $
                         , region_map_set = region_map_set $
                         , direction_set = direction_set $
                         , storm_phase_set = storm_phase_set $
                         , substorm_phase_set = substorm_phase_set $
                         , coor_set = coor_set $
                         , property_map_set = property_map_set $
                         , property_map_type_set = property_map_type_set $
                         , energy_set = energy_set $
                         , imfBz_set = imfBz_set $
                         , imfBy_set = imfBy_set $
                         , swP_set = swP_set $
                         , swV_set = swV_set $
                         , range_input = range_input
     ENDFOR 

     
     
     

  ENDIF 

  
END 
