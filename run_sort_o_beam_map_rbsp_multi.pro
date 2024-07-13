;----------------------------------------------------------------
; 2016-2020 original
;----------------------------------------------------------------
pro run_Beta_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid= grid, ratio_correction = ratio_correction 
  sort_o_beam_map_rbsp_multi, store_tplot = 1 $   
                             , time_start=time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat $
                             , non_sort_map = 1 $
                             , plot_2d = 1 $
                             , direction_set = ['outflow']  $ ;,'outflow'] ;['any', 'para','anti','both','outflow'] $
                             , region_map_set =  [ 'Tail']  $ ;, 'Tail'] ;,'BetaLE005','all','Dayside',] $
                             , property_map_set = ['energy'] $
                             , events_map = 1 $ 
                             , coor_set = [['X_GSM','Beta','MLT'],['DIST','Beta','MLT']] $
                             , ps_plot = 1 $
                             , subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid= grid, ratio_correction = ratio_correction 
end

pro run_substorm_sort_2d_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid= grid, ratio_correction = ratio_correction 
  sort_o_beam_map_rbsp_multi, store_tplot = 1 $   
                             , time_start=time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat $
                             , non_sort_map = 1 $
                             , plot_2d = 1 $
                             , direction_set = ['outflow']  $                 ;,'outflow'] ;['any', 'para','anti','both','outflow'] $
                             , region_map_set =  [ 'all','Lobe', 'BL', 'PS']  $ ;, 'Tail'] ;,'BetaLE005','all','Dayside',] $
                             , property_map_set = ['energy'] $
                             , events_map = 1 $ 
                             , coor_set = [['X_GSM', 'Y_GSM', 'Z_GSM'], ['X_GSM', 'Z_GSM', 'Y_GSM'],['Y_GSM','Z_GSM','X_GSM'],['X_GSM','Beta','MLT'],['DIST','Beta','MLT']] $
                             , ps_plot = 1 $
                             , substorm_phase_set =  ['all', 'substorm_time', 'non_substorm_time']  $
                                               , subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold  , grid= grid , ratio_correction = ratio_correction    
end
;----------------------------------------------------------------
pro run_storm_sort_2d_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid= grid, ratio_correction = ratio_correction 
  sort_o_beam_map_rbsp_multi, store_tplot = 1 $   
                             , time_start=time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat $
                             , non_sort_map = 1 $
                             , plot_2d = 1 $
                             , direction_set = ['outflow']  $                 ;,'outflow'] ;['any', 'para','anti','both','outflow'] $
                             , region_map_set =  [ 'all','Lobe', 'BL', 'PS']  $ ;, 'Tail'] ;,'BetaLE005','all','Dayside',] $
                             , property_map_set = ['energy'] $
                             , events_map = 1 $ 
                             , coor_set = [['X_GSM', 'Y_GSM', 'Z_GSM'], ['X_GSM', 'Z_GSM', 'Y_GSM'],['Y_GSM','Z_GSM','X_GSM'],['X_GSM','Beta','MLT'],['DIST','Beta','MLT']] $
                             , ps_plot = 1 $
                             , storm_phase_set= ['storm_time', 'nonstorm_time'] $
                                       , subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction              
end
;----------------------------------------------------------------
pro run_kp_sort_2d_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction
  sort_o_beam_map_rbsp_multi, store_tplot = 1 $   
                             , time_start=time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat $
                             , sort_kp_map = 1 $
                             , plot_2d = 1 $
                             , direction_set = ['outflow']  $                 ;,'outflow'] ;['any', 'para','anti','both','outflow'] $
                             , region_map_set =  [ 'all','Lobe', 'BL', 'PS']  $ ;, 'Tail'] ;,'BetaLE005','all','Dayside',] $
                             , property_map_set = ['energy'] $
                             , events_map = 1 $ 
                             , coor_set = [['X_GSM', 'Y_GSM', 'Z_GSM'], ['X_GSM', 'Z_GSM', 'Y_GSM'],['Y_GSM','Z_GSM','X_GSM'],['X_GSM','Beta','MLT'],['DIST','Beta','MLT']] $
                             , ps_plot = 1 $
;                       , swV_set=  [[0.,450.],[450.,900]] $
                                      , subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction               
end
;----------------------------------------------------------------

pro run_swV_sort_2d_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction
  sort_o_beam_map_rbsp_multi, store_tplot = 1 $   
                             , time_start=time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat $
                             , non_sort_map = 1 $
                             , plot_2d = 1 $
                             , direction_set = ['outflow']  $                 ;,'outflow'] ;['any', 'para','anti','both','outflow'] $
                             , region_map_set =  [ 'all','Lobe', 'BL', 'PS']  $ ;, 'Tail'] ;,'BetaLE005','all','Dayside',] $
                             , property_map_set = ['energy'] $
                             , events_map = 1 $
                             , coor_set = [['X_GSM', 'Y_GSM', 'Z_GSM'], ['X_GSM', 'Z_GSM', 'Y_GSM'],['Y_GSM','Z_GSM','X_GSM'],['X_GSM','Beta','MLT'],['DIST','Beta','MLT']] $
                             , ps_plot = 1 $
                             , swV_set=  [[0.,450.],[450.,900]] $
                                 , subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction                    
end
;----------------------------------------------------------------
pro run_energy_sort_2d_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction
  sort_o_beam_map_rbsp_multi, store_tplot = 1 $   
                             , time_start=time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat $
                             , non_sort_map = 1 $
                             , plot_2d = 1 $
                             , direction_set = ['outflow']  $                 ;,'outflow'] ;['any', 'para','anti','both','outflow'] $
                             , region_map_set =  [ 'all','Lobe', 'BL', 'PS']  $ ;, 'Tail'] ;,'BetaLE005','all','Dayside',] $
                             , property_map_set = ['energy'] $
                             , events_map = 1 $
                             , coor_set = [['X_GSM', 'Y_GSM', 'Z_GSM'], ['X_GSM', 'Z_GSM', 'Y_GSM'],['Y_GSM','Z_GSM','X_GSM'],['X_GSM','Beta','MLT'],['DIST','Beta','MLT']] $
                             , ps_plot = 1 $
                             , energy_set= [[1.,100], [100., 1000.],[1000.,3000.],[3000.,40000]]  $
                                    , subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction            
end
;----------------------------------------------------------------
pro run_swP_sort_2d_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction
  sort_o_beam_map_rbsp_multi, store_tplot = 1 $   
                             , time_start=time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat $
                             , non_sort_map = 1 $
                             , plot_2d = 1 $
                             , direction_set = ['outflow']  $                 ;,'outflow'] ;['any', 'para','anti','both','outflow'] $
                             , region_map_set =  [ 'all','Lobe', 'BL', 'PS']  $ ;, 'Tail'] ;,'BetaLE005','all','Dayside',] $
                             , property_map_set = ['energy'] $
                             , events_map = 1 $
                             , coor_set = [['X_GSM', 'Y_GSM', 'Z_GSM'], ['X_GSM', 'Z_GSM', 'Y_GSM'],['Y_GSM','Z_GSM','X_GSM'],['X_GSM','Beta','MLT'],['DIST','Beta','MLT']] $
                             , ps_plot = 1 $
                             , swP_set=  [[0.,2.],[2.,20]] $
                                  , subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction             
end
;----------------------------------------------------------------
pro run_imfbybz_sort_sliced_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction
  sort_o_beam_map_rbsp_multi, store_tplot = 1 $
                             , time_start=time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat $
                             , sliced_with_input_range = 1 $
                                ;                     , non_sort_map = 1 $
                             , plot_slice = 1 $
                             , direction_set = ['outflow'] $                  ;,'outflow'] ;['any', 'para','anti','both','outflow'] $
                             , region_map_set =  [ 'all','Lobe', 'BL', 'PS', 'Tail'] $ ;,'BetaLE005','all','Dayside',] $
                             , property_map_set = ['energy'] $
                             , events_map = 1 $
                             , coor_set = [['X_GSM', 'Y_GSM', 'Z_GSM'], ['X_GSM', 'Z_GSM', 'Y_GSM'],['Y_GSM','Z_GSM','X_GSM']] $ ;,['X_GSM','Beta','MLT'],['DIST','Beta','MLT']] $
                             , ps_plot = 1 $
                             , imfBy_set= [[-25,0],[0,25]] $
                             , imfBz_set= [[-25,0],[0,25]] $
                                  , subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction             
end
;----------------------------------------------------------------
pro run_imfby_sort_sliced_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction
  sort_o_beam_map_rbsp_multi, store_tplot = 1 $
                             , time_start=time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat $
                             , sliced_with_input_range = 1 $
;                       , non_sort_map = 1 $
                             , plot_slice = 1 $
                             , direction_set = ['outflow'] $                  ;,'outflow'] ;['any', 'para','anti','both','outflow'] $
                             , region_map_set =  [ 'all','Lobe', 'BL', 'PS', 'Tail']$ ;,'BetaLE005','all','Dayside',] $
                             , property_map_set = ['energy'] $
                             , events_map = 1 $
                             , coor_set = [['X_GSM', 'Y_GSM', 'Z_GSM'], ['X_GSM', 'Z_GSM', 'Y_GSM'],['Y_GSM','Z_GSM','X_GSM']] $ ;,['X_GSM','Beta','MLT'],['DIST','Beta','MLT']] $
                             , ps_plot = 1 $
                             , imfBy_set= [[-25,0],[0,25]]  $
                                 , subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction              
end
;----------------------------------------------------------------
pro run_imfbybz_sort_2d_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction
  sort_o_beam_map_rbsp_multi, store_tplot = 1 $   
                             , time_start=time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat $
                             , non_sort_map = 1 $
                             , plot_2d = 1 $
                             , direction_set = ['outflow']  $                 ;,'outflow'] ;['any', 'para','anti','both','outflow'] $
                             , region_map_set =  [ 'all','Lobe', 'BL', 'PS', 'Tail']$ ;,'BetaLE005','all','Dayside',] $
                             , property_map_set = ['energy'] $
                             , events_map = 1 $
                             , coor_set = [['X_GSM', 'Y_GSM', 'Z_GSM'], ['X_GSM', 'Z_GSM', 'Y_GSM'],['Y_GSM','Z_GSM','X_GSM'],['X_GSM','Beta','MLT'],['DIST','Beta','MLT']] $
                             , ps_plot = 1 $
                             , imfBy_set= [[-25,0],[0,25]] $
                             , imfBz_set= [[-25,0],[0,25]]  $
                                , subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction               
end
;----------------------------------------------------------------
pro run_imfby_sort_2d_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction
  sort_o_beam_map_rbsp_multi, store_tplot = 1 $   
                             , time_start=time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat $
                             , non_sort_map = 1 $
                             , plot_2d = 1 $
                             , direction_set = ['outflow']  $                 ;,'outflow'] ;['any', 'para','anti','both','outflow'] $
                             , region_map_set =  [ 'all','Lobe', 'BL', 'PS', 'Tail']$ ;,'BetaLE005','all','Dayside',] $
                             , property_map_set = ['energy'] $
                             , events_map = 1 $
                             , coor_set = [['X_GSM', 'Y_GSM', 'Z_GSM'], ['X_GSM', 'Z_GSM', 'Y_GSM'],['Y_GSM','Z_GSM','X_GSM'],['X_GSM','Beta','MLT'],['DIST','Beta','MLT']] $
                             , ps_plot = 1 $
                             , imfBy_set= [[-25,0],[0,25]] $
                                    , subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction            
end
;----------------------------------------------------------------
pro run_hemi_imfby_sort_2d_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction
  sort_o_beam_map_rbsp_multi, store_tplot = 1 $   
                             , time_start=time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat $
                             , sort_hemi_map = 1 $
                             , plot_2d = 1 $
                             , direction_set = ['outflow']  $                 ;,'outflow'] ;['any', 'para','anti','both','outflow'] $
                             , region_map_set =  [ 'all','Lobe', 'BL', 'PS', 'Tail'] $ ;,'BetaLE005','all','Dayside',] $
                             , property_map_set = ['energy'] $
                             , events_map = 1 $
                             , coor_set = [['X_GSM', 'Y_GSM', 'Z_GSM']] $
                             , ps_plot = 1 $
                             , imfBy_set= [[-25,0],[0,25]]  $
                                , subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction               
end
;----------------------------------------------------------------
pro run_hemi_sort_2d_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction
  sort_o_beam_map_rbsp_multi, store_tplot = 1 $   
                             , time_start=time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat $
                             , sort_hemi_map = 1 $
                             , plot_2d = 1 $
                             , direction_set = ['outflow']  $                 ;,'outflow'] ;['any', 'para','anti','both','outflow'] $
                             , region_map_set =  [ 'all','Lobe', 'BL', 'PS', 'Tail'] $ ;,'BetaLE005','all','Dayside',] $
                             , property_map_set = ['energy'] $
                             , events_map = 1 $
                             , coor_set = [['X_GSM', 'Y_GSM', 'Z_GSM']] $
                             , ps_plot = 1 $
                                  , subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction             
end
;---------------------------------------------------------------
 pro run_hemi_sort_sliced_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction
   sort_o_beam_map_rbsp_multi, store_tplot = 1 $
                              , time_start=time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat $
                              , hemi_sort_sliced = 1 $
                              , direction_set = ['outflow'] $                  ;,'outflow'] ;['any', 'para','anti','both','outflow'] $
                              , region_map_set =  [ 'all','Lobe', 'BL', 'PS', 'Tail'] $ ;,'BetaLE005','all','Dayside',] $
 ;                             , property_map_set = ['energy'] $
                              , events_map = 1 $
;                              , coor_set = [['X_GSM', 'Y_GSM', 'Z_GSM'], ['X_GSM', 'Z_GSM', 'Y_GSM'],['Y_GSM','Z_GSM','X_GSM']] $ ;,['X_GSM','Beta','MLT'],['DIST','Beta','MLT']] $
                              , ps_plot = 1 $
                                   , subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction             
 end 
;---------------------------------------------------------------
pro run_non_sort_sliced_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction
  sort_o_beam_map_rbsp_multi, store_tplot = 1 $
                             , time_start=time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat $
                             , sliced_with_input_range = 1 $
;                       , non_sort_map = 0 $
                             , plot_slice = 1 $
                             , direction_set = ['outflow'] $                  ;,'outflow'] ;['any', 'para','anti','both','outflow'] $
                             , region_map_set =  [ 'all','Lobe', 'BL', 'PS']  $ ;, 'Tail'] ;,'BetaLE005','all','Dayside',] $
                             , property_map_set = ['energy'] $
                             , events_map = 1 $
                             , coor_set = [['X_GSM', 'Y_GSM', 'Z_GSM'], ['X_GSM', 'Z_GSM', 'Y_GSM'],['Y_GSM','Z_GSM','X_GSM']] $ ;,['X_GSM','Beta','MLT'],['DIST','Beta','MLT']] $
                             , ps_plot = 1 $
                                  , subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction             
end

;----------------------------------------------------------------
pro run_non_sort_2d_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction , flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction
  sort_o_beam_map_rbsp_multi, store_tplot = 1 $   
                             , time_start=time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat $
                             , non_sort_map = 1 $
                             , plot_2d = 1 $
                             , direction_set = ['outflow']  $                        ;,'outflow'] ;['any', 'para','anti','both','outflow'] $
                             , region_map_set =  [ 'all','Lobe', 'BL', 'PS', 'Tail'] $ ;,'BetaLE005','all','Dayside',] $
                             , property_map_set = ['energy'] $
                             , events_map = 1 $
                             , coor_set = [['X_GSM', 'Y_GSM', 'Z_GSM'], ['X_GSM', 'Z_GSM', 'Y_GSM'],['Y_GSM','Z_GSM','X_GSM'],['X_GSM','Beta','MLT'],['DIST','Beta','MLT'], ['L','Beta','MLT']] $
                             , ps_plot = 1 $
                             , subtraction = subtraction , flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction             
end

;----------------------------------------------------------------
pro run_non_sort_2d_property_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction , flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction
  sort_o_beam_map_rbsp_multi, store_tplot = 1 $   
                             , time_start=time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat $
                             , non_sort_map = 1 $
                             , plot_2d = 1 $
                             , direction_set = ['outflow']  $                        ;,'outflow'] ;['any', 'para','anti','both','outflow'] $
                             , region_map_set =  [ 'all','Lobe', 'BL', 'PS', 'Tail'] $ ;,'BetaLE005','all','Dayside',] $
                             , property_map_set = ['energy'] $
                             , events_map = 0 $
                             , coor_set = [['X_GSM', 'Y_GSM', 'Z_GSM'], ['X_GSM', 'Z_GSM', 'Y_GSM'],['Y_GSM','Z_GSM','X_GSM'],['X_GSM','Beta','MLT'],['DIST','Beta','MLT'], ['L','Beta','MLT']] $
                             , ps_plot = 1 $
                             , subtraction = subtraction , flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction             
end

;----------------------------------------------------------------
pro run_non_sort_points_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction
  
  sort_o_beam_map_rbsp_multi, store_tplot = 1 $    
                             , time_start=time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat $
                             , non_sort_map = 1 $
                             , direction_set = ['outflow'] $   
                             , region_map_set =  ['Lobe''Lobe', 'BL', 'PS','Tail'] $ 
                             , point_plot = 1 $
                             , coor_set = [['X_GSM', 'Y_GSM', 'Z_GSM'], ['X_GSM', 'Z_GSM', 'Y_GSM'],['Y_GSM','Z_GSM','X_GSM'],['X_GSM','Beta','MLT'],['DIST','Beta','MLT']] $
                             , ps_plot = 1 $
                               , subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction                        
end

;-------------------------------------------
; main programs
;-----------------------------------------
pro run_sort_o_beam_map_rbsp_multi, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat , subtraction = subtraction , flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction

  if ~keyword_set(flux_threshold) then flux_threshold = [0.5,0.75,1]
  if ~keyword_set(def_pap_factor) then def_pap_factor = [3,2,1.1]
  if ~keyword_set(average_time) then average_time = 300
  if ~keyword_set(time_start) then time_start = '2013-01-01'
  if ~keyword_set(time_end) then time_end = '2019-11-01'
  if ~KEYWORD_SET(subtraction) then subtraction =1
  if ~KEYWORD_SET(multi_peak) then multi_peak = 1
  if ~KEYWORD_SET(remove_directional_pa) then remove_directional_pa = 1
  if ~KEYWORD_SET(sample_size_threshold) then sample_size_threshold = 27
  if ~KEYWORD_SET(grid) then grid = 2
  
;  run_non_sort_points_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction
  
;  run_non_sort_2d_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction, flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction

;  run_non_sort_sliced_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction , flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction
  
;  run_hemi_sort_2d_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction , flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction

;  run_hemi_sort_sliced_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction , flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction
  
;  run_hemi_imfby_sort_2d_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction , flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction
  
;  run_imfby_sort_2d_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction , flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction
  
;  run_imfbybz_sort_2d_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction , flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction
  
;  run_imfby_sort_sliced_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction , flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction

;  run_imfbybz_sort_sliced_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction , flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction

;  run_swP_sort_2d_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction , flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction

;  run_energy_sort_2d_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction , flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction
  
;  run_swV_sort_2d_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction , flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction

;  run_storm_sort_2d_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction , flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction

;  run_kp_sort_2d_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction , flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction

;  run_Beta_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction , flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction

;-- only runs for property maps --
;  run_non_sort_2d_property_map_rbsp, time_start = time_start, time_end = time_end, stop = stop, read_from_dat = read_from_dat, subtraction = subtraction , flux_threshold = flux_threshold, def_pap_factor = def_pap_factor , average_time = average_time , multi_peak = multi_peak , remove_bidirectional_pa = remove_directional_pa,  sample_size_threshold = sample_size_threshold, grid = grid, ratio_correction = ratio_correction

end 
