;---------------------------------------------------------------
;Purpose: Plot pitch angle around with given energy range and store pitch angle peak into tplot string
;
;Inputs:  sc, sp, units_name, epcut_name, erange_name, pa_range
;Keywords:pa_name, average_time, start_time, END_time, bin_size_pa
;
;---------------------------------------------------------------

PRO plot_pa_spec_around_energy_peak_rbsp, sc, sp, units_name, epcut_name, erange_name, pa_range = pa_range, pa_name = pa_name, average_time = average_time,  n_pa_bins = n_pa_bins, time_avg = time_avg

;----------------------------------------------------------
; check keywords and set the energybins range to nrange           
;---------------------------------------------------------
  n_pa_bins = 11

  RBSP_ENERGY_BIN = [51767.680,44428.695,38130.121,32724.498,28085.268,24103.668,20686.559,17753.877,15236.896,13076.798,11222.936,9631.8994,8266.4062,7094.5161,6088.7222,5225.5273,4484.7422,3848.9187,3303.2842,2834.9644,2433.0547,2088.1287,1792.0959,1538.0620,1319.9771,1132.8461,972.23694,834.42133,716.16302,614.57758,527.48431,452.70224,388.54303,333.45901,286.18381,245.59184,210.76859,180.86984,155.26245,133.24290,114.31875,98.138245,84.208954,72.319794,62.048695,53.254951,45.727501,39.184952,33.627300,28.913851,24.763199,21.245699,18.290998,15.688050,13.436850,11.537399,9.9193497,8.5123501,7.3163996,6.2611499,5.3465996,4.6431003,3.9396000,3.3767998,2.9546998,2.5326002,2.1808500,1.8290999,1.5476999,1.3366499,1.1959500,0.98490000]

  RBSP_ENERGY_BIN_INT = ROUND(RBSP_ENERGY_BIN)
  
  pa_pa_defined = [4.5000000,18.000000,36.000000,54.000000,72.000000,90.000000,108.00000,126.00000,144.00000,162.00000,175.50000]

;  IF ~KEYWORD_SET(bin_size_pa) THEN bin_size_pa = 22.5
  if ~KEYWORD_SET(n_pa_bins) then n_pa_bins = n_pa_bins else  n_pa_bins = 11 ; = 180./bin_size_pa

;This units is for pitch angle tlot names of mms
  CASE units_name OF 
     'DIFF FLUX': units_str = 'DIFFFLUX' 
;     'Counts':units_str = 'COUNTS'  
     'EFLUX': units_str = 'EFLUX'
  ENDCASE
;This units is for pitch angle tlot names of mms
  CASE sp OF 
     '3': sp_str = '3'  
     '0': sp_str = '0'
  ENDCASE

  CASE sc OF 
     '1': sc_str = 'A'  
     '2': sc_str = 'B'
  ENDCASE
  
  IF ~KEYWORD_SET(average_time) EQ 0 THEN average_time = 300 ;default average time: 300s

;  sc_str = STRING(sc, FORMAT = '(i1.1)')
;---------------------------------------------------------
; Get energy peak and energy range
;---------------------------------------------------------
  get_data, epcut_name, data = data
  time_ep = data.x
  energy_peak = data.y
  n_time = N_ELEMENTS(time_ep)

  energybins = data.energybins         
  nenergybins = N_ELEMENTS(energybins)

  get_data, erange_name, data = data
  energy_range = data.y

;-- get the full timespan  
  get_timespan, interval
;-----------------------------------------------------------------
;plot pa in the energy range and store pa specta data
;-----------------------------------------------------------------
  time_pa = time_ep
  flux_pa = DBLARR(n_time, n_pa_bins)
  pa_pa = DBLARR(n_time, n_pa_bins)

  flux_pa(*) = !VALUES.F_NAN
  pa_pa(*) = !VALUES.F_NAN
  
; plot pa with energy_range
; May need to check no magnetic field error n_mag_error = 0
  FOR jjj = 0, n_time-1 DO BEGIN 
     time = start_time + jjj * average_time 
 ;    ts = (time - average_time/2) > time_double('2013-01-01')
     index = where(time_ep GT time AND time_ep LE time + average_time, ct)
     IF ct GE 2 THEN stop ELSE loc = index(0)
     
     IF TOTAL(energy_range(loc, *),/NAN) GT 0 THEN BEGIN 
        energy = [min(energy_range(loc, *)),max(energy_range(loc, *))]

        timespan, time, average_time, /SECONDS

        plot_rbsp_hope_l3_paspec, sc, sp, units_name, energy_range = energy, energy_units_in_flux = 'eV', eph_data=0, aux_data=0

        e_min = STRCOMPRESS(STRING(energy(0), FORMAT = '(i5.5)'), /REMOVE_ALL)
        e_max = STRCOMPRESS(STRING(energy(1), FORMAT = '(i5.5)'), /REMOVE_ALL)
       
        s_pa_name = 'RBSP' + sc_str + $
                    '_HOPE_l3_paspec_sp' + sp_str + $
                    '_' + units_str + '_' + e_min + '_' + e_max
      
        tplot_names, s_pa_name, names = names
        IF names(0) NE '' THEN BEGIN 
           time_trim_tplot_variable, s_pa_name, time, time+average_time, keep_v = 1
           get_data, s_pa_name, data = s_pa, dlim = dlim, lim = lim
          
           s_time_pa = s_pa.x
           s_flux_pa = s_pa.y
           s_pa_pa = s_pa.v
           
           store_data, delete = s_pa_name

;A check point to verify the number of pitch angle bins is  the same
;as defined. At this moment we don't know the data details
           IF N_ELEMENTS(s_flux_pa(0,*)) NE n_pa_bins THEN stop

;average flux data over average_time and save them into arrays 
            IF units_name EQ 'DIFF FLUX' THEN  y_data =  MEAN(s_flux_pa[*,*],DIM=1,/Nan)
            IF units_name EQ 'EFLUX' THEN y_data = TOTAL(s_flux_pa(*, *), 1,/Nan)
     
           if N_elements(y_data) ne N_elements(flux_pa(loc,*)) then begin
              print,  N_elements(y_data)
              print, x_data
              continue
           endif
           flux_pa(loc, *) = y_data
           pa_pa(loc, * ) = x_data

;if pitch angle range is set
           IF KEYWORD_SET(pa_range) THEN BEGIN 
              pitch_angle = s_pa_pa(0,*)
              index = where(pitch_angle LT pa_range(0) OR pitch_angle GT pa_range(1), ct)
              IF ct GT 0 THEN flux_pa(*, index) = !VALUES.F_NAN
           ENDIF 

        ENDIF
  ENDFOR    

; store pitch angle tplot
  str = {x: time_pa, y: flux_pa, v: pa_pa}
  store_data, pa_name, data = str, dlim = dlim, lim = lim
  options, pa_name, 'ytitle', 'PA!CEN Peak'
  zlim, pa_name, 0.1, 100

;set the timespan as before 
  timespan, interval(0), interval(1)-interval(0), /SECONDS

END
