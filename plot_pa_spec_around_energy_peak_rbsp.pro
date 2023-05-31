;---------------------------------------------------------------
;Purpose: Plot pitch angle around with given energy range and store pitch angle peak into tplot string
;
;Inputs:  sc, sp, units_name, epcut_name, erange_name, pa_range
;Keywords:pa_name, average_time, start_time, END_time, bin_size_pa
;
;---------------------------------------------------------------

PRO plot_pa_spec_around_energy_peak_rbsp, sc, sp, units_name, epcut_name, erange_name, pa_range = pa_range, pa_name = pa_name, average_time = average_time, bin_size_pa = bin_size_pa

;----------------------------------------------------------
; check keywords and set the energybins range to nrange           
;---------------------------------------------------------
  IF NOT KEYWORD_SET(bin_size_pa) THEN bin_size_pa = 22.5

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
  

  IF NOT KEYWORD_SET(average_time) EQ 0 THEN average_time = 300 ;default average time: 300s

;  sc_str = STRING(sc, FORMAT = '(i1.1)')
;---------------------------------------------------------
; Get energy peak and energy range
;---------------------------------------------------------
  get_data, epcut_name, data = data
  time_ep = data.x
  energy_peak = data.y
  n_time = N_ELEMENTS(time_ep)
;  n_time = floor((end_time-start_time)/average_time)

  energybins = data.energybins         
  nenergybins = N_ELEMENTS(energybins)

  get_data, erange_name, data = data
  energy_range = data.y

;-- get the full timespan  
  get_timespan, interval
  start_time = interval(0)
  end_time = interval(1)
;find energy range
;  energy_range = DBLARR(2, n_time)

;store the energy range into a 2D array according to energybins range(n_range)
;  IF n_range EQ 0 THEN BEGIN 
;     energy_range(0, *) = energy_peak
;     energy_range(1, *) = energy_peak
;  ENDIF ELSE BEGIN 
;     FOR jjj = 0, n_time-1 DO BEGIN 
;        ebin = where(round(energybins) EQ round(energy_peak(jjj)))
;        IF ebin NE -1 THEN BEGIN 
;           energy_range(0, jjj) = energybins((ebin+n_range) < (nenergybins-1))
;           energy_range(1, jjj) = energybins((ebin-n_range) > 0)
;        ENDIF ELSE BEGIN 
;           energy_range(*, jjj) = !VALUES.F_NAN
;        ENDELSE 
;     ENDFOR 
;  ENDELSE

;-----------------------------------------------------------------
;plot pa in the energy range and store pa specta data
;-----------------------------------------------------------------
  n_pa_bins = 180./bin_size_pa
  time_pa = time_ep
  flux_pa = DBLARR(n_time, n_pa_bins)
  pa_pa = DBLARR(n_time, n_pa_bins)

; plot pa with energy_range
; May need to check no magnetic field error n_mag_error = 0

  FOR jjj = 0, n_time-1 DO BEGIN 
     time = start_time + jjj * average_time 
     ts = (time -average_time/2) > time_double('2013-01-01')
     index = where(time_ep GT time AND time_ep LE time + average_time, ct)
     IF ct GE 2 THEN stop ELSE loc = index(0)
     
     IF TOTAL(energy_range(loc, *),/NAN) GT 0 THEN BEGIN 

        energy = [min(energy_range(loc, *)),max(energy_range(loc, *))]
        timespan,ts, average_time, /SECONDS

        plot_rbsp_hope_l3_paspec, sc, sp, units_name, energy_range = energy, energy_units_in_flux = 'keV', eph_data=0, aux_data=0

        e_min = STRCOMPRESS(STRING(energy(0), FORMAT = '(i5.5)'), /REMOVE_ALL)
        e_max = STRCOMPRESS(STRING(energy(1), FORMAT = '(i5.5)'), /REMOVE_ALL)
       
        s_pa_name = 'RBSP' + sc_str + $
                    '_HOPE_l3_paspec_sp' + sp_str + $
                    '_' + units_str + '_' + e_min + '_' + e_max
      
        tplot_names, s_pa_name, names = names
        IF names(0) NE '' THEN BEGIN 
           time_trim_tplot_variable, s_pa_name, time-average_time/2, time+average_time
           get_data, s_pa_name, data = s_pa, dlim = dlim, lim = lim
          
           s_time_pa = s_pa.x
           s_flux_pa = s_pa.y
           s_pa_pa = s_pa.v
           
           store_data, delete = s_pa_name

;A check point to verify the number of pitch angle bins is  the same
;as defined. At this moment we don't know the data details
           IF N_ELEMENTS(s_pa_pa(0,*)) NE n_pa_bins THEN x_data = s_pa_pa ELSE  x_data = REFORM(s_pa_pa(0, *))

;average flux data over average_time and save them into arrays 
           IF units_name EQ 'DIFF FLUX' THEN  y_data = TOTAL(s_flux_pa(*, *),2 ,/Nan)/TOTAL(FINITE(s_flux_pa(*, 0)),/NAN)
           IF units_name EQ 'EFLUX' THEN   y_data = TOTAL(s_flux_pa(*, *), 2,/Nan)
      ;          stop  
           if N_elements(y_data) ne N_elements(flux_pa(loc,*)) then begin
              print,  N_elements(y_data)
              print, x_data
              continue
           endif
           flux_pa(loc, *) = y_data
           pa_pa(loc, * ) = x_data

;if pitch angle range is set
           IF KEYWORD_SET(pa_range) THEN BEGIN 
              index = where(FINITE(pa_pa(*,0)))
              pitch_angle = pa_pa(index(0),*)
              index = where(pitch_angle LT pa_range(0) OR pitch_angle GT pa_range(1), ct)
              IF ct GT 0 THEN flux_pa(*, index) = !VALUES.F_NAN
           ENDIF 

        ENDIF ELSE BEGIN
           flux_pa(loc, *) = !VALUES.F_NAN
           pa_pa(loc, * ) = !VALUES.F_NAN
        ENDELSE 
     ENDIF   ELSE BEGIN 
        flux_pa(loc, *) = !VALUES.F_NAN
        pa_pa(loc, * ) = !VALUES.F_NAN
     ENDELSE 

  ENDFOR    

; store pitch angle tplot
  str = {x: time_pa, y: flux_pa, v: pa_pa}
  store_data, pa_name, data = str, dlim = dlim, lim = lim
  options, pa_name, 'ytitle', 'PA!CEN Peak'
  zlim, pa_name, 0.1, 100

;set the timespan as before 
  timespan, interval(0), interval(1)-interval(0), /SECONDS

END
