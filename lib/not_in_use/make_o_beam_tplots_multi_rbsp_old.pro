; ------------------------------------------------------------------
; Purpose: make tplots in idl and/or ps for streaming O+
; Inputs: sc_str, t_s, t_e, t_dt, output_path, all_tplot_names
; Keywords: displaytime, ps, idl_plot
; Written by Jing Liao
; Written on 04/15/2021
; -------------------------------------------------------------------

pro make_o_beam_tplots_multi_rbsp_old, t_s, t_dt, output_path, all_tplot_names, displaytime = displaytime, ps = ps, idl_plot = idl_plot, to_plot = to_plot
  
  common SHARE1, ENERGY_BINS, DENERGY_BINS, PA_BINS, ERROR_MESSAGE

  to_plot = ['identification', 'procedure1', 'procedure_para', 'procedure_anti']

  dispersion_list_filename = 'data/rbsp_Marissa_dispersion.csv'
  dispersion_list_data = read_csv(dispersion_list_filename) ; , header = dispersion_list_header)
  dispersion_list_start_time_str = dispersion_list_data.field01
  dispersion_list_start_time = TIME_DOUBLE(dispersion_list_start_time_str)
  ; dispersion_list_start_date_str = strmid(dispersion_list_start_time_str, 0, 10)
  dispersion_list_duration = dispersion_list_data.field02 * 60.

  options, '*', 'panel_size', 1
  options, '*', 'zticks', 3
  options, [all_tplot_names.diffflux_o1_parallel_name, all_tplot_names.diffflux_o1_antiparallel_name, all_tplot_names.parallel_pa_name, all_tplot_names.antiparallel_pa_name, all_tplot_names.parallel_pap_name, all_tplot_names.antiparallel_pap_name, all_tplot_names.parallel_pap_beam_name, all_tplot_names.antiparallel_pap_beam_name, all_tplot_names.pap_beam_combine_name], 'ztitle', ''

  ylim, all_tplot_names.p_total_name, 0.01, 3, 1
  ylim, all_tplot_names.beta_name, 0.01, 10, 1
  ylim, [all_tplot_names.diffflux_o1_parallel_name + '_Original', all_tplot_names.diffflux_o1_antiparallel_name + '_Original', all_tplot_names.diffflux_o1_parallel_name, all_tplot_names.diffflux_o1_antiparallel_name, all_tplot_names.diffflux_o1_parallel_subtracted_name, all_tplot_names.diffflux_o1_antiparallel_subtracted_name], 1., 6.e4, 1
  ; ylim, [all_tplot_names.parallel_dispersion_estimated_distance_name, all_tplot_names.antiparallel_dispersion_estimated_distance_name], 0, 50, 0

  zlim, [all_tplot_names.diffflux_o1_parallel_name + '_Original', all_tplot_names.diffflux_o1_antiparallel_name + '_Original', all_tplot_names.diffflux_o1_name, all_tplot_names.diffflux_o1_parallel_name, all_tplot_names.diffflux_o1_antiparallel_name, all_tplot_names.diffflux_o1_parallel_subtracted_name, all_tplot_names.diffflux_o1_antiparallel_subtracted_name], 0.1, 100, 1

  zlim, [all_tplot_names.eflux_o1_parallel_name, all_tplot_names.eflux_o1_antiparallel_name], 100, 10000, 1

  zlim, [all_tplot_names.parallel_pa_name, all_tplot_names.antiparallel_pa_name], 0.1, 100, 1
  zlim, [all_tplot_names.parallel_pa_h_name, all_tplot_names.antiparallel_pa_h_name], 10, 1e4
  zlim, [all_tplot_names.diffflux_o1_name, all_tplot_names.diffflux_o1_pa_name], 0.01, 100
  zlim, [all_tplot_names.parallel_pa_eflux_name, all_tplot_names.antiparallel_pa_eflux_name], 1e3, 1e6
  zlim, [all_tplot_names.parallel_pa_eflux_h_name, all_tplot_names.antiparallel_pa_eflux_h_name], 1e6, 1e9

  options, all_tplot_names.x_gsm_name, 'ytitle', 'XGSM'
  options, all_tplot_names.y_gsm_name, 'ytitle', 'YGSM'
  options, all_tplot_names.z_gsm_name, 'ytitle', 'ZGSM'
  options, all_tplot_names.ilat_name, 'ytitle', 'ILAT'
  options, all_tplot_names.l_name, 'ytitle', 'L'
  options, all_tplot_names.mlt_name, 'ytitle', 'MLT'
  options, all_tplot_names.dist_name, 'ytitle', 'DIST'

  options, all_tplot_names.beta_name, 'ytitle', '!C!Cbeta'

  options, all_tplot_names.diffflux_h1_name, 'ytitle', 'H!U+!N(eV)'
  options, all_tplot_names.diffflux_h1_pa_name, 'ytitle', 'H!U+!N(PA)'
  options, all_tplot_names.diffflux_o1_name, 'ytitle', 'O!U+!N(eV)'
  options, all_tplot_names.diffflux_o1_pa_name, 'ytitle', 'O!U+!N(PA)'

  options, all_tplot_names.diffflux_o1_parallel_name, 'ytitle', 'O!U+!N(eV)!Cpara'
  options, all_tplot_names.diffflux_o1_antiparallel_name, 'ytitle', 'O!U+!N(eV)!Canti'
  options, all_tplot_names.diffflux_o1_parallel_subtracted_name, 'ytitle', 'O!U+!N!(eV)!Cpara sub'
  options, all_tplot_names.diffflux_o1_antiparallel_subtracted_name, 'ytitle', 'O!U+!N(eV)!Canti sub'

  options, all_tplot_names.parallel_epcut_beam_name, 'color', 0
  options, all_tplot_names.parallel_epcut_beam_name, 'thick', 4
  options, all_tplot_names.parallel_epcut_beam_name, 'symsize', 0.5

  options, all_tplot_names.antiparallel_epcut_beam_name, 'color', 0
  options, all_tplot_names.antiparallel_epcut_beam_name, 'thick', 4
  options, all_tplot_names.antiparallel_epcut_beam_name, 'symsize', 0.5

  options, all_tplot_names.antiparallel_epcut_beam_name, 'symsize', 0.5

  options, all_tplot_names.pap_beam_combine_name, 'ytitle', ' O!U+!CPitch Angle!CBeam'

  options, all_tplot_names.parallel_pa_name, 'ytitle', 'PA!CEN peak'
  options, all_tplot_names.antiparallel_pa_name, 'ytitle', 'PA!CEN peak'

  options, [all_tplot_names.diffflux_o1_parallel_name + '_erange', all_tplot_names.diffflux_o1_antiparallel_name + '_erange'], 'color', 2

  options, [all_tplot_names.parallel_pap_name, all_tplot_names.antiparallel_pap_name], 'ytitle', 'PA!CPeak'
  options, [all_tplot_names.parallel_pap_beam_name, all_tplot_names.antiparallel_pap_beam_name], 'ytitle', 'PA!CBeam'
  options, [all_tplot_names.parallel_dispersion_name, all_tplot_names.antiparallel_dispersion_name], 'color', 1
  options, [all_tplot_names.parallel_dispersion_name, all_tplot_names.antiparallel_dispersion_name], 'thick', 5
  options, [all_tplot_names.parallel_dispersion_inverse_v_fitting_name, all_tplot_names.antiparallel_dispersion_inverse_v_fitting_name], 'color', 1

  options, all_tplot_names.parallel_dispersion_inverse_v_name, 'ytitle', 'para!C1/v'
  options, all_tplot_names.antiparallel_dispersion_inverse_v_name, 'ytitle', 'anti!C1/v'
  options, [all_tplot_names.parallel_dispersion_inverse_v_name, all_tplot_names.antiparallel_dispersion_inverse_v_name], 'psym', 3

  options, all_tplot_names.parallel_dispersion_estimated_distance_name, 'ytitle', 'para!Cdist'
  options, all_tplot_names.antiparallel_dispersion_estimated_distance_name, 'ytitle', 'anti!Cdist'
  options, [all_tplot_names.parallel_dispersion_estimated_distance_name, all_tplot_names.antiparallel_dispersion_estimated_distance_name], 'psym', '-7'

  var_label = [all_tplot_names.x_gsm_name, all_tplot_names.y_gsm_name, all_tplot_names.z_gsm_name, all_tplot_names.ilat_name, all_tplot_names.l_name, all_tplot_names.mlt_name, all_tplot_names.dist_name]

  if ~keyword_set(displaytime) then displaytime = t_dt
  if ~keyword_set(idl_plot) and ~keyword_set(ps) then RETURN

  ;----------------------------------
 ; identification result plot
;----------------------------------
  index = where(to_plot eq 'identification', ct)
  if ct gt 0 then begin
    for idisplay = 0, ceil(t_dt / displaytime) - 1 do begin
      timespan, t_s + idisplay * displaytime, displaytime, /seconds

      if keyword_set(ps) then begin
        ps_folder = output_path + 'plots/' + 'obeam_day/'
        spawn, 'mkdir -p ' + ps_folder
        ts_plot = time_string(t_s + idisplay * displaytime)
        te_plot = time_string(t_s + (idisplay + 1) * displaytime)

        date_s = extract_date_string(ts_plot)
        time_s = extract_time_string(ts_plot)
        date_e = extract_date_string(te_plot)
        time_e = extract_time_string(te_plot)

        fln = ps_folder + 'o_beam' + date_s + '_' + time_s + '_to_' + date_e + '_' + time_e + '_' + 'identification.ps'
        popen, fln, /port
      endif

      tplot_names, all_tplot_names.diffflux_o1_parallel_subtracted_name, names = names
      if keyword_set(names) then begin
        tplot, [all_tplot_names.beta_name, all_tplot_names.diffflux_h1_name $
          ; , all_tplot_names.diffflux_h1_pa_name $
          , all_tplot_names.diffflux_o1_name, all_tplot_names.diffflux_o1_pa_name $
          , all_tplot_names.diffflux_o1_parallel_subtracted_name, all_tplot_names.diffflux_o1_antiparallel_subtracted_name $
          ; , all_tplot_names.o1_density_name $
          ], var_label = var_label

        ; tplot_panel, v = all_tplot_names.h1_density_name, o = all_tplot_names.o1_density_name
        tplot_panel, v = all_tplot_names.diffflux_o1_parallel_subtracted_name, o = all_tplot_names.parallel_epcut_beam_name, psym = -1
        tplot_panel, v = all_tplot_names.diffflux_o1_antiparallel_subtracted_name, o = all_tplot_names.antiparallel_epcut_beam_name, psym = -1
      endif else begin
        tplot, [all_tplot_names.beta_name, all_tplot_names.diffflux_h1_name $
          ; , all_tplot_names.diffflux_h1_pa_name $
          , all_tplot_names.diffflux_o1_name, all_tplot_names.diffflux_o1_pa_name $
          , all_tplot_names.diffflux_o1_parallel_name, all_tplot_names.diffflux_o1_antiparallel_name $
          ; , all_tplot_names.o1_density_name $
          ], var_label = var_label

        ; tplot_panel, v = all_tplot_names.h1_density_name, o = all_tplot_names.o1_density_name
        tplot_panel, v = all_tplot_names.diffflux_o1_parallel_name, o = all_tplot_names.parallel_epcut_beam_name, psym = 1
        tplot_panel, v = all_tplot_names.diffflux_o1_antiparallel_name, o = all_tplot_names.antiparallel_epcut_beam_name, psym = 1
      endelse

      yline, all_tplot_names.beta_name, offset = 0.05, col = 2, thick = 2
      yline, all_tplot_names.beta_name, offset = 1, col = 2, thick = 2

      ; index = WHERE(dispersion_list_start_time GE t_s+idisplay*displaytime AND  dispersion_list_start_time LE t_s+(idisplay+1) * displaytime, ct)
      ; FOR ii = 0, ct-1 DO BEGIN
      ; iindex = index[ii]
      ; timebar, dispersion_list_start_time[index[ii]], color = 2
      ; timebar, dispersion_list_start_time[index[ii]] + dispersion_list_duration[index[ii]], color = 3
      ; ENDFOR

      if keyword_set(ps) then begin
        pclose
        spawn, 'mogrify -format png -alpha opaque -density 150 ' + fln
        spawn, 'rm -f ' + fln
      endif else stop
    endfor
  endif

  ; identification procedure 1
  index = where(to_plot eq 'procedure1', ct)
  if ct gt 0 then begin
    for idisplay = 0, ceil(t_dt / displaytime) - 1 do begin
      timespan, t_s + idisplay * displaytime, displaytime, /seconds

      if keyword_set(ps) then begin
        ps_folder = output_path + 'plots/' + 'obeam_day/procedure/'
        spawn, 'mkdir -p ' + ps_folder

        ts_plot = time_string(t_s + idisplay * displaytime)
        te_plot = time_string(t_s + (idisplay + 1) * displaytime)

        date_s = extract_date_string(ts_plot)
        time_s = extract_time_string(ts_plot)
        date_e = extract_date_string(te_plot)
        time_e = extract_time_string(te_plot)

        fln = ps_folder + 'o_beam' + date_s + '_' + time_s + '_to_' + date_e + '_' + time_e + '_' + 'procedure1.ps'
        popen, fln, /port
      endif

      tplot_names, all_tplot_names.diffflux_o1_parallel_subtracted_name, names = names
      if keyword_set(names) then begin
        tplot, [all_tplot_names.diffflux_o1_parallel_name + '_Original', all_tplot_names.eflux_o1_parallel_name, all_tplot_names.diffflux_o1_parallel_name, all_tplot_names.diffflux_o1_parallel_subtracted_name $
          , all_tplot_names.diffflux_o1_antiparallel_name + '_Original', all_tplot_names.eflux_o1_antiparallel_name, all_tplot_names.diffflux_o1_antiparallel_name, all_tplot_names.diffflux_o1_antiparallel_subtracted_name $
          ], var_label = var_label
        tplot_panel, v = all_tplot_names.diffflux_o1_parallel_subtracted_name, o = all_tplot_names.parallel_epcut_name, psym = -1
        tplot_panel, v = all_tplot_names.diffflux_o1_antiparallel_subtracted_name, o = all_tplot_names.antiparallel_epcut_name, psym = -1
      endif else begin
        tplot, [all_tplot_names.diffflux_o1_parallel_name + '_Original', all_tplot_names.eflux_o1_parallel_name, all_tplot_names.diffflux_o1_parallel_name $
          , all_tplot_names.diffflux_o1_antiparallel_name + '_Original', all_tplot_names.eflux_o1_antiparallel_name, all_tplot_names.diffflux_o1_antiparallel_name $
          ], var_label = var_label
        tplot_panel, v = all_tplot_names.diffflux_o1_parallel_name, o = all_tplot_names.parallel_epcut_name, psym = 1
        tplot_panel, v = all_tplot_names.diffflux_o1_antiparallel_name, o = all_tplot_names.antiparallel_epcut_name, psym = 1
      endelse

      ; index = WHERE(dispersion_list_start_time GE t_s+idisplay*displaytime AND  dispersion_list_start_time LE t_s+(idisplay+1) * displaytime, ct)
      ; FOR ii = 0, ct-1 DO BEGIN
      ; iindex = index[ii]
      ; timebar, dispersion_list_start_time[index[ii]], color = 2
      ; timebar, dispersion_list_start_time[index[ii]] + dispersion_list_duration[index[ii]], color = 3
      ; ENDFOR

      if keyword_set(ps) then begin
        pclose
        spawn, 'mogrify -format png -alpha opaque -density 150 ' + fln
        spawn, 'rm -f ' + fln
      endif else stop
    endfor
  endif

  index = where(to_plot eq 'procedure_para', ct)
  if ct gt 0 then begin
    ; --- seperate pitch angle data and pitch angle peak beam into different energy pitch angle, so they can be plotted ---
    nenergybins = n_elements(ENERGY_BINS)

; --- FOR all pa plots (before peak and beam filter)
    get_data, all_tplot_names.parallel_pa_name, data = pa_data, dlim = dlim, lim = lim
    
; --- FOR all pap plots (before peak and beam filter)
; get_data, all_tplot_names.parallel_pap_name,data=data,dlim=dlim

; --- FOR all pap beam plots (before peak and beam filter)
    get_data, all_tplot_names.parallel_pap_beam_name, data = pa_beam_data, dlim = dlim

; --- plot tplots for different time frames ---
    for idisplay = 0, ceil(t_dt / displaytime) - 1 do begin
      its = t_s + idisplay * displaytime
      ite = its + displaytime
      idt = displaytime
      timespan, its, idt, /seconds

      if keyword_set(ps) then begin
        ps_folder = output_path + 'plots/' + 'obeam_day/procedure/'
        spawn, 'mkdir -p ' + ps_folder

        ts_plot = time_string(its)
        te_plot = time_string(ite)
        date_s = extract_date_string(ts_plot)
        time_s = extract_time_string(ts_plot)
        date_e = extract_date_string(te_plot)
        time_e = extract_time_string(te_plot)

        fln = ps_folder + 'o_beam' + date_s + '_' + time_s + '_to_' + date_e + '_' + time_e + '_multi_para.ps'
        popen, fln, /port
      endif
      ; ---creat and prepare pitch angel plots
      for k = 0, nenergybins - 1 do begin
        ; --- FOR pap beam plots
        data_y = reform((pa_beam_data.y[*, *, k] gt 0) * pa_beam_data.v[*, *, k])
        index = where(data_y eq 0, ct)
        if ct gt 0 then data_y[index] = !values.f_nan

        index = where(pa_beam_data.x lt its or pa_beam_data.x gt ite, ct)
        data_y[index, *] = !values.f_nan

        if total(data_y, /nan) gt 0 then begin
          store_data, all_tplot_names.parallel_pap_beam_name + '_' + string(k, format = '(i2.2)'), data = {x: pa_beam_data.x, y: data_y}
          ; --- FOR pap plots (before beam filter)
          store_data, all_tplot_names.parallel_pa_name + '_' + string(k, format = '(i2.2)'), data = {x: pa_data.x, y: reform(pa_data.y[*, *, k]), v: reform(pa_data.v[*, *, k])}, dlim = dlim, lim = lim
          options, all_tplot_names.parallel_pa_name + '_' + string(k, format = '(i2.2)'), 'ytitle', 'para!C!C' + string(ENERGY_BINS[k], format = '(i5.1)')
        endif
      endfor
      options, all_tplot_names.parallel_pap_beam_name + '_??', 'color', 0
      
      ; --- plot data ----
      tplot_names, all_tplot_names.diffflux_o1_parallel_subtracted_name, names = names
      if keyword_set(names) then begin
        tplot_names, all_tplot_names.parallel_pa_name + '_??', names = names
        nnames = n_elements(names)
        panel_per_page = 10.
        npages = ceil(nnames / panel_per_page)
        for ipage = 0, npages - 1 do begin
          tplot, [all_tplot_names.diffflux_o1_parallel_subtracted_name, names[((ipage * 9) < (nnames - 1)) : (panel_per_page - 1 + ipage * panel_per_page < (nnames - 1))]], var_label = var_label
          tplot_panel, v = all_tplot_names.diffflux_o1_parallel_subtracted_name, o = all_tplot_names.parallel_epcut_beam_name, psym = 1
          for k = 0, nenergybins - 1 do tplot_panel, o = all_tplot_names.parallel_pap_beam_name + '_' + string(k, format = '(i2.2)'), v = all_tplot_names.parallel_pa_name + '_' + string(k, format = '(i2.2)'), psym = 7
        end
      endif else begin
        tplot_names, all_tplot_names.parallel_pa_name + '_??', names = names
        tplot, [all_tplot_names.diffflux_o1_parallel_name, names], var_label = var_label
        tplot_panel, v = all_tplot_names.diffflux_o1_parallel_name, o = all_tplot_names.parallel_epcut_beam_name, psym = 1
        for k = 0, nenergybins - 1 do tplot_panel, o = all_tplot_names.parallel_pap_beam_name + '_' + string(k, format = '(i2.2)'), v = all_tplot_names.parallel_pa_name + '_' + string(k, format = '(i2.2)'), psym = 7
      endelse

      index = where(dispersion_list_start_time ge t_s + idisplay * displaytime and dispersion_list_start_time le t_s + (idisplay + 1) * displaytime, ct)
      for ii = 0, ct - 1 do begin
        iindex = index[ii]
        timebar, dispersion_list_start_time[index[ii]], color = 2
        timebar, dispersion_list_start_time[index[ii]] + dispersion_list_duration[index[ii]], color = 3
      endfor

      if keyword_set(ps) then begin
        pclose
        spawn, 'mogrify -format png -alpha opaque -density 150 ' + fln
        spawn, 'rm -f ' + fln
      endif else stop

      tplot_names, all_tplot_names.parallel_pa_name + '_??', names = names
      store_data, delete = names
      tplot_names, all_tplot_names.parallel_pap_name + '_??', names = names
      store_data, delete = names
      tplot_names, all_tplot_names.parallel_pap_beam_name + '_??', names = names
      store_data, delete = names
    endfor
  endif

  index = where(to_plot eq 'procedure_anti', ct)
  if ct gt 0 then begin
    ; energy = ENERGY_BINS
    ; --- seperate pitch angle data and pitch angle peak beam into different energy pitch angle, so they can be plotted ---
    nenergybins = n_elements(ENERGY_BINS)

    ; --- FOR all pa plots (before peak and beam filter)
    get_data, all_tplot_names.antiparallel_pa_name, data = pa_data, dlim = dlim, lim = lim
    ; get_data, all_tplot_names.antiparallel_pap_name,data=pap_data,dlim=dlim
    get_data, all_tplot_names.antiparallel_pap_beam_name, data = pa_beam_data, dlim = dlim
    ; stop

    ; --- plot tplots for different time frames ---
    for idisplay = 0, ceil(t_dt / displaytime) - 1 do begin
      its = t_s + idisplay * displaytime
      ite = its + displaytime
      idt = displaytime
      timespan, its, idt, /seconds

      if keyword_set(ps) then begin
        ps_folder = output_path + 'plots/' + 'obeam_day/procedure/'
        spawn, 'mkdir -p ' + ps_folder

        ts_plot = time_string(its)
        te_plot = time_string(ite)
        date_s = extract_date_string(ts_plot)
        time_s = extract_time_string(ts_plot)
        date_e = extract_date_string(te_plot)
        time_e = extract_time_string(te_plot)

        fln = ps_folder + 'o_beam' + date_s + '_' + time_s + '_to_' + date_e + '_' + time_e + '_multi_anti.ps'
        popen, fln, /port
      endif

      ; ---creat and prepare pitch angel plots
      for k = 0, nenergybins - 1 do begin
        ; --- FOR pap beam plots
        data_y = reform((pa_beam_data.y[*, *, k] gt 0) * pa_beam_data.v[*, *, k])
        index = where(data_y eq 0, ct)
        if ct gt 0 then data_y(index) = !values.f_nan

        index = where(pa_beam_data.x lt its or pa_beam_data.x gt ite, ct)
        data_y[index, *] = !values.f_nan

        if total(data_y, /nan) gt 0 then begin
          store_data, all_tplot_names.antiparallel_pap_beam_name + '_' + string(k, format = '(i2.2)'), data = {x: pa_beam_data.x, y: data_y}
          ; --- FOR pap plots (before beam filter)
          store_data, all_tplot_names.antiparallel_pa_name + '_' + string(k, format = '(i2.2)'), data = {x: pa_data.x, y: reform(pa_data.y[*, *, k]), v: reform(pa_data.v[*, *, k])}, dlim = dlim, lim = lim
          options, all_tplot_names.antiparallel_pa_name + '_' + string(k, format = '(i2.2)'), 'ytitle', 'anti!C!C' + string(ENERGY_BINS[k], format = '(i5.1)')
        endif
      endfor
      options, all_tplot_names.antiparallel_pap_beam_name + '_??', 'color', 0
      ; --- plot data ----
      tplot_names, all_tplot_names.diffflux_o1_antiparallel_subtracted_name, names = names
      if keyword_set(names) then begin
        tplot_names, all_tplot_names.antiparallel_pa_name + '_??', names = names
        nnames = n_elements(names)
        panel_per_page = 10.
        npages = ceil(nnames / panel_per_page)
        for ipage = 0, npages - 1 do begin
          tplot, [all_tplot_names.diffflux_o1_antiparallel_subtracted_name, names[((ipage * 9) < (nnames - 1)) : (panel_per_page - 1 + ipage * panel_per_page < (nnames - 1))]], var_label = var_label
          tplot_panel, v = all_tplot_names.diffflux_o1_antiparallel_subtracted_name, o = all_tplot_names.antiparallel_epcut_beam_name, psym = 1
          for k = 0, nenergybins - 1 do tplot_panel, o = all_tplot_names.antiparallel_pap_beam_name + '_' + string(k, format = '(i2.2)'), v = all_tplot_names.antiparallel_pa_name + '_' + string(k, format = '(i2.2)'), psym = 7
        end
      endif else begin
        tplot_names, all_tplot_names.antiparallel_pa_name + '_??', names = names
        tplot, [all_tplot_names.diffflux_o1_antiparallel_name, names], var_label = var_label
        tplot_panel, v = all_tplot_names.diffflux_o1_antiparallel_name, o = all_tplot_names.antiparallel_epcut_beam_name, psym = 1
        for k = 0, nenergybins - 1 do tplot_panel, o = all_tplot_names.antiparallel_pap_beam_name + '_' + string(k, format = '(i2.2)'), v = all_tplot_names.antiparallel_pa_name + '_' + string(k, format = '(i2.2)'), psym = 7
      endelse

      index = where(dispersion_list_start_time ge t_s + idisplay * displaytime and dispersion_list_start_time le t_s + (idisplay + 1) * displaytime, ct)
      for ii = 0, ct - 1 do begin
        iindex = index[ii]
        timebar, dispersion_list_start_time[index[ii]], color = 2
        timebar, dispersion_list_start_time[index[ii]] + dispersion_list_duration[index[ii]], color = 3
      endfor

      if keyword_set(ps) then begin
        pclose
        spawn, 'mogrify -format png -alpha opaque -density 150 ' + fln
        spawn, 'rm -f ' + fln
      endif else stop
    endfor

    tplot_names, all_tplot_names.antiparallel_pa_name + '_??', names = names
    store_data, delete = names
    tplot_names, all_tplot_names.antiparallel_pap_name + '_??', names = names
    store_data, delete = names
    tplot_names, all_tplot_names.antiparallel_pap_beam_name + '_??', names = names
    store_data, delete = names
  endif
  timespan, t_s, t_dt, /seconds
end