;+
; NAME: get_rbsp_hope_l3_paspec
;
; PURPOSE: To read the HOPE-L3_PA cdf files and create pa spectra for a particular energy range
;
; INPUT: probe_str --> 'A' or 'B' for probes a or b
;
; KEYWORDS:
;        species --> 0,1,2 or 3 for species H+, e-, He+, O+
;        units --> 'DIFF FLUX' or 'EFLUX'
;        energy_units_in_flux --> 'eV' or 'keV' for 'DIFF FLUX' units only.
;        energy_range --> the energy range used for the pitch angle spectra calculation in eV [er_s, er_e]
;        energy_bins_range --> the range of the energy bins used for the pitch angle spectra calculation [eb_s, eb_e]
;        path --> Data file path if not the one defined by the RBSP_A_HOPE_PA or
;                 RBSP_B_HOPE_PA environment variables
;        fln --> Filename if not the default
;        pa_weighting --> Apply pitch angle weighting (for testing purposes)
;
; CREATED by: C.G. Mouikis (11/2013)
;
; MODifICATION HISTORY:
;  6/23/2023 - (cgm): Updated to properly handle EFLUX units
;
; FILE SOURCE: http://www.rbsp-ect.lanl.gov/data_pub/rbspa/hope/level3/pitchangle/ or
;              http://www.rbsp-ect.lanl.gov/data_pub/rbspb/hope/level3/pitchangle/
;
; ATTENTION:
;            - The file name specification is: 'rbsp' + probe_str + '_ect-hope-PA*-L3_' + date + '*.cdf'
;            which makes does not restrict which file version (3D or 4D) is selected. So, if both file types
;            for a particular date exist in the sape directory, it becomes random which one is chosen. This
;            can be done by specifying the file name using the "fln" keyword
;
;            - Also, if two data files exist in the path directory with different version numbers the one with
;            the higher version number is selected
;
; CATEGORY:
;           RBSP, HOPE, Core
;
; @author Chris Mouikis
;
;-
pro get_rbsp_hope_l3_paspec, $
  probe_str, $
  species, $
  units, $
  energy_range, $
  energy_units_in_flux = energy_units_in_flux, $
  energy_bins_range = energy_bins_range, $
  aux_data = aux_data, $
  eph_data = eph_data, $
  name = name, $
  path = path, $
  fln = fln
  

  common get_error, get_err_no, get_err_msg, default_verbose

  ; --------------------------------------------------------------------
  ; Find data files to read
  ; ---------------------------------------------------------------------
  ; Jing: set energy bin constant:
  RBSP_ENERGY_BIN = [51767.680, 44428.695, 38130.121, 32724.498, 28085.268, 24103.668, 20686.559, 17753.877, 15236.896, 13076.798, 11222.936, 9631.8994, 8266.4062, 7094.5161, 6088.7222, 5225.5273, 4484.7422, 3848.9187, 3303.2842, 2834.9644, 2433.0547, 2088.1287, 1792.0959, 1538.0620, 1319.9771, 1132.8461, 972.23694, 834.42133, 716.16302, 614.57758, 527.48431, 452.70224, 388.54303, 333.45901, 286.18381, 245.59184, 210.76859, 180.86984, 155.26245, 133.24290, 114.31875, 98.138245, 84.208954, 72.319794, 62.048695, 53.254951, 45.727501, 39.184952, 33.627300, 28.913851, 24.763199, 21.245699, 18.290998, 15.688050, 13.436850, 11.537399, 9.9193497, 8.5123501, 7.3163996, 6.2611499, 5.3465996, 4.6431003, 3.9396000, 3.3767998, 2.9546998, 2.5326002, 2.1808500, 1.8290999, 1.5476999, 1.3366499, 1.1959500, 0.98490000]

  RBSP_DENERGY_BIN = [3882.5762, 3332.1523, 2859.7593, 2454.3374, 2106.3953, 1807.7751, 1551.4919, 1331.5408, 1142.7672, 980.75989, 841.72021, 722.39246, 619.98047, 532.08875, 456.65417, 391.91458, 336.35568, 288.66891, 247.74632, 212.62233, 182.47911, 156.60965, 134.40720, 115.35465, 98.998283, 84.963455, 72.917770, 62.581600, 53.712231, 46.093319, 39.561325, 33.952671, 29.140728, 25.009428, 21.463787, 18.419390, 15.807645, 13.565239, 11.644684, 9.9932184, 8.5739069, 7.3603687, 6.3156719, 5.4239845, 4.6536522, 3.9941216, 3.4295628, 2.9388714, 2.5220475, 2.1685388, 1.8572400, 1.5934275, 1.3718250, 1.1766038, 1.0077637, 0.86530501, 0.74395126, 0.63842630, 0.54873002, 0.46958625, 0.40099499, 0.34823254, 0.29547000, 0.25325999, 0.22160248, 0.18994503, 0.16356376, 0.13718250, 0.11607750, 0.10024875, 0.089696258, 0.073867500]
  nenergybins = n_elements(RBSP_ENERGY_BIN)

  ; Get data path
  if ~keyword_set(path) then path2 = '' else path2 = path
  if path2 eq '' then path2 = getenv('RBSP_' + probe_str + '_HOPE_PA')

  ; Find data file with manually entered filename
  if not keyword_set(fln) then begin
    fln2 = ''
  endif else begin
    fln2 = fln
    files_found = file_search(path2 + '/' + fln2, count = ifln)
    if ifln le 0 then begin
      get_err_no = 1
      get_err_msg = 'File ' + fln + ' not found'
      message, get_err_msg, /continue
      return
    endif
  endelse

  ; Find data files.
  ; Filenames are reconstructed using the timespan set
  if fln2 eq '' then begin
    ; Find days that correspond to time interval selected
    get_timespan, time_interval

    t_s = gettime(time_interval[0]) ; start time in tplot-time
    t_e = gettime(time_interval[1]) ; end time in tplot-time

    t_s_str = time_struct(t_s) ; start_time tplot time structure
    t_e_str = time_struct(t_e) ; end_time tplot time structure

    mjd_s = julday(t_s_str.month, t_s_str.date, t_s_str.year) ; start julian day
    mjd_e = julday(t_e_str.month, t_e_str.date, t_e_str.year) ; end julian day

    ndys = (mjd_e - mjd_s) + 1 ; number of days to be loaded

    ; Last day is not included if hour=min=sec=0
    if t_e_str.hour eq 0 and t_e_str.min eq 0 and t_e_str.sec eq 0 then $
      ndys = ndys - 1

    ; Reconstruct date strings and search for the corresponding files
    files_found = ''
    for indys = 0, ndys - 1 do begin
      date = time_double(strmid(time_string(time_interval[0]), 0, 4) + $
        strmid(time_string(time_interval[0]), 5, 2) + $
        strmid(time_string(time_interval[0]), 8, 2)) + $
        indys * 86400.

      date_str = strmid(time_string(date), 0, 4) + $
        strmid(time_string(date), 5, 2) + $
        strmid(time_string(date), 8, 2)

      fln2 = 'rbsp' + strlowcase(probe_str) + '*_ect-hope-*_' + date_str + '*.cdf'

      path_fln = file_search(path2 + '/' + fln2, count = ifln)
      ; if more than one files for the same date are found the last one
      ; in the list is selected. It is assumed that this will be the most
      ; recent one if the version numbers are properly incremented.
      if ifln gt 0 then begin
        files_found = [files_found, path_fln(ifln - 1)]
      endif
    endfor

    if n_elements(files_found) - 1 eq 0 then begin
      get_err_no = 1
      get_err_msg = 'Data files not found for time interval'
      message, get_err_msg, /continue
      return
    endif else begin
      files_found = files_found[1 : n_elements(files_found) - 1]
    endelse
  endif
  ; <<--------------------------------------------------------------------

  ; >>--------------------------------------------------------------------
  ; Open and read CDF files
  ; ----------------------------------------------------------------------
  ; Determine the variables to be read from the CDF files
  case species of
    0: begin
      var2get = ['FPDU', 'Epoch_Ion', 'PITCH_ANGLE', 'HOPE_ENERGY_Ion', 'ENERGY_Ion_DELTA']
      if keyword_set(eph_data) then begin
        var2get = [var2get, ['POSITION_ION', 'L_ION', 'L_STAR_ION', 'MLT_ION']]
      endif
      if keyword_set(aux_data) then begin
        var2get = [var2get, ['B_CALC_ION', 'B_EQ_ION', 'I_ION']]
      endif
    end
    1: begin
      var2get = ['FEDU', 'Epoch_Ele', 'PITCH_ANGLE', 'HOPE_ENERGY_Ele', 'ENERGY_Ele_DELTA']
      if keyword_set(eph_data) then begin
        var2get = [var2get, 'Position_Ele', 'L_Ele', 'L_star_Ele', 'MLT_Ele']
      endif
      if keyword_set(aux_data) then begin
        var2get = [var2get, ['B_CALC_ELE', 'B_EQ_ELE', 'I_ELE']]
      endif
    end
    2: begin
      var2get = ['FHEDU', 'Epoch_Ion', 'PITCH_ANGLE', 'HOPE_ENERGY_Ion', 'ENERGY_Ion_DELTA']
      if keyword_set(eph_data) then begin
        var2get = [var2get, 'Position_Ion', 'L_Ion', 'L_star_Ion', 'MLT_Ion']
      endif
      if keyword_set(aux_data) then begin
        var2get = [var2get, ['B_CALC_ION', 'B_EQ_ION', 'I_ION']]
      endif
    end
    3: begin
      var2get = ['FODU', 'Epoch_Ion', 'PITCH_ANGLE', 'HOPE_ENERGY_Ion', 'ENERGY_Ion_DELTA']
      if keyword_set(eph_data) then begin
        var2get = [var2get, 'Position_Ion', 'L_Ion', 'L_star_Ion', 'MLT_Ion']
      endif
      if keyword_set(aux_data) then begin
        var2get = [var2get, ['B_CALC_ION', 'B_EQ_ION', 'I_ION']]
      endif
    end
  endcase

  ; Loop through the CDF files found and read the data
  append_flag = 0
  for iday = 0, n_elements(files_found) - 1 do begin ; Loop over daily files

    ds = get_cdf_data(file = files_found(iday), var2get = var2get)

    ; -------------------------------------------------------------------
    ; Get data
    ; Default flux units: 's!E-1!Ncm!E-2!Nster!E-1!NkeV!E-1!N'
    ; Default energy units: 'eV'
    ; -------------------------------------------------------------------
    ; Read Epoch Time
    if species eq 1 then begin ; epoch time
      t_ele = reform(ds.epoch_ele.data)
      t_str = time_double(t_ele, /epoch)
    endif else begin
      t_ion = reform(ds.epoch_ion.data)
      t_str = time_double(t_ion, /epoch)
    endelse ; epoch time

    ; Limit data arrays to time interval requested
    get_timespan, tt
    itime = where(t_str ge tt[0] and t_str le tt[1], c_itime)
    if c_itime le 1 then begin
      get_err_no = 1
      get_err_msg = 'Less than 2 data points found for time interval'
      message, get_err_msg, /continue
      return
    endif

    time = t_str[itime]

    ; Set energy and pa arrays
    pa_data = ds.pitch_angle.data
    if species eq 1 then begin
      en_data = reverse(ds.hope_energy_ele.data[*, itime]) ; order energy bins from high to low
      en_delta_data = reverse(ds.energy_ele_delta.data[*, itime]) ; order energy bins from high to low
    endif else begin
      en_data = reverse(ds.hope_energy_ion.data[*, itime]) ; order energy bins from high to low
      en_delta_data = reverse(ds.energy_ion_delta.data[*, itime]) ; order energy bins from high to low
    endelse

    ; --------------- Jing: remove all energy bins with min 24 eV.  -----------

    index_invalid = where(en_data[nenergybins - 1, *] gt 20, ct_invalid)
    if ct_invalid gt 0 then begin
      en_data[*, index_invalid] = cmreplicate(RBSP_ENERGY_BIN, ct_invalid)
      en_delta_data[*, index_invalid] = cmreplicate(RBSP_DENERGY_BIN, ct_invalid)
    endif
    ; --------------- Jing: remove all energy bins with min 24 eV.  -----------

    en_data_middle = reform(en_data[*, 0])

    en_data_l = reform(en_data[*, 0]) - (reform(en_delta_data[*, 0]) / 2.)
    en_data_u = reform(en_data[*, 0]) + (reform(en_delta_data[*, 0]) / 2.)

    if keyword_set(energy_bins_range) then begin
      if energy_bins_range[0] le energy_bins_range[1] then begin
        en_ind_array = indgen(energy_bins_range[1] - energy_bins_range[0] + 1) + energy_bins_range[0]
      endif else begin
        en_ind_array = indgen(energy_bins_range[0] - energy_bins_range[1] + 1) + energy_bins_range[1]
      endelse
      energy_range[0] = en_data_middle[en_ind_array[n_elements(en_ind_array) - 1]]
      energy_range[1] = en_data_middle[en_ind_array[0]]
    endif else begin
      en_start = min(abs(en_data_l - energy_range[0]), en_index_l)
      en_end = min(abs(en_data_u - energy_range[1]), en_index_u)
      en_ind_array = indgen((en_index_l - en_index_u + 1)) + en_index_u
      energy_range[0] = en_data_middle[en_ind_array[n_elements(en_ind_array) - 1]]
      energy_range[1] = en_data_middle[en_ind_array[0]]
    endelse

    case species of
      0: begin
        data_or = reverse(ds.fpdu.data[*, *, itime], 1)
      end
      1: begin
        data_or = reverse(ds.fedu.data[*, *, itime], 1)
      end
      2: begin
        data_or = reverse(ds.fhedu.data[*, *, itime], 1)
      end
      3: begin
        data_or = reverse(ds.fodu.data[*, *, itime], 1)
      end
    endcase

    fill_index = where(data_or eq -1.00000e+31, c_ifill)
    if c_ifill gt 0 then begin
      data_or[fill_index] = !values.f_nan
    endif
    ; --------------- Jing: remove all data_or with min 24 eV.  -----------
    index_invalid = where(en_data[nenergybins - 1, *] gt 20, ct_invalid)

    if ct_invalid gt 0 then begin
      data_or[*, *, index_invalid] = !values.f_nan
    endif
    ; --------------- Jing: remove all data_or with min 24 eV.  -----------

    ; Convert to EFLUX units
    data_y = data_or
    npa = n_elements(data_or[0, *, 0])
    if units eq 'EFLUX' then begin
      ; Since the Default flux units: 's!E-1!Ncm!E-2!Nster!E-1!NkeV!E-1!N'
      ; multiply by the energy array wich is in eV and therefore devide by 1000
      for ipa = 0, npa - 1 do data_y[*, ipa, *] = data_y[*, ipa, *] * (en_data / 1000.)
    endif

    ; Convert to eV in flux units
    if units eq 'DIFF FLUX' then begin
      if energy_units_in_flux eq 'eV' then data_y = data_y / 1000.
    endif

    ; average over selected energies
    data_y = transpose(reform(mean(data_y[en_ind_array, *, *], dim = 1, /nan)))
    e_data = transpose(en_data) ; to follow the tplot variable convention

    ; Get ephemeris parameters
    if keyword_set(eph_data) then begin
      if species eq 1 then begin
        dist_data = sqrt(reform(ds.position_ele.data[0, itime] ^ 2) + $
          reform(ds.position_ele.data[1, itime] ^ 2) + $
          reform(ds.position_ele.data[2, itime] ^ 2)) / 6371.0
        l_data = reform(ds.l_ele.data[itime])
        l_star_data = reform(ds.l_star_ele.data[itime])
        mlt_data = reform(ds.mlt_ele.data[itime])
      endif else begin
        dist_data = sqrt(reform(ds.position_ion.data[0, itime] ^ 2) + $
          reform(ds.position_ion.data[1, itime] ^ 2) + $
          reform(ds.position_ion.data[2, itime] ^ 2)) / 6371.0
        l_data = reform(ds.l_ion.data[itime])
        l_star_data = reform(ds.l_star_ion.data[itime])
        mlt_data = reform(ds.mlt_ion.data[itime])
      endelse
    endif

    ; Get auxiliary parameters
    if keyword_set(aux_data) then begin
      if species eq 1 then begin
        b_calc_data = reform(ds.b_calc_ele.data[itime])
        b_eq_data = reform(ds.b_eq_ele.data[itime])
        i_data = reform(ds.i_ele.data[itime])
      endif else begin
        b_calc_data = reform(ds.b_calc_ion.data[itime])
        b_eq_data = reform(ds.b_eq_ion.data[itime])
        i_data = reform(ds.i_ion.data[itime])
      endelse
    endif

    if append_flag eq 0 then begin ; append_flag
      data_x = time
      data_yy = data_y
      data_v = e_data
      if keyword_set(eph_data) then begin
        data_dist = dist_data
        data_L = l_data
        data_l_star = l_star_data
        data_mlt = mlt_data
      endif
      if keyword_set(aux_data) then begin
        data_b_calc = b_calc_data
        data_b_eq = b_eq_data
        data_i = i_data
      endif

      append_flag = 1
    endif else begin
      data_x = [data_x, time]
      data_yy = [data_yy, data_y]
      data_v = [data_v, e_data]
      if keyword_set(eph_data) then begin
        data_dist = [data_dist, dist_data]
        data_L = [data_L, l_data]
        data_l_star = [data_l_star, l_star_data]
        data_mlt = [data_mlt, mlt_data]
      endif
      if keyword_set(aux_data) then begin
        data_b_calc = [data_b_calc, b_calc_data]
        data_b_eq = [data_b_eq, b_eq_data]
        data_i = [data_i, i_data]
      endif
    endelse ; append_flag
  endfor ; Loop over daily files

  ; >>--------------------------------------------------------------------
  ; Create tplot variables
  ; ----------------------------------------------------------------------
  species_str = ['H!U+!N', 'e!U-!N', 'He!U+!N', 'O!U+!N']

  if ~keyword_set(name) then begin
    name = 'RBSP' + probe_str + $
      '_HOPE_l3_paspec_sp' + string(species, format = '(i1.1)') + $
      '_' + strupcase(strcompress(units, /remove_all)) + $
      '_' + string(energy_range[0], format = '(i5.5)') + '_' + string(energy_range[1], format = '(i5.5)')
  endif

  store_data, name, $
    data = {x: data_x, y: data_yy, v: pa_data, $
      flux_units: units, en_units: 'eV', flux_en_units: energy_units_in_flux, $
      species: species}, $
    dlim = {panel_size: 2, ylog: 0, zlog: 1, spec: 1, no_interp: 1}

  if keyword_set(eph_data) then begin
    store_data, name + '_DIST', data = {x: data_x, y: data_dist}, $
      dlim = {panel_size: 1, ytitle: 'Dist'}
    store_data, name + '_L', data = {x: data_x, y: data_L}, $
      dlim = {panel_size: 1, ytitle: 'L'}
    store_data, name + '_L_STAR', data = {x: data_x, y: data_l_star}, $
      dlim = {panel_size: 1, ytitle: 'L*'}
    store_data, name + '_MLT', data = {x: data_x, y: data_mlt}, $
      dlim = {panel_size: 1, ytitle: 'MLT'}

    ylim, name + '_DIST', 0, 8
    ylim, name + '_L', 0, 8
    ylim, name + '_L_STAR', 0, 8
    ylim, name + '_MLT', 0, 24
  endif

  if keyword_set(aux_data) then begin
    store_data, name + '_B_CALC', data = {x: data_x, y: data_b_calc}, $
      dlim = {panel_size: 1, ytitle: 'B model'}
    store_data, name + '_B_EQ', data = {x: data_x, y: data_b_eq}, $
      dlim = {panel_size: 1, ytitle: 'B eq model'}
    store_data, name + '_I', data = {x: data_x, y: data_i}, $
      dlim = {panel_size: 1, ytitle: 'I (bounce)'}

    ylim, name + '_B_CALC', 0, 400
    ylim, name + '_B_EQ', 0, 400
    ylim, name + '_I', 0, 5
  endif

  if keyword_set(eph_data) then begin
    store_data, name + '_DIST', data = {x: data_x, y: data_dist}, $
      dlim = {panel_size: 1, ytitle: 'Dist'}
    store_data, name + '_L', data = {x: data_x, y: data_L}, $
      dlim = {panel_size: 1, ytitle: 'L'}
    store_data, name + '_L_STAR', data = {x: data_x, y: data_l_star}, $
      dlim = {panel_size: 1, ytitle: 'L*'}
    store_data, name + '_MLT', data = {x: data_x, y: data_mlt}, $
      dlim = {panel_size: 1, ytitle: 'MLT'}

    ylim, name + '_DIST', 0, 8
    ylim, name + '_L', 0, 8
    ylim, name + '_L_STAR', 0, 8
    ylim, name + '_MLT', 0, 24
  endif

  if keyword_set(aux_data) then begin
    store_data, name + '_B_CALC', data = {x: data_x, y: data_b_calc}, $
      dlim = {panel_size: 1, ytitle: 'B model'}
    store_data, name + '_B_EQ', data = {x: data_x, y: data_b_eq}, $
      dlim = {panel_size: 1, ytitle: 'B eq model'}
    store_data, name + '_I', data = {x: data_x, y: data_i}, $
      dlim = {panel_size: 1, ytitle: 'I (bounce)'}

    ylim, name + '_B_CALC', 0, 400
    ylim, name + '_B_EQ', 0, 400
    ylim, name + '_I', 0, 5
  endif
  ; <<--------------------------------------------------------------------
end