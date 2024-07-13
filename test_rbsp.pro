pro test_rbsp 

  ; time='2013-02-28/00:00:00'
  ; timespan, time, 25, /hour         ; SECONDS, MINUTES, HOURS, DAYS (DEFAULT)

  time = '2016-07-08/12:00:00'
  timespan, time, 1, /h

  probe = [1]
  specie = [3]
  bmodel = 'T89Q' ; ; 'OP77Q', 'T89Q' 'T89D', 'TS04D'

  ; energy_range =[24,37] ;[1, 50000]    ; Max range (eV): [1, 50000]
  ; energy_bins_range = [0,71]    ; 0: highest - 71:lowest
  energy_units_in_flux = 'eV' ; per 'keV' or per 'eV'

  units_name = 'DIFF FLUX' ; 'DIFF FLUX', 'EFLUX', 'DIST FUNC

  coordinates = 'GSM'

  eph_data = 0 ; (0|1)
  aux_data = 0 ; (0|1)
  ; eph_sc = probe(n_elements(probe) - 1) ; (1,2)

  moments = ['P']
  angle = [[-90, 90], [0, 360]]
  energy = [0, 50000]

  case units_name of
    'DIFF FLUX': units_str = 'DIFFFLUX'
    ; 'Counts':units_str = 'COUNTS'
    'EFLUX': units_str = 'EFLUX'
  endcase
  ; This units is for pitch angle tlot names of rbsp
  case specie of
    '3': sp_str = '3'
    '0': sp_str = '0'
  endcase

  case probe of
    '1': sc_str = 'A'
    '2': sc_str = 'B'
  endcase

  ; ----------------------------------------------------------------------
  ; PATH = '/net/home/ssc/amenz/ccat_user/data/'
  ; FLN  = 'rbspa_rel01_ect-hope-sci-L2SA_20130115_v3.0.1.cdf'
  ; NEW_NAME = 'HOPE_pitch_angle'

  plot_rbsp_hope_l3_red_enspec, probe, specie, units_name, pa_range = [0, 180], aux_data = 0, eph_data = 0
  ; get_timespan, interval
  ; time_trim_tplot_variable, 1, interval[0], interval[1], keep_v=keep_v

  plot_rbsp_hope_l3_paspec, probe, specie, units_name, energy_range = energy, energy_units_in_flux = energy_units_in_flux, eph_data = eph_data, aux_data = aux_data

  plot_rbsp_emfisis_mag, probe, coordinates

  plot_hope_3dmom, probe, specie, moments, angle, energy ; , distribution = distribution, frs = frs, inst_coord = inst_coord
  
  ; ----------------------------------------------------------------------
  ; Read ephemeris information and plot ephemeris axis
  ; ----------------------------------------------------------------------
  plot_rbsp_magephem, probe, bfield_model = bmodel, all = 1
  ; extract x,y,z ephemeris data for tplot purpose
  get_data, 'RBSPA_Rgse_T89Q', data = data, dlim = dlim, lim = lim
  store_data, 'RBSP' + sc_str + '_EPHEM_' + bmodel + '_GSE_X', data = {x: data.x, y: data.y[*, 0]}
  store_data, 'RBSP' + sc_str + '_EPHEM_' + bmodel + '_GSE_Y', data = {x: data.x, y: data.y[*, 1]}
  store_data, 'RBSP' + sc_str + '_EPHEM_' + bmodel + '_GSE_Z', data = {x: data.x, y: data.y[*, 2]}
  get_data, 'RBSPA_Rgsm_T89Q', data = data, dlim = dlim, lim = lim
  store_data, 'RBSP' + sc_str + '_EPHEM_' + bmodel + '_GSM_X', data = {x: data.x, y: data.y[*, 0]}
  store_data, 'RBSP' + sc_str + '_EPHEM_' + bmodel + '_GSM_Y', data = {x: data.x, y: data.y[*, 1]}
  store_data, 'RBSP' + sc_str + '_EPHEM_' + bmodel + '_GSM_Z', data = {x: data.x, y: data.y[*, 2]}
  ; stop
  var_label = 'RBSP' + sc_str + '_EPHEM_' + bmodel + '_'
  var_label = var_label + ['GSE_X', 'GSE_Y', 'GSE_Z']
  var_label = ['RBSPA_CDMAG_MLT_T89Q', 'RBSPA_InvLat_T89Q'] ; ,'InvLat_T89Q','CDMAG_R_T89Q']
  tplot, var_label = var_label

  ylim, '*enspec*', 1, 5e4
  ylim, '*paspec*', 0, 180
  tplot, [1, 2]

  stop
end