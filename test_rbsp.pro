pro test_rbsp

  time='2014-10-26/00:00:00'
  timespan, time, 1, /D         ; SECONDS, MINUTES, HOURS, DAYS (DEFAULT)

  probe = [1]
  specie = [3]

  energy_range = [1, 50000]    ; Max range (eV): [1, 50000]
;  energy_bins_range = [0,71]    ; 0: highest - 71:lowest
  energy_units_in_flux = 'keV'  ; per 'keV' or per 'eV'

  units_name = 'DIFF FLUX'      ; 'DIFF FLUX', 'EFLUX', 'DIST FUNC

  coordinates = 'GSM'

  eph_data = 0                      ; (0|1)
  aux_data = 0                      ; (0|1)
  eph_sc = probe(n_elements(probe)-1) ; (1,2)

  moments = ['P']
  angle = [[-90,90],[0,360]]
  energy = [0,50000]
;----------------------------------------------------------------------
;PATH = '/net/home/ssc/amenz/ccat_user/data/'
;FLN  = 'rbspa_rel01_ect-hope-sci-L2SA_20130115_v3.0.1.cdf'
;NEW_NAME = 'HOPE_pitch_angle'

plot_rbsp_hope_3d_enspec, probe, specie, units_name

;plot_rbsp_hope_paspec, probe, specie, units_name 		
  plot_rbsp_hope_l3_paspec, probe, specie, units_name, energy_range = energy_range, energy_bins_range = energy_bins_range,energy_units_in_flux = energy_units_in_flux, eph_data=eph_data, aux_data=aux_data
     
;plot_rbsp_emfisis_mag, probe, coordinates

plot_hope_3dmom, probe, specie, moments, angle, energy, distribution=distribution, frs=frs, inst_coord=inst_coord 

;----------------------------------------------------------------------
; Read ephemeris information and plot ephemeris axis
;----------------------------------------------------------------------
;get_rbsp_ephemeris, eph_sc, /MLT, /L_SHELL_D, /ILAT_D, /DIST
;get_rbsp_hope_ephem.pro
;var_label = 'EPH_SC'+STRING(eph_sc, FORMAT='(i1.1)')+'_'
;var_label = var_label + ['MLT','L_SHELL_D','ILAT_D','DIST']
;tplot, var_label=var_label

;tplot,[1,2,3]

  stop
end
