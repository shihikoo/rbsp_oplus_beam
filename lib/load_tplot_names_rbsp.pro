function load_tplot_names_rbsp, probe, sp_str, bmodel, parallel_pa_range, antiparallel_pa_range, energy_range = energy_range
  
  coord = 'GSM'

  if ~keyword_set(energy_range) then energy_range = [0, 51767]
  energy_low_str = strcompress(string(energy_range[0], format = '(i5.5)'), /remove_all)
  energy_high_str = strcompress(string(energy_range[1], format = '(i5.5)'), /remove_all)

  parallel_pa_low_str = strcompress(string(parallel_pa_range[0], format = '(i4.4)'), /remove_all)
  parallel_pa_high_str = strcompress(string(parallel_pa_range[1], format = '(i4.4)'), /remove_all)

  antiparallel_pa_low_str = strcompress(string(antiparallel_pa_range[0], format = '(i4.4)'), /remove_all)
  antiparallel_pa_high_str = strcompress(string(antiparallel_pa_range[1], format = '(i4.4)'), /remove_all)

  all_tplot_names = { $
    ephemeris_names: 'RBSP' + probe + '*_' + bmodel $
    , gse_name: 'RBSP' + probe + '_' + 'Rgse_' + bmodel $
    , x_gse_name: 'RBSP' + probe+ '_' + bmodel + '_GSE_X' $
    , y_gse_name: 'RBSP'+ probe + '_' + bmodel + '_GSE_Y' $
    , z_gse_name: 'RBSP'+ probe + '_' + bmodel + '_GSE_Z' $
    , gsm_name: 'RBSP' + probe + '_' + 'Rgsm_' + bmodel $
    , x_gsm_name: 'RBSP'+ probe + '_' + bmodel + '_GSM_X' $
    , y_gsm_name: 'RBSP' + probe+ '_' + bmodel + '_GSM_Y' $
    , z_gsm_name: 'RBSP'+ probe + '_' + bmodel + '_GSM_Z' $
    , mlt_name: 'RBSP' + probe + '_' + 'CDMAG_MLT_' + bmodel $
    , ilat_name: 'RBSP' + probe + '_' + 'InvLat_' + bmodel $
    , l_name: 'RBSP' + probe + '_' + 'Lsimple_' + bmodel $
    , dist_name: 'RBSP' + probe + '_' + 'CDMAG_R_' + bmodel $
    , mag_names: 'RBSP' + probe + '_EMFISIS_L3_MAG_4SEC_' + coord + '*' $
    , mag_name: 'RBSP' + probe + '_EMFISIS_L3_MAG_4SEC_' + coord $
    , mag_pressure_name: 'RBSP' + probe + '_EMFISIS_L3_MAG_4SEC_' + coord + '_MAG_PR' $
    , bx_name: 'RBSP' + probe + '_EMFISIS_L3_MAG_4SEC_' + coord + '_X' $
    , by_gsm_name: 'RBSP' + probe + '_EMFISIS_L3_MAG_4SEC_' + coord + '_Y' $
    , bz_gsm_name: 'RBSP' + probe + '_EMFISIS_L3_MAG_4SEC_' + coord + '_Z' $
    , bt_name: 'RBSP' + probe + '_EMFISIS_L3_MAG_4SEC_' + coord + '_T' $
    , moments_names: 'HOPE_3DMOM_' + probe + '_EN' + energy_low_str + '_' + energy_high_str + '_MT*' $
    , h1_pressure_name: 'HOPE_3DMOM_' + probe + '_EN' + energy_low_str + '_' + energy_high_str + '_MTP_SP0_T' $
    , o1_pressure_name: 'HOPE_3DMOM_' + probe + '_EN' + energy_low_str + '_' + energy_high_str + '_MTP_SP3_T' $
    , h1_density_name: 'HOPE_3DMOM_' + probe + '_EN' + energy_low_str + '_' + energy_high_str + '_MTD_SP0' $
    , o1_density_name: 'HOPE_3DMOM_' + probe + '_EN' + energy_low_str + '_' + energy_high_str + '_MTD_SP3' $
    , h1_velocity_name: 'HOPE_3DMOM_' + probe + '_EN' + energy_low_str + '_' + energy_high_str + '_MTV_SP0' $
    , o1_velocity_name: 'HOPE_3DMOM_' + probe + '_EN' + energy_low_str + '_' + energy_high_str + '_MTV_SP3' $
    , h1_velocity_t_name: 'HOPE_3DMOM_' + probe + '_EN' + energy_low_str + '_' + energy_high_str + '_MTV_SP0_T' $
    , o1_velocity_t_name: 'HOPE_3DMOM_' + probe + '_EN' + energy_low_str + '_' + energy_high_str + '_MTV_SP3_T' $
    , h1_velocity_x_name: 'HOPE_3DMOM_' + probe + '_EN' + energy_low_str + '_' + energy_high_str + '_MTV_SP0_X' $
    , o1_velocity_x_name: 'HOPE_3DMOM_' + probe + '_EN' + energy_low_str + '_' + energy_high_str + '_MTV_SP3_X' $
    , h1_velocity_y_name: 'HOPE_3DMOM_' + probe + '_EN' + energy_low_str + '_' + energy_high_str + '_MTV_SP0_Y' $
    , o1_velocity_y_name: 'HOPE_3DMOM_' + probe + '_EN' + energy_low_str + '_' + energy_high_str + '_MTV_SP3_Y' $
    , h1_velocity_z_name: 'HOPE_3DMOM_' + probe + '_EN' + energy_low_str + '_' + energy_high_str + '_MTV_SP0_Z' $
    , o1_velocity_z_name: 'HOPE_3DMOM_' + probe + '_EN' + energy_low_str + '_' + energy_high_str + '_MTV_SP3_Z' $
    , h1_velocity_par_name: 'HOPE_3DMOM_' + probe + '_EN' + energy_low_str + '_' + energy_high_str + '_MTV_SP0_par' $     
    , o1_velocity_par_name: 'HOPE_3DMOM_' + probe + '_EN' + energy_low_str + '_' + energy_high_str + '_MTV_SP3_par' $
    , h1_velocity_perp_name: 'HOPE_3DMOM_' + probe + '_EN' + energy_low_str + '_' + energy_high_str + '_MTV_SP0_perp' $
    , o1_velocity_perp_name: 'HOPE_3DMOM_' + probe + '_EN' + energy_low_str + '_' + energy_high_str + '_MTV_SP3_perp' $
    , beta_name: 'Plasma_Beta_SC' + probe $
    , p_total_name: 'Pressure_total_SC' + probe $
    , density_ratio_name: 'Density_ratio_oplus_hplus_SC' + probe $
    , diffflux_h1_name: 'RBSP' + probe + '_HOPE_l3_red_enspec_sp0_DIFFFLUX_0000_0180' $
    , eflux_h1_name: 'RBSP' + probe + '_HOPE_l3_red_enspec_sp0_EFLUX_0000_0180' $
    , diffflux_h1_pa_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp0_DIFFFLUX_00000_51767' $
    , eflux_h1_pa_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp0_EFLUX_00000_51767' $
    , diffflux_o1_name: 'RBSP' + probe + '_HOPE_l3_red_enspec_sp3_DIFFFLUX_0000_0180' $
    , eflux_o1_name: 'RBSP' + probe + '_HOPE_l3_red_enspec_sp3_EFLUX_0000_0180' $
    , diffflux_o1_pa_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp3_DIFFFLUX_00000_51767' $
    , eflux_o1_pa_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp3_EFLUX_00000_51767' $
    , diffflux_h1_para_name: 'RBSP' + probe + '_HOPE_l3_red_enspec_sp0_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str $
    , diffflux_h1_perp_name: 'RBSP' + probe + '_HOPE_l3_red_enspec_sp0_DIFFFLUX_0075_0105' $
    , diffflux_h1_anti_name: 'RBSP' + probe + '_HOPE_l3_red_enspec_sp0_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str $

    , diffflux_ion_parallel_name: 'RBSP' + probe + '_HOPE_l3_red_enspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str $
    , diffflux_ion_parallel_subtracted_name: 'RBSP' + probe + '_HOPE_l3_red_enspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str + '_subtracted' $
    , int_diffflux_ion_parallel_subtracted_name: 'RBSP' + probe + '_HOPE_l3_red_enspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str + '_subtracted_int' $
    , eflux_ion_parallel_name: 'RBSP' + probe + '_HOPE_l3_red_enspec_sp'+sp_str+'_EFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str $
    , diffflux_ion_antiparallel_name: 'RBSP' + probe + '_HOPE_l3_red_enspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str $
    , diffflux_ion_antiparallel_subtracted_name: 'RBSP' + probe + '_HOPE_l3_red_enspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str + '_subtracted' $
    , int_diffflux_ion_antiparallel_subtracted_name: 'RBSP' + probe + '_HOPE_l3_red_enspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str + '_subtracted_int' $
    , eflux_ion_antiparallel_name: 'RBSP' + probe + '_HOPE_l3_red_enspec_sp'+sp_str+'_EFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str $
    , parallel_epcut_name: 'RBSP' + probe + '_HOPE_l3_red_enspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str + '_epcut' $
    , parallel_erange_name: 'RBSP' + probe + '_HOPE_l3_red_enspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str + '_erange' $
    , antiparallel_epcut_name: 'RBSP' + probe + '_HOPE_l3_red_enspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str + '_epcut' $
    , antiparallel_erange_name: 'RBSP' + probe + '_HOPE_l3_red_enspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str + '_erange' $

    , parallel_pa_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str $
    , parallel_pa_eflux_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_EFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str $
    ; , parallel_pa_h_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp0_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str $
    ; , parallel_pa_eflux_h_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp0_EFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str $

    , parallel_pap_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str + '_PAP' $
    , parallel_pap_beam_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str + '_PAP_beam' $
    , parallel_pap_range_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str + '_PAP_range' $
    , parallel_epcut_beam_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str + '_epcut_beam' $
    , parallel_epcut_beam_denergy_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str + '_beam_denergy' $
    , parallel_erange_beam_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str + '_erange_beam' $

   , parallel_density_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str + '_density' $
    ,parallel_pressure_name:'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str +'_pressure'  $
   , parallel_velocity_name:'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str +'_velocity'  $
   , parallel_temperature_name:'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str +'_temperature'  $

    , antiparallel_pa_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str $
    , antiparallel_pa_eflux_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_EFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str $
    , antiparallel_pa_h_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp0_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str $
    , antiparallel_pa_eflux_h_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp0_EFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str $
    , antiparallel_pap_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str + '_PAP' $
    , antiparallel_pap_beam_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str + '_PAP_beam' $
    , antiparallel_pap_range_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str + '_PAP_range' $
    , antiparallel_epcut_beam_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str + '_epcut_beam' $
    , antiparallel_epcut_beam_denergy_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str + 'epcut_beam_denergy' $
    , antiparallel_erange_beam_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str + 'erange_beam' $

    , antiparallel_density_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str + '_density' $
    , antiparallel_pressure_name:'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str +'_pressure' $
   , antiparallel_velocity_name:'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str +'_velocity'  $
   , antiparallel_temperature_name:'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str +'_temperature'  $

    , pap_beam_combine_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_COMBINED' + '_beam' $

    , omni_tplot_names: 'OMNI_HR*' $
    , imf_bx_name: 'OMNI_HR_Bx_gse' $
    , imf_by_gsm_name: 'OMNI_HR_By_gsm' $
    , imf_bz_gsm_name: 'OMNI_HR_Bz_gsm' $
    , sw_v_name: 'OMNI_HR_flow_speed' $
    , sw_p_name: 'OMNI_HR_flow_pressure' $
    , sw_n_name: 'OMNI_HR_proton_density' $
    , sw_t_name: 'OMNI_HR_temperature' $
    , sw_mack_number_name: 'OMNI_HR_alfven_mack_number' $
    , dst_name: 'OMNI_HR_SYM_H' $
    , ae_name: 'OMNI_HR_AE_Index' $
    , kp_name: 'Kp_Index' $
    , f107_name: 'F10_7_Index' $
    , storm_phase_tplot_name: 'storm_phase' $
    , substorm_phase_tplot_name: 'substorm_phase' $

    , parallel_beam_inverse_v_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str + '_epcut_beam' + '_1_of_velocity' $
    , antiparallel_beam_inverse_v_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str + '_epcut_beam' + '_1_of_velocity' $
    , region_name: 'Spacecraft_region_sc' + probe $
    , electric_field_h_name: 'electric_field_h_sc' + probe  $
    , electric_field_o_name: 'electric_field_o_sc' + probe  $
    , parallel_dispersion_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str + '_epcut_beam_dispersion' $
    , antiparallel_dispersion_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str + '_epcut_beam_dispersion' $
    , parallel_dispersion_inverse_v_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str + '_epcut' + '_dispersion_1_of_velocity' $
    , antiparallel_dispersion_inverse_v_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str + '_epcut' + '_dispersion_1_of_velocity' $
    , parallel_dispersion_inverse_v_fitting_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str + '_epcut' + '_dispersion_1_of_velocity_fitting' $
    , antiparallel_dispersion_inverse_v_fitting_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str + '_epcut' + '_dispersion_1_of_velocity_fitting' $
    , parallel_dispersion_estimated_distance_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str + '_epcut' + '_dispersion_estimated_distance' $
    , antiparallel_dispersion_estimated_distance_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str + '_epcut' + '_dispersion_estimated_distance' $
    , parallel_dispersion_estimated_distance_error_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_'+parallel_pa_low_str +'_'+ parallel_pa_high_str +'_epcut' +'_dispersion_estimated_distance_error' $
    , antiparallel_dispersion_estimated_distance_error_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str +'_epcut' +'_dispersion_estimated_distance_error' $
    , parallel_dispersion_inverse_v_fitting_chisq_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str + '_epcut' + '_dispersion_1_of_velocity_fitting_chisq' $
    , antiparallel_dispersion_inverse_v_fitting_chisq_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str + '_epcut' + '_dispersion_1_of_velocity_fitting_chisq' $
    , parallel_dispersion_inverse_v_fitting_rsquare_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str + '_epcut' + '_dispersion_1_of_velocity_fitting_rsquare' $
    , antiparallel_dispersion_inverse_v_fitting_rsquare_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str + '_epcut' + '_dispersion_1_of_velocity_fitting_rsquare' $
    , parallel_dispersion_inverse_v_fitting_status_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str + '_epcut' + '_dispersion_1_of_velocity_fitting_status' $
    , antiparallel_dispersion_inverse_v_fitting_status_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str + '_epcut' + '_dispersion_1_of_velocity_fitting_status' $
    , parallel_dispersion_inverse_v_fitting_dof_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str + '_epcut' + '_dispersion_1_of_velocity_fitting_dof' $
    , antiparallel_dispersion_inverse_v_fitting_dof_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str + '_epcut' + '_dispersion_1_of_velocity_fitting_dof' $
    , parallel_dispersion_inverse_v_fitting_sigma_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str + '_epcut' + '_dispersion_1_of_velocity_fitting_sigma' $
    , antiparallel_dispersion_inverse_v_fitting_sigma_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str + '_epcut' + '_dispersion_1_of_velocity_fitting_sigma' $
    , parallel_dispersion_n_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + parallel_pa_low_str + '_' + parallel_pa_high_str + '_epcut_beam_dispersion_n' $
    , antiparallel_dispersion_n_name: 'RBSP' + probe + '_HOPE_l3_paspec_sp'+sp_str+'_DIFFFLUX_' + antiparallel_pa_low_str + '_' + antiparallel_pa_high_str + '_epcut_beam_dispersion_n' $
  
    , parallel_imf_bx_name: 'parallel_OMNI_HR_Bx_gse_delayed' $
    , parallel_imf_by_gsm_name: 'parallel_OMNI_HR_By_gsm_delayed' $
    , parallel_imf_bz_gsm_name: 'parallel_OMNI_HR_Bz_gsm_delayed' $
    , parallel_sw_v_name: 'parallel_OMNI_HR_flow_speed_delayed' $
    , parallel_sw_p_name: 'parallel_OMNI_HR_flow_pressure_delayed' $
    , parallel_sw_n_name: 'parallel_OMNI_HR_proton_density_delayed' $
    , parallel_sw_t_name: 'parallel_OMNI_HR_temperature_delayed' $
    , parallel_sw_mack_number_name: 'parallel_OMNI_HR_alfven_mack_number_delayed' $

    , antiparallel_imf_bx_name: 'antiparallel_OMNI_HR_Bx_gse_delayed' $
    , antiparallel_imf_by_gsm_name: 'antiparallel_OMNI_HR_By_gsm_delayed' $
    , antiparallel_imf_bz_gsm_name: 'antiparallel_OMNI_HR_Bz_gsm_delayed' $
    , antiparallel_sw_v_name: 'antiparallel_OMNI_HR_flow_speed_delayed' $
    , antiparallel_sw_p_name: 'antiparallel_OMNI_HR_flow_pressure_delayed' $
    , antiparallel_sw_n_name: 'antiparallel_OMNI_HR_proton_density_delayed' $
    , antiparallel_sw_t_name: 'antiparallel_OMNI_HR_temperature_delayed' $
    , antiparallel_sw_mack_number_name: 'antiparallel_OMNI_HR_alfven_mack_number_delayed' $

    , parallel_imf_bx_1h_name: 'parallel_OMNI_HR_Bx_gse_delayed_1h' $
    , parallel_imf_by_gsm_1h_name: 'parallel_OMNI_HR_By_gsm_delayed_1h' $
    , parallel_imf_bz_gsm_1h_name: 'parallel_OMNI_HR_Bz_gsm_delayed_1h' $
    , parallel_sw_v_1h_name: 'parallel_OMNI_HR_flow_speed_delayed_1h' $
    , parallel_sw_p_1h_name: 'parallel_OMNI_HR_flow_pressure_delayed_1h' $
    , parallel_sw_n_1h_name: 'parallel_OMNI_HR_proton_density_delayed_1h' $
    , parallel_sw_t_1h_name: 'parallel_OMNI_HR_temperature_delayed_1h' $
    , parallel_sw_mack_number_1h_name: 'parallel_OMNI_HR_alfven_mack_number_delayed_1h' $

    , antiparallel_imf_bx_1h_name: 'antiparallel_OMNI_HR_Bx_gse_delayed_1h' $
    ,  antiparallel_imf_by_gsm_1h_name: 'antiparallel_OMNI_HR_By_gsm_delayed_1h' $
    ,  antiparallel_imf_bz_gsm_1h_name: 'antiparallel_OMNI_HR_Bz_gsm_delayed_1h' $
    ,  antiparallel_sw_v_1h_name: 'antiparallel_OMNI_HR_flow_speed_delayed_1h' $
    ,  antiparallel_sw_p_1h_name: 'antiparallel_OMNI_HR_flow_pressure_delayed_1h' $
    ,  antiparallel_sw_n_1h_name: 'antiparallel_OMNI_HR_proton_density_delayed_1h' $
    ,  antiparallel_sw_t_1h_name: 'antiparallel_OMNI_HR_temperature_delayed_1h' $
    ,  antiparallel_sw_mack_number_1h_name: 'antiparallel_OMNI_HR_alfven_mack_number_delayed_1h' $

    , model_b: 'model_b_sc' + probe $
    , field_line_length_para: 'parallel_field_line_length_sc' + probe $
    , field_line_length_anti: 'antiparallel_field_line_length_sc' + probe $
    }

  RETURN, all_tplot_names
end