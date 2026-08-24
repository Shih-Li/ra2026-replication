# 02_variable_lists.R
# Direct translation of the global control lists at the top of the authors' Stata analysis file.

controls_head <- c(
  "deci_male", "deci_edu_level", "deci_marr", "deci_age", "deci_head",
  "head_male", "head_edu_level"
)

controls_family <- c(
  "hh_size", "s6_1othrhh_demo", "s18_8othrlatuse_deci",
  "num_baby", "num_children", "num_adult", "num_women", "num_job", "num_pension",
  "num_other_earn", "s14_6respreli", "wolof"
)

controls_house <- c(
  "own_house", "yrinhouse", "live_more10", "moveout_in5", "stay_more10",
  "stay_forever", "twostories", "num_floors", "num_rooms",
  "hh_electricity", "house_clean_always_bl", "wide_road_always_bl",
  "sandy_never_bl", "s18_2ttlpits_deci", "s18_11pitctyd_deci",
  "s26_5animalin_deci", "s26_6animalout_deci", "s7_5roomsrentout_demo", "hh_flooding",
  "floor_tile", "roof_slab", "newhousehold_deci"
)

controls_have <- c(
  "have_cellphone_bl", "have_radio_bl", "have_television_bl", "have_computer_bl",
  "have_bike_bl", "have_motorcycle_bl", "have_car_bl", "have_fan_bl", "have_ac_bl", "have_fridge_bl",
  "have_gasoven_bl", "have_washmachine_bl", "have_microwave_bl", "have_generator_bl", "hh_assets",
  "hh_animals", "have_cows_howmany_bl", "have_sheep_howmany_bl", "have_goats_howmany_bl",
  "have_pigs_howmany_bl", "have_chickens_howmany_bl", "have_otheran_howmany_bl",
  "s7_11awatermeter_demo", "noland", "wealth_ind1_bl"
)

controls_finance <- c(
  "hh_jewelry", "jewelry_bl", "hh_wealthy", "account_any_bl", "account_saving_bl",
  "hh_tontine", "tontines_bl", "hh_tontine_recv1", "s17_14wari_deci"
)

controls_desludge <- c(
  "desludge_1xyear", "desludge_dry", "desludge_rainy", "desludge_freq",
  "desludge_ever", "desludge_twice", "desludge_ever2",
  "desludge_manual", "desludge_mech", "desludge_both",
  "never", "desludge_last_wi1year", "mech_year", "manu_year",
  "desludge_last", "desludge_last_rain", "desludge_last_2days",
  "desludge_manfam_last", "desludge_manbp_last", "desludge_mech_last"
)

controls_network <- c(
  "num_all_nghbrknow", "num_all_nghbrtea", "num_all_nghbrleadhealth",
  "num_all_nghbrsanitation", "num_all_hhwealthy"
)

controls_pref <- c(
  "trust", "advantage", "time_pref_now", "time_pref_delay", "time_consistent", "time_hyperbolic",
  "time_odd", "save_desludge_bl", "prefer_payonce_bl", "s21_3posrecip_deci", "s21_4negrecip_deci"
)

controls_health <- c(
  "diarrhea_all", "diarrhea_share", "diarrhea_children", "diarrhea_baby",
  "cough_all", "cough_share", "cough_children", "cough_baby"
)

controls_survey <- c(
  "s26_7enumprob_deci", "reliable", "s26_8accompany_deci", "s26_10qrrelang_deci",
  "deci_bl_date", "months_between", "in_endline"
)

controls_all <- c(
  controls_head, controls_family, controls_house, controls_have, controls_finance,
  controls_desludge, controls_network, controls_pref, controls_health, controls_survey
)
