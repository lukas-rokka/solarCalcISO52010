// Copyright (C) 2019 Lukas Lundström

#include <Rcpp.h>
using namespace Rcpp;

// Interpolates table 8 of ISO52010:2017. The brightness coefficients for 
// anisotropic sky conditions (Perez model) as a function of the clearness parameter eps.
// @param eps The dimensionless clearness parameter
// @param ff The brightness coefficients
void interp_f(double eps, double (&ff)[2][3], int interp) {
  static double f_tbl[8][3][3] = 
    { { {-0.008,  0.588, -0.062 }, {-0.060,  0.072, -0.022 }, { 1.065, 0, 0 } },
      { { 0.130,  0.683, -0.151 }, {-0.019,  0.066, -0.029 }, { 1.140, 0, 0 } },
      { { 0.330,  0.487, -0.221 }, { 0.055, -0.064, -0.026 }, { 1.365, 0, 0 } },
      { { 0.568,  0.187, -0.295 }, { 0.109, -0.152, -0.014 }, { 1.725, 0, 0 } },
      { { 0.873, -0.392, -0.362 }, { 0.226, -0.462,  0.001 }, { 2.375, 0, 0 } },
      { { 1.132, -1.237, -0.412 }, { 0.288, -0.823,  0.056 }, { 3.650, 0, 0 } },
      { { 1.060, -1.600, -0.359 }, { 0.264, -1.127,  0.131 }, { 5.350, 0, 0 } },
      { { 0.678, -0.327, -0.250 }, { 0.156, -1.377,  0.251 }, { 6.200, 0, 0 } } };
  int ind;
  
  if (eps >= 6.2) {
    for (int i = 0; i < 3; i++) {
      ff[0][i] = f_tbl[7][0][i];
      ff[1][i] = f_tbl[7][1][i];
    }
    return;
  }
  else if (eps <= 1.065) {
    for (int i = 0; i < 3; i++) {
      ff[0][i] = f_tbl[0][0][i];
      ff[1][i] = f_tbl[0][1][i];
    }
    return;
  }
  
  if (interp > 0) {
    if (eps > 5.35) ind = 7;
    else if (eps > 3.65) ind = 6;
    else if (eps > 2.375) ind = 5;
    else if (eps > 1.725) ind = 4;
    else if (eps > 1.365) ind = 3;
    else if (eps > 1.14) ind = 2;
    else ind = 1;
    
    for (int i = 0; i < 3; i++) {
      ff[0][i] = ((f_tbl[ind][0][i])*(eps - f_tbl[ind-1][2][0]) + (f_tbl[ind-1][0][i])*(f_tbl[ind][2][0] - eps)) / (f_tbl[ind][2][0] - f_tbl[ind-1][2][0]);
      ff[1][i] = ((f_tbl[ind][1][i])*(eps - f_tbl[ind-1][2][0]) + (f_tbl[ind-1][1][i])*(f_tbl[ind][2][0] - eps)) / (f_tbl[ind][2][0] - f_tbl[ind-1][2][0]);
    }
  } else {
    if (eps >= 4.5) ind = 6;
    else if ( eps >= 2.8) ind = 5;
    else if ( eps >= 1.95) ind = 4;
    else if ( eps >= 1.5) ind = 3;
    else if ( eps >= 1.23) ind = 2;
    else ind = 1; //if ( eps >= 1.065) {
    
    for (int i = 0; i < 3; i++) {
      ff[0][i] = f_tbl[ind][0][i];
      ff[1][i] = f_tbl[ind][1][i];
    }
    
  }
  return;
}


/*
// for testing interp_f
// [[Rcpp::export]]

NumericMatrix test_interp(double eps, int interp) {
  NumericMatrix ff(2,3);
  double f_ij[2][3];
  interp_f(eps, f_ij, interp);
  for (int i=0; i < 2; i++) 
    for (int j=0; j < 3; j++)
      ff(i, j) = f_ij[i][j];
      
  return(ff);
}
*/

//' @title ISO 52010-1:2017 solar irradiance calculation
//' 
//' @description Calculate solar irradiance on a surface with arbitrary orientation and tilt
//' 
//' @param lat Latitude, in radians
//' @param lng Longitude, in radians
//' @param tz Time zone of the irradiance data, in hours. E.g. +1 for central European 
//' (UTC+1) time zones. Use 0, if data is recorded in UTC time.
//' @param t_shift Decimal hours to shift timestamps with. E.g. timestamp 12:00 usually refer to the
//' solar irradaince during time interval 11:00 - 12:00, then use t_shift = 0.5. 
//' If the timestamp 12:00 refer to interval 11:45 - 12:15, use t_shift = 0.0. 
//' If the timestamp 12:00 refer to interval 11:45 - 12:00, use t_shift = 0.125.
//' @param surfaceAzimuths Vector of surface azimuths, in radians
//' @param surfaceTilts Vector of surface tilts, in radians. Need to be same length as surfaceAzimuths
//' @param n_hour Vector representing the hour of the day for every time step (double 0-24)
//' @param n_day Vector representing the day of the year for every time step (integer 1-366)
//' @param G_dir Vector of direct normal irradiance, W/m2
//' @param G_dif Vector of diffuse horizontal irradiance, W/m2
//' @param albedo Vector of solar reflectivity of the ground, 0.2 can be used if actual conditions are unknown.
//' @param interp_perez Set to 0 to interpolate from the Parez table (clearness index	and	brightness coefficients). 
//' Set to 1 to use binned values, this follow the procedure of the ISO 52010-1:2017 standard. 
//' @return A matrix where first column holds the solar altitude for each time step,
//' from second column calculated total direct irradiances (I_tot_dir) for each surface are returned, 
//' and from column position 1 + "number of surfaces" total diffuse irradiances are returned
//' @export 
// [[Rcpp::export]]
NumericMatrix rcpp_ISO52010(double lat, double lng, double tz, double t_shift, 
                            NumericVector surfaceAzimuths, NumericVector surfaceTilts,
                            NumericVector n_hour, NumericVector n_day,
                            NumericVector G_dir, NumericVector G_dif, 
                            NumericVector albedo, int interp_perez) {
  
  // local meridian in rad. - for west & + for east of the prime meridian.
  double PI = 3.1415927;
  int n = n_hour.size();
  int n_surfaces = surfaceAzimuths.size();
  NumericMatrix I_tot(n, n_surfaces*2 + 1);
  
  double t_eq;
  double cos_lat = cos(lat);
  double sin_lat = sin(lat);
  //double fai;
  
  for (int i=0; i < n; i++) {
    
    if ((G_dif[i] + G_dir[i]) < 0.1) {
      for (int j=0; j < n_surfaces*2; j++) 
        I_tot(i, 1 + j) = 0;
      continue; // skip hours with sunshine below threshold
    }
    
    // ISO52010:2017 eq 2: earth orbit deviation as a function of the day, in radians
    double R_dc = 2.0*PI*n_day[i]/365.0;
    
    // ISO52010:2017 eq 1: solar declination, in radians
    double delta = (0.33281 - 22.984*cos(R_dc) - 0.3499*cos(2*R_dc) - 0.1398*cos(3*R_dc) + 3.7872*sin(R_dc) + 0.03205*sin(2*R_dc) + 0.07187*sin(3*R_dc))*PI/180;
    double sin_delta = sin(delta);  // calc once
    double cos_delta = cos(delta);  // calc once
    
    // ISO52010:2017 eq 3-7: the equations of time
    if (n_day[i] < 21) {
      t_eq = 2.6 + 0.44*n_day[i];
    } else if (n_day[i] < 136) {
      t_eq = 5.2 + 9.0*cos((n_day[i] - 43)*0.0357);
    } else if (n_day[i] < 241) {
      t_eq = 1.4 - 5.0*cos((n_day[i] - 135)*0.0449);
    } else if (n_day[i] < 336) {
      t_eq = -6.3 - 10.0*cos((n_day[i] - 306)*0.036);
    } else {
      t_eq = 0.45*(n_day[i] - 359);
    }
    
    // ISO52010:2017 eq 8: The time shift, i hours. 15 degrees ~ 0.2618 rad
    double t_shift2 = tz - lng/0.2618;
    
    // ISO52010:2017 eq 9: The solar time
    double t_sol = n_hour[i] - t_eq / 60.0 - t_shift2;
    
    // ISO52010:2017 eq 10: solar hour angle in radians. 
    // NOTE: standard use 12 + 0.5 to shift to middle of full hour. Here t_shift is added instead, 
    // e.g. use t_shift=0.5 for hourly interval data and t_shift=0.125 for 15 min data. 
    // NOTE: 0:xx o'clock should be converted to 24:xx o'clock
    double H = 15 * (12 + t_shift - t_sol) * PI / 180.0;
    double cos_H = cos(H);  
    
    // fixes H between -180 and 180 degree (can be skipped if no sunshine at midnight is expected,
    // or 0 o'clock is denoted as the 24th hour of the day before)
    if (H > PI) H = H - 2*PI;
    else if (H < -PI) H = H + 2*PI;
    
    // ISO52010:2017 eq 11: Calculate solar altitude angle in radians.
    double alfa = asin(sin_delta*sin_lat + cos_delta*cos_lat*cos_H);
    if (alfa < 0.0001) alfa = 0;
    
    // ISO52010:2017 eq 12: the solar zenith angle
    double theta_z = PI/2 - alfa;
    
    // fai, solar azimuth angle, not calculted (can be used for shading calc)
    /*
    // calc once
    double fai_0 = cos(asin(sin(alfa)));
    
    // ISO52010:2017 eq 13: auxiliary variable
    double sin_fai_1 = cos_delta * sin(H) / fai_0;
    
    // ISO52010:2017 eq 14: auxiliary variable
    double cos_fai_1 = (cos_lat*sin_delta + sin_lat*cos_delta*cos(PI-H)) / fai_0;
    
    // ISO52010:2017 eq 15: auxiliary variable
    // double fai_2 = asin(cos_delta*sin(PI-H)) / fai_0; see https://github.com/kristss/solarCalcISO52010/commit/afc7cd20f4dda3f975c3caf9893ab393dd5f7d22
    double fai_2 = asin((cos_delta*sin(PI-H)) / fai_0)
    
    // ISO52010:2017 eq 16: the solar azimuth angle
    if ((sin_fai_1 >= 0.0) & (cos_fai_1 > 0.0)) {
    fai = (PI - fai_2);
    } else if (cos_fai_1 < 0.0 ) {
    fai = fai_2;
    } else {
    fai = -(PI + fai_2);
    }
    */
    
    // ISO52010:2017 eq 20-21: The air mass, the distance solar beams travel through atmosphere
    // 0.175 ~ 10 degrees
    double m = (alfa > 0.175) ? 1/sin(alfa) : 1/(sin(alfa) + 0.15*pow(alfa*180/PI + 3.885, -1.253));
    
    // ISO52010:2017 eq 27: extra-terrestrial radiation, G_sol;c=1370 W/m2
    double I_ext = 1370*(1 + 0.033*cos(R_dc));
    
    // ISO52010:2017 eq 29: cos(85*pi/180)=0.087156
    double b = std::max(0.087156, cos(theta_z));
    
    // ISO52010:2017 eq 30: clearness parameter, anisotropic sky conditions (Perez model)
    double eps = (G_dif[i] <= 0.0) ? 999 : ((G_dif[i]+G_dir[i])/(G_dif[i]) + 1.014*pow(alfa, 3.0)) / (1 + 1.014*pow(alfa, 3.0));
    
    // get interpolated f_ij values, based on ISO52010:2017 table 8
    // ISO52010 use table 8 as a binned lookup table, here it is interoplated. 
    double f_ij[2][3];
    interp_f(eps, f_ij, interp_perez);
    
    // ISO52010:2017 eq 31: sky brightness parameter
    double Delta_sky = m * G_dif[i] / I_ext;
    
    // ISO52010:2017 eq 32: The circumsolar brightness coefficient
    double F1 = std::max(0.0, f_ij[0][0] + f_ij[0][1]*Delta_sky + f_ij[0][2]*theta_z);
    // ISO52010:2017 eq 33: horizontal brightness coefficient
    double F2 = f_ij[1][0] + f_ij[1][1]*Delta_sky + f_ij[1][2]*theta_z;
    
    
    I_tot(i, 0) = alfa;
    
    for (int s = 0; s < n_surfaces; s++) {
      
      double cos_beta_ic = cos(surfaceTilts[s]);    // cos of suface tilt angle
      double sin_beta_ic = sin(surfaceTilts[s]);    // sin of tilt angle
      double cos_upsilon = cos(surfaceAzimuths[s]); // cos of surface azimuth angle
      double sin_upsilon = sin(surfaceAzimuths[s]); // sin of surface azimuth angle
      // ISO52010:2017 eq 35: ground reflection. 
      double I_dif_grnd = (G_dif[i] + G_dir[i] * sin(alfa)) * albedo[i]* (1 - cos_beta_ic) / 2;
      // if tilt angle = 90 d (vertical) then eq 35 can be simplified to
      // double I_dif_grnd = (G_dif[i] + G_dir[i] * sin(alfa)) * albedo[i] / 2;
      
      // ISO52010:2017 eq 17: The solar angle of incidence
      
      double cos_theta_ic = sin_delta*sin_lat*cos_beta_ic - 
        sin_delta*cos_lat*sin_beta_ic*cos_upsilon + 
        cos_delta*cos_lat*cos_beta_ic*cos_H + 
        cos_delta*sin_lat*sin_beta_ic*cos_upsilon*cos_H + 
        cos_delta*sin_beta_ic*sin_upsilon*sin(H);
      
      // the case of vertical walls, eq 17 can be simplified to
      //double cos_theta_ic = 0 - sin_delta*cos_lat*cos_upsilon + 
      //  0 + cos_delta*sin_lat*cos_upsilon*cos_H + 
      //  cos_delta*sin_upsilon*sin(H);
      
      // ISO52010:2017 eq 28: (error in text, theta_sol should be cos_theta_ic)
      double a = std::max(0.0, cos_theta_ic);
      
      // ISO52010:2017 eq 34: diffuse irradiance.
      double I_dif = G_dif[i]*((1 - F1)*(1 + cos_beta_ic)/2 + F1*a/b + F2*sin_beta_ic);
      // simpylfied version when tilt angle = 90 d
      // double I_dif = G_dif[i]*((1-F1)/2 + F1*a/b + F2);
      
      // ISO52010:2017 eq 36: circumsolar irradiance;
      double I_circum = G_dif[i]*F1*a/b;
      
      // ISO52010:2017 eq 38: total diffuse irradiance;
      double I_dif_tot = I_dif - I_circum + I_dif_grnd;
      
      // ISO52010:2017 eq 26: The solar beam irradiance
      double I_dir = (cos_theta_ic < 0) ? 0.0 : cos_theta_ic*G_dir[i];
      
      // ISO52010:2017 eq 37: total direct irradiance;
      double I_dir_tot = I_dir + I_circum;
      
      // ISO52010:2017 eq 39: hemispherical or total solar irradiance;
      //double I_tot = I_dif_tot + I_dir_tot;
      
      I_tot(i, 1 + s) = I_dir_tot;
      I_tot(i, 1 + n_surfaces + s) = I_dif_tot;
      //I_tot(i, 1 + s) = eps;
      //I_tot(i, 1 + n_surfaces + s) = I_dir_tot + I_dif_tot;
      
    }
    
  }
  return I_tot;
}

//' @title ISO 52010-1:2017 solar altitude and azimuth
//' 
//' @description Calculate solar altitude and azimuth
//' 
//' @param lat Latitude, in radians
//' @param lng Longitude, in radians
//' @param tz Time zone of the irradiance data, in hours. E.g. +1 for central European 
//' (UTC+1) time zones. Use 0, if data is recorded in UTC time.
//' @param t_shift Decimal hours to shift timestamps with. E.g. timestamp 12:00 usually refer to the
//' solar irradaince during time interval 11:00 - 12:00, then use t_shift = 0.5. 
//' If the timestamp 12:00 refer to interval 11:45 - 12:15, use t_shift = 0.0. 
//' If the timestamp 12:00 refer to interval 11:45 - 12:00, use t_shift = 0.125.
//' @param n_hour Vector representing the hour of the day for every time step (double 0-24)
//' @param n_day Vector representing the day of the year for every time step (integer 1-366)
//' @return A matrix where first column holds the solar altitude for each time step and second column holds solar azimuth angle
//' @export 
// [[Rcpp::export]]
NumericMatrix rcpp_ISO52010_angles(double lat, double lng, double tz, double t_shift, 
                                 NumericVector n_hour, NumericVector n_day) {
  
  double PI = 3.1415927;
  // local meridian in rad. - for west & + for east of the prime meridian.
  
  int n = n_hour.size();
  
  double t_eq;
  double cos_lat = cos(lat);
  double sin_lat = sin(lat);
  double fai;
  NumericMatrix ret(n, 2);
  
  for (int i=0; i < n; i++) {
    
    // ISO52010:2017 eq 2: earth orbit deviation as a function of the day, in radians
    double R_dc = 2.0*PI*n_day[i]/365.0;
    
    // ISO52010:2017 eq 1: solar declination, in radians
    double delta = (0.33281 - 22.984*cos(R_dc) - 0.3499*cos(2*R_dc) - 0.1398*cos(3*R_dc) + 3.7872*sin(R_dc) + 0.03205*sin(2*R_dc) + 0.07187*sin(3*R_dc))*PI/180;
    double sin_delta = sin(delta);  // calc once
    double cos_delta = cos(delta);  // calc once
    
    // ISO52010:2017 eq 3-7: the equations of time
    if (n_day[i] < 21) {
      t_eq = 2.6 + 0.44*n_day[i];
    } else if (n_day[i] < 136) {
      t_eq = 5.2 + 9.0*cos((n_day[i] - 43)*0.0357);
    } else if (n_day[i] < 241) {
      t_eq = 1.4 - 5.0*cos((n_day[i] - 135)*0.0449);
    } else if (n_day[i] < 336) {
      t_eq = -6.3 - 10.0*cos((n_day[i] - 306)*0.036);
    } else {
      t_eq = 0.45*(n_day[i] - 359);
    }
    
    // ISO52010:2017 eq 8: The time shift, i hours. 15 degrees ~ 0.2618 rad
    double t_shift2 = tz - lng/0.2618;
    
    // ISO52010:2017 eq 9: The solar time
    double t_sol = n_hour[i] - t_eq / 60.0 - t_shift2;
    
    // ISO52010:2017 eq 10: solar hour angle in radians. 
    // NOTE: standard use 12 + 0.5 to shift to middle of full hour. Here t_shift is added instead, 
    // e.g. use t_shift=0.5 for hourly interval data and t_shift=0.125 for 15 min data. 
    // NOTE: 0:xx o'clock should be converted to 24:xx o'clock
    double H = 15 * (12 + t_shift - t_sol) * PI / 180.0;
    double cos_H = cos(H);  
    
    // fixes H between -180 and 180 degree (can be skipped if no sunshine at midnight is expected,
    // or 0 o'clock is denoted as the 24th hour of the day before)
    if (H > PI) H = H - 2*PI;
    else if (H < -PI) H = H + 2*PI;
    
    // ISO52010:2017 eq 11: Calculate solar altitude angle in radians.
    double alfa = asin(sin_delta*sin_lat + cos_delta*cos_lat*cos_H);
    if (alfa < 0.0001) alfa = 0;
    
    // ISO52010:2017 eq 12: the solar zenith angle
    double theta_z = PI/2 - alfa;
    
    // calc once
    double fai_0 = cos(asin(sin(alfa)));
    
    // ISO52010:2017 eq 13: auxiliary variable
    double sin_fai_1 = cos_delta * sin(H) / fai_0;
    
    // ISO52010:2017 eq 14: auxiliary variable
    double cos_fai_1 = (cos_lat*sin_delta + sin_lat*cos_delta*cos(PI-H)) / fai_0;
    
    // ISO52010:2017 eq 15: auxiliary variable
    // double fai_2 = asin(cos_delta*sin(PI-H)) / fai_0; see https://github.com/kristss/solarCalcISO52010/commit/afc7cd20f4dda3f975c3caf9893ab393dd5f7d22
    double fai_2 = asin((cos_delta*sin(PI-H)) / fai_0);
    
    // ISO52010:2017 eq 16: the solar azimuth angle
    if ((sin_fai_1 >= 0.0) & (cos_fai_1 > 0.0)) {
      fai = (PI - fai_2);
    } else if (cos_fai_1 < 0.0 ) {
      fai = fai_2;
    } else {
      fai = -(PI + fai_2);
    }
    ret(i, 0) = alfa;
    ret(i, 1) = fai;
  }
  return ret;
}
