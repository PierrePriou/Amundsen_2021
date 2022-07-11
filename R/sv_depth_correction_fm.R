#' Depth-corrected volume backscattering strength
#'
#' Corrects vertical profiles of broadband volume backscattering strength according to changes in depth.
#' 
#' @param Sv volume backscattering strength that needs to be corrected (dB re 1 m-1)
#' @param range range (m)
#' @param f_nominal nominal frequency (kHz)
#' @param f_start start frequency (kHz)
#' @param f_end end frequency (kHz)
#' @param c_cal sound speed(m / s) from calibration file
#' @param t_cal pulse compressed effective pulse duration (ms)
#' @param psi_cal two-way beam angle (dB re 1 steradian)
#' @param p_cal transmitted power (W)
#' @param abs_depth_cal absorption depth (m) from calibration file
#' @param pH_cal acidity (pH) from calibration file
#' @param sal_cal salinity (ppt) from calibration file
#' @param temp_cal temperature (°C) from calibration file
#' @param lat latitude (decimal degrees)
#' @param c_new sound speed (m / s) at depth z
#' @param abs_new absorption coefficient (dB / m) at depth z
#' 
#' @return Corrected volume backscattering coefficient
#' 
#' @examples
#' sv_correction_fm()
#' 
#' @export 
sv_depth_correction_fm <- 
  function (Sv, 
            range,
            f_nominal,
            f_start,
            f_end,
            c_cal,
            t_cal,
            psi_cal,
            p_cal,
            abs_depth_cal,
            pH_cal,
            sal_cal,
            temp_cal,
            lat,
            c_new,
            abs_new) {
    
    ## Set up constants 
    
    # Convert frequencies to Hz
    f_nominal <- f_nominal * 10 ^ 3
    f_center <- mean(c(f_start, f_end)) * 10 ^ 3 # Center frequency (Hz)
    
    # Absorption coefficient calibration
    a_cal <- oce::swSoundAbsorption(frequency = f_nominal, 
                                    salinity = sal_cal,
                                    temperature = temp_cal,
                                    pressure = oce::gsw_p_from_z(abs_depth_cal * -1, 
                                                                 lat), 
                                    pH = pH_cal, 
                                    formulation = "francois-garrison") 
    
    # Pulse compressed in seconds
    t_cal <- t_cal * 10 ^ -3
    
    # Equivalent two-way beam angle (steradian)
    equi_psi <- 10 ^ ((psi_cal + 20 * log10(f_nominal / f_center)) / 10) 
    
    # Wavelength at centre frequency (m)
    lambda_fc <- c_new / f_center 
    
    
    ## Sv corrections for change in sound speed and absorption coefficients
    
    # Time for signal to return (sec)
    time_return <- ((c_cal * t_cal / 4 + range) * 2) / c_cal
    
    # Corrected range with new values of sound speed (m)
    range_new <- (time_return * c_new / 2) - (c_new * t_cal / 4) 
    
    # Corrected received power (dB re 1 W)
    p_new <- Sv - (20 * log10(range)) - (2 * a_cal * range) +
      (10 * log10((p_cal * (lambda_fc ^ 2)) / (16 * pi ^ 2))) + 
      (10 * log10((c_cal * t_cal * equi_psi) / 2)) 
    
    # Corrected Sv (dB re 1 m-1)
    Sv_new <- p_new  + (20 * log10(range_new)) + 
      (2 * abs_new * range_new) -
      (10 * log10((p_cal * (lambda_fc ^ 2)) / (16 * pi ^ 2))) -
      (10 * log10((c_new * t_cal * equi_psi) / 2)) 
    
    # Return corrected Sv
    return(Sv_new)
  }
