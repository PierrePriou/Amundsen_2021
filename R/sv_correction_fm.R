sv_correction_fm <- 
  function (f_nominal, f_start, f_end, c_cal, t_cal, psi_cal, p_cal, abs_depth_cal,
            pH_cal, sal_cal, temp_cal, lat, Sv, range, c_new, abs_new) 
{
  
  if (missing(f_nominal)) 
    stop("missing f_nominal")
  # if (f_nominal!=38 || f_nominal!=70 || f_nominal!=120 || f_nominal!=200 || f_nominal!=333) 
  #   stop("must provide nominal frequency (kHz) among 38, 70, 120, 200, or 333 kHz")
  if (missing(f_start)) 
    stop("must provide start frequency (kHz) from calibration file")
  if (missing(f_end)) 
    stop("must provide end frequency (kHz) from calibration file")
  if (missing(c_cal)) 
    stop("must provide original sound speed (m/s) from calibration file")
  if (missing(Sv)) 
    stop("must provide Sv (dB) that needs correction")
  if (missing(range)) 
    stop("must provide range")
  if (missing(c_new)) 
    stop("must provide new sound speed (m/s)")
  if (missing(abs_depth_cal)) 
    stop("must provide absorption depth from calibration file (m)")
  if (missing(pH_cal)) 
    stop("must provide acidity from calibration file (pH)")
  if (missing(sal_cal)) 
    stop("must provide salinity from calibration file (ppt)")
  if (missing(temp_cal)) 
    stop("must provide temperature from calibration file (degree C)")
  if (missing(lat)) 
    stop("must provide latitude")
  if (missing(t_cal)) 
    stop("must provide pulse compresed effective pulse duration (msec)")  
  if (missing(psi_cal)) 
    stop("must provide two-way beam anlge (dB re 1 steradian)")
  if (missing(p_cal)) 
    stop("must provide transmitted power (W)")
  if (missing(abs_new)) 
    stop("must provide new absorption coeficient (dB/m)")
  
  
  library(oce)
  # Convert units and calculate constants
  f_nominal <- f_nominal*10^3                                   # Nominal frequency (Hz)
  f_center <- mean(c(f_start, f_end))*10^3                      # Center frequency (Hz)
  a_cal <- swSoundAbsorption(frequency=f_nominal, salinity=sal_cal, temperature=temp_cal,
                             pressure=gsw_p_from_z(abs_depth_cal*-1, lat), pH=pH_cal, 
                             formulation="francois-garrison")   # Absorption coefficient
  t_cal <- t_cal*10^-3                                          # Pulse compressed effective pulse duration (sec)
  equi_psi <- 10^((psi_cal + 20*log10(f_nominal/f_center))/10)  # Equivalent two-way beam angle (steradian)
  lambda_fc <- c_new/f_center                                   # Wavelength at center frequency (m)
  # Calculate variables
  time_return <- ((c_cal*t_cal/4+range)*2)/c_cal   # Time for signal to return (sec)
  range_new <- (time_return*c_new/2)-(c_new*t_cal/4) # Corrected range with new values of sound speed (m)
  # Corrected received power (dB re 1 W)
  p_new <- Sv - (20*log10(range)) - (2*a_cal*range) + (10*log10((p_cal*(lambda_fc^2))/(16*pi^2))) + 
    (10*log10((c_cal*t_cal*equi_psi)/2))
  # Corrected Sv (dB re 1 m-1)
  Sv_corrected <- p_new + (20*log10(range_new)) + (2*abs_new*range_new) - (10*log10((p_cal*(lambda_fc^2))/(16*pi^2))) - 
    (10*log10((c_new*t_cal*equi_psi)/2))
  # Return value
  res <- Sv_corrected
  res
}


# # Test values at 38 kHz
# f_nominal <- 38         # Nominal frequency (kHz)
# f_start <- 36           # Start frequency (kHz)
# f_end <- 45             # End frequency (kHz)
# c_cal <- 1450           # Original sound speed (m/s)
# t_cal <- 0.12351        # Pulse compressed effective pulse duration (msec)
# psi_cal <- -12.5        # Two-way beam angle (dB re 1 steradian)
# p_cal <- 450            # Transmitted power (W)
# abs_depth_cal <- 300    # Absorption depth (m)
# pH_cal <- 8             # pH (pH)
# sal_cal <- 34           # Salinity
# temp_cal <- 0           # Temperature (degree Celsius)
# lat <- 78               # Latitude (decimal degrees)
# Sv <- -37.6819          # Sv (dB re 1 m-1)
# range <- 4              # Range (m)
# c_new <- 1436.209       # New sound speed (m/s)
# abs_new <- 0.008995094  # New absorption coefficient


# test <- Sv_FM_depth_correct(f_nominal=38, f_start=36, f_end=45, c_cal=1450, t_cal=0.12351, psi_cal=-12.5,
#                    p_cal=450, abs_depth_cal=300, pH_cal=8, sal_cal=34, temp_cal=0, lat=78,
#                    Sv=-37.6819, range=4, c_new=1436.209, abs_new=0.008995094)
