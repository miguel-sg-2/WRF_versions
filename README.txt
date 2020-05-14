@@@@@@@@@@@@@
@ WRFv4.1.5 @
@@@@@@@@@@@@@

including...
-Generalized Actuator Disk (GAD) wind turbine model
-Cell perturpation method (CPM) for downscaling turbulence in nested simulations

@@@@@@@@@@@
@ WRF-GAD @
@@@@@@@@@@@

---Relevant citations for WRF-GAD---------------------------------------------------------------------------------------

Mirocha, J.D., Kosovic, B., Aitken, M.L., and Lundquist, J.K. (2014) Implementation of a generalized actuator disk wind 
turbine model into the Weather Research and Forecasting model for large-eddy simulation applications. J. Renew. Sustain. 
Energy, 6, 013104. doi:10.1063/1.4861061

Aitken, M.L., Kosovic, B., Mirocha, J.D., and Lundquist, J.K. (2014) Large eddy simulation of wind turbine wake dynamics 
in the stable boundary layer using the Weather Research and Forecasting Model. J. Renew. Sustain. Energy, 6, 033137.
doi:10.1063/1.4885111

Mirocha, J.D., Rajewski, D.A., Marjanovic, N., Lundquist, J.K., Kosovic, B., Draxl, C., and Churchfield, M.J. (2015) 
Investigating wind turbine impacts on near-wake flow using profiling lidar data and large-eddy simulations with an 
actuator disk model. J. Renew. Sustain. Energy, 7, 043143. doi:10.1063/1.4928873

Marjanovic, N. (2015) Simulation of the Atmospheric Boundary Layer for Wind Energy Applications. Ph.D. Thesis, 
University of California, Berkeley, CA, USA.

Arthur, R.S., Mirocha, J.D., Marjanovic, N., Hirth, B.D., Schroeder, J.L., Wharton, S., and Chow, F.K. (2020) Multi-
scale simulation of wind farm performance during a frontal passage. Atmosphere, 11(3), 245. doi:10.3390/atmos11030245

---WRF-GAD files--------------------------------------------------------------------------------------------------------

WRF/Registry/registry.windturb          #Registry file for GAD code
WRF/dyn_em/module_first_rk_step_part2.F #Calls GAD driver
WRF/phys/module_wind_param_driver.F     #Driver of GAD code, called from dyn_em/module_first_rk_step_part2.F
WRF/phys/module_gad_*.F                 #Parameterization file for different turbines / turbine-specific functions
WRF/phys/module_gen_act_disk.F          #Primary GAD code, called from driver

---Notes on WRF-GAD usage-----------------------------------------------------------------------------------------------

Namelist variables (see WRF/Registry/registry.windturb):

wp_opt             = turns on GAD package if >0. Option 3 includes yawing, option 2 does not.
turbine_opt        = which turbine to simulate (default=15 for PSU 1.5MW, as in WRF/phys/module_gad_psu15.F)
n_turbines         = the number of turbines to be simulated.
n_timeseries       = the length of the time series used for averaging turbine inflow conditions.
n_tmeturb          = must be set equal to n_turbines*n_timeseries, 
                     used to index time series arrays v0t and d0t.
turb_loc_in_latlon = if T, turbine location in windturb_spec is specified in lat/lon. 
                     if F, turbine location in windturb_spec is specified in meters from the edge of the domain.
windturb_spec      = name of text file used to specify turbine parameters. 
                     data is given as follows, space delimited, one row per turbine:

X[m]/LON Y[m]/LAT HUB_HEIGHT[m] ROTOR_DIAMETER[m] BLADE_LENGTH[m] THETA_TURBINE[deg.] CUTIN_SPEED[m/s] CUTOUT_SPEED[m/s]

Notes: -ROTOR_DIAMETER includes hub
       -BLADE_LENGTH does not include hub
       -THETA_TURBINE is the initial direction the turbine is facing, measured counterclockwise from west-facing:
                     (0/360=west, 90=south, 180=east, 270=north) 
                     can also be thought of as the angle of the rotor plane w.r.t. the x-axis
       -Make sure the number of rows in the file equals n_turbines (no header or extra line at bottom)

------------------------------------------------------------------------------------------------------------------------

@@@@@@@@@@@
@ WRF-CPM @
@@@@@@@@@@@           

---Relevant citations for WRF-CPM---------------------------------------------------------------------------------------

---WRF-CPM files--------------------------------------------------------------------------------------------------------

---Notes on WRF-CPM usage-----------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
