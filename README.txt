@@@@@@@@@@@@@
@ WRFv4.1.5 @
@@@@@@@@@@@@@

including...
-Generalized Actuator Disk (GAD) wind turbine model
-Cell perturpation method (CPM) for downscaling turbulence in nested simulations
-Idealized surface layer parameterization

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

Y[m]/LAT X[m]/LON HUB_HEIGHT[m] ROTOR_DIAMETER[m] BLADE_LENGTH[m] THETA_TURBINE[deg.] CUTIN_SPEED[m/s] CUTOUT_SPEED[m/s]

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

Muñoz-Esparza, D., Kosović, B., Mirocha, J.D., and van Beeck, J. (2014) Bridging the Transition from Mesoscale to 
Microscale Turbulence in Numerical Weather Prediction Models. Boundary-Layer Meteorol, 153, 409–440. 
doi:10.1007/s10546-014-9956-9.

Muñoz-Esparza, D., Kosović, B., van Beeck, J., and Mirocha, J.D. (2015) A stochastic perturbation method to generate 
inflow turbulence in large-eddy simulation models: Application to neutrally stratified atmospheric boundary layers. 
Physics of Fluids, 27, 035102. doi:10.1063/1.4913572.

Muñoz-Esparza, D., Lundquist, J.K., Sauer, J.A., Kosović, B., and Linn, R.R. (2017) Coupled mesoscale-LES modeling of a
diurnal cycle during the CWEX-13 field campaign: From weather to boundary-layer eddies. Journal of Advances in Modeling 
Earth Systems, 9, 1572–1594. doi:10.1002/2017MS000960.

Mazzaro, L.J., Muñoz‐Esparza, D., Lundquist, J.K., and Linn, R.R. (2017) Nested mesoscale‐to‐LES modeling of the 
atmospheric boundary layer in the presence of under‐resolved convective structures. J. Adv. Model. Earth Syst., 9, 
1795–1810. doi:10.1002/2017MS000912.

---WRF-CPM files--------------------------------------------------------------------------------------------------------

WRF/Registry/registry.cell_pert         #Registry file for CPM code
WRF/dyn_em/start_em.F                   #CPM code for initial perturbations included in WRF/dyn_em/start_em.F
WRF/dyn_em/solve_em.F                   #CPM code included in WRF/dyn_em/solve_em.F

---Notes on WRF-CPM usage-----------------------------------------------------------------------------------------------


Namelist variables (see WRF/Registry/registry.cell_pert for complete set of variables):

initial_pert       = activates temperature perturbations near the boundaries for initial perturbation
amp_pert_initial   = amplitude of the initial temperature perturbation
pertz_gp_initial   = number of vertical grid points for initial temperature perturbation
cell_pert          = activates cell-based perturbation
cell_pert_amp      = maximum amplitude for the potential temperature perturbations
pert_timestep      = activates temperature perturbations at a given time step frequency
pert_nts           = number of time steps after which perturbations are seeded (1/f_p)
cell_zbottom       = z-grid point where the perturbation starts
cell_ztop          = z-grid point where the perturbations ends

Notes: -The amplitude of the potential temperature perturbations should be obtained for Ec = 0.16 
        (Muñoz-Esparza et al., 2015)
       -The time step for the perturbations should roughly satisfy /Gamma = 1 (Muñoz-Esparza et al., 2015)

------------------------------------------------------------------------------------------------------------------------

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@ WRF-Idealized surface layer parameterization @
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

---WRF-Idealized surface layer files------------------------------------------------------------------------------------

WRF/phys/module_sf_spec_ideal_tsk.F     #Module driving surface temperature from cooling rate or heatflux specified in namelist
WRF/Registry/registry.les               #Registry file containing variables for idealized surface layer
WRF/phys/module_surface_driver.F        #Idealized surface layer called from WRF/phys/module_surface_driver.F
