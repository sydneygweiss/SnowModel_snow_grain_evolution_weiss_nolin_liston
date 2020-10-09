c snowpack_code.f

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE SNOWPACK_CODE(nx,ny,Tair_grid,rh_grid,ro_nsnow,
     &  dt,swe_depth,Tsfc,snow_d,prec_grid,runoff,Qm,rain,
     &  sprec,iter,w_balance,sum_prec,sum_runoff,xro_snow,
     &  undef,ro_snow,ro_snow_grid,soft_snow_d,sum_sprec,
     &  snow_depth,windspd_grid,Qsi_grid,sum_Qcs,canopy_int,
     &  Qcs,vegtype,forest_LAI,albedo,glacier_melt,
     &  canopy_unload,sum_unload,sum_glacmelt,run_snowtran,
     &  swemelt,d_canopy_int,sum_d_canopy_int,snow_d_init,
     &  sfc_pressure,Qe,sfc_sublim_flag,sum_sfcsublim,
     &  sum_swemelt,corr_factor,icorr_factor_index,swesublim,
     &  swe_depth_old,canopy_int_old,JJ,max_layers,melt_flag,
     &  ro_snowmax,tsls_threshold,dz_snow_min,tslsnowfall,
     &  change_layer,dy_snow,swe_lyr,ro_layer,T_old,gamma,
     &  multilayer_snowpack,seaice_run,seaice_conc,
     &  fc_param,liqfrac,d_opt,ssa,grain_sz,dendricity,
     &  sphericity,history,
     &  microstructure_flag,b92_flag,c13_flag,f06_flag,
     &  t07_flag,zi,snow_age,viscosity,terrain_slope,dangl,
     &  snthrm_flag)

      implicit none

      include 'snowmodel.inc'

      integer nx,ny,iter,i,j,max_iter

      integer max_layers,multilayer_snowpack,k,n_tsteps_in_day,irec
      integer JJ(nx_max,ny_max)
      integer melt_flag(nx_max,ny_max,nz_max)

      real ro_snowmax,tsls_threshold,dz_snow_min,Cp_snow,
     &  fc_param
      real tslsnowfall(nx_max,ny_max)
      real change_layer(nx_max,ny_max)
      real dy_snow(nx_max,ny_max,nz_max)
      real swe_lyr(nx_max,ny_max,nz_max)
      real ro_layer(nx_max,ny_max,nz_max)
      real T_old(nx_max,ny_max,nz_max)
      real gamma(nx_max,ny_max,nz_max)

      real layerFlux(nx_max,ny_max,nz_max)
      real ml_ret(nx_max,ny_max,nz_max)
      real liqfracml(nx_max,ny_max,nz_max)
      real liqfrac(nx_max,ny_max,nx_max) 
      real icefracml(nx_max,ny_max,nz_max)
      
      !sw 
      real liqfrac_z(nz_max) 
      real dendricity(nx_max,ny_max,nz_max)
      real sphericity(nx_max,ny_max,nz_max)
      real d_opt(nx_max,ny_max,nz_max)
      real ssa(nx_max,ny_max,nz_max)
      real zi(nx_max,ny_max,nz_max)
      real snow_age(nx_max,ny_max,nz_max)
      real viscosity(nx_max,ny_max,nz_max)
      real history(nx_max,ny_max,nz_max)
      real grain_sz(nx_max,ny_max,nz_max)
      real dangl(nx_max,ny_max,nz_max)
      real dendricity_z(nz_max)
      real sphericity_z(nz_max)
      real d_opt_z(nz_max)
      real ssa_z(nz_max) 
      real temp_grad_z(nz_max)
      real temp_grad(nz_max)
      real zi_z(nz_max)
      real grain_sz_z(nz_max)
      real snow_age_z(nz_max)
      real viscosity_z(nz_max)
      real history_z(nz_max)
      real mass_of_layer(nz_max)
      real dangl_z(nz_max)
      real mass_accumulation(nz_max)
      real terrain_slope(nx_max,ny_max)
      integer microstructure_flag
      integer b92_flag
      integer c13_flag 
      integer t07_flag
      integer f06_flag 
      integer snthrm_flag
      integer melt_flag_z(nz_max)
      real dy_snow_z(nz_max)
      real swe_lyr_z(nz_max)
      real ro_layer_z(nz_max)
      real T_old_z(nz_max)
      real gamma_z(nz_max)
      real ml_ret_z(nz_max)

      real Tair_grid(nx_max,ny_max)
      real rh_grid(nx_max,ny_max)
      real prec_grid(nx_max,ny_max)
      real windspd_grid(nx_max,ny_max)
      real Qsi_grid(nx_max,ny_max)
      real vegtype(nx_max,ny_max)
      real albedo(nx_max,ny_max)
      real glacier_melt(nx_max,ny_max)
      real canopy_unload(nx_max,ny_max)
      real sum_unload(nx_max,ny_max)
      real sum_glacmelt(nx_max,ny_max)
      real sum_swemelt(nx_max,ny_max)
      real swemelt(nx_max,ny_max)
      real swesublim(nx_max,ny_max)
      real snow_d_init(nx_max,ny_max)
      real swe_depth_old(nx_max,ny_max)
      real canopy_int_old(nx_max,ny_max)
      real seaice_conc(nx_max,ny_max)

      real ro_nsnow(nx_max,ny_max),snow_d(nx_max,ny_max),
     &  runoff(nx_max,ny_max),rain(nx_max,ny_max),
     &  sprec(nx_max,ny_max),w_balance(nx_max,ny_max),
     &  sum_prec(nx_max,ny_max),sum_runoff(nx_max,ny_max),
     &  xro_snow(nx_max,ny_max),sfc_pressure(nx_max,ny_max),
     &  ro_snow_grid(nx_max,ny_max),swe_depth(nx_max,ny_max),
     &  Tsfc(nx_max,ny_max),Qm(nx_max,ny_max),
     &  soft_snow_d(nx_max,ny_max),sum_sprec(nx_max,ny_max),
     &  ro_snow,snow_depth(nx_max,ny_max),sum_Qcs(nx_max,ny_max),
     &  canopy_int(nx_max,ny_max),Qcs(nx_max,ny_max),
     &  d_canopy_int(nx_max,ny_max),sum_d_canopy_int(nx_max,ny_max),
     &  Qe(nx_max,ny_max),sum_sfcsublim(nx_max,ny_max)

      real dt,undef,Cp,xLf,Tf,A1,A2,ro_water,xLs,ro_ice,Twb,
     &  run_snowtran,sfc_sublim_flag,seaice_run,Cp_water

      real corr_factor(nx_max,ny_max,max_obs_dates+1)
      integer icorr_factor_index(max_time_steps)

      integer nftypes
      parameter (nftypes=5)
      real forest_LAI(nftypes)

      print *,'   solving the snow-cover evolution'
      if (iter .eq. 1) then 
        open(172, file = 'ssa_A.txt')
        open(173, file = 'ssa_B.txt')
        open(174, file = 'ssa_C.txt')
        open(175, file = 'ssa_A2.txt')
        open(176, file = 'ssa_B2.txt')
        open(177, file = 'ssa_C2.txt')
        open(178, file = 'zfact1.txt')
        open(179, file = 'zfact2.txt')
        open(180, file = 'zdenom1.txt')
        open(181, file = 'zdenom2.txt')
        open(182, file = 'ssa_t_placeholder.txt') 
        open(183, file = 'snow_age.txt')
        open(184, file ='tg.txt') 
        open(185, file = 'T_old.txt') 
        open(186, file = 'ssa_dt.txt') 
        open(187, file = 'ssa.txt') 
        open(213, file = 'liqfac.txt') 
      endif 
     
      
    
c Define the constants used in the computations.
      CALL CONSTS_SNOWPACK(Cp,xLs,ro_ice,xLf,Tf,A1,A2,ro_water,
     &  Cp_snow,ro_snowmax,Cp_water)

c Run the snowpack evolution sub-model.
      do j=1,ny
        do i=1,nx

c Extract the vertical column for this i,j point, and send it
c   to the subroutine. *** Note that I should use f95, then I would
c   not have to do this (I could pass in subsections of the arrays).
          if (multilayer_snowpack.eq.1) then
            do k=1,nz_max
              melt_flag_z(k) = melt_flag(i,j,k)
              dy_snow_z(k) = dy_snow(i,j,k)
              swe_lyr_z(k) = swe_lyr(i,j,k)
              ro_layer_z(k) = ro_layer(i,j,k)
              T_old_z(k) = T_old(i,j,k)
              gamma_z(k) = gamma(i,j,k)

c J.PFLUG
c variables for multilayer percolation investigation
c              layerFlux_z(k) = layerFlux(i,j,k)
              ml_ret_z(k) = ml_ret(i,j,k)
c              liqfrac_z(k) = liqfracml(i,j,k)
              liqfrac_z(k) = liqfrac(i,j,k) 
c              icefrac_z(k) = icefracml(i,j,k)
c END J.PFLUG

              if (microstructure_flag .eq. 1) then
                dendricity_z(k) =  dendricity(i,j,k)
                sphericity_z(k) = sphericity(i,j,k)
                d_opt_z(k) = d_opt(i,j,k)
                ssa_z(k) = ssa(i,j,k)
                grain_sz_z(k) = grain_sz(i,j,k)
                history_z(k) = history(i,j,k)
                viscosity_z(k) = viscosity(i,j,k)
                snow_age_z(k) =  snow_age(i,j,k)
                zi_z(k) = zi(i,j,k)
                dangl_z(k) = dangl(i,j,k)
                if (k .eq. 1) then 
                  temp_grad(k) = T_old_z(k) - Tf - 1 
                   !assumption that ground is -1 degrees 
                else 
                   temp_grad(k) = T_old_z(k-1) - T_old_z(k) 
                endif 
                if (temp_grad(k) .lt. -100) then 
                  temp_grad(k) = 0.0 
                endif 
                !print*, temp_grad(k) 
               !if (k .eq. 1) then 
                  !temp_grad_z(k) = T_old_z(k) - Tf - 1 
                  !assumption that ground is -1 degrees 
               ! else 
                  !temp_grad_z(k) = T_old_z(k-1) - T_old_z(k) 
               !  endif 
               !  temp_grad_z(k) = temp_grad(i,j,k) 
                mass_of_layer(k) = ro_layer_z(k) * dy_snow_z(k) 
                if(k .eq. 1) then 
                  mass_accumulation(k) = 0 
                else 
                  mass_accumulation(k) = mass_of_layer(k-1) + 
     &             mass_of_layer(k) 
                endif
              endif 
            enddo
          endif

          CALL SNOWPACK_CORE(Twb,Tf,Tair_grid(i,j),rh_grid(i,j),xLs,
     &      Cp,sfc_pressure(i,j),ro_nsnow(i,j),dt,ro_snow,
     &      swe_depth(i,j),Tsfc(i,j),A1,A2,snow_d(i,j),ro_water,
     &      ro_ice,prec_grid(i,j),runoff(i,j),Qm(i,j),xLf,rain(i,j),
     &      sprec(i,j),iter,w_balance(i,j),sum_prec(i,j),
     &      sum_runoff(i,j),xro_snow(i,j),undef,
     &      soft_snow_d(i,j),sum_sprec(i,j),ro_snow_grid(i,j),
     &      snow_depth(i,j),windspd_grid(i,j),Qsi_grid(i,j),
     &      sum_Qcs(i,j),canopy_int(i,j),Qcs(i,j),vegtype(i,j),
     &      forest_LAI,albedo(i,j),canopy_unload(i,j),
     &      sum_unload(i,j),sum_glacmelt(i,j),run_snowtran,
     &      swemelt(i,j),d_canopy_int(i,j),sum_d_canopy_int(i,j),
     &      snow_d_init(i,j),Qe(i,j),glacier_melt(i,j),
     &      sfc_sublim_flag,sum_sfcsublim(i,j),sum_swemelt(i,j),
     &      corr_factor(i,j,-icorr_factor_index(iter)),
     &      icorr_factor_index(iter),swesublim(i,j),
     &      swe_depth_old(i,j),canopy_int_old(i,j),JJ(i,j),
     &      max_layers,melt_flag_z,ro_snowmax,tsls_threshold,
     &      dz_snow_min,tslsnowfall(i,j),change_layer(i,j),dy_snow_z,
     &      swe_lyr_z,ro_layer_z,T_old_z,gamma_z,multilayer_snowpack,
     &      Cp_snow,seaice_run,fc_param,Cp_water,ml_ret_z,liqfrac_z,
!     &      temp_grad_z,d_opt_z,ssa_z,grain_sz_z,

     &      temp_grad,d_opt_z,ssa_z,grain_sz_z,
     &      dendricity_z,sphericity_z,microstructure_flag,b92_flag,
     &      c13_flag,t07_flag,f06_flag,terrain_slope(i,j),
     &      zi_z,history_z,
     &      viscosity_z,snow_age_z,mass_accumulation,dangl_z,
     &      snthrm_flag)

c Re-build the 3-D arrays.  See note above about using f95 to avoid this.
          if (multilayer_snowpack.eq.1) then
            do k=1,nz_max
              melt_flag(i,j,k) = melt_flag_z(k)
              dy_snow(i,j,k) = dy_snow_z(k)
              swe_lyr(i,j,k) = swe_lyr_z(k)
              ro_layer(i,j,k) = ro_layer_z(k)
              T_old(i,j,k) = T_old_z(k)
              gamma(i,j,k) = gamma_z(k)
c              layerFlux(i,j,k) = layerFlux_z(k)
              ml_ret(i,j,k) = ml_ret_z(k)
c              liqfracml(i,j,k) = liqfrac_z(k)
              liqfrac(i,j,k) = liqfrac_z(k) 
c              icefracml(i,j,k) = icefrac_z(k)
              if (microstructure_flag .eq. 1) then
                dendricity(i,j,k) =  dendricity_z(k)
                sphericity(i,j,k) = sphericity_z(k)
                d_opt(i,j,k) = d_opt_z(k) 
                ssa(i,j,k) = ssa_z(k)
                grain_sz(i,j,k) = grain_sz_z(k)
                history(i,j,k) = history_z(k)  
                viscosity(i,j,k) = viscosity_z(k) 
                snow_age(i,j,k) =  snow_age_z(k)
                zi(i,j,k) = zi_z(k)
                dangl(i,j,k) = dangl_z(k)
                !temp_grad(i,j,k) = temp_grad_z(k) 
              endif 
            enddo
          endif

        enddo
      enddo

      if (run_snowtran.eq.0.0) then
        do j=1,ny
          do i=1,nx
          swe_depth_old(i,j) = swe_depth(i,j)
          canopy_int_old(i,j) = canopy_int(i,j)
          enddo
        enddo
      endif

c Read in the sea ice concentration.  These are daily data, so
c   first calculate which record in the data file this time step
c   corresponds to.
      if (seaice_run.ne.0.0) then
        n_tsteps_in_day = nint(86400.0 / dt)
        if (mod(iter-1,n_tsteps_in_day).eq.0) then
          irec = int((real(iter) - 0.5) * dt / 86400.0) + 1
          print *,'sea ice irec =',irec
          read (445,rec=irec) ((seaice_conc(i,j),i=1,nx),j=1,ny)
        endif
      endif

c If this simulation is not running SnowTran-3D, then zero out
c   the ocean grid cells that have no sea ice here.  If it is
c   running with SnowTran-3D, then do this in the SnowTran-3D
c   subroutine.
      if (run_snowtran.eq.0.0) then
        if (seaice_run.ne.0.0) then
          CALL ZERO_SEAICE_SNOW(nx,ny,snow_depth,ro_snow_grid,
     &      ro_snow,swe_depth,swe_depth_old,canopy_int_old,JJ,
     &      tslsnowfall,dy_snow,swe_lyr,ro_layer,T_old,
     &      multilayer_snowpack,tsls_threshold,seaice_conc)
        endif
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE SNOWPACK_CORE(Twb,Tf,Tair,rh,xLs,
     &  Cp,sfc_pressure,ro_nsnow,dt,ro_snow,
     &  swe_depth,Tsfc,A1,A2,snow_d,ro_water,
     &  ro_ice,prec,runoff,Qm,xLf,rain,
     &  sprec,iter,w_balance,sum_prec,
     &  sum_runoff,xro_snow,undef,
     &  soft_snow_d,sum_sprec,ro_snow_grid,
     &  snow_depth,windspd,Qsi,
     &  sum_Qcs,canopy_int,Qcs,vegtype,
     &  forest_LAI,albedo,canopy_unload,
     &  sum_unload,sum_glacmelt,run_snowtran,
     &  swemelt,d_canopy_int,sum_d_canopy_int,
     &  snow_d_init,Qe,glacier_melt,
     &  sfc_sublim_flag,sum_sfcsublim,sum_swemelt,
     &  corr_factor,
     &  icorr_factor_index,swesublim,
     &  swe_depth_old,canopy_int_old,JJ,
     &  max_layers,melt_flag,ro_snowmax,tsls_threshold,
     &  dz_snow_min,tslsnowfall,change_layer,dy_snow,
     &  swe_lyr,ro_layer,T_old,gamma,multilayer_snowpack,
     &  Cp_snow,seaice_run,fc_param,Cp_water,ml_ret,liqfrac,
     &  temp_grad,d_opt,ssa,grain_sz,
     &  dendricity,sphericity,microstructure_flag,b92_flag,
     &  c13_flag,t07_flag,f06_flag,terrain_slope,zi,history,
     &  viscosity,snow_age,mass_accumulation,dangl,snthrm_flag)

      implicit none

      include 'snowmodel.inc'

      integer iter,icorr_factor_index

      integer JJ,max_layers,multilayer_snowpack
      
      real ro_snowmax,tsls_threshold,dz_snow_min,tslsnowfall,Cp_snow,
     &  Cp_water

      integer melt_flag(nz_max)
      real change_layer
      real dy_snow(nz_max)
      real swe_lyr(nz_max)
      real ro_layer(nz_max)
      real T_old(nz_max)
      real gamma(nz_max)
      real ml_ret(nz_max)
      !sw 
      real liqfrac(nz_max) 
      real dendricity(nz_max)
      real sphericity(nz_max)
      real d_opt(nz_max)
      real grain_sz(nz_max)
      real ssa(nz_max) 
      real temp_grad(nz_max)
      real mass_accumulation(nz_max)
      real terrain_slope
      real dangl(nz_max)
      integer microstructure_flag
      integer b92_flag
      integer c13_flag 
      integer t07_flag
      integer f06_flag 
      integer snthrm_flag
      real history(nz_max)
      real zi(nz_max)
      real snow_age(nz_max)
      real viscosity(nz_max)
      !sw 
      real dendricity_n,sphericity_n,d_opt_n,
     & ssa_n,grain_sz_n,viscosity_n,zi_n,snow_age_n,history_n,
     & dangl_n 

      real Twb,Tf,Tair,rh,xLs,Cp,ro_nsnow,dt,ro_snow,swe_depth,
     &  Tsfc,A1,A2,snow_d,ro_water,ro_ice,prec,runoff,Qm,xLf,rain,
     &  sprec,w_balance,sum_prec,sum_runoff,xro_snow,undef,
     &  soft_snow_d,sum_sprec,ro_snow_grid,snow_depth,sprec_grnd,
     &  windspd,Qsi,sum_Qcs,canopy_int,Qcs,canopy_unload,
     &  vegtype,albedo,glacier_melt,sum_unload,sum_glacmelt,
     &  run_snowtran,swemelt,d_canopy_int,sfc_pressure,
     &  sum_d_canopy_int,snow_d_init,Qe,sfc_sublim_flag,
     &  sum_sfcsublim,sum_swemelt,corr_factor,swesublim,
     &  swe_depth_old,canopy_int_old,sprec_grnd_ml,seaice_run,
     &  mLayerVolFracLiqTrial,fc_param

      integer nftypes
      parameter (nftypes=5)
      real forest_LAI(nftypes)

c Calculate the canopy sublimation, loading and unloading.  Note
c   that here I have assumed that evergreen trees are type 1.
      if (vegtype.le.5.0) then
        CALL CANOPY_SNOW(rh,Tair,windspd,Qsi,sum_Qcs,albedo,
     &    canopy_int,sprec,Qcs,dt,canopy_unload,
     &    forest_LAI(nint(vegtype)),sum_unload,d_canopy_int,
     &    sum_d_canopy_int)
        sprec_grnd = sprec + canopy_unload - d_canopy_int
        sprec_grnd_ml = sprec - d_canopy_int
      else
        Qcs = 0.0
        sprec_grnd = sprec
        sprec_grnd_ml = sprec
      endif

c Solve for the wet bulb temperature.
      CALL SOLVEWB(Twb,Tf,Tair,rh,xLs,Cp,sfc_pressure)

c Compute the new snow density.
      CALL NSNOWDEN(ro_nsnow,Twb,Tf,dt)
      
      if (microstructure_flag .eq. 1) then 
        CALL NSNOWMICRO(windspd,dendricity_n,sphericity_n,d_opt_n,
     &  ssa_n,grain_sz_n,viscosity_n,zi_n,snow_age_n,history_n,
     &  ro_nsnow,Tsfc,b92_flag,c13_flag,snthrm_flag,dangl_n)
      endif 
      
      
     
c Call the multi-layer snowpack model.
      if (multilayer_snowpack.eq.1) then

        CALL MULTI_LAYER_SNOW(JJ,ro_layer,Tf,dt,ro_water,
     &    ro_ice,T_old,dy_snow,swe_lyr,Qm,ro_snowmax,rain,
     &    xLf,Cp_snow,melt_flag,runoff,tslsnowfall,ro_nsnow,
     &    sprec,Tsfc,tsls_threshold,gamma,max_layers,change_layer,
     &    dz_snow_min,snow_depth,swe_depth,undef,canopy_unload,
     &    vegtype,glacier_melt,sum_glacmelt,sum_swemelt,snow_d,
     &    Qe,sfc_sublim_flag,sum_sfcsublim,soft_snow_d,ro_snow,
     &    sum_sprec,sprec_grnd_ml,sum_prec,prec,sum_runoff,
     &    ro_snow_grid,xro_snow,swesublim,A1,A2,
     &    fc_param,Cp_water,ml_ret,icorr_factor_index,corr_factor,
     &    liqfrac,microstructure_flag,
     &    windspd,temp_grad,d_opt,grain_sz,ssa,dendricity,
     &    sphericity,t07_flag,f06_flag,b92_flag,c13_flag,zi,viscosity,
     &    snow_age,history,terrain_slope,dendricity_n,
     &    sphericity_n,d_opt_n,
     &    ssa_n,grain_sz_n,viscosity_n,zi_n,snow_age_n,history_n,
     &    mass_accumulation,dangl_n,dangl,iter,snthrm_flag)

c Call the original single-layer snowpack model.
      else

c Compute the snow density change due to settling.
        CALL DDENSITY(ro_snow_grid,swe_depth,Tf,Tsfc,dt,A1,A2,
     &    snow_depth,ro_water,ro_ice)

c Compute the melt, rain, and snow contributions to modifying
c   the snowpack depth, density, and snow water equivalent.
        CALL SNOWPACK(swe_depth,snow_d,ro_snow_grid,
     &    prec,ro_water,ro_nsnow,runoff,Qm,xLf,dt,rain,sprec,
     &    sum_prec,sum_runoff,soft_snow_d,sum_sprec,ro_snow,
     &    snow_depth,sprec_grnd,vegtype,glacier_melt,sum_glacmelt,
     &    swemelt,canopy_unload,Qe,sfc_sublim_flag,sum_sfcsublim,
     &    sum_swemelt,corr_factor,icorr_factor_index,swesublim,
     &    ro_snowmax)

c Post process the data for output.
        CALL POSTPROC(ro_snow_grid,xro_snow,snow_depth,undef)
      endif

c Perform a water balance check (see notes in this subroutine).
      if (seaice_run.eq.0.0) then
        if (run_snowtran.eq.0.0) then
          CALL WATERBAL_SNOWPACK(w_balance,prec,Qcs,runoff,
     &    d_canopy_int,swe_depth,glacier_melt,swe_depth_old,iter,
     &    swesublim,canopy_unload,canopy_int_old,canopy_int)

c         CALL WATERBAL_SNOWPACK_sums(w_balance,sum_prec,sum_Qcs,
c    &      sum_runoff,canopy_int,swe_depth,sum_glacmelt,iter,
c    &      snow_d_init,ro_snow,ro_water,sum_sfcsublim)
        endif
      endif
      
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE CANOPY_SNOW(rh,Tair,windspd,Qsi,sum_Qcs,albedo,
     &  canopy_int,sprec,Qcs,dt,canopy_unload,
     &  forest_LAI,sum_unload,d_canopy_int,
     &  sum_d_canopy_int)

      implicit none

      real rh,Tair,windspd,V_s,Qsi,forest_LAI,dt,xImax,canopy_int,
     &  d_canopy_int,Qcs,Ce,sprec,C_0,unload_melt,canopy_unload,
     &  sum_Qcs,albedo,sum_unload,sum_d_canopy_int

c Note that all of this must deal with the (kg/m2)=(mm), => (m)
c   issues.  Precip is in (m), all of these equations are in
c   (kg/m2), and I want the outputs to be in (m).

c Compute the sublimation loss rate coefficient for canopy snow.
      CALL SUBLIM_COEF(rh,Tair,windspd,V_s,Qsi,albedo)

c Maximum interception storage.
      xImax = 4.4 * forest_LAI

c Change in canopy load due to snow precipitation during this time
c   step.  Convert the canopy interception to mm.
      canopy_int = 1000.0 * canopy_int
      d_canopy_int = 0.7 * (xImax - canopy_int) *
     &  ( 1.0 - exp((- sprec)*1000.0/xImax))

c Update the interception load.
      canopy_int = canopy_int + d_canopy_int

c Canopy exposure coefficient.
      if (canopy_int.eq.0.0) then
        Ce = 0.0
      else
c Pomeroy's k_c value
c       Ce = 0.0114 * (canopy_int/xImax)**(-0.4)
c My k_c value.
        Ce = 0.00995 * (canopy_int/xImax)**(-0.4)
      endif

c Canopy sublimation (kg/m2), (a negative mumber).  Make sure that
c   you don't sublimate more than is available.
      Qcs = Ce * canopy_int * V_s * dt
      Qcs = -min(canopy_int,-Qcs)

c Remove the sublimated moisture from the canopy store.
      canopy_int = canopy_int + Qcs
c Save the sublimation in (m).
      Qcs = Qcs / 1000.0
      sum_Qcs = sum_Qcs + Qcs

c Perform a second unloading due to melt.  Assume an unloading rate
c   of 5.0 mm/day/C.
      C_0 = 5.0 / 86400.0
      unload_melt = C_0 * max(0.0,Tair-273.16) * dt
      unload_melt = min(canopy_int,unload_melt)
      canopy_int = canopy_int - unload_melt

c Keep track of the unloaded snow that reached the ground during
c   this time step (m) (this will add to the snow depth).
      canopy_unload = unload_melt / 1000.0
      d_canopy_int = d_canopy_int / 1000.0

c Save a summing array of this unloaded snow.
      sum_unload = sum_unload + canopy_unload

c Save a summing array of the change in canopy load.
      sum_d_canopy_int = sum_d_canopy_int + d_canopy_int

c Save the interception load for the next time step.  Convert to m.
      canopy_int = canopy_int / 1000.0

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE SUBLIM_COEF(rh,Tair,windspd,V_s,Qsi,albedo)

c Compute the sublimation loss rate coefficient for canopy snow.

      implicit none

      real pi,ro_ice,xM,R,R_dryair,vonKarman,visc_air,h_s,xlamdaT,
     &  D,ro_sat,sigma,V_s,radius,xmass,windspd,rh,Tair,Qsi,Sp,
     &  xN_r,xNu,xSh,top,bottom,omega,albedo

c Constants.
      pi = 2.0 * acos(0.0)
      ro_ice = 917.0
      xM = 18.01
      R = 8313.
      R_dryair = 287.
      vonKarman = 0.4
      visc_air = 13.e-6
      h_s = 2.838e6
      xlamdaT = 0.024

c Particle radius.
      radius = 5.0e-4

c Particle mass.
      xmass = 4.0/3.0 * pi * ro_ice * radius**3

c Diffusivity of water vapor in the atmosphere.
      D = 2.06e-5 * (Tair/273.)**(1.75)

c Saturation density of water vapor.
      ro_sat = 0.622 / (R_dryair * Tair) *
     &  611.15 * exp(22.452 * (Tair - 273.16) / (Tair - 0.61))

c Humidity deficit.
      sigma = 0.01 * rh - 1.0
      sigma = min(0.0,sigma)
      sigma = max(-1.0,sigma)

c Reynolds, Nusselt, and Sherwood numbers.
      xN_r = 2.0 * radius * windspd / visc_air
      xNu = 1.79 + 0.606 * xN_r**(0.5)
      xSh = xNu

c Solar radiation absorbed by the snow particle.  Here assume that
c   the general snow albedo is the same as the snow particle albedo.
      Sp = pi * radius**2 * (1.0 - albedo) * Qsi

c Sublimation-loss rate coefficient for an ice sphere.
      omega = ((h_s * xM)/(R * Tair) - 1.0) / (xlamdaT * Tair * xNu)
      top = 2.0 * pi * radius * sigma - Sp * omega
      bottom = h_s * omega + 1.0/(D * ro_sat * xSh)
      V_s = (top/bottom)/xmass

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE WATERBAL_SNOWPACK(w_balance,prec,Qcs,runoff,
     &  d_canopy_int,swe_depth,glacier_melt,swe_depth_old,iter,
     &  swesublim,canopy_unload,canopy_int_old,canopy_int)

      implicit none

      integer iter

      real w_balance,prec,Qcs,runoff,d_canopy_int,swe_depth_old,
     &  swe_depth,glacier_melt,swesublim,canopy_unload,canopy_int_old,
     &  canopy_int

c Note that the following balances should hold.  These aren't quite
c   right, but it is a place to start.
c   Canopy Balance (forest):
c     canopy = sprec - unload + Qcs ==> unload = sprec - canopy + Qcs
c
c   Snowpack Balance (forest):
c     swe_d = unload + rain - runoff ==>
c       canopy + swe_d = sprec + rain + Qcs - runoff
c     prec = sprec + rain
c     sum_rain  = sum_sprec - sum_prec
c
c   Snowpack Balance (non-forest):
c     swe_d = sprec + rain - runoff + subl + salt + susp + subgrid +
c       glaciermelt
c
c   Everywhere:
c     w_balance = sum_prec + sum_Qcs - sum_runoff + sum_subl +
c       sum_trans - canopy_int - swe_depth + sum_glacmelt
c
c   The related variables that would need to be brought in are:
c      d_canopy_int,sum_d_canopy_int,sum_unload

c This subroutine is called for the case where SnowTran-3D is not
c   run.  The subroutine WATERBAL_SNOWTRAN is used if the model
c   simulation includes SnowTran-3D.
c     w_balance = swe_depth_old - swe_depth + prec - runoff +
c    &  glacier_melt - swesublim + canopy_int_old - canopy_int -
c    &  d_canopy_int + Qcs + canopy_unload

c Do the snowpack.
c     w_balance = swe_depth_old - swe_depth + prec - runoff -
c    &  glacier_melt - swesublim

c Do the canopy.
c     w_balance = canopy_int_old - canopy_int + d_canopy_int +
c    &  Qcs - canopy_unload

c Do the snowpack and canopy store.
      w_balance = swe_depth_old - swe_depth + prec - runoff +
     &  glacier_melt - swesublim + canopy_int_old - canopy_int +
     &  Qcs

      if (abs(w_balance).gt.1.0e-5) then
        print*,'water imbalance found, iter =',iter,' ',w_balance
        print*,swe_depth_old,swe_depth,prec,runoff,glacier_melt, 
     &    swesublim,canopy_int_old,canopy_int,Qcs
      endif
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE WATERBAL_SNOWPACK_sums(w_balance,sum_prec,sum_Qcs,
     &  sum_runoff,canopy_int,swe_depth,sum_glacmelt,iter,
     &  snow_d_init,ro_snow,ro_water,sum_sfcsublim)

      implicit none

      integer iter

      real w_balance,sum_prec,sum_Qcs,sum_runoff,canopy_int,
     &  swe_depth,sum_glacmelt,snow_d_init,ro_snow,ro_water,
     &  sum_sfcsublim

c Note that the following balances should hold.  These aren't quite
c   right, but it is a place to start.
c   Canopy Balance (forest):
c     canopy = sprec - unload + Qcs ==> unload = sprec - canopy + Qcs
c
c   Snowpack Balance (forest):
c     swe_d = unload + rain - runoff ==>
c       canopy + swe_d = sprec + rain + Qcs - runoff
c     prec = sprec + rain
c     sum_rain  = sum_sprec - sum_prec
c
c   Snowpack Balance (non-forest):
c     swe_d = sprec + rain - runoff + subl + salt + susp + subgrid +
c       glaciermelt
c
c   Everywhere:
c     w_balance = sum_prec + sum_Qcs - sum_runoff + sum_subl +
c       sum_trans - canopy_int - swe_depth + sum_glacmelt
c
c   The related variables that would need to be brought in are:
c      d_canopy_int,sum_d_canopy_int,sum_unload

c This subroutine is called for the case where SnowTran-3D is not
c   run.  The subroutine WATERBAL_SNOWTRAN is used if the model
c   simulation includes SnowTran-3D.
      w_balance = sum_prec + sum_Qcs - sum_runoff - canopy_int -
     &  swe_depth + sum_glacmelt + snow_d_init * ro_snow/ro_water -
     &  sum_sfcsublim

      if (abs(w_balance).gt.1.0e-4)
     &  print*,'water imbalance found, iter =',iter,' ',w_balance

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc;cccccc

      SUBROUTINE SNOWPACK(swe_depth,snow_d,ro_snow_grid,
     &  prec,ro_water,ro_nsnow,runoff,Qm,xLf,dt,rain,sprec,
     &  sum_prec,sum_runoff,soft_snow_d,sum_sprec,ro_snow,
     &  snow_depth,sprec_grnd,vegtype,glacier_melt,sum_glacmelt,
     &  swemelt,canopy_unload,Qe,sfc_sublim_flag,sum_sfcsublim,
     &  sum_swemelt,corr_factor,icorr_factor_index,swesublim,
     &  ro_snowmax)

      implicit none

      real ro_snowmax,runoff,Qm,swe_depth,potmelt,swemelt,dt,
     &  ro_water,xLf,snow_depth,ro_snow_grid,snow_d_melt,dz_water,
     &  soft_snow_d,prec,rain,snow_d,sum_sprec,sum_prec,
     &  sum_runoff,ro_nsnow,sprec,ro_snow,snow_d_new,sprec_grnd,
     &  vegtype,glacier_melt,sum_glacmelt,canopy_unload,Qe,
     &  xLsublim,potsublim,swesublim,snow_d_sublim,sfc_sublim_flag,
     &  sum_sfcsublim,sum_swemelt,corr_factor,potmelt_tmp
      integer icorr_factor_index

      runoff = 0.0

c SURFACE SUBLIMATION.

c Whether static-surface (non-blowing snow) sublimation is included
c   in the model calculations is controlled by the sfc_sublim_flag.
c   I am waiting for the flux-tower data Matthew and I are collecting
c   in Alaska, to compare with the model simulations, before
c   including this part of the model in all simulations.

c If the sfc_sublim_flag is turned on, the latent heat flux (Qe)
c   calculated in ENBAL is used to add/remove snow from the snowpack.
c   xLsublim = xLf + xLv = 2.5104x10^6 J/kg + 3.334x10^5 J/kg, and
c   potsublim is in m swe.

      if (swe_depth.gt.0.0  .and.  sfc_sublim_flag.eq.1.0) then
        if (Qe.lt.0.0) then

c Compute the snow-surface sublimation (m, swe).
          xLsublim = 2.844e6
          potsublim = (- dt) * Qe / (ro_water * xLsublim)
          swesublim = min(potsublim,swe_depth)

c Save a summing array of the static surface snow sublimation.
          sum_sfcsublim = sum_sfcsublim + swesublim

c Compute the change in snow depth.  Assume that this sublimated
c   snow does not change the snow density and does not change the
c   soft snow depth.  It only reduces the snow depth and the
c   associated swe depth.
          swe_depth = swe_depth - swesublim
          if (swe_depth.eq.0.0) then
            snow_depth = 0.0
          else
            snow_d_sublim = swesublim * ro_water / ro_snow_grid
            snow_depth = snow_depth - snow_d_sublim
          endif
        else
          swesublim = 0.0
        endif
      else
        swesublim = 0.0
      endif

c MELTING.

c If melting occurs, decrease the snow depth, and place the melt
c   water in the 'runoff' variable.  Keep track of the liquid water
c   produced.

      if (Qm.gt.0.0) then

c Compute the snow melt (m).
        potmelt = dt * Qm / (ro_water * xLf)

c Account for any snowmelt data assimilation.
        if (icorr_factor_index.lt.0.0) then
          potmelt_tmp = potmelt * corr_factor
          swemelt = min(potmelt_tmp,swe_depth)
c Handle the case of no snowmelt data assimilation.
        else
          swemelt = min(potmelt,swe_depth)
        endif

c Compute any glacier or permanent snow-field melt (m water equiv.).
        if (vegtype.eq.20.0) then
          glacier_melt = potmelt - swemelt
        else
          glacier_melt = 0.0
        endif

c Save a summing array of the glacier melt.
        sum_glacmelt = sum_glacmelt + glacier_melt

c Save the runoff contribution.
        runoff = runoff + glacier_melt

c Save a summing array of the snow melt.
        sum_swemelt = sum_swemelt + swemelt

c Compute the change in snow depth.
        snow_d_melt = swemelt * ro_water / ro_snow_grid
        snow_depth = snow_depth - snow_d_melt
        snow_depth = max(0.0,snow_depth)

c Compute the changes in snow density resulting from the melt.
c   Assume that the melted snow is redistributed through the new
c   snow depth up to a maximum density.  Any additional melt water
c   is added to the runoff.
        if (snow_depth.eq.0.0) then
          ro_snow_grid = ro_snowmax
          runoff = runoff + swemelt
        else
          ro_snow_grid = swe_depth * ro_water / snow_depth
        endif

        if (ro_snow_grid.gt.ro_snowmax) then
          dz_water = snow_depth *
     &      (ro_snow_grid - ro_snowmax) / ro_water
          ro_snow_grid = ro_snowmax
          swe_depth = snow_depth * ro_snow_grid / ro_water
          runoff = runoff + dz_water
        else
          swe_depth = snow_depth * ro_snow_grid / ro_water
        endif

        soft_snow_d = 0.0

      else

c These prevent values from the previous time step from being
c   carried through to the next time step.
        swemelt = 0.0
        glacier_melt = 0.0

      endif

c PRECIPITATION.

c Precipitation falling as rain on snow contributes to a snow
c   density increase, precipitation falling as snow adds to the
c   snow depth, and rain falling on bare ground contributes to the
c   runoff.

c We have precipitation.
      if (prec.gt.0.0) then
        rain = prec - sprec

c We have rain.
        if (rain.gt.0.0) then

c Rain on snow.  Note that we can also have snow unloading here.
c   Assume this unloading is wet as rain.
          if (snow_depth.gt.0.0) then
            swe_depth = swe_depth + rain + canopy_unload
            ro_snow_grid = swe_depth * ro_water / snow_depth
            if (ro_snow_grid.gt.ro_snowmax) then
              dz_water = snow_depth * (ro_snow_grid - ro_snowmax) /
     &          ro_water
              ro_snow_grid = ro_snowmax
              swe_depth = snow_depth * ro_snow_grid / ro_water
              runoff = runoff + dz_water
            endif

c Rain on bare ground.  Assume any unloading is as wet as rain.
          else
            runoff = runoff + rain + canopy_unload
          endif

c We have snow precipitation (on either snow or bare ground).
        else
          swe_depth = swe_depth + sprec_grnd
          snow_d_new = ro_water / ro_nsnow * sprec_grnd
          snow_depth = snow_depth + snow_d_new
          ro_snow_grid = ro_water * swe_depth / snow_depth
        endif

c Here we handle the case where there is no precipitation, but
c   there is snow falling from the canopy to the snowpack.
      else
        rain = 0.0
        if (sprec_grnd.gt.0.0) then
          swe_depth = swe_depth + sprec_grnd
          snow_d_new = ro_water / ro_snow * sprec_grnd
          snow_depth = snow_depth + snow_d_new
          ro_snow_grid = ro_water * swe_depth / snow_depth
        endif
      endif

c The following are set up to be compatible with SnowTran-3D, and
c   are in snow-depth units.  The sum_sprec corrections are done
c   in the SnowTran-3D code.
      soft_snow_d = soft_snow_d + sprec_grnd * ro_water / ro_snow
      snow_d = swe_depth * ro_water / ro_snow
c     sum_sprec = sum_sprec + sprec_grnd * ro_water / ro_snow
      sum_sprec = sum_sprec + sprec_grnd

c The following are in swe-depth units.
      sum_prec = sum_prec + prec
      sum_runoff = sum_runoff + runoff

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE DDENSITY(ro_snow_grid,swe_depth,Tf,Tsfc,dt,A1,A2,
     &  snow_depth,ro_water,ro_ice)

      implicit none

      real snow_depth,Tsg,Tf,Tsnow,Tsfc,ro_snow_grid,dt,A1,A2,
     &  swe_depth_star,ro_ice,ro_water,swe_depth

      if (snow_depth.gt.0.0) then

c Assume that the snow-ground interface temperature is -1.0 C.
        Tsg = Tf - 1.0
        Tsnow = 0.5 * (Tsg + Tsfc)
        swe_depth_star= 0.5 * swe_depth
        ro_snow_grid = ro_snow_grid + dt *
     &    (A1 * swe_depth_star * ro_snow_grid *
     &    exp((- 0.08)*(Tf-Tsnow)) * exp((- A2)*ro_snow_grid))
        ro_snow_grid = min(ro_ice,ro_snow_grid)
        snow_depth = ro_water * swe_depth / ro_snow_grid

      endif

      return
      end
  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE SOLVEWB(xnew,Tf,Tair,rh,xLs,Cp,sfc_pressure)

      implicit none

      real A,B,C,ea,rh,Tair,Tf,tol,old,fprime,xLs,Cp,funct,xnew,
     &  sfc_pressure

      integer maxiter,i

c Coeffs for saturation vapor pressure over water (Buck 1981).
c   Note: temperatures for Buck`s equations are in deg C, and
c   vapor pressures are in mb.  Do the adjustments so that the
c   calculations are done with temperatures in K, and vapor
c   pressures in Pa.

c Over water.
        A = 6.1121 * 100.0
        B = 17.502
        C = 240.97
c Over ice.
c       A = 6.1115 * 100.0
c       B = 22.452
c       C = 272.55

c Atmospheric vapor pressure from relative humidity data.
      ea = rh / 100.0 * A * exp((B * (Tair - Tf))/(C + (Tair - Tf)))

c Solve for the wet bulb temperature.
      tol = 1.0e-2
      maxiter = 20
      old = Tair

      do i=1,maxiter
        fprime = 1.0 + xLs/Cp * 0.622/sfc_pressure * log(10.0) *
     &    2353. * (10.0**(11.40 - 2353./old)) / old**2
        funct = old - Tair + xLs/Cp * 0.622/sfc_pressure *
     &    (10.0**(11.40-2353./old) - ea)
        xnew = old - funct/fprime
        if (abs(xnew - old).lt.tol) return
        old = xnew
      end do

c If the maximum iterations are exceeded, send a message and set
c   the wet bulb temperature to the air temperature.
      write (*,102)
  102 format('max iteration exceeded when solving for Twb')
      xnew = Tair

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE NSNOWDEN(ro_nsnow,Twb,Tf,dt)

      implicit none

      real Twgmax,Tf,Twb,ro_nsnow,scalefact,dt,wt

      Twgmax = Tf + 1.0
      if (Twb.ge.258.16 .and. Twb.le.Twgmax) then
        ro_nsnow = 50. + 1.7 * (Twb - 258.16)**1.5
      elseif (Twb.lt.258.16) then
        ro_nsnow = 50.0
      else
        ro_nsnow = 158.8
      endif


c For one day time steps, this equation gives a new snow density at
c   the end of the 24 hour period which is too low, by an approximate
c   factor of X.  Thus, for a daily time step, I scale the density by
c   X before returning it to the main program.

      scalefact = 1.0
      if (dt.eq.86400.0) then
        if (ro_nsnow.le.158.8) then
          wt = 1.0 + (50.0 - ro_nsnow) / 108.8
          ro_nsnow = wt * scalefact * ro_nsnow + ro_nsnow
          ro_nsnow = min(158.8,ro_nsnow)
        endif
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE POSTPROC(ro_snow_grid,xro_snow,snow_depth,undef)

      implicit none

      real snow_depth,xro_snow,undef,ro_snow_grid

      if (snow_depth.eq.0.0) then
        xro_snow = undef
      else
        xro_snow = ro_snow_grid
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE CONSTS_SNOWPACK(Cp,xLs,ro_ice,xLf,Tf,A1,A2,ro_water,
     &  Cp_snow,ro_snowmax,Cp_water)

      implicit none

      real Cp,xLs,ro_ice,xLf,Tf,A1,A2,ro_water,Cp_snow,ro_snowmax,
     &  Cp_water

      Cp = 1004.
      xLs = 2.500e6
      ro_ice = 917.0
      xLf = 3.34e5
      Tf = 273.16
      A1 = 0.0013
      A2 = 0.021
      ro_water = 1000.0
      Cp_snow = 2106.
      ro_snowmax = 550.0
      Cp_water = 4180.0
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE MULTI_LAYER_SNOW(JJ,ro_layer,Tf,dt,ro_water,
     &  ro_ice,T_old,dy_snow,swe_lyr,Qm,ro_snowmax,rain,
     &  xLf,Cp_snow,melt_flag,runoff,tslsnowfall,ro_nsnow,
     &  sprec,Tsfc,tsls_threshold,gamma,max_layers,change_layer,
     &  dz_snow_min,snow_depth,swe_depth,undef,canopy_unload,
     &  vegtype,glacier_melt,sum_glacmelt,sum_swemelt,snow_d,
     &  Qe,sfc_sublim_flag,sum_sfcsublim,soft_snow_d,ro_snow,
     &  sum_sprec,sprec_grnd_ml,sum_prec,prec,sum_runoff,
     &  ro_snow_grid,xro_snow,swesublim,A1,A2,fc_param,
     &  Cp_water,ml_ret,icorr_factor_index,corr_factor,liqfrac,
     &  microstructure_flag,
     &  windspd,temp_grad,d_opt,grain_sz,ssa,dendricity,
     &  sphericity,t07_flag,f06_flag,b92_flag,c13_flag,zi,viscosity,
     &  snow_age,history,terrain_slope,dendricity_n,
     &  sphericity_n,d_opt_n,
     &  ssa_n,grain_sz_n,viscosity_n,zi_n,snow_age_n,history_n,
     &  mass_accumulation,dangl_n,dangl,iter,snthrm_flag)

      implicit none

      include 'snowmodel.inc'

      integer JJ,max_layers
      integer melt_flag(nz_max)
      integer i,j
      integer icorr_factor_index

      real dy_snow(nz_max)
      real swe_lyr(nz_max)
      real ro_layer(nz_max)
      real T_old(nz_max)
      real gamma(nz_max)

      real ml_ret(nz_max)
      real liqfrac(nz_max) 
      !sw
      real d_opt(nz_max)
      real grain_sz(nz_max)
      real ssa(nz_max)
      real dendricity(nz_max)
      real sphericity(nz_max)
      real zi(nz_max)
      real viscosity(nz_max)
      real snow_age(nz_max)
      real history(nz_max)
      integer t07_flag
      integer f06_flag
      integer b92_flag
      integer c13_flag
      integer snthrm_flag
      integer microstructure_flag
      real windspd
      real terrain_slope
      real mass_accumulation(nz_max) 
      real temp_grad(nz_max) 
      real dangl(nz_max)
      real sfc_pressure
      integer iter 
        
      !sw 
      real dendricity_n,sphericity_n,d_opt_n,
     & ssa_n,grain_sz_n,viscosity_n,zi_n,snow_age_n,history_n,
     & dangl_n
      

      real Tf,dt,ro_water,ro_ice,Qm,ro_snowmax,rain,xLf,Cp_snow,
     &  runoff,tslsnowfall,ro_nsnow,sprec,Tsfc,tsls_threshold,
     &  dz_snow_min,snow_depth,swe_depth,undef,change_layer,
     &  canopy_unload,vegtype,glacier_melt,sum_glacmelt,sum_swemelt,
     &  soft_snow_d,Qe,sfc_sublim_flag,sum_sfcsublim,snow_d,
     &  ro_snow,sum_sprec,sprec_grnd_ml,sum_prec,prec,sum_runoff,
     &  ro_snow_grid,xro_snow,swesublim,A1,A2,Tk,fc_param,Cp_water,
     &  corr_factor

c THIS IS THE MULTI-LAYER SNOWPACK MODEL.

c Note there is some confusion with the dy - dz notation used here.
c   In the multi-layer code 'y' is the vertical coordinate.  This is
c   a hold-over from a time when my temperature solution code had
c   y going up-down.

      if (microstructure_flag .eq. 1)then 
        CALL MICROSTRUCTURE(liqfrac,microstructure_flag,
     &   windspd,temp_grad,d_opt,grain_sz,ssa,dendricity,
     &   sphericity,t07_flag,f06_flag,b92_flag,c13_flag,
     &   zi,snow_age,ro_layer,T_old,Tf,history,dy_snow,dt,JJ,dangl,
     &   iter,snthrm_flag,sfc_pressure)
      endif 

c Compute the snow density change due to compaction.
      CALL DDENSITY_ML(ro_layer,Tf,dt,ro_water,ro_ice,
     &  T_old,JJ,dy_snow,A1,A2,terrain_slope,microstructure_flag,
     &  t07_flag,f06_flag,b92_flag,c13_flag,viscosity,
     &  mass_accumulation,dendricity,sphericity,history,
     &  ssa,liqfrac)
     
c Compute wind compaction 
      if (microstructure_flag .eq. 1)then 
        CALL WIND_COMPACTION(ro_layer,dendricity,
     & sphericity,windspd,dy_snow,zi,grain_sz,d_opt,JJ)
      endif 
      
c Calculate the rainfall from prec and sprec.
      if (prec.gt.0.0) then
        rain = prec - sprec
      else
        rain = 0.0
      endif

c J.PFLUG
c included PRECIP_ML, MERGE_LAYERS_ML, SNOWTEMP_ML, and POST_PROC_ML
c within the MELT_SNOW_ML routine for convenience with the dynamic 
c timestep. This can be easily changed by looking at the code in 
c MELT_SNOW_ML 

c Distribute surface melt and rain precipitation through the snowpack.
      CALL MELT_SNOW_ML(JJ,swe_lyr,ro_water,ro_layer,Qm,dt,
     &  dy_snow,ro_snowmax,rain,xLf,Cp_snow,Tf,T_old,melt_flag,
     &  runoff,canopy_unload,swe_depth,snow_depth,vegtype,
     &  glacier_melt,sum_glacmelt,sum_swemelt,soft_snow_d,Qe,
     &  sfc_sublim_flag,sum_sfcsublim,swesublim,fc_param,
     &  Cp_water,ml_ret,
     &  change_layer,dz_snow_min,gamma,max_layers,prec,ro_nsnow,
     &  ro_snow,ro_snow_grid,snow_d,sprec_grnd_ml,sum_prec,
     &  sum_runoff,sum_sprec,tsfc,tsls_threshold,tslsnowfall,undef,
     &  xro_snow,icorr_factor_index,corr_factor,liqfrac,
     &  d_opt,ssa,grain_sz,sphericity,history,
     &  zi,snow_age,viscosity,microstructure_flag,dendricity,dangl,
     &  d_opt_n,dangl_n,dendricity_n,grain_sz_n,history_n,snow_age_n,
     &  sphericity_n,viscosity_n,zi_n,ssa_n)
     
c Account for the accumulation of snow precipitation on the snowpack.
c      CALL PRECIP_ML(JJ,ro_layer,dy_snow,ro_water,tslsnowfall,
c     &  swe_lyr,ro_nsnow,T_old,Tsfc,tsls_threshold,dt,
c     &  melt_flag,soft_snow_d,ro_snow,sum_sprec,sprec_grnd_ml,
c     &  sum_prec,prec,sum_runoff,runoff,snow_d,snow_depth,swe_depth,
c     &  ml_ret)

c Merge layers if the number of layers exceeds some maximum number of
c   layers or if a layer gets thinner than some minimum thickness.
c      CALL MERGE_LAYERS_ML(JJ,ro_layer,dy_snow,swe_lyr,T_old,
c     &  ro_water,max_layers,change_layer,dz_snow_min,melt_flag)
     
c Calculate the temperature of each snow layer.
c      CALL SNOWTEMP_ML(gamma,T_old,Tsfc,JJ,dt,ro_layer,Cp_snow,
c     &  Tf,dy_snow,melt_flag)

c Postprocess the data.
c      CALL POST_PROC_ML(JJ,dy_snow,snow_depth,swe_depth,undef,
c     &  swe_lyr,gamma,ro_layer,melt_flag,T_old,Tf,ro_snow_grid,
c     &  ro_water,xro_snow)

c END J.PFLUG
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE GETGAMMA(JJ,ro_layer,gamma)

      implicit none

      include 'snowmodel.inc'

      integer j,JJ
      real ro_layer(nz_max)
      real gamma(nz_max)

c Compute the snow thermal conductivity (gamma) from the snow density.
      do j=1,JJ
        if (ro_layer(j).lt.156.0) then
          gamma(j) = 0.023 + 0.234 * (ro_layer(j)/1000.0)
        else
          gamma(j) = 0.138 - 1.01 * (ro_layer(j)/1000.0) + 3.233 *
     &      (ro_layer(j)/1000.0)**2
        endif
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE POST_PROC_ML(JJ,dy_snow,snow_depth,swe_depth,undef,
     &  swe_lyr,gamma,ro_layer,melt_flag,T_old,Tf,ro_snow_grid,
     &  ro_water,xro_snow)

      implicit none

      include 'snowmodel.inc'

      integer j,JJ

      real dy_snow(nz_max)
      real swe_lyr(nz_max)
      real gamma(nz_max)
      real ro_layer(nz_max)
      real T_old(nz_max)
      integer melt_flag(nz_max)

      real snow_depth,swe_depth,undef,Tf,ro_snow_grid,ro_water,
     &  xro_snow

c Calculate the total snow and swe depth, and the bulk snow density.
      snow_depth = 0.0
      swe_depth = 0.0
      do j=1,JJ
        snow_depth = snow_depth + dy_snow(j)
        swe_depth = swe_depth + swe_lyr(j)
      enddo
      ro_snow_grid = swe_depth * ro_water / snow_depth

c Set any areas outside the snowpack to undef.
      do j=JJ+1,nz_max
        gamma(j) = undef
        ro_layer(j) = undef
        T_old(j) = undef + Tf
        melt_flag(j) = nint(undef)
        dy_snow(j) = undef
        swe_lyr(j) = undef
      enddo

c Clean up the snow density array so there are no values when
c   there is no snow.
      if (snow_depth.eq.0.0) then
        xro_snow = undef
      else
        xro_snow = ro_snow_grid
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE MERGE_LAYERS_ML(JJ,ro_layer,dy_snow,swe_lyr,T_old,
     &  ro_water,max_layers,change_layer,dz_snow_min,melt_flag,
     &  dendricity,sphericity,ssa,grain_sz,d_opt,viscosity,
     &  microstructure_flag,snow_age,history,zi,dangl)

      implicit none

      include 'snowmodel.inc'

      integer j,JJ,jjj,j_small,max_layers,icount,k

      real swe_lyr(nz_max)
      real ro_layer(nz_max)
      real dy_snow(nz_max)
      real T_old(nz_max)
      integer melt_flag(nz_max)
      
      !sw 
      integer microstructure_flag 
      real dendricity(nz_max)
      real sphericity(nz_max)
      real d_opt(nz_max)
      real viscosity(nz_max)
      real ssa(nz_max) 
      real snow_age(nz_max)
      real history(nz_max)
      real grain_sz(nz_max)
      real zi(nz_max)
      real dangl(nz_max)
      real dy_snow_small,ro_water,dz_snow_min,change_layer

c Merge layers if the number of layers exceeds some maximum number of
c   layers or if a layer gets thinner than some minimum thickness.
c   Do this in snow_depth space because that is the grid the snow
c   temperatures are being calculated on.

c If any layer is thinner than the minimum layer thickness, merge it
c   with the layer below.  If that layer is layer 1, merge it with
c   layer 2.  If there is only one layer left, let it be smaller than
c   the minimum thickness.  Don't do any of this if a new layer is
c   being built; only do it for layers below the top layer.
      change_layer = 0.0

c Count how many layers are less than the minimum thickness, excluding
c   the case where there is only one layer.
      icount = 0
      if (JJ.gt.1) then
c       do j=1,JJ
        do j=1,JJ-1
          if (dy_snow(j).lt.dz_snow_min) then
            icount = icount + 1
          endif
        enddo
      endif



c Note that if two thin layers are together, the merge may take
c   out the other one.
      do k=1,icount
        change_layer = 1.0

c This gets and processes the last occurance.
c       do j=1,JJ
        do j=1,JJ-1
          if (dy_snow(j).lt.dz_snow_min) then
            j_small = j
          endif
        enddo

        if (j_small.eq.1) then
          dy_snow(1) = dy_snow(1) + dy_snow(2)
          swe_lyr(1) = swe_lyr(1) + swe_lyr(2)
          ro_layer(1) = swe_lyr(1) * ro_water / dy_snow(1)
          T_old(1) = T_old(2)
          melt_flag(1) = melt_flag(2)
          !sw 
          if(microstructure_flag .eq. 1) then 
            dendricity(1) = (dendricity(1) + dendricity(2)) /2
            sphericity(1) = (sphericity(1) + sphericity(2)) / 2
            d_opt(1) = (d_opt(1) + d_opt(2))/2
            grain_sz(1) = grain_sz(1) + grain_sz(2) 
            ssa(1) = (ssa(1) + ssa(2))/2
            viscosity(1) = (viscosity(1) + viscosity(2)) /2
            snow_age(1) = (snow_age(1) +  snow_age(2))/2
            ! snow_age(1) = max(snow_age(1) + snow_age(2))
            history(1) =  max(history(1), history(2))
            ! history(1) =  (history(1) +  history(2))/2
            zi(1) = (zi(1) + zi(2)) / 2 
            dangl(1) = (dangl(1) + dangl(2)) /2 
            
          endif
          JJ = JJ - 1
          do jjj=2,JJ
            dy_snow(jjj) = dy_snow(jjj+1)
              swe_lyr(jjj) = swe_lyr(jjj+1)
            ro_layer(jjj) = swe_lyr(jjj) * ro_water / dy_snow(jjj)
            T_old(jjj) = T_old(jjj+1)
            melt_flag(jjj) = melt_flag(jjj+1)
            !sw 
            if(microstructure_flag .eq. 1) then
              dendricity(jjj) = dendricity(jjj+1)
              sphericity(jjj) = sphericity(jjj+1)
              d_opt(jjj) = d_opt(jjj+1)
              grain_sz(jjj) = grain_sz(jjj+1)
              ssa(jjj) = ssa(jjj+1)
              viscosity(jjj) = viscosity(jjj+1)
              snow_age(jjj) = snow_age(jjj+1)
              history(jjj) = history(jjj+1)
              zi(jjj) = zi(jjj+1)
              dangl(jjj) = dangl(jjj+1) 
            endif 
          enddo
        else
          dy_snow(j_small-1) = dy_snow(j_small-1) + dy_snow(j_small)
          swe_lyr(j_small-1) = swe_lyr(j_small-1) + swe_lyr(j_small)
          ro_layer(j_small-1) = swe_lyr(j_small-1) * ro_water /
     &      dy_snow(j_small-1)
          T_old(j_small-1) = T_old(j_small)
          melt_flag(j_small-1) = melt_flag(j_small)
          !sw  
          if(microstructure_flag .eq. 1) then 
            dendricity(j_small-1) = (dendricity(j_small-1) + 
     &      dendricity(j_small)) /2 
            sphericity(j_small-1) = (sphericity(j_small-1) +
     &      sphericity(j_small)) /2 
            d_opt(j_small-1) = (d_opt(j_small-1) + d_opt(j_small))/2 
            grain_sz(j_small-1) = (grain_sz(j_small-1) +
     &      grain_sz(j_small)) /2 
            ssa(j_small-1) = (ssa(j_small-1) + ssa(j_small))/2
            viscosity(j_small-1) = (viscosity(j_small-1) + 
     &      viscosity(j_small)) / 2 
c           snow_age(j_small-1) = MAX(snow_age(j_small-1),
c    &      snow_age(j_small))
            history(j_small-1) = MAX(history(j_small-1), 
     &      history(j_small))
            snow_age(j_small-1) = (snow_age(j_small-1)+
     &      snow_age(j_small))/2
c           history(j_small-1) =(history(j_small-1)+ 
c    &      history(j_small))/2
            zi(j_small-1) = (zi(j_small-1) + zi(j_small))/2
            dangl(j_small-1) = (dangl(j_small-1) + dangl(j_small))/2
          endif 
          
          JJ = JJ - 1
          do jjj=j_small,JJ
            dy_snow(jjj) = dy_snow(jjj+1)
            swe_lyr(jjj) = swe_lyr(jjj+1)
            ro_layer(jjj) = swe_lyr(jjj) * ro_water / dy_snow(jjj)
            T_old(jjj) = T_old(jjj+1)
            melt_flag(jjj) = melt_flag(jjj+1)
            !sw 
            if(microstructure_flag .eq. 1) then 
              dendricity(jjj) = dendricity(jjj+1) 
              sphericity(jjj) = sphericity(jjj+1) 
              d_opt(jjj) = d_opt(jjj+1)
              grain_sz(jjj) = grain_sz(jjj+1) 
              ssa(jjj) = ssa(jjj+1)
              viscosity(jjj) = viscosity(jjj+1)
              snow_age(jjj) = snow_age(jjj+1)
              history(jjj) = history(jjj+1)
              zi(jjj) = zi(jjj+1)
              dangl(jjj) = dangl(jjj+1) 
            endif 
          enddo
        endif
      enddo



c Where the number of layers exceeds some maximum number of layers,
c   find the thinnest layer and merge it with the one below.  For the
c   case where the thinnest layer is the bottom layer, merge it with
c   layer 2.
      if (JJ.eq.max_layers+1) then
        change_layer = 1.0
c Find the thinnest layer.
        dy_snow_small = 1000.0
        do j=1,JJ
          if (dy_snow(j).lt.dy_snow_small) then
            dy_snow_small = dy_snow(j)
            j_small = j
          endif
        enddo

c Adjust accordingly.  Note that layers below the thin layer do not
c   change, unless the thin layer is layer 1.  Also, since the layer
c   is thin, assign the new layer the thick layer temperature.
        if (j_small.eq.1) then
          dy_snow(1) = dy_snow(1) + dy_snow(2)
          swe_lyr(1) = swe_lyr(1) + swe_lyr(2)
          ro_layer(1) = swe_lyr(1) * ro_water / dy_snow(1)
          T_old(1) = T_old(2)
          melt_flag(1) = melt_flag(2)
          !sw
          if (microstructure_flag .eq. 1) then  
            dendricity(1) = (dendricity(1) + dendricity(2)) /2
            sphericity(1) = (sphericity(1) + sphericity(2)) / 2 
            d_opt(1) = (d_opt(1) + d_opt(2))/2
            grain_sz(1) = grain_sz(1) + grain_sz(2) 
            ssa(1) = (ssa(1) + ssa(2))/2
            viscosity(1) = (viscosity(1) + viscosity(2)) /2
            !snow_age(1) = MAX (snow_age(1), snow_age(2))
            snow_age(1) = (snow_age(1) + snow_age(2))/2 
            history(1) =  MAX(history(1),history(2))
            zi(1) = (zi(1) + zi(2)) / 2 
            dangl(1) = (dangl(1) + dangl(2)) /2 
          endif
          JJ = JJ - 1
          do jjj=2,JJ
            dy_snow(jjj) = dy_snow(jjj+1)
              swe_lyr(jjj) = swe_lyr(jjj+1)
            ro_layer(jjj) = swe_lyr(jjj) * ro_water / dy_snow(jjj)
            T_old(jjj) = T_old(jjj+1)
            melt_flag(jjj) = melt_flag(jjj+1)
            !sw 
            if(microstructure_flag .eq. 1) then
              dendricity(jjj) = dendricity(jjj+1)
              sphericity(jjj) = sphericity(jjj+1) 
              d_opt(jjj) = d_opt(jjj+1)
              grain_sz(jjj) = grain_sz(jjj+1)
              ssa(jjj) = ssa(jjj+1)
              viscosity(jjj) = viscosity(jjj+1)
              snow_age(jjj) = snow_age(jjj+1)
              history(jjj) = history(jjj+1)
              zi(jjj) = zi(jjj+1)
              dangl(jjj) = dangl(jjj+1) 
            endif 
          enddo
        else
          dy_snow(j_small-1) = dy_snow(j_small-1) + dy_snow(j_small)
          swe_lyr(j_small-1) = swe_lyr(j_small-1) + swe_lyr(j_small)
          ro_layer(j_small-1) = swe_lyr(j_small-1) * ro_water /
     &      dy_snow(j_small-1)
          T_old(j_small-1) = T_old(j_small)
          melt_flag(j_small-1) = melt_flag(j_small)
         !sw  
          if(microstructure_flag .eq. 1) then 
            dendricity(j_small-1) = (dendricity(j_small-1) + 
     &      dendricity(j_small)) /2 
            sphericity(j_small-1) = (sphericity(j_small-1) +
     &      sphericity(j_small)) /2 
            d_opt(j_small-1) = (d_opt(j_small-1) + d_opt(j_small))/2 
            grain_sz(j_small-1) = (grain_sz(j_small-1) +
     &      grain_sz(j_small)) /2 
            ssa(j_small-1) = (ssa(j_small-1) + ssa(j_small))/2
            viscosity(j_small-1) = (viscosity(j_small-1) + 
     &      viscosity(j_small)) / 2 
c           snow_age(j_small-1) = MAX(snow_age(j_small-1),
c    &      snow_age(j_small))
            snow_age(j_small-1) = (snow_age(j_small-1)+
     &      snow_age(j_small))/2 
            history(j_small-1) = MAX(history(j_small-1), 
     &      history(j_small))
            zi(j_small-1) = (zi(j_small-1) + zi(j_small))/2
            dangl(j_small-1) = (dangl(j_small-1) + dangl(j_small))/2
          endif
          JJ = JJ - 1
          do jjj=j_small,JJ
            dy_snow(jjj) = dy_snow(jjj+1)
            swe_lyr(jjj) = swe_lyr(jjj+1)
            ro_layer(jjj) = swe_lyr(jjj) * ro_water / dy_snow(jjj)
            T_old(jjj) = T_old(jjj+1)
            melt_flag(jjj) = melt_flag(jjj+1)
            !sw 
            if(microstructure_flag .eq. 1) then 
              dendricity(jjj) = dendricity(jjj+1) 
              sphericity(jjj) = sphericity(jjj+1) 
              d_opt(jjj) = d_opt(jjj+1)
              grain_sz(jjj) = grain_sz(jjj+1) 
              ssa(jjj) = ssa(jjj+1)
              viscosity(jjj) = viscosity(jjj+1)
              snow_age(jjj) = snow_age(jjj+1)
              history(jjj) = history(jjj+1)
              zi(jjj) = zi(jjj+1)
              dangl(jjj) = dangl(jjj+1) 
            endif 
          enddo
        endif
      endif

c Now that we are done with change_layer, set it equal to j_small,
c   the position of the change.
      if (change_layer.eq.1.0) change_layer = real(j_small)

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE MELT_SNOW_ML(JJ,swe_lyr,ro_water,ro_layer,Qm,dt,
     &  dy_snow,ro_snowmax,rain,xLf,Cp_snow,Tf,T_old,melt_flag,
     &  runoff,canopy_unload,swe_depth,snow_depth,vegtype,
     &  glacier_melt,sum_glacmelt,sum_swemelt,soft_snow_d,Qe,
     &  sfc_sublim_flag,sum_sfcsublim,swesublim,fc_param,
     &  Cp_water,ml_ret,
     &  change_layer,dz_snow_min,gamma,max_layers,prec,ro_nsnow,
     &  ro_snow,ro_snow_grid,snow_d,sprec_grnd_ml,sum_prec,
     &  sum_runoff,sum_sprec,tsfc,tsls_threshold,tslsnowfall,undef,
     &  xro_snow,icorr_factor_index,corr_factor,liqfrac,
     &  d_opt,ssa,grain_sz,sphericity,history,
     &  zi,snow_age,viscosity,microstructure_flag,dendricity,dangl,
     &  d_opt_n,dangl_n,dendricity_n,grain_sz_n,history_n,snow_age_n,
     &  sphericity_n,viscosity_n,zi_n,ssa_n)


      implicit none

      include 'snowmodel.inc'

      integer j,JJ

      real swe_lyr(nz_max)
      real ro_layer(nz_max)
      real dy_snow(nz_max)
      real T_old(nz_max)

      real liqflux,sumup

      real mLayerTheta(nz_max)
      real liqfrac(nz_max)
      real icefrac(nz_max)
      real liqfluxar(nz_max)
      real ml_ret(nz_max)
      real diff(nz_max)
      !sw 
      !real liqfrac(nz_max) 
      
      !sw 
      ! real fracliquid(nz_max) 
      real d_opt(nz_max)
      real ssa(nz_max)
      real grain_sz(nz_max)
      real sphericity(nz_max)
      real history(nz_max)
      real zi(nz_max)
      real snow_age(nz_max)
      real viscosity(nz_max)
      real dendricity(nz_max)
      real dangl(nz_max)
      integer microstructure_flag
      
      real d_opt_n,dangl_n,dendricity_n,grain_sz_n,history_n,snow_age_n,
     &  sphericity_n,viscosity_n,zi_n,ssa_n
      
      
      real swemelt,extra,ro_water,swe_space,add,runoff,ro_snowmax,
     &  rain,delta_T,xLf,Cp_snow,Tf,dt,Qm,canopy_unload,
     &  potmelt,swe_depth,snow_depth,vegtype,glacier_melt,
     &  sum_glacmelt,sum_swemelt,soft_snow_d,Qe,sfc_sublim_flag,
     &  xLsublim,potsublim,swesublim,sum_sfcsublim,Cp_water

      real fc_param,dt_new
 
      integer melt_flag(nz_max)

      integer max_layers,iteration
      real change_layer,dz_snow_min,prec,ro_nsnow,ro_snow,ro_snow_grid,
     &  snow_d,sprec_grnd_ml,sum_prec,sum_runoff,sum_sprec,tsfc,
     &  tsls_threshold,tslsnowfall,undef,xro_snow
      real gamma(nz_max)

      real swe_depth_old,snow_depth_old,sum_glacmelt_old,runoff_old,
     &  sum_swemelt_old
      real swe_lyr_old(nz_max),dy_snow_old(nz_max),
     &  ro_layer_old(nz_max),melt_flag_old(nz_max)
      integer JJ_old,b,k
      real sprec_grnd_ml_new,prec_new,runoff_sumup

      integer icorr_factor_index
      real potmelt_tmp,corr_factor

c Initialize the runoff array.
      runoff = 0.0
      
c SURFACE SUBLIMATION.

c Whether static-surface (non-blowing snow) sublimation is included
c   in the model calculations is controlled by the sfc_sublim_flag.
c   I am waiting for the flux-tower data Matthew and I are collecting
c   in Alaska, to compare with the model simulations, before
c   including this part of the model in all simulations.

c If the sfc_sublim_flag is turned on, the latent heat flux (Qe)
c   calculated in ENBAL is used to add/remove snow from the snowpack.
c   xLsublim = xLf + xLv = 2.5104x10^6 J/kg + 3.334x10^5 J/kg, and
c   potsublim is in m swe.
c      do b = 1,iteration

      if (swe_depth.gt.0.0  .and.  sfc_sublim_flag.eq.1.0) then
        if (Qe.lt.0.0) then
c Compute the snow-surface sublimation (m, swe).
          xLsublim = 2.844e6
          potsublim = (- dt) * Qe / (ro_water * xLsublim)
          swesublim = min(potsublim,swe_depth)
c Save a summing array of the static surface snow sublimation.
          sum_sfcsublim = sum_sfcsublim + swesublim
        else
          swesublim = 0.0
        endif
      else
        swesublim = 0.0
      endif

c Modify the swe layer thicknesses, and reduce the number of layers
c   if needed.
      if (swesublim.gt.0.0) then
c Check to see whether this sublimation requires a layer reduction.
        CALL REDUCE_LAYERS(swesublim,swe_lyr,JJ)

c Build the new snow layer thicknesses, and recalculate the total
c   snow and swe depths.  Assume this sublimated snow does not
c   change the snow density and does not change the soft snow depth.
c   It only reduces the snow depth and the associated swe depth.
        snow_depth = 0.0
        swe_depth = 0.0
        do j=1,JJ
          dy_snow(j) = swe_lyr(j) * ro_water / ro_layer(j)
          snow_depth = snow_depth + dy_snow(j)
          swe_depth = swe_depth + swe_lyr(j)
        enddo
      endif

c J.PFLUG
c Initialize the dynamic timestep iteration
      iteration = 1
      swe_depth_old = swe_depth
      snow_depth_old = snow_depth
      JJ_old = JJ
      do j=1,JJ
        swe_lyr_old(j) = swe_lyr(j)
        dy_snow_old(j) = dy_snow(j)
        ro_layer_old(j) = ro_layer(j)
        melt_flag_old(j) = melt_flag(j)
      enddo
      sum_glacmelt_old = sum_glacmelt
      runoff_old = 0.0
      sum_swemelt_old = sum_swemelt
c if no convergence, return to this point
8888  dt_new = dt/iteration
      runoff_sumup = 0.0

c run through the new number of dynamic timesteps
      do b = 1,iteration
c reinitialize runoff for this dynamic timestep
        runoff = 0.0
c MELTING.

        if (Qm.gt.0.0) then

c Convert the melt energy to water equivalent melt depth (m).
          potmelt = dt_new * Qm / (ro_water * xLf)

c Account for any snowmelt data assimilation.
c       if (icorr_factor_index.lt.0.0.and.corr_factor.lt.3.0) then
       if (icorr_factor_index.lt.0.0) then
         potmelt_tmp = potmelt * corr_factor
         swemelt = min(potmelt_tmp,swe_depth)
c Handle the case of no snowmelt data assimilation.
       else
         swemelt = min(potmelt,swe_depth)
       endif

c Compute any glacier or permanent snow-field melt (m water equiv.).
          if (vegtype.eq.20.0) then
            glacier_melt = potmelt - swemelt
          else
            glacier_melt = 0.0
          endif

c Save a summing array of the glacier melt.
          sum_glacmelt = sum_glacmelt + glacier_melt

c Save the runoff contribution.
          runoff = runoff + glacier_melt

c Save a summing array of the snow melt.
          sum_swemelt = sum_swemelt + swemelt

c In the presence of melt, zero out the soft snow layer.
          soft_snow_d = 0.0

        else

c These prevent values from the previous time step from being
c   carried through to the next time step.
          swemelt = 0.0
          glacier_melt = 0.0

        endif

c Handle the case where rain and canopy_unload fall on snow-free
c   ground (this is not included in the code below, nor in the
c   PRECIP_ML subroutine, so I include it here).
        if (swe_depth.eq.0.0) then
          runoff = runoff+(rain/iteration)+
     &      (canopy_unload/iteration)
        endif

c Deal with melting snow.

        if (swemelt.gt.0.0) then
c Check to see whether this melt leads to a reduction in layers.
          CALL REDUCE_LAYERS(swemelt,swe_lyr,JJ)

c Build the new snow layer thicknesses, and initiate the melt_flag.
          do j=1,JJ
            dy_snow(j) = swe_lyr(j) * ro_water / ro_layer(j)
            melt_flag(j) = 0
          enddo
        endif

c Add the melt, rain, and canopy unloading (assumed to be wet as rain)
c   to the remaining snow layer thicknesses, up to the maximum snow
c   density, and let the rest of the melt drain out the snowpack bottom
c   as runoff.

c variable to track how much liquid water is available for transport
        if (swe_depth.gt.0.0) then
          sumup = swemelt+(rain/iteration)+
     &      (canopy_unload/iteration)
        
c determine layer properties prior to flux (in fractional form)
          do j=JJ,1,-1
            mLayerTheta(j) = swe_lyr(j)/dy_snow(j)
          enddo

c for each layer, investigate the snowpack composition and then
c instantiate fluxes based on incoming liquid water
          do j=JJ,1,-1

            liqfluxar(j) = sumup

c determine layer ice and liquid content based on initial 
c densities and temperatures
            CALL UPDATE_STATE(T_old(j),mLayerTheta(j),
     &        liqfrac(j),icefrac(j),fc_param,Tf,ro_water,
     &        ml_ret(j))

c determine the amount of liquid water in fractional form
c input in the liquid flux routine
            extra = liqfrac(j)

c determine the unsaturated flux
            CALL SNOWLIQFLX(dt_new,extra,liqfrac(j),icefrac(j),
     &        liqflux,ml_ret(j))
c           print *,'1',extra 
c           print *,'2',liqfrac(j) 

c check to see if unstable
c unstable is defined as fluxes larger than the available water
            if (liqflux.gt.sumup) then
c if the timestep is larger than 10 minutes and the liquid
c water content is larger than 0.1 mm
              if (dt_new.gt.600.and.sumup.gt.0.0001) then
c increase the timestep and throw message
                iteration = iteration + 1
c                if (iteration.eq.2) then
c                  print *,'employing dynamic timestep for convergence'
c                endif

c reinstantiate state variables
                swe_depth = swe_depth_old
                snow_depth = snow_depth_old
                JJ = JJ_old
                do k=1,JJ
                  swe_lyr(k) = swe_lyr_old(k)
                  dy_snow(k) = dy_snow_old(k)
                  ro_layer(k) = ro_layer_old(k)
                  melt_flag(k) = melt_flag_old(k)
                enddo
                sum_glacmelt = sum_glacmelt_old
                runoff = runoff_old
                sum_swemelt = sum_swemelt_old
                goto 8888
              else
c if approaching convergence, force to converge
                liqflux = sumup
              endif
            endif
        
c rebuild the snowpack
            swe_lyr(j) = swe_lyr(j) + sumup - liqflux
            ro_layer(j) = swe_lyr(j) * ro_water/dy_snow(j)
            sumup = liqflux

          enddo

c flux out of the last snow layer goes to runoff
          runoff = runoff + sumup

c recalculate layer depths
          do j=JJ,1,-1
            dy_snow(j) = swe_lyr(j) * ro_water / ro_layer(j)
          enddo

c Also take into account the refreezing of this liquid in a cold
c   snowpack.  Assume that the liquid will fully warm each layer before
c   moving on to the next layer.
          do j=JJ,1,-1

c calculate the amount of liquid that remains in the layer for a step
            if (j.gt.1) then
              diff(j) = liqfluxar(j)-liqfluxar(j-1)
            else
              diff(j) = liqfluxar(j)-runoff
            endif

c Compute the change in temperature that would result if this liquid
c   was used to freeze and raise the snow temperature.
            delta_T = (diff(j) * xLf) / (Cp_snow * dy_snow(j))

c Use this potential temperature change to adjust the snow
c   temperature in the presence of the liquid.
            T_old(j) = T_old(j) + delta_T
            T_old(j) = min(T_old(j),Tf)

c Keep track of which layers have been pushed to Tf.  This will be
c   used in the temperature solution subroutine to fix the snow
c   temperature at Tf (if melt_flag = 1).
            if (T_old(j).eq.Tf) then
              melt_flag(j) = 1
            else
              melt_flag(j) = 0
            endif

          enddo
        endif

c subsample precipitation based on the dynamic timestep
        sprec_grnd_ml_new = sprec_grnd_ml/iteration
        prec_new = prec/iteration

c Account for the accumulation of snow precipitation on the snowpack.
        CALL PRECIP_ML(JJ,ro_layer,dy_snow,ro_water,tslsnowfall,
     &    swe_lyr,ro_nsnow,T_old,Tsfc,tsls_threshold,dt_new,
     &    melt_flag,soft_snow_d,ro_snow,sum_sprec,sprec_grnd_ml_new,
     &    sum_prec,prec_new,sum_runoff,runoff,snow_d,snow_depth,
     &    swe_depth,ml_ret,microstructure_flag,dendricity_n,
     &    sphericity_n,d_opt_n,viscosity_n,ssa_n,zi_n,snow_age_n,
     &    history_n,grain_sz_n,dendricity,sphericity,d_opt,viscosity,
     &    ssa,snow_age,history,grain_sz,zi,dangl_n,dangl)

c Merge layers if the number of layers exceeds some maximum number of
c   layers or if a layer gets thinner than some minimum thickness.
        CALL MERGE_LAYERS_ML(JJ,ro_layer,dy_snow,swe_lyr,T_old,
     &    ro_water,max_layers,change_layer,dz_snow_min,melt_flag,
     &    dendricity,sphericity,ssa,grain_sz,d_opt,viscosity,
     &    microstructure_flag,snow_age,history,zi,dangl)

c Calculate the temperature of each snow layer.
        CALL SNOWTEMP_ML(gamma,T_old,Tsfc,JJ,dt_new,ro_layer,Cp_snow,
     &    Tf,dy_snow,melt_flag)

c Postprocess the data.
        CALL POST_PROC_ML(JJ,dy_snow,snow_depth,swe_depth,undef,
     &    swe_lyr,gamma,ro_layer,melt_flag,T_old,Tf,ro_snow_grid,
     &    ro_water,xro_snow)


c keep track of the accumulated runoff for the dynamic timestep
      runoff_sumup = runoff_sumup + runoff
      enddo

c total runoff for the set timestep
      runoff = runoff_sumup

c END J.PFLUG
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc          
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE REDUCE_LAYERS(swemelt,swe_lyr,JJ)

      implicit none

      include 'snowmodel.inc'

      integer j,JJ
      real swe_lyr(nz_max)
      real eps,swemelt_tmp,swemelt,excess

      eps = 1e-6
      swemelt_tmp = swemelt

c The use of eps here does not allow the vertical grid increment to
c   be less that eps.

      do j=JJ,1,-1
        excess = swe_lyr(j) - swemelt_tmp

c       if (excess.gt.0.0) then
        if (excess.gt.eps) then
          swe_lyr(j) = excess
          JJ = j
          return
c       elseif (excess.eq.0.0) then
        elseif (excess.ge.0.0 .and. excess.le.eps) then
          JJ = j - 1
          return
        else
          swemelt_tmp = - excess
        endif
      enddo

c If there is no snow left.
      JJ = 0

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE PRECIP_ML(JJ,ro_layer,dy_snow,ro_water,tslsnowfall,
     &  swe_lyr,ro_nsnow,T_old,Tsfc,tsls_threshold,dt,
     &  melt_flag,soft_snow_d,ro_snow,sum_sprec,sprec_grnd_ml,
     &  sum_prec,prec,sum_runoff,runoff,snow_d,snow_depth,swe_depth,
     &  ml_ret,microstructure_flag,dendricity_n,sphericity_n,d_opt_n,
     &  viscosity_n,ssa_n,zi_n,snow_age_n,history_n,grain_sz_n,
     &  dendricity,sphericity,d_opt,viscosity,ssa,
     &  snow_age,history,grain_sz,zi,dangl_n,dangl)

      implicit none

      include 'snowmodel.inc'

      integer j,JJ

      real dy_snow(nz_max)
      real ro_layer(nz_max)
      real swe_lyr(nz_max)
      real T_old(nz_max)
      real ml_ret(nz_max)
      real ro_nsnow,ro_water,z_nsnow,tsls_threshold,
     &  z_snowtopl,sweq_topl,Tsfc,tslsnowfall,dt,soft_snow_d,ro_snow,
     &  sum_sprec,sprec_grnd_ml,sum_prec,prec,sum_runoff,runoff,snow_d,
     &  snow_depth,swe_depth

      integer melt_flag(nz_max)
      !sw 
      integer microstructure_flag 
      real dendricity(nz_max)
      real sphericity(nz_max)
      real d_opt(nz_max)
      real viscosity(nz_max)
      real ssa(nz_max) 
      real snow_age(nz_max)
      real history(nz_max)
      real grain_sz(nz_max)
      real zi(nz_max)
      real dangl(nz_max)
      real dendricity_n,sphericity_n,d_opt_n,viscosity_n,ssa_n,zi_n,
     &  snow_age_n,history_n,grain_sz_n,dangl_n  

c If the melt from the previous subroutine reduced the snowpack
c   to no snow, reset the time since last snowfall to the threshold,
c   otherwise you will not build a new layer on the bare ground.
      if (JJ.eq.0) tslsnowfall = tsls_threshold

c Create and/or modify the snow c.v.'s to account for new snowfall.
      if (sprec_grnd_ml.gt.0.0) then
        if(tslsnowfall.ge.tsls_threshold.and.sprec_grnd_ml.gt.1e-6) then
c Create a new layer if snowfall has stopped for a period of time
c   greater or equal to the defined threshold.
          JJ = JJ + 1
          z_nsnow = ro_water / ro_nsnow * sprec_grnd_ml
          dy_snow(JJ) = z_nsnow
          ro_layer(JJ) = ro_nsnow
c J.PFLUG
          ml_ret(JJ) = 0.00001
c END J.PFLUG
          swe_lyr(JJ) =  ro_layer(JJ) * dy_snow(JJ) / ro_water
c Define this new snow layer to have the surface temperature.
          T_old(JJ) = Tsfc
          melt_flag(JJ) = 0
          if (microstructure_flag .eq. 1) then
            dendricity(JJ) = dendricity_n
            sphericity(JJ) = sphericity_n
            d_opt(JJ) = d_opt_n
            viscosity(JJ) = viscosity_n
            ssa(JJ) = ssa_n 
            zi(JJ) = zi_n 
            snow_age(JJ) = snow_age_n 
            history(JJ) = history_n     
            grain_sz(JJ) = grain_sz_n
            dangl(JJ) = dangl_n
          endif
  
        else
c Add to the existing top layer.
          z_nsnow = ro_water / ro_nsnow * sprec_grnd_ml
          z_snowtopl = dy_snow(JJ) + z_nsnow
          sweq_topl = sprec_grnd_ml + dy_snow(JJ) * ro_layer(JJ) /
     &      ro_water
          dy_snow(JJ) = dy_snow(JJ) + z_nsnow
          ro_layer(JJ) = ro_water * sweq_topl / z_snowtopl
          swe_lyr(JJ) = ro_layer(JJ) * dy_snow(JJ) / ro_water
        endif
    
        if (microstructure_flag .eq. 1) then 
          dendricity(JJ) = (dendricity(JJ) + dendricity_n) / 2
          sphericity(JJ) = (sphericity(JJ) + sphericity_n) / 2
          d_opt(JJ) = (d_opt(JJ) + d_opt_n) / 2     
          viscosity(JJ) = (viscosity(JJ) + viscosity_n) /2 
          ssa(JJ) = (ssa(JJ) + ssa_n) / 2 
          zi(JJ) = (zi(JJ) + zi_n) / 2 
          snow_age(JJ) = (snow_age(JJ) + snow_age_n) / 2  
          history(JJ) = (history(JJ) + snow_age_n) / 2 
          grain_sz(JJ) = (grain_sz(JJ) + grain_sz_n) / 2
          dangl(JJ) = (dangl(JJ) + dangl_n)/2
        endif   


c Update the total swe and snow depths.
        snow_depth = 0.0
        swe_depth = 0.0
        do j=1,JJ
          snow_depth = snow_depth + dy_snow(j)
          swe_depth = swe_depth + swe_lyr(j)
        enddo
      endif

c Define the time since last snowfall, in hours.  Handle the case
c   where there is no snow on the ground.
      if (sprec_grnd_ml.gt.0.0) then
        tslsnowfall = 0.0
      else
        tslsnowfall = tslsnowfall + dt / 3600.0
      endif
      if (JJ.eq.0) tslsnowfall = tsls_threshold

c The following are set up to be compatible with SnowTran-3D, and
c   are in snow-depth units.  The sum_sprec corrections are done
c   in the SnowTran-3D code.
      soft_snow_d = soft_snow_d + sprec_grnd_ml * ro_water / ro_snow
      snow_d = swe_depth * ro_water / ro_snow
c     sum_sprec = sum_sprec + sprec_grnd_ml * ro_water / ro_snow
      sum_sprec = sum_sprec + sprec_grnd_ml

c The following are in swe-depth units.
      sum_prec = sum_prec + prec
      sum_runoff = sum_runoff + runoff

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE DDENSITY_ML(ro_layer,Tf,dt,ro_water,ro_ice,
     &  T_old,JJ,dy_snow,A1,A2,terrain_slope,microstructure_flag,
     &  t07_flag,f06_flag,b92_flag,c13_flag,viscosity,
     &  mass_accumulation,dendricity,sphericity,history,
     &  ssa,liqfrac)

      implicit none

      include 'snowmodel.inc'

      integer j,JJ,jjj

      real dy_snow(nz_max)
      real ro_layer(nz_max)
      real T_old(nz_max)
      real sweqstar(nz_max)
      real sweql(nz_max)

      !sw
       
      real terrain_slope
      integer microstructure_flag
      integer t07_flag
      integer f06_flag
      integer b92_flag
      integer c13_flag
      real ro_snow_2(nz_max)
      real mass_accumulation(nz_max)
      real dendricity(nz_max)
      real sphericity(nz_max)
      real history(nz_max)
      real viscosity(nz_max)
      real ssa(nz_max)
      real liqfrac(nz_max)
      real dendricity_old(nz_max) 
      real sphericity_old(nz_max) 
      real A1,A2,ro_water,ro_ice,dt,Tf

      if (JJ.gt.0) then

        do j=1,JJ
          sweql(j) = ro_layer(j) / ro_water * dy_snow(j)
        enddo

        do jjj=1,JJ
          sweqstar(jjj) = sweql(jjj) / 2.0
          do j=jjj+1,JJ
            sweqstar(jjj) = sweqstar(jjj) + sweql(j)
          enddo
        enddo

        do j=1,JJ
          ro_layer(j) = ro_layer(j) + dt * (A1 * sweqstar(j) *
     &      ro_layer(j) *
     &      exp(-0.08*(Tf-T_old(j))) * exp(-A2*ro_layer(j)))
          ro_layer(j) = min(ro_ice,ro_layer(j))
          dy_snow(j) = sweql(j) * ro_water / ro_layer(j)
        enddo

      endif 
      if (microstructure_flag .eq. 1) then 
c MAYBE JUST HAVE THIS BECAUSE OTHER CALCULATIONS ARE CONFUSING
c Equations represent changes viscosity, which ultimately affects
c density, or just density itself as a function of microstructure. 
c if T07 or F06 are the routines used, use this method 
c       if ((T07_flag .eq. 1) .or. (F06_flag .eq. 1)) then 
c         do j=1,JJ
c           ro_snow_2(j) = ro_layer(j) + (ro_layer(j) * (-6.6e-3  
c    &       * ssa(j) * (9.81 * COS(terrain_slope) 
c    &       * mass_accumulation(j))**0.18))
c           dy_snow(j) = dy_snow(j)* (ro_layer(j)/ro_snow_2(j))
c           ro_layer(j) = ro_snow_2(j)
c         enddo 
c       endif  
c if B92 or C13 are the routines used, use this method 
c       if ((B92_flag .eq. 1) .or. (C13_flag .eq. 1)) then
c         do j=1,JJ
c           dendricity_old(j) = dendricity (j) 
c           sphericity_old(j) = sphericity (j) 
c           dendricity(j) = dendricity(j) / (-99)
c           sphericity(j) = sphericity(j) / 99 
c           if (liqfrac(j) .gt. 1e-8) then 
c             viscosity(j) = viscosity(j) / (1.0 + 60.0 * 
c    &        lifrac(j) / dy_snow(j))
c           endif 
c           if ((liqfrac(j) .le. 0.1) .and. (history(j) .ge. 2.0)) 
c    &      then
c             viscosity(j) = viscosity(j) * 10.0 
c           endif 
c           if (history(j) .eq. 1) then 
c            if ((dendricity(j) .ge. 0.0) .and. (dendricity(j) .le. 
c    &        0.51)) then 
c              viscosity(j) = viscosity(j) * MIN(4.0, EXP(MIN(4e-4,
c    &          sphericity(j) - 2e-4) / 1e-4)) 
c             endif 
c             if ((dendricity(j) .ge. (1.0e-4 * (4.0 - 
c    &        sphericity(j)))) .and. (sphericity(j) .le. 0.51)) then
c               viscosity(j) = viscosity(j) * MIN(4.0, EXP(MIN(4e-4, 
c    &          (1e-4 * (4 - sphericity(j))) - 2e-4) / 1e-4))
c             endif
c           endif
c           ro_snow_2(j) = ro_layer(j) + ro_layer(j) * dt * 
c    &      (9.81 * COS(terrain_slope) * mass_accumulation(j) /
c    &      viscosity(j)) 
c           dy_snow(j) = dy_snow(j)* (ro_layer(j)/ro_snow_2(j))
c           ro_layer(j) = ro_snow_2(j)  
c           dendricity(j) = dendricity_old(j)
c           sphericity(j) = sphericity_old(j) 
c         enddo 
c       endif 
      endif 
      

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE SNOWTEMP_ML(gamma,T_old,Tsfc,JJ,dt,ro_layer,Cp_snow,
     &  Tf,dy_snow,melt_flag)

      implicit none

      include 'snowmodel.inc'

      real gamma(nz_max)
      real ro_layer(nz_max)
      real dy_snow(nz_max)
      real g_b_ns(nz_max+1)
      real f_n(nz_max+1)
      real aN(nz_max)
      real aP0(nz_max)
      real aS(nz_max)
      real dely_p(nz_max+1)
      real dy_p(nz_max)
      real y_crds(nz_max+2)
      real y_wall(nz_max+1)
      real A_sub(nz_max)
      real A_super(nz_max)
      real A_main(nz_max)
      real b_vector(nz_max)
      real T_old(nz_max)
      real Sc(nz_max)
      real Sp(nz_max)

      integer melt_flag(nz_max)

      integer j,JJ
      real Tsfc,T_N,bc_N,bc_S,Cp_snow,Tf,dt,Tsg

c Define the snow thermal conductivity (gamma) for each layer.
      CALL GETGAMMA(JJ,ro_layer,gamma)

      if (JJ.gt.1) then

c Update the control volume information.
        CALL GETCV(JJ,dy_p,dy_snow)
        CALL CV_INFO(dely_p,f_n,y_crds,y_wall,dy_p,JJ)

c Compute the general equation coefficients.
        CALL GAMMA1(g_b_ns,gamma,f_n,JJ)
        CALL GE_COEF(aN,aS,aP0,dy_p,dely_p,g_b_ns,dt,JJ,
     &    ro_layer,Cp_snow)

c---------------------------------------------------------------------
c---------------------------------------------------------------------
c Account for the boundary conditions.
c   South boundary condition:
c     For T_S = known, define 
c       bc_S = aS(1) * T_S;         where T_S = known
c     For dT_S/dn = 0, define
c       bc_S = 0.0
c       aS(1) = 0.0
c   North boundary condition:
c     For T_N = known, define 
c       bc_N = aN(JJ) * T_N;        where T_N = known
c     For dT_N/dn = 0, define
c       bc_N = 0.0
c       aN(JJ) = 0.0
c---------------------------------------------------------------------
c---------------------------------------------------------------------

c Define the upper and lower boundary conditions.
        T_N = Tsfc
        bc_N = aN(JJ) * T_N
        bc_S = 0.0
        aS(1) = 0.0

c Provide the source terms.

c Force the source terms to produce Tf at the positions where melting
c   occurred during this time step.
        do j=1,JJ
          if (melt_flag(j).eq.1) then
            Sc(j) = 10e30 * Tf
            Sp(j) = -10e30
          else
            Sc(j) = 0.0
            Sp(j) = 0.0
          endif
        enddo

c Configure the information for the matrix solver.
        CALL PREPSOLVE(A_sub,A_super,A_main,b_vector,T_old,
     &    dy_p,bc_S,bc_N,Sc,Sp,aN,aS,aP0,JJ)

c Solve the system of equations.
        CALL TRISOLVE(T_old,A_sub,A_main,A_super,b_vector,JJ)

      elseif (JJ.eq.1) then
c Assume that the snow-ground interface temperature is -1.0 C.
c J.PFLUG
c changed for melt-out conditions
        if (melt_flag(1).eq.1) then
          T_old(1) = Tf
        else
          Tsg = Tf - 1.0
          T_old(1) = 0.5 * (Tsg + Tsfc)
        endif
c END J.PFLUG
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE GETCV(JJ,dy_p,dy_snow)

      implicit none

      include 'snowmodel.inc'

      real dy_p(nz_max)
      real dy_snow(nz_max)

      integer j,JJ

c Provide values of Control Volume size in the y direction.
      do j=1,JJ
        dy_p(j) = dy_snow(j)
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE PREPSOLVE(A_sub,A_super,A_main,b_vector,T_old,
     &  dy_p,bc_S,bc_N,Sc,Sp,aN,aS,aP0,JJ)

      implicit none

      include 'snowmodel.inc'

      real aP(nz_max)
      real aN(nz_max)
      real aS(nz_max)
      real Sp(nz_max)
      real Sc(nz_max)
      real aP0(nz_max)
      real dy_p(nz_max)
      real T_old(nz_max)
      real b_vector(nz_max)
      real A_sub(nz_max)
      real A_super(nz_max)
      real A_main(nz_max)

      integer j,jj
      real bc_S,bc_N

c Compute matrix diagonal and b coeffs.
      do j=1,JJ
        aP(j) = aN(j) + aS(j) + aP0(j) - Sp(j) * dy_p(j)
        b_vector(j) = Sc(j) * dy_p(j) + aP0(j) * T_old(j)
      enddo

c Modify b to account for dirichlet boundary conditions.
      b_vector(1) = b_vector(1) + bc_S
      b_vector(JJ) = b_vector(JJ) + bc_N

c Prepare to call the tridiagonal solver.
      do j=1,JJ-1
        A_sub(j) = - aS(j+1)
        A_super(j) = - aN(j)
      enddo

      do j=1,JJ
        A_main(j) = aP(j)
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE CV_INFO(dely_p,f_n,y_crds,y_wall,dy_p,JJ)

      implicit none

      include 'snowmodel.inc'

      real dy_pbc(nz_max+2)
      real dely_p(nz_max+1)
      real f_n(nz_max+1)
      real dy_p(nz_max)
      real y_crds(nz_max+2)
      real y_wall(nz_max+1)

      integer j,JJ
      real temp

c PRESSURE CONTROL VOLUME SIZE AND POSITION INFORMATION

c Include exterior boundary pressure grid points.
      dy_pbc(1) = 0.0
      do j=2,JJ+1
        dy_pbc(j) = dy_p(j-1)
      enddo
      dy_pbc(JJ+2) = 0.0

c Compute the distance between pressure grid points.
      do j=1,JJ+1
        dely_p(j) = .5 * (dy_pbc(j) + dy_pbc(j+1))
      enddo

c Compute the distance between the pressure grid points and the control
c   volume wall.  (The following is true because the grid points do
c   pressure are defined to be in the center of the control volume.)
c   And then compute f_e and f_n.  These two steps are combined below.
      do j=1,JJ+1
        f_n(j) = .5 * dy_pbc(j+1) / dely_p(j)
      enddo

c Compute the x and y coordinates of the pressure c.v. grid points,
c   including boundaries.
      temp = 0.0
      do j=1,JJ+2
        y_crds(j) = temp + .5 * dy_pbc(j)
        temp = temp + dy_pbc(j)
      enddo

c Compute the x and y coordinates of the pressure c.v. walls.
      y_wall(1) = 0.0
      do j=2,JJ+1
        y_wall(j) = y_wall(j-1) + dy_p(j-1)
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE GAMMA1(g_b_ns,gamma,f_n,JJ)

      implicit none

      include 'snowmodel.inc'

      real g_b_ns(nz_max+1)
      real gamma(nz_max)
      real g_ns(nz_max+2)
      real f_n(nz_max+1)

      integer j,JJ

c This provides gamma information on c.v. walls.

c Include gamma just outside of n, s boundaries.
      g_ns(1) = gamma(1)
      do j=2,JJ+1
        g_ns(j) = gamma(j-1)
      enddo
      g_ns(JJ+2) = gamma(JJ)

c Compute gamma (diffusion coefficient) at the n, s control
c   volume boundaries using equation 4.9, p. 45.
      do j=1,JJ+1
        g_b_ns(j) = 1.0/((1.0 - f_n(j))/g_ns(j) + f_n(j)/g_ns(j+1))
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE GE_COEF(aN,aS,aP0,dy_p,dely_p,g_b_ns,dt,JJ,
     &  ro_layer,Cp_snow)

      implicit none

      include 'snowmodel.inc'

      real aN(nz_max)
      real aS(nz_max)
      real aP0(nz_max)
      real dely_p(nz_max+1)
      real g_b_ns(nz_max+1)
      real dy_p(nz_max)
      real ro_layer(nz_max)

      integer j,JJ
      real Cp_snow,dt

c CALCULATE THE COEFFICIENTS aP, for the general phi equation.
      do j=2,JJ+1
        aN(j-1) = g_b_ns(j)   / dely_p(j)
        aS(j-1) = g_b_ns(j-1) / dely_p(j-1)
      enddo

      do j=1,JJ
        aP0(j) = ro_layer(j) * Cp_snow * dy_p(j) / dt
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE TRISOLVE(x,asub,amain,asuper,b,JJ)

      implicit none

      include 'snowmodel.inc'

      real asub(nz_max)
      real asuper(nz_max)
      real amain(nz_max)
      real b(nz_max)
      real x(nz_max)
      real z(nz_max)
      real lmain(nz_max)
      real lsub(nz_max)
      real usuper(nz_max)

      integer j,JJ

      lmain(1) = amain(1)
      usuper(1) = asuper(1)/lmain(1)

      do j=2,JJ-1
        lsub(j-1) = asub(j-1)
        lmain(j) = amain(j) - lsub(j-1) * usuper(j-1)
        usuper(j) = asuper(j) / lmain(j)
      enddo

      lsub(JJ-1) = asub(JJ-1)
      lmain(JJ) = amain(JJ) - lsub(JJ-1) * usuper(JJ-1)
      z(1) = b(1) / lmain(1)

      do j=2,JJ
        z(j) = 1.0 / lmain(j) * (b(j) - lsub(j-1) * z(j-1))
      enddo

      x(JJ) = z(JJ)

      do j=JJ-1,1,-1
        x(j) = z(j) - usuper(j) * x(j+1)
      enddo

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE ZERO_SNOW(nx,ny,snow_depth,ro_snow_grid,ro_snow,
     &  swe_depth,swe_depth_old,canopy_int_old,JJ,sum_swemelt,
     &  tslsnowfall,dy_snow,swe_lyr,ro_layer,T_old,sum_sprec,
     &  multilayer_snowpack,tsls_threshold,d_opt,grain_sz,ssa,
     &  dendricity,sphericity,zi,snow_age,history,
     &  dangl,microstructure_flag,viscosity)

      implicit none

      include 'snowmodel.inc'

      integer nx,ny,i,j,k

      integer multilayer_snowpack
      integer JJ(nx_max,ny_max)

      real tsls_threshold,ro_snow
      real tslsnowfall(nx_max,ny_max)
      real dy_snow(nx_max,ny_max,nz_max)
      real swe_lyr(nx_max,ny_max,nz_max)
      real ro_layer(nx_max,ny_max,nz_max)
      real T_old(nx_max,ny_max,nz_max)
      real swe_depth_old(nx_max,ny_max)
      real canopy_int_old(nx_max,ny_max)
      real swe_depth(nx_max,ny_max)
      real snow_depth(nx_max,ny_max)
      real ro_snow_grid(nx_max,ny_max)
      real sum_sprec(nx_max,ny_max)
      real sum_swemelt(nx_max,ny_max)
      real d_opt(nx_max,ny_max,nz_max) 
      real grain_sz(nx_max,ny_max,nz_max)
      real ssa(nx_max,ny_max,nz_max)
      real dendricity(nx_max,ny_max,nz_max)
      real sphericity(nx_max,ny_max,nz_max)
      real zi(nx_max,ny_max,nz_max)
      real snow_age(nx_max,ny_max,nz_max)
      real history(nx_max,ny_max,nz_max)
      real dangl(nx_max,ny_max,nz_max)
      real viscosity(nx_max,ny_max,nz_max)
      integer microstructure_flag
      
      

      print *,'ZEROING OUT THE SNOW ARRAYS'
      print *,'ZEROING OUT THE SNOW ARRAYS'
      print *,'ZEROING OUT THE SNOW ARRAYS'

      do j=1,ny
        do i=1,nx
          canopy_int_old(i,j) = 0.0
          swe_depth_old(i,j) = 0.0
          snow_depth(i,j) = 0.0
          ro_snow_grid(i,j) = ro_snow
          swe_depth(i,j) = 0.0
          sum_sprec(i,j) = 0.0
          sum_swemelt(i,j) = 0.0
        enddo
      enddo

      if (multilayer_snowpack.eq.1) then
        do j=1,ny
          do i=1,nx
            tslsnowfall(i,j) = tsls_threshold
            do k=1,JJ(i,j)
              dy_snow(i,j,k) = 0.0
              swe_lyr(i,j,k) = 0.0
              ro_layer(i,j,k) = ro_snow
              T_old(i,j,k) = 273.16
              if (microstructure_flag .eq. 1) then 
                d_opt(i,j,k) = 0.0
                grain_sz(i,j,k) = 0.0
                ssa(i,j,k) = 0.0
                dendricity(i,j,k) = 0.0
                sphericity(i,j,k) = 0.0
                zi(i,j,k) = 0.0
                snow_age(i,j,k) = 0.0
                history(i,j,k) = 0.0
                dangl(i,j,k) = 0.0
                viscosity(i,j,k) = 0.0
              endif
  
            enddo
            JJ(i,j) = 0
          enddo
        enddo
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE ZERO_SEAICE_SNOW(nx,ny,snow_depth,ro_snow_grid,
     &  ro_snow,swe_depth,swe_depth_old,canopy_int_old,JJ,
     &  tslsnowfall,dy_snow,swe_lyr,ro_layer,T_old,
     &  multilayer_snowpack,tsls_threshold,seaice_conc)

      implicit none

      include 'snowmodel.inc'

      integer nx,ny,i,j,k

      integer multilayer_snowpack
      integer JJ(nx_max,ny_max)

      real tsls_threshold,ro_snow
      real tslsnowfall(nx_max,ny_max)
      real dy_snow(nx_max,ny_max,nz_max)
      real swe_lyr(nx_max,ny_max,nz_max)
      real ro_layer(nx_max,ny_max,nz_max)
      real T_old(nx_max,ny_max,nz_max)
      real swe_depth_old(nx_max,ny_max)
      real canopy_int_old(nx_max,ny_max)
      real swe_depth(nx_max,ny_max)
      real snow_depth(nx_max,ny_max)
      real ro_snow_grid(nx_max,ny_max)
      real seaice_conc(nx_max,ny_max)

      do j=1,ny
        do i=1,nx
          if (seaice_conc(i,j).eq.0.0) then
            canopy_int_old(i,j) = 0.0
            swe_depth_old(i,j) = 0.0
            snow_depth(i,j) = 0.0
            ro_snow_grid(i,j) = ro_snow
            swe_depth(i,j) = 0.0
          endif
        enddo
      enddo

      if (multilayer_snowpack.eq.1) then
        do j=1,ny
          do i=1,nx
            if (seaice_conc(i,j).eq.0.0) then
              tslsnowfall(i,j) = tsls_threshold
              do k=1,JJ(i,j)
                dy_snow(i,j,k) = 0.0
                swe_lyr(i,j,k) = 0.0
                ro_layer(i,j,k) = ro_snow
                T_old(i,j,k) = 273.16
              enddo
              JJ(i,j) = 0
            endif
          enddo
        enddo
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE SNOWLIQFLX(dt,mLayerVolFracLiqTrial,
     &  liqfrac,icefrac,liqflux,
     &  mLayerThetaResid)

      implicit none

c public variables

      real mLayerVolFracLiqTrial,liqfrac,icefrac,liqflux,dt

      real pore_space,availCap,relSaturn,iden_ice,mLayerThetaResid,
     &  k_snow,mult,mw

      parameter (iden_ice = 917)

c compute the pore space. Note that water can fill some of this
c pore space
        pore_space = 1.0 - icefrac
       
c can also choose to define here if not expliclty resolving
c or being defined in UPDATE_STATE
c residual water content
c        mLayerThetaResid = 0.02

c compute fluxes
c check that flow occurs
        if(mLayerVolFracLiqTrial.gt.mLayerThetaResid)then

c compute the available capacity
          availCap = pore_space - mLayerThetaResid

c compute the saturated hydraulic conductivity
c method adapted from Colbeck (1972).
          k_snow = (3.41875e-5)*exp(15.9*(1-icefrac))
c           mult = x
c           k_snow = (mult)*exp(15.9*(1-icefrac))

c if grain size is calculated/known, use Shimizu (1970) 
c relationship
c          k_snow = (0.00077)*(grain_sz**2)*(9.8/1.787)*
c     &      exp(-0.0078*mLayerTheta*1000)
         
c compute the relative saturation
          if (availCap.gt.0.0) then
            relSaturn = (mLayerVolFracLiqTrial -
     &        mLayerThetaResid) / availCap
          else
            relSaturn = 1.0
          endif

c calculate the flux out of the snowpack or layer
c           liqflux = dt*k_snow*relSaturn**3.0
           mw = 3.0
           liqflux = dt*k_snow*relSaturn**mw
c if grain size is known, use the van Genuchten equation
c           n = (15.68*exp(-0.46*2*grain_sz))+1
c           m = 1 - (1/n)
c           liqflux = dt*k_snow*(relSaturn**0.5)*
c     &       ((1-((1-(relSaturn**(1/m)))**m))**2)
        else
            
c flow does not occur
          liqflux = 0.0

        endif
c       print *, relSaturn  
c       print *, mLayerVolFracLiqTrial
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE UPDATE_STATE(Tk,mLayerTheta,mLayerVolFracLiq,
     &  mLayerVolFracIce,fc_param,Tf,ro_water,
     &  retent)

      implicit none

      real fracliquid,fc_param,Tf,Tk,mLayerVolFracLiq,
     &  mLayerVolFracIce,iden_ice,mLayerTheta,
     &  ro_water,retent

      parameter (iden_ice = 917)

c fraction of liquid water
      fracliquid = 1.0/(1.0 + (fc_param*(Tf -
     &  min(Tk,Tf)))**2.0)

c calculate volumetric fractions of liquid and ice content
      mLayerVolFracLiq = fracliquid*mLayerTheta
      mLayerVolFracIce = (1.0 - fracliquid)*mLayerTheta*
     &  (ro_water/iden_ice)

c calculate the retention
c     use this if wanting to explicitly track through time
c     will likely need to output to snowmodel_main to track evolution
c      retent = min(0.02,max(retent,0.75*mLayerVolFracLiq))
c     else use parameterized value
      retent = 0.02
c     print *, fracliquid 
c     print *,'0', mLayerVolFracLiq
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE MICROSTRUCTURE(liqfrac,microstructure_flag,
     &  windspd,temp_grad,d_opt,grain_sz,ssa,dendricity,
     &  sphericity,t07_flag,f06_flag,b92_flag,c13_flag,
     &  zi,snow_age,ro_layer,T_old,Tf,history,dy_snow,dt,JJ,dangl,
     &  iter,snthrm_flag,sfc_pressure)
      
      implicit none 
     
    
      
      include 'snowmodel.inc'
      !sw 
      !real fracliquid(nz_max)
      real liqfrac(nz_max)
      real d_opt(nz_max)
      real grain_sz(nz_max)
      real ssa(nz_max)
      real dendricity(nz_max)
      real sphericity(nz_max) 
      real zi(nz_max)
      real snow_age(nz_max)
      real history(nz_max)
      real temp_grad(nz_max) 
      integer t07_flag
      integer f06_flag
      integer b92_flag
      integer c13_flag
      integer snthrm_flag
      integer microstructure_flag
      real windspd
      real dy_snow(nz_max)
      real ro_layer(nz_max) 
      real T_old(nz_max)
      real dangl(nz_max)
      real sfc_pressure
      
      integer itt 
      integer maxx 
      parameter (maxx =2728)
      real idtemp(maxx)
      real idgrad(maxx)
      real idrho(maxx) 
      real drdt0(maxx) 
      real kappa(maxx) 
      real tau(maxx) 

      real Tf,dt
      integer JJ
      integer iter 
c     print *,d_opt
c      print *, dendricity  
      if (b92_flag .eq. 1) then 
        !print *, d_opt
        call B92(Tf,T_old,dendricity,sphericity,
     &  snow_age,liqfrac,temp_grad,JJ,
     &  ro_layer,grain_sz,dy_snow,dt,dangl,history,
     &  d_opt)  
      endif 

      if (c13_flag .eq. 1) then 
        call C13(Tf,T_old,sphericity,d_opt,ssa,liqfrac,
     &  temp_grad,dt,snow_age,history,JJ,ro_layer,dy_snow,
     &  dangl) 
      endif 
      
      if(f06_flag .eq. 1 .and. iter .eq. 1) then 
      open(unit=15, file='all.txt')
c	  Read in the file to individual column vector
        do itt = 1, maxx
          read(15,*) idtemp(itt), idgrad(itt), idrho(itt), 
     &     drdt0(itt), kappa(itt), tau(itt) 
        enddo
  200   FORMAT(F5.2,I2,I3,F6.5,F6.3,E20.10) 
      close(unit=15)
      endif 
      if(f06_flag .eq. 1) then 
        call F06(d_opt,ssa,liqfrac,temp_grad,T_old,dt,JJ,
     &  ro_layer,Tf,snow_age,idtemp(maxx),idgrad(maxx),idrho(maxx), 
     &  drdt0(maxx),kappa(maxx),tau(maxx))
       endif 
      
      if(t07_flag .eq. 1) then 
        call T07(d_opt,ssa,liqfrac,temp_grad,T_old,dt,snow_age,
     &   JJ,Tf)
      endif 
      
      if (snthrm_flag .eq. 1) then 
        call SNTHRM(dt,ro_layer,T_old,Tf,liqfrac,grain_sz,JJ,
     &  sfc_pressure,ssa)
      endif 
      
      return
      end 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc	  
      SUBROUTINE B92(Tf,T_old,dendricity,sphericity,
     &  snow_age,liqfrac,temp_grad,JJ,ro_layer,grain_sz,
     &  dy_snow,dt,dangl,history,d_opt)
      
      implicit none 
          
      include 'snowmodel.inc'
      !sw 
      real liqfrac(nz_max)
      real d_opt(nz_max)
      real grain_sz(nz_max)
      real ssa(nz_max)
      real dendricity(nz_max)
      real sphericity(nz_max)
      real viscosity(nz_max)
      real snow_age(nz_max)
      real temp_grad(nz_max) 
      real dy_snow(nz_max)
      real ro_layer(nz_max) 
      real delta_dend1(nz_max)
      real delta_spher1(nz_max)
      real delta_dend(nz_max)
      real delta_spher(nz_max)
      real delta_grain(nz_max)
      real T_old(nz_max) 
      real vapor_pressure(nz_max)
      real zvsphe(nz_max)
      real zcoeff_sph(nz_max)
      real history(nz_max)
      real dangl(nz_max)
      real Tf 
      real f,g,h,phi 
      real dt 
      real pi 
      parameter (pi = 3.14) 
      integer JJ,j
      
!      print *, 'b92',dendricity 

c Dry snow metamorphism, B92 Method 
      do j=1,JJ
c       print *, d_opt(j) 
        if (liqfrac(j) .gt. 1e-8) then 
        !Wet Snow Metamorphism 
        !Rate of Dendricity Change
          delta_dend1(j) = MAX((7.2338E-7 * liqfrac(j)**3), 
     &    2314.81481 * EXP(-6000 / 273.15)) 
        !Rate of Sphericity Change
          delta_spher1(j) = delta_dend(j)
          zvsphe(j) = 1 
          zcoeff_sph(j) = 2 
c         print *,'is this why?' 
        elseif (temp_grad(j) .lt. 5) then
          !Dry Snow Metamorphism
          !Low Gradient Metamorphism
          vapor_pressure(j) = EXP(-6000 / T_old(j))
          !Rate of Dendricity Change
          delta_dend1(j) = 2314.81481 * vapor_pressure(j) 
          delta_spher1(j) = 11574.074 * vapor_pressure(j)
          zvsphe(j) = 1 
          zcoeff_sph(j) = 2
        else 
         !Dry Snow Metamorphism
         !High Gradient Metamorphism
         vapor_pressure(j) = EXP(-6000 / T_old(j)) * temp_grad(j)
     &   **0.4 
         !Rate of Decrease Decrease
         delta_dend1(j) = 2314.81481 * vapor_pressure(j)
         !Rate of Sphericity Change
         delta_spher1(j) = -2314.81481 * vapor_pressure(j)
         zvsphe(j) = 1E20 
         zcoeff_sph(j) = 3 
        endif 

        if (dendricity(j) .lt. -1e-8) then 
          !Dendritic Case 
          delta_dend(j) = -dendricity(j) / 99 - delta_dend1(j) * dt 
          delta_spher(j) = sphericity(j) / 99 + delta_spher1(j) * dt 
         !call SET_THRESH(temp_grad(j),liqfrac(j),delta_spher(j))
         !call SET_THRESH(temp_grad,liqfrac,delta_spher)
          if ((liqfrac(j) .gt. 1e-8) .or. (temp_grad(j).le.5)) then
            delta_spher(j) = MIN(1.0,delta_spher(j))
          else
            delta_spher(j) = MAX(0.0,delta_spher(j)) 
          endif
          if (delta_dend(j) .le. 1e-8) then 
            sphericity(j) = delta_spher(j) * 99 
            grain_sz(j) = 4e-4 - 1e-4 * MIN (delta_spher(j), 
     &      zvsphe(j))
          else 
            dendricity(j) = -delta_dend(j) * 99 
            sphericity(j) = delta_spher(j) * 99 
          endif 
        elseif((history(j) .ne. 1) .or. (grain_sz(j) .lt. 5e-4)) then
          !Non-dendrtic and not completely spheric 
          delta_spher(j) = sphericity(j) / 99 + delta_spher1(j)* dt 
         !call SET_THRESH(temp_grad(j),liqfrac(j),delta_spher(j))
         !call SET_THRESH(temp_grad,liqfrac,delta_spher) 
          if ((liqfrac(j) .gt. 1e-8) .or. (temp_grad(j).le.5)) then
            delta_spher(j) = MIN(1.0,delta_spher(j))
c           print *,'here1'
          else
            delta_spher(j) = MAX(0.0,delta_spher(j)) 
          endif
          sphericity(j) = delta_spher(j) * 99 
c         print *, 'here3'
        elseif(liqfrac(j) .gt. 1e-8) then
          !Wet snow evolution of grain size 		
          grain_sz(j) = 2.0 * (3.0 / (4.0 * pi) * (4.0 * pi /
     &    3.0 * (grain_sz(j) / 2.0)**3.0 + (1.28e-17 + 
     &    4.22e-19 * liqfrac(j)**3.0) *dt)) * 
     &    (1.0/3.0)
        elseif((history(j) .eq. 2) .or. (history(j) .eq. 3)) then 
         !case where history is 2 or 3 
          delta_spher(j) = sphericity(j) / 99 + delta_spher1(j) * dt * 
     &    EXP(MIN(0.0, ((3e-4 - grain_sz(j)) / 1e-4)))
          delta_spher(j) = MIN(delta_spher(j), 0.5) 
         !call SET_THRESH(temp_grad(j),liqfrac(j),delta_spher(j))
         !call SET_THRESH(temp_grad,liqfrac,delta_spher)
          if ((liqfrac(j) .gt. 1e-8) .or. (temp_grad(j).le.5)) then
            delta_spher(j) = MIN(1.0,delta_spher(j))
          else
            delta_spher(j) = MAX(0.0,delta_spher(j)) 
          endif
          sphericity(j) = delta_spher(j) * 99 
        else 
         !non-dendrtic and sphericity = 0 
         call MARBOUTY(ro_layer(j),T_old(j),temp_grad(j),dangl(j))
         grain_sz(j) = grain_sz(j) + dangl(j) * 1.0417E-9 * dt 
        endif
        !print *, sphericity(j) 
        if (dendricity(j) .lt. -99) then 
          dendricity(j) = -99
        elseif (dendricity(j) .gt. 0) then 
          dendricity(j) = 0 
        endif
        if (sphericity(j) .gt. 99) then 
          sphericity(j) = 99 
        elseif (sphericity(j) .lt. -10) then 
          sphericity(j) = 99
        elseif (sphericity(j) .lt. 0 .and. sphericity(j) 
     &   .gt. -10) then 
          sphericity(j) = 0
       
        elseif (sphericity(j) .gt. 99) then 
          sphericity(j) = 99 
        endif 
        !print *, dendricity(j)
        !print *, sphericity(j) 
        if (dendricity(j) .lt. -1) then 
          d_opt(j) = 1e-4 * (((-dendricity(j) / 99) * 
     &   (sphericity(j) / 99 - 3 )) - sphericity(j) / 99 + 4)
c         print *, 'here'
        else 
          d_opt(j) = grain_sz(j) * sphericity(j)/99 + MAX(0.0004,
     &    0.5 * grain_sz(j)) * (1-sphericity(j)/99) 
c         print *, 'here1' 
        endif 
        
        ssa(j) = (6 / (d_opt(j) * 917.0)) 
c       print*, ssa(j)  
        snow_age(j) = snow_age(j) + dt/3600
c       print *,d_opt(j) 

c only non-dendritic crystals can have a history above 0 
        if (d_opt(j) .ge. (1e-4 * (4 - (sphericity(j)/99)))) then 
          if (liqfrac(j) .le. 1e-8) then
            if (((sphericity(j)/99) .le. 0.1) .and. (history(j) .eq. 0)) 
     &      then 
              history(j) = 1 
            endif 
          !should be this, but I'm going to put a different one in 	
c	  	  elseif (((liqfrac(j)/dy_snow(j)) .gt. 0.005) .and. 
          !!!!FIX LINE BELOW! IT's NOT RIGHT !
c	 &	  ((1-sphericity(j) .lt. 0.1/99))
c    &    then 
          elseif ((liqfrac(j) .gt. 1e-8) .and. 
     &    ((1-sphericity(j)/99) .le. 0.1))then 
            if (history(j) .eq. 0) then 
              history(j) = 2  
            elseif (history(j) .eq. 1) then
              history(j) = 2 
            elseif (history(j) .eq. 2) then 
              history(j) = 3
            elseif (history(j) .eq. 3) then 
              history(j) = 4
            elseif (history(j) .eq. 4) then 
              history(j) = 5
            endif
          endif
        endif
c       print *, ssa(j) 
      enddo
c     print *, JJ 
      return 
      end 
      
      SUBROUTINE MARBOUTY(ro_layer,T_old,temp_grad,dangl)   
        implicit none 
        real ro_layer
        real T_old  
        real temp_grad
        real dangl 
        real f,g,h 
        real phi 
        
        phi = 1.0417*10**-9 

c       if( T_old .ge. 273.15 - 40) then
c         if ( T_old .ge. 273.15- 6) then
c           dangl = 0.7 + 0.3 * (273.15 - T_old) / 6
c         elseif( T_old .ge. 273.15 - 22 ) then
c           dangl = 1 - 0.8 * (273.15 - 6 - T_old) / 16 
c         else
c           dangl = 0.2 - 0.2 * (273.15 - 22 - T_old) / 18 
c         endif
c        !DENSITY INFLUENCE.
c         if ( ro_layer .le. 400 ) then
c           if ( ro_layer .gt. 150 ) then
c             dangl = dangl * ( 1. - (ro_layer-150)/(400-150))
c           endif
c          !TEMPERATURE GRADIENT INFLUENCE.
c           if ( temp_grad .le. 70 ) then
c             if ( temp_grad .le. 25 ) then
c               dangl = dangl * 0.1 * (temp_grad-15)/(25-15)
c             elseif( temp_grad .le. 40 ) then
c               dangl = dangl * ( 0.1 + 0.55 * (temp_grad-25)/(40-25) )
c             elseif( temp_grad .le. 50 )then
c               dangl = dangl * ( 0.65 + 0.2 * (temp_grad-40)/(50-40) )
c             else
c               dangl = dangl * ( 0.85 + 0.15 * (temp_grad-50)/(70-50) )
c             endif
c           endif
c         else
c           dangl = 0
c         endif
c       else
c         dangl = 0
c       endif
!!!!!!!!!!!!!!!!!!!!!! NEW MARBOUTY         
          if (T_old .lt. 273.16-40) then
            f = 0 
          elseif ((T_old .lt. (273.16 - 22)) .and. 
     &   (T_old .ge. 273.16-40))then 
            f = 0.011 * (T_old-273.16+40) 
          elseif ((T_old .lt. 273.16 - 6) .and. 
     &   (T_old .ge. 273.16-22))then 
            f = 0.2 + 0.005 * (T_old-273.16+22) 
          else 
            f = 1 - 0.05 * (T_old - 273.16) 
          endif 
      
          if (ro_layer .lt. 150) then 
            h = 1 
          elseif ((ro_layer .lt. 400) .and.
     &  (ro_layer .gt. 150)) then 
            h = 1 - 0.004 * (ro_layer - 150) 
          else 
            h = 0 
          endif 
      
          if (temp_grad .lt. 15) then 
            g = 0
          elseif ((temp_grad .lt. 25) .and. (temp_grad .ge.15)) then 
            g = 0.01 * (temp_grad - 15)     
          elseif ((temp_grad .lt. 40) .and. (temp_grad .ge.25)) then 
            g = 0.1 + 0.037 * (temp_grad - 25)     
          elseif ((temp_grad .lt. 50) .and. (temp_grad .ge.40)) then 
            g = 0.65 + 0.02 * (temp_grad - 40)     
          elseif ((temp_grad .lt. 70) .and. (temp_grad .ge.50)) then 
            g = 0.85 + 0.0075 * (temp_grad - 50)     
          else 
            g = 1 
          endif 
      
          dangl = phi * f * h * g 
          return
          end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc	  
      SUBROUTINE C13(Tf,T_old,sphericity,d_opt,ssa,liqfrac,
     &  temp_grad,dt,snow_age,history,JJ,ro_layer,dy_snow,
     &  dangl) 
      
      implicit none 
     
      include 'snowmodel.inc'
      !sw 
      real liqfrac(nz_max)
      real d_opt(nz_max)
      real ssa(nz_max)
      real sphericity(nz_max) 
      real snow_age(nz_max)
      real history(nz_max)
      real temp_grad(nz_max)
      real T_old(nz_max)
      real dy_snow(nz_max)
      real ro_layer(nz_max) 
      real delta_spher(nz_max)
      real delta_dopt(nz_max) 
      real delta_dopt1(nz_max)
      real delta_spher1(nz_max) 
      real vapor_pressure(nz_max)
      real zvsphe(nz_max)
      real zcoeff_sph(nz_max)
      real dangl(nz_max)
      real ZSPHE(nz_max)
      real EXD(nz_max) 
      real dt,Tf,pi 
      parameter (pi = 3.14)
      integer JJ,j
C ORIGNINAL
c      do j=1,JJ 
c        if (liqfrac(j) .gt. 0.4) then 
c        !Wet Snow Metamorphism 
c        !Rate of Dendricity Change
c          delta_dopt1(j) = MAX((7.2338E-7 * liqfrac(j)**3),     
c     &    2314.81481 * EXP(-6000 / 273.15)) 
c        !Rate of Sphericity Change
c          delta_spher1(j) = delta_dopt1(j)
c          zvsphe(j) = 1 
c          zcoeff_sph(j) = 2
c        elseif (temp_grad(j) .lt. 5) then
c          !Dry Snow Metamorphism
c          !Low Gradient Metamorphism
c          vapor_pressure(j) = EXP(-6000 / T_old(j))
c          !Rate of Dendricity Change
c          delta_dopt1(j) = 2314.81481 * vapor_pressure(j) 
c          delta_spher1(j) = 11574.074 * vapor_pressure(j)
c          zvsphe(j) = 1 
c          zcoeff_sph(j) = 2
c        else 
c          !Dry Snow Metamorphism
c          !High Gradient Metamorphism
c          vapor_pressure(j) = EXP(-6000 / T_old(j)) * temp_grad(j)
c     &    **0.4 
c          !Rate of Decrease Decrease
c          delta_dopt1(j) = 2314.81481 * vapor_pressure(j)
c          !Rate of Sphericity Change
c          delta_spher1(j) = -2314.81481 * vapor_pressure(j)
c          zvsphe(j) = 1E20 
c          zcoeff_sph(j) = 3
c        endif 
c
c        ZSPHE(j) = sphericity(j) + delta_spher1(j) * dt
c         
c        if (liqfrac(j).gt.0.4 .OR. temp_grad(j) .lt. 5) THEN
c         ZSPHE(j) = MIN(1.,ZSPHE(j))
c        else
c         ZSPHE(j) = MAX(0.,ZSPHE(j))
c        endif 
c        if ( liqfrac(j) .lt. 0.4) then      
c          if ((d_opt(j) .lt. (1e-4*(4.-ZSPHE(j)))) .and.
c     &     (temp_grad(j) .lt. 5)) then
cc           print *, 'before', d_opt(j)
cc           print *, ZSPHE(j)
cc           d_opt(j) = d_opt(j) + 1e-8 * dt * 
cc    &       (delta_spher1(j)*(d_opt(j)/((1e-8)-1.)/
cc    &       (ZSPHE(j)-3.) - delta_dopt1(j)*(ZSPHE(j)-3.)))
c            d_opt(j) = d_opt(j) - dt * 
c     &       (delta_spher1(j)*(d_opt(j)/ 
c     &       (ZSPHE(j)-3.) - delta_dopt1(j)*(ZSPHE(j)-3.)))
c          elseif(d_opt(j) .lt. 1e-4 .and. temp_grad(j) .gt. 5) then 
c            d_opt(j) =  d_opt(j) + 1e-8 * dt * delta_dopt1(j) *
c     &       zcoeff_sph(j)
c          endif          
c        
c        elseif(d_opt (j) .gt. (1e-4*(4-ZSPHE(j)))) then
c          d_opt(j) = d_opt(j) - 1e-8 * dt * delta_spher1(j) * 2.* 
c     &   ZSPHE(j)
c        elseif(liqfrac(j) .gt. 0.4) then 
c          d_opt(j) = 2. * ( 3./(4.*3.14) * 
c     &             ( 4. * 3.14/3. * (d_opt(j)/2.)**3 + 
c     &               ( 1.28e-17 + 4.22e-19 * liqfrac(j)**3 ) * dt )
c     &                  )**(1./3.)
c          
c        elseif(liqfrac(j) .gt. 0.4  .AND. temp_grad(j) .ge. 5 ) then
c          call MARBOUTY(ro_layer(j),T_old(j),temp_grad(j),dangl(j))
c           d_opt(j) = d_opt(j) + 0.5 * dangl(j) * 1.0417E-9 * dt 
c        endif          
c        sphericity(j) = ZSPHE(j)
c        if (d_opt(j) .gt. 0.0013) then 
c          d_opt(j) = 0.0013
c        endif 
c        if (d_opt(j) .lt. 1e-4) then 
c          d_opt(j) = 1e-4
c        endif 
c! only   non-dendritic crystals can have a history above 0 
c          if (d_opt(j) .ge. (1e-4 * (4 - sphericity(j)))) then 
c            if (liqfrac(j) .le. 0) then
c              if ((sphericity(j) .le. 0.1) .and. (history(j) 
c     &         .eq. 0)) then 
c                history(j) = 1 
c              endif 
c            !should be this, but I'm going to put a different one in 	
c!       elseif (((liqfrac(j)/dy_snow(j)) .gt. 0.005) .and. 
c            !!!!FIX LINE BELOW! IT's NOT RIGHT !
c!  &      ((1-sphericity(j) .lt. 0.1/99))
c!    &      then 
c            elseif ((liqfrac(j) .gt. 0) .and. 
c     &      ((1-sphericity(j)) .le. 0.1))then 
c              if (history(j) .eq. 0) then 
c                history(j) = 2  
c              elseif (history(j) .eq. 1) then
c                history(j) = 2 
c              elseif (history(j) .eq. 2) then 
c                history(j) = 3    
c              elseif (history(j) .eq. 3) then 
c                history(j) = 4
c              elseif (history(j) .eq. 4) then 
c                history(j) = 5
c              endif
c            endif
c          endif
c        ssa(j) = 6 / (d_opt(j) * 917) 
c        snow_age(j) = snow_age(j) + dt/3600 
c      enddo 
C ORIGINAL
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc      
      do j=1,JJ 
       
        write(213,*) liqfrac(j)
        !print *, liqfrac(j)
        if( temp_grad(j) .le. 5. .and. liqfrac(j) .lt. 0.1)then
        
            EXD(j) = exp(-6000./T_old(j))
            sphericity(j) = max(0.,min(1.,sphericity(j) + 11574.074
     &         *EXD(j)*dt))
            !size = 4.*pow(10.,-4.) - 1.*pow(10.,-4.)*sphericity(j)
        
           ! dendr case
            if( d_opt(j) .lt. 10.**(-4.)*(4.-sphericity(j))-10**-6)
     &       then
                if( sphericity(j) .lt. 1.)then
                   d_opt(j) = d_opt(j) + 10.**(-4.)*dt*(-2314.81481*
     &              EXD(j)*
     &              (sphericity(j)-3.)+11574.074*EXD(j)/
     &                (sphericity(j)-3.)
     &                   *(d_opt(j)/10.**(-4)-1))
                else
                  d_opt(j) = d_opt(j) + (2*10.**(-4.)*2314.81481*
     &             EXD(j)) *dt
            ! non dendr case
                endif 
            else
                if( sphericity(j) .lt. 1.)then
                    d_opt(j) = d_opt(j) - 2.*10.**(-4.)*sphericity(j)*
     &              11574.074*EXD(j)*dt
                else
                    d_opt(j) = d_opt(j)
                endif 
            endif 
        endif 
        ! temperature gradient .gt. 5 K/m and .lt. 15 K/m
        if( temp_grad(j) .gt. 5 .and. liqfrac(j) .lt. 0.1)then
            if( temp_grad(j) .le. 15.)then
        
                EXD(j) = exp(-6000./T_old(j))*(temp_grad(j)**0.4)
                sphericity(j) = max(0.,min(1.,sphericity(j) - 
     &           2314.81481 *EXD(j)*dt))
                !size = 4.*pow(10.,-4.) - 1.*pow(10.,-4.)*sphericity(j)
        
                ! dendr case
                if( d_opt(j) .lt. 10.**(-4.)*(4.-
     &            sphericity(j))-10**-6)then
                    if( sphericity(j) .gt. 10**-6)then
                     d_opt(j) = d_opt(j) + 10.**(-4.)*dt*(-2314.81481
     &                *EXD(j)* (sphericity(j)-3.)-2314.81481*EXD(j)/(
     &                    sphericity(j)-3.)*
     &                     (d_opt(j)/10.**(-4)-1))
                    else
                        d_opt(j) = d_opt(j) + (3*10.**(-4.)
     &                    *2314.81481*EXD(j))*dt
                    endif 
       
                ! non dendr case
                else
                    if( sphericity(j) .gt. 10**-6)then
                     d_opt(j) = d_opt(j) + 2.*10.**(-4.)*
     &                  sphericity(j)*
     &                      2314.81481*EXD(j)*dt
                    else
                        d_opt(j) = d_opt(j)
                    endif 
                endif  
            endif 
        endif
        
        ! temperature gradient .gt. 15 K/m
        if( temp_grad(j) .gt. 15 .and. liqfrac(j) .lt. 0.1)then 
       
            EXD(j) = exp(-6000./T_old(j))*(temp_grad(j)**0.4)
            sphericity(j) = max(0.,min(1.,sphericity(j) - 2314.81481
     &         *EXD(j)*dt))
            !size = 4.*pow(10.,-4.) - 1.*pow(10.,-4.)*sphericity(j)
        
            ! dendr case
            if( d_opt(j) .lt. 10.**(-4.)*(4.-sphericity(j))-10**-6)
     &        then
                !size = 4.*pow(10.,-4.) - 1.*pow(10.,-4.)*sphericity(j)
                if( sphericity(j) .gt. 0.)then
                    d_opt(j) = d_opt(j) + 10.**(-4.)*dt*(-2314.81481
     &                *EXD(j)*
     &              (sphericity(j)-3.)-2314.81481*EXD(j)/
     &                 (sphericity(j)-3.)
     &                   *(d_opt(j)/10.**(-4)-1))
                else
                    d_opt(j) = d_opt(j) + (3*10.**(-4.)*2314.81481*
     &              EXD(j))*dt
                endif 
            
        
            ! non dendr case
            else
                if( sphericity(j) .gt. 0.)then
                    !size = 4.*pow(10.,-4.) - 1.*pow(10.,-4.)
                    !*sphericity(j)
                    d_opt(j) = d_opt(j) + 2.*10.**(-4.)*
     &                   sphericity(j)*2314.81481*
     &               EXD(j)*dt
        
                else
                    !size = size + size_marbouty(T_old(j),ro_layer(j)
                    !,temp_grad(j))*dt
                    !if( size .lt. 8.*10**(-4.))then
                    !d_opt(j) = 4.*10**(-4.)
                    !else)then 
                    !d_opt(j) = size/2.
                
                    call MARBOUTY(ro_layer(j),T_old(j),
     &                temp_grad(j),dangl(j))
                    d_opt(j) = d_opt(j) + 0.5*dangl(j)*dt
                endif 
            endif 
        endif 
 
        if (liqfrac(j) .ge. 0.1) then 
          !Wet Snow Metamorphism 
          !Rate of Dendricity Change
            delta_dopt1(j) = MAX((7.2338E-7 * liqfrac(j)**3),
     &         2314.81481 * EXP(-6000 / 273.15)) 
          !Rate of Sphericity Change
            delta_spher1(j) = delta_dopt1(j)
            zvsphe(j) = 1 
            zcoeff_sph(j) = 2   
            sphericity(j) = sphericity(j) + delta_spher1(j) * dt
            sphericity(j) = MIN(1.,sphericity(j))
            if (temp_grad(j) .lt. 5) then 
                 d_opt(j) = 2. * ( 3./(4.*3.14) * 
     &             ( 4. * 3.14/3. * (d_opt(j)/2.)**3 + 
     &               ( 1.28e-17 + 4.22e-19 * liqfrac(j)**3 ) * dt )
     &                  )**(1./3.)
            else 
              call MARBOUTY(ro_layer(j),T_old(j),
     &          temp_grad(j),dangl(j))
                     d_opt(j) = d_opt(j) + 0.5*dangl(j)*dt
            endif 
        endif 
 
 
       ssa(j) = 6 / (d_opt(j) * 917)
       snow_age(j) = snow_age(j) + dt/3600 
      !write(187,*) ssa(j) 
      enddo 
   
      return 
      end 
        

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     SUBROUTINE F06(d_opt,ssa,liqfrac,temp_grad,T_old,dt,JJ,    
c    &  ro_layer,Tf,snow_age) 
       SUBROUTINE F06(d_opt,ssa,liqfrac,temp_grad,T_old,dt,JJ,
     &  ro_layer,Tf,snow_age,idtemp,idgrad,idrho, 
     &  drdt0,kappa,tau)
         
      implicit none 
      include 'snowmodel.inc'
      real liqfrac(nz_max)
      real d_opt(nz_max)
      real ssa(nz_max)
      real temp_grad(nz_max)
      real ro_layer(nz_max) 
      real T_old(nz_max) 
      real snow_age(nz_max)
      real Tf 
      real dt 
      integer itt 
      integer maxx 
      parameter (maxx =2728)
      integer idgrad_var(nz_max)
      real idtemp_var(nz_max)
      integer idrho_var(nz_max)
      real drdt(nz_max)
      real drdt0_var(nz_max)
      real kappa_var(nz_max)
      real tau_var(nz_max) 
      real optr(nz_max)
      real optr0
      integer idrho(maxx)
      integer idgrad(maxx)
      real idtemp(maxx)
      real drdt0(maxx)
      real kappa(maxx)
      real tau(maxx) 
 
      real a 
      integer b 
      integer c 
      real d,e,f 
      
      integer j, JJ 
      
c     open(unit=15, file='all.txt')
c	  Read in the file to individual column vector
c       do itt = 1, maxx
c         read(15,*) idtemp(itt), idgrad(itt), idrho(itt), 
c    &     drdt0(itt), kappa(itt), tau(itt) 
c       enddo
c 200   FORMAT(F5.2,I2,I3,F6.5,F6.3,E20.10) 
c     close(unit=15)
      do j=1,JJ 
        if (liqfrac(j) .le. 0.1) then
          if (snow_age(j) .le. 2) then 
            ssa(j) = ssa(j) - (1.1e-6 + 3.1e-8 * (T_old(j) - 
     &      Tf)) * (ssa(j)**3.1) * (dt/3600) 
            d_opt(j) = 6.0 / (917.0 * ssa(j)) 
          else 
            idrho_var(j) = MIN(ABS(INT((ro_layer(j) - 25) / 50) 
     &      + 1),8)
            idrho_var(j) = idrho_var(j) * 50 
            idgrad_var(j) = MIN(ABS(INT((temp_grad(j) - 5) /  
     &      10 + 2)), 31) 
            idgrad_var(j) = (idgrad_var(j) * 10) - 10         
            idtemp_var(j) = MIN(ABS(INT((T_old(j) - 225.65) /  
     &      5 + 2)), 11) 
            if (T_old(j) .lt. 221.0) then 
              idtemp_var(j) = 223.15  
            else 
              idtemp_var(j) = idtemp_var(j) * 5.0 + 218.15 
            endif 
            optr0 = 1.0e-4 / 2.0 * 10.0**6 
            optr(j) = MAX((d_opt(j) / 2.0) * 10.0**6, optr0)  
            a = idtemp_var(j)
            b = idgrad_var(j)
            c = idrho_var(j)

            call read_file(a,b,c,d,e,f,idtemp,idgrad,idrho,itt,
     &      maxx,drdt0,kappa,tau)
      
            kappa_var(j) = d
            drdt0_var(j) = e
            tau_var(j) = f
            
            drdt(j) = drdt0_var(j) * (tau_var(j) / (optr(j) -
     &      optr0 + tau_var(j)))**(1.0 / kappa_var(j))
            optr(j) = optr(j) + drdt(j) * (dt/3600.0) 
            optr(j) = MIN(optr(j), 3.0 / (917.0 * 2.0) *10.0**6.0)
            d_opt(j) = (optr(j) * 2.0)/10.**6.0
            ssa(j) = (6.0 / (917.0 * d_opt(j))) * 10 
            snow_age(j)= snow_age(j) + dt/3600
          endif  
        endif 
        if (ssa(j) .gt. 65.4308) then 
          ssa(j) = 65.4308
        endif 
        if(ssa(j) .lt. 5) then 
          ssa(j) = 5
        endif

        write(187,*) ssa(j) 
      enddo 
      return 
      end  
                 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine read_file(idtemp_var,idgrad_var,idrho_var,
     & kappa_var,drdt0_var,tau_var,idtemp,idgrad,idrho,itt,maxx
     & ,drdt0,kappa,tau)
   
      implicit none
      
      integer idrho(2728)
      integer idgrad(2728)
      real idtemp(2728)
      real drdt0(2728)
      real  kappa(2728) 
      real tau(2728) 

      real idtemp_var
      integer idgrad_var
      integer  idrho_var
      real drdt0_var
      real kappa_var 
      real tau_var 
      integer maxx
      integer itt, mm, nn, loc_temp, loc_grad, oo, loc_rho
      
c Find location of first variable
      do mm = 1,maxx 
        if (idtemp(mm) .eq. idtemp_var) then 
          loc_temp = mm 
        exit 
        endif 
      enddo
c Find location of second variable
      do nn = loc_temp, loc_temp+247
        if (idgrad(nn) .eq. idgrad_var) then
          loc_grad = nn
        exit
        endif
      enddo
c Find location of third variable
      do oo = loc_grad, loc_grad+7
        if (idrho(oo) .eq. idrho_var) then
        loc_rho = oo
        exit
        endif
      enddo 
c  Find the drdt0
      drdt0_var = drdt0(loc_rho)
      kappa_var = kappa(loc_rho) 
      tau_var = tau(loc_rho)


      return
      end 
          
      

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE T07(d_opt,ssa,liqfrac,
     & temp_grad,T_old,dt,snow_age,JJ,Tf) 
           
      implicit none

      include 'snowmodel.inc'
      real liqfrac(nz_max)
      real d_opt(nz_max)
      real ssa(nz_max)
      real T_old(nz_max)
      real snow_age(nz_max)
      real temp_grad(nz_max)
      real ssa_A(nz_max) 
      real ssa_B(nz_max) 
      real ssa_C(nz_max) 
      real ssa_A2(nz_max) 
      real ssa_B2(nz_max) 
      real ssa_C2(nz_max) 
      real denom1(nz_max) 
      real denom2(nz_max)  
      real fact1(nz_max) 
      real fact2(nz_max) 
      real Tf 
      real ssa_0 
      real ssa_t(nz_max)
      real ssa_t_dt(nz_max)
      real ssa_t_placeholder(nz_max)
      real zdenom_1(nz_max) 
      real zdenom_2(nz_max)
      real zfact_1(nz_max)
      real zfact_2(nz_max)
      real ssa_dt(nz_max) 
      integer j, JJ 
      real dt 
         
       ssa_0 = 6.0 / (917.0 * 1e-4) * 10.0 

      do j=1,JJ 
        if (liqfrac(j) .le. 0.1) then
            write(185,*) T_old(j) 
            ssa_A(j) = 0.659 * ssa_0 - 27.2 * (T_old(j) - Tf
     &      -2.03)
            write(172,*) ssa_A(j) 
            ssa_B(j) = 0.0961 * ssa_0 - 3.44 * (T_old(j) - Tf
     &      + 1.90) 
            write(173,*) ssa_B(j) 
            ssa_C(j) = -0.341 * ssa_0 - 27.2 * (T_old(j) - Tf -
     &      2.03)
            write(174,*) ssa_C(j)
            ssa_A2(j) = 0.629 * ssa_0 - 15.0 * (T_old(j) - Tf -
     &       11.2)
            write(175,*) ssa_A2(j) 
            ssa_B2(j) = 0.0760 * ssa_0 - 1.76 * (T_old(j) - Tf
     &      -2.96)
            write(176,*) ssa_B2(j) 
            ssa_C2(j) = -0.371 * ssa_0 - 15.0 * (T_old(j) - Tf -
     &      11.2)
            write(177,*) ssa_C2(j) 

            !snow_age(j) = snow_age(j)/24 
            
            write(183,*) snow_age(j) 
            zdenom_1(j) = (snow_age(j)) + EXP(ssa_C(j)/ssa_B(j))
            write(180,*) zdenom_1(j) 

            zdenom_2(j) = (snow_age(j)) + EXP(ssa_C2(j)/ssa_B2(j))
            write(181,*) zdenom_2(j) 

            zfact_1(j) = 0.5 + 0.5 * TANH(0.5 * (temp_grad(j) - 10))
            write(184,*) temp_grad(j) 
            write(178,*) zfact_1(j) 
            zfact_2(j) = 0.5 - 0.5 * TANH(0.5 * (temp_grad(j) - 10))
            write(179,*) zfact_2(j) 

            if (snow_age(j) .le. 0.3) then 
             ssa_t_placeholder(j) = ssa_0
            endif 
c           ssa_t_placeholder(j) = ssa_t_placeholder(j) + (dt/3600) * 
c    &      (zfact_1(j)
c    &      * (-ssa_B(j) / zdenom_1(j)) + zfact_2(j) * (-ssa_B2(j) / 
c    &      zdenom_2(j)) + (dt/3600) * (zfact_1(j) * (ssa_B(j) / 
c    &      zdenom_1(j)**2) + zfact_2(j) * (ssa_B(j)/ zdenom_2(j)**2))
c    &      * 1/2)
             
            ssa_dt(j) = ((zfact_1(j)
     &      * (-ssa_B(j) / zdenom_1(j)) + zfact_2(j) * (-ssa_B2(j) / 
     &      zdenom_2(j)) + (zfact_1(j) * (ssa_B(j) / 
     &      zdenom_1(j)**2) + zfact_2(j) * (ssa_B(j)/ zdenom_2(j)**2))
     &      * 1/2)) 
            if (ssa_dt(j) .gt. 0) then 
              ssa_dt(j) = 0 
            endif 
            ssa_t_placeholder(j) = ssa_t_placeholder(j) + ssa_dt(j)
            ! print *, ssa_dt(j) 
            write(186,*) ssa_dt(j) 
            write(182,*) ssa_t_placeholder(j) 
            ssa_t_placeholder(j) = MAX(ssa_t_placeholder(j),8.*10.)
            !print *, ssa_t_placeholder(j) 
            d_opt(j) = 6 / (917 * ssa_t_placeholder(j)/10)
c           if (d_opt(j) .lt. 1e-4) then 
c            d_opt(j) = 1e-4 
c           endif 
c           if (d_opt(j) .gt. 0.0013) then 
c             d_opt(j) = 0.0013
c           endif 
            !snow_age(j) = snow_age(j) * 24 
            snow_age(j) = snow_age(j) + dt/3600 
            ssa(j) = 6 / (917 * d_opt(j))
            !print *, ssa(j) 
            if (ssa(j) .gt. 65.4308) then 
              ssa(j) = 65.4308
            endif

c       ssa_0 = 6.0 / (917.0 * 1e-4) * 10.0 
c 
c        ssa_0 = 6.0 / (917.0 * 1e-4) 
c     do j=1,JJ 
c       !print *, liqfrac(j)
c       if (liqfrac(j) .le. 0.5) then
c           ssa_A(j) = 0.659 * ssa_0 - 27.2 * (T_old(j) - Tf
c    &      -2.03)
c           ssa_B(j) = 0.0961 * ssa_0 - 3.44 * (T_old(j) - Tf
c    &      + 1.90)
c           ssa_C(j) = -0.341 * ssa_0 - 27.2 * (T_old(j) - Tf -
c    &      2.03)
c           ssa_A2(j) = 0.629 * ssa_0 - 15.0 * (T_old(j) - Tf -
c    &       11.2)
c           ssa_B2(j) = 0.0760 * ssa_0 - 1.76 * (T_old(j) - Tf
c    &      -2.96)
c           ssa_C2(j) = -0.371 * ssa_0 - 15.0 * (T_old(j) - Tf -
c    &      11.2)
c           !print *, T_old(j) 
c           !ssa_t_placeholder(j) = (6.0 / (917.0 * d_opt(j))) * 10.0
c           snow_age(j) = snow_age(j)/24 
c           !print *, snow_age(j)     
c           !orig
c           !zdenom_1(j) = (snow_age(j)*24) + EXP(ssa_C(j)/ssa_B(j))
c 
c           !zdenom_2(j) = (snow_age(j)*24) + EXP(ssa_C2(j)/ssa_B2(j))
c 
c           !edit
c           zdenom_1(j) = (snow_age(j)) + EXP(ssa_C(j)/ssa_B(j))
c 
c           zdenom_2(j) = (snow_age(j)) + EXP(ssa_C2(j)/ssa_B2(j))
c 
c           zfact_1(j) = 0.5 + 0.5 * TANH(0.5 * (temp_grad(j) - 10))
c 
c           zfact_2(j) = 0.5 - 0.5 * TANH(0.5 * (temp_grad(j) - 10))
c           !print *, zfact_1(j) 
c rignial   
c           ssa_t(j) = ssa_t_placeholder(j) + (dt/3600) * (zfact_1(j)
c    &      * (-ssa_B(j) / zdenom_1(j)) + zfact_2(j) * (-ssa_B2(j) / 
c    &      zdenom_2(j)) + (dt/3600) * (zfact_1(j) * (ssa_B(j) / 
c    &      zdenom_1(j)**2) + zfact_2(j) * (ssa_B(j)/ zdenom_2(j)**2))
c    &      * 1/2)
c    
c dited     
c           if (snow_age(j) .le. 0.3) then 
c            ssa_t_placeholder(j) = 654
c           endif 
c 
c           ssa_t_placeholder(j) = ssa_t_placeholder(j) + (dt/3600) * 
c    &      (zfact_1(j)
c    &      * (-ssa_B(j) / zdenom_1(j)) + zfact_2(j) * (-ssa_B2(j) / 
c    &      zdenom_2(j)) + (dt/3600) * (zfact_1(j) * (ssa_B(j) / 
c    &      zdenom_1(j)**2) + zfact_2(j) * (ssa_B(j)/ zdenom_2(j)**2))
c    &      * 1/2)
c            print *, ssa_t_placeholder(j) 
c           !print *, temp_grad(j) 
c 
c            
c          ssa_t(j) = (0.5 + 0.5 * TANH(0.5 * (temp_grad(j) - 10.)))*
c    &      (ssa_A(j) - ssa_B(j) * LOG(snow_age(j) * 24 + EXP(ssa_C(j)
c    &       / ssa_B(j)))) + (0.5 - 0.5 * TANH(0.5 * (temp_grad(j)
c    &      - 10))) * (ssa_A2(j) - ssa_B2(j) * LOG(snow_age(j) * 24 
c    &      + EXP(ssa_C2(j) / ssa_B2(j))))
c           !print *, snow_age(j)            
c           ssa_t_dt(j) = (0.5+0.5 * TANH(0.5 * (temp_grad(j) - 10)))*
c    &      (ssa_A(j) - ssa_B(j) * LOG(snow_age(j) * 24 + dt /3600 +
c    &      EXP(ssa_C(j) / ssa_B(j)))) + (0.5 - 0.5 * TANH
c    &      (0.5 * (temp_grad(j) - 10))) * (ssa_A2(j) - ssa_B2(j)*
c    &      LOG(snow_age(j) * 24 + dt / 3600 +EXP(ssa_C2(j) / 
c    &      ssa_B2(j))))
c           
c           ssa_t_placeholder(j) = ssa_t_placeholder(j) + (ssa_t_dt(j)
c    &      - ssa_t(j))
c          
c           ssa_t_placeholder(j) = MAX(ssa_t_placeholder(j),8.*10.)
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c From Carmagnola's Original Code 
c         snow_age(j) = snow_age(j) / 24 
c         ssa_t_placeholder(j) = ssa_t_placeholder(j) + dt/3600*
c    &    (-(0.5+0.5*TANH(0.5*
c    &    (temp_grad(j)-10.)))*
c    &    ssa_B(j)/(snow_age(j)+EXP(ssa_C(j)/ssa_B(j))))  
c    &    +dt/3600*(-(0.5-0.5
c    &    *TANH(0.5*(temp_grad(j)-10.)))*ssa_B2(j)/(snow_age(j)+
c    &    EXP(ssa_C2(j)/ssa_B2(j)))) + dt/3600**2.*((0.5+0.5*TANH
c    &    (0.5*(temp_grad(j)-10.)))*ssa_B(j)/(snow_age(j)
c    &    +EXP(ssa_C(j)/ssa_B(j)))**2.)*1./2.+ dt/3600**2.
c    &    *((0.5-0.5*TANH(0.5*(temp_grad(j)-10.)))*
c    &    ssa_B2(j)/(snow_age(j)+EXP(ssa_C2(j)/ssa_B2(j)))**2.)*1./2.
c         
c          print*, ssa_t_placeholder(j)
c          !print *, snow_age(j)
c          !print *, temp_grad(j) 
c       
c ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c           ! ssa_t_placeholder(j) = MAX(ssa_t_placeholder(j),5.*10.)
c           !print *, ssa_t_placeholder(j) 
c           !d_opt(j) = 6 / (917 * ssa_t_placeholder(j) * 10)
c            ssa_t_placeholder(j) = MAX(ssa_t_placeholder(j),8.)
c           ! print *, ssa_t_placeholder(j) 
c            d_opt(j) = 6 / (917 * ssa_t_placeholder(j))
c           if (d_opt(j) .lt. 1e-4) then 
c            d_opt(j) = 1e-4 
c           endif 
c           if (d_opt(j) .gt. 0.0013) then 
c             d_opt(j) = 0.0013
c           endif 
c           snow_age(j) = snow_age(j) * 24 
c           snow_age(j) = snow_age(j) + dt/3600 
c           
c           !snow_age(j) = snow_age(j) + dt
c           print*, snow_age(j)   
c           
c            ssa(j) = 6 / (917 * d_opt(j))
c           
c           if (ssa(j) .gt. 65.4308) then 
c             ssa(j) = 65.4308
c           endif
c 
c           ssa(j) = 6 / (917 * d_opt(j))
c           !print *, ssa(j) 
        endif

       write(187,*) ssa(j) 
      enddo 
      return 
      end 
 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
cc      SUBROUTINE SNTHRM(dt,ro_layer,T_old,Tf,liqfrac,grain_sz,JJ,
cc     & sfc_pressure)
cc      !!!!! ORIGINAL 
cc      include 'snowmodel.inc'
cc      real sfc_pressure
cc      real ro_layer(nz_max) 
cc      real porosity(nz_max)
cc      real solidporosity(nz_max)
cc      real T_old(nz_max) 
cc      real Tf 
cc      parameter (gg1 = 5.0e-7)
cc      parameter (gg2 = 4.0e-12)
cc      parameter (dice = 917.0)
cc      parameter (de0 = 0.9e-4)
cc      real liqfrac(nz_max)
cc      real uvapor(nz_max)
cc      real dbvdt(nz_max)
cc      real df (nz_max)
cc      real dzzz(nz_max)
cc      real ufvapor(nz_max)
cc      real vaporvol
cc      real dum 
cc      real grain_sz(nz_max) 
cc      real dt 
cc      
cc      parameter(e0 = 6.1368) 
cc      parameter(rw = 461.296)
cc      parameter(dlv = 2.5045e6)
cc      parameter(dls = 2.838e6)
cc      
cc      parameter(dlsdrw = 6152.231973) 
cc      parameter(bvi0 = 8048137432.0)
cc      parameter(dlvdrw = 5429.268843)
cc      parameter(bvw0 = 570454630.6)
cc      integer JJ 
cc      real dicevol(nz_max)
ccCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC	  
cc      do j=1,JJ
cc        dicevol(j) = ro_layer(j) / 917.0
cc        porosity(j) = 1.0 - dicevol(j)
cc        solidporosity(j) = porosity(j) 
cc
cc        if (solidporosity(j) .gt. 1.0) then 
cc          solidporosity(j) = 1.0
cc        endif 
cc        if (solidporosity(j) .lt. 0.0) then 
cc          solidporosity(j) = 0.0
cc        endif 
cc      enddo
ccc    Diffusion coefficient for each snow layer.
ccCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
cc      do j=1,JJ
cc
cc        de = de0 * (1000.0 / sfc_pressure) * (T_old(j) / Tf)**6
cc
cc        if (liqfrac(j) .gt. 0.02) then
cc          dbvdt(j) = bvw0 * exp(- dlvdrw / T_old(j)) *
cc     &      (dlvdrw / T_old(j) - 1.0) / T_old(j)**2
cc        else
cc          dbvdt(j) = bvi0 * exp(- dlsdrw / T_old(j)) *
cc     &      (dlsdrw / T_old(j) - 1.0) / T_old(j)**2
cc        endif
cc
cc        df(j) = de * dbvdt(j)
ccc       if (df(j).le.0.0) print *,'df(j) = 0.0'
cc        vaporvol = solidporosity(j) - liqfrac(j)
cc        dbvdt(j) = vaporvol * dbvdt(j)
cc
cc      enddo
ccCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
cc  
ccc Vapor flux across upper nodal boundary, net flux, and "average"
ccc   flux at centroid (used in grain growth algorithm).  See page
ccc   45 of Patankar (1980) for a description of what is being done
ccc   in the "dum" calculation.
ccc Assume the vapor flux at the soil-snow interface is zero.
cc      uvapor(1) = 0.0
cc
cc      do j=1,JJ-1
cc
cc        dum = 2.0 * df(j) * df(j+1) * (T_old(j+1) - T_old(j)) /
cc     &    (dzzz(j+1) * df(j) + dzzz(j) * df(j+1))
cc
ccc Note that the uvapor(j) used here is using the values from
ccc   the previous time step.  This is why I have had to pass it
ccc   in and out of the subroutine, so it remembers the previous
ccc   values from the last time step.
cc        ufvapor(j) = 0.5 * (abs(uvapor(j)) + abs(dum))
cc        uvapor(j) = uvapor(j) - dum
cc
cc      enddo
cc
ccc We need to figure out how to deal with the top and bottom
ccc   boundaries of the domain.  Note that uvapor(nlayers) has not
ccc   been defined yet so I have commented it out.
ccc     ufvapor(nlayers) = 0.5 * (abs(uvapor(nlayers)) + abs(dum))
cc
ccc This seems to be a reasonable way to do things; setting the
ccc   boundary nodes equal to the nearest interior node values.
cc      ufvapor(JJ) = ufvapor(JJ-1)
cc      ufvapor(1) = ufvapor(2)
cc      uvapor(JJ) = uvapor(JJ-1)
cc      uvapor(1) = uvapor(2)
cc  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc 
cc      do j=1,JJ
cc
ccc 	 if (grain_sz(j) .le. 0.0) then
ccc        print *, 'execution halted because grain_sz <= 0.0'
ccc         print *, 'layer = ',k,'  grain_sz(k) =',grain_sz(k)
ccc         stop
ccc       endif
cc
ccc Dry snow: The cut-off bewteen dry and wet snow is arbitrarily
ccc   set at 0.0001.
cc        if (liqfrac(j) .lt. 0.1) then !was 1e-4 not 0.1 
cc
ccc The max vapor flux available for growth is arbitrarily set at
ccc   1.0e-6.
cc          if (abs(ufvapor(j)) .lt. 1.0e-6) then
cc            grain_sz(j) = grain_sz(j) + dt * gg1 * abs(ufvapor(j)) / 
cc     &       grain_sz(j)
cc          else
cc            grain_sz(j) = grain_sz(j) + dt * gg1 * 1.0e-6 / grain_sz(j)
cc          endif
cc
ccc Wet snow: Different equations for liquid volume fraction
ccc   above and below 0.09.
cc        else
cc          if (liqfrac(j) .lt. 0.2) then !was 0.09 not 0.2  
cc            grain_sz(j) = grain_sz(j) + dt * gg2 * (liqfrac(j) + 0.05) /
cc     &       grain_sz(j)
cc          else
cc            grain_sz(j) = grain_sz(j) + dt * gg2 * 0.14 / grain_sz(j)
cc          endif
cc        endif
cc
ccc Max grain size arbitrarily set at 5mm. 
cc        if (grain_sz(j) .gt. 5.0e-3) then 
cc          grain_sz(j) = 5.0e-3
cc        endif 
cc
cc      enddo
cc      return 
cc      end 
cc   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE NSNOWMICRO(windspd,dendricity_n,sphericity_n,d_opt_n,
     & ssa_n,grain_sz_n,viscosity_n,zi_n,snow_age_n,history_n,
     & ro_nsnow,Tsfc,b92_flag,c13_flag,snthrm_flag,dangl_n)
        implicit none
     
        real windspd,Tsfc
         !sw 
        real dendricity_n,sphericity_n,d_opt_n,ro_nsnow,
     &  ssa_n,grain_sz_n,viscosity_n,zi_n,snow_age_n,history_n,
     &  dangl_n
        integer b92_flag,c13_flag,snthrm_flag

     
        if ((b92_flag .eq. 1) .or. (c13_flag .eq. 1))then
          dendricity_n = -99 
          sphericity_n = 0 
        else 
          dendricity_n = MIN(MAX(1.29 - 0.17 * windspd, 20.0), 1.0)
          sphericity_n = MIN(MAX(0.08 * windspd  + 0.38, 0.5), 0.9)
        endif 
c       d_opt_n = 1e-4 * ((dendricity_n * (sphericity_n - 3)) -
c    &  sphericity_n + 4)
        d_opt_n = 1e-4 
        ssa_n = (6.0 / (d_opt_n * 917.0))  
        grain_sz_n = 1e-4 * (4 - sphericity_n)
        if (snthrm_flag .eq. 1) then
          grain_sz_n = 1.6e-4 + 1.1e-13 * ro_nsnow**4
           if (grain_sz_n .lt. 2.5e-5) then 
             grain_sz_n = 2.5e-5
           endif 
        endif 
        dangl_n=0
c from line 1580 in snowcro.F90
c I think the units here are Pa/second. Not entirely sure.
c Likely better way to initalize viscosity 
        viscosity_n= 0.05 * ro_nsnow**(-0.0371 * (Tsfc - 
     &  273.16) + 4.4) * (1e-4 * EXP(0.018 * ro_nsnow) + 1.0)
       
        zi_n = 0 
        snow_age_n = 0 
        history_n = 0

        
        return
        end 


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE SNTHRM(dt,ro_layer,T_old,Tf,liqfrac,grain_sz,JJ,
     & sfc_pressure,ssa)
      !!! GLENS NEWEST VERSION 
      include 'snowmodel.inc'
      real sfc_pressure
      real ro_layer(nz_max) 
      real porosity(nz_max)
      real solidporosity(nz_max)
      real T_old(nz_max) 
      real Tf 
      parameter (gg1 = 5.0e-7)
      parameter (gg2 = 4.0e-12)
      parameter (dice = 917.0)
      parameter (de0 = 9.2e-5)
      real liqfrac(nz_max)
      real uvapor(nz_max)
      real dbvdt(nz_max)
      real df (nz_max)
      real dzzz(nz_max)
      real dz(nz_max)
      real ufvapor(nz_max)
      real vaporvol
      real dum 
      real grain_sz(nz_max) 
      real ssa(nz_max)
      real dt 
      real C_kT(nz_max)
      real Des
      real diff_coef(nz_max) 
      integer nzm 
      !parameter(e0 = 6.1368) adjusted from this 		
      parameter (e0 = 613.6)
      parameter(rw = 461.296)
      parameter(dlv = 2.505e6)
      parameter(dls = 2.838e6)
      
      data xLvw_o_Rw /5427.952/
      data c1w /5.6739e8/
      data xLvi_o_Rw /6149.513/
      data c1i /7.9639e9/
      !parameter(dlsdrw = 6152.231973) 		
      !parameter(bvi0 = 8048137432.0)		
      !parameter(dlvdrw = 5429.268843)		
      !parameter(bvw0 = 570454630.6)		
      real dicevol(nz_max)
      !real dz_bndry(JJ+1) 		
      !real T_layer_bndry(JJ+1)		
      !real diff_coef_bndry(JJ+1)		
      real dz_bndry(nz_max+1) 
      real T_layer_bndry(nz_max+1)
      real diff_coef_bndry(nz_max+1)
      ! JJ above may need to be nz_max 		
      real Uv_bot
      real Uv_top
      real vapor_flux(nz_max)
      integer JJ 
c     data grain_scale /1.0/		
c     data grain_scale /0.5/		
      data grain_scale/2.5/ 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC	  		
      pi = 2.0 * acos(0.0) 
      do j=1,JJ
        dicevol(j) = ro_layer(j) / 917.0
        porosity(j) = 1.0 - dicevol(j)

        if (porosity(j) .gt. 1.0) then 
          porosity(j) = 1.0
        endif 
        if (porosity(j) .lt. 0.0) then 
          porosity(j) = 0.0
        endif 
      enddo
c    Diffusion coefficient for each snow layer.		
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC		
      do j=1,JJ

c        de = de0 * (1000.0 / sfc_pressure) * (T_old(j) / Tf)**6		

       ! if (liqfrac(j) .gt. 0.02) then #1 
       if (liqfrac(j) .gt. 0.1) then 
c         dbvdt(j) = bvw0 * exp(- dlvdrw / T_old(j)) *		
c    &      (dlvdrw / T_old(j) - 1.0) / T_old(j)**2		
          C_kT(j) = c1w * exp(- xLvw_o_Rw / T_old(j)) *
     &      (xLvw_o_Rw / T_old(j) - 1.0) / T_old(j)**2
        else
c         dbvdt(j) = bvi0 * exp(- dlsdrw / T_old(j)) *		
c    &      (dlsdrw / T_old(j) - 1.0) / T_old(j)**2		
          C_kT(j) = c1i * exp(- xLvi_o_Rw / T_old(j)) *
     &      (xLvi_o_Rw / T_old(j) - 1.0) / T_old(j)**2
        endif

c       df(j) = de * dbvdt(j)		
c       if (df(j).le.0.0) print *,'df(j) = 0.0'		
        vaporvol = porosity(j) - liqfrac(j)
c       dbvdt(j) = vaporvol * dbvdt(j)		
        vaporvol = max(0.0,vaporvol)
        C_kT(k) = vaporvol * C_kT(k) 
        Des = de0 * (100000.0 / sfc_pressure) * (T_old(j) / Tf)**6 
        diff_coef(k) = Des * C_kT(k) 
      enddo
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC		
  
c Vapor flux across upper nodal boundary, net flux, and "average"		
c   flux at centroid (used in grain growth algorithm).  See page		
c   45 of Patankar (1980) for a description of what is being done		
c   in the "dum" calculation.		
c Assume the vapor flux at the soil-snow interface is zero.		
c     uvapor(1) = 0.0		

c     do j=1,JJ-1		

c       dum = 2.0 * df(j) * df(j+1) * (T_old(j+1) - T_old(j)) /		
c    &    (dzzz(j+1) * df(j) + dzzz(j) * df(j+1))		
c		
c Note that the uvapor(j) used here is using the values from		
c   the previous time step.  This is why I have had to pass it		
c   in and out of the subroutine, so it remembers the previous		
c   values from the last time step.		
c       ufvapor(j) = 0.5 * (abs(uvapor(j)) + abs(dum))		
c       uvapor(j) = uvapor(j) - dum		

c     enddo		

c We need to figure out how to deal with the top and bottom		
c   boundaries of the domain.  Note that uvapor(nlayers) has not		
c   been defined yet so I have commented it out.		
c     ufvapor(nlayers) = 0.5 * (abs(uvapor(nlayers)) + abs(dum))		

c This seems to be a reasonable way to do things; setting the		
c   boundary nodes equal to the nearest interior node values.		
c     ufvapor(JJ) = ufvapor(JJ-1)		
c     ufvapor(1) = ufvapor(2)		
c     uvapor(JJ) = uvapor(JJ-1)		
c     uvapor(1) = uvapor(2)		
c Number of shifted layers 		
      nzm = JJ + 1 
c Control volume size 		
      dz_bndry(1) = 0.0
      do k = 2,nzm 
        dz_bndry(k) = dz(k-1) 
      enddo 
      dz_bndry(nzm_1) = 0.0 
c Temperature 		
      T_layer_bndry(1) = T_old(1) 
      do k=2,nz m
        T_layer_bndry(k) = T_old(k-1)
      enddo
      T_layer_bndry(nzm+1) = T_old(nzm-1) 
c Diffusion coefficent 		
      diff_coef_bndry(1) = diff_coef(1) 
      do k=2,nzm
        diff_coef_bndry(k) = diff_coef(k-1)
      enddo
      diff_coef_bndry(nzm+1) =diff_coef(nzm-1)


      do k=2,nzm
        Uv_bot = - 2.0 * (T_layer_bndry(k) - T_layer_bndry(k-1)) /
     &    (dz_bndry(k-1) / diff_coef_bndry(k-1) +
     &    dz_bndry(k) / diff_coef_bndry(k))
        Uv_top = - 2.0 * (T_layer_bndry(k+1) - T_layer_bndry(k)) /
     &    (dz_bndry(k) / diff_coef_bndry(k) +
     &    dz_bndry(k+1) / diff_coef_bndry(k+1))
        vapor_flux(k-1) = (abs(Uv_bot) + abs(Uv_top)) / 2.0 
      enddo 
      vapor_flux(1) = vapor_flux(2)
      vapor_flux(nlayers) = vapor_flux(nlayers-1)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc		
 
      do j=1,JJ

c 	 if (grain_sz(j) .le. 0.0) then		
c        print *, 'execution halted because grain_sz <= 0.0'		
c         print *, 'layer = ',k,'  grain_sz(k) =',grain_sz(k)		
c         stop		
c       endif		

c Dry snow: The cut-off bewteen dry and wet snow is arbitrarily		
c   set at 0.0001.		
        !if (liqfrac(j) .lt. 1.0e-4) then #1 
         if (liqfrac(j) .lt. 0.1) then 

c The max vapor flux available for growth is arbitrarily set at		
c   1.0e-6.		
          if (abs(vapor_flux(j)) .lt. 1.0e-6) then
            grain_sz(j) = grain_sz(j) + grain_scale * dt * gg1 * 
     &       abs(vapor_flux(j)) /  grain_sz(j)
          else
            grain_sz(j) = grain_sz(j) + grain_scale *dt * gg1 *
     &       1.0e-6 / grain_sz(j)
          endif

c Wet snow: Different equations for liquid volume fraction		
c   above and below 0.09.		
        else
          !if (liqfrac(j) .lt. 0.09) then #1 
          if (liqfrac(j) .lt. 0.2) then 

            grain_sz(j) = grain_sz(j) + grain_scale * dt * gg2 *
     &       (liqfrac(j) + 0.05) / grain_sz(j)
          else
            grain_sz(j) = grain_sz(j) + grain_scale* dt * gg2 *
     &       0.14 / grain_sz(j)
          endif
        endif

c Max grain size arbitrarily set at 5mm. 		
        if (grain_sz(j) .gt. 5.0e-3) then 
          grain_sz(j) = 5.0e-3
        endif
        !ssa(j) = -14.57*ALOG(grain_sz(j))-69.199
        !ssa(j) = ALOG(grain_sz(j)) 
        !print *, grain_sz(j) 
      enddo
       
      do j=1,JJ
        ssa(j) = -14.57*ALOG(grain_sz(j))-69.199

        write(187,*) ssa(j) 
      enddo 
      return 
      end 

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE WIND_COMPACTION(ro_layer,dendricity,
     & sphericity,windspd,dy_snow,zi,grain_sz,d_opt,JJ)
        
        implicit none 
        include 'snowmodel.inc'
        real ro_layer(nz_max) 
        real dendricity(nz_max) 
        real sphericity(nz_max) 
        real windspd 
        real dy_snow(nz_max) 
        real zi(nz_max) 
        real grain_sz(nz_max) 
        real f_rho(nz_max) 
        real MO(nz_max) 
        real SI(nz_max) 
        real delta_dend(nz_max)
        real delta_ro_snow(nz_max)
        real delta_spher(nz_max)
        real delta_grain(nz_max)
        real tau(nz_max)  
        real d_opt(nz_max)
        integer j,JJ 
       
      do j=1,JJ 
        f_rho(j) = 1.25 - 0.0042 * (MAX(50.0 , ro_layer(j)) - 50.0)
        if (dendricity(j) .gt. 0) then 
          MO(j) = 0.34 * (0. 75 * dendricity(j) - 0.5 * sphericity(j) 
     &     + 0.5) + (0.66 * f_rho(j))  
        else 
          MO(j) = 0.34 * (-0.583 * grain_sz(j) - 0.833 * sphericity(j) 
     &     + 0.833) + (0.66 * f_rho(j)) 
        endif 
        SI(j) = -2.868 * EXP(-0.085 * windspd) + 1 + MO(j)
        if((3.25 - SI(j)) .gt. 0) then
          zi(j) = (dy_snow(j) *(3.25 - SI(j))) + zi(j)
          ! 48 units are hours, not sure if it should 
          ! be in seconds 
          tau(j) = 48.0 / MAX (0.0, SI(j)  * EXP(-zi(j) / 0.1))
          ! 350 is max snow density 
          delta_ro_snow(j) = (350.0 - ro_layer(j)) / tau(j)
          if (dendricity(j) .gt. 0) then
            delta_dend(j) = dendricity(j) / (2 * tau(j))
            delta_spher(j) = (1 - sphericity(j)) / tau(j)
          endif
          if (dendricity(j) .eq. 0) then 
            delta_spher(j) = (1 - sphericity(j)) / tau(j) 
            delta_grain(j) = 5.1e-4 / tau(j)
          endif
      
          ro_layer(j) = ro_layer(j) + delta_ro_snow(j)
          dendricity(j) = delta_dend(j) + dendricity(j)
          sphericity(j) = delta_spher(j) + sphericity(j)
          grain_sz(j) = delta_grain(j) + grain_sz(j) 
          if (dendricity(j) .gt. 0) then 
            d_opt(j) = 10e-4 * (dendricity(j) + (1 - dendricity(j)) * 
     &      (4 - sphericity(j)))
          else 
            d_opt(j) = grain_sz(j) * sphericity(j) + 
     &      (1 - sphericity(j)) * MAX(4.1e-4,grain_sz(j)/2)     
          endif  
        exit 
        endif        
      enddo
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
