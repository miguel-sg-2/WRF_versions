MODULE module_gad_nrel2p8

REAL, PARAMETER :: pi=3.1415926535897932384626433

CONTAINS

!===============================================================================
!
!  NREL 2.8MW Turbine 
! hub_height: 88.5, rotor_diameter: 127.0; hub diameter: 4.0864; blade length = 61.4568
!
!===============================================================================

!=========================================================================

FUNCTION calc_rpm_nrel2p8( x ) result( rpm )

!_________________________________________________________________________
!
! Data from NREL 2.8 MW
! x =  wind speed
!_________________________________________________________________________


  REAL, INTENT( in ) :: x

  INTEGER, PARAMETER :: nws = 50

  REAL, DIMENSION( 0:nws-1 ) :: ws
  REAL, DIMENSION( 0:nws-1 ) :: rp

  REAL :: rpm
  INTEGER :: i

  !print*,'FUNCTION calc_rpm_nrel2p8'

  rpm = 0.0

  rp(0) = 5.0
  rp(1) = 5.0
  rp(2) = 5.0
  rp(3) = 5.0
  rp(4) = 5.0
  rp(5) = 5.0
  rp(6) = 5.0
  rp(7) = 5.089
  rp(8) = 5.491
  rp(9) = 6.019
  rp(10) = 6.621
  rp(11) = 7.223
  rp(12) = 7.825
  rp(13) = 8.427
  rp(14) = 9.029
  rp(15) = 9.631
  rp(16) = 10.233
  rp(17) = 10.835
  rp(18) = 11.437
  rp(19) = 11.825
  rp(20) = 12.039
  rp(21) = 12.039
  rp(22) = 12.039
  rp(23) = 12.039
  rp(24) = 12.039
  rp(25) = 12.039
  rp(26) = 12.039
  rp(27) = 12.039
  rp(28) = 12.039
  rp(29) = 12.039
  rp(30) = 12.039
  rp(31) = 12.039
  rp(32) = 12.039
  rp(33) = 12.039
  rp(34) = 12.039
  rp(35) = 12.039
  rp(36) = 12.039
  rp(37) = 12.039
  rp(38) = 12.039
  rp(39) = 12.039
  rp(40) = 12.039
  rp(41) = 12.039
  rp(42) = 12.039
  rp(43) = 12.039
  rp(44) = 12.039
  rp(45) = 12.039
  rp(46) = 12.039
  rp(47) = 12.039
  rp(48) = 12.039
  rp(49) = 12.039

  ws(0) = 0.5
  ws(1) = 1.0
  ws(2) = 1.5
  ws(3) = 2.0
  ws(4) = 2.5
  ws(5) = 3.0
  ws(6) = 3.5
  ws(7) = 4.0
  ws(8) = 4.5
  ws(9) = 5.0
  ws(10) = 5.5
  ws(11) = 6.0
  ws(12) = 6.5
  ws(13) = 7.0
  ws(14) = 7.5
  ws(15) = 8.0
  ws(16) = 8.5
  ws(17) = 9.0
  ws(18) = 9.5
  ws(19) = 10.0
  ws(20) = 10.5
  ws(21) = 11.0
  ws(22) = 11.5
  ws(23) = 12.0
  ws(24) = 12.5
  ws(25) = 13.0
  ws(26) = 13.5
  ws(27) = 14.0
  ws(28) = 14.5
  ws(29) = 15.0
  ws(30) = 15.5
  ws(31) = 16.0
  ws(32) = 16.5
  ws(33) = 17.0
  ws(34) = 17.5
  ws(35) = 18.0
  ws(36) = 18.5
  ws(37) = 19.0
  ws(38) = 19.5
  ws(39) = 20.0
  ws(40) = 20.5
  ws(41) = 21.0
  ws(42) = 21.5
  ws(43) = 22.0
  ws(44) = 22.5
  ws(45) = 23.0
  ws(46) = 23.5
  ws(47) = 24.0
  ws(48) = 24.5
  ws(49) = 25.0

  DO i = 1,nws-1
     
     IF ( ( x .GE. ws(i-1) ) .AND. ( x .LT. ws(i) ) ) THEN
        rpm = rp(i-1) + (x - ws(i-1))*( ( rp(i) - rp(i-1) )/( ws(i) - ws(i-1) ) )
        GOTO 1
     ENDIF

  END DO
  
  IF ( x .LT. ws(0) ) rpm = rp(0)
  IF ( x .EQ. ws(nws-1) ) rpm = rp(nws-1)

1 CONTINUE
 
!PRINT*,x,rpm
!_________________________________________________________________________

END FUNCTION calc_rpm_nrel2p8
!_________________________________________________________________________
 
!=========================================================================

FUNCTION calc_pitch_nrel2p8( x ) RESULT( pitch )

!_________________________________________________________________________
!
! Data from NREL 2.8 MW
! x = wind speeds
!_________________________________________________________________________
 
  REAL, INTENT( IN ) :: x

  INTEGER, PARAMETER :: nws = 50

  REAL, DIMENSION( 0:nws-1 ) :: ws 
  REAL, DIMENSION( 0:nws-1 ) :: p

  REAL :: pitch
  INTEGER :: i

  pitch = 0.0

  p(0) = 0.0
  p(1) = 0.0
  p(2) = 0.0
  p(3) = 0.0
  p(4) = 0.0
  p(5) = 0.0
  p(6) = 0.0
  p(7) = 0.0
  p(8) = 0.0
  p(9) = 0.0
  p(10) = 0.0
  p(11) = 0.0
  p(12) = 0.0
  p(13) = 0.0
  p(14) = 0.0
  p(15) = 0.603
  p(16) = 2.585
  p(17) = 4.057
  p(18) = 5.232
  p(19) = 6.094
  p(20) = 6.88
  p(21) = 7.918
  p(22) = 9.022
  p(23) = 10.077
  p(24) = 11.013
  p(25) = 11.922
  p(26) = 12.759
  p(27) = 13.571
  p(28) = 14.346
  p(29) = 15.088
  p(30) = 15.823
  p(31) = 16.52
  p(32) = 17.218
  p(33) = 17.882
  p(34) = 18.529
  p(35) = 19.176
  p(36) = 19.799
  p(37) = 20.422
  p(38) = 21.032
  p(39) = 21.616
  p(40) = 22.2
  p(41) = 22.771
  p(42) = 23.332
  p(43) = 23.892
  p(44) = 24.446
  p(45) = 24.997
  p(46) = 25.548
  p(47) = 26.079
  p(48) = 26.606
  p(49) = 27.133
    
  ws(0) = 0.5
  ws(1) = 1.0
  ws(2) = 1.5
  ws(3) = 2.0
  ws(4) = 2.5
  ws(5) = 3.0
  ws(6) = 3.5
  ws(7) = 4.0
  ws(8) = 4.5
  ws(9) = 5.0
  ws(10) = 5.5
  ws(11) = 6.0
  ws(12) = 6.5
  ws(13) = 7.0
  ws(14) = 7.5
  ws(15) = 8.0
  ws(16) = 8.5
  ws(17) = 9.0
  ws(18) = 9.5
  ws(19) = 10.0
  ws(20) = 10.5
  ws(21) = 11.0
  ws(22) = 11.5
  ws(23) = 12.0
  ws(24) = 12.5
  ws(25) = 13.0
  ws(26) = 13.5
  ws(27) = 14.0
  ws(28) = 14.5
  ws(29) = 15.0
  ws(30) = 15.5
  ws(31) = 16.0
  ws(32) = 16.5
  ws(33) = 17.0
  ws(34) = 17.5
  ws(35) = 18.0
  ws(36) = 18.5
  ws(37) = 19.0
  ws(38) = 19.5
  ws(39) = 20.0
  ws(40) = 20.5
  ws(41) = 21.0
  ws(42) = 21.5
  ws(43) = 22.0
  ws(44) = 22.5
  ws(45) = 23.0
  ws(46) = 23.5
  ws(47) = 24.0
  ws(48) = 24.5
  ws(49) = 25.0

  DO i = 1,nws-1

     IF ( ( x .GT. ws(i-1) ) .AND. ( x .LE. ws(i) ) ) THEN 
        pitch = p(i-1) + (x - ws(i-1))*( ( p(i) - p(i-1) )/( ws(i) - ws(i-1) ) )
        GOTO 2
     ENDIF
     
  ENDDO

  IF ( x .LE. ws(0) ) pitch = p(0)
  IF ( x .GT. ws(nws-1) ) pitch = p(nws-1)

2 CONTINUE
  
!PRINT*,x,pitch
!_________________________________________________________________________

END FUNCTION calc_pitch_nrel2p8

!=========================================================================

FUNCTION calc_fnft_nrel2p8( v0, rho, blades, bladelen, diameter,           &
                         radius, pitchdeg, rpm, dscale, cscale)         &
                         result( fnft )
!_________________________________________________________________________
!
!  --v0       - "free stream" velocity normal to the turbine rotor plane, [m/s]
!  --rho      - air density, [kg/m^3]
!  --blades   - number of turbine blades, [non-dimensional]
!  --diameter - turbine rotor diameter, [m]
!  --xcoord   - streamwise coordinate of the grid point with respect to
!               local coordinate system with the origin at the turbine hub, [m]
!  --ycoord   - cross-stream coordinate of the grid point with respect to
!               local coordinate system with the origin at the turbine hub, [m]
!  --zcoord   - vertical coordinate of the grid point with respect to
!               local coordinate system with the origin at the turbine hub, [m]
!
!  --fnft     - one-dimensional array (two members) with FUNCTION output
!               specific normal and tangential forces per unit area, [N/m^2]
!
     real, intent( in ) :: v0, rho, blades, bladelen, diameter
     real, intent( in ) :: dscale, cscale 
     real, intent( in ) :: radius
     real, intent( in ) :: rpm
     real, intent( in ) :: pitchdeg
     real, dimension( 2 )  :: fnft
     real               :: rps, omega 
     real               :: radmax, radmin
     real               :: an, at
     real               :: anold, atold
     real               :: cord
     real               :: twist
     real               :: xi, xideg, beta, betadeg, psi, psideg
     real               :: vn, vt
     real               :: cl, cd
     real               :: cn, ct
     real               :: solid
     real               :: fac1, fac2, tiploss, hubloss, lossfac
     real               :: vr
     real               :: lift, drag
     real               :: fn, ft
     real               :: epsi, epscrit
     real               :: cd_nac
     integer            :: iter, itmax
!
     rps   = rpm/60.0
     omega = 2.0*pi*rps
!
     radmax = 0.5*diameter
     radmin = radmax - bladelen
!

  fnft(1) = 0.0
  fnft(2) = 0.0

  IF (radius .LT. radmax) THEN !JDM
     
     IF (radius .GT. radmin) THEN !JDM ! MSG CHANGED FROM .GE. TO .GT.

        an = 0.0
        at = 0.0
!
        iter = 0
        epsi = 1.0        
        epscrit = 1.e-5
        itmax   = 20
!
!    --Incidence angle

        xideg = calc_twist_nrel2p8( radius/dscale ) + pitchdeg
        xi    = xideg*pi/180.0
!
!    --Cord length 

        cord  = cscale * calc_chord_nrel2p8( radius/dscale )
!
!    --Compute induction factors an and at iteratively

        do while ( ( epsi .gt. epscrit ) .and. ( iter .lt. itmax ) ) 
!
           iter = iter + 1
!
!       --Normal and tangential velocity components

           an = 0.02 !JDM This is just the tip loss component of an. This was computed using the
                     !    IDL code as the difference between an computed with and without tip loss
                     !    across the power curve. The value of 0.02 was nearly constant.

           vn = v0*( 1.0 - an )
           vt = omega*radius*( 1.0 + at ) 
 
!       --Advance angle 
  
           psi    = atan2(vn,vt)
           psideg = psi*180.0/pi
! print*,'iter',iter
! print*,'v0,  omega, radius',v0,omega,radius
! print*,'vn, at, vt',vn,at,vt
! print*,'psi',psi
! print*,'------'

!       --Incidence angle  
 
           beta    = psi - xi
           betadeg = beta*180.0/pi

!       --Lift and drag coefficients  
     
           cl = calc_cl_nrel2p8( radius/dscale, betadeg )
           cd = calc_cd_nrel2p8( radius/dscale, betadeg )
  
!       --Normal and tangential force coefficients     
  
           cn = cl*cos( psi ) + cd*sin( psi )
           ct = cl*sin( psi ) - cd*cos( psi )

!       --Solidity factor

           solid = cord*blades / ( 2.0*pi*radius )

!       --Tip and hub loss

!          When psi < 0, fac1 and fac 2 can give bizarre answers, and resulting in tip and hub loss terms - NaN.
!          Some logic should be added to keep things from going crazy
           
!          RSA update: the argument of acos should have an abs. value <= 1. When psi
!          is negative, fac1 and/or fac2 are negative, causing exp(-fac) to be a
!          large positive number, causing acos to return NaN. When psi is positive, fac1 and fac2 are
!          positive, such that exp(-fac) is small and positive, which is ok.
!          Thus, we have added logic around the calls to calc_fnft above such
!          that when v0 < 0 (ie psi < 0), this function is not called and the
!          force is set to 0. The turbine should still yaw into place eventually.

           if ( abs( psi ) .gt. 0.001 ) then
             if ( psi .lt. (pi-0.01)) then  !! if STATEMENT ADDED BY MSG TO AVOID GETTING NAN IN tiploss AND hubloss
              fac1 = blades / 2.0*( radmax - radius ) / ( radius*sin( psi ) )
              fac2 = blades / 2.0*( radius - radmin ) / ( radmin*sin( psi ) )

              tiploss = (2.0/pi)*acos( exp( -fac1 ) )
              hubloss = (2.0/pi)*acos( exp( -fac2 ) )

              lossfac = tiploss*hubloss

!             --Induction factors

              anold = an
              atold = at

!JDM             an = ( 1. + 4.0*lossfac*sin( psi )*sin( psi )/ ( solid*cn ) )**(-1.0)

              at = ( 4.0*lossfac*sin( psi )*cos( psi )/ ( solid*ct ) - 1.0 )**(-1.0)

! print*,'inside of iteration'
! print*,'iter',iter
! print*,'omega, radius, radmin, radmax',omega,radius,radmin,radmax
! print*,'v0, vn, vt, an, at',v0,vn,vt,an,at
! print*,'psi, fac1, fac2',psi,fac1,fac2
! print*,'tiploss, hubloss, lossfac',tiploss,hubloss,lossfac
! print*,'------'

            else

             anold = an
             atold = at

             an = 0.0
             at = 0.0


            endif
          else

             anold = an
             atold = at

             an = 0.0
             at = 0.0


          endif
!       --Convergence criteria

           epsi = sqrt( ( anold - an )**2.0 + ( atold - at )**2.0 )
  
        end do
        if (isnan(at)) then
                print*,'at is NaN lossfac=',lossfac,'psi=',psi,'solid=',solid,'ct=',ct, & 
                        'tiploss=',tiploss,'hubloss=',hubloss,'fac1=',fac1,'fac2=',fac2,'radius=',radius
        endif
!        if (at .GT. 0) then
!                print*,'at is > 0 at=',at,'psi=',psi,'fac1=',fac1,'fac2=',fac2,'radius=',radius
!        endif
        if (isnan(at)) then
                print*,'ADD LOGIC SO THAT PSI > 0 deg AND PSI < 180 deg (add error margins to each limit) '
        endif
!print*,'radius,radmax,radmin',radius,radmax,radmin
!print*,'radius - radmin',radius - radmin
!print*,'fac1,fac2',fac1,fac2
!print*,'psi,sin(psi)',psi,sin(psi)
!print*,'tiploss,hubloss',tiploss,hubloss
!print*,'cl,cd',cl,cd
!print*,'cn,ct',cn,ct

!    --Normal and tangential velocity components

       vn = v0*( 1.0 - an )

       vt = omega*radius*( 1.0 + at ) 

!    --Relative velocity

       vr = sqrt( vn*vn + vt*vt ) 
! 
!    --Advance angle   

       psi    = atan2(vn,vt)
       psideg = psi*180.0/pi
        if (isnan(psi)) then
                print*,'vn=',vn,'vt=',vt,'v0=',v0,'an=',an,'omega=',omega,'radius=',radius,'at=',at
        endif
!
!    --Incidence angle   

       beta    = psi - xi
       betadeg = beta*180.0/pi

! print*,'outside of iteration'
! print*,'v0, vn, at, vt',v0,vn,at,vt
! print*,'psideg, betadeg',psideg,betadeg
! print*,'------'

!    --Lift and drag coefficients     
  
       cl = calc_cl_nrel2p8( radius/dscale, betadeg )
       cd = calc_cd_nrel2p8( radius/dscale, betadeg )

!    --Lift and drag forces

       lift = 0.5*rho*vr*vr*cord*cl 
       drag = 0.5*rho*vr*vr*cord*cd 

!    --Specific normal and tangential forces

       fn = lift*cos( psi ) + drag*sin( psi )
       ft = lift*sin( psi ) - drag*cos( psi )

!       print*,'psi',psideg

       fn = blades*fn/(2.0*pi*radius)
       ft = blades*ft/(2.0*pi*radius)

       if( fn .lt. 0.0 ) fn = 0.0
       if( ft .lt. 0.0 ) ft = 0.0
        if (isnan(fn)) then
                print*,'fn is NaN, psi=',psi,'lift=',lift,'drag=',drag
        endif
        if (isnan(ft)) then
                print*,'ft is NaN, psi=',psi,'lift=',lift,'drag=',drag
        endif
       fnft(1) = fn
       fnft(2) = ft              

!print*,'radius,radmax,radmin',radius,radmax,radmin
!print*,'radius - radmin',radius - radmin
!print*,'fac1,fac2',fac1,fac2
!print*,'psi,sin(psi)',psi,sin(psi)
!print*,'tiploss,hubloss',tiploss,hubloss
!print*,'cl,cd',cl,cd
!print*,'cn,ct',cn,ct
!print*,'fn,ft',fn,ft


     ENDIF !radius .GE. radmin

  ENDIF   !radius .LT. radmax

END FUNCTION calc_fnft_nrel2p8

!=========================================================================

FUNCTION calc_twist_nrel2p8( x ) RESULT( twist )
!_________________________________________________________________________
!
! Computes blade twist as a function of radial distance from hub center
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: x

  INTEGER, PARAMETER :: ns = 30

  REAL, DIMENSION( 0:ns-1 ) :: r
  REAL, DIMENSION( 0:ns-1 ) :: t

  REAL :: twist 
  INTEGER :: i

  twist = 0.0

  !print*,'FUNCTION calc_twist_nrel2p8'
  
  r(0) = 2.043
  r(1) = 4.162
  r(2) = 6.282
  r(3) = 8.401
  r(4) = 10.52
  r(5) = 12.639
  r(6) = 14.758
  r(7) = 16.878
  r(8) = 18.997
  r(9) = 21.116
  r(10) = 23.235
  r(11) = 25.354
  r(12) = 27.474
  r(13) = 29.593
  r(14) = 31.712
  r(15) = 33.831
  r(16) = 35.95
  r(17) = 38.07
  r(18) = 40.189
  r(19) = 42.308
  r(20) = 44.427
  r(21) = 46.546
  r(22) = 48.666
  r(23) = 50.785
  r(24) = 52.904
  r(25) = 55.023
  r(26) = 57.142
  r(27) = 59.262
  r(28) = 61.381
  r(29) = 63.5
 
  t(0) = 19.996
  t(1) = 19.31
  t(2) = 18.317
  t(3) = 17.088
  t(4) = 15.692
  t(5) = 13.823
  t(6) = 11.246
  t(7) = 8.603
  t(8) = 6.549
  t(9) = 5.408
  t(10) = 4.553
  t(11) = 3.85
  t(12) = 3.21
  t(13) = 2.548
  t(14) = 1.869
  t(15) = 1.247
  t(16) = 0.759
  t(17) = 0.466
  t(18) = 0.273
  t(19) = 0.123
  t(20) = -0.019
  t(21) = -0.184
  t(22) = -0.343
  t(23) = -0.503
  t(24) = -0.694
  t(25) = -0.946
  t(26) = -1.331
  t(27) = -1.858
  t(28) = -2.493
  t(29) = -3.205
  
  DO i = 1,ns-1
 
     IF ( ( x .GT. r(i-1) ) .AND. ( x .LE. r(i) ) ) THEN
        twist = t(i-1) + (x - r(i-1))*( ( t(i) - t(i-1) )/( r(i) - r(i-1) ) )
        goto 1
     ENDIF

1 CONTINUE

     IF ( x .LT. r(0) ) THEN
        PRINT*,'—————————————'
        PRINT*,'radius < blade root in calc_twist_nrel2p8.pro'
	PRINT*, 'radius: ', x
        PRINT*,'—————————————'
        STOP
     ENDIF
     
     IF ( x .GT. r(ns-1) ) THEN
        PRINT*,'—————————————'
        PRINT*,'radius >  blade length in calc_twist_nrel2p8.pro'
	PRINT*,'radius: ', x
        PRINT*,'—————————————'
        STOP
     ENDIF 
     
  ENDDO
  
!PRINT*,x,twist
  
!_________________________________________________________________________
  
END FUNCTION calc_twist_nrel2p8
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_chord_nrel2p8( x ) RESULT( chord )
!_________________________________________________________________________
!
! Data from NREL 2.8 MW
! x = radial distance from hub center
!_________________________________________________________________________
 

  REAL, INTENT( IN ) :: x

  INTEGER, PARAMETER :: ns = 30

  REAL, DIMENSION( 0:ns-1 ) :: r
  REAL, DIMENSION( 0:ns-1 ) :: c

  REAL :: chord 
  INTEGER :: i

  chord = 0.0

  !print*,'FUNCTION calc_chord_nrel2p8'
  
  r(0) = 2.043
  r(1) = 4.162
  r(2) = 6.282
  r(3) = 8.401
  r(4) = 10.52
  r(5) = 12.639
  r(6) = 14.758
  r(7) = 16.878
  r(8) = 18.997
  r(9) = 21.116
  r(10) = 23.235
  r(11) = 25.354
  r(12) = 27.474
  r(13) = 29.593
  r(14) = 31.712
  r(15) = 33.831
  r(16) = 35.95
  r(17) = 38.07
  r(18) = 40.189
  r(19) = 42.308
  r(20) = 44.427
  r(21) = 46.546
  r(22) = 48.666
  r(23) = 50.785
  r(24) = 52.904
  r(25) = 55.023
  r(26) = 57.142
  r(27) = 59.262
  r(28) = 61.381
  r(29) = 63.5
 
  c(0) = 2.566
  c(1) = 3.362
  c(2) = 4.045
  c(3) = 4.535
  c(4) = 4.75
  c(5) = 4.738
  c(6) = 4.683
  c(7) = 4.602
  c(8) = 4.506
  c(9) = 4.38
  c(10) = 4.184
  c(11) = 3.946
  c(12) = 3.703
  c(13) = 3.476
  c(14) = 3.235
  c(15) = 2.995
  c(16) = 2.775
  c(17) = 2.592
  c(18) = 2.425
  c(19) = 2.274
  c(20) = 2.147
  c(21) = 2.052
  c(22) = 1.991
  c(23) = 1.945
  c(24) = 1.888
  c(25) = 1.795
  c(26) = 1.573
  c(27) = 1.206
  c(28) = 0.734
  c(29) = 0.2


  DO i = 1,ns-1
     IF ( ( x .GE. r(i-1) ) .AND. ( x .LT. r(i) ) ) THEN
        chord = c(i-1) + (x - r(i-1))*( ( c(i) - c(i-1) )/( r(i) - r(i-1) ) )
        GOTO 1
     ENDIF
  ENDDO

  IF ( x .LT. r(0) ) chord = 0.0 !No airfoil here, just a cylinder
  IF ( x .EQ. r(ns-1) ) chord = c(ns-1)
  
  IF ( x .GT. r(ns-1) ) THEN 
     PRINT*,'—————————————'
     PRINT*,'radius > blade length in calc_chord_nrel2p8.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
  
1 CONTINUE
  
!PRINT*,x,chord
   
!_________________________________________________________________________
  
END FUNCTION calc_chord_nrel2p8
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_cl_nrel2p8( x,y ) RESULT( cl )
!_________________________________________________________________________
!
! Calls appropriate airfoil section table depending upon radial distance
! from blade root. Data from NREL 2.8 MW
!
! calc_cl1_nrel2p8 uses data from Cylinder1.dat
! calc_cl2_nrel2p8 uses data from Cylinder2.dat
! calc_cl3_nrel2p8 uses data from DU40_A17.dat
! calc_cl4_nrel2p8 uses data from DU35_A17.dat
! calc_cl5_nrel2p8 uses data from DU30_A17.dat
! calc_cl6_nrel2p8 uses data from DU25_A17.dat
! calc_cl7_nrel2p8 uses data from DU21_A17.dat
! calc_cl8_nrel2p8 uses data from NACA64_A17.dat
!
! x = radial distance from hub center
! y = angle of attack
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: x,y

  INTEGER, PARAMETER :: ns = 30

  REAL, DIMENSION( 0:ns-1 ) :: r

  REAL :: cl

  INTEGER :: i

  !print*,'FUNCTION calc_cl_nrel2p8'

  cl = 0.0
  
  r(0) = 2.043
  r(1) = 4.162
  r(2) = 6.282
  r(3) = 8.401
  r(4) = 10.52
  r(5) = 12.639
  r(6) = 14.758
  r(7) = 16.878
  r(8) = 18.997
  r(9) = 21.116
  r(10) = 23.235
  r(11) = 25.354
  r(12) = 27.474
  r(13) = 29.593
  r(14) = 31.712
  r(15) = 33.831
  r(16) = 35.95
  r(17) = 38.07
  r(18) = 40.189
  r(19) = 42.308
  r(20) = 44.427
  r(21) = 46.546
  r(22) = 48.666
  r(23) = 50.785
  r(24) = 52.904
  r(25) = 55.023
  r(26) = 57.142
  r(27) = 59.262
  r(28) = 61.381
  r(29) = 63.5
  
  IF ( ( x .GT. r( 0) ) .AND. ( x .LE. r( 1) ) ) THEN
        cl = calc_cl1_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 1) ) .AND. ( x .LE. r(2) ) ) THEN
	cl = calc_cl2_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 2) ) .AND. ( x .LE. r(3) ) ) THEN
	cl = calc_cl3_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 3) ) .AND. ( x .LE. r(4) ) ) THEN
	cl = calc_cl4_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 4) ) .AND. ( x .LE. r(5) ) ) THEN
	cl = calc_cl5_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 5) ) .AND. ( x .LE. r(6) ) ) THEN
	cl = calc_cl6_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 6) ) .AND. ( x .LE. r(7) ) ) THEN
	cl = calc_cl7_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 7) ) .AND. ( x .LE. r(8) ) ) THEN
	cl = calc_cl8_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 8) ) .AND. ( x .LE. r(9) ) ) THEN
	cl = calc_cl9_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 9) ) .AND. ( x .LE. r(10) ) ) THEN
	cl = calc_cl10_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 10) ) .AND. ( x .LE. r(11) ) ) THEN
	cl = calc_cl11_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 11) ) .AND. ( x .LE. r(12) ) ) THEN
	cl = calc_cl12_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 12) ) .AND. ( x .LE. r(13) ) ) THEN
	cl = calc_cl13_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 13) ) .AND. ( x .LE. r(14) ) ) THEN
	cl = calc_cl14_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 14) ) .AND. ( x .LE. r(15) ) ) THEN
	cl = calc_cl15_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 15) ) .AND. ( x .LE. r(16) ) ) THEN
	cl = calc_cl16_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 16) ) .AND. ( x .LE. r(17) ) ) THEN
	cl = calc_cl17_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 17) ) .AND. ( x .LE. r(18) ) ) THEN
	cl = calc_cl18_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 18) ) .AND. ( x .LE. r(19) ) ) THEN
	cl = calc_cl19_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 19) ) .AND. ( x .LE. r(20) ) ) THEN
	cl = calc_cl20_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 20) ) .AND. ( x .LE. r(21) ) ) THEN
	cl = calc_cl21_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 21) ) .AND. ( x .LE. r(22) ) ) THEN
	cl = calc_cl22_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 22) ) .AND. ( x .LE. r(23) ) ) THEN
	cl = calc_cl23_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 23) ) .AND. ( x .LE. r(24) ) ) THEN
	cl = calc_cl24_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 24) ) .AND. ( x .LE. r(25) ) ) THEN
	cl = calc_cl25_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 25) ) .AND. ( x .LE. r(26) ) ) THEN
	cl = calc_cl26_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 26) ) .AND. ( x .LE. r(27) ) ) THEN
	cl = calc_cl27_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 27) ) .AND. ( x .LE. r(28) ) ) THEN
	cl = calc_cl28_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 28) ) .AND. ( x .LE. r(29) ) ) THEN
	cl = calc_cl29_nrel2p8(y)
	ELSEIF ( ( x .GT. r( 29) ) .AND. ( x .LE. r(30) ) ) THEN
	cl = calc_cl30_nrel2p8(y)
  ENDIF
  
  IF ( x .LT. r(0) ) THEN 
     PRINT*,'—————————————'
     PRINT*,'radius less than blade root in calc_cl_nrel2p8.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
  IF ( x .GT. r(ns-1) ) THEN 
     PRINT*,'—————————————'
     PRINT*,'radius greater than blade length in calc_cl_nrel2p8.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
  
!_________________________________________________________________________
  
END FUNCTION calc_cl_nrel2p8
!_________________________________________________________________________


FUNCTION calc_cl1_nrel2p8( y ) RESULT( cl1 )
!_________________________________________________________________________
!
! Data from NREL 2.8 MW: Cylinder1
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL :: cl1

  cl1 = 0.0001
!_________________________________________________________________________
  
END FUNCTION calc_cl1_nrel2p8
!_________________________________________________________________________


!=========================================================================
FUNCTION calc_cl2_nrel2p8( y ) RESULT( cl2 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl2
INTEGER :: i

 cl2 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.0062]
  cl(0:1, 2  ) = [-174.0, 0.0125]
  cl(0:1, 3  ) = [-171.0, 0.0188]
  cl(0:1, 4  ) = [-168.0, 0.0234]
  cl(0:1, 5  ) = [-165.0, 0.0272]
  cl(0:1, 6  ) = [-162.0, 0.0311]
  cl(0:1, 7  ) = [-159.0, 0.0336]
  cl(0:1, 8  ) = [-156.0, 0.0338]
  cl(0:1, 9  ) = [-153.0, 0.0339]
  cl(0:1, 10  ) = [-150.0, 0.034]
  cl(0:1, 11  ) = [-147.0, 0.034]
  cl(0:1, 12  ) = [-144.0, 0.034]
  cl(0:1, 13  ) = [-141.0, 0.0341]
  cl(0:1, 14  ) = [-138.0, 0.0337]
  cl(0:1, 15  ) = [-135.0, 0.033]
  cl(0:1, 16  ) = [-132.0, 0.0324]
  cl(0:1, 17  ) = [-129.0, 0.0315]
  cl(0:1, 18  ) = [-126.0, 0.03]
  cl(0:1, 19  ) = [-123.0, 0.0285]
  cl(0:1, 20  ) = [-120.0, 0.0271]
  cl(0:1, 21  ) = [-117.0, 0.0248]
  cl(0:1, 22  ) = [-114.0, 0.0226]
  cl(0:1, 23  ) = [-111.0, 0.0203]
  cl(0:1, 24  ) = [-108.0, 0.0177]
  cl(0:1, 25  ) = [-105.0, 0.0149]
  cl(0:1, 26  ) = [-102.0, 0.0121]
  cl(0:1, 27  ) = [-99.0, 0.0092]
  cl(0:1, 28  ) = [-96.0, 0.0061]
  cl(0:1, 29  ) = [-93.0, 0.0031]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.0031]
  cl(0:1, 32  ) = [-84.0, -0.0061]
  cl(0:1, 33  ) = [-81.0, -0.0092]
  cl(0:1, 34  ) = [-78.0, -0.0121]
  cl(0:1, 35  ) = [-75.0, -0.0149]
  cl(0:1, 36  ) = [-72.0, -0.0177]
  cl(0:1, 37  ) = [-69.0, -0.0203]
  cl(0:1, 38  ) = [-66.0, -0.0226]
  cl(0:1, 39  ) = [-63.0, -0.0248]
  cl(0:1, 40  ) = [-60.0, -0.0271]
  cl(0:1, 41  ) = [-57.0, -0.0285]
  cl(0:1, 42  ) = [-54.0, -0.03]
  cl(0:1, 43  ) = [-51.0, -0.0315]
  cl(0:1, 44  ) = [-48.0, -0.0324]
  cl(0:1, 45  ) = [-45.0, -0.033]
  cl(0:1, 46  ) = [-42.0, -0.0337]
  cl(0:1, 47  ) = [-39.0, -0.0341]
  cl(0:1, 48  ) = [-36.0, -0.034]
  cl(0:1, 49  ) = [-33.0, -0.034]
  cl(0:1, 50  ) = [-30.0, -0.034]
  cl(0:1, 51  ) = [-29.3939, -0.0339]
  cl(0:1, 52  ) = [-28.7879, -0.0339]
  cl(0:1, 53  ) = [-28.1818, -0.0339]
  cl(0:1, 54  ) = [-27.5758, -0.0339]
  cl(0:1, 55  ) = [-26.9697, -0.0339]
  cl(0:1, 56  ) = [-26.3636, -0.0338]
  cl(0:1, 57  ) = [-25.7576, -0.0338]
  cl(0:1, 58  ) = [-25.1515, -0.0338]
  cl(0:1, 59  ) = [-24.5455, -0.0338]
  cl(0:1, 60  ) = [-23.9394, -0.0337]
  cl(0:1, 61  ) = [-23.3333, -0.0337]
  cl(0:1, 62  ) = [-22.7273, -0.0337]
  cl(0:1, 63  ) = [-22.1212, -0.0337]
  cl(0:1, 64  ) = [-21.5152, -0.0337]
  cl(0:1, 65  ) = [-20.9091, -0.0336]
  cl(0:1, 66  ) = [-20.303, -0.0336]
  cl(0:1, 67  ) = [-19.697, -0.033]
  cl(0:1, 68  ) = [-19.0909, -0.0318]
  cl(0:1, 69  ) = [-18.4848, -0.0305]
  cl(0:1, 70  ) = [-17.8788, -0.0293]
  cl(0:1, 71  ) = [-17.2727, -0.028]
  cl(0:1, 72  ) = [-16.6667, -0.0268]
  cl(0:1, 73  ) = [-16.0606, -0.0255]
  cl(0:1, 74  ) = [-15.4545, -0.0243]
  cl(0:1, 75  ) = [-14.8485, -0.0231]
  cl(0:1, 76  ) = [-14.2424, -0.0218]
  cl(0:1, 77  ) = [-13.6364, -0.0206]
  cl(0:1, 78  ) = [-13.0303, -0.0193]
  cl(0:1, 79  ) = [-12.4242, -0.0181]
  cl(0:1, 80  ) = [-11.8182, -0.0169]
  cl(0:1, 81  ) = [-11.2121, -0.0156]
  cl(0:1, 82  ) = [-10.6061, -0.0144]
  cl(0:1, 83  ) = [-10.0, -0.0132]
  cl(0:1, 84  ) = [-9.3939, -0.0111]
  cl(0:1, 85  ) = [-8.7879, -0.0091]
  cl(0:1, 86  ) = [-8.1818, -0.0071]
  cl(0:1, 87  ) = [-7.5758, -0.0051]
  cl(0:1, 88  ) = [-6.9697, -0.0031]
  cl(0:1, 89  ) = [-6.3636, -0.0011]
  cl(0:1, 90  ) = [-5.7576, -0.0003]
  cl(0:1, 91  ) = [-5.1515, -0.0002]
  cl(0:1, 92  ) = [-4.5455, -0.0001]
  cl(0:1, 93  ) = [-3.9394, -0.0001]
  cl(0:1, 94  ) = [-3.3333, -0.0]
  cl(0:1, 95  ) = [-2.7273, 0.0025]
  cl(0:1, 96  ) = [-2.1212, 0.0097]
  cl(0:1, 97  ) = [-1.5152, 0.0169]
  cl(0:1, 98  ) = [-0.9091, 0.0216]
  cl(0:1, 99  ) = [-0.303, 0.0242]
  cl(0:1, 100  ) = [0.303, 0.0269]
  cl(0:1, 101  ) = [0.9091, 0.0296]
  cl(0:1, 102  ) = [1.5152, 0.0322]
  cl(0:1, 103  ) = [2.1212, 0.0367]
  cl(0:1, 104  ) = [2.7273, 0.042]
  cl(0:1, 105  ) = [3.3333, 0.0473]
  cl(0:1, 106  ) = [3.9394, 0.0526]
  cl(0:1, 107  ) = [4.5455, 0.0579]
  cl(0:1, 108  ) = [5.1515, 0.0632]
  cl(0:1, 109  ) = [5.7576, 0.0679]
  cl(0:1, 110  ) = [6.3636, 0.0715]
  cl(0:1, 111  ) = [6.9697, 0.075]
  cl(0:1, 112  ) = [7.5758, 0.0786]
  cl(0:1, 113  ) = [8.1818, 0.0827]
  cl(0:1, 114  ) = [8.7879, 0.086]
  cl(0:1, 115  ) = [9.3939, 0.0864]
  cl(0:1, 116  ) = [10.0, 0.0861]
  cl(0:1, 117  ) = [10.6061, 0.0115]
  cl(0:1, 118  ) = [11.2121, 0.0018]
  cl(0:1, 119  ) = [11.8182, 0.0016]
  cl(0:1, 120  ) = [12.4242, 0.0538]
  cl(0:1, 121  ) = [13.0303, 0.0707]
  cl(0:1, 122  ) = [13.6364, 0.0682]
  cl(0:1, 123  ) = [14.2424, 0.065]
  cl(0:1, 124  ) = [14.8485, 0.06]
  cl(0:1, 125  ) = [15.4545, 0.0543]
  cl(0:1, 126  ) = [16.0606, 0.0489]
  cl(0:1, 127  ) = [16.6667, 0.049]
  cl(0:1, 128  ) = [17.2727, 0.049]
  cl(0:1, 129  ) = [17.8788, 0.0488]
  cl(0:1, 130  ) = [18.4848, 0.0485]
  cl(0:1, 131  ) = [19.0909, 0.0483]
  cl(0:1, 132  ) = [19.697, 0.0481]
  cl(0:1, 133  ) = [20.303, 0.048]
  cl(0:1, 134  ) = [20.9091, 0.0481]
  cl(0:1, 135  ) = [21.5152, 0.0481]
  cl(0:1, 136  ) = [22.1212, 0.0481]
  cl(0:1, 137  ) = [22.7273, 0.0481]
  cl(0:1, 138  ) = [23.3333, 0.0482]
  cl(0:1, 139  ) = [23.9394, 0.0482]
  cl(0:1, 140  ) = [24.5455, 0.0482]
  cl(0:1, 141  ) = [25.1515, 0.0483]
  cl(0:1, 142  ) = [25.7576, 0.0483]
  cl(0:1, 143  ) = [26.3636, 0.0483]
  cl(0:1, 144  ) = [26.9697, 0.0484]
  cl(0:1, 145  ) = [27.5758, 0.0484]
  cl(0:1, 146  ) = [28.1818, 0.0484]
  cl(0:1, 147  ) = [28.7879, 0.0485]
  cl(0:1, 148  ) = [29.3939, 0.0485]
  cl(0:1, 149  ) = [30.0, 0.0485]
  cl(0:1, 150  ) = [33.0, 0.0486]
  cl(0:1, 151  ) = [36.0, 0.0486]
  cl(0:1, 152  ) = [39.0, 0.0487]
  cl(0:1, 153  ) = [42.0, 0.0481]
  cl(0:1, 154  ) = [45.0, 0.0472]
  cl(0:1, 155  ) = [48.0, 0.0463]
  cl(0:1, 156  ) = [51.0, 0.045]
  cl(0:1, 157  ) = [54.0, 0.0429]
  cl(0:1, 158  ) = [57.0, 0.0408]
  cl(0:1, 159  ) = [60.0, 0.0386]
  cl(0:1, 160  ) = [63.0, 0.0354]
  cl(0:1, 161  ) = [66.0, 0.0322]
  cl(0:1, 162  ) = [69.0, 0.029]
  cl(0:1, 163  ) = [72.0, 0.0253]
  cl(0:1, 164  ) = [75.0, 0.0213]
  cl(0:1, 165  ) = [78.0, 0.0173]
  cl(0:1, 166  ) = [81.0, 0.0131]
  cl(0:1, 167  ) = [84.0, 0.0088]
  cl(0:1, 168  ) = [87.0, 0.0044]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.0031]
  cl(0:1, 171  ) = [96.0, -0.0061]
  cl(0:1, 172  ) = [99.0, -0.0092]
  cl(0:1, 173  ) = [102.0, -0.0121]
  cl(0:1, 174  ) = [105.0, -0.0149]
  cl(0:1, 175  ) = [108.0, -0.0177]
  cl(0:1, 176  ) = [111.0, -0.0203]
  cl(0:1, 177  ) = [114.0, -0.0226]
  cl(0:1, 178  ) = [117.0, -0.0248]
  cl(0:1, 179  ) = [120.0, -0.0271]
  cl(0:1, 180  ) = [123.0, -0.0285]
  cl(0:1, 181  ) = [126.0, -0.03]
  cl(0:1, 182  ) = [129.0, -0.0315]
  cl(0:1, 183  ) = [132.0, -0.0324]
  cl(0:1, 184  ) = [135.0, -0.033]
  cl(0:1, 185  ) = [138.0, -0.0337]
  cl(0:1, 186  ) = [141.0, -0.0341]
  cl(0:1, 187  ) = [144.0, -0.034]
  cl(0:1, 188  ) = [147.0, -0.034]
  cl(0:1, 189  ) = [150.0, -0.034]
  cl(0:1, 190  ) = [153.0, -0.0339]
  cl(0:1, 191  ) = [156.0, -0.0338]
  cl(0:1, 192  ) = [159.0, -0.0336]
  cl(0:1, 193  ) = [162.0, -0.0311]
  cl(0:1, 194  ) = [165.0, -0.0272]
  cl(0:1, 195  ) = [168.0, -0.0234]
  cl(0:1, 196  ) = [171.0, -0.0188]
  cl(0:1, 197  ) = [174.0, -0.0125]
  cl(0:1, 198  ) = [177.0, -0.0062]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl2 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl2 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl2 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl2_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl2 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl2_nrel2p8.pro' 
  ENDIF

!print,y,cl2

!_________________________________________________________________________
END FUNCTION calc_cl2_nrel2p8
!_________________________________________________________________________


!_________________________________________________________________________
FUNCTION calc_cl3_nrel2p8( y ) RESULT( cl3 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl3
INTEGER :: i

 cl3 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.0528]
  cl(0:1, 2  ) = [-174.0, 0.1061]
  cl(0:1, 3  ) = [-171.0, 0.1592]
  cl(0:1, 4  ) = [-168.0, 0.1974]
  cl(0:1, 5  ) = [-165.0, 0.2284]
  cl(0:1, 6  ) = [-162.0, 0.2593]
  cl(0:1, 7  ) = [-159.0, 0.2798]
  cl(0:1, 8  ) = [-156.0, 0.2793]
  cl(0:1, 9  ) = [-153.0, 0.2789]
  cl(0:1, 10  ) = [-150.0, 0.2787]
  cl(0:1, 11  ) = [-147.0, 0.2785]
  cl(0:1, 12  ) = [-144.0, 0.2783]
  cl(0:1, 13  ) = [-141.0, 0.2782]
  cl(0:1, 14  ) = [-138.0, 0.2746]
  cl(0:1, 15  ) = [-135.0, 0.2693]
  cl(0:1, 16  ) = [-132.0, 0.2639]
  cl(0:1, 17  ) = [-129.0, 0.2564]
  cl(0:1, 18  ) = [-126.0, 0.2443]
  cl(0:1, 19  ) = [-123.0, 0.2322]
  cl(0:1, 20  ) = [-120.0, 0.2201]
  cl(0:1, 21  ) = [-117.0, 0.2018]
  cl(0:1, 22  ) = [-114.0, 0.1835]
  cl(0:1, 23  ) = [-111.0, 0.1652]
  cl(0:1, 24  ) = [-108.0, 0.1439]
  cl(0:1, 25  ) = [-105.0, 0.1211]
  cl(0:1, 26  ) = [-102.0, 0.0982]
  cl(0:1, 27  ) = [-99.0, 0.0747]
  cl(0:1, 28  ) = [-96.0, 0.0498]
  cl(0:1, 29  ) = [-93.0, 0.0249]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.0249]
  cl(0:1, 32  ) = [-84.0, -0.0498]
  cl(0:1, 33  ) = [-81.0, -0.0747]
  cl(0:1, 34  ) = [-78.0, -0.0982]
  cl(0:1, 35  ) = [-75.0, -0.1211]
  cl(0:1, 36  ) = [-72.0, -0.1439]
  cl(0:1, 37  ) = [-69.0, -0.1652]
  cl(0:1, 38  ) = [-66.0, -0.1835]
  cl(0:1, 39  ) = [-63.0, -0.2018]
  cl(0:1, 40  ) = [-60.0, -0.2201]
  cl(0:1, 41  ) = [-57.0, -0.2322]
  cl(0:1, 42  ) = [-54.0, -0.2443]
  cl(0:1, 43  ) = [-51.0, -0.2564]
  cl(0:1, 44  ) = [-48.0, -0.2639]
  cl(0:1, 45  ) = [-45.0, -0.2693]
  cl(0:1, 46  ) = [-42.0, -0.2746]
  cl(0:1, 47  ) = [-39.0, -0.2782]
  cl(0:1, 48  ) = [-36.0, -0.2783]
  cl(0:1, 49  ) = [-33.0, -0.2785]
  cl(0:1, 50  ) = [-30.0, -0.2787]
  cl(0:1, 51  ) = [-29.3939, -0.2787]
  cl(0:1, 52  ) = [-28.7879, -0.2788]
  cl(0:1, 53  ) = [-28.1818, -0.2788]
  cl(0:1, 54  ) = [-27.5758, -0.2789]
  cl(0:1, 55  ) = [-26.9697, -0.2789]
  cl(0:1, 56  ) = [-26.3636, -0.279]
  cl(0:1, 57  ) = [-25.7576, -0.2791]
  cl(0:1, 58  ) = [-25.1515, -0.2791]
  cl(0:1, 59  ) = [-24.5455, -0.2792]
  cl(0:1, 60  ) = [-23.9394, -0.2793]
  cl(0:1, 61  ) = [-23.3333, -0.2794]
  cl(0:1, 62  ) = [-22.7273, -0.2795]
  cl(0:1, 63  ) = [-22.1212, -0.2796]
  cl(0:1, 64  ) = [-21.5152, -0.2797]
  cl(0:1, 65  ) = [-20.9091, -0.2798]
  cl(0:1, 66  ) = [-20.303, -0.28]
  cl(0:1, 67  ) = [-19.697, -0.2752]
  cl(0:1, 68  ) = [-19.0909, -0.2655]
  cl(0:1, 69  ) = [-18.4848, -0.2559]
  cl(0:1, 70  ) = [-17.8788, -0.2462]
  cl(0:1, 71  ) = [-17.2727, -0.2366]
  cl(0:1, 72  ) = [-16.6667, -0.2271]
  cl(0:1, 73  ) = [-16.0606, -0.2175]
  cl(0:1, 74  ) = [-15.4545, -0.208]
  cl(0:1, 75  ) = [-14.8485, -0.1985]
  cl(0:1, 76  ) = [-14.2424, -0.1891]
  cl(0:1, 77  ) = [-13.6364, -0.1797]
  cl(0:1, 78  ) = [-13.0303, -0.1703]
  cl(0:1, 79  ) = [-12.4242, -0.161]
  cl(0:1, 80  ) = [-11.8182, -0.1517]
  cl(0:1, 81  ) = [-11.2121, -0.1424]
  cl(0:1, 82  ) = [-10.6061, -0.1332]
  cl(0:1, 83  ) = [-10.0, -0.124]
  cl(0:1, 84  ) = [-9.3939, -0.1084]
  cl(0:1, 85  ) = [-8.7879, -0.093]
  cl(0:1, 86  ) = [-8.1818, -0.0776]
  cl(0:1, 87  ) = [-7.5758, -0.0624]
  cl(0:1, 88  ) = [-6.9697, -0.0474]
  cl(0:1, 89  ) = [-6.3636, -0.0325]
  cl(0:1, 90  ) = [-5.7576, -0.0233]
  cl(0:1, 91  ) = [-5.1515, -0.0175]
  cl(0:1, 92  ) = [-4.5455, -0.0122]
  cl(0:1, 93  ) = [-3.9394, -0.0074]
  cl(0:1, 94  ) = [-3.3333, -0.0029]
  cl(0:1, 95  ) = [-2.7273, 0.0169]
  cl(0:1, 96  ) = [-2.1212, 0.0654]
  cl(0:1, 97  ) = [-1.5152, 0.114]
  cl(0:1, 98  ) = [-0.9091, 0.1513]
  cl(0:1, 99  ) = [-0.303, 0.1798]
  cl(0:1, 100  ) = [0.303, 0.2083]
  cl(0:1, 101  ) = [0.9091, 0.2368]
  cl(0:1, 102  ) = [1.5152, 0.2621]
  cl(0:1, 103  ) = [2.1212, 0.2982]
  cl(0:1, 104  ) = [2.7273, 0.3413]
  cl(0:1, 105  ) = [3.3333, 0.3843]
  cl(0:1, 106  ) = [3.9394, 0.4274]
  cl(0:1, 107  ) = [4.5455, 0.4705]
  cl(0:1, 108  ) = [5.1515, 0.5136]
  cl(0:1, 109  ) = [5.7576, 0.5517]
  cl(0:1, 110  ) = [6.3636, 0.5815]
  cl(0:1, 111  ) = [6.9697, 0.6103]
  cl(0:1, 112  ) = [7.5758, 0.6396]
  cl(0:1, 113  ) = [8.1818, 0.6737]
  cl(0:1, 114  ) = [8.7879, 0.7006]
  cl(0:1, 115  ) = [9.3939, 0.706]
  cl(0:1, 116  ) = [10.0, 0.7069]
  cl(0:1, 117  ) = [10.6061, 0.224]
  cl(0:1, 118  ) = [11.2121, 0.1403]
  cl(0:1, 119  ) = [11.8182, 0.1314]
  cl(0:1, 120  ) = [12.4242, 0.446]
  cl(0:1, 121  ) = [13.0303, 0.556]
  cl(0:1, 122  ) = [13.6364, 0.5442]
  cl(0:1, 123  ) = [14.2424, 0.5279]
  cl(0:1, 124  ) = [14.8485, 0.4897]
  cl(0:1, 125  ) = [15.4545, 0.4487]
  cl(0:1, 126  ) = [16.0606, 0.4129]
  cl(0:1, 127  ) = [16.6667, 0.4125]
  cl(0:1, 128  ) = [17.2727, 0.4111]
  cl(0:1, 129  ) = [17.8788, 0.4086]
  cl(0:1, 130  ) = [18.4848, 0.4061]
  cl(0:1, 131  ) = [19.0909, 0.4036]
  cl(0:1, 132  ) = [19.697, 0.4012]
  cl(0:1, 133  ) = [20.303, 0.3998]
  cl(0:1, 134  ) = [20.9091, 0.3997]
  cl(0:1, 135  ) = [21.5152, 0.3995]
  cl(0:1, 136  ) = [22.1212, 0.3994]
  cl(0:1, 137  ) = [22.7273, 0.3992]
  cl(0:1, 138  ) = [23.3333, 0.3991]
  cl(0:1, 139  ) = [23.9394, 0.399]
  cl(0:1, 140  ) = [24.5455, 0.3989]
  cl(0:1, 141  ) = [25.1515, 0.3988]
  cl(0:1, 142  ) = [25.7576, 0.3987]
  cl(0:1, 143  ) = [26.3636, 0.3986]
  cl(0:1, 144  ) = [26.9697, 0.3985]
  cl(0:1, 145  ) = [27.5758, 0.3984]
  cl(0:1, 146  ) = [28.1818, 0.3983]
  cl(0:1, 147  ) = [28.7879, 0.3983]
  cl(0:1, 148  ) = [29.3939, 0.3982]
  cl(0:1, 149  ) = [30.0, 0.3982]
  cl(0:1, 150  ) = [33.0, 0.3979]
  cl(0:1, 151  ) = [36.0, 0.3976]
  cl(0:1, 152  ) = [39.0, 0.3975]
  cl(0:1, 153  ) = [42.0, 0.3923]
  cl(0:1, 154  ) = [45.0, 0.3846]
  cl(0:1, 155  ) = [48.0, 0.3769]
  cl(0:1, 156  ) = [51.0, 0.366]
  cl(0:1, 157  ) = [54.0, 0.3488]
  cl(0:1, 158  ) = [57.0, 0.3315]
  cl(0:1, 159  ) = [60.0, 0.3143]
  cl(0:1, 160  ) = [63.0, 0.2882]
  cl(0:1, 161  ) = [66.0, 0.2621]
  cl(0:1, 162  ) = [69.0, 0.2361]
  cl(0:1, 163  ) = [72.0, 0.2056]
  cl(0:1, 164  ) = [75.0, 0.173]
  cl(0:1, 165  ) = [78.0, 0.1404]
  cl(0:1, 166  ) = [81.0, 0.1068]
  cl(0:1, 167  ) = [84.0, 0.0712]
  cl(0:1, 168  ) = [87.0, 0.0356]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.0249]
  cl(0:1, 171  ) = [96.0, -0.0498]
  cl(0:1, 172  ) = [99.0, -0.0747]
  cl(0:1, 173  ) = [102.0, -0.0982]
  cl(0:1, 174  ) = [105.0, -0.1211]
  cl(0:1, 175  ) = [108.0, -0.1439]
  cl(0:1, 176  ) = [111.0, -0.1652]
  cl(0:1, 177  ) = [114.0, -0.1835]
  cl(0:1, 178  ) = [117.0, -0.2018]
  cl(0:1, 179  ) = [120.0, -0.2201]
  cl(0:1, 180  ) = [123.0, -0.2322]
  cl(0:1, 181  ) = [126.0, -0.2443]
  cl(0:1, 182  ) = [129.0, -0.2564]
  cl(0:1, 183  ) = [132.0, -0.2639]
  cl(0:1, 184  ) = [135.0, -0.2693]
  cl(0:1, 185  ) = [138.0, -0.2746]
  cl(0:1, 186  ) = [141.0, -0.2782]
  cl(0:1, 187  ) = [144.0, -0.2783]
  cl(0:1, 188  ) = [147.0, -0.2785]
  cl(0:1, 189  ) = [150.0, -0.2787]
  cl(0:1, 190  ) = [153.0, -0.2789]
  cl(0:1, 191  ) = [156.0, -0.2793]
  cl(0:1, 192  ) = [159.0, -0.2798]
  cl(0:1, 193  ) = [162.0, -0.2593]
  cl(0:1, 194  ) = [165.0, -0.2284]
  cl(0:1, 195  ) = [168.0, -0.1974]
  cl(0:1, 196  ) = [171.0, -0.1592]
  cl(0:1, 197  ) = [174.0, -0.1061]
  cl(0:1, 198  ) = [177.0, -0.0528]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl3 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl3 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl3 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl3_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl3 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl3_nrel2p8.pro' 
  ENDIF

!print,y,cl3

END FUNCTION calc_cl3_nrel2p8
!_________________________________________________________________________


!_________________________________________________________________________
FUNCTION calc_cl4_nrel2p8( y ) RESULT( cl4 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl4
INTEGER :: i

 cl4 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.104]
  cl(0:1, 2  ) = [-174.0, 0.2089]
  cl(0:1, 3  ) = [-171.0, 0.3134]
  cl(0:1, 4  ) = [-168.0, 0.3851]
  cl(0:1, 5  ) = [-165.0, 0.4404]
  cl(0:1, 6  ) = [-162.0, 0.4959]
  cl(0:1, 7  ) = [-159.0, 0.5306]
  cl(0:1, 8  ) = [-156.0, 0.5239]
  cl(0:1, 9  ) = [-153.0, 0.5174]
  cl(0:1, 10  ) = [-150.0, 0.5112]
  cl(0:1, 11  ) = [-147.0, 0.5077]
  cl(0:1, 12  ) = [-144.0, 0.5043]
  cl(0:1, 13  ) = [-141.0, 0.5011]
  cl(0:1, 14  ) = [-138.0, 0.4925]
  cl(0:1, 15  ) = [-135.0, 0.4811]
  cl(0:1, 16  ) = [-132.0, 0.4699]
  cl(0:1, 17  ) = [-129.0, 0.4549]
  cl(0:1, 18  ) = [-126.0, 0.4326]
  cl(0:1, 19  ) = [-123.0, 0.4103]
  cl(0:1, 20  ) = [-120.0, 0.388]
  cl(0:1, 21  ) = [-117.0, 0.3553]
  cl(0:1, 22  ) = [-114.0, 0.3226]
  cl(0:1, 23  ) = [-111.0, 0.29]
  cl(0:1, 24  ) = [-108.0, 0.2523]
  cl(0:1, 25  ) = [-105.0, 0.212]
  cl(0:1, 26  ) = [-102.0, 0.1718]
  cl(0:1, 27  ) = [-99.0, 0.1305]
  cl(0:1, 28  ) = [-96.0, 0.087]
  cl(0:1, 29  ) = [-93.0, 0.0435]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.0435]
  cl(0:1, 32  ) = [-84.0, -0.087]
  cl(0:1, 33  ) = [-81.0, -0.1305]
  cl(0:1, 34  ) = [-78.0, -0.1718]
  cl(0:1, 35  ) = [-75.0, -0.212]
  cl(0:1, 36  ) = [-72.0, -0.2523]
  cl(0:1, 37  ) = [-69.0, -0.29]
  cl(0:1, 38  ) = [-66.0, -0.3226]
  cl(0:1, 39  ) = [-63.0, -0.3553]
  cl(0:1, 40  ) = [-60.0, -0.388]
  cl(0:1, 41  ) = [-57.0, -0.4103]
  cl(0:1, 42  ) = [-54.0, -0.4326]
  cl(0:1, 43  ) = [-51.0, -0.4549]
  cl(0:1, 44  ) = [-48.0, -0.4699]
  cl(0:1, 45  ) = [-45.0, -0.4811]
  cl(0:1, 46  ) = [-42.0, -0.4925]
  cl(0:1, 47  ) = [-39.0, -0.5011]
  cl(0:1, 48  ) = [-36.0, -0.5043]
  cl(0:1, 49  ) = [-33.0, -0.5077]
  cl(0:1, 50  ) = [-30.0, -0.5112]
  cl(0:1, 51  ) = [-29.3939, -0.5124]
  cl(0:1, 52  ) = [-28.7879, -0.5137]
  cl(0:1, 53  ) = [-28.1818, -0.5149]
  cl(0:1, 54  ) = [-27.5758, -0.5162]
  cl(0:1, 55  ) = [-26.9697, -0.5175]
  cl(0:1, 56  ) = [-26.3636, -0.5188]
  cl(0:1, 57  ) = [-25.7576, -0.5201]
  cl(0:1, 58  ) = [-25.1515, -0.5214]
  cl(0:1, 59  ) = [-24.5455, -0.5227]
  cl(0:1, 60  ) = [-23.9394, -0.524]
  cl(0:1, 61  ) = [-23.3333, -0.5254]
  cl(0:1, 62  ) = [-22.7273, -0.5267]
  cl(0:1, 63  ) = [-22.1212, -0.5281]
  cl(0:1, 64  ) = [-21.5152, -0.5295]
  cl(0:1, 65  ) = [-20.9091, -0.5308]
  cl(0:1, 66  ) = [-20.303, -0.5322]
  cl(0:1, 67  ) = [-19.697, -0.525]
  cl(0:1, 68  ) = [-19.0909, -0.5093]
  cl(0:1, 69  ) = [-18.4848, -0.4937]
  cl(0:1, 70  ) = [-17.8788, -0.478]
  cl(0:1, 71  ) = [-17.2727, -0.4625]
  cl(0:1, 72  ) = [-16.6667, -0.4469]
  cl(0:1, 73  ) = [-16.0606, -0.4315]
  cl(0:1, 74  ) = [-15.4545, -0.4161]
  cl(0:1, 75  ) = [-14.8485, -0.4007]
  cl(0:1, 76  ) = [-14.2424, -0.3854]
  cl(0:1, 77  ) = [-13.6364, -0.3702]
  cl(0:1, 78  ) = [-13.0303, -0.3551]
  cl(0:1, 79  ) = [-12.4242, -0.34]
  cl(0:1, 80  ) = [-11.8182, -0.325]
  cl(0:1, 81  ) = [-11.2121, -0.3101]
  cl(0:1, 82  ) = [-10.6061, -0.2952]
  cl(0:1, 83  ) = [-10.0, -0.2805]
  cl(0:1, 84  ) = [-9.3939, -0.2534]
  cl(0:1, 85  ) = [-8.7879, -0.2265]
  cl(0:1, 86  ) = [-8.1818, -0.1998]
  cl(0:1, 87  ) = [-7.5758, -0.1734]
  cl(0:1, 88  ) = [-6.9697, -0.1473]
  cl(0:1, 89  ) = [-6.3636, -0.1217]
  cl(0:1, 90  ) = [-5.7576, -0.0985]
  cl(0:1, 91  ) = [-5.1515, -0.077]
  cl(0:1, 92  ) = [-4.5455, -0.0563]
  cl(0:1, 93  ) = [-3.9394, -0.0367]
  cl(0:1, 94  ) = [-3.3333, -0.0163]
  cl(0:1, 95  ) = [-2.7273, 0.0236]
  cl(0:1, 96  ) = [-2.1212, 0.0915]
  cl(0:1, 97  ) = [-1.5152, 0.1595]
  cl(0:1, 98  ) = [-0.9091, 0.2234]
  cl(0:1, 99  ) = [-0.303, 0.2842]
  cl(0:1, 100  ) = [0.303, 0.3451]
  cl(0:1, 101  ) = [0.9091, 0.4059]
  cl(0:1, 102  ) = [1.5152, 0.4604]
  cl(0:1, 103  ) = [2.1212, 0.5262]
  cl(0:1, 104  ) = [2.7273, 0.6008]
  cl(0:1, 105  ) = [3.3333, 0.6755]
  cl(0:1, 106  ) = [3.9394, 0.7503]
  cl(0:1, 107  ) = [4.5455, 0.825]
  cl(0:1, 108  ) = [5.1515, 0.8997]
  cl(0:1, 109  ) = [5.7576, 0.968]
  cl(0:1, 110  ) = [6.3636, 1.0254]
  cl(0:1, 111  ) = [6.9697, 1.0815]
  cl(0:1, 112  ) = [7.5758, 1.1384]
  cl(0:1, 113  ) = [8.1818, 1.2003]
  cl(0:1, 114  ) = [8.7879, 1.2523]
  cl(0:1, 115  ) = [9.3939, 1.2769]
  cl(0:1, 116  ) = [10.0, 1.2963]
  cl(0:1, 117  ) = [10.6061, 0.7614]
  cl(0:1, 118  ) = [11.2121, 0.6157]
  cl(0:1, 119  ) = [11.8182, 0.5896]
  cl(0:1, 120  ) = [12.4242, 0.8401]
  cl(0:1, 121  ) = [13.0303, 0.9365]
  cl(0:1, 122  ) = [13.6364, 0.9308]
  cl(0:1, 123  ) = [14.2424, 0.9215]
  cl(0:1, 124  ) = [14.8485, 0.8799]
  cl(0:1, 125  ) = [15.4545, 0.8391]
  cl(0:1, 126  ) = [16.0606, 0.8059]
  cl(0:1, 127  ) = [16.6667, 0.8008]
  cl(0:1, 128  ) = [17.2727, 0.7947]
  cl(0:1, 129  ) = [17.8788, 0.7872]
  cl(0:1, 130  ) = [18.4848, 0.7798]
  cl(0:1, 131  ) = [19.0909, 0.7723]
  cl(0:1, 132  ) = [19.697, 0.7649]
  cl(0:1, 133  ) = [20.303, 0.7602]
  cl(0:1, 134  ) = [20.9091, 0.7583]
  cl(0:1, 135  ) = [21.5152, 0.7563]
  cl(0:1, 136  ) = [22.1212, 0.7543]
  cl(0:1, 137  ) = [22.7273, 0.7524]
  cl(0:1, 138  ) = [23.3333, 0.7505]
  cl(0:1, 139  ) = [23.9394, 0.7486]
  cl(0:1, 140  ) = [24.5455, 0.7467]
  cl(0:1, 141  ) = [25.1515, 0.7448]
  cl(0:1, 142  ) = [25.7576, 0.743]
  cl(0:1, 143  ) = [26.3636, 0.7411]
  cl(0:1, 144  ) = [26.9697, 0.7393]
  cl(0:1, 145  ) = [27.5758, 0.7375]
  cl(0:1, 146  ) = [28.1818, 0.7357]
  cl(0:1, 147  ) = [28.7879, 0.7339]
  cl(0:1, 148  ) = [29.3939, 0.7321]
  cl(0:1, 149  ) = [30.0, 0.7304]
  cl(0:1, 150  ) = [33.0, 0.7254]
  cl(0:1, 151  ) = [36.0, 0.7205]
  cl(0:1, 152  ) = [39.0, 0.7158]
  cl(0:1, 153  ) = [42.0, 0.7034]
  cl(0:1, 154  ) = [45.0, 0.6872]
  cl(0:1, 155  ) = [48.0, 0.671]
  cl(0:1, 156  ) = [51.0, 0.6497]
  cl(0:1, 157  ) = [54.0, 0.6178]
  cl(0:1, 158  ) = [57.0, 0.5859]
  cl(0:1, 159  ) = [60.0, 0.5541]
  cl(0:1, 160  ) = [63.0, 0.5075]
  cl(0:1, 161  ) = [66.0, 0.4609]
  cl(0:1, 162  ) = [69.0, 0.4143]
  cl(0:1, 163  ) = [72.0, 0.3605]
  cl(0:1, 164  ) = [75.0, 0.303]
  cl(0:1, 165  ) = [78.0, 0.2455]
  cl(0:1, 166  ) = [81.0, 0.1865]
  cl(0:1, 167  ) = [84.0, 0.1243]
  cl(0:1, 168  ) = [87.0, 0.0622]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.0435]
  cl(0:1, 171  ) = [96.0, -0.087]
  cl(0:1, 172  ) = [99.0, -0.1305]
  cl(0:1, 173  ) = [102.0, -0.1718]
  cl(0:1, 174  ) = [105.0, -0.212]
  cl(0:1, 175  ) = [108.0, -0.2523]
  cl(0:1, 176  ) = [111.0, -0.29]
  cl(0:1, 177  ) = [114.0, -0.3226]
  cl(0:1, 178  ) = [117.0, -0.3553]
  cl(0:1, 179  ) = [120.0, -0.388]
  cl(0:1, 180  ) = [123.0, -0.4103]
  cl(0:1, 181  ) = [126.0, -0.4326]
  cl(0:1, 182  ) = [129.0, -0.4549]
  cl(0:1, 183  ) = [132.0, -0.4699]
  cl(0:1, 184  ) = [135.0, -0.4811]
  cl(0:1, 185  ) = [138.0, -0.4925]
  cl(0:1, 186  ) = [141.0, -0.5011]
  cl(0:1, 187  ) = [144.0, -0.5043]
  cl(0:1, 188  ) = [147.0, -0.5077]
  cl(0:1, 189  ) = [150.0, -0.5112]
  cl(0:1, 190  ) = [153.0, -0.5174]
  cl(0:1, 191  ) = [156.0, -0.5239]
  cl(0:1, 192  ) = [159.0, -0.5306]
  cl(0:1, 193  ) = [162.0, -0.4959]
  cl(0:1, 194  ) = [165.0, -0.4404]
  cl(0:1, 195  ) = [168.0, -0.3851]
  cl(0:1, 196  ) = [171.0, -0.3134]
  cl(0:1, 197  ) = [174.0, -0.2089]
  cl(0:1, 198  ) = [177.0, -0.104]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl4 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl4 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl4 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl4_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl4 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl4_nrel2p8.pro' 
  ENDIF

!print,y,cl4

END FUNCTION calc_cl4_nrel2p8
!_________________________________________________________________________


!_________________________________________________________________________
FUNCTION calc_cl5_nrel2p8( y ) RESULT( cl5 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl5
INTEGER :: i

 cl5 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.1317]
  cl(0:1, 2  ) = [-174.0, 0.2646]
  cl(0:1, 3  ) = [-171.0, 0.3969]
  cl(0:1, 4  ) = [-168.0, 0.4837]
  cl(0:1, 5  ) = [-165.0, 0.5477]
  cl(0:1, 6  ) = [-162.0, 0.6117]
  cl(0:1, 7  ) = [-159.0, 0.6491]
  cl(0:1, 8  ) = [-156.0, 0.6336]
  cl(0:1, 9  ) = [-153.0, 0.6181]
  cl(0:1, 10  ) = [-150.0, 0.6026]
  cl(0:1, 11  ) = [-147.0, 0.5939]
  cl(0:1, 12  ) = [-144.0, 0.5851]
  cl(0:1, 13  ) = [-141.0, 0.5764]
  cl(0:1, 14  ) = [-138.0, 0.563]
  cl(0:1, 15  ) = [-135.0, 0.5472]
  cl(0:1, 16  ) = [-132.0, 0.5313]
  cl(0:1, 17  ) = [-129.0, 0.5119]
  cl(0:1, 18  ) = [-126.0, 0.4852]
  cl(0:1, 19  ) = [-123.0, 0.4585]
  cl(0:1, 20  ) = [-120.0, 0.4318]
  cl(0:1, 21  ) = [-117.0, 0.3946]
  cl(0:1, 22  ) = [-114.0, 0.3574]
  cl(0:1, 23  ) = [-111.0, 0.3202]
  cl(0:1, 24  ) = [-108.0, 0.278]
  cl(0:1, 25  ) = [-105.0, 0.2332]
  cl(0:1, 26  ) = [-102.0, 0.1885]
  cl(0:1, 27  ) = [-99.0, 0.1428]
  cl(0:1, 28  ) = [-96.0, 0.0952]
  cl(0:1, 29  ) = [-93.0, 0.0476]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.0476]
  cl(0:1, 32  ) = [-84.0, -0.0952]
  cl(0:1, 33  ) = [-81.0, -0.1428]
  cl(0:1, 34  ) = [-78.0, -0.1885]
  cl(0:1, 35  ) = [-75.0, -0.2332]
  cl(0:1, 36  ) = [-72.0, -0.278]
  cl(0:1, 37  ) = [-69.0, -0.3202]
  cl(0:1, 38  ) = [-66.0, -0.3574]
  cl(0:1, 39  ) = [-63.0, -0.3946]
  cl(0:1, 40  ) = [-60.0, -0.4318]
  cl(0:1, 41  ) = [-57.0, -0.4585]
  cl(0:1, 42  ) = [-54.0, -0.4852]
  cl(0:1, 43  ) = [-51.0, -0.5119]
  cl(0:1, 44  ) = [-48.0, -0.5313]
  cl(0:1, 45  ) = [-45.0, -0.5472]
  cl(0:1, 46  ) = [-42.0, -0.563]
  cl(0:1, 47  ) = [-39.0, -0.5764]
  cl(0:1, 48  ) = [-36.0, -0.5851]
  cl(0:1, 49  ) = [-33.0, -0.5939]
  cl(0:1, 50  ) = [-30.0, -0.6026]
  cl(0:1, 51  ) = [-29.3939, -0.6057]
  cl(0:1, 52  ) = [-28.7879, -0.6088]
  cl(0:1, 53  ) = [-28.1818, -0.612]
  cl(0:1, 54  ) = [-27.5758, -0.6151]
  cl(0:1, 55  ) = [-26.9697, -0.6183]
  cl(0:1, 56  ) = [-26.3636, -0.6214]
  cl(0:1, 57  ) = [-25.7576, -0.6245]
  cl(0:1, 58  ) = [-25.1515, -0.6277]
  cl(0:1, 59  ) = [-24.5455, -0.6308]
  cl(0:1, 60  ) = [-23.9394, -0.6339]
  cl(0:1, 61  ) = [-23.3333, -0.6371]
  cl(0:1, 62  ) = [-22.7273, -0.6402]
  cl(0:1, 63  ) = [-22.1212, -0.6433]
  cl(0:1, 64  ) = [-21.5152, -0.6465]
  cl(0:1, 65  ) = [-20.9091, -0.6496]
  cl(0:1, 66  ) = [-20.303, -0.6528]
  cl(0:1, 67  ) = [-19.697, -0.6463]
  cl(0:1, 68  ) = [-19.0909, -0.6302]
  cl(0:1, 69  ) = [-18.4848, -0.6141]
  cl(0:1, 70  ) = [-17.8788, -0.598]
  cl(0:1, 71  ) = [-17.2727, -0.582]
  cl(0:1, 72  ) = [-16.6667, -0.5659]
  cl(0:1, 73  ) = [-16.0606, -0.5498]
  cl(0:1, 74  ) = [-15.4545, -0.5337]
  cl(0:1, 75  ) = [-14.8485, -0.5176]
  cl(0:1, 76  ) = [-14.2424, -0.5015]
  cl(0:1, 77  ) = [-13.6364, -0.4855]
  cl(0:1, 78  ) = [-13.0303, -0.4694]
  cl(0:1, 79  ) = [-12.4242, -0.4533]
  cl(0:1, 80  ) = [-11.8182, -0.4372]
  cl(0:1, 81  ) = [-11.2121, -0.4211]
  cl(0:1, 82  ) = [-10.6061, -0.405]
  cl(0:1, 83  ) = [-10.0, -0.3889]
  cl(0:1, 84  ) = [-9.3939, -0.357]
  cl(0:1, 85  ) = [-8.7879, -0.3251]
  cl(0:1, 86  ) = [-8.1818, -0.2931]
  cl(0:1, 87  ) = [-7.5758, -0.2612]
  cl(0:1, 88  ) = [-6.9697, -0.2293]
  cl(0:1, 89  ) = [-6.3636, -0.1973]
  cl(0:1, 90  ) = [-5.7576, -0.1653]
  cl(0:1, 91  ) = [-5.1515, -0.1334]
  cl(0:1, 92  ) = [-4.5455, -0.1014]
  cl(0:1, 93  ) = [-3.9394, -0.0694]
  cl(0:1, 94  ) = [-3.3333, -0.0332]
  cl(0:1, 95  ) = [-2.7273, 0.024]
  cl(0:1, 96  ) = [-2.1212, 0.0932]
  cl(0:1, 97  ) = [-1.5152, 0.1625]
  cl(0:1, 98  ) = [-0.9091, 0.2318]
  cl(0:1, 99  ) = [-0.303, 0.3011]
  cl(0:1, 100  ) = [0.303, 0.3703]
  cl(0:1, 101  ) = [0.9091, 0.4396]
  cl(0:1, 102  ) = [1.5152, 0.5092]
  cl(0:1, 103  ) = [2.1212, 0.5864]
  cl(0:1, 104  ) = [2.7273, 0.6672]
  cl(0:1, 105  ) = [3.3333, 0.748]
  cl(0:1, 106  ) = [3.9394, 0.8288]
  cl(0:1, 107  ) = [4.5455, 0.9096]
  cl(0:1, 108  ) = [5.1515, 0.9904]
  cl(0:1, 109  ) = [5.7576, 1.0684]
  cl(0:1, 110  ) = [6.3636, 1.1416]
  cl(0:1, 111  ) = [6.9697, 1.2141]
  cl(0:1, 112  ) = [7.5758, 1.2866]
  cl(0:1, 113  ) = [8.1818, 1.3591]
  cl(0:1, 114  ) = [8.7879, 1.425]
  cl(0:1, 115  ) = [9.3939, 1.4779]
  cl(0:1, 116  ) = [10.0, 1.5276]
  cl(0:1, 117  ) = [10.6061, 1.2104]
  cl(0:1, 118  ) = [11.2121, 1.0675]
  cl(0:1, 119  ) = [11.8182, 1.0405]
  cl(0:1, 120  ) = [12.4242, 1.0205]
  cl(0:1, 121  ) = [13.0303, 1.0096]
  cl(0:1, 122  ) = [13.6364, 1.0074]
  cl(0:1, 123  ) = [14.2424, 1.0076]
  cl(0:1, 124  ) = [14.8485, 1.009]
  cl(0:1, 125  ) = [15.4545, 1.0115]
  cl(0:1, 126  ) = [16.0606, 1.0129]
  cl(0:1, 127  ) = [16.6667, 1.002]
  cl(0:1, 128  ) = [17.2727, 0.9904]
  cl(0:1, 129  ) = [17.8788, 0.978]
  cl(0:1, 130  ) = [18.4848, 0.9656]
  cl(0:1, 131  ) = [19.0909, 0.9533]
  cl(0:1, 132  ) = [19.697, 0.9409]
  cl(0:1, 133  ) = [20.303, 0.9325]
  cl(0:1, 134  ) = [20.9091, 0.928]
  cl(0:1, 135  ) = [21.5152, 0.9236]
  cl(0:1, 136  ) = [22.1212, 0.9191]
  cl(0:1, 137  ) = [22.7273, 0.9146]
  cl(0:1, 138  ) = [23.3333, 0.9101]
  cl(0:1, 139  ) = [23.9394, 0.9057]
  cl(0:1, 140  ) = [24.5455, 0.9012]
  cl(0:1, 141  ) = [25.1515, 0.8967]
  cl(0:1, 142  ) = [25.7576, 0.8922]
  cl(0:1, 143  ) = [26.3636, 0.8878]
  cl(0:1, 144  ) = [26.9697, 0.8833]
  cl(0:1, 145  ) = [27.5758, 0.8788]
  cl(0:1, 146  ) = [28.1818, 0.8743]
  cl(0:1, 147  ) = [28.7879, 0.8699]
  cl(0:1, 148  ) = [29.3939, 0.8654]
  cl(0:1, 149  ) = [30.0, 0.8609]
  cl(0:1, 150  ) = [33.0, 0.8484]
  cl(0:1, 151  ) = [36.0, 0.8359]
  cl(0:1, 152  ) = [39.0, 0.8234]
  cl(0:1, 153  ) = [42.0, 0.8042]
  cl(0:1, 154  ) = [45.0, 0.7816]
  cl(0:1, 155  ) = [48.0, 0.759]
  cl(0:1, 156  ) = [51.0, 0.7312]
  cl(0:1, 157  ) = [54.0, 0.6931]
  cl(0:1, 158  ) = [57.0, 0.6549]
  cl(0:1, 159  ) = [60.0, 0.6167]
  cl(0:1, 160  ) = [63.0, 0.5636]
  cl(0:1, 161  ) = [66.0, 0.5105]
  cl(0:1, 162  ) = [69.0, 0.4574]
  cl(0:1, 163  ) = [72.0, 0.3971]
  cl(0:1, 164  ) = [75.0, 0.3333]
  cl(0:1, 165  ) = [78.0, 0.2694]
  cl(0:1, 166  ) = [81.0, 0.2041]
  cl(0:1, 167  ) = [84.0, 0.1361]
  cl(0:1, 168  ) = [87.0, 0.068]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.0476]
  cl(0:1, 171  ) = [96.0, -0.0952]
  cl(0:1, 172  ) = [99.0, -0.1428]
  cl(0:1, 173  ) = [102.0, -0.1885]
  cl(0:1, 174  ) = [105.0, -0.2332]
  cl(0:1, 175  ) = [108.0, -0.278]
  cl(0:1, 176  ) = [111.0, -0.3202]
  cl(0:1, 177  ) = [114.0, -0.3574]
  cl(0:1, 178  ) = [117.0, -0.3946]
  cl(0:1, 179  ) = [120.0, -0.4318]
  cl(0:1, 180  ) = [123.0, -0.4585]
  cl(0:1, 181  ) = [126.0, -0.4852]
  cl(0:1, 182  ) = [129.0, -0.5119]
  cl(0:1, 183  ) = [132.0, -0.5313]
  cl(0:1, 184  ) = [135.0, -0.5472]
  cl(0:1, 185  ) = [138.0, -0.563]
  cl(0:1, 186  ) = [141.0, -0.5764]
  cl(0:1, 187  ) = [144.0, -0.5851]
  cl(0:1, 188  ) = [147.0, -0.5939]
  cl(0:1, 189  ) = [150.0, -0.6026]
  cl(0:1, 190  ) = [153.0, -0.6181]
  cl(0:1, 191  ) = [156.0, -0.6336]
  cl(0:1, 192  ) = [159.0, -0.6491]
  cl(0:1, 193  ) = [162.0, -0.6117]
  cl(0:1, 194  ) = [165.0, -0.5477]
  cl(0:1, 195  ) = [168.0, -0.4837]
  cl(0:1, 196  ) = [171.0, -0.3969]
  cl(0:1, 197  ) = [174.0, -0.2646]
  cl(0:1, 198  ) = [177.0, -0.1317]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl5 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl5 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl5 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl5_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl5 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl5_nrel2p8.pro' 
  ENDIF

!print,y,cl5

END FUNCTION calc_cl5_nrel2p8
!_________________________________________________________________________


!_________________________________________________________________________
FUNCTION calc_cl6_nrel2p8( y ) RESULT( cl6 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl6
INTEGER :: i

 cl6 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.1432]
  cl(0:1, 2  ) = [-174.0, 0.2878]
  cl(0:1, 3  ) = [-171.0, 0.4317]
  cl(0:1, 4  ) = [-168.0, 0.5235]
  cl(0:1, 5  ) = [-165.0, 0.5834]
  cl(0:1, 6  ) = [-162.0, 0.6431]
  cl(0:1, 7  ) = [-159.0, 0.6785]
  cl(0:1, 8  ) = [-156.0, 0.6589]
  cl(0:1, 9  ) = [-153.0, 0.6394]
  cl(0:1, 10  ) = [-150.0, 0.6199]
  cl(0:1, 11  ) = [-147.0, 0.609]
  cl(0:1, 12  ) = [-144.0, 0.5982]
  cl(0:1, 13  ) = [-141.0, 0.5874]
  cl(0:1, 14  ) = [-138.0, 0.5723]
  cl(0:1, 15  ) = [-135.0, 0.5552]
  cl(0:1, 16  ) = [-132.0, 0.5381]
  cl(0:1, 17  ) = [-129.0, 0.5175]
  cl(0:1, 18  ) = [-126.0, 0.49]
  cl(0:1, 19  ) = [-123.0, 0.4624]
  cl(0:1, 20  ) = [-120.0, 0.4349]
  cl(0:1, 21  ) = [-117.0, 0.3971]
  cl(0:1, 22  ) = [-114.0, 0.3594]
  cl(0:1, 23  ) = [-111.0, 0.3217]
  cl(0:1, 24  ) = [-108.0, 0.2791]
  cl(0:1, 25  ) = [-105.0, 0.2341]
  cl(0:1, 26  ) = [-102.0, 0.189]
  cl(0:1, 27  ) = [-99.0, 0.1431]
  cl(0:1, 28  ) = [-96.0, 0.0954]
  cl(0:1, 29  ) = [-93.0, 0.0477]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.0477]
  cl(0:1, 32  ) = [-84.0, -0.0954]
  cl(0:1, 33  ) = [-81.0, -0.1431]
  cl(0:1, 34  ) = [-78.0, -0.189]
  cl(0:1, 35  ) = [-75.0, -0.2341]
  cl(0:1, 36  ) = [-72.0, -0.2791]
  cl(0:1, 37  ) = [-69.0, -0.3217]
  cl(0:1, 38  ) = [-66.0, -0.3594]
  cl(0:1, 39  ) = [-63.0, -0.3971]
  cl(0:1, 40  ) = [-60.0, -0.4349]
  cl(0:1, 41  ) = [-57.0, -0.4624]
  cl(0:1, 42  ) = [-54.0, -0.49]
  cl(0:1, 43  ) = [-51.0, -0.5175]
  cl(0:1, 44  ) = [-48.0, -0.5381]
  cl(0:1, 45  ) = [-45.0, -0.5552]
  cl(0:1, 46  ) = [-42.0, -0.5723]
  cl(0:1, 47  ) = [-39.0, -0.5873]
  cl(0:1, 48  ) = [-36.0, -0.5982]
  cl(0:1, 49  ) = [-33.0, -0.609]
  cl(0:1, 50  ) = [-30.0, -0.6199]
  cl(0:1, 51  ) = [-29.3939, -0.6238]
  cl(0:1, 52  ) = [-28.7879, -0.6278]
  cl(0:1, 53  ) = [-28.1818, -0.6318]
  cl(0:1, 54  ) = [-27.5758, -0.6358]
  cl(0:1, 55  ) = [-26.9697, -0.6398]
  cl(0:1, 56  ) = [-26.3636, -0.6438]
  cl(0:1, 57  ) = [-25.7576, -0.6478]
  cl(0:1, 58  ) = [-25.1515, -0.6518]
  cl(0:1, 59  ) = [-24.5455, -0.6558]
  cl(0:1, 60  ) = [-23.9394, -0.6598]
  cl(0:1, 61  ) = [-23.3333, -0.6637]
  cl(0:1, 62  ) = [-22.7273, -0.6676]
  cl(0:1, 63  ) = [-22.1212, -0.6715]
  cl(0:1, 64  ) = [-21.5152, -0.6754]
  cl(0:1, 65  ) = [-20.9091, -0.6792]
  cl(0:1, 66  ) = [-20.303, -0.6831]
  cl(0:1, 67  ) = [-19.697, -0.6772]
  cl(0:1, 68  ) = [-19.0909, -0.6616]
  cl(0:1, 69  ) = [-18.4848, -0.646]
  cl(0:1, 70  ) = [-17.8788, -0.6304]
  cl(0:1, 71  ) = [-17.2727, -0.6148]
  cl(0:1, 72  ) = [-16.6667, -0.5992]
  cl(0:1, 73  ) = [-16.0606, -0.5836]
  cl(0:1, 74  ) = [-15.4545, -0.568]
  cl(0:1, 75  ) = [-14.8485, -0.5524]
  cl(0:1, 76  ) = [-14.2424, -0.5368]
  cl(0:1, 77  ) = [-13.6364, -0.5212]
  cl(0:1, 78  ) = [-13.0303, -0.5055]
  cl(0:1, 79  ) = [-12.4242, -0.4899]
  cl(0:1, 80  ) = [-11.8182, -0.4742]
  cl(0:1, 81  ) = [-11.2121, -0.4586]
  cl(0:1, 82  ) = [-10.6061, -0.4429]
  cl(0:1, 83  ) = [-10.0, -0.4272]
  cl(0:1, 84  ) = [-9.3939, -0.3945]
  cl(0:1, 85  ) = [-8.7879, -0.3617]
  cl(0:1, 86  ) = [-8.1818, -0.329]
  cl(0:1, 87  ) = [-7.5758, -0.2967]
  cl(0:1, 88  ) = [-6.9697, -0.2646]
  cl(0:1, 89  ) = [-6.3636, -0.2326]
  cl(0:1, 90  ) = [-5.7576, -0.2006]
  cl(0:1, 91  ) = [-5.1515, -0.1689]
  cl(0:1, 92  ) = [-4.5455, -0.1375]
  cl(0:1, 93  ) = [-3.9394, -0.1068]
  cl(0:1, 94  ) = [-3.3333, -0.0763]
  cl(0:1, 95  ) = [-2.7273, -0.023]
  cl(0:1, 96  ) = [-2.1212, 0.0561]
  cl(0:1, 97  ) = [-1.5152, 0.134]
  cl(0:1, 98  ) = [-0.9091, 0.211]
  cl(0:1, 99  ) = [-0.303, 0.2873]
  cl(0:1, 100  ) = [0.303, 0.3627]
  cl(0:1, 101  ) = [0.9091, 0.4372]
  cl(0:1, 102  ) = [1.5152, 0.5136]
  cl(0:1, 103  ) = [2.1212, 0.5938]
  cl(0:1, 104  ) = [2.7273, 0.6744]
  cl(0:1, 105  ) = [3.3333, 0.7549]
  cl(0:1, 106  ) = [3.9394, 0.8355]
  cl(0:1, 107  ) = [4.5455, 0.9161]
  cl(0:1, 108  ) = [5.1515, 0.9966]
  cl(0:1, 109  ) = [5.7576, 1.0766]
  cl(0:1, 110  ) = [6.3636, 1.1554]
  cl(0:1, 111  ) = [6.9697, 1.2341]
  cl(0:1, 112  ) = [7.5758, 1.3125]
  cl(0:1, 113  ) = [8.1818, 1.3876]
  cl(0:1, 114  ) = [8.7879, 1.4587]
  cl(0:1, 115  ) = [9.3939, 1.5267]
  cl(0:1, 116  ) = [10.0, 1.5936]
  cl(0:1, 117  ) = [10.6061, 1.5327]
  cl(0:1, 118  ) = [11.2121, 1.5066]
  cl(0:1, 119  ) = [11.8182, 1.522]
  cl(0:1, 120  ) = [12.4242, 1.0613]
  cl(0:1, 121  ) = [13.0303, 0.9866]
  cl(0:1, 122  ) = [13.6364, 0.9954]
  cl(0:1, 123  ) = [14.2424, 1.0092]
  cl(0:1, 124  ) = [14.8485, 1.0266]
  cl(0:1, 125  ) = [15.4545, 1.0514]
  cl(0:1, 126  ) = [16.0606, 1.0796]
  cl(0:1, 127  ) = [16.6667, 1.0643]
  cl(0:1, 128  ) = [17.2727, 1.0487]
  cl(0:1, 129  ) = [17.8788, 1.0327]
  cl(0:1, 130  ) = [18.4848, 1.0164]
  cl(0:1, 131  ) = [19.0909, 1.0006]
  cl(0:1, 132  ) = [19.697, 0.9859]
  cl(0:1, 133  ) = [20.303, 0.9759]
  cl(0:1, 134  ) = [20.9091, 0.9704]
  cl(0:1, 135  ) = [21.5152, 0.9649]
  cl(0:1, 136  ) = [22.1212, 0.9594]
  cl(0:1, 137  ) = [22.7273, 0.9538]
  cl(0:1, 138  ) = [23.3333, 0.9482]
  cl(0:1, 139  ) = [23.9394, 0.9426]
  cl(0:1, 140  ) = [24.5455, 0.9369]
  cl(0:1, 141  ) = [25.1515, 0.9312]
  cl(0:1, 142  ) = [25.7576, 0.9256]
  cl(0:1, 143  ) = [26.3636, 0.9198]
  cl(0:1, 144  ) = [26.9697, 0.9141]
  cl(0:1, 145  ) = [27.5758, 0.9084]
  cl(0:1, 146  ) = [28.1818, 0.9027]
  cl(0:1, 147  ) = [28.7879, 0.897]
  cl(0:1, 148  ) = [29.3939, 0.8913]
  cl(0:1, 149  ) = [30.0, 0.8856]
  cl(0:1, 150  ) = [33.0, 0.87]
  cl(0:1, 151  ) = [36.0, 0.8545]
  cl(0:1, 152  ) = [39.0, 0.839]
  cl(0:1, 153  ) = [42.0, 0.8175]
  cl(0:1, 154  ) = [45.0, 0.7931]
  cl(0:1, 155  ) = [48.0, 0.7687]
  cl(0:1, 156  ) = [51.0, 0.7393]
  cl(0:1, 157  ) = [54.0, 0.6999]
  cl(0:1, 158  ) = [57.0, 0.6606]
  cl(0:1, 159  ) = [60.0, 0.6212]
  cl(0:1, 160  ) = [63.0, 0.5673]
  cl(0:1, 161  ) = [66.0, 0.5134]
  cl(0:1, 162  ) = [69.0, 0.4595]
  cl(0:1, 163  ) = [72.0, 0.3987]
  cl(0:1, 164  ) = [75.0, 0.3344]
  cl(0:1, 165  ) = [78.0, 0.2701]
  cl(0:1, 166  ) = [81.0, 0.2046]
  cl(0:1, 167  ) = [84.0, 0.1364]
  cl(0:1, 168  ) = [87.0, 0.0682]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.0477]
  cl(0:1, 171  ) = [96.0, -0.0954]
  cl(0:1, 172  ) = [99.0, -0.1431]
  cl(0:1, 173  ) = [102.0, -0.189]
  cl(0:1, 174  ) = [105.0, -0.2341]
  cl(0:1, 175  ) = [108.0, -0.2791]
  cl(0:1, 176  ) = [111.0, -0.3217]
  cl(0:1, 177  ) = [114.0, -0.3594]
  cl(0:1, 178  ) = [117.0, -0.3971]
  cl(0:1, 179  ) = [120.0, -0.4349]
  cl(0:1, 180  ) = [123.0, -0.4624]
  cl(0:1, 181  ) = [126.0, -0.49]
  cl(0:1, 182  ) = [129.0, -0.5175]
  cl(0:1, 183  ) = [132.0, -0.5381]
  cl(0:1, 184  ) = [135.0, -0.5552]
  cl(0:1, 185  ) = [138.0, -0.5723]
  cl(0:1, 186  ) = [141.0, -0.5874]
  cl(0:1, 187  ) = [144.0, -0.5982]
  cl(0:1, 188  ) = [147.0, -0.609]
  cl(0:1, 189  ) = [150.0, -0.6199]
  cl(0:1, 190  ) = [153.0, -0.6394]
  cl(0:1, 191  ) = [156.0, -0.6589]
  cl(0:1, 192  ) = [159.0, -0.6785]
  cl(0:1, 193  ) = [162.0, -0.6435]
  cl(0:1, 194  ) = [165.0, -0.5884]
  cl(0:1, 195  ) = [168.0, -0.5235]
  cl(0:1, 196  ) = [171.0, -0.4317]
  cl(0:1, 197  ) = [174.0, -0.2878]
  cl(0:1, 198  ) = [177.0, -0.1432]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl6 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl6 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl6 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl6_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl6 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl6_nrel2p8.pro' 
  ENDIF

!print,y,cl6

END FUNCTION calc_cl6_nrel2p8
!_________________________________________________________________________


!_________________________________________________________________________
FUNCTION calc_cl7_nrel2p8( y ) RESULT( cl7 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl7
INTEGER :: i

 cl7 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.1472]
  cl(0:1, 2  ) = [-174.0, 0.2957]
  cl(0:1, 3  ) = [-171.0, 0.4435]
  cl(0:1, 4  ) = [-168.0, 0.5368]
  cl(0:1, 5  ) = [-165.0, 0.6016]
  cl(0:1, 6  ) = [-162.0, 0.6662]
  cl(0:1, 7  ) = [-159.0, 0.7023]
  cl(0:1, 8  ) = [-156.0, 0.6797]
  cl(0:1, 9  ) = [-153.0, 0.6571]
  cl(0:1, 10  ) = [-150.0, 0.6346]
  cl(0:1, 11  ) = [-147.0, 0.6221]
  cl(0:1, 12  ) = [-144.0, 0.6095]
  cl(0:1, 13  ) = [-141.0, 0.597]
  cl(0:1, 14  ) = [-138.0, 0.5807]
  cl(0:1, 15  ) = [-135.0, 0.5625]
  cl(0:1, 16  ) = [-132.0, 0.5443]
  cl(0:1, 17  ) = [-129.0, 0.5227]
  cl(0:1, 18  ) = [-126.0, 0.4944]
  cl(0:1, 19  ) = [-123.0, 0.4661]
  cl(0:1, 20  ) = [-120.0, 0.4378]
  cl(0:1, 21  ) = [-117.0, 0.3996]
  cl(0:1, 22  ) = [-114.0, 0.3613]
  cl(0:1, 23  ) = [-111.0, 0.3231]
  cl(0:1, 24  ) = [-108.0, 0.2802]
  cl(0:1, 25  ) = [-105.0, 0.2349]
  cl(0:1, 26  ) = [-102.0, 0.1896]
  cl(0:1, 27  ) = [-99.0, 0.1434]
  cl(0:1, 28  ) = [-96.0, 0.0956]
  cl(0:1, 29  ) = [-93.0, 0.0478]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.0478]
  cl(0:1, 32  ) = [-84.0, -0.0956]
  cl(0:1, 33  ) = [-81.0, -0.1434]
  cl(0:1, 34  ) = [-78.0, -0.1896]
  cl(0:1, 35  ) = [-75.0, -0.2349]
  cl(0:1, 36  ) = [-72.0, -0.2802]
  cl(0:1, 37  ) = [-69.0, -0.3231]
  cl(0:1, 38  ) = [-66.0, -0.3613]
  cl(0:1, 39  ) = [-63.0, -0.3996]
  cl(0:1, 40  ) = [-60.0, -0.4378]
  cl(0:1, 41  ) = [-57.0, -0.4661]
  cl(0:1, 42  ) = [-54.0, -0.4944]
  cl(0:1, 43  ) = [-51.0, -0.5227]
  cl(0:1, 44  ) = [-48.0, -0.5443]
  cl(0:1, 45  ) = [-45.0, -0.5625]
  cl(0:1, 46  ) = [-42.0, -0.5807]
  cl(0:1, 47  ) = [-39.0, -0.597]
  cl(0:1, 48  ) = [-36.0, -0.6095]
  cl(0:1, 49  ) = [-33.0, -0.6221]
  cl(0:1, 50  ) = [-30.0, -0.6346]
  cl(0:1, 51  ) = [-29.3939, -0.6392]
  cl(0:1, 52  ) = [-28.7879, -0.6437]
  cl(0:1, 53  ) = [-28.1818, -0.6483]
  cl(0:1, 54  ) = [-27.5758, -0.6529]
  cl(0:1, 55  ) = [-26.9697, -0.6574]
  cl(0:1, 56  ) = [-26.3636, -0.662]
  cl(0:1, 57  ) = [-25.7576, -0.6666]
  cl(0:1, 58  ) = [-25.1515, -0.6712]
  cl(0:1, 59  ) = [-24.5455, -0.6757]
  cl(0:1, 60  ) = [-23.9394, -0.6803]
  cl(0:1, 61  ) = [-23.3333, -0.6849]
  cl(0:1, 62  ) = [-22.7273, -0.6894]
  cl(0:1, 63  ) = [-22.1212, -0.694]
  cl(0:1, 64  ) = [-21.5152, -0.6985]
  cl(0:1, 65  ) = [-20.9091, -0.703]
  cl(0:1, 66  ) = [-20.303, -0.7076]
  cl(0:1, 67  ) = [-19.697, -0.7024]
  cl(0:1, 68  ) = [-19.0909, -0.6876]
  cl(0:1, 69  ) = [-18.4848, -0.6728]
  cl(0:1, 70  ) = [-17.8788, -0.658]
  cl(0:1, 71  ) = [-17.2727, -0.6431]
  cl(0:1, 72  ) = [-16.6667, -0.6283]
  cl(0:1, 73  ) = [-16.0606, -0.6135]
  cl(0:1, 74  ) = [-15.4545, -0.5987]
  cl(0:1, 75  ) = [-14.8485, -0.5838]
  cl(0:1, 76  ) = [-14.2424, -0.569]
  cl(0:1, 77  ) = [-13.6364, -0.5542]
  cl(0:1, 78  ) = [-13.0303, -0.5394]
  cl(0:1, 79  ) = [-12.4242, -0.5246]
  cl(0:1, 80  ) = [-11.8182, -0.5097]
  cl(0:1, 81  ) = [-11.2121, -0.4949]
  cl(0:1, 82  ) = [-10.6061, -0.4801]
  cl(0:1, 83  ) = [-10.0, -0.4653]
  cl(0:1, 84  ) = [-9.3939, -0.433]
  cl(0:1, 85  ) = [-8.7879, -0.4008]
  cl(0:1, 86  ) = [-8.1818, -0.3686]
  cl(0:1, 87  ) = [-7.5758, -0.3364]
  cl(0:1, 88  ) = [-6.9697, -0.3044]
  cl(0:1, 89  ) = [-6.3636, -0.2723]
  cl(0:1, 90  ) = [-5.7576, -0.2404]
  cl(0:1, 91  ) = [-5.1515, -0.2084]
  cl(0:1, 92  ) = [-4.5455, -0.1766]
  cl(0:1, 93  ) = [-3.9394, -0.145]
  cl(0:1, 94  ) = [-3.3333, -0.1139]
  cl(0:1, 95  ) = [-2.7273, -0.06]
  cl(0:1, 96  ) = [-2.1212, 0.0228]
  cl(0:1, 97  ) = [-1.5152, 0.1052]
  cl(0:1, 98  ) = [-0.9091, 0.1874]
  cl(0:1, 99  ) = [-0.303, 0.2695]
  cl(0:1, 100  ) = [0.303, 0.3513]
  cl(0:1, 101  ) = [0.9091, 0.433]
  cl(0:1, 102  ) = [1.5152, 0.5148]
  cl(0:1, 103  ) = [2.1212, 0.5959]
  cl(0:1, 104  ) = [2.7273, 0.6764]
  cl(0:1, 105  ) = [3.3333, 0.7568]
  cl(0:1, 106  ) = [3.9394, 0.8373]
  cl(0:1, 107  ) = [4.5455, 0.9178]
  cl(0:1, 108  ) = [5.1515, 0.9983]
  cl(0:1, 109  ) = [5.7576, 1.0788]
  cl(0:1, 110  ) = [6.3636, 1.1593]
  cl(0:1, 111  ) = [6.9697, 1.2397]
  cl(0:1, 112  ) = [7.5758, 1.3198]
  cl(0:1, 113  ) = [8.1818, 1.3957]
  cl(0:1, 114  ) = [8.7879, 1.4685]
  cl(0:1, 115  ) = [9.3939, 1.5411]
  cl(0:1, 116  ) = [10.0, 1.6138]
  cl(0:1, 117  ) = [10.6061, 1.6684]
  cl(0:1, 118  ) = [11.2121, 1.7069]
  cl(0:1, 119  ) = [11.8182, 1.7459]
  cl(0:1, 120  ) = [12.4242, 1.0978]
  cl(0:1, 121  ) = [13.0303, 0.9734]
  cl(0:1, 122  ) = [13.6364, 0.9885]
  cl(0:1, 123  ) = [14.2424, 1.0109]
  cl(0:1, 124  ) = [14.8485, 1.0428]
  cl(0:1, 125  ) = [15.4545, 1.0827]
  cl(0:1, 126  ) = [16.0606, 1.1207]
  cl(0:1, 127  ) = [16.6667, 1.1043]
  cl(0:1, 128  ) = [17.2727, 1.0878]
  cl(0:1, 129  ) = [17.8788, 1.0714]
  cl(0:1, 130  ) = [18.4848, 1.0548]
  cl(0:1, 131  ) = [19.0909, 1.0384]
  cl(0:1, 132  ) = [19.697, 1.0222]
  cl(0:1, 133  ) = [20.303, 1.0109]
  cl(0:1, 134  ) = [20.9091, 1.0044]
  cl(0:1, 135  ) = [21.5152, 0.998]
  cl(0:1, 136  ) = [22.1212, 0.9915]
  cl(0:1, 137  ) = [22.7273, 0.985]
  cl(0:1, 138  ) = [23.3333, 0.9785]
  cl(0:1, 139  ) = [23.9394, 0.9719]
  cl(0:1, 140  ) = [24.5455, 0.9654]
  cl(0:1, 141  ) = [25.1515, 0.9589]
  cl(0:1, 142  ) = [25.7576, 0.9524]
  cl(0:1, 143  ) = [26.3636, 0.9458]
  cl(0:1, 144  ) = [26.9697, 0.9393]
  cl(0:1, 145  ) = [27.5758, 0.9328]
  cl(0:1, 146  ) = [28.1818, 0.9262]
  cl(0:1, 147  ) = [28.7879, 0.9197]
  cl(0:1, 148  ) = [29.3939, 0.9132]
  cl(0:1, 149  ) = [30.0, 0.9066]
  cl(0:1, 150  ) = [33.0, 0.8887]
  cl(0:1, 151  ) = [36.0, 0.8707]
  cl(0:1, 152  ) = [39.0, 0.8528]
  cl(0:1, 153  ) = [42.0, 0.8295]
  cl(0:1, 154  ) = [45.0, 0.8035]
  cl(0:1, 155  ) = [48.0, 0.7775]
  cl(0:1, 156  ) = [51.0, 0.7467]
  cl(0:1, 157  ) = [54.0, 0.7063]
  cl(0:1, 158  ) = [57.0, 0.6659]
  cl(0:1, 159  ) = [60.0, 0.6255]
  cl(0:1, 160  ) = [63.0, 0.5709]
  cl(0:1, 161  ) = [66.0, 0.5162]
  cl(0:1, 162  ) = [69.0, 0.4616]
  cl(0:1, 163  ) = [72.0, 0.4003]
  cl(0:1, 164  ) = [75.0, 0.3356]
  cl(0:1, 165  ) = [78.0, 0.2709]
  cl(0:1, 166  ) = [81.0, 0.2049]
  cl(0:1, 167  ) = [84.0, 0.1366]
  cl(0:1, 168  ) = [87.0, 0.0683]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.0478]
  cl(0:1, 171  ) = [96.0, -0.0956]
  cl(0:1, 172  ) = [99.0, -0.1434]
  cl(0:1, 173  ) = [102.0, -0.1896]
  cl(0:1, 174  ) = [105.0, -0.2349]
  cl(0:1, 175  ) = [108.0, -0.2802]
  cl(0:1, 176  ) = [111.0, -0.3231]
  cl(0:1, 177  ) = [114.0, -0.3613]
  cl(0:1, 178  ) = [117.0, -0.3996]
  cl(0:1, 179  ) = [120.0, -0.4378]
  cl(0:1, 180  ) = [123.0, -0.4661]
  cl(0:1, 181  ) = [126.0, -0.4944]
  cl(0:1, 182  ) = [129.0, -0.5227]
  cl(0:1, 183  ) = [132.0, -0.5443]
  cl(0:1, 184  ) = [135.0, -0.5625]
  cl(0:1, 185  ) = [138.0, -0.5807]
  cl(0:1, 186  ) = [141.0, -0.597]
  cl(0:1, 187  ) = [144.0, -0.6095]
  cl(0:1, 188  ) = [147.0, -0.6221]
  cl(0:1, 189  ) = [150.0, -0.6346]
  cl(0:1, 190  ) = [153.0, -0.6571]
  cl(0:1, 191  ) = [156.0, -0.6797]
  cl(0:1, 192  ) = [159.0, -0.7023]
  cl(0:1, 193  ) = [162.0, -0.6663]
  cl(0:1, 194  ) = [165.0, -0.6028]
  cl(0:1, 195  ) = [168.0, -0.5368]
  cl(0:1, 196  ) = [171.0, -0.4435]
  cl(0:1, 197  ) = [174.0, -0.2957]
  cl(0:1, 198  ) = [177.0, -0.1472]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl7 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl7 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl7 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl7_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl7 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl7_nrel2p8.pro' 
  ENDIF

!print,y,cl7

END FUNCTION calc_cl7_nrel2p8
!_________________________________________________________________________


!_________________________________________________________________________
FUNCTION calc_cl8_nrel2p8( y ) RESULT( cl8 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl8
INTEGER :: i

 cl8 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.143]
  cl(0:1, 2  ) = [-174.0, 0.288]
  cl(0:1, 3  ) = [-171.0, 0.432]
  cl(0:1, 4  ) = [-168.0, 0.5315]
  cl(0:1, 5  ) = [-165.0, 0.6113]
  cl(0:1, 6  ) = [-162.0, 0.6946]
  cl(0:1, 7  ) = [-159.0, 0.7408]
  cl(0:1, 8  ) = [-156.0, 0.7161]
  cl(0:1, 9  ) = [-153.0, 0.6914]
  cl(0:1, 10  ) = [-150.0, 0.6667]
  cl(0:1, 11  ) = [-147.0, 0.6529]
  cl(0:1, 12  ) = [-144.0, 0.6391]
  cl(0:1, 13  ) = [-141.0, 0.6253]
  cl(0:1, 14  ) = [-138.0, 0.6077]
  cl(0:1, 15  ) = [-135.0, 0.5883]
  cl(0:1, 16  ) = [-132.0, 0.5688]
  cl(0:1, 17  ) = [-129.0, 0.5458]
  cl(0:1, 18  ) = [-126.0, 0.5161]
  cl(0:1, 19  ) = [-123.0, 0.4863]
  cl(0:1, 20  ) = [-120.0, 0.4565]
  cl(0:1, 21  ) = [-117.0, 0.4165]
  cl(0:1, 22  ) = [-114.0, 0.3765]
  cl(0:1, 23  ) = [-111.0, 0.3365]
  cl(0:1, 24  ) = [-108.0, 0.2917]
  cl(0:1, 25  ) = [-105.0, 0.2445]
  cl(0:1, 26  ) = [-102.0, 0.1972]
  cl(0:1, 27  ) = [-99.0, 0.1492]
  cl(0:1, 28  ) = [-96.0, 0.0994]
  cl(0:1, 29  ) = [-93.0, 0.0497]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.0497]
  cl(0:1, 32  ) = [-84.0, -0.0994]
  cl(0:1, 33  ) = [-81.0, -0.1492]
  cl(0:1, 34  ) = [-78.0, -0.1972]
  cl(0:1, 35  ) = [-75.0, -0.2445]
  cl(0:1, 36  ) = [-72.0, -0.2917]
  cl(0:1, 37  ) = [-69.0, -0.3365]
  cl(0:1, 38  ) = [-66.0, -0.3765]
  cl(0:1, 39  ) = [-63.0, -0.4165]
  cl(0:1, 40  ) = [-60.0, -0.4565]
  cl(0:1, 41  ) = [-57.0, -0.4863]
  cl(0:1, 42  ) = [-54.0, -0.5161]
  cl(0:1, 43  ) = [-51.0, -0.5458]
  cl(0:1, 44  ) = [-48.0, -0.5693]
  cl(0:1, 45  ) = [-45.0, -0.5896]
  cl(0:1, 46  ) = [-42.0, -0.6083]
  cl(0:1, 47  ) = [-39.0, -0.6254]
  cl(0:1, 48  ) = [-36.0, -0.6393]
  cl(0:1, 49  ) = [-33.0, -0.6529]
  cl(0:1, 50  ) = [-30.0, -0.6667]
  cl(0:1, 51  ) = [-29.3939, -0.671]
  cl(0:1, 52  ) = [-28.7879, -0.6754]
  cl(0:1, 53  ) = [-28.1818, -0.6798]
  cl(0:1, 54  ) = [-27.5758, -0.6843]
  cl(0:1, 55  ) = [-26.9697, -0.6888]
  cl(0:1, 56  ) = [-26.3636, -0.6934]
  cl(0:1, 57  ) = [-25.7576, -0.698]
  cl(0:1, 58  ) = [-25.1515, -0.7029]
  cl(0:1, 59  ) = [-24.5455, -0.7077]
  cl(0:1, 60  ) = [-23.9394, -0.7126]
  cl(0:1, 61  ) = [-23.3333, -0.7179]
  cl(0:1, 62  ) = [-22.7273, -0.7232]
  cl(0:1, 63  ) = [-22.1212, -0.7284]
  cl(0:1, 64  ) = [-21.5152, -0.7317]
  cl(0:1, 65  ) = [-20.9091, -0.7342]
  cl(0:1, 66  ) = [-20.303, -0.7381]
  cl(0:1, 67  ) = [-19.697, -0.7328]
  cl(0:1, 68  ) = [-19.0909, -0.7184]
  cl(0:1, 69  ) = [-18.4848, -0.7042]
  cl(0:1, 70  ) = [-17.8788, -0.6902]
  cl(0:1, 71  ) = [-17.2727, -0.6762]
  cl(0:1, 72  ) = [-16.6667, -0.6624]
  cl(0:1, 73  ) = [-16.0606, -0.6486]
  cl(0:1, 74  ) = [-15.4545, -0.635]
  cl(0:1, 75  ) = [-14.8485, -0.6215]
  cl(0:1, 76  ) = [-14.2424, -0.6084]
  cl(0:1, 77  ) = [-13.6364, -0.5954]
  cl(0:1, 78  ) = [-13.0303, -0.5826]
  cl(0:1, 79  ) = [-12.4242, -0.5701]
  cl(0:1, 80  ) = [-11.8182, -0.5577]
  cl(0:1, 81  ) = [-11.2121, -0.5455]
  cl(0:1, 82  ) = [-10.6061, -0.5338]
  cl(0:1, 83  ) = [-10.0, -0.5225]
  cl(0:1, 84  ) = [-9.3939, -0.4987]
  cl(0:1, 85  ) = [-8.7879, -0.4749]
  cl(0:1, 86  ) = [-8.1818, -0.4502]
  cl(0:1, 87  ) = [-7.5758, -0.4163]
  cl(0:1, 88  ) = [-6.9697, -0.3785]
  cl(0:1, 89  ) = [-6.3636, -0.3413]
  cl(0:1, 90  ) = [-5.7576, -0.3033]
  cl(0:1, 91  ) = [-5.1515, -0.2639]
  cl(0:1, 92  ) = [-4.5455, -0.2228]
  cl(0:1, 93  ) = [-3.9394, -0.1807]
  cl(0:1, 94  ) = [-3.3333, -0.1342]
  cl(0:1, 95  ) = [-2.7273, -0.0719]
  cl(0:1, 96  ) = [-2.1212, 0.0071]
  cl(0:1, 97  ) = [-1.5152, 0.0873]
  cl(0:1, 98  ) = [-0.9091, 0.1682]
  cl(0:1, 99  ) = [-0.303, 0.2498]
  cl(0:1, 100  ) = [0.303, 0.3321]
  cl(0:1, 101  ) = [0.9091, 0.415]
  cl(0:1, 102  ) = [1.5152, 0.4975]
  cl(0:1, 103  ) = [2.1212, 0.578]
  cl(0:1, 104  ) = [2.7273, 0.658]
  cl(0:1, 105  ) = [3.3333, 0.7376]
  cl(0:1, 106  ) = [3.9394, 0.817]
  cl(0:1, 107  ) = [4.5455, 0.8958]
  cl(0:1, 108  ) = [5.1515, 0.9742]
  cl(0:1, 109  ) = [5.7576, 1.0519]
  cl(0:1, 110  ) = [6.3636, 1.1286]
  cl(0:1, 111  ) = [6.9697, 1.2047]
  cl(0:1, 112  ) = [7.5758, 1.2785]
  cl(0:1, 113  ) = [8.1818, 1.3484]
  cl(0:1, 114  ) = [8.7879, 1.4139]
  cl(0:1, 115  ) = [9.3939, 1.4769]
  cl(0:1, 116  ) = [10.0, 1.5387]
  cl(0:1, 117  ) = [10.6061, 1.5818]
  cl(0:1, 118  ) = [11.2121, 1.6085]
  cl(0:1, 119  ) = [11.8182, 1.6262]
  cl(0:1, 120  ) = [12.4242, 1.1708]
  cl(0:1, 121  ) = [13.0303, 1.0706]
  cl(0:1, 122  ) = [13.6364, 1.0732]
  cl(0:1, 123  ) = [14.2424, 1.0821]
  cl(0:1, 124  ) = [14.8485, 1.1035]
  cl(0:1, 125  ) = [15.4545, 1.1309]
  cl(0:1, 126  ) = [16.0606, 1.1529]
  cl(0:1, 127  ) = [16.6667, 1.1389]
  cl(0:1, 128  ) = [17.2727, 1.1257]
  cl(0:1, 129  ) = [17.8788, 1.1135]
  cl(0:1, 130  ) = [18.4848, 1.1041]
  cl(0:1, 131  ) = [19.0909, 1.0942]
  cl(0:1, 132  ) = [19.697, 1.0782]
  cl(0:1, 133  ) = [20.303, 1.0659]
  cl(0:1, 134  ) = [20.9091, 1.0574]
  cl(0:1, 135  ) = [21.5152, 1.049]
  cl(0:1, 136  ) = [22.1212, 1.0407]
  cl(0:1, 137  ) = [22.7273, 1.0332]
  cl(0:1, 138  ) = [23.3333, 1.0256]
  cl(0:1, 139  ) = [23.9394, 1.0181]
  cl(0:1, 140  ) = [24.5455, 1.0111]
  cl(0:1, 141  ) = [25.1515, 1.0042]
  cl(0:1, 142  ) = [25.7576, 0.9973]
  cl(0:1, 143  ) = [26.3636, 0.9906]
  cl(0:1, 144  ) = [26.9697, 0.9841]
  cl(0:1, 145  ) = [27.5758, 0.9776]
  cl(0:1, 146  ) = [28.1818, 0.9712]
  cl(0:1, 147  ) = [28.7879, 0.9649]
  cl(0:1, 148  ) = [29.3939, 0.9587]
  cl(0:1, 149  ) = [30.0, 0.9524]
  cl(0:1, 150  ) = [33.0, 0.9328]
  cl(0:1, 151  ) = [36.0, 0.9133]
  cl(0:1, 152  ) = [39.0, 0.8934]
  cl(0:1, 153  ) = [42.0, 0.8689]
  cl(0:1, 154  ) = [45.0, 0.8422]
  cl(0:1, 155  ) = [48.0, 0.8133]
  cl(0:1, 156  ) = [51.0, 0.7798]
  cl(0:1, 157  ) = [54.0, 0.7373]
  cl(0:1, 158  ) = [57.0, 0.6947]
  cl(0:1, 159  ) = [60.0, 0.6522]
  cl(0:1, 160  ) = [63.0, 0.595]
  cl(0:1, 161  ) = [66.0, 0.5379]
  cl(0:1, 162  ) = [69.0, 0.4807]
  cl(0:1, 163  ) = [72.0, 0.4167]
  cl(0:1, 164  ) = [75.0, 0.3492]
  cl(0:1, 165  ) = [78.0, 0.2818]
  cl(0:1, 166  ) = [81.0, 0.2131]
  cl(0:1, 167  ) = [84.0, 0.1421]
  cl(0:1, 168  ) = [87.0, 0.071]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.0497]
  cl(0:1, 171  ) = [96.0, -0.0994]
  cl(0:1, 172  ) = [99.0, -0.1492]
  cl(0:1, 173  ) = [102.0, -0.1972]
  cl(0:1, 174  ) = [105.0, -0.2445]
  cl(0:1, 175  ) = [108.0, -0.2917]
  cl(0:1, 176  ) = [111.0, -0.3365]
  cl(0:1, 177  ) = [114.0, -0.3765]
  cl(0:1, 178  ) = [117.0, -0.4165]
  cl(0:1, 179  ) = [120.0, -0.4565]
  cl(0:1, 180  ) = [123.0, -0.4863]
  cl(0:1, 181  ) = [126.0, -0.5161]
  cl(0:1, 182  ) = [129.0, -0.5458]
  cl(0:1, 183  ) = [132.0, -0.5688]
  cl(0:1, 184  ) = [135.0, -0.5883]
  cl(0:1, 185  ) = [138.0, -0.6077]
  cl(0:1, 186  ) = [141.0, -0.6253]
  cl(0:1, 187  ) = [144.0, -0.6391]
  cl(0:1, 188  ) = [147.0, -0.6529]
  cl(0:1, 189  ) = [150.0, -0.6667]
  cl(0:1, 190  ) = [153.0, -0.6914]
  cl(0:1, 191  ) = [156.0, -0.7161]
  cl(0:1, 192  ) = [159.0, -0.7408]
  cl(0:1, 193  ) = [162.0, -0.6925]
  cl(0:1, 194  ) = [165.0, -0.6041]
  cl(0:1, 195  ) = [168.0, -0.5257]
  cl(0:1, 196  ) = [171.0, -0.4241]
  cl(0:1, 197  ) = [174.0, -0.2827]
  cl(0:1, 198  ) = [177.0, -0.1404]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl8 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl8 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl8 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl8_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl8 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl8_nrel2p8.pro' 
  ENDIF

!print,y,cl8

END FUNCTION calc_cl8_nrel2p8
!_________________________________________________________________________

!_________________________________________________________________________
FUNCTION calc_cl9_nrel2p8( y ) RESULT( cl9 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl9
INTEGER :: i

 cl9 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.1354]
  cl(0:1, 2  ) = [-174.0, 0.2737]
  cl(0:1, 3  ) = [-171.0, 0.4105]
  cl(0:1, 4  ) = [-168.0, 0.5205]
  cl(0:1, 5  ) = [-165.0, 0.6174]
  cl(0:1, 6  ) = [-162.0, 0.7204]
  cl(0:1, 7  ) = [-159.0, 0.7799]
  cl(0:1, 8  ) = [-156.0, 0.7541]
  cl(0:1, 9  ) = [-153.0, 0.7283]
  cl(0:1, 10  ) = [-150.0, 0.7024]
  cl(0:1, 11  ) = [-147.0, 0.6881]
  cl(0:1, 12  ) = [-144.0, 0.6737]
  cl(0:1, 13  ) = [-141.0, 0.6593]
  cl(0:1, 14  ) = [-138.0, 0.6409]
  cl(0:1, 15  ) = [-135.0, 0.6205]
  cl(0:1, 16  ) = [-132.0, 0.6]
  cl(0:1, 17  ) = [-129.0, 0.576]
  cl(0:1, 18  ) = [-126.0, 0.5446]
  cl(0:1, 19  ) = [-123.0, 0.5132]
  cl(0:1, 20  ) = [-120.0, 0.4819]
  cl(0:1, 21  ) = [-117.0, 0.4397]
  cl(0:1, 22  ) = [-114.0, 0.3975]
  cl(0:1, 23  ) = [-111.0, 0.3553]
  cl(0:1, 24  ) = [-108.0, 0.308]
  cl(0:1, 25  ) = [-105.0, 0.2582]
  cl(0:1, 26  ) = [-102.0, 0.2083]
  cl(0:1, 27  ) = [-99.0, 0.1576]
  cl(0:1, 28  ) = [-96.0, 0.1051]
  cl(0:1, 29  ) = [-93.0, 0.0525]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.0525]
  cl(0:1, 32  ) = [-84.0, -0.1051]
  cl(0:1, 33  ) = [-81.0, -0.1576]
  cl(0:1, 34  ) = [-78.0, -0.2083]
  cl(0:1, 35  ) = [-75.0, -0.2582]
  cl(0:1, 36  ) = [-72.0, -0.308]
  cl(0:1, 37  ) = [-69.0, -0.3553]
  cl(0:1, 38  ) = [-66.0, -0.3975]
  cl(0:1, 39  ) = [-63.0, -0.4397]
  cl(0:1, 40  ) = [-60.0, -0.4819]
  cl(0:1, 41  ) = [-57.0, -0.5132]
  cl(0:1, 42  ) = [-54.0, -0.5446]
  cl(0:1, 43  ) = [-51.0, -0.576]
  cl(0:1, 44  ) = [-48.0, -0.6013]
  cl(0:1, 45  ) = [-45.0, -0.6237]
  cl(0:1, 46  ) = [-42.0, -0.6422]
  cl(0:1, 47  ) = [-39.0, -0.6595]
  cl(0:1, 48  ) = [-36.0, -0.6742]
  cl(0:1, 49  ) = [-33.0, -0.6881]
  cl(0:1, 50  ) = [-30.0, -0.7024]
  cl(0:1, 51  ) = [-29.3939, -0.7061]
  cl(0:1, 52  ) = [-28.7879, -0.7098]
  cl(0:1, 53  ) = [-28.1818, -0.7135]
  cl(0:1, 54  ) = [-27.5758, -0.7175]
  cl(0:1, 55  ) = [-26.9697, -0.7217]
  cl(0:1, 56  ) = [-26.3636, -0.7258]
  cl(0:1, 57  ) = [-25.7576, -0.7303]
  cl(0:1, 58  ) = [-25.1515, -0.7351]
  cl(0:1, 59  ) = [-24.5455, -0.74]
  cl(0:1, 60  ) = [-23.9394, -0.745]
  cl(0:1, 61  ) = [-23.3333, -0.7509]
  cl(0:1, 62  ) = [-22.7273, -0.7568]
  cl(0:1, 63  ) = [-22.1212, -0.7627]
  cl(0:1, 64  ) = [-21.5152, -0.7655]
  cl(0:1, 65  ) = [-20.9091, -0.7672]
  cl(0:1, 66  ) = [-20.303, -0.771]
  cl(0:1, 67  ) = [-19.697, -0.7664]
  cl(0:1, 68  ) = [-19.0909, -0.7532]
  cl(0:1, 69  ) = [-18.4848, -0.7405]
  cl(0:1, 70  ) = [-17.8788, -0.7281]
  cl(0:1, 71  ) = [-17.2727, -0.7158]
  cl(0:1, 72  ) = [-16.6667, -0.7036]
  cl(0:1, 73  ) = [-16.0606, -0.6915]
  cl(0:1, 74  ) = [-15.4545, -0.6798]
  cl(0:1, 75  ) = [-14.8485, -0.6684]
  cl(0:1, 76  ) = [-14.2424, -0.6575]
  cl(0:1, 77  ) = [-13.6364, -0.6469]
  cl(0:1, 78  ) = [-13.0303, -0.6366]
  cl(0:1, 79  ) = [-12.4242, -0.6266]
  cl(0:1, 80  ) = [-11.8182, -0.617]
  cl(0:1, 81  ) = [-11.2121, -0.6078]
  cl(0:1, 82  ) = [-10.6061, -0.5993]
  cl(0:1, 83  ) = [-10.0, -0.5917]
  cl(0:1, 84  ) = [-9.3939, -0.5791]
  cl(0:1, 85  ) = [-8.7879, -0.5667]
  cl(0:1, 86  ) = [-8.1818, -0.5528]
  cl(0:1, 87  ) = [-7.5758, -0.5162]
  cl(0:1, 88  ) = [-6.9697, -0.4698]
  cl(0:1, 89  ) = [-6.3636, -0.4232]
  cl(0:1, 90  ) = [-5.7576, -0.3748]
  cl(0:1, 91  ) = [-5.1515, -0.3234]
  cl(0:1, 92  ) = [-4.5455, -0.2685]
  cl(0:1, 93  ) = [-3.9394, -0.212]
  cl(0:1, 94  ) = [-3.3333, -0.1484]
  cl(0:1, 95  ) = [-2.7273, -0.079]
  cl(0:1, 96  ) = [-2.1212, -0.0035]
  cl(0:1, 97  ) = [-1.5152, 0.0738]
  cl(0:1, 98  ) = [-0.9091, 0.1518]
  cl(0:1, 99  ) = [-0.303, 0.2308]
  cl(0:1, 100  ) = [0.303, 0.3105]
  cl(0:1, 101  ) = [0.9091, 0.3908]
  cl(0:1, 102  ) = [1.5152, 0.4713]
  cl(0:1, 103  ) = [2.1212, 0.551]
  cl(0:1, 104  ) = [2.7273, 0.6301]
  cl(0:1, 105  ) = [3.3333, 0.7085]
  cl(0:1, 106  ) = [3.9394, 0.7863]
  cl(0:1, 107  ) = [4.5455, 0.8624]
  cl(0:1, 108  ) = [5.1515, 0.9377]
  cl(0:1, 109  ) = [5.7576, 1.0113]
  cl(0:1, 110  ) = [6.3636, 1.0823]
  cl(0:1, 111  ) = [6.9697, 1.1516]
  cl(0:1, 112  ) = [7.5758, 1.216]
  cl(0:1, 113  ) = [8.1818, 1.2767]
  cl(0:1, 114  ) = [8.7879, 1.3312]
  cl(0:1, 115  ) = [9.3939, 1.3796]
  cl(0:1, 116  ) = [10.0, 1.4247]
  cl(0:1, 117  ) = [10.6061, 1.4486]
  cl(0:1, 118  ) = [11.2121, 1.4564]
  cl(0:1, 119  ) = [11.8182, 1.4415]
  cl(0:1, 120  ) = [12.4242, 1.2581]
  cl(0:1, 121  ) = [13.0303, 1.2178]
  cl(0:1, 122  ) = [13.6364, 1.2016]
  cl(0:1, 123  ) = [14.2424, 1.1881]
  cl(0:1, 124  ) = [14.8485, 1.1803]
  cl(0:1, 125  ) = [15.4545, 1.1788]
  cl(0:1, 126  ) = [16.0606, 1.1769]
  cl(0:1, 127  ) = [16.6667, 1.1658]
  cl(0:1, 128  ) = [17.2727, 1.1567]
  cl(0:1, 129  ) = [17.8788, 1.1498]
  cl(0:1, 130  ) = [18.4848, 1.1499]
  cl(0:1, 131  ) = [19.0909, 1.149]
  cl(0:1, 132  ) = [19.697, 1.134]
  cl(0:1, 133  ) = [20.303, 1.1211]
  cl(0:1, 134  ) = [20.9091, 1.1105]
  cl(0:1, 135  ) = [21.5152, 1.0999]
  cl(0:1, 136  ) = [22.1212, 1.0896]
  cl(0:1, 137  ) = [22.7273, 1.0812]
  cl(0:1, 138  ) = [23.3333, 1.0727]
  cl(0:1, 139  ) = [23.9394, 1.0643]
  cl(0:1, 140  ) = [24.5455, 1.0572]
  cl(0:1, 141  ) = [25.1515, 1.0502]
  cl(0:1, 142  ) = [25.7576, 1.0433]
  cl(0:1, 143  ) = [26.3636, 1.0369]
  cl(0:1, 144  ) = [26.9697, 1.031]
  cl(0:1, 145  ) = [27.5758, 1.0251]
  cl(0:1, 146  ) = [28.1818, 1.0193]
  cl(0:1, 147  ) = [28.7879, 1.0141]
  cl(0:1, 148  ) = [29.3939, 1.0088]
  cl(0:1, 149  ) = [30.0, 1.0035]
  cl(0:1, 150  ) = [33.0, 0.983]
  cl(0:1, 151  ) = [36.0, 0.9631]
  cl(0:1, 152  ) = [39.0, 0.9422]
  cl(0:1, 153  ) = [42.0, 0.9174]
  cl(0:1, 154  ) = [45.0, 0.891]
  cl(0:1, 155  ) = [48.0, 0.8591]
  cl(0:1, 156  ) = [51.0, 0.8228]
  cl(0:1, 157  ) = [54.0, 0.778]
  cl(0:1, 158  ) = [57.0, 0.7332]
  cl(0:1, 159  ) = [60.0, 0.6884]
  cl(0:1, 160  ) = [63.0, 0.6281]
  cl(0:1, 161  ) = [66.0, 0.5679]
  cl(0:1, 162  ) = [69.0, 0.5076]
  cl(0:1, 163  ) = [72.0, 0.4401]
  cl(0:1, 164  ) = [75.0, 0.3688]
  cl(0:1, 165  ) = [78.0, 0.2976]
  cl(0:1, 166  ) = [81.0, 0.2251]
  cl(0:1, 167  ) = [84.0, 0.1501]
  cl(0:1, 168  ) = [87.0, 0.075]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.0525]
  cl(0:1, 171  ) = [96.0, -0.1051]
  cl(0:1, 172  ) = [99.0, -0.1576]
  cl(0:1, 173  ) = [102.0, -0.2083]
  cl(0:1, 174  ) = [105.0, -0.2582]
  cl(0:1, 175  ) = [108.0, -0.308]
  cl(0:1, 176  ) = [111.0, -0.3553]
  cl(0:1, 177  ) = [114.0, -0.3975]
  cl(0:1, 178  ) = [117.0, -0.4397]
  cl(0:1, 179  ) = [120.0, -0.4819]
  cl(0:1, 180  ) = [123.0, -0.5132]
  cl(0:1, 181  ) = [126.0, -0.5446]
  cl(0:1, 182  ) = [129.0, -0.576]
  cl(0:1, 183  ) = [132.0, -0.6]
  cl(0:1, 184  ) = [135.0, -0.6205]
  cl(0:1, 185  ) = [138.0, -0.6409]
  cl(0:1, 186  ) = [141.0, -0.6593]
  cl(0:1, 187  ) = [144.0, -0.6737]
  cl(0:1, 188  ) = [147.0, -0.6881]
  cl(0:1, 189  ) = [150.0, -0.7024]
  cl(0:1, 190  ) = [153.0, -0.7283]
  cl(0:1, 191  ) = [156.0, -0.7541]
  cl(0:1, 192  ) = [159.0, -0.7799]
  cl(0:1, 193  ) = [162.0, -0.7156]
  cl(0:1, 194  ) = [165.0, -0.6047]
  cl(0:1, 195  ) = [168.0, -0.5046]
  cl(0:1, 196  ) = [171.0, -0.3903]
  cl(0:1, 197  ) = [174.0, -0.2602]
  cl(0:1, 198  ) = [177.0, -0.1287]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl9 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl9 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl9 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl9_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl9 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl9_nrel2p8.pro' 
  ENDIF

!print,y,cl9

END FUNCTION calc_cl9_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cl10_nrel2p8( y ) RESULT( cl10 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl10
INTEGER :: i

 cl10 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.1295]
  cl(0:1, 2  ) = [-174.0, 0.2622]
  cl(0:1, 3  ) = [-171.0, 0.3933]
  cl(0:1, 4  ) = [-168.0, 0.5102]
  cl(0:1, 5  ) = [-165.0, 0.6198]
  cl(0:1, 6  ) = [-162.0, 0.7314]
  cl(0:1, 7  ) = [-159.0, 0.797]
  cl(0:1, 8  ) = [-156.0, 0.7708]
  cl(0:1, 9  ) = [-153.0, 0.7446]
  cl(0:1, 10  ) = [-150.0, 0.7184]
  cl(0:1, 11  ) = [-147.0, 0.7038]
  cl(0:1, 12  ) = [-144.0, 0.6892]
  cl(0:1, 13  ) = [-141.0, 0.6747]
  cl(0:1, 14  ) = [-138.0, 0.6559]
  cl(0:1, 15  ) = [-135.0, 0.6351]
  cl(0:1, 16  ) = [-132.0, 0.6143]
  cl(0:1, 17  ) = [-129.0, 0.5898]
  cl(0:1, 18  ) = [-126.0, 0.5577]
  cl(0:1, 19  ) = [-123.0, 0.5256]
  cl(0:1, 20  ) = [-120.0, 0.4936]
  cl(0:1, 21  ) = [-117.0, 0.4504]
  cl(0:1, 22  ) = [-114.0, 0.4072]
  cl(0:1, 23  ) = [-111.0, 0.3641]
  cl(0:1, 24  ) = [-108.0, 0.3156]
  cl(0:1, 25  ) = [-105.0, 0.2646]
  cl(0:1, 26  ) = [-102.0, 0.2135]
  cl(0:1, 27  ) = [-99.0, 0.1615]
  cl(0:1, 28  ) = [-96.0, 0.1077]
  cl(0:1, 29  ) = [-93.0, 0.0538]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.0538]
  cl(0:1, 32  ) = [-84.0, -0.1077]
  cl(0:1, 33  ) = [-81.0, -0.1615]
  cl(0:1, 34  ) = [-78.0, -0.2135]
  cl(0:1, 35  ) = [-75.0, -0.2646]
  cl(0:1, 36  ) = [-72.0, -0.3156]
  cl(0:1, 37  ) = [-69.0, -0.3641]
  cl(0:1, 38  ) = [-66.0, -0.4072]
  cl(0:1, 39  ) = [-63.0, -0.4504]
  cl(0:1, 40  ) = [-60.0, -0.4936]
  cl(0:1, 41  ) = [-57.0, -0.5256]
  cl(0:1, 42  ) = [-54.0, -0.5577]
  cl(0:1, 43  ) = [-51.0, -0.5898]
  cl(0:1, 44  ) = [-48.0, -0.616]
  cl(0:1, 45  ) = [-45.0, -0.6393]
  cl(0:1, 46  ) = [-42.0, -0.6576]
  cl(0:1, 47  ) = [-39.0, -0.675]
  cl(0:1, 48  ) = [-36.0, -0.6899]
  cl(0:1, 49  ) = [-33.0, -0.7039]
  cl(0:1, 50  ) = [-30.0, -0.7184]
  cl(0:1, 51  ) = [-29.3939, -0.7218]
  cl(0:1, 52  ) = [-28.7879, -0.7251]
  cl(0:1, 53  ) = [-28.1818, -0.7285]
  cl(0:1, 54  ) = [-27.5758, -0.7322]
  cl(0:1, 55  ) = [-26.9697, -0.7362]
  cl(0:1, 56  ) = [-26.3636, -0.7401]
  cl(0:1, 57  ) = [-25.7576, -0.7444]
  cl(0:1, 58  ) = [-25.1515, -0.7493]
  cl(0:1, 59  ) = [-24.5455, -0.7541]
  cl(0:1, 60  ) = [-23.9394, -0.7591]
  cl(0:1, 61  ) = [-23.3333, -0.7653]
  cl(0:1, 62  ) = [-22.7273, -0.7715]
  cl(0:1, 63  ) = [-22.1212, -0.7777]
  cl(0:1, 64  ) = [-21.5152, -0.784]
  cl(0:1, 65  ) = [-20.9091, -0.7901]
  cl(0:1, 66  ) = [-20.303, -0.797]
  cl(0:1, 67  ) = [-19.697, -0.7954]
  cl(0:1, 68  ) = [-19.0909, -0.7853]
  cl(0:1, 69  ) = [-18.4848, -0.7753]
  cl(0:1, 70  ) = [-17.8788, -0.7654]
  cl(0:1, 71  ) = [-17.2727, -0.7555]
  cl(0:1, 72  ) = [-16.6667, -0.7457]
  cl(0:1, 73  ) = [-16.0606, -0.736]
  cl(0:1, 74  ) = [-15.4545, -0.7263]
  cl(0:1, 75  ) = [-14.8485, -0.7167]
  cl(0:1, 76  ) = [-14.2424, -0.7074]
  cl(0:1, 77  ) = [-13.6364, -0.6981]
  cl(0:1, 78  ) = [-13.0303, -0.6889]
  cl(0:1, 79  ) = [-12.4242, -0.6799]
  cl(0:1, 80  ) = [-11.8182, -0.671]
  cl(0:1, 81  ) = [-11.2121, -0.6622]
  cl(0:1, 82  ) = [-10.6061, -0.6536]
  cl(0:1, 83  ) = [-10.0, -0.6454]
  cl(0:1, 84  ) = [-9.3939, -0.6366]
  cl(0:1, 85  ) = [-8.7879, -0.6279]
  cl(0:1, 86  ) = [-8.1818, -0.6187]
  cl(0:1, 87  ) = [-7.5758, -0.5802]
  cl(0:1, 88  ) = [-6.9697, -0.529]
  cl(0:1, 89  ) = [-6.3636, -0.4739]
  cl(0:1, 90  ) = [-5.7576, -0.4166]
  cl(0:1, 91  ) = [-5.1515, -0.3559]
  cl(0:1, 92  ) = [-4.5455, -0.2915]
  cl(0:1, 93  ) = [-3.9394, -0.2253]
  cl(0:1, 94  ) = [-3.3333, -0.1541]
  cl(0:1, 95  ) = [-2.7273, -0.0816]
  cl(0:1, 96  ) = [-2.1212, -0.0076]
  cl(0:1, 97  ) = [-1.5152, 0.0683]
  cl(0:1, 98  ) = [-0.9091, 0.1449]
  cl(0:1, 99  ) = [-0.303, 0.2226]
  cl(0:1, 100  ) = [0.303, 0.3008]
  cl(0:1, 101  ) = [0.9091, 0.3797]
  cl(0:1, 102  ) = [1.5152, 0.459]
  cl(0:1, 103  ) = [2.1212, 0.5383]
  cl(0:1, 104  ) = [2.7273, 0.6171]
  cl(0:1, 105  ) = [3.3333, 0.6949]
  cl(0:1, 106  ) = [3.9394, 0.7719]
  cl(0:1, 107  ) = [4.5455, 0.8468]
  cl(0:1, 108  ) = [5.1515, 0.9206]
  cl(0:1, 109  ) = [5.7576, 0.9922]
  cl(0:1, 110  ) = [6.3636, 1.0606]
  cl(0:1, 111  ) = [6.9697, 1.1267]
  cl(0:1, 112  ) = [7.5758, 1.1867]
  cl(0:1, 113  ) = [8.1818, 1.2431]
  cl(0:1, 114  ) = [8.7879, 1.2925]
  cl(0:1, 115  ) = [9.3939, 1.334]
  cl(0:1, 116  ) = [10.0, 1.3713]
  cl(0:1, 117  ) = [10.6061, 1.3862]
  cl(0:1, 118  ) = [11.2121, 1.3852]
  cl(0:1, 119  ) = [11.8182, 1.3549]
  cl(0:1, 120  ) = [12.4242, 1.3125]
  cl(0:1, 121  ) = [13.0303, 1.2868]
  cl(0:1, 122  ) = [13.6364, 1.2617]
  cl(0:1, 123  ) = [14.2424, 1.2377]
  cl(0:1, 124  ) = [14.8485, 1.2153]
  cl(0:1, 125  ) = [15.4545, 1.1997]
  cl(0:1, 126  ) = [16.0606, 1.1866]
  cl(0:1, 127  ) = [16.6667, 1.1769]
  cl(0:1, 128  ) = [17.2727, 1.1696]
  cl(0:1, 129  ) = [17.8788, 1.1652]
  cl(0:1, 130  ) = [18.4848, 1.1695]
  cl(0:1, 131  ) = [19.0909, 1.1728]
  cl(0:1, 132  ) = [19.697, 1.1584]
  cl(0:1, 133  ) = [20.303, 1.1453]
  cl(0:1, 134  ) = [20.9091, 1.1337]
  cl(0:1, 135  ) = [21.5152, 1.1221]
  cl(0:1, 136  ) = [22.1212, 1.111]
  cl(0:1, 137  ) = [22.7273, 1.1022]
  cl(0:1, 138  ) = [23.3333, 1.0933]
  cl(0:1, 139  ) = [23.9394, 1.0845]
  cl(0:1, 140  ) = [24.5455, 1.0773]
  cl(0:1, 141  ) = [25.1515, 1.0704]
  cl(0:1, 142  ) = [25.7576, 1.0635]
  cl(0:1, 143  ) = [26.3636, 1.0573]
  cl(0:1, 144  ) = [26.9697, 1.0517]
  cl(0:1, 145  ) = [27.5758, 1.0461]
  cl(0:1, 146  ) = [28.1818, 1.0407]
  cl(0:1, 147  ) = [28.7879, 1.0359]
  cl(0:1, 148  ) = [29.3939, 1.0311]
  cl(0:1, 149  ) = [30.0, 1.0263]
  cl(0:1, 150  ) = [33.0, 1.0055]
  cl(0:1, 151  ) = [36.0, 0.9855]
  cl(0:1, 152  ) = [39.0, 0.9643]
  cl(0:1, 153  ) = [42.0, 0.9394]
  cl(0:1, 154  ) = [45.0, 0.9132]
  cl(0:1, 155  ) = [48.0, 0.88]
  cl(0:1, 156  ) = [51.0, 0.8425]
  cl(0:1, 157  ) = [54.0, 0.7967]
  cl(0:1, 158  ) = [57.0, 0.7509]
  cl(0:1, 159  ) = [60.0, 0.7051]
  cl(0:1, 160  ) = [63.0, 0.6434]
  cl(0:1, 161  ) = [66.0, 0.5818]
  cl(0:1, 162  ) = [69.0, 0.5201]
  cl(0:1, 163  ) = [72.0, 0.4509]
  cl(0:1, 164  ) = [75.0, 0.3779]
  cl(0:1, 165  ) = [78.0, 0.305]
  cl(0:1, 166  ) = [81.0, 0.2307]
  cl(0:1, 167  ) = [84.0, 0.1538]
  cl(0:1, 168  ) = [87.0, 0.0769]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.0538]
  cl(0:1, 171  ) = [96.0, -0.1077]
  cl(0:1, 172  ) = [99.0, -0.1615]
  cl(0:1, 173  ) = [102.0, -0.2135]
  cl(0:1, 174  ) = [105.0, -0.2646]
  cl(0:1, 175  ) = [108.0, -0.3156]
  cl(0:1, 176  ) = [111.0, -0.3641]
  cl(0:1, 177  ) = [114.0, -0.4072]
  cl(0:1, 178  ) = [117.0, -0.4504]
  cl(0:1, 179  ) = [120.0, -0.4936]
  cl(0:1, 180  ) = [123.0, -0.5256]
  cl(0:1, 181  ) = [126.0, -0.5577]
  cl(0:1, 182  ) = [129.0, -0.5898]
  cl(0:1, 183  ) = [132.0, -0.6143]
  cl(0:1, 184  ) = [135.0, -0.6351]
  cl(0:1, 185  ) = [138.0, -0.6559]
  cl(0:1, 186  ) = [141.0, -0.6747]
  cl(0:1, 187  ) = [144.0, -0.6892]
  cl(0:1, 188  ) = [147.0, -0.7038]
  cl(0:1, 189  ) = [150.0, -0.7184]
  cl(0:1, 190  ) = [153.0, -0.7446]
  cl(0:1, 191  ) = [156.0, -0.7708]
  cl(0:1, 192  ) = [159.0, -0.797]
  cl(0:1, 193  ) = [162.0, -0.7254]
  cl(0:1, 194  ) = [165.0, -0.6049]
  cl(0:1, 195  ) = [168.0, -0.4873]
  cl(0:1, 196  ) = [171.0, -0.3668]
  cl(0:1, 197  ) = [174.0, -0.2445]
  cl(0:1, 198  ) = [177.0, -0.1207]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl10 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl10 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl10 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl10_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl10 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl10_nrel2p8.pro' 
  ENDIF

!print,y,cl10

END FUNCTION calc_cl10_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cl11_nrel2p8( y ) RESULT( cl11 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl11
INTEGER :: i

 cl11 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.125]
  cl(0:1, 2  ) = [-174.0, 0.253]
  cl(0:1, 3  ) = [-171.0, 0.3795]
  cl(0:1, 4  ) = [-168.0, 0.4988]
  cl(0:1, 5  ) = [-165.0, 0.6152]
  cl(0:1, 6  ) = [-162.0, 0.7271]
  cl(0:1, 7  ) = [-159.0, 0.7932]
  cl(0:1, 8  ) = [-156.0, 0.7682]
  cl(0:1, 9  ) = [-153.0, 0.7434]
  cl(0:1, 10  ) = [-150.0, 0.7185]
  cl(0:1, 11  ) = [-147.0, 0.704]
  cl(0:1, 12  ) = [-144.0, 0.6894]
  cl(0:1, 13  ) = [-141.0, 0.6749]
  cl(0:1, 14  ) = [-138.0, 0.6562]
  cl(0:1, 15  ) = [-135.0, 0.6354]
  cl(0:1, 16  ) = [-132.0, 0.6147]
  cl(0:1, 17  ) = [-129.0, 0.5901]
  cl(0:1, 18  ) = [-126.0, 0.5581]
  cl(0:1, 19  ) = [-123.0, 0.526]
  cl(0:1, 20  ) = [-120.0, 0.4939]
  cl(0:1, 21  ) = [-117.0, 0.4508]
  cl(0:1, 22  ) = [-114.0, 0.4076]
  cl(0:1, 23  ) = [-111.0, 0.3644]
  cl(0:1, 24  ) = [-108.0, 0.3159]
  cl(0:1, 25  ) = [-105.0, 0.2648]
  cl(0:1, 26  ) = [-102.0, 0.2137]
  cl(0:1, 27  ) = [-99.0, 0.1617]
  cl(0:1, 28  ) = [-96.0, 0.1078]
  cl(0:1, 29  ) = [-93.0, 0.0539]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.0539]
  cl(0:1, 32  ) = [-84.0, -0.1078]
  cl(0:1, 33  ) = [-81.0, -0.1617]
  cl(0:1, 34  ) = [-78.0, -0.2137]
  cl(0:1, 35  ) = [-75.0, -0.2648]
  cl(0:1, 36  ) = [-72.0, -0.3159]
  cl(0:1, 37  ) = [-69.0, -0.3644]
  cl(0:1, 38  ) = [-66.0, -0.4076]
  cl(0:1, 39  ) = [-63.0, -0.4508]
  cl(0:1, 40  ) = [-60.0, -0.4939]
  cl(0:1, 41  ) = [-57.0, -0.526]
  cl(0:1, 42  ) = [-54.0, -0.5581]
  cl(0:1, 43  ) = [-51.0, -0.5901]
  cl(0:1, 44  ) = [-48.0, -0.6163]
  cl(0:1, 45  ) = [-45.0, -0.6396]
  cl(0:1, 46  ) = [-42.0, -0.6579]
  cl(0:1, 47  ) = [-39.0, -0.6752]
  cl(0:1, 48  ) = [-36.0, -0.6901]
  cl(0:1, 49  ) = [-33.0, -0.704]
  cl(0:1, 50  ) = [-30.0, -0.7185]
  cl(0:1, 51  ) = [-29.3939, -0.7218]
  cl(0:1, 52  ) = [-28.7879, -0.7251]
  cl(0:1, 53  ) = [-28.1818, -0.7284]
  cl(0:1, 54  ) = [-27.5758, -0.7322]
  cl(0:1, 55  ) = [-26.9697, -0.7361]
  cl(0:1, 56  ) = [-26.3636, -0.74]
  cl(0:1, 57  ) = [-25.7576, -0.7443]
  cl(0:1, 58  ) = [-25.1515, -0.7491]
  cl(0:1, 59  ) = [-24.5455, -0.7539]
  cl(0:1, 60  ) = [-23.9394, -0.7589]
  cl(0:1, 61  ) = [-23.3333, -0.7651]
  cl(0:1, 62  ) = [-22.7273, -0.7713]
  cl(0:1, 63  ) = [-22.1212, -0.7774]
  cl(0:1, 64  ) = [-21.5152, -0.7903]
  cl(0:1, 65  ) = [-20.9091, -0.8059]
  cl(0:1, 66  ) = [-20.303, -0.8195]
  cl(0:1, 67  ) = [-19.697, -0.8247]
  cl(0:1, 68  ) = [-19.0909, -0.8216]
  cl(0:1, 69  ) = [-18.4848, -0.8182]
  cl(0:1, 70  ) = [-17.8788, -0.8146]
  cl(0:1, 71  ) = [-17.2727, -0.8108]
  cl(0:1, 72  ) = [-16.6667, -0.807]
  cl(0:1, 73  ) = [-16.0606, -0.8031]
  cl(0:1, 74  ) = [-15.4545, -0.797]
  cl(0:1, 75  ) = [-14.8485, -0.7896]
  cl(0:1, 76  ) = [-14.2424, -0.7788]
  cl(0:1, 77  ) = [-13.6364, -0.7674]
  cl(0:1, 78  ) = [-13.0303, -0.7554]
  cl(0:1, 79  ) = [-12.4242, -0.743]
  cl(0:1, 80  ) = [-11.8182, -0.7302]
  cl(0:1, 81  ) = [-11.2121, -0.7172]
  cl(0:1, 82  ) = [-10.6061, -0.7036]
  cl(0:1, 83  ) = [-10.0, -0.6891]
  cl(0:1, 84  ) = [-9.3939, -0.6762]
  cl(0:1, 85  ) = [-8.7879, -0.6641]
  cl(0:1, 86  ) = [-8.1818, -0.6542]
  cl(0:1, 87  ) = [-7.5758, -0.6146]
  cl(0:1, 88  ) = [-6.9697, -0.5618]
  cl(0:1, 89  ) = [-6.3636, -0.4992]
  cl(0:1, 90  ) = [-5.7576, -0.4346]
  cl(0:1, 91  ) = [-5.1515, -0.3673]
  cl(0:1, 92  ) = [-4.5455, -0.2972]
  cl(0:1, 93  ) = [-3.9394, -0.2258]
  cl(0:1, 94  ) = [-3.3333, -0.1541]
  cl(0:1, 95  ) = [-2.7273, -0.0812]
  cl(0:1, 96  ) = [-2.1212, -0.0071]
  cl(0:1, 97  ) = [-1.5152, 0.0688]
  cl(0:1, 98  ) = [-0.9091, 0.1454]
  cl(0:1, 99  ) = [-0.303, 0.223]
  cl(0:1, 100  ) = [0.303, 0.3012]
  cl(0:1, 101  ) = [0.9091, 0.38]
  cl(0:1, 102  ) = [1.5152, 0.4592]
  cl(0:1, 103  ) = [2.1212, 0.5384]
  cl(0:1, 104  ) = [2.7273, 0.6171]
  cl(0:1, 105  ) = [3.3333, 0.6948]
  cl(0:1, 106  ) = [3.9394, 0.7717]
  cl(0:1, 107  ) = [4.5455, 0.8464]
  cl(0:1, 108  ) = [5.1515, 0.9202]
  cl(0:1, 109  ) = [5.7576, 0.9917]
  cl(0:1, 110  ) = [6.3636, 1.06]
  cl(0:1, 111  ) = [6.9697, 1.1262]
  cl(0:1, 112  ) = [7.5758, 1.1862]
  cl(0:1, 113  ) = [8.1818, 1.243]
  cl(0:1, 114  ) = [8.7879, 1.2934]
  cl(0:1, 115  ) = [9.3939, 1.3364]
  cl(0:1, 116  ) = [10.0, 1.3747]
  cl(0:1, 117  ) = [10.6061, 1.3913]
  cl(0:1, 118  ) = [11.2121, 1.3925]
  cl(0:1, 119  ) = [11.8182, 1.3651]
  cl(0:1, 120  ) = [12.4242, 1.3399]
  cl(0:1, 121  ) = [13.0303, 1.2852]
  cl(0:1, 122  ) = [13.6364, 1.2564]
  cl(0:1, 123  ) = [14.2424, 1.2294]
  cl(0:1, 124  ) = [14.8485, 1.2059]
  cl(0:1, 125  ) = [15.4545, 1.1894]
  cl(0:1, 126  ) = [16.0606, 1.1757]
  cl(0:1, 127  ) = [16.6667, 1.166]
  cl(0:1, 128  ) = [17.2727, 1.159]
  cl(0:1, 129  ) = [17.8788, 1.1552]
  cl(0:1, 130  ) = [18.4848, 1.1604]
  cl(0:1, 131  ) = [19.0909, 1.1648]
  cl(0:1, 132  ) = [19.697, 1.1524]
  cl(0:1, 133  ) = [20.303, 1.1411]
  cl(0:1, 134  ) = [20.9091, 1.1308]
  cl(0:1, 135  ) = [21.5152, 1.1206]
  cl(0:1, 136  ) = [22.1212, 1.1106]
  cl(0:1, 137  ) = [22.7273, 1.1018]
  cl(0:1, 138  ) = [23.3333, 1.093]
  cl(0:1, 139  ) = [23.9394, 1.0841]
  cl(0:1, 140  ) = [24.5455, 1.077]
  cl(0:1, 141  ) = [25.1515, 1.0702]
  cl(0:1, 142  ) = [25.7576, 1.0633]
  cl(0:1, 143  ) = [26.3636, 1.0571]
  cl(0:1, 144  ) = [26.9697, 1.0516]
  cl(0:1, 145  ) = [27.5758, 1.046]
  cl(0:1, 146  ) = [28.1818, 1.0406]
  cl(0:1, 147  ) = [28.7879, 1.0359]
  cl(0:1, 148  ) = [29.3939, 1.0311]
  cl(0:1, 149  ) = [30.0, 1.0264]
  cl(0:1, 150  ) = [33.0, 1.0057]
  cl(0:1, 151  ) = [36.0, 0.9858]
  cl(0:1, 152  ) = [39.0, 0.9646]
  cl(0:1, 153  ) = [42.0, 0.9399]
  cl(0:1, 154  ) = [45.0, 0.9138]
  cl(0:1, 155  ) = [48.0, 0.8805]
  cl(0:1, 156  ) = [51.0, 0.843]
  cl(0:1, 157  ) = [54.0, 0.7972]
  cl(0:1, 158  ) = [57.0, 0.7514]
  cl(0:1, 159  ) = [60.0, 0.7056]
  cl(0:1, 160  ) = [63.0, 0.6439]
  cl(0:1, 161  ) = [66.0, 0.5822]
  cl(0:1, 162  ) = [69.0, 0.5205]
  cl(0:1, 163  ) = [72.0, 0.4513]
  cl(0:1, 164  ) = [75.0, 0.3783]
  cl(0:1, 165  ) = [78.0, 0.3053]
  cl(0:1, 166  ) = [81.0, 0.2309]
  cl(0:1, 167  ) = [84.0, 0.154]
  cl(0:1, 168  ) = [87.0, 0.077]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.0539]
  cl(0:1, 171  ) = [96.0, -0.1078]
  cl(0:1, 172  ) = [99.0, -0.1617]
  cl(0:1, 173  ) = [102.0, -0.2137]
  cl(0:1, 174  ) = [105.0, -0.2648]
  cl(0:1, 175  ) = [108.0, -0.3159]
  cl(0:1, 176  ) = [111.0, -0.3644]
  cl(0:1, 177  ) = [114.0, -0.4076]
  cl(0:1, 178  ) = [117.0, -0.4508]
  cl(0:1, 179  ) = [120.0, -0.4939]
  cl(0:1, 180  ) = [123.0, -0.526]
  cl(0:1, 181  ) = [126.0, -0.5581]
  cl(0:1, 182  ) = [129.0, -0.5901]
  cl(0:1, 183  ) = [132.0, -0.6147]
  cl(0:1, 184  ) = [135.0, -0.6354]
  cl(0:1, 185  ) = [138.0, -0.6562]
  cl(0:1, 186  ) = [141.0, -0.6749]
  cl(0:1, 187  ) = [144.0, -0.6894]
  cl(0:1, 188  ) = [147.0, -0.704]
  cl(0:1, 189  ) = [150.0, -0.7185]
  cl(0:1, 190  ) = [153.0, -0.7426]
  cl(0:1, 191  ) = [156.0, -0.7668]
  cl(0:1, 192  ) = [159.0, -0.7911]
  cl(0:1, 193  ) = [162.0, -0.7193]
  cl(0:1, 194  ) = [165.0, -0.5994]
  cl(0:1, 195  ) = [168.0, -0.4729]
  cl(0:1, 196  ) = [171.0, -0.3527]
  cl(0:1, 197  ) = [174.0, -0.2352]
  cl(0:1, 198  ) = [177.0, -0.1162]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl11 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl11 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl11 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl11_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl11 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl11_nrel2p8.pro' 
  ENDIF

!print,y,cl11

END FUNCTION calc_cl11_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cl12_nrel2p8( y ) RESULT( cl12 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl12
INTEGER :: i

 cl12 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.1206]
  cl(0:1, 2  ) = [-174.0, 0.2438]
  cl(0:1, 3  ) = [-171.0, 0.3658]
  cl(0:1, 4  ) = [-168.0, 0.4841]
  cl(0:1, 5  ) = [-165.0, 0.6016]
  cl(0:1, 6  ) = [-162.0, 0.7132]
  cl(0:1, 7  ) = [-159.0, 0.7801]
  cl(0:1, 8  ) = [-156.0, 0.7587]
  cl(0:1, 9  ) = [-153.0, 0.7377]
  cl(0:1, 10  ) = [-150.0, 0.7168]
  cl(0:1, 11  ) = [-147.0, 0.7024]
  cl(0:1, 12  ) = [-144.0, 0.6881]
  cl(0:1, 13  ) = [-141.0, 0.6738]
  cl(0:1, 14  ) = [-138.0, 0.6553]
  cl(0:1, 15  ) = [-135.0, 0.6346]
  cl(0:1, 16  ) = [-132.0, 0.614]
  cl(0:1, 17  ) = [-129.0, 0.5895]
  cl(0:1, 18  ) = [-126.0, 0.5576]
  cl(0:1, 19  ) = [-123.0, 0.5256]
  cl(0:1, 20  ) = [-120.0, 0.4936]
  cl(0:1, 21  ) = [-117.0, 0.4505]
  cl(0:1, 22  ) = [-114.0, 0.4073]
  cl(0:1, 23  ) = [-111.0, 0.3642]
  cl(0:1, 24  ) = [-108.0, 0.3158]
  cl(0:1, 25  ) = [-105.0, 0.2647]
  cl(0:1, 26  ) = [-102.0, 0.2136]
  cl(0:1, 27  ) = [-99.0, 0.1616]
  cl(0:1, 28  ) = [-96.0, 0.1077]
  cl(0:1, 29  ) = [-93.0, 0.0539]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.0539]
  cl(0:1, 32  ) = [-84.0, -0.1077]
  cl(0:1, 33  ) = [-81.0, -0.1616]
  cl(0:1, 34  ) = [-78.0, -0.2136]
  cl(0:1, 35  ) = [-75.0, -0.2647]
  cl(0:1, 36  ) = [-72.0, -0.3158]
  cl(0:1, 37  ) = [-69.0, -0.3642]
  cl(0:1, 38  ) = [-66.0, -0.4073]
  cl(0:1, 39  ) = [-63.0, -0.4505]
  cl(0:1, 40  ) = [-60.0, -0.4936]
  cl(0:1, 41  ) = [-57.0, -0.5256]
  cl(0:1, 42  ) = [-54.0, -0.5576]
  cl(0:1, 43  ) = [-51.0, -0.5895]
  cl(0:1, 44  ) = [-48.0, -0.6157]
  cl(0:1, 45  ) = [-45.0, -0.6388]
  cl(0:1, 46  ) = [-42.0, -0.6569]
  cl(0:1, 47  ) = [-39.0, -0.6741]
  cl(0:1, 48  ) = [-36.0, -0.6888]
  cl(0:1, 49  ) = [-33.0, -0.7025]
  cl(0:1, 50  ) = [-30.0, -0.7168]
  cl(0:1, 51  ) = [-29.3939, -0.72]
  cl(0:1, 52  ) = [-28.7879, -0.7233]
  cl(0:1, 53  ) = [-28.1818, -0.7265]
  cl(0:1, 54  ) = [-27.5758, -0.7302]
  cl(0:1, 55  ) = [-26.9697, -0.7341]
  cl(0:1, 56  ) = [-26.3636, -0.7379]
  cl(0:1, 57  ) = [-25.7576, -0.7421]
  cl(0:1, 58  ) = [-25.1515, -0.7469]
  cl(0:1, 59  ) = [-24.5455, -0.7516]
  cl(0:1, 60  ) = [-23.9394, -0.7565]
  cl(0:1, 61  ) = [-23.3333, -0.7626]
  cl(0:1, 62  ) = [-22.7273, -0.7687]
  cl(0:1, 63  ) = [-22.1212, -0.7748]
  cl(0:1, 64  ) = [-21.5152, -0.7939]
  cl(0:1, 65  ) = [-20.9091, -0.8196]
  cl(0:1, 66  ) = [-20.303, -0.8418]
  cl(0:1, 67  ) = [-19.697, -0.8571]
  cl(0:1, 68  ) = [-19.0909, -0.8659]
  cl(0:1, 69  ) = [-18.4848, -0.8742]
  cl(0:1, 70  ) = [-17.8788, -0.8822]
  cl(0:1, 71  ) = [-17.2727, -0.8901]
  cl(0:1, 72  ) = [-16.6667, -0.8979]
  cl(0:1, 73  ) = [-16.0606, -0.9055]
  cl(0:1, 74  ) = [-15.4545, -0.9055]
  cl(0:1, 75  ) = [-14.8485, -0.9007]
  cl(0:1, 76  ) = [-14.2424, -0.8839]
  cl(0:1, 77  ) = [-13.6364, -0.8651]
  cl(0:1, 78  ) = [-13.0303, -0.8449]
  cl(0:1, 79  ) = [-12.4242, -0.8234]
  cl(0:1, 80  ) = [-11.8182, -0.8016]
  cl(0:1, 81  ) = [-11.2121, -0.7794]
  cl(0:1, 82  ) = [-10.6061, -0.7561]
  cl(0:1, 83  ) = [-10.0, -0.7314]
  cl(0:1, 84  ) = [-9.3939, -0.7109]
  cl(0:1, 85  ) = [-8.7879, -0.6939]
  cl(0:1, 86  ) = [-8.1818, -0.6823]
  cl(0:1, 87  ) = [-7.5758, -0.6418]
  cl(0:1, 88  ) = [-6.9697, -0.5881]
  cl(0:1, 89  ) = [-6.3636, -0.5186]
  cl(0:1, 90  ) = [-5.7576, -0.4477]
  cl(0:1, 91  ) = [-5.1515, -0.3751]
  cl(0:1, 92  ) = [-4.5455, -0.3008]
  cl(0:1, 93  ) = [-3.9394, -0.2258]
  cl(0:1, 94  ) = [-3.3333, -0.1531]
  cl(0:1, 95  ) = [-2.7273, -0.0796]
  cl(0:1, 96  ) = [-2.1212, -0.005]
  cl(0:1, 97  ) = [-1.5152, 0.0711]
  cl(0:1, 98  ) = [-0.9091, 0.1478]
  cl(0:1, 99  ) = [-0.303, 0.2257]
  cl(0:1, 100  ) = [0.303, 0.304]
  cl(0:1, 101  ) = [0.9091, 0.3827]
  cl(0:1, 102  ) = [1.5152, 0.4618]
  cl(0:1, 103  ) = [2.1212, 0.5407]
  cl(0:1, 104  ) = [2.7273, 0.6189]
  cl(0:1, 105  ) = [3.3333, 0.6962]
  cl(0:1, 106  ) = [3.9394, 0.7729]
  cl(0:1, 107  ) = [4.5455, 0.8476]
  cl(0:1, 108  ) = [5.1515, 0.9213]
  cl(0:1, 109  ) = [5.7576, 0.9928]
  cl(0:1, 110  ) = [6.3636, 1.0613]
  cl(0:1, 111  ) = [6.9697, 1.1279]
  cl(0:1, 112  ) = [7.5758, 1.1889]
  cl(0:1, 113  ) = [8.1818, 1.2472]
  cl(0:1, 114  ) = [8.7879, 1.3011]
  cl(0:1, 115  ) = [9.3939, 1.3491]
  cl(0:1, 116  ) = [10.0, 1.3912]
  cl(0:1, 117  ) = [10.6061, 1.4137]
  cl(0:1, 118  ) = [11.2121, 1.4224]
  cl(0:1, 119  ) = [11.8182, 1.4051]
  cl(0:1, 120  ) = [12.4242, 1.361]
  cl(0:1, 121  ) = [13.0303, 1.2712]
  cl(0:1, 122  ) = [13.6364, 1.2333]
  cl(0:1, 123  ) = [14.2424, 1.1993]
  cl(0:1, 124  ) = [14.8485, 1.1743]
  cl(0:1, 125  ) = [15.4545, 1.1569]
  cl(0:1, 126  ) = [16.0606, 1.1427]
  cl(0:1, 127  ) = [16.6667, 1.1333]
  cl(0:1, 128  ) = [17.2727, 1.1269]
  cl(0:1, 129  ) = [17.8788, 1.1242]
  cl(0:1, 130  ) = [18.4848, 1.1314]
  cl(0:1, 131  ) = [19.0909, 1.1387]
  cl(0:1, 132  ) = [19.697, 1.1322]
  cl(0:1, 133  ) = [20.303, 1.1259]
  cl(0:1, 134  ) = [20.9091, 1.1197]
  cl(0:1, 135  ) = [21.5152, 1.1136]
  cl(0:1, 136  ) = [22.1212, 1.1069]
  cl(0:1, 137  ) = [22.7273, 1.0982]
  cl(0:1, 138  ) = [23.3333, 1.0895]
  cl(0:1, 139  ) = [23.9394, 1.0808]
  cl(0:1, 140  ) = [24.5455, 1.0738]
  cl(0:1, 141  ) = [25.1515, 1.067]
  cl(0:1, 142  ) = [25.7576, 1.0602]
  cl(0:1, 143  ) = [26.3636, 1.0542]
  cl(0:1, 144  ) = [26.9697, 1.0487]
  cl(0:1, 145  ) = [27.5758, 1.0432]
  cl(0:1, 146  ) = [28.1818, 1.0379]
  cl(0:1, 147  ) = [28.7879, 1.0333]
  cl(0:1, 148  ) = [29.3939, 1.0286]
  cl(0:1, 149  ) = [30.0, 1.0239]
  cl(0:1, 150  ) = [33.0, 1.0036]
  cl(0:1, 151  ) = [36.0, 0.984]
  cl(0:1, 152  ) = [39.0, 0.9631]
  cl(0:1, 153  ) = [42.0, 0.9385]
  cl(0:1, 154  ) = [45.0, 0.9126]
  cl(0:1, 155  ) = [48.0, 0.8795]
  cl(0:1, 156  ) = [51.0, 0.8422]
  cl(0:1, 157  ) = [54.0, 0.7965]
  cl(0:1, 158  ) = [57.0, 0.7508]
  cl(0:1, 159  ) = [60.0, 0.7052]
  cl(0:1, 160  ) = [63.0, 0.6435]
  cl(0:1, 161  ) = [66.0, 0.5819]
  cl(0:1, 162  ) = [69.0, 0.5203]
  cl(0:1, 163  ) = [72.0, 0.4511]
  cl(0:1, 164  ) = [75.0, 0.3782]
  cl(0:1, 165  ) = [78.0, 0.3052]
  cl(0:1, 166  ) = [81.0, 0.2309]
  cl(0:1, 167  ) = [84.0, 0.1539]
  cl(0:1, 168  ) = [87.0, 0.077]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.0539]
  cl(0:1, 171  ) = [96.0, -0.1077]
  cl(0:1, 172  ) = [99.0, -0.1616]
  cl(0:1, 173  ) = [102.0, -0.2136]
  cl(0:1, 174  ) = [105.0, -0.2647]
  cl(0:1, 175  ) = [108.0, -0.3158]
  cl(0:1, 176  ) = [111.0, -0.3642]
  cl(0:1, 177  ) = [114.0, -0.4073]
  cl(0:1, 178  ) = [117.0, -0.4505]
  cl(0:1, 179  ) = [120.0, -0.4936]
  cl(0:1, 180  ) = [123.0, -0.5256]
  cl(0:1, 181  ) = [126.0, -0.5576]
  cl(0:1, 182  ) = [129.0, -0.5895]
  cl(0:1, 183  ) = [132.0, -0.614]
  cl(0:1, 184  ) = [135.0, -0.6346]
  cl(0:1, 185  ) = [138.0, -0.6553]
  cl(0:1, 186  ) = [141.0, -0.6738]
  cl(0:1, 187  ) = [144.0, -0.6881]
  cl(0:1, 188  ) = [147.0, -0.7024]
  cl(0:1, 189  ) = [150.0, -0.7168]
  cl(0:1, 190  ) = [153.0, -0.7349]
  cl(0:1, 191  ) = [156.0, -0.753]
  cl(0:1, 192  ) = [159.0, -0.7716]
  cl(0:1, 193  ) = [162.0, -0.7003]
  cl(0:1, 194  ) = [165.0, -0.5835]
  cl(0:1, 195  ) = [168.0, -0.4581]
  cl(0:1, 196  ) = [171.0, -0.341]
  cl(0:1, 197  ) = [174.0, -0.2274]
  cl(0:1, 198  ) = [177.0, -0.1125]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl12 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl12 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl12 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl12_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl12 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl12_nrel2p8.pro' 
  ENDIF

!print,y,cl12

END FUNCTION calc_cl12_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cl13_nrel2p8( y ) RESULT( cl13 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl13
INTEGER :: i

 cl13 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.1168]
  cl(0:1, 2  ) = [-174.0, 0.2358]
  cl(0:1, 3  ) = [-171.0, 0.3536]
  cl(0:1, 4  ) = [-168.0, 0.4698]
  cl(0:1, 5  ) = [-165.0, 0.5861]
  cl(0:1, 6  ) = [-162.0, 0.6972]
  cl(0:1, 7  ) = [-159.0, 0.7651]
  cl(0:1, 8  ) = [-156.0, 0.7478]
  cl(0:1, 9  ) = [-153.0, 0.7313]
  cl(0:1, 10  ) = [-150.0, 0.7148]
  cl(0:1, 11  ) = [-147.0, 0.7007]
  cl(0:1, 12  ) = [-144.0, 0.6866]
  cl(0:1, 13  ) = [-141.0, 0.6725]
  cl(0:1, 14  ) = [-138.0, 0.6542]
  cl(0:1, 15  ) = [-135.0, 0.6337]
  cl(0:1, 16  ) = [-132.0, 0.6132]
  cl(0:1, 17  ) = [-129.0, 0.5889]
  cl(0:1, 18  ) = [-126.0, 0.557]
  cl(0:1, 19  ) = [-123.0, 0.5251]
  cl(0:1, 20  ) = [-120.0, 0.4932]
  cl(0:1, 21  ) = [-117.0, 0.4502]
  cl(0:1, 22  ) = [-114.0, 0.4071]
  cl(0:1, 23  ) = [-111.0, 0.364]
  cl(0:1, 24  ) = [-108.0, 0.3156]
  cl(0:1, 25  ) = [-105.0, 0.2646]
  cl(0:1, 26  ) = [-102.0, 0.2136]
  cl(0:1, 27  ) = [-99.0, 0.1616]
  cl(0:1, 28  ) = [-96.0, 0.1077]
  cl(0:1, 29  ) = [-93.0, 0.0539]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.0539]
  cl(0:1, 32  ) = [-84.0, -0.1077]
  cl(0:1, 33  ) = [-81.0, -0.1616]
  cl(0:1, 34  ) = [-78.0, -0.2136]
  cl(0:1, 35  ) = [-75.0, -0.2646]
  cl(0:1, 36  ) = [-72.0, -0.3156]
  cl(0:1, 37  ) = [-69.0, -0.364]
  cl(0:1, 38  ) = [-66.0, -0.4071]
  cl(0:1, 39  ) = [-63.0, -0.4502]
  cl(0:1, 40  ) = [-60.0, -0.4932]
  cl(0:1, 41  ) = [-57.0, -0.5251]
  cl(0:1, 42  ) = [-54.0, -0.557]
  cl(0:1, 43  ) = [-51.0, -0.5889]
  cl(0:1, 44  ) = [-48.0, -0.6149]
  cl(0:1, 45  ) = [-45.0, -0.6379]
  cl(0:1, 46  ) = [-42.0, -0.6559]
  cl(0:1, 47  ) = [-39.0, -0.6729]
  cl(0:1, 48  ) = [-36.0, -0.6873]
  cl(0:1, 49  ) = [-33.0, -0.7008]
  cl(0:1, 50  ) = [-30.0, -0.7148]
  cl(0:1, 51  ) = [-29.3939, -0.718]
  cl(0:1, 52  ) = [-28.7879, -0.7212]
  cl(0:1, 53  ) = [-28.1818, -0.7244]
  cl(0:1, 54  ) = [-27.5758, -0.728]
  cl(0:1, 55  ) = [-26.9697, -0.7318]
  cl(0:1, 56  ) = [-26.3636, -0.7355]
  cl(0:1, 57  ) = [-25.7576, -0.7397]
  cl(0:1, 58  ) = [-25.1515, -0.7444]
  cl(0:1, 59  ) = [-24.5455, -0.749]
  cl(0:1, 60  ) = [-23.9394, -0.7538]
  cl(0:1, 61  ) = [-23.3333, -0.7598]
  cl(0:1, 62  ) = [-22.7273, -0.7658]
  cl(0:1, 63  ) = [-22.1212, -0.7718]
  cl(0:1, 64  ) = [-21.5152, -0.7964]
  cl(0:1, 65  ) = [-20.9091, -0.8308]
  cl(0:1, 66  ) = [-20.303, -0.8612]
  cl(0:1, 67  ) = [-19.697, -0.8869]
  cl(0:1, 68  ) = [-19.0909, -0.9078]
  cl(0:1, 69  ) = [-18.4848, -0.9284]
  cl(0:1, 70  ) = [-17.8788, -0.9488]
  cl(0:1, 71  ) = [-17.2727, -0.969]
  cl(0:1, 72  ) = [-16.6667, -0.9891]
  cl(0:1, 73  ) = [-16.0606, -1.0092]
  cl(0:1, 74  ) = [-15.4545, -1.0154]
  cl(0:1, 75  ) = [-14.8485, -1.013]
  cl(0:1, 76  ) = [-14.2424, -0.9892]
  cl(0:1, 77  ) = [-13.6364, -0.962]
  cl(0:1, 78  ) = [-13.0303, -0.9324]
  cl(0:1, 79  ) = [-12.4242, -0.9008]
  cl(0:1, 80  ) = [-11.8182, -0.8688]
  cl(0:1, 81  ) = [-11.2121, -0.8368]
  cl(0:1, 82  ) = [-10.6061, -0.8033]
  cl(0:1, 83  ) = [-10.0, -0.7681]
  cl(0:1, 84  ) = [-9.3939, -0.7397]
  cl(0:1, 85  ) = [-8.7879, -0.7178]
  cl(0:1, 86  ) = [-8.1818, -0.7045]
  cl(0:1, 87  ) = [-7.5758, -0.6632]
  cl(0:1, 88  ) = [-6.9697, -0.609]
  cl(0:1, 89  ) = [-6.3636, -0.5337]
  cl(0:1, 90  ) = [-5.7576, -0.4576]
  cl(0:1, 91  ) = [-5.1515, -0.3809]
  cl(0:1, 92  ) = [-4.5455, -0.3033]
  cl(0:1, 93  ) = [-3.9394, -0.2256]
  cl(0:1, 94  ) = [-3.3333, -0.1518]
  cl(0:1, 95  ) = [-2.7273, -0.0773]
  cl(0:1, 96  ) = [-2.1212, -0.0021]
  cl(0:1, 97  ) = [-1.5152, 0.0742]
  cl(0:1, 98  ) = [-0.9091, 0.1511]
  cl(0:1, 99  ) = [-0.303, 0.2293]
  cl(0:1, 100  ) = [0.303, 0.3078]
  cl(0:1, 101  ) = [0.9091, 0.3864]
  cl(0:1, 102  ) = [1.5152, 0.4652]
  cl(0:1, 103  ) = [2.1212, 0.5437]
  cl(0:1, 104  ) = [2.7273, 0.6214]
  cl(0:1, 105  ) = [3.3333, 0.6982]
  cl(0:1, 106  ) = [3.9394, 0.7746]
  cl(0:1, 107  ) = [4.5455, 0.8491]
  cl(0:1, 108  ) = [5.1515, 0.9228]
  cl(0:1, 109  ) = [5.7576, 0.9943]
  cl(0:1, 110  ) = [6.3636, 1.0631]
  cl(0:1, 111  ) = [6.9697, 1.1302]
  cl(0:1, 112  ) = [7.5758, 1.1924]
  cl(0:1, 113  ) = [8.1818, 1.2525]
  cl(0:1, 114  ) = [8.7879, 1.3102]
  cl(0:1, 115  ) = [9.3939, 1.3635]
  cl(0:1, 116  ) = [10.0, 1.41]
  cl(0:1, 117  ) = [10.6061, 1.4392]
  cl(0:1, 118  ) = [11.2121, 1.4563]
  cl(0:1, 119  ) = [11.8182, 1.4505]
  cl(0:1, 120  ) = [12.4242, 1.3774]
  cl(0:1, 121  ) = [13.0303, 1.254]
  cl(0:1, 122  ) = [13.6364, 1.2064]
  cl(0:1, 123  ) = [14.2424, 1.1651]
  cl(0:1, 124  ) = [14.8485, 1.1385]
  cl(0:1, 125  ) = [15.4545, 1.1201]
  cl(0:1, 126  ) = [16.0606, 1.1052]
  cl(0:1, 127  ) = [16.6667, 1.0961]
  cl(0:1, 128  ) = [17.2727, 1.0904]
  cl(0:1, 129  ) = [17.8788, 1.089]
  cl(0:1, 130  ) = [18.4848, 1.0985]
  cl(0:1, 131  ) = [19.0909, 1.109]
  cl(0:1, 132  ) = [19.697, 1.1093]
  cl(0:1, 133  ) = [20.303, 1.1087]
  cl(0:1, 134  ) = [20.9091, 1.1072]
  cl(0:1, 135  ) = [21.5152, 1.1056]
  cl(0:1, 136  ) = [22.1212, 1.1026]
  cl(0:1, 137  ) = [22.7273, 1.094]
  cl(0:1, 138  ) = [23.3333, 1.0855]
  cl(0:1, 139  ) = [23.9394, 1.0769]
  cl(0:1, 140  ) = [24.5455, 1.07]
  cl(0:1, 141  ) = [25.1515, 1.0634]
  cl(0:1, 142  ) = [25.7576, 1.0567]
  cl(0:1, 143  ) = [26.3636, 1.0508]
  cl(0:1, 144  ) = [26.9697, 1.0454]
  cl(0:1, 145  ) = [27.5758, 1.04]
  cl(0:1, 146  ) = [28.1818, 1.0348]
  cl(0:1, 147  ) = [28.7879, 1.0303]
  cl(0:1, 148  ) = [29.3939, 1.0257]
  cl(0:1, 149  ) = [30.0, 1.0211]
  cl(0:1, 150  ) = [33.0, 1.0012]
  cl(0:1, 151  ) = [36.0, 0.9819]
  cl(0:1, 152  ) = [39.0, 0.9613]
  cl(0:1, 153  ) = [42.0, 0.9369]
  cl(0:1, 154  ) = [45.0, 0.9113]
  cl(0:1, 155  ) = [48.0, 0.8784]
  cl(0:1, 156  ) = [51.0, 0.8412]
  cl(0:1, 157  ) = [54.0, 0.7957]
  cl(0:1, 158  ) = [57.0, 0.7502]
  cl(0:1, 159  ) = [60.0, 0.7046]
  cl(0:1, 160  ) = [63.0, 0.6431]
  cl(0:1, 161  ) = [66.0, 0.5816]
  cl(0:1, 162  ) = [69.0, 0.52]
  cl(0:1, 163  ) = [72.0, 0.4509]
  cl(0:1, 164  ) = [75.0, 0.378]
  cl(0:1, 165  ) = [78.0, 0.3051]
  cl(0:1, 166  ) = [81.0, 0.2308]
  cl(0:1, 167  ) = [84.0, 0.1539]
  cl(0:1, 168  ) = [87.0, 0.0769]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.0539]
  cl(0:1, 171  ) = [96.0, -0.1077]
  cl(0:1, 172  ) = [99.0, -0.1616]
  cl(0:1, 173  ) = [102.0, -0.2136]
  cl(0:1, 174  ) = [105.0, -0.2646]
  cl(0:1, 175  ) = [108.0, -0.3156]
  cl(0:1, 176  ) = [111.0, -0.364]
  cl(0:1, 177  ) = [114.0, -0.4071]
  cl(0:1, 178  ) = [117.0, -0.4502]
  cl(0:1, 179  ) = [120.0, -0.4932]
  cl(0:1, 180  ) = [123.0, -0.5251]
  cl(0:1, 181  ) = [126.0, -0.557]
  cl(0:1, 182  ) = [129.0, -0.5889]
  cl(0:1, 183  ) = [132.0, -0.6132]
  cl(0:1, 184  ) = [135.0, -0.6337]
  cl(0:1, 185  ) = [138.0, -0.6542]
  cl(0:1, 186  ) = [141.0, -0.6725]
  cl(0:1, 187  ) = [144.0, -0.6866]
  cl(0:1, 188  ) = [147.0, -0.7007]
  cl(0:1, 189  ) = [150.0, -0.7148]
  cl(0:1, 190  ) = [153.0, -0.7261]
  cl(0:1, 191  ) = [156.0, -0.7374]
  cl(0:1, 192  ) = [159.0, -0.7494]
  cl(0:1, 193  ) = [162.0, -0.6785]
  cl(0:1, 194  ) = [165.0, -0.5654]
  cl(0:1, 195  ) = [168.0, -0.4449]
  cl(0:1, 196  ) = [171.0, -0.3315]
  cl(0:1, 197  ) = [174.0, -0.221]
  cl(0:1, 198  ) = [177.0, -0.1096]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl13 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl13 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl13 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl13_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl13 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl13_nrel2p8.pro' 
  ENDIF

!print,y,cl13

END FUNCTION calc_cl13_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cl14_nrel2p8( y ) RESULT( cl14 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl14
INTEGER :: i

 cl14 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.1136]
  cl(0:1, 2  ) = [-174.0, 0.2289]
  cl(0:1, 3  ) = [-171.0, 0.3433]
  cl(0:1, 4  ) = [-168.0, 0.4571]
  cl(0:1, 5  ) = [-165.0, 0.5711]
  cl(0:1, 6  ) = [-162.0, 0.6818]
  cl(0:1, 7  ) = [-159.0, 0.7506]
  cl(0:1, 8  ) = [-156.0, 0.7375]
  cl(0:1, 9  ) = [-153.0, 0.7252]
  cl(0:1, 10  ) = [-150.0, 0.7129]
  cl(0:1, 11  ) = [-147.0, 0.6991]
  cl(0:1, 12  ) = [-144.0, 0.6852]
  cl(0:1, 13  ) = [-141.0, 0.6713]
  cl(0:1, 14  ) = [-138.0, 0.6531]
  cl(0:1, 15  ) = [-135.0, 0.6328]
  cl(0:1, 16  ) = [-132.0, 0.6124]
  cl(0:1, 17  ) = [-129.0, 0.5882]
  cl(0:1, 18  ) = [-126.0, 0.5564]
  cl(0:1, 19  ) = [-123.0, 0.5247]
  cl(0:1, 20  ) = [-120.0, 0.4929]
  cl(0:1, 21  ) = [-117.0, 0.4499]
  cl(0:1, 22  ) = [-114.0, 0.4069]
  cl(0:1, 23  ) = [-111.0, 0.3639]
  cl(0:1, 24  ) = [-108.0, 0.3155]
  cl(0:1, 25  ) = [-105.0, 0.2645]
  cl(0:1, 26  ) = [-102.0, 0.2135]
  cl(0:1, 27  ) = [-99.0, 0.1616]
  cl(0:1, 28  ) = [-96.0, 0.1077]
  cl(0:1, 29  ) = [-93.0, 0.0539]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.0539]
  cl(0:1, 32  ) = [-84.0, -0.1077]
  cl(0:1, 33  ) = [-81.0, -0.1616]
  cl(0:1, 34  ) = [-78.0, -0.2135]
  cl(0:1, 35  ) = [-75.0, -0.2645]
  cl(0:1, 36  ) = [-72.0, -0.3155]
  cl(0:1, 37  ) = [-69.0, -0.3639]
  cl(0:1, 38  ) = [-66.0, -0.4069]
  cl(0:1, 39  ) = [-63.0, -0.4499]
  cl(0:1, 40  ) = [-60.0, -0.4929]
  cl(0:1, 41  ) = [-57.0, -0.5247]
  cl(0:1, 42  ) = [-54.0, -0.5564]
  cl(0:1, 43  ) = [-51.0, -0.5882]
  cl(0:1, 44  ) = [-48.0, -0.6141]
  cl(0:1, 45  ) = [-45.0, -0.637]
  cl(0:1, 46  ) = [-42.0, -0.6548]
  cl(0:1, 47  ) = [-39.0, -0.6717]
  cl(0:1, 48  ) = [-36.0, -0.686]
  cl(0:1, 49  ) = [-33.0, -0.6992]
  cl(0:1, 50  ) = [-30.0, -0.7129]
  cl(0:1, 51  ) = [-29.3939, -0.7161]
  cl(0:1, 52  ) = [-28.7879, -0.7192]
  cl(0:1, 53  ) = [-28.1818, -0.7223]
  cl(0:1, 54  ) = [-27.5758, -0.7259]
  cl(0:1, 55  ) = [-26.9697, -0.7296]
  cl(0:1, 56  ) = [-26.3636, -0.7333]
  cl(0:1, 57  ) = [-25.7576, -0.7374]
  cl(0:1, 58  ) = [-25.1515, -0.742]
  cl(0:1, 59  ) = [-24.5455, -0.7465]
  cl(0:1, 60  ) = [-23.9394, -0.7513]
  cl(0:1, 61  ) = [-23.3333, -0.7572]
  cl(0:1, 62  ) = [-22.7273, -0.7631]
  cl(0:1, 63  ) = [-22.1212, -0.769]
  cl(0:1, 64  ) = [-21.5152, -0.7986]
  cl(0:1, 65  ) = [-20.9091, -0.84]
  cl(0:1, 66  ) = [-20.303, -0.8776]
  cl(0:1, 67  ) = [-19.697, -0.9125]
  cl(0:1, 68  ) = [-19.0909, -0.9447]
  cl(0:1, 69  ) = [-18.4848, -0.9766]
  cl(0:1, 70  ) = [-17.8788, -1.0083]
  cl(0:1, 71  ) = [-17.2727, -1.04]
  cl(0:1, 72  ) = [-16.6667, -1.0716]
  cl(0:1, 73  ) = [-16.0606, -1.1031]
  cl(0:1, 74  ) = [-15.4545, -1.1151]
  cl(0:1, 75  ) = [-14.8485, -1.1148]
  cl(0:1, 76  ) = [-14.2424, -1.0842]
  cl(0:1, 77  ) = [-13.6364, -1.049]
  cl(0:1, 78  ) = [-13.0303, -1.0105]
  cl(0:1, 79  ) = [-12.4242, -0.9692]
  cl(0:1, 80  ) = [-11.8182, -0.9278]
  cl(0:1, 81  ) = [-11.2121, -0.8865]
  cl(0:1, 82  ) = [-10.6061, -0.8435]
  cl(0:1, 83  ) = [-10.0, -0.7989]
  cl(0:1, 84  ) = [-9.3939, -0.7634]
  cl(0:1, 85  ) = [-8.7879, -0.7369]
  cl(0:1, 86  ) = [-8.1818, -0.7219]
  cl(0:1, 87  ) = [-7.5758, -0.6801]
  cl(0:1, 88  ) = [-6.9697, -0.6255]
  cl(0:1, 89  ) = [-6.3636, -0.5454]
  cl(0:1, 90  ) = [-5.7576, -0.4652]
  cl(0:1, 91  ) = [-5.1515, -0.3852]
  cl(0:1, 92  ) = [-4.5455, -0.3052]
  cl(0:1, 93  ) = [-3.9394, -0.2255]
  cl(0:1, 94  ) = [-3.3333, -0.1501]
  cl(0:1, 95  ) = [-2.7273, -0.0745]
  cl(0:1, 96  ) = [-2.1212, 0.0014]
  cl(0:1, 97  ) = [-1.5152, 0.078]
  cl(0:1, 98  ) = [-0.9091, 0.1551]
  cl(0:1, 99  ) = [-0.303, 0.2337]
  cl(0:1, 100  ) = [0.303, 0.3122]
  cl(0:1, 101  ) = [0.9091, 0.3907]
  cl(0:1, 102  ) = [1.5152, 0.4692]
  cl(0:1, 103  ) = [2.1212, 0.5474]
  cl(0:1, 104  ) = [2.7273, 0.6244]
  cl(0:1, 105  ) = [3.3333, 0.7007]
  cl(0:1, 106  ) = [3.9394, 0.7767]
  cl(0:1, 107  ) = [4.5455, 0.8511]
  cl(0:1, 108  ) = [5.1515, 0.9246]
  cl(0:1, 109  ) = [5.7576, 0.9961]
  cl(0:1, 110  ) = [6.3636, 1.0653]
  cl(0:1, 111  ) = [6.9697, 1.1329]
  cl(0:1, 112  ) = [7.5758, 1.1965]
  cl(0:1, 113  ) = [8.1818, 1.2585]
  cl(0:1, 114  ) = [8.7879, 1.3195]
  cl(0:1, 115  ) = [9.3939, 1.3772]
  cl(0:1, 116  ) = [10.0, 1.4278]
  cl(0:1, 117  ) = [10.6061, 1.4635]
  cl(0:1, 118  ) = [11.2121, 1.4885]
  cl(0:1, 119  ) = [11.8182, 1.4938]
  cl(0:1, 120  ) = [12.4242, 1.3902]
  cl(0:1, 121  ) = [13.0303, 1.2356]
  cl(0:1, 122  ) = [13.6364, 1.1798]
  cl(0:1, 123  ) = [14.2424, 1.1326]
  cl(0:1, 124  ) = [14.8485, 1.1043]
  cl(0:1, 125  ) = [15.4545, 1.0851]
  cl(0:1, 126  ) = [16.0606, 1.0696]
  cl(0:1, 127  ) = [16.6667, 1.0606]
  cl(0:1, 128  ) = [17.2727, 1.0557]
  cl(0:1, 129  ) = [17.8788, 1.0555]
  cl(0:1, 130  ) = [18.4848, 1.0673]
  cl(0:1, 131  ) = [19.0909, 1.0808]
  cl(0:1, 132  ) = [19.697, 1.0876]
  cl(0:1, 133  ) = [20.303, 1.0924]
  cl(0:1, 134  ) = [20.9091, 1.0952]
  cl(0:1, 135  ) = [21.5152, 1.098]
  cl(0:1, 136  ) = [22.1212, 1.0985]
  cl(0:1, 137  ) = [22.7273, 1.0901]
  cl(0:1, 138  ) = [23.3333, 1.0817]
  cl(0:1, 139  ) = [23.9394, 1.0732]
  cl(0:1, 140  ) = [24.5455, 1.0665]
  cl(0:1, 141  ) = [25.1515, 1.0599]
  cl(0:1, 142  ) = [25.7576, 1.0534]
  cl(0:1, 143  ) = [26.3636, 1.0476]
  cl(0:1, 144  ) = [26.9697, 1.0423]
  cl(0:1, 145  ) = [27.5758, 1.037]
  cl(0:1, 146  ) = [28.1818, 1.0319]
  cl(0:1, 147  ) = [28.7879, 1.0274]
  cl(0:1, 148  ) = [29.3939, 1.0229]
  cl(0:1, 149  ) = [30.0, 1.0185]
  cl(0:1, 150  ) = [33.0, 0.9989]
  cl(0:1, 151  ) = [36.0, 0.9799]
  cl(0:1, 152  ) = [39.0, 0.9596]
  cl(0:1, 153  ) = [42.0, 0.9355]
  cl(0:1, 154  ) = [45.0, 0.91]
  cl(0:1, 155  ) = [48.0, 0.8773]
  cl(0:1, 156  ) = [51.0, 0.8403]
  cl(0:1, 157  ) = [54.0, 0.7949]
  cl(0:1, 158  ) = [57.0, 0.7495]
  cl(0:1, 159  ) = [60.0, 0.7041]
  cl(0:1, 160  ) = [63.0, 0.6427]
  cl(0:1, 161  ) = [66.0, 0.5812]
  cl(0:1, 162  ) = [69.0, 0.5198]
  cl(0:1, 163  ) = [72.0, 0.4507]
  cl(0:1, 164  ) = [75.0, 0.3779]
  cl(0:1, 165  ) = [78.0, 0.305]
  cl(0:1, 166  ) = [81.0, 0.2308]
  cl(0:1, 167  ) = [84.0, 0.1539]
  cl(0:1, 168  ) = [87.0, 0.0769]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.0539]
  cl(0:1, 171  ) = [96.0, -0.1077]
  cl(0:1, 172  ) = [99.0, -0.1616]
  cl(0:1, 173  ) = [102.0, -0.2135]
  cl(0:1, 174  ) = [105.0, -0.2645]
  cl(0:1, 175  ) = [108.0, -0.3155]
  cl(0:1, 176  ) = [111.0, -0.3639]
  cl(0:1, 177  ) = [114.0, -0.4069]
  cl(0:1, 178  ) = [117.0, -0.4499]
  cl(0:1, 179  ) = [120.0, -0.4929]
  cl(0:1, 180  ) = [123.0, -0.5247]
  cl(0:1, 181  ) = [126.0, -0.5564]
  cl(0:1, 182  ) = [129.0, -0.5882]
  cl(0:1, 183  ) = [132.0, -0.6124]
  cl(0:1, 184  ) = [135.0, -0.6328]
  cl(0:1, 185  ) = [138.0, -0.6531]
  cl(0:1, 186  ) = [141.0, -0.6713]
  cl(0:1, 187  ) = [144.0, -0.6852]
  cl(0:1, 188  ) = [147.0, -0.6991]
  cl(0:1, 189  ) = [150.0, -0.7129]
  cl(0:1, 190  ) = [153.0, -0.7177]
  cl(0:1, 191  ) = [156.0, -0.7225]
  cl(0:1, 192  ) = [159.0, -0.7281]
  cl(0:1, 193  ) = [162.0, -0.6575]
  cl(0:1, 194  ) = [165.0, -0.548]
  cl(0:1, 195  ) = [168.0, -0.4335]
  cl(0:1, 196  ) = [171.0, -0.3237]
  cl(0:1, 197  ) = [174.0, -0.2158]
  cl(0:1, 198  ) = [177.0, -0.1072]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl14 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl14 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl14 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl14_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl14 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl14_nrel2p8.pro' 
  ENDIF

!print,y,cl14

END FUNCTION calc_cl14_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cl15_nrel2p8( y ) RESULT( cl15 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl15
INTEGER :: i

 cl15 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.111]
  cl(0:1, 2  ) = [-174.0, 0.2232]
  cl(0:1, 3  ) = [-171.0, 0.3349]
  cl(0:1, 4  ) = [-168.0, 0.4464]
  cl(0:1, 5  ) = [-165.0, 0.558]
  cl(0:1, 6  ) = [-162.0, 0.6683]
  cl(0:1, 7  ) = [-159.0, 0.7381]
  cl(0:1, 8  ) = [-156.0, 0.7287]
  cl(0:1, 9  ) = [-153.0, 0.72]
  cl(0:1, 10  ) = [-150.0, 0.7113]
  cl(0:1, 11  ) = [-147.0, 0.6976]
  cl(0:1, 12  ) = [-144.0, 0.684]
  cl(0:1, 13  ) = [-141.0, 0.6703]
  cl(0:1, 14  ) = [-138.0, 0.6522]
  cl(0:1, 15  ) = [-135.0, 0.632]
  cl(0:1, 16  ) = [-132.0, 0.6117]
  cl(0:1, 17  ) = [-129.0, 0.5877]
  cl(0:1, 18  ) = [-126.0, 0.556]
  cl(0:1, 19  ) = [-123.0, 0.5243]
  cl(0:1, 20  ) = [-120.0, 0.4926]
  cl(0:1, 21  ) = [-117.0, 0.4496]
  cl(0:1, 22  ) = [-114.0, 0.4067]
  cl(0:1, 23  ) = [-111.0, 0.3637]
  cl(0:1, 24  ) = [-108.0, 0.3154]
  cl(0:1, 25  ) = [-105.0, 0.2644]
  cl(0:1, 26  ) = [-102.0, 0.2135]
  cl(0:1, 27  ) = [-99.0, 0.1615]
  cl(0:1, 28  ) = [-96.0, 0.1077]
  cl(0:1, 29  ) = [-93.0, 0.0538]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.0538]
  cl(0:1, 32  ) = [-84.0, -0.1077]
  cl(0:1, 33  ) = [-81.0, -0.1615]
  cl(0:1, 34  ) = [-78.0, -0.2135]
  cl(0:1, 35  ) = [-75.0, -0.2644]
  cl(0:1, 36  ) = [-72.0, -0.3154]
  cl(0:1, 37  ) = [-69.0, -0.3637]
  cl(0:1, 38  ) = [-66.0, -0.4067]
  cl(0:1, 39  ) = [-63.0, -0.4496]
  cl(0:1, 40  ) = [-60.0, -0.4926]
  cl(0:1, 41  ) = [-57.0, -0.5243]
  cl(0:1, 42  ) = [-54.0, -0.556]
  cl(0:1, 43  ) = [-51.0, -0.5877]
  cl(0:1, 44  ) = [-48.0, -0.6135]
  cl(0:1, 45  ) = [-45.0, -0.6363]
  cl(0:1, 46  ) = [-42.0, -0.6539]
  cl(0:1, 47  ) = [-39.0, -0.6707]
  cl(0:1, 48  ) = [-36.0, -0.6848]
  cl(0:1, 49  ) = [-33.0, -0.6979]
  cl(0:1, 50  ) = [-30.0, -0.7113]
  cl(0:1, 51  ) = [-29.3939, -0.7144]
  cl(0:1, 52  ) = [-28.7879, -0.7175]
  cl(0:1, 53  ) = [-28.1818, -0.7206]
  cl(0:1, 54  ) = [-27.5758, -0.7241]
  cl(0:1, 55  ) = [-26.9697, -0.7277]
  cl(0:1, 56  ) = [-26.3636, -0.7314]
  cl(0:1, 57  ) = [-25.7576, -0.7354]
  cl(0:1, 58  ) = [-25.1515, -0.7399]
  cl(0:1, 59  ) = [-24.5455, -0.7444]
  cl(0:1, 60  ) = [-23.9394, -0.7491]
  cl(0:1, 61  ) = [-23.3333, -0.7549]
  cl(0:1, 62  ) = [-22.7273, -0.7607]
  cl(0:1, 63  ) = [-22.1212, -0.7666]
  cl(0:1, 64  ) = [-21.5152, -0.8007]
  cl(0:1, 65  ) = [-20.9091, -0.847]
  cl(0:1, 66  ) = [-20.303, -0.8906]
  cl(0:1, 67  ) = [-19.697, -0.9331]
  cl(0:1, 68  ) = [-19.0909, -0.9746]
  cl(0:1, 69  ) = [-18.4848, -1.016]
  cl(0:1, 70  ) = [-17.8788, -1.0574]
  cl(0:1, 71  ) = [-17.2727, -1.0986]
  cl(0:1, 72  ) = [-16.6667, -1.1399]
  cl(0:1, 73  ) = [-16.0606, -1.1812]
  cl(0:1, 74  ) = [-15.4545, -1.1979]
  cl(0:1, 75  ) = [-14.8485, -1.1993]
  cl(0:1, 76  ) = [-14.2424, -1.163]
  cl(0:1, 77  ) = [-13.6364, -1.1208]
  cl(0:1, 78  ) = [-13.0303, -1.0746]
  cl(0:1, 79  ) = [-12.4242, -1.0252]
  cl(0:1, 80  ) = [-11.8182, -0.9757]
  cl(0:1, 81  ) = [-11.2121, -0.9266]
  cl(0:1, 82  ) = [-10.6061, -0.8757]
  cl(0:1, 83  ) = [-10.0, -0.8232]
  cl(0:1, 84  ) = [-9.3939, -0.7821]
  cl(0:1, 85  ) = [-8.7879, -0.7514]
  cl(0:1, 86  ) = [-8.1818, -0.7351]
  cl(0:1, 87  ) = [-7.5758, -0.6928]
  cl(0:1, 88  ) = [-6.9697, -0.6379]
  cl(0:1, 89  ) = [-6.3636, -0.5542]
  cl(0:1, 90  ) = [-5.7576, -0.4708]
  cl(0:1, 91  ) = [-5.1515, -0.3883]
  cl(0:1, 92  ) = [-4.5455, -0.3065]
  cl(0:1, 93  ) = [-3.9394, -0.2253]
  cl(0:1, 94  ) = [-3.3333, -0.148]
  cl(0:1, 95  ) = [-2.7273, -0.071]
  cl(0:1, 96  ) = [-2.1212, 0.0057]
  cl(0:1, 97  ) = [-1.5152, 0.0826]
  cl(0:1, 98  ) = [-0.9091, 0.16]
  cl(0:1, 99  ) = [-0.303, 0.239]
  cl(0:1, 100  ) = [0.303, 0.3176]
  cl(0:1, 101  ) = [0.9091, 0.396]
  cl(0:1, 102  ) = [1.5152, 0.4742]
  cl(0:1, 103  ) = [2.1212, 0.5519]
  cl(0:1, 104  ) = [2.7273, 0.6281]
  cl(0:1, 105  ) = [3.3333, 0.7038]
  cl(0:1, 106  ) = [3.9394, 0.7793]
  cl(0:1, 107  ) = [4.5455, 0.8535]
  cl(0:1, 108  ) = [5.1515, 0.9269]
  cl(0:1, 109  ) = [5.7576, 0.9985]
  cl(0:1, 110  ) = [6.3636, 1.068]
  cl(0:1, 111  ) = [6.9697, 1.1364]
  cl(0:1, 112  ) = [7.5758, 1.2014]
  cl(0:1, 113  ) = [8.1818, 1.2653]
  cl(0:1, 114  ) = [8.7879, 1.3286]
  cl(0:1, 115  ) = [9.3939, 1.3889]
  cl(0:1, 116  ) = [10.0, 1.4431]
  cl(0:1, 117  ) = [10.6061, 1.4842]
  cl(0:1, 118  ) = [11.2121, 1.5161]
  cl(0:1, 119  ) = [11.8182, 1.5307]
  cl(0:1, 120  ) = [12.4242, 1.3998]
  cl(0:1, 121  ) = [13.0303, 1.2162]
  cl(0:1, 122  ) = [13.6364, 1.1553]
  cl(0:1, 123  ) = [14.2424, 1.1048]
  cl(0:1, 124  ) = [14.8485, 1.0752]
  cl(0:1, 125  ) = [15.4545, 1.0551]
  cl(0:1, 126  ) = [16.0606, 1.0391]
  cl(0:1, 127  ) = [16.6667, 1.0304]
  cl(0:1, 128  ) = [17.2727, 1.026]
  cl(0:1, 129  ) = [17.8788, 1.0269]
  cl(0:1, 130  ) = [18.4848, 1.0405]
  cl(0:1, 131  ) = [19.0909, 1.0566]
  cl(0:1, 132  ) = [19.697, 1.0689]
  cl(0:1, 133  ) = [20.303, 1.0784]
  cl(0:1, 134  ) = [20.9091, 1.0849]
  cl(0:1, 135  ) = [21.5152, 1.0915]
  cl(0:1, 136  ) = [22.1212, 1.0951]
  cl(0:1, 137  ) = [22.7273, 1.0868]
  cl(0:1, 138  ) = [23.3333, 1.0784]
  cl(0:1, 139  ) = [23.9394, 1.0701]
  cl(0:1, 140  ) = [24.5455, 1.0635]
  cl(0:1, 141  ) = [25.1515, 1.057]
  cl(0:1, 142  ) = [25.7576, 1.0505]
  cl(0:1, 143  ) = [26.3636, 1.0448]
  cl(0:1, 144  ) = [26.9697, 1.0396]
  cl(0:1, 145  ) = [27.5758, 1.0344]
  cl(0:1, 146  ) = [28.1818, 1.0294]
  cl(0:1, 147  ) = [28.7879, 1.025]
  cl(0:1, 148  ) = [29.3939, 1.0206]
  cl(0:1, 149  ) = [30.0, 1.0162]
  cl(0:1, 150  ) = [33.0, 0.9969]
  cl(0:1, 151  ) = [36.0, 0.9783]
  cl(0:1, 152  ) = [39.0, 0.9581]
  cl(0:1, 153  ) = [42.0, 0.9342]
  cl(0:1, 154  ) = [45.0, 0.909]
  cl(0:1, 155  ) = [48.0, 0.8764]
  cl(0:1, 156  ) = [51.0, 0.8395]
  cl(0:1, 157  ) = [54.0, 0.7943]
  cl(0:1, 158  ) = [57.0, 0.749]
  cl(0:1, 159  ) = [60.0, 0.7037]
  cl(0:1, 160  ) = [63.0, 0.6423]
  cl(0:1, 161  ) = [66.0, 0.5809]
  cl(0:1, 162  ) = [69.0, 0.5196]
  cl(0:1, 163  ) = [72.0, 0.4506]
  cl(0:1, 164  ) = [75.0, 0.3778]
  cl(0:1, 165  ) = [78.0, 0.3049]
  cl(0:1, 166  ) = [81.0, 0.2307]
  cl(0:1, 167  ) = [84.0, 0.1538]
  cl(0:1, 168  ) = [87.0, 0.0769]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.0538]
  cl(0:1, 171  ) = [96.0, -0.1077]
  cl(0:1, 172  ) = [99.0, -0.1615]
  cl(0:1, 173  ) = [102.0, -0.2135]
  cl(0:1, 174  ) = [105.0, -0.2644]
  cl(0:1, 175  ) = [108.0, -0.3154]
  cl(0:1, 176  ) = [111.0, -0.3637]
  cl(0:1, 177  ) = [114.0, -0.4067]
  cl(0:1, 178  ) = [117.0, -0.4496]
  cl(0:1, 179  ) = [120.0, -0.4926]
  cl(0:1, 180  ) = [123.0, -0.5243]
  cl(0:1, 181  ) = [126.0, -0.556]
  cl(0:1, 182  ) = [129.0, -0.5877]
  cl(0:1, 183  ) = [132.0, -0.6117]
  cl(0:1, 184  ) = [135.0, -0.632]
  cl(0:1, 185  ) = [138.0, -0.6522]
  cl(0:1, 186  ) = [141.0, -0.6703]
  cl(0:1, 187  ) = [144.0, -0.684]
  cl(0:1, 188  ) = [147.0, -0.6976]
  cl(0:1, 189  ) = [150.0, -0.7113]
  cl(0:1, 190  ) = [153.0, -0.7106]
  cl(0:1, 191  ) = [156.0, -0.7098]
  cl(0:1, 192  ) = [159.0, -0.7097]
  cl(0:1, 193  ) = [162.0, -0.6392]
  cl(0:1, 194  ) = [165.0, -0.5327]
  cl(0:1, 195  ) = [168.0, -0.4242]
  cl(0:1, 196  ) = [171.0, -0.3175]
  cl(0:1, 197  ) = [174.0, -0.2117]
  cl(0:1, 198  ) = [177.0, -0.1053]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl15 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl15 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl15 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl15_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl15 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl15_nrel2p8.pro' 
  ENDIF

!print,y,cl15

END FUNCTION calc_cl15_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cl16_nrel2p8( y ) RESULT( cl16 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl16
INTEGER :: i

 cl16 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.1094]
  cl(0:1, 2  ) = [-174.0, 0.2198]
  cl(0:1, 3  ) = [-171.0, 0.3297]
  cl(0:1, 4  ) = [-168.0, 0.4397]
  cl(0:1, 5  ) = [-165.0, 0.5496]
  cl(0:1, 6  ) = [-162.0, 0.6594]
  cl(0:1, 7  ) = [-159.0, 0.7303]
  cl(0:1, 8  ) = [-156.0, 0.7235]
  cl(0:1, 9  ) = [-153.0, 0.717]
  cl(0:1, 10  ) = [-150.0, 0.7104]
  cl(0:1, 11  ) = [-147.0, 0.6968]
  cl(0:1, 12  ) = [-144.0, 0.6832]
  cl(0:1, 13  ) = [-141.0, 0.6697]
  cl(0:1, 14  ) = [-138.0, 0.6517]
  cl(0:1, 15  ) = [-135.0, 0.6315]
  cl(0:1, 16  ) = [-132.0, 0.6114]
  cl(0:1, 17  ) = [-129.0, 0.5874]
  cl(0:1, 18  ) = [-126.0, 0.5557]
  cl(0:1, 19  ) = [-123.0, 0.524]
  cl(0:1, 20  ) = [-120.0, 0.4924]
  cl(0:1, 21  ) = [-117.0, 0.4495]
  cl(0:1, 22  ) = [-114.0, 0.4065]
  cl(0:1, 23  ) = [-111.0, 0.3636]
  cl(0:1, 24  ) = [-108.0, 0.3153]
  cl(0:1, 25  ) = [-105.0, 0.2644]
  cl(0:1, 26  ) = [-102.0, 0.2134]
  cl(0:1, 27  ) = [-99.0, 0.1615]
  cl(0:1, 28  ) = [-96.0, 0.1077]
  cl(0:1, 29  ) = [-93.0, 0.0538]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.0538]
  cl(0:1, 32  ) = [-84.0, -0.1077]
  cl(0:1, 33  ) = [-81.0, -0.1615]
  cl(0:1, 34  ) = [-78.0, -0.2134]
  cl(0:1, 35  ) = [-75.0, -0.2644]
  cl(0:1, 36  ) = [-72.0, -0.3153]
  cl(0:1, 37  ) = [-69.0, -0.3636]
  cl(0:1, 38  ) = [-66.0, -0.4065]
  cl(0:1, 39  ) = [-63.0, -0.4495]
  cl(0:1, 40  ) = [-60.0, -0.4924]
  cl(0:1, 41  ) = [-57.0, -0.524]
  cl(0:1, 42  ) = [-54.0, -0.5557]
  cl(0:1, 43  ) = [-51.0, -0.5874]
  cl(0:1, 44  ) = [-48.0, -0.6131]
  cl(0:1, 45  ) = [-45.0, -0.6358]
  cl(0:1, 46  ) = [-42.0, -0.6534]
  cl(0:1, 47  ) = [-39.0, -0.6701]
  cl(0:1, 48  ) = [-36.0, -0.6841]
  cl(0:1, 49  ) = [-33.0, -0.697]
  cl(0:1, 50  ) = [-30.0, -0.7104]
  cl(0:1, 51  ) = [-29.3939, -0.7135]
  cl(0:1, 52  ) = [-28.7879, -0.7165]
  cl(0:1, 53  ) = [-28.1818, -0.7196]
  cl(0:1, 54  ) = [-27.5758, -0.723]
  cl(0:1, 55  ) = [-26.9697, -0.7266]
  cl(0:1, 56  ) = [-26.3636, -0.7302]
  cl(0:1, 57  ) = [-25.7576, -0.7342]
  cl(0:1, 58  ) = [-25.1515, -0.7387]
  cl(0:1, 59  ) = [-24.5455, -0.7432]
  cl(0:1, 60  ) = [-23.9394, -0.7478]
  cl(0:1, 61  ) = [-23.3333, -0.7536]
  cl(0:1, 62  ) = [-22.7273, -0.7594]
  cl(0:1, 63  ) = [-22.1212, -0.7651]
  cl(0:1, 64  ) = [-21.5152, -0.8033]
  cl(0:1, 65  ) = [-20.9091, -0.8508]
  cl(0:1, 66  ) = [-20.303, -0.8978]
  cl(0:1, 67  ) = [-19.697, -0.9447]
  cl(0:1, 68  ) = [-19.0909, -0.9916]
  cl(0:1, 69  ) = [-18.4848, -1.0384]
  cl(0:1, 70  ) = [-17.8788, -1.0853]
  cl(0:1, 71  ) = [-17.2727, -1.1322]
  cl(0:1, 72  ) = [-16.6667, -1.179]
  cl(0:1, 73  ) = [-16.0606, -1.2259]
  cl(0:1, 74  ) = [-15.4545, -1.2454]
  cl(0:1, 75  ) = [-14.8485, -1.2478]
  cl(0:1, 76  ) = [-14.2424, -1.208]
  cl(0:1, 77  ) = [-13.6364, -1.1618]
  cl(0:1, 78  ) = [-13.0303, -1.1112]
  cl(0:1, 79  ) = [-12.4242, -1.0569]
  cl(0:1, 80  ) = [-11.8182, -1.0027]
  cl(0:1, 81  ) = [-11.2121, -0.9491]
  cl(0:1, 82  ) = [-10.6061, -0.8936]
  cl(0:1, 83  ) = [-10.0, -0.837]
  cl(0:1, 84  ) = [-9.3939, -0.7932]
  cl(0:1, 85  ) = [-8.7879, -0.7592]
  cl(0:1, 86  ) = [-8.1818, -0.7421]
  cl(0:1, 87  ) = [-7.5758, -0.6996]
  cl(0:1, 88  ) = [-6.9697, -0.6446]
  cl(0:1, 89  ) = [-6.3636, -0.5588]
  cl(0:1, 90  ) = [-5.7576, -0.4738]
  cl(0:1, 91  ) = [-5.1515, -0.3899]
  cl(0:1, 92  ) = [-4.5455, -0.3072]
  cl(0:1, 93  ) = [-3.9394, -0.2251]
  cl(0:1, 94  ) = [-3.3333, -0.1452]
  cl(0:1, 95  ) = [-2.7273, -0.0664]
  cl(0:1, 96  ) = [-2.1212, 0.0112]
  cl(0:1, 97  ) = [-1.5152, 0.0886]
  cl(0:1, 98  ) = [-0.9091, 0.1662]
  cl(0:1, 99  ) = [-0.303, 0.2457]
  cl(0:1, 100  ) = [0.303, 0.3245]
  cl(0:1, 101  ) = [0.9091, 0.4027]
  cl(0:1, 102  ) = [1.5152, 0.4805]
  cl(0:1, 103  ) = [2.1212, 0.5577]
  cl(0:1, 104  ) = [2.7273, 0.6329]
  cl(0:1, 105  ) = [3.3333, 0.7078]
  cl(0:1, 106  ) = [3.9394, 0.7827]
  cl(0:1, 107  ) = [4.5455, 0.8567]
  cl(0:1, 108  ) = [5.1515, 0.93]
  cl(0:1, 109  ) = [5.7576, 1.0015]
  cl(0:1, 110  ) = [6.3636, 1.0716]
  cl(0:1, 111  ) = [6.9697, 1.1408]
  cl(0:1, 112  ) = [7.5758, 1.2075]
  cl(0:1, 113  ) = [8.1818, 1.2729]
  cl(0:1, 114  ) = [8.7879, 1.3363]
  cl(0:1, 115  ) = [9.3939, 1.3958]
  cl(0:1, 116  ) = [10.0, 1.452]
  cl(0:1, 117  ) = [10.6061, 1.4964]
  cl(0:1, 118  ) = [11.2121, 1.5322]
  cl(0:1, 119  ) = [11.8182, 1.5523]
  cl(0:1, 120  ) = [12.4242, 1.4049]
  cl(0:1, 121  ) = [13.0303, 1.1964]
  cl(0:1, 122  ) = [13.6364, 1.1369]
  cl(0:1, 123  ) = [14.2424, 1.0885]
  cl(0:1, 124  ) = [14.8485, 1.0581]
  cl(0:1, 125  ) = [15.4545, 1.0376]
  cl(0:1, 126  ) = [16.0606, 1.0213]
  cl(0:1, 127  ) = [16.6667, 1.0127]
  cl(0:1, 128  ) = [17.2727, 1.0086]
  cl(0:1, 129  ) = [17.8788, 1.0102]
  cl(0:1, 130  ) = [18.4848, 1.0249]
  cl(0:1, 131  ) = [19.0909, 1.0425]
  cl(0:1, 132  ) = [19.697, 1.058]
  cl(0:1, 133  ) = [20.303, 1.0702]
  cl(0:1, 134  ) = [20.9091, 1.0789]
  cl(0:1, 135  ) = [21.5152, 1.0877]
  cl(0:1, 136  ) = [22.1212, 1.0931]
  cl(0:1, 137  ) = [22.7273, 1.0848]
  cl(0:1, 138  ) = [23.3333, 1.0765]
  cl(0:1, 139  ) = [23.9394, 1.0683]
  cl(0:1, 140  ) = [24.5455, 1.0617]
  cl(0:1, 141  ) = [25.1515, 1.0553]
  cl(0:1, 142  ) = [25.7576, 1.0489]
  cl(0:1, 143  ) = [26.3636, 1.0432]
  cl(0:1, 144  ) = [26.9697, 1.038]
  cl(0:1, 145  ) = [27.5758, 1.0329]
  cl(0:1, 146  ) = [28.1818, 1.0279]
  cl(0:1, 147  ) = [28.7879, 1.0236]
  cl(0:1, 148  ) = [29.3939, 1.0192]
  cl(0:1, 149  ) = [30.0, 1.0149]
  cl(0:1, 150  ) = [33.0, 0.9958]
  cl(0:1, 151  ) = [36.0, 0.9773]
  cl(0:1, 152  ) = [39.0, 0.9572]
  cl(0:1, 153  ) = [42.0, 0.9335]
  cl(0:1, 154  ) = [45.0, 0.9083]
  cl(0:1, 155  ) = [48.0, 0.8758]
  cl(0:1, 156  ) = [51.0, 0.8391]
  cl(0:1, 157  ) = [54.0, 0.7939]
  cl(0:1, 158  ) = [57.0, 0.7486]
  cl(0:1, 159  ) = [60.0, 0.7034]
  cl(0:1, 160  ) = [63.0, 0.6421]
  cl(0:1, 161  ) = [66.0, 0.5808]
  cl(0:1, 162  ) = [69.0, 0.5194]
  cl(0:1, 163  ) = [72.0, 0.4505]
  cl(0:1, 164  ) = [75.0, 0.3777]
  cl(0:1, 165  ) = [78.0, 0.3049]
  cl(0:1, 166  ) = [81.0, 0.2307]
  cl(0:1, 167  ) = [84.0, 0.1538]
  cl(0:1, 168  ) = [87.0, 0.0769]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.0538]
  cl(0:1, 171  ) = [96.0, -0.1077]
  cl(0:1, 172  ) = [99.0, -0.1615]
  cl(0:1, 173  ) = [102.0, -0.2134]
  cl(0:1, 174  ) = [105.0, -0.2644]
  cl(0:1, 175  ) = [108.0, -0.3153]
  cl(0:1, 176  ) = [111.0, -0.3636]
  cl(0:1, 177  ) = [114.0, -0.4065]
  cl(0:1, 178  ) = [117.0, -0.4495]
  cl(0:1, 179  ) = [120.0, -0.4924]
  cl(0:1, 180  ) = [123.0, -0.524]
  cl(0:1, 181  ) = [126.0, -0.5557]
  cl(0:1, 182  ) = [129.0, -0.5874]
  cl(0:1, 183  ) = [132.0, -0.6114]
  cl(0:1, 184  ) = [135.0, -0.6315]
  cl(0:1, 185  ) = [138.0, -0.6517]
  cl(0:1, 186  ) = [141.0, -0.6697]
  cl(0:1, 187  ) = [144.0, -0.6832]
  cl(0:1, 188  ) = [147.0, -0.6968]
  cl(0:1, 189  ) = [150.0, -0.7104]
  cl(0:1, 190  ) = [153.0, -0.7064]
  cl(0:1, 191  ) = [156.0, -0.7023]
  cl(0:1, 192  ) = [159.0, -0.6985]
  cl(0:1, 193  ) = [162.0, -0.6276]
  cl(0:1, 194  ) = [165.0, -0.523]
  cl(0:1, 195  ) = [168.0, -0.4183]
  cl(0:1, 196  ) = [171.0, -0.3137]
  cl(0:1, 197  ) = [174.0, -0.2092]
  cl(0:1, 198  ) = [177.0, -0.1041]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl16 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl16 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl16 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl16_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl16 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl16_nrel2p8.pro' 
  ENDIF

!print,y,cl16

END FUNCTION calc_cl16_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cl17_nrel2p8( y ) RESULT( cl17 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl17
INTEGER :: i

 cl17 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.109]
  cl(0:1, 2  ) = [-174.0, 0.2191]
  cl(0:1, 3  ) = [-171.0, 0.3286]
  cl(0:1, 4  ) = [-168.0, 0.4381]
  cl(0:1, 5  ) = [-165.0, 0.5476]
  cl(0:1, 6  ) = [-162.0, 0.6572]
  cl(0:1, 7  ) = [-159.0, 0.729]
  cl(0:1, 8  ) = [-156.0, 0.7236]
  cl(0:1, 9  ) = [-153.0, 0.7178]
  cl(0:1, 10  ) = [-150.0, 0.7124]
  cl(0:1, 11  ) = [-147.0, 0.6986]
  cl(0:1, 12  ) = [-144.0, 0.6848]
  cl(0:1, 13  ) = [-141.0, 0.671]
  cl(0:1, 14  ) = [-138.0, 0.6528]
  cl(0:1, 15  ) = [-135.0, 0.6325]
  cl(0:1, 16  ) = [-132.0, 0.6122]
  cl(0:1, 17  ) = [-129.0, 0.5881]
  cl(0:1, 18  ) = [-126.0, 0.5563]
  cl(0:1, 19  ) = [-123.0, 0.5245]
  cl(0:1, 20  ) = [-120.0, 0.4928]
  cl(0:1, 21  ) = [-117.0, 0.4498]
  cl(0:1, 22  ) = [-114.0, 0.4068]
  cl(0:1, 23  ) = [-111.0, 0.3638]
  cl(0:1, 24  ) = [-108.0, 0.3155]
  cl(0:1, 25  ) = [-105.0, 0.2645]
  cl(0:1, 26  ) = [-102.0, 0.2135]
  cl(0:1, 27  ) = [-99.0, 0.1615]
  cl(0:1, 28  ) = [-96.0, 0.1077]
  cl(0:1, 29  ) = [-93.0, 0.0538]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.0538]
  cl(0:1, 32  ) = [-84.0, -0.1077]
  cl(0:1, 33  ) = [-81.0, -0.1615]
  cl(0:1, 34  ) = [-78.0, -0.2135]
  cl(0:1, 35  ) = [-75.0, -0.2645]
  cl(0:1, 36  ) = [-72.0, -0.3155]
  cl(0:1, 37  ) = [-69.0, -0.3638]
  cl(0:1, 38  ) = [-66.0, -0.4068]
  cl(0:1, 39  ) = [-63.0, -0.4498]
  cl(0:1, 40  ) = [-60.0, -0.4928]
  cl(0:1, 41  ) = [-57.0, -0.5245]
  cl(0:1, 42  ) = [-54.0, -0.5563]
  cl(0:1, 43  ) = [-51.0, -0.5881]
  cl(0:1, 44  ) = [-48.0, -0.6139]
  cl(0:1, 45  ) = [-45.0, -0.6368]
  cl(0:1, 46  ) = [-42.0, -0.6546]
  cl(0:1, 47  ) = [-39.0, -0.6714]
  cl(0:1, 48  ) = [-36.0, -0.6856]
  cl(0:1, 49  ) = [-33.0, -0.6988]
  cl(0:1, 50  ) = [-30.0, -0.7124]
  cl(0:1, 51  ) = [-29.3939, -0.7156]
  cl(0:1, 52  ) = [-28.7879, -0.7187]
  cl(0:1, 53  ) = [-28.1818, -0.7218]
  cl(0:1, 54  ) = [-27.5758, -0.7253]
  cl(0:1, 55  ) = [-26.9697, -0.729]
  cl(0:1, 56  ) = [-26.3636, -0.7327]
  cl(0:1, 57  ) = [-25.7576, -0.7368]
  cl(0:1, 58  ) = [-25.1515, -0.7413]
  cl(0:1, 59  ) = [-24.5455, -0.7459]
  cl(0:1, 60  ) = [-23.9394, -0.7506]
  cl(0:1, 61  ) = [-23.3333, -0.7566]
  cl(0:1, 62  ) = [-22.7273, -0.7626]
  cl(0:1, 63  ) = [-22.1212, -0.7687]
  cl(0:1, 64  ) = [-21.5152, -0.8072]
  cl(0:1, 65  ) = [-20.9091, -0.8501]
  cl(0:1, 66  ) = [-20.303, -0.8948]
  cl(0:1, 67  ) = [-19.697, -0.9393]
  cl(0:1, 68  ) = [-19.0909, -0.9837]
  cl(0:1, 69  ) = [-18.4848, -1.0284]
  cl(0:1, 70  ) = [-17.8788, -1.0731]
  cl(0:1, 71  ) = [-17.2727, -1.1179]
  cl(0:1, 72  ) = [-16.6667, -1.1629]
  cl(0:1, 73  ) = [-16.0606, -1.2081]
  cl(0:1, 74  ) = [-15.4545, -1.2278]
  cl(0:1, 75  ) = [-14.8485, -1.2315]
  cl(0:1, 76  ) = [-14.2424, -1.1956]
  cl(0:1, 77  ) = [-13.6364, -1.1532]
  cl(0:1, 78  ) = [-13.0303, -1.1062]
  cl(0:1, 79  ) = [-12.4242, -1.054]
  cl(0:1, 80  ) = [-11.8182, -1.0014]
  cl(0:1, 81  ) = [-11.2121, -0.9486]
  cl(0:1, 82  ) = [-10.6061, -0.8937]
  cl(0:1, 83  ) = [-10.0, -0.8381]
  cl(0:1, 84  ) = [-9.3939, -0.7954]
  cl(0:1, 85  ) = [-8.7879, -0.7591]
  cl(0:1, 86  ) = [-8.1818, -0.7399]
  cl(0:1, 87  ) = [-7.5758, -0.6955]
  cl(0:1, 88  ) = [-6.9697, -0.639]
  cl(0:1, 89  ) = [-6.3636, -0.5531]
  cl(0:1, 90  ) = [-5.7576, -0.4679]
  cl(0:1, 91  ) = [-5.1515, -0.3838]
  cl(0:1, 92  ) = [-4.5455, -0.3009]
  cl(0:1, 93  ) = [-3.9394, -0.2183]
  cl(0:1, 94  ) = [-3.3333, -0.1358]
  cl(0:1, 95  ) = [-2.7273, -0.0552]
  cl(0:1, 96  ) = [-2.1212, 0.0236]
  cl(0:1, 97  ) = [-1.5152, 0.1015]
  cl(0:1, 98  ) = [-0.9091, 0.1795]
  cl(0:1, 99  ) = [-0.303, 0.2591]
  cl(0:1, 100  ) = [0.303, 0.3378]
  cl(0:1, 101  ) = [0.9091, 0.4158]
  cl(0:1, 102  ) = [1.5152, 0.4931]
  cl(0:1, 103  ) = [2.1212, 0.5699]
  cl(0:1, 104  ) = [2.7273, 0.6443]
  cl(0:1, 105  ) = [3.3333, 0.7186]
  cl(0:1, 106  ) = [3.9394, 0.793]
  cl(0:1, 107  ) = [4.5455, 0.8668]
  cl(0:1, 108  ) = [5.1515, 0.9399]
  cl(0:1, 109  ) = [5.7576, 1.0114]
  cl(0:1, 110  ) = [6.3636, 1.0818]
  cl(0:1, 111  ) = [6.9697, 1.1515]
  cl(0:1, 112  ) = [7.5758, 1.2183]
  cl(0:1, 113  ) = [8.1818, 1.2828]
  cl(0:1, 114  ) = [8.7879, 1.3415]
  cl(0:1, 115  ) = [9.3939, 1.3918]
  cl(0:1, 116  ) = [10.0, 1.4407]
  cl(0:1, 117  ) = [10.6061, 1.4745]
  cl(0:1, 118  ) = [11.2121, 1.5021]
  cl(0:1, 119  ) = [11.8182, 1.5185]
  cl(0:1, 120  ) = [12.4242, 1.3811]
  cl(0:1, 121  ) = [13.0303, 1.1739]
  cl(0:1, 122  ) = [13.6364, 1.1272]
  cl(0:1, 123  ) = [14.2424, 1.0889]
  cl(0:1, 124  ) = [14.8485, 1.0601]
  cl(0:1, 125  ) = [15.4545, 1.0409]
  cl(0:1, 126  ) = [16.0606, 1.0257]
  cl(0:1, 127  ) = [16.6667, 1.0175]
  cl(0:1, 128  ) = [17.2727, 1.0139]
  cl(0:1, 129  ) = [17.8788, 1.0162]
  cl(0:1, 130  ) = [18.4848, 1.0306]
  cl(0:1, 131  ) = [19.0909, 1.0477]
  cl(0:1, 132  ) = [19.697, 1.0629]
  cl(0:1, 133  ) = [20.303, 1.075]
  cl(0:1, 134  ) = [20.9091, 1.0838]
  cl(0:1, 135  ) = [21.5152, 1.0921]
  cl(0:1, 136  ) = [22.1212, 1.0972]
  cl(0:1, 137  ) = [22.7273, 1.0889]
  cl(0:1, 138  ) = [23.3333, 1.0806]
  cl(0:1, 139  ) = [23.9394, 1.0723]
  cl(0:1, 140  ) = [24.5455, 1.0656]
  cl(0:1, 141  ) = [25.1515, 1.0591]
  cl(0:1, 142  ) = [25.7576, 1.0525]
  cl(0:1, 143  ) = [26.3636, 1.0467]
  cl(0:1, 144  ) = [26.9697, 1.0415]
  cl(0:1, 145  ) = [27.5758, 1.0362]
  cl(0:1, 146  ) = [28.1818, 1.0312]
  cl(0:1, 147  ) = [28.7879, 1.0267]
  cl(0:1, 148  ) = [29.3939, 1.0222]
  cl(0:1, 149  ) = [30.0, 1.0178]
  cl(0:1, 150  ) = [33.0, 0.9983]
  cl(0:1, 151  ) = [36.0, 0.9794]
  cl(0:1, 152  ) = [39.0, 0.9591]
  cl(0:1, 153  ) = [42.0, 0.9351]
  cl(0:1, 154  ) = [45.0, 0.9097]
  cl(0:1, 155  ) = [48.0, 0.877]
  cl(0:1, 156  ) = [51.0, 0.8401]
  cl(0:1, 157  ) = [54.0, 0.7947]
  cl(0:1, 158  ) = [57.0, 0.7493]
  cl(0:1, 159  ) = [60.0, 0.704]
  cl(0:1, 160  ) = [63.0, 0.6426]
  cl(0:1, 161  ) = [66.0, 0.5811]
  cl(0:1, 162  ) = [69.0, 0.5197]
  cl(0:1, 163  ) = [72.0, 0.4507]
  cl(0:1, 164  ) = [75.0, 0.3778]
  cl(0:1, 165  ) = [78.0, 0.305]
  cl(0:1, 166  ) = [81.0, 0.2308]
  cl(0:1, 167  ) = [84.0, 0.1539]
  cl(0:1, 168  ) = [87.0, 0.0769]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.0538]
  cl(0:1, 171  ) = [96.0, -0.1077]
  cl(0:1, 172  ) = [99.0, -0.1615]
  cl(0:1, 173  ) = [102.0, -0.2135]
  cl(0:1, 174  ) = [105.0, -0.2645]
  cl(0:1, 175  ) = [108.0, -0.3155]
  cl(0:1, 176  ) = [111.0, -0.3638]
  cl(0:1, 177  ) = [114.0, -0.4068]
  cl(0:1, 178  ) = [117.0, -0.4498]
  cl(0:1, 179  ) = [120.0, -0.4928]
  cl(0:1, 180  ) = [123.0, -0.5245]
  cl(0:1, 181  ) = [126.0, -0.5563]
  cl(0:1, 182  ) = [129.0, -0.5881]
  cl(0:1, 183  ) = [132.0, -0.6122]
  cl(0:1, 184  ) = [135.0, -0.6325]
  cl(0:1, 185  ) = [138.0, -0.6528]
  cl(0:1, 186  ) = [141.0, -0.671]
  cl(0:1, 187  ) = [144.0, -0.6848]
  cl(0:1, 188  ) = [147.0, -0.6986]
  cl(0:1, 189  ) = [150.0, -0.7124]
  cl(0:1, 190  ) = [153.0, -0.707]
  cl(0:1, 191  ) = [156.0, -0.7023]
  cl(0:1, 192  ) = [159.0, -0.6971]
  cl(0:1, 193  ) = [162.0, -0.6253]
  cl(0:1, 194  ) = [165.0, -0.521]
  cl(0:1, 195  ) = [168.0, -0.4168]
  cl(0:1, 196  ) = [171.0, -0.3126]
  cl(0:1, 197  ) = [174.0, -0.2084]
  cl(0:1, 198  ) = [177.0, -0.1037]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl17 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl17 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl17 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl17_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl17 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl17_nrel2p8.pro' 
  ENDIF

!print,y,cl17

END FUNCTION calc_cl17_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cl18_nrel2p8( y ) RESULT( cl18 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl18
INTEGER :: i

 cl18 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.1087]
  cl(0:1, 2  ) = [-174.0, 0.2184]
  cl(0:1, 3  ) = [-171.0, 0.3276]
  cl(0:1, 4  ) = [-168.0, 0.4369]
  cl(0:1, 5  ) = [-165.0, 0.5461]
  cl(0:1, 6  ) = [-162.0, 0.6553]
  cl(0:1, 7  ) = [-159.0, 0.728]
  cl(0:1, 8  ) = [-156.0, 0.7246]
  cl(0:1, 9  ) = [-153.0, 0.7213]
  cl(0:1, 10  ) = [-150.0, 0.7201]
  cl(0:1, 11  ) = [-147.0, 0.7053]
  cl(0:1, 12  ) = [-144.0, 0.6906]
  cl(0:1, 13  ) = [-141.0, 0.6759]
  cl(0:1, 14  ) = [-138.0, 0.6571]
  cl(0:1, 15  ) = [-135.0, 0.6362]
  cl(0:1, 16  ) = [-132.0, 0.6153]
  cl(0:1, 17  ) = [-129.0, 0.5907]
  cl(0:1, 18  ) = [-126.0, 0.5585]
  cl(0:1, 19  ) = [-123.0, 0.5264]
  cl(0:1, 20  ) = [-120.0, 0.4942]
  cl(0:1, 21  ) = [-117.0, 0.451]
  cl(0:1, 22  ) = [-114.0, 0.4078]
  cl(0:1, 23  ) = [-111.0, 0.3645]
  cl(0:1, 24  ) = [-108.0, 0.316]
  cl(0:1, 25  ) = [-105.0, 0.2649]
  cl(0:1, 26  ) = [-102.0, 0.2137]
  cl(0:1, 27  ) = [-99.0, 0.1617]
  cl(0:1, 28  ) = [-96.0, 0.1078]
  cl(0:1, 29  ) = [-93.0, 0.0539]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.0539]
  cl(0:1, 32  ) = [-84.0, -0.1078]
  cl(0:1, 33  ) = [-81.0, -0.1617]
  cl(0:1, 34  ) = [-78.0, -0.2137]
  cl(0:1, 35  ) = [-75.0, -0.2649]
  cl(0:1, 36  ) = [-72.0, -0.316]
  cl(0:1, 37  ) = [-69.0, -0.3645]
  cl(0:1, 38  ) = [-66.0, -0.4078]
  cl(0:1, 39  ) = [-63.0, -0.451]
  cl(0:1, 40  ) = [-60.0, -0.4942]
  cl(0:1, 41  ) = [-57.0, -0.5264]
  cl(0:1, 42  ) = [-54.0, -0.5585]
  cl(0:1, 43  ) = [-51.0, -0.5907]
  cl(0:1, 44  ) = [-48.0, -0.617]
  cl(0:1, 45  ) = [-45.0, -0.6404]
  cl(0:1, 46  ) = [-42.0, -0.6588]
  cl(0:1, 47  ) = [-39.0, -0.6763]
  cl(0:1, 48  ) = [-36.0, -0.6913]
  cl(0:1, 49  ) = [-33.0, -0.7054]
  cl(0:1, 50  ) = [-30.0, -0.7201]
  cl(0:1, 51  ) = [-29.3939, -0.7234]
  cl(0:1, 52  ) = [-28.7879, -0.7268]
  cl(0:1, 53  ) = [-28.1818, -0.7302]
  cl(0:1, 54  ) = [-27.5758, -0.734]
  cl(0:1, 55  ) = [-26.9697, -0.7379]
  cl(0:1, 56  ) = [-26.3636, -0.7419]
  cl(0:1, 57  ) = [-25.7576, -0.7462]
  cl(0:1, 58  ) = [-25.1515, -0.7511]
  cl(0:1, 59  ) = [-24.5455, -0.756]
  cl(0:1, 60  ) = [-23.9394, -0.7611]
  cl(0:1, 61  ) = [-23.3333, -0.768]
  cl(0:1, 62  ) = [-22.7273, -0.7748]
  cl(0:1, 63  ) = [-22.1212, -0.7817]
  cl(0:1, 64  ) = [-21.5152, -0.8124]
  cl(0:1, 65  ) = [-20.9091, -0.8464]
  cl(0:1, 66  ) = [-20.303, -0.8824]
  cl(0:1, 67  ) = [-19.697, -0.9173]
  cl(0:1, 68  ) = [-19.0909, -0.9517]
  cl(0:1, 69  ) = [-18.4848, -0.9875]
  cl(0:1, 70  ) = [-17.8788, -1.0235]
  cl(0:1, 71  ) = [-17.2727, -1.0596]
  cl(0:1, 72  ) = [-16.6667, -1.0966]
  cl(0:1, 73  ) = [-16.0606, -1.1344]
  cl(0:1, 74  ) = [-15.4545, -1.1539]
  cl(0:1, 75  ) = [-14.8485, -1.1618]
  cl(0:1, 76  ) = [-14.2424, -1.1405]
  cl(0:1, 77  ) = [-13.6364, -1.1132]
  cl(0:1, 78  ) = [-13.0303, -1.0808]
  cl(0:1, 79  ) = [-12.4242, -1.0373]
  cl(0:1, 80  ) = [-11.8182, -0.9914]
  cl(0:1, 81  ) = [-11.2121, -0.943]
  cl(0:1, 82  ) = [-10.6061, -0.8914]
  cl(0:1, 83  ) = [-10.0, -0.8386]
  cl(0:1, 84  ) = [-9.3939, -0.7972]
  cl(0:1, 85  ) = [-8.7879, -0.7577]
  cl(0:1, 86  ) = [-8.1818, -0.7296]
  cl(0:1, 87  ) = [-7.5758, -0.6778]
  cl(0:1, 88  ) = [-6.9697, -0.6153]
  cl(0:1, 89  ) = [-6.3636, -0.5292]
  cl(0:1, 90  ) = [-5.7576, -0.4437]
  cl(0:1, 91  ) = [-5.1515, -0.359]
  cl(0:1, 92  ) = [-4.5455, -0.2756]
  cl(0:1, 93  ) = [-3.9394, -0.1926]
  cl(0:1, 94  ) = [-3.3333, -0.1093]
  cl(0:1, 95  ) = [-2.7273, -0.0278]
  cl(0:1, 96  ) = [-2.1212, 0.0521]
  cl(0:1, 97  ) = [-1.5152, 0.1306]
  cl(0:1, 98  ) = [-0.9091, 0.2088]
  cl(0:1, 99  ) = [-0.303, 0.2876]
  cl(0:1, 100  ) = [0.303, 0.3657]
  cl(0:1, 101  ) = [0.9091, 0.4432]
  cl(0:1, 102  ) = [1.5152, 0.5203]
  cl(0:1, 103  ) = [2.1212, 0.5968]
  cl(0:1, 104  ) = [2.7273, 0.6714]
  cl(0:1, 105  ) = [3.3333, 0.7457]
  cl(0:1, 106  ) = [3.9394, 0.8199]
  cl(0:1, 107  ) = [4.5455, 0.8936]
  cl(0:1, 108  ) = [5.1515, 0.9668]
  cl(0:1, 109  ) = [5.7576, 1.0382]
  cl(0:1, 110  ) = [6.3636, 1.108]
  cl(0:1, 111  ) = [6.9697, 1.1769]
  cl(0:1, 112  ) = [7.5758, 1.2392]
  cl(0:1, 113  ) = [8.1818, 1.2971]
  cl(0:1, 114  ) = [8.7879, 1.3455]
  cl(0:1, 115  ) = [9.3939, 1.3761]
  cl(0:1, 116  ) = [10.0, 1.3984]
  cl(0:1, 117  ) = [10.6061, 1.3929]
  cl(0:1, 118  ) = [11.2121, 1.39]
  cl(0:1, 119  ) = [11.8182, 1.3922]
  cl(0:1, 120  ) = [12.4242, 1.2937]
  cl(0:1, 121  ) = [13.0303, 1.1438]
  cl(0:1, 122  ) = [13.6364, 1.1178]
  cl(0:1, 123  ) = [14.2424, 1.0929]
  cl(0:1, 124  ) = [14.8485, 1.071]
  cl(0:1, 125  ) = [15.4545, 1.057]
  cl(0:1, 126  ) = [16.0606, 1.046]
  cl(0:1, 127  ) = [16.6667, 1.0393]
  cl(0:1, 128  ) = [17.2727, 1.0375]
  cl(0:1, 129  ) = [17.8788, 1.0421]
  cl(0:1, 130  ) = [18.4848, 1.0549]
  cl(0:1, 131  ) = [19.0909, 1.0695]
  cl(0:1, 132  ) = [19.697, 1.0828]
  cl(0:1, 133  ) = [20.303, 1.094]
  cl(0:1, 134  ) = [20.9091, 1.1024]
  cl(0:1, 135  ) = [21.5152, 1.1089]
  cl(0:1, 136  ) = [22.1212, 1.1124]
  cl(0:1, 137  ) = [22.7273, 1.104]
  cl(0:1, 138  ) = [23.3333, 1.0956]
  cl(0:1, 139  ) = [23.9394, 1.0872]
  cl(0:1, 140  ) = [24.5455, 1.08]
  cl(0:1, 141  ) = [25.1515, 1.073]
  cl(0:1, 142  ) = [25.7576, 1.0661]
  cl(0:1, 143  ) = [26.3636, 1.0599]
  cl(0:1, 144  ) = [26.9697, 1.0542]
  cl(0:1, 145  ) = [27.5758, 1.0485]
  cl(0:1, 146  ) = [28.1818, 1.0431]
  cl(0:1, 147  ) = [28.7879, 1.0383]
  cl(0:1, 148  ) = [29.3939, 1.0335]
  cl(0:1, 149  ) = [30.0, 1.0287]
  cl(0:1, 150  ) = [33.0, 1.0077]
  cl(0:1, 151  ) = [36.0, 0.9875]
  cl(0:1, 152  ) = [39.0, 0.9661]
  cl(0:1, 153  ) = [42.0, 0.9411]
  cl(0:1, 154  ) = [45.0, 0.9148]
  cl(0:1, 155  ) = [48.0, 0.8814]
  cl(0:1, 156  ) = [51.0, 0.8438]
  cl(0:1, 157  ) = [54.0, 0.7979]
  cl(0:1, 158  ) = [57.0, 0.752]
  cl(0:1, 159  ) = [60.0, 0.7061]
  cl(0:1, 160  ) = [63.0, 0.6443]
  cl(0:1, 161  ) = [66.0, 0.5825]
  cl(0:1, 162  ) = [69.0, 0.5207]
  cl(0:1, 163  ) = [72.0, 0.4515]
  cl(0:1, 164  ) = [75.0, 0.3784]
  cl(0:1, 165  ) = [78.0, 0.3053]
  cl(0:1, 166  ) = [81.0, 0.231]
  cl(0:1, 167  ) = [84.0, 0.154]
  cl(0:1, 168  ) = [87.0, 0.077]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.0539]
  cl(0:1, 171  ) = [96.0, -0.1078]
  cl(0:1, 172  ) = [99.0, -0.1617]
  cl(0:1, 173  ) = [102.0, -0.2137]
  cl(0:1, 174  ) = [105.0, -0.2649]
  cl(0:1, 175  ) = [108.0, -0.316]
  cl(0:1, 176  ) = [111.0, -0.3645]
  cl(0:1, 177  ) = [114.0, -0.4078]
  cl(0:1, 178  ) = [117.0, -0.451]
  cl(0:1, 179  ) = [120.0, -0.4942]
  cl(0:1, 180  ) = [123.0, -0.5264]
  cl(0:1, 181  ) = [126.0, -0.5585]
  cl(0:1, 182  ) = [129.0, -0.5907]
  cl(0:1, 183  ) = [132.0, -0.6153]
  cl(0:1, 184  ) = [135.0, -0.6362]
  cl(0:1, 185  ) = [138.0, -0.6571]
  cl(0:1, 186  ) = [141.0, -0.6759]
  cl(0:1, 187  ) = [144.0, -0.6906]
  cl(0:1, 188  ) = [147.0, -0.7053]
  cl(0:1, 189  ) = [150.0, -0.7201]
  cl(0:1, 190  ) = [153.0, -0.7102]
  cl(0:1, 191  ) = [156.0, -0.7033]
  cl(0:1, 192  ) = [159.0, -0.6961]
  cl(0:1, 193  ) = [162.0, -0.6234]
  cl(0:1, 194  ) = [165.0, -0.5195]
  cl(0:1, 195  ) = [168.0, -0.4156]
  cl(0:1, 196  ) = [171.0, -0.3117]
  cl(0:1, 197  ) = [174.0, -0.2078]
  cl(0:1, 198  ) = [177.0, -0.1034]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl18 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl18 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl18 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl18_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl18 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl18_nrel2p8.pro' 
  ENDIF

!print,y,cl18

END FUNCTION calc_cl18_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cl19_nrel2p8( y ) RESULT( cl19 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl19
INTEGER :: i

 cl19 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.1085]
  cl(0:1, 2  ) = [-174.0, 0.218]
  cl(0:1, 3  ) = [-171.0, 0.327]
  cl(0:1, 4  ) = [-168.0, 0.436]
  cl(0:1, 5  ) = [-165.0, 0.545]
  cl(0:1, 6  ) = [-162.0, 0.654]
  cl(0:1, 7  ) = [-159.0, 0.7273]
  cl(0:1, 8  ) = [-156.0, 0.7265]
  cl(0:1, 9  ) = [-153.0, 0.7264]
  cl(0:1, 10  ) = [-150.0, 0.7294]
  cl(0:1, 11  ) = [-147.0, 0.7136]
  cl(0:1, 12  ) = [-144.0, 0.6978]
  cl(0:1, 13  ) = [-141.0, 0.682]
  cl(0:1, 14  ) = [-138.0, 0.6623]
  cl(0:1, 15  ) = [-135.0, 0.6407]
  cl(0:1, 16  ) = [-132.0, 0.6191]
  cl(0:1, 17  ) = [-129.0, 0.5939]
  cl(0:1, 18  ) = [-126.0, 0.5613]
  cl(0:1, 19  ) = [-123.0, 0.5287]
  cl(0:1, 20  ) = [-120.0, 0.496]
  cl(0:1, 21  ) = [-117.0, 0.4525]
  cl(0:1, 22  ) = [-114.0, 0.409]
  cl(0:1, 23  ) = [-111.0, 0.3654]
  cl(0:1, 24  ) = [-108.0, 0.3167]
  cl(0:1, 25  ) = [-105.0, 0.2654]
  cl(0:1, 26  ) = [-102.0, 0.214]
  cl(0:1, 27  ) = [-99.0, 0.1619]
  cl(0:1, 28  ) = [-96.0, 0.1079]
  cl(0:1, 29  ) = [-93.0, 0.054]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.054]
  cl(0:1, 32  ) = [-84.0, -0.1079]
  cl(0:1, 33  ) = [-81.0, -0.1619]
  cl(0:1, 34  ) = [-78.0, -0.214]
  cl(0:1, 35  ) = [-75.0, -0.2654]
  cl(0:1, 36  ) = [-72.0, -0.3167]
  cl(0:1, 37  ) = [-69.0, -0.3654]
  cl(0:1, 38  ) = [-66.0, -0.409]
  cl(0:1, 39  ) = [-63.0, -0.4525]
  cl(0:1, 40  ) = [-60.0, -0.496]
  cl(0:1, 41  ) = [-57.0, -0.5287]
  cl(0:1, 42  ) = [-54.0, -0.5613]
  cl(0:1, 43  ) = [-51.0, -0.5939]
  cl(0:1, 44  ) = [-48.0, -0.6208]
  cl(0:1, 45  ) = [-45.0, -0.6448]
  cl(0:1, 46  ) = [-42.0, -0.664]
  cl(0:1, 47  ) = [-39.0, -0.6822]
  cl(0:1, 48  ) = [-36.0, -0.6982]
  cl(0:1, 49  ) = [-33.0, -0.7134]
  cl(0:1, 50  ) = [-30.0, -0.7294]
  cl(0:1, 51  ) = [-29.3939, -0.7331]
  cl(0:1, 52  ) = [-28.7879, -0.7368]
  cl(0:1, 53  ) = [-28.1818, -0.7404]
  cl(0:1, 54  ) = [-27.5758, -0.7446]
  cl(0:1, 55  ) = [-26.9697, -0.7489]
  cl(0:1, 56  ) = [-26.3636, -0.7532]
  cl(0:1, 57  ) = [-25.7576, -0.7579]
  cl(0:1, 58  ) = [-25.1515, -0.7632]
  cl(0:1, 59  ) = [-24.5455, -0.7685]
  cl(0:1, 60  ) = [-23.9394, -0.774]
  cl(0:1, 61  ) = [-23.3333, -0.7819]
  cl(0:1, 62  ) = [-22.7273, -0.7898]
  cl(0:1, 63  ) = [-22.1212, -0.7978]
  cl(0:1, 64  ) = [-21.5152, -0.8172]
  cl(0:1, 65  ) = [-20.9091, -0.8407]
  cl(0:1, 66  ) = [-20.303, -0.8649]
  cl(0:1, 67  ) = [-19.697, -0.8874]
  cl(0:1, 68  ) = [-19.0909, -0.9092]
  cl(0:1, 69  ) = [-18.4848, -0.9331]
  cl(0:1, 70  ) = [-17.8788, -0.9573]
  cl(0:1, 71  ) = [-17.2727, -0.9816]
  cl(0:1, 72  ) = [-16.6667, -1.0074]
  cl(0:1, 73  ) = [-16.0606, -1.0342]
  cl(0:1, 74  ) = [-15.4545, -1.0521]
  cl(0:1, 75  ) = [-14.8485, -1.0641]
  cl(0:1, 76  ) = [-14.2424, -1.0609]
  cl(0:1, 77  ) = [-13.6364, -1.053]
  cl(0:1, 78  ) = [-13.0303, -1.0404]
  cl(0:1, 79  ) = [-12.4242, -1.0096]
  cl(0:1, 80  ) = [-11.8182, -0.9744]
  cl(0:1, 81  ) = [-11.2121, -0.9332]
  cl(0:1, 82  ) = [-10.6061, -0.8871]
  cl(0:1, 83  ) = [-10.0, -0.839]
  cl(0:1, 84  ) = [-9.3939, -0.7984]
  cl(0:1, 85  ) = [-8.7879, -0.7551]
  cl(0:1, 86  ) = [-8.1818, -0.7128]
  cl(0:1, 87  ) = [-7.5758, -0.6502]
  cl(0:1, 88  ) = [-6.9697, -0.5793]
  cl(0:1, 89  ) = [-6.3636, -0.4933]
  cl(0:1, 90  ) = [-5.7576, -0.4076]
  cl(0:1, 91  ) = [-5.1515, -0.3224]
  cl(0:1, 92  ) = [-4.5455, -0.2386]
  cl(0:1, 93  ) = [-3.9394, -0.1553]
  cl(0:1, 94  ) = [-3.3333, -0.0728]
  cl(0:1, 95  ) = [-2.7273, 0.0088]
  cl(0:1, 96  ) = [-2.1212, 0.0891]
  cl(0:1, 97  ) = [-1.5152, 0.168]
  cl(0:1, 98  ) = [-0.9091, 0.2463]
  cl(0:1, 99  ) = [-0.303, 0.3235]
  cl(0:1, 100  ) = [0.303, 0.4006]
  cl(0:1, 101  ) = [0.9091, 0.4775]
  cl(0:1, 102  ) = [1.5152, 0.5543]
  cl(0:1, 103  ) = [2.1212, 0.6307]
  cl(0:1, 104  ) = [2.7273, 0.706]
  cl(0:1, 105  ) = [3.3333, 0.7804]
  cl(0:1, 106  ) = [3.9394, 0.8546]
  cl(0:1, 107  ) = [4.5455, 0.9284]
  cl(0:1, 108  ) = [5.1515, 1.0015]
  cl(0:1, 109  ) = [5.7576, 1.0727]
  cl(0:1, 110  ) = [6.3636, 1.1413]
  cl(0:1, 111  ) = [6.9697, 1.2086]
  cl(0:1, 112  ) = [7.5758, 1.2644]
  cl(0:1, 113  ) = [8.1818, 1.3133]
  cl(0:1, 114  ) = [8.7879, 1.3493]
  cl(0:1, 115  ) = [9.3939, 1.3569]
  cl(0:1, 116  ) = [10.0, 1.3462]
  cl(0:1, 117  ) = [10.6061, 1.2925]
  cl(0:1, 118  ) = [11.2121, 1.2521]
  cl(0:1, 119  ) = [11.8182, 1.2367]
  cl(0:1, 120  ) = [12.4242, 1.1861]
  cl(0:1, 121  ) = [13.0303, 1.1159]
  cl(0:1, 122  ) = [13.6364, 1.1107]
  cl(0:1, 123  ) = [14.2424, 1.1002]
  cl(0:1, 124  ) = [14.8485, 1.0893]
  cl(0:1, 125  ) = [15.4545, 1.0827]
  cl(0:1, 126  ) = [16.0606, 1.0775]
  cl(0:1, 127  ) = [16.6667, 1.0725]
  cl(0:1, 128  ) = [17.2727, 1.0722]
  cl(0:1, 129  ) = [17.8788, 1.0786]
  cl(0:1, 130  ) = [18.4848, 1.0886]
  cl(0:1, 131  ) = [19.0909, 1.0992]
  cl(0:1, 132  ) = [19.697, 1.1091]
  cl(0:1, 133  ) = [20.303, 1.1183]
  cl(0:1, 134  ) = [20.9091, 1.1252]
  cl(0:1, 135  ) = [21.5152, 1.1295]
  cl(0:1, 136  ) = [22.1212, 1.1311]
  cl(0:1, 137  ) = [22.7273, 1.1226]
  cl(0:1, 138  ) = [23.3333, 1.114]
  cl(0:1, 139  ) = [23.9394, 1.1055]
  cl(0:1, 140  ) = [24.5455, 1.0978]
  cl(0:1, 141  ) = [25.1515, 1.0903]
  cl(0:1, 142  ) = [25.7576, 1.0827]
  cl(0:1, 143  ) = [26.3636, 1.076]
  cl(0:1, 144  ) = [26.9697, 1.0698]
  cl(0:1, 145  ) = [27.5758, 1.0637]
  cl(0:1, 146  ) = [28.1818, 1.0578]
  cl(0:1, 147  ) = [28.7879, 1.0525]
  cl(0:1, 148  ) = [29.3939, 1.0473]
  cl(0:1, 149  ) = [30.0, 1.042]
  cl(0:1, 150  ) = [33.0, 1.0192]
  cl(0:1, 151  ) = [36.0, 0.9974]
  cl(0:1, 152  ) = [39.0, 0.9746]
  cl(0:1, 153  ) = [42.0, 0.9485]
  cl(0:1, 154  ) = [45.0, 0.9211]
  cl(0:1, 155  ) = [48.0, 0.8868]
  cl(0:1, 156  ) = [51.0, 0.8484]
  cl(0:1, 157  ) = [54.0, 0.8018]
  cl(0:1, 158  ) = [57.0, 0.7552]
  cl(0:1, 159  ) = [60.0, 0.7086]
  cl(0:1, 160  ) = [63.0, 0.6464]
  cl(0:1, 161  ) = [66.0, 0.5842]
  cl(0:1, 162  ) = [69.0, 0.522]
  cl(0:1, 163  ) = [72.0, 0.4524]
  cl(0:1, 164  ) = [75.0, 0.3791]
  cl(0:1, 165  ) = [78.0, 0.3058]
  cl(0:1, 166  ) = [81.0, 0.2312]
  cl(0:1, 167  ) = [84.0, 0.1541]
  cl(0:1, 168  ) = [87.0, 0.0771]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.054]
  cl(0:1, 171  ) = [96.0, -0.1079]
  cl(0:1, 172  ) = [99.0, -0.1619]
  cl(0:1, 173  ) = [102.0, -0.214]
  cl(0:1, 174  ) = [105.0, -0.2654]
  cl(0:1, 175  ) = [108.0, -0.3167]
  cl(0:1, 176  ) = [111.0, -0.3654]
  cl(0:1, 177  ) = [114.0, -0.409]
  cl(0:1, 178  ) = [117.0, -0.4525]
  cl(0:1, 179  ) = [120.0, -0.496]
  cl(0:1, 180  ) = [123.0, -0.5287]
  cl(0:1, 181  ) = [126.0, -0.5613]
  cl(0:1, 182  ) = [129.0, -0.5939]
  cl(0:1, 183  ) = [132.0, -0.6191]
  cl(0:1, 184  ) = [135.0, -0.6407]
  cl(0:1, 185  ) = [138.0, -0.6623]
  cl(0:1, 186  ) = [141.0, -0.682]
  cl(0:1, 187  ) = [144.0, -0.6978]
  cl(0:1, 188  ) = [147.0, -0.7136]
  cl(0:1, 189  ) = [150.0, -0.7294]
  cl(0:1, 190  ) = [153.0, -0.715]
  cl(0:1, 191  ) = [156.0, -0.7051]
  cl(0:1, 192  ) = [159.0, -0.6954]
  cl(0:1, 193  ) = [162.0, -0.6221]
  cl(0:1, 194  ) = [165.0, -0.5184]
  cl(0:1, 195  ) = [168.0, -0.4147]
  cl(0:1, 196  ) = [171.0, -0.311]
  cl(0:1, 197  ) = [174.0, -0.2074]
  cl(0:1, 198  ) = [177.0, -0.1032]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl19 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl19 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl19 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl19_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl19 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl19_nrel2p8.pro' 
  ENDIF

!print,y,cl19

END FUNCTION calc_cl19_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cl20_nrel2p8( y ) RESULT( cl20 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl20
INTEGER :: i

 cl20 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.1084]
  cl(0:1, 2  ) = [-174.0, 0.2178]
  cl(0:1, 3  ) = [-171.0, 0.3267]
  cl(0:1, 4  ) = [-168.0, 0.4356]
  cl(0:1, 5  ) = [-165.0, 0.5445]
  cl(0:1, 6  ) = [-162.0, 0.6534]
  cl(0:1, 7  ) = [-159.0, 0.727]
  cl(0:1, 8  ) = [-156.0, 0.7287]
  cl(0:1, 9  ) = [-153.0, 0.7311]
  cl(0:1, 10  ) = [-150.0, 0.7355]
  cl(0:1, 11  ) = [-147.0, 0.719]
  cl(0:1, 12  ) = [-144.0, 0.7025]
  cl(0:1, 13  ) = [-141.0, 0.6859]
  cl(0:1, 14  ) = [-138.0, 0.6657]
  cl(0:1, 15  ) = [-135.0, 0.6437]
  cl(0:1, 16  ) = [-132.0, 0.6216]
  cl(0:1, 17  ) = [-129.0, 0.596]
  cl(0:1, 18  ) = [-126.0, 0.5631]
  cl(0:1, 19  ) = [-123.0, 0.5301]
  cl(0:1, 20  ) = [-120.0, 0.4972]
  cl(0:1, 21  ) = [-117.0, 0.4535]
  cl(0:1, 22  ) = [-114.0, 0.4097]
  cl(0:1, 23  ) = [-111.0, 0.366]
  cl(0:1, 24  ) = [-108.0, 0.3171]
  cl(0:1, 25  ) = [-105.0, 0.2657]
  cl(0:1, 26  ) = [-102.0, 0.2143]
  cl(0:1, 27  ) = [-99.0, 0.162]
  cl(0:1, 28  ) = [-96.0, 0.108]
  cl(0:1, 29  ) = [-93.0, 0.054]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.054]
  cl(0:1, 32  ) = [-84.0, -0.108]
  cl(0:1, 33  ) = [-81.0, -0.162]
  cl(0:1, 34  ) = [-78.0, -0.2143]
  cl(0:1, 35  ) = [-75.0, -0.2657]
  cl(0:1, 36  ) = [-72.0, -0.3171]
  cl(0:1, 37  ) = [-69.0, -0.366]
  cl(0:1, 38  ) = [-66.0, -0.4097]
  cl(0:1, 39  ) = [-63.0, -0.4535]
  cl(0:1, 40  ) = [-60.0, -0.4972]
  cl(0:1, 41  ) = [-57.0, -0.5301]
  cl(0:1, 42  ) = [-54.0, -0.5631]
  cl(0:1, 43  ) = [-51.0, -0.596]
  cl(0:1, 44  ) = [-48.0, -0.6232]
  cl(0:1, 45  ) = [-45.0, -0.6477]
  cl(0:1, 46  ) = [-42.0, -0.6673]
  cl(0:1, 47  ) = [-39.0, -0.6862]
  cl(0:1, 48  ) = [-36.0, -0.7027]
  cl(0:1, 49  ) = [-33.0, -0.7187]
  cl(0:1, 50  ) = [-30.0, -0.7355]
  cl(0:1, 51  ) = [-29.3939, -0.7394]
  cl(0:1, 52  ) = [-28.7879, -0.7433]
  cl(0:1, 53  ) = [-28.1818, -0.7471]
  cl(0:1, 54  ) = [-27.5758, -0.7515]
  cl(0:1, 55  ) = [-26.9697, -0.756]
  cl(0:1, 56  ) = [-26.3636, -0.7606]
  cl(0:1, 57  ) = [-25.7576, -0.7655]
  cl(0:1, 58  ) = [-25.1515, -0.771]
  cl(0:1, 59  ) = [-24.5455, -0.7766]
  cl(0:1, 60  ) = [-23.9394, -0.7824]
  cl(0:1, 61  ) = [-23.3333, -0.791]
  cl(0:1, 62  ) = [-22.7273, -0.7996]
  cl(0:1, 63  ) = [-22.1212, -0.8082]
  cl(0:1, 64  ) = [-21.5152, -0.82]
  cl(0:1, 65  ) = [-20.9091, -0.8346]
  cl(0:1, 66  ) = [-20.303, -0.8491]
  cl(0:1, 67  ) = [-19.697, -0.8626]
  cl(0:1, 68  ) = [-19.0909, -0.8755]
  cl(0:1, 69  ) = [-18.4848, -0.8898]
  cl(0:1, 70  ) = [-17.8788, -0.9043]
  cl(0:1, 71  ) = [-17.2727, -0.9189]
  cl(0:1, 72  ) = [-16.6667, -0.9344]
  cl(0:1, 73  ) = [-16.0606, -0.9505]
  cl(0:1, 74  ) = [-15.4545, -0.9647]
  cl(0:1, 75  ) = [-14.8485, -0.9774]
  cl(0:1, 76  ) = [-14.2424, -0.986]
  cl(0:1, 77  ) = [-13.6364, -0.9925]
  cl(0:1, 78  ) = [-13.0303, -0.9966]
  cl(0:1, 79  ) = [-12.4242, -0.9781]
  cl(0:1, 80  ) = [-11.8182, -0.9539]
  cl(0:1, 81  ) = [-11.2121, -0.9209]
  cl(0:1, 82  ) = [-10.6061, -0.8816]
  cl(0:1, 83  ) = [-10.0, -0.8392]
  cl(0:1, 84  ) = [-9.3939, -0.7989]
  cl(0:1, 85  ) = [-8.7879, -0.7517]
  cl(0:1, 86  ) = [-8.1818, -0.6937]
  cl(0:1, 87  ) = [-7.5758, -0.6212]
  cl(0:1, 88  ) = [-6.9697, -0.5428]
  cl(0:1, 89  ) = [-6.3636, -0.4572]
  cl(0:1, 90  ) = [-5.7576, -0.3718]
  cl(0:1, 91  ) = [-5.1515, -0.2865]
  cl(0:1, 92  ) = [-4.5455, -0.2028]
  cl(0:1, 93  ) = [-3.9394, -0.1198]
  cl(0:1, 94  ) = [-3.3333, -0.0385]
  cl(0:1, 95  ) = [-2.7273, 0.0425]
  cl(0:1, 96  ) = [-2.1212, 0.1227]
  cl(0:1, 97  ) = [-1.5152, 0.2015]
  cl(0:1, 98  ) = [-0.9091, 0.2794]
  cl(0:1, 99  ) = [-0.303, 0.3553]
  cl(0:1, 100  ) = [0.303, 0.4313]
  cl(0:1, 101  ) = [0.9091, 0.5076]
  cl(0:1, 102  ) = [1.5152, 0.5839]
  cl(0:1, 103  ) = [2.1212, 0.6601]
  cl(0:1, 104  ) = [2.7273, 0.7355]
  cl(0:1, 105  ) = [3.3333, 0.81]
  cl(0:1, 106  ) = [3.9394, 0.884]
  cl(0:1, 107  ) = [4.5455, 0.9575]
  cl(0:1, 108  ) = [5.1515, 1.0304]
  cl(0:1, 109  ) = [5.7576, 1.1013]
  cl(0:1, 110  ) = [6.3636, 1.1689]
  cl(0:1, 111  ) = [6.9697, 1.2347]
  cl(0:1, 112  ) = [7.5758, 1.2858]
  cl(0:1, 113  ) = [8.1818, 1.3282]
  cl(0:1, 114  ) = [8.7879, 1.3541]
  cl(0:1, 115  ) = [9.3939, 1.3443]
  cl(0:1, 116  ) = [10.0, 1.3122]
  cl(0:1, 117  ) = [10.6061, 1.227]
  cl(0:1, 118  ) = [11.2121, 1.1622]
  cl(0:1, 119  ) = [11.8182, 1.1353]
  cl(0:1, 120  ) = [12.4242, 1.116]
  cl(0:1, 121  ) = [13.0303, 1.0998]
  cl(0:1, 122  ) = [13.6364, 1.1071]
  cl(0:1, 123  ) = [14.2424, 1.1094]
  cl(0:1, 124  ) = [14.8485, 1.1106]
  cl(0:1, 125  ) = [15.4545, 1.1108]
  cl(0:1, 126  ) = [16.0606, 1.1103]
  cl(0:1, 127  ) = [16.6667, 1.1061]
  cl(0:1, 128  ) = [17.2727, 1.1061]
  cl(0:1, 129  ) = [17.8788, 1.1117]
  cl(0:1, 130  ) = [18.4848, 1.118]
  cl(0:1, 131  ) = [19.0909, 1.1243]
  cl(0:1, 132  ) = [19.697, 1.1301]
  cl(0:1, 133  ) = [20.303, 1.1357]
  cl(0:1, 134  ) = [20.9091, 1.1402]
  cl(0:1, 135  ) = [21.5152, 1.1429]
  cl(0:1, 136  ) = [22.1212, 1.1433]
  cl(0:1, 137  ) = [22.7273, 1.1347]
  cl(0:1, 138  ) = [23.3333, 1.1261]
  cl(0:1, 139  ) = [23.9394, 1.1174]
  cl(0:1, 140  ) = [24.5455, 1.1094]
  cl(0:1, 141  ) = [25.1515, 1.1015]
  cl(0:1, 142  ) = [25.7576, 1.0936]
  cl(0:1, 143  ) = [26.3636, 1.0865]
  cl(0:1, 144  ) = [26.9697, 1.08]
  cl(0:1, 145  ) = [27.5758, 1.0735]
  cl(0:1, 146  ) = [28.1818, 1.0673]
  cl(0:1, 147  ) = [28.7879, 1.0618]
  cl(0:1, 148  ) = [29.3939, 1.0563]
  cl(0:1, 149  ) = [30.0, 1.0507]
  cl(0:1, 150  ) = [33.0, 1.0267]
  cl(0:1, 151  ) = [36.0, 1.0039]
  cl(0:1, 152  ) = [39.0, 0.9802]
  cl(0:1, 153  ) = [42.0, 0.9533]
  cl(0:1, 154  ) = [45.0, 0.9252]
  cl(0:1, 155  ) = [48.0, 0.8903]
  cl(0:1, 156  ) = [51.0, 0.8514]
  cl(0:1, 157  ) = [54.0, 0.8044]
  cl(0:1, 158  ) = [57.0, 0.7573]
  cl(0:1, 159  ) = [60.0, 0.7103]
  cl(0:1, 160  ) = [63.0, 0.6478]
  cl(0:1, 161  ) = [66.0, 0.5853]
  cl(0:1, 162  ) = [69.0, 0.5228]
  cl(0:1, 163  ) = [72.0, 0.453]
  cl(0:1, 164  ) = [75.0, 0.3795]
  cl(0:1, 165  ) = [78.0, 0.3061]
  cl(0:1, 166  ) = [81.0, 0.2314]
  cl(0:1, 167  ) = [84.0, 0.1543]
  cl(0:1, 168  ) = [87.0, 0.0771]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.054]
  cl(0:1, 171  ) = [96.0, -0.108]
  cl(0:1, 172  ) = [99.0, -0.162]
  cl(0:1, 173  ) = [102.0, -0.2143]
  cl(0:1, 174  ) = [105.0, -0.2657]
  cl(0:1, 175  ) = [108.0, -0.3171]
  cl(0:1, 176  ) = [111.0, -0.366]
  cl(0:1, 177  ) = [114.0, -0.4097]
  cl(0:1, 178  ) = [117.0, -0.4535]
  cl(0:1, 179  ) = [120.0, -0.4972]
  cl(0:1, 180  ) = [123.0, -0.5301]
  cl(0:1, 181  ) = [126.0, -0.5631]
  cl(0:1, 182  ) = [129.0, -0.596]
  cl(0:1, 183  ) = [132.0, -0.6216]
  cl(0:1, 184  ) = [135.0, -0.6437]
  cl(0:1, 185  ) = [138.0, -0.6657]
  cl(0:1, 186  ) = [141.0, -0.6859]
  cl(0:1, 187  ) = [144.0, -0.7025]
  cl(0:1, 188  ) = [147.0, -0.719]
  cl(0:1, 189  ) = [150.0, -0.7355]
  cl(0:1, 190  ) = [153.0, -0.72]
  cl(0:1, 191  ) = [156.0, -0.7074]
  cl(0:1, 192  ) = [159.0, -0.6951]
  cl(0:1, 193  ) = [162.0, -0.6215]
  cl(0:1, 194  ) = [165.0, -0.5179]
  cl(0:1, 195  ) = [168.0, -0.4143]
  cl(0:1, 196  ) = [171.0, -0.3107]
  cl(0:1, 197  ) = [174.0, -0.2072]
  cl(0:1, 198  ) = [177.0, -0.1031]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl20 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl20 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl20 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl20_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl20 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl20_nrel2p8.pro' 
  ENDIF

!print,y,cl20

END FUNCTION calc_cl20_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cl21_nrel2p8( y ) RESULT( cl21 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl21
INTEGER :: i

 cl21 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.1087]
  cl(0:1, 2  ) = [-174.0, 0.2184]
  cl(0:1, 3  ) = [-171.0, 0.3276]
  cl(0:1, 4  ) = [-168.0, 0.4365]
  cl(0:1, 5  ) = [-165.0, 0.5452]
  cl(0:1, 6  ) = [-162.0, 0.654]
  cl(0:1, 7  ) = [-159.0, 0.7275]
  cl(0:1, 8  ) = [-156.0, 0.7311]
  cl(0:1, 9  ) = [-153.0, 0.7343]
  cl(0:1, 10  ) = [-150.0, 0.7365]
  cl(0:1, 11  ) = [-147.0, 0.7199]
  cl(0:1, 12  ) = [-144.0, 0.7032]
  cl(0:1, 13  ) = [-141.0, 0.6866]
  cl(0:1, 14  ) = [-138.0, 0.6663]
  cl(0:1, 15  ) = [-135.0, 0.6442]
  cl(0:1, 16  ) = [-132.0, 0.6221]
  cl(0:1, 17  ) = [-129.0, 0.5963]
  cl(0:1, 18  ) = [-126.0, 0.5634]
  cl(0:1, 19  ) = [-123.0, 0.5304]
  cl(0:1, 20  ) = [-120.0, 0.4974]
  cl(0:1, 21  ) = [-117.0, 0.4536]
  cl(0:1, 22  ) = [-114.0, 0.4099]
  cl(0:1, 23  ) = [-111.0, 0.3661]
  cl(0:1, 24  ) = [-108.0, 0.3172]
  cl(0:1, 25  ) = [-105.0, 0.2657]
  cl(0:1, 26  ) = [-102.0, 0.2143]
  cl(0:1, 27  ) = [-99.0, 0.162]
  cl(0:1, 28  ) = [-96.0, 0.108]
  cl(0:1, 29  ) = [-93.0, 0.054]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.054]
  cl(0:1, 32  ) = [-84.0, -0.108]
  cl(0:1, 33  ) = [-81.0, -0.162]
  cl(0:1, 34  ) = [-78.0, -0.2143]
  cl(0:1, 35  ) = [-75.0, -0.2657]
  cl(0:1, 36  ) = [-72.0, -0.3172]
  cl(0:1, 37  ) = [-69.0, -0.3661]
  cl(0:1, 38  ) = [-66.0, -0.4099]
  cl(0:1, 39  ) = [-63.0, -0.4536]
  cl(0:1, 40  ) = [-60.0, -0.4974]
  cl(0:1, 41  ) = [-57.0, -0.5304]
  cl(0:1, 42  ) = [-54.0, -0.5634]
  cl(0:1, 43  ) = [-51.0, -0.5963]
  cl(0:1, 44  ) = [-48.0, -0.6236]
  cl(0:1, 45  ) = [-45.0, -0.6481]
  cl(0:1, 46  ) = [-42.0, -0.6679]
  cl(0:1, 47  ) = [-39.0, -0.6868]
  cl(0:1, 48  ) = [-36.0, -0.7035]
  cl(0:1, 49  ) = [-33.0, -0.7196]
  cl(0:1, 50  ) = [-30.0, -0.7365]
  cl(0:1, 51  ) = [-29.3939, -0.7404]
  cl(0:1, 52  ) = [-28.7879, -0.7443]
  cl(0:1, 53  ) = [-28.1818, -0.7482]
  cl(0:1, 54  ) = [-27.5758, -0.7526]
  cl(0:1, 55  ) = [-26.9697, -0.7572]
  cl(0:1, 56  ) = [-26.3636, -0.7618]
  cl(0:1, 57  ) = [-25.7576, -0.7667]
  cl(0:1, 58  ) = [-25.1515, -0.7723]
  cl(0:1, 59  ) = [-24.5455, -0.7779]
  cl(0:1, 60  ) = [-23.9394, -0.7838]
  cl(0:1, 61  ) = [-23.3333, -0.7925]
  cl(0:1, 62  ) = [-22.7273, -0.8012]
  cl(0:1, 63  ) = [-22.1212, -0.8099]
  cl(0:1, 64  ) = [-21.5152, -0.8202]
  cl(0:1, 65  ) = [-20.9091, -0.8296]
  cl(0:1, 66  ) = [-20.303, -0.8392]
  cl(0:1, 67  ) = [-19.697, -0.8493]
  cl(0:1, 68  ) = [-19.0909, -0.8597]
  cl(0:1, 69  ) = [-18.4848, -0.8694]
  cl(0:1, 70  ) = [-17.8788, -0.8788]
  cl(0:1, 71  ) = [-17.2727, -0.8883]
  cl(0:1, 72  ) = [-16.6667, -0.8972]
  cl(0:1, 73  ) = [-16.0606, -0.9058]
  cl(0:1, 74  ) = [-15.4545, -0.9148]
  cl(0:1, 75  ) = [-14.8485, -0.9242]
  cl(0:1, 76  ) = [-14.2424, -0.9349]
  cl(0:1, 77  ) = [-13.6364, -0.9466]
  cl(0:1, 78  ) = [-13.0303, -0.9594]
  cl(0:1, 79  ) = [-12.4242, -0.9491]
  cl(0:1, 80  ) = [-11.8182, -0.9335]
  cl(0:1, 81  ) = [-11.2121, -0.9074]
  cl(0:1, 82  ) = [-10.6061, -0.8744]
  cl(0:1, 83  ) = [-10.0, -0.8374]
  cl(0:1, 84  ) = [-9.3939, -0.7974]
  cl(0:1, 85  ) = [-8.7879, -0.7469]
  cl(0:1, 86  ) = [-8.1818, -0.6764]
  cl(0:1, 87  ) = [-7.5758, -0.5975]
  cl(0:1, 88  ) = [-6.9697, -0.5147]
  cl(0:1, 89  ) = [-6.3636, -0.4298]
  cl(0:1, 90  ) = [-5.7576, -0.345]
  cl(0:1, 91  ) = [-5.1515, -0.2603]
  cl(0:1, 92  ) = [-4.5455, -0.1773]
  cl(0:1, 93  ) = [-3.9394, -0.095]
  cl(0:1, 94  ) = [-3.3333, -0.0145]
  cl(0:1, 95  ) = [-2.7273, 0.0657]
  cl(0:1, 96  ) = [-2.1212, 0.1453]
  cl(0:1, 97  ) = [-1.5152, 0.2237]
  cl(0:1, 98  ) = [-0.9091, 0.301]
  cl(0:1, 99  ) = [-0.303, 0.3761]
  cl(0:1, 100  ) = [0.303, 0.4514]
  cl(0:1, 101  ) = [0.9091, 0.5271]
  cl(0:1, 102  ) = [1.5152, 0.6028]
  cl(0:1, 103  ) = [2.1212, 0.6783]
  cl(0:1, 104  ) = [2.7273, 0.7534]
  cl(0:1, 105  ) = [3.3333, 0.8274]
  cl(0:1, 106  ) = [3.9394, 0.9011]
  cl(0:1, 107  ) = [4.5455, 0.9741]
  cl(0:1, 108  ) = [5.1515, 1.0465]
  cl(0:1, 109  ) = [5.7576, 1.1171]
  cl(0:1, 110  ) = [6.3636, 1.1843]
  cl(0:1, 111  ) = [6.9697, 1.249]
  cl(0:1, 112  ) = [7.5758, 1.2988]
  cl(0:1, 113  ) = [8.1818, 1.3396]
  cl(0:1, 114  ) = [8.7879, 1.3601]
  cl(0:1, 115  ) = [9.3939, 1.343]
  cl(0:1, 116  ) = [10.0, 1.3075]
  cl(0:1, 117  ) = [10.6061, 1.2174]
  cl(0:1, 118  ) = [11.2121, 1.1488]
  cl(0:1, 119  ) = [11.8182, 1.1201]
  cl(0:1, 120  ) = [12.4242, 1.1064]
  cl(0:1, 121  ) = [13.0303, 1.1]
  cl(0:1, 122  ) = [13.6364, 1.109]
  cl(0:1, 123  ) = [14.2424, 1.1201]
  cl(0:1, 124  ) = [14.8485, 1.1308]
  cl(0:1, 125  ) = [15.4545, 1.1352]
  cl(0:1, 126  ) = [16.0606, 1.1368]
  cl(0:1, 127  ) = [16.6667, 1.1321]
  cl(0:1, 128  ) = [17.2727, 1.1306]
  cl(0:1, 129  ) = [17.8788, 1.1327]
  cl(0:1, 130  ) = [18.4848, 1.1353]
  cl(0:1, 131  ) = [19.0909, 1.1381]
  cl(0:1, 132  ) = [19.697, 1.14]
  cl(0:1, 133  ) = [20.303, 1.1414]
  cl(0:1, 134  ) = [20.9091, 1.1428]
  cl(0:1, 135  ) = [21.5152, 1.1452]
  cl(0:1, 136  ) = [22.1212, 1.1453]
  cl(0:1, 137  ) = [22.7273, 1.1367]
  cl(0:1, 138  ) = [23.3333, 1.128]
  cl(0:1, 139  ) = [23.9394, 1.1194]
  cl(0:1, 140  ) = [24.5455, 1.1113]
  cl(0:1, 141  ) = [25.1515, 1.1033]
  cl(0:1, 142  ) = [25.7576, 1.0954]
  cl(0:1, 143  ) = [26.3636, 1.0882]
  cl(0:1, 144  ) = [26.9697, 1.0817]
  cl(0:1, 145  ) = [27.5758, 1.0752]
  cl(0:1, 146  ) = [28.1818, 1.0689]
  cl(0:1, 147  ) = [28.7879, 1.0633]
  cl(0:1, 148  ) = [29.3939, 1.0578]
  cl(0:1, 149  ) = [30.0, 1.0522]
  cl(0:1, 150  ) = [33.0, 1.028]
  cl(0:1, 151  ) = [36.0, 1.005]
  cl(0:1, 152  ) = [39.0, 0.9811]
  cl(0:1, 153  ) = [42.0, 0.9541]
  cl(0:1, 154  ) = [45.0, 0.9259]
  cl(0:1, 155  ) = [48.0, 0.8909]
  cl(0:1, 156  ) = [51.0, 0.8519]
  cl(0:1, 157  ) = [54.0, 0.8048]
  cl(0:1, 158  ) = [57.0, 0.7577]
  cl(0:1, 159  ) = [60.0, 0.7106]
  cl(0:1, 160  ) = [63.0, 0.648]
  cl(0:1, 161  ) = [66.0, 0.5855]
  cl(0:1, 162  ) = [69.0, 0.523]
  cl(0:1, 163  ) = [72.0, 0.4531]
  cl(0:1, 164  ) = [75.0, 0.3796]
  cl(0:1, 165  ) = [78.0, 0.3061]
  cl(0:1, 166  ) = [81.0, 0.2314]
  cl(0:1, 167  ) = [84.0, 0.1543]
  cl(0:1, 168  ) = [87.0, 0.0771]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.054]
  cl(0:1, 171  ) = [96.0, -0.108]
  cl(0:1, 172  ) = [99.0, -0.162]
  cl(0:1, 173  ) = [102.0, -0.2143]
  cl(0:1, 174  ) = [105.0, -0.2657]
  cl(0:1, 175  ) = [108.0, -0.3172]
  cl(0:1, 176  ) = [111.0, -0.3661]
  cl(0:1, 177  ) = [114.0, -0.4099]
  cl(0:1, 178  ) = [117.0, -0.4536]
  cl(0:1, 179  ) = [120.0, -0.4974]
  cl(0:1, 180  ) = [123.0, -0.5304]
  cl(0:1, 181  ) = [126.0, -0.5634]
  cl(0:1, 182  ) = [129.0, -0.5963]
  cl(0:1, 183  ) = [132.0, -0.6221]
  cl(0:1, 184  ) = [135.0, -0.6442]
  cl(0:1, 185  ) = [138.0, -0.6663]
  cl(0:1, 186  ) = [141.0, -0.6866]
  cl(0:1, 187  ) = [144.0, -0.7032]
  cl(0:1, 188  ) = [147.0, -0.7199]
  cl(0:1, 189  ) = [150.0, -0.7365]
  cl(0:1, 190  ) = [153.0, -0.724]
  cl(0:1, 191  ) = [156.0, -0.71]
  cl(0:1, 192  ) = [159.0, -0.6958]
  cl(0:1, 193  ) = [162.0, -0.6223]
  cl(0:1, 194  ) = [165.0, -0.5187]
  cl(0:1, 195  ) = [168.0, -0.4152]
  cl(0:1, 196  ) = [171.0, -0.3115]
  cl(0:1, 197  ) = [174.0, -0.2077]
  cl(0:1, 198  ) = [177.0, -0.1034]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl21 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl21 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl21 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl21_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl21 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl21_nrel2p8.pro' 
  ENDIF

!print,y,cl21

END FUNCTION calc_cl21_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cl22_nrel2p8( y ) RESULT( cl22 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl22
INTEGER :: i

 cl22 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.113]
  cl(0:1, 2  ) = [-174.0, 0.2271]
  cl(0:1, 3  ) = [-171.0, 0.3406]
  cl(0:1, 4  ) = [-168.0, 0.4498]
  cl(0:1, 5  ) = [-165.0, 0.5568]
  cl(0:1, 6  ) = [-162.0, 0.6638]
  cl(0:1, 7  ) = [-159.0, 0.7352]
  cl(0:1, 8  ) = [-156.0, 0.7367]
  cl(0:1, 9  ) = [-153.0, 0.7372]
  cl(0:1, 10  ) = [-150.0, 0.7353]
  cl(0:1, 11  ) = [-147.0, 0.7188]
  cl(0:1, 12  ) = [-144.0, 0.7023]
  cl(0:1, 13  ) = [-141.0, 0.6858]
  cl(0:1, 14  ) = [-138.0, 0.6656]
  cl(0:1, 15  ) = [-135.0, 0.6436]
  cl(0:1, 16  ) = [-132.0, 0.6216]
  cl(0:1, 17  ) = [-129.0, 0.5959]
  cl(0:1, 18  ) = [-126.0, 0.563]
  cl(0:1, 19  ) = [-123.0, 0.5301]
  cl(0:1, 20  ) = [-120.0, 0.4972]
  cl(0:1, 21  ) = [-117.0, 0.4534]
  cl(0:1, 22  ) = [-114.0, 0.4097]
  cl(0:1, 23  ) = [-111.0, 0.366]
  cl(0:1, 24  ) = [-108.0, 0.3171]
  cl(0:1, 25  ) = [-105.0, 0.2657]
  cl(0:1, 26  ) = [-102.0, 0.2142]
  cl(0:1, 27  ) = [-99.0, 0.162]
  cl(0:1, 28  ) = [-96.0, 0.108]
  cl(0:1, 29  ) = [-93.0, 0.054]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.054]
  cl(0:1, 32  ) = [-84.0, -0.108]
  cl(0:1, 33  ) = [-81.0, -0.162]
  cl(0:1, 34  ) = [-78.0, -0.2142]
  cl(0:1, 35  ) = [-75.0, -0.2657]
  cl(0:1, 36  ) = [-72.0, -0.3171]
  cl(0:1, 37  ) = [-69.0, -0.366]
  cl(0:1, 38  ) = [-66.0, -0.4097]
  cl(0:1, 39  ) = [-63.0, -0.4534]
  cl(0:1, 40  ) = [-60.0, -0.4972]
  cl(0:1, 41  ) = [-57.0, -0.5301]
  cl(0:1, 42  ) = [-54.0, -0.563]
  cl(0:1, 43  ) = [-51.0, -0.5959]
  cl(0:1, 44  ) = [-48.0, -0.6231]
  cl(0:1, 45  ) = [-45.0, -0.6476]
  cl(0:1, 46  ) = [-42.0, -0.6672]
  cl(0:1, 47  ) = [-39.0, -0.686]
  cl(0:1, 48  ) = [-36.0, -0.7026]
  cl(0:1, 49  ) = [-33.0, -0.7185]
  cl(0:1, 50  ) = [-30.0, -0.7353]
  cl(0:1, 51  ) = [-29.3939, -0.7392]
  cl(0:1, 52  ) = [-28.7879, -0.743]
  cl(0:1, 53  ) = [-28.1818, -0.7469]
  cl(0:1, 54  ) = [-27.5758, -0.7512]
  cl(0:1, 55  ) = [-26.9697, -0.7558]
  cl(0:1, 56  ) = [-26.3636, -0.7603]
  cl(0:1, 57  ) = [-25.7576, -0.7652]
  cl(0:1, 58  ) = [-25.1515, -0.7708]
  cl(0:1, 59  ) = [-24.5455, -0.7763]
  cl(0:1, 60  ) = [-23.9394, -0.7821]
  cl(0:1, 61  ) = [-23.3333, -0.7906]
  cl(0:1, 62  ) = [-22.7273, -0.7991]
  cl(0:1, 63  ) = [-22.1212, -0.8076]
  cl(0:1, 64  ) = [-21.5152, -0.8171]
  cl(0:1, 65  ) = [-20.9091, -0.8237]
  cl(0:1, 66  ) = [-20.303, -0.8307]
  cl(0:1, 67  ) = [-19.697, -0.8391]
  cl(0:1, 68  ) = [-19.0909, -0.8483]
  cl(0:1, 69  ) = [-18.4848, -0.8545]
  cl(0:1, 70  ) = [-17.8788, -0.8602]
  cl(0:1, 71  ) = [-17.2727, -0.8657]
  cl(0:1, 72  ) = [-16.6667, -0.8692]
  cl(0:1, 73  ) = [-16.0606, -0.8711]
  cl(0:1, 74  ) = [-15.4545, -0.8746]
  cl(0:1, 75  ) = [-14.8485, -0.8794]
  cl(0:1, 76  ) = [-14.2424, -0.888]
  cl(0:1, 77  ) = [-13.6364, -0.899]
  cl(0:1, 78  ) = [-13.0303, -0.9128]
  cl(0:1, 79  ) = [-12.4242, -0.9058]
  cl(0:1, 80  ) = [-11.8182, -0.8941]
  cl(0:1, 81  ) = [-11.2121, -0.8723]
  cl(0:1, 82  ) = [-10.6061, -0.8438]
  cl(0:1, 83  ) = [-10.0, -0.8115]
  cl(0:1, 84  ) = [-9.3939, -0.7738]
  cl(0:1, 85  ) = [-8.7879, -0.7244]
  cl(0:1, 86  ) = [-8.1818, -0.6514]
  cl(0:1, 87  ) = [-7.5758, -0.5703]
  cl(0:1, 88  ) = [-6.9697, -0.4849]
  cl(0:1, 89  ) = [-6.3636, -0.4013]
  cl(0:1, 90  ) = [-5.7576, -0.3178]
  cl(0:1, 91  ) = [-5.1515, -0.2346]
  cl(0:1, 92  ) = [-4.5455, -0.1528]
  cl(0:1, 93  ) = [-3.9394, -0.0717]
  cl(0:1, 94  ) = [-3.3333, 0.0077]
  cl(0:1, 95  ) = [-2.7273, 0.087]
  cl(0:1, 96  ) = [-2.1212, 0.1656]
  cl(0:1, 97  ) = [-1.5152, 0.2432]
  cl(0:1, 98  ) = [-0.9091, 0.3197]
  cl(0:1, 99  ) = [-0.303, 0.3942]
  cl(0:1, 100  ) = [0.303, 0.4689]
  cl(0:1, 101  ) = [0.9091, 0.5439]
  cl(0:1, 102  ) = [1.5152, 0.6189]
  cl(0:1, 103  ) = [2.1212, 0.6937]
  cl(0:1, 104  ) = [2.7273, 0.7681]
  cl(0:1, 105  ) = [3.3333, 0.8417]
  cl(0:1, 106  ) = [3.9394, 0.9148]
  cl(0:1, 107  ) = [4.5455, 0.9874]
  cl(0:1, 108  ) = [5.1515, 1.0593]
  cl(0:1, 109  ) = [5.7576, 1.1295]
  cl(0:1, 110  ) = [6.3636, 1.1965]
  cl(0:1, 111  ) = [6.9697, 1.2602]
  cl(0:1, 112  ) = [7.5758, 1.3097]
  cl(0:1, 113  ) = [8.1818, 1.3504]
  cl(0:1, 114  ) = [8.7879, 1.3709]
  cl(0:1, 115  ) = [9.3939, 1.3567]
  cl(0:1, 116  ) = [10.0, 1.327]
  cl(0:1, 117  ) = [10.6061, 1.2471]
  cl(0:1, 118  ) = [11.2121, 1.1863]
  cl(0:1, 119  ) = [11.8182, 1.1613]
  cl(0:1, 120  ) = [12.4242, 1.1484]
  cl(0:1, 121  ) = [13.0303, 1.1416]
  cl(0:1, 122  ) = [13.6364, 1.1445]
  cl(0:1, 123  ) = [14.2424, 1.1536]
  cl(0:1, 124  ) = [14.8485, 1.1636]
  cl(0:1, 125  ) = [15.4545, 1.1666]
  cl(0:1, 126  ) = [16.0606, 1.1666]
  cl(0:1, 127  ) = [16.6667, 1.1597]
  cl(0:1, 128  ) = [17.2727, 1.1545]
  cl(0:1, 129  ) = [17.8788, 1.1508]
  cl(0:1, 130  ) = [18.4848, 1.1496]
  cl(0:1, 131  ) = [19.0909, 1.1489]
  cl(0:1, 132  ) = [19.697, 1.1471]
  cl(0:1, 133  ) = [20.303, 1.1443]
  cl(0:1, 134  ) = [20.9091, 1.1427]
  cl(0:1, 135  ) = [21.5152, 1.1442]
  cl(0:1, 136  ) = [22.1212, 1.1431]
  cl(0:1, 137  ) = [22.7273, 1.1344]
  cl(0:1, 138  ) = [23.3333, 1.1257]
  cl(0:1, 139  ) = [23.9394, 1.117]
  cl(0:1, 140  ) = [24.5455, 1.109]
  cl(0:1, 141  ) = [25.1515, 1.1011]
  cl(0:1, 142  ) = [25.7576, 1.0932]
  cl(0:1, 143  ) = [26.3636, 1.0861]
  cl(0:1, 144  ) = [26.9697, 1.0797]
  cl(0:1, 145  ) = [27.5758, 1.0732]
  cl(0:1, 146  ) = [28.1818, 1.067]
  cl(0:1, 147  ) = [28.7879, 1.0615]
  cl(0:1, 148  ) = [29.3939, 1.0559]
  cl(0:1, 149  ) = [30.0, 1.0504]
  cl(0:1, 150  ) = [33.0, 1.0264]
  cl(0:1, 151  ) = [36.0, 1.0037]
  cl(0:1, 152  ) = [39.0, 0.98]
  cl(0:1, 153  ) = [42.0, 0.9532]
  cl(0:1, 154  ) = [45.0, 0.9251]
  cl(0:1, 155  ) = [48.0, 0.8902]
  cl(0:1, 156  ) = [51.0, 0.8513]
  cl(0:1, 157  ) = [54.0, 0.8043]
  cl(0:1, 158  ) = [57.0, 0.7573]
  cl(0:1, 159  ) = [60.0, 0.7103]
  cl(0:1, 160  ) = [63.0, 0.6478]
  cl(0:1, 161  ) = [66.0, 0.5853]
  cl(0:1, 162  ) = [69.0, 0.5228]
  cl(0:1, 163  ) = [72.0, 0.453]
  cl(0:1, 164  ) = [75.0, 0.3795]
  cl(0:1, 165  ) = [78.0, 0.3061]
  cl(0:1, 166  ) = [81.0, 0.2314]
  cl(0:1, 167  ) = [84.0, 0.1543]
  cl(0:1, 168  ) = [87.0, 0.0771]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.054]
  cl(0:1, 171  ) = [96.0, -0.108]
  cl(0:1, 172  ) = [99.0, -0.162]
  cl(0:1, 173  ) = [102.0, -0.2142]
  cl(0:1, 174  ) = [105.0, -0.2657]
  cl(0:1, 175  ) = [108.0, -0.3171]
  cl(0:1, 176  ) = [111.0, -0.366]
  cl(0:1, 177  ) = [114.0, -0.4097]
  cl(0:1, 178  ) = [117.0, -0.4534]
  cl(0:1, 179  ) = [120.0, -0.4972]
  cl(0:1, 180  ) = [123.0, -0.5301]
  cl(0:1, 181  ) = [126.0, -0.563]
  cl(0:1, 182  ) = [129.0, -0.5959]
  cl(0:1, 183  ) = [132.0, -0.6216]
  cl(0:1, 184  ) = [135.0, -0.6436]
  cl(0:1, 185  ) = [138.0, -0.6656]
  cl(0:1, 186  ) = [141.0, -0.6858]
  cl(0:1, 187  ) = [144.0, -0.7023]
  cl(0:1, 188  ) = [147.0, -0.7188]
  cl(0:1, 189  ) = [150.0, -0.7353]
  cl(0:1, 190  ) = [153.0, -0.7283]
  cl(0:1, 191  ) = [156.0, -0.7179]
  cl(0:1, 192  ) = [159.0, -0.7069]
  cl(0:1, 193  ) = [162.0, -0.6348]
  cl(0:1, 194  ) = [165.0, -0.5315]
  cl(0:1, 195  ) = [168.0, -0.4281]
  cl(0:1, 196  ) = [171.0, -0.3233]
  cl(0:1, 197  ) = [174.0, -0.2155]
  cl(0:1, 198  ) = [177.0, -0.1073]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl22 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl22 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl22 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl22_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl22 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl22_nrel2p8.pro' 
  ENDIF

!print,y,cl22

END FUNCTION calc_cl22_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cl23_nrel2p8( y ) RESULT( cl23 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl23
INTEGER :: i

 cl23 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.1214]
  cl(0:1, 2  ) = [-174.0, 0.2439]
  cl(0:1, 3  ) = [-171.0, 0.3658]
  cl(0:1, 4  ) = [-168.0, 0.4755]
  cl(0:1, 5  ) = [-165.0, 0.5791]
  cl(0:1, 6  ) = [-162.0, 0.6828]
  cl(0:1, 7  ) = [-159.0, 0.7499]
  cl(0:1, 8  ) = [-156.0, 0.7454]
  cl(0:1, 9  ) = [-153.0, 0.74]
  cl(0:1, 10  ) = [-150.0, 0.7326]
  cl(0:1, 11  ) = [-147.0, 0.7164]
  cl(0:1, 12  ) = [-144.0, 0.7002]
  cl(0:1, 13  ) = [-141.0, 0.6841]
  cl(0:1, 14  ) = [-138.0, 0.6641]
  cl(0:1, 15  ) = [-135.0, 0.6423]
  cl(0:1, 16  ) = [-132.0, 0.6205]
  cl(0:1, 17  ) = [-129.0, 0.595]
  cl(0:1, 18  ) = [-126.0, 0.5622]
  cl(0:1, 19  ) = [-123.0, 0.5294]
  cl(0:1, 20  ) = [-120.0, 0.4967]
  cl(0:1, 21  ) = [-117.0, 0.453]
  cl(0:1, 22  ) = [-114.0, 0.4094]
  cl(0:1, 23  ) = [-111.0, 0.3657]
  cl(0:1, 24  ) = [-108.0, 0.3169]
  cl(0:1, 25  ) = [-105.0, 0.2655]
  cl(0:1, 26  ) = [-102.0, 0.2142]
  cl(0:1, 27  ) = [-99.0, 0.1619]
  cl(0:1, 28  ) = [-96.0, 0.1079]
  cl(0:1, 29  ) = [-93.0, 0.054]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.054]
  cl(0:1, 32  ) = [-84.0, -0.1079]
  cl(0:1, 33  ) = [-81.0, -0.1619]
  cl(0:1, 34  ) = [-78.0, -0.2142]
  cl(0:1, 35  ) = [-75.0, -0.2655]
  cl(0:1, 36  ) = [-72.0, -0.3169]
  cl(0:1, 37  ) = [-69.0, -0.3657]
  cl(0:1, 38  ) = [-66.0, -0.4094]
  cl(0:1, 39  ) = [-63.0, -0.453]
  cl(0:1, 40  ) = [-60.0, -0.4967]
  cl(0:1, 41  ) = [-57.0, -0.5294]
  cl(0:1, 42  ) = [-54.0, -0.5622]
  cl(0:1, 43  ) = [-51.0, -0.595]
  cl(0:1, 44  ) = [-48.0, -0.6221]
  cl(0:1, 45  ) = [-45.0, -0.6463]
  cl(0:1, 46  ) = [-42.0, -0.6657]
  cl(0:1, 47  ) = [-39.0, -0.6843]
  cl(0:1, 48  ) = [-36.0, -0.7006]
  cl(0:1, 49  ) = [-33.0, -0.7162]
  cl(0:1, 50  ) = [-30.0, -0.7326]
  cl(0:1, 51  ) = [-29.3939, -0.7364]
  cl(0:1, 52  ) = [-28.7879, -0.7402]
  cl(0:1, 53  ) = [-28.1818, -0.7439]
  cl(0:1, 54  ) = [-27.5758, -0.7482]
  cl(0:1, 55  ) = [-26.9697, -0.7526]
  cl(0:1, 56  ) = [-26.3636, -0.757]
  cl(0:1, 57  ) = [-25.7576, -0.7619]
  cl(0:1, 58  ) = [-25.1515, -0.7673]
  cl(0:1, 59  ) = [-24.5455, -0.7727]
  cl(0:1, 60  ) = [-23.9394, -0.7784]
  cl(0:1, 61  ) = [-23.3333, -0.7865]
  cl(0:1, 62  ) = [-22.7273, -0.7945]
  cl(0:1, 63  ) = [-22.1212, -0.8026]
  cl(0:1, 64  ) = [-21.5152, -0.8109]
  cl(0:1, 65  ) = [-20.9091, -0.8165]
  cl(0:1, 66  ) = [-20.303, -0.8225]
  cl(0:1, 67  ) = [-19.697, -0.83]
  cl(0:1, 68  ) = [-19.0909, -0.8383]
  cl(0:1, 69  ) = [-18.4848, -0.8415]
  cl(0:1, 70  ) = [-17.8788, -0.8438]
  cl(0:1, 71  ) = [-17.2727, -0.846]
  cl(0:1, 72  ) = [-16.6667, -0.8445]
  cl(0:1, 73  ) = [-16.0606, -0.8401]
  cl(0:1, 74  ) = [-15.4545, -0.8378]
  cl(0:1, 75  ) = [-14.8485, -0.8372]
  cl(0:1, 76  ) = [-14.2424, -0.8409]
  cl(0:1, 77  ) = [-13.6364, -0.8468]
  cl(0:1, 78  ) = [-13.0303, -0.8553]
  cl(0:1, 79  ) = [-12.4242, -0.8472]
  cl(0:1, 80  ) = [-11.8182, -0.8354]
  cl(0:1, 81  ) = [-11.2121, -0.8154]
  cl(0:1, 82  ) = [-10.6061, -0.7901]
  cl(0:1, 83  ) = [-10.0, -0.7617]
  cl(0:1, 84  ) = [-9.3939, -0.7284]
  cl(0:1, 85  ) = [-8.7879, -0.6841]
  cl(0:1, 86  ) = [-8.1818, -0.618]
  cl(0:1, 87  ) = [-7.5758, -0.5381]
  cl(0:1, 88  ) = [-6.9697, -0.4516]
  cl(0:1, 89  ) = [-6.3636, -0.3699]
  cl(0:1, 90  ) = [-5.7576, -0.2885]
  cl(0:1, 91  ) = [-5.1515, -0.2073]
  cl(0:1, 92  ) = [-4.5455, -0.1273]
  cl(0:1, 93  ) = [-3.9394, -0.048]
  cl(0:1, 94  ) = [-3.3333, 0.03]
  cl(0:1, 95  ) = [-2.7273, 0.1079]
  cl(0:1, 96  ) = [-2.1212, 0.1853]
  cl(0:1, 97  ) = [-1.5152, 0.2618]
  cl(0:1, 98  ) = [-0.9091, 0.3374]
  cl(0:1, 99  ) = [-0.303, 0.4113]
  cl(0:1, 100  ) = [0.303, 0.4854]
  cl(0:1, 101  ) = [0.9091, 0.5597]
  cl(0:1, 102  ) = [1.5152, 0.6339]
  cl(0:1, 103  ) = [2.1212, 0.708]
  cl(0:1, 104  ) = [2.7273, 0.7817]
  cl(0:1, 105  ) = [3.3333, 0.8548]
  cl(0:1, 106  ) = [3.9394, 0.9273]
  cl(0:1, 107  ) = [4.5455, 0.9993]
  cl(0:1, 108  ) = [5.1515, 1.0708]
  cl(0:1, 109  ) = [5.7576, 1.1407]
  cl(0:1, 110  ) = [6.3636, 1.2074]
  cl(0:1, 111  ) = [6.9697, 1.2703]
  cl(0:1, 112  ) = [7.5758, 1.3197]
  cl(0:1, 113  ) = [8.1818, 1.3612]
  cl(0:1, 114  ) = [8.7879, 1.3865]
  cl(0:1, 115  ) = [9.3939, 1.3836]
  cl(0:1, 116  ) = [10.0, 1.3663]
  cl(0:1, 117  ) = [10.6061, 1.3079]
  cl(0:1, 118  ) = [11.2121, 1.2635]
  cl(0:1, 119  ) = [11.8182, 1.2462]
  cl(0:1, 120  ) = [12.4242, 1.2332]
  cl(0:1, 121  ) = [13.0303, 1.2228]
  cl(0:1, 122  ) = [13.6364, 1.213]
  cl(0:1, 123  ) = [14.2424, 1.21]
  cl(0:1, 124  ) = [14.8485, 1.2096]
  cl(0:1, 125  ) = [15.4545, 1.2063]
  cl(0:1, 126  ) = [16.0606, 1.2013]
  cl(0:1, 127  ) = [16.6667, 1.1905]
  cl(0:1, 128  ) = [17.2727, 1.1796]
  cl(0:1, 129  ) = [17.8788, 1.1683]
  cl(0:1, 130  ) = [18.4848, 1.1628]
  cl(0:1, 131  ) = [19.0909, 1.1587]
  cl(0:1, 132  ) = [19.697, 1.1534]
  cl(0:1, 133  ) = [20.303, 1.1467]
  cl(0:1, 134  ) = [20.9091, 1.1421]
  cl(0:1, 135  ) = [21.5152, 1.1416]
  cl(0:1, 136  ) = [22.1212, 1.138]
  cl(0:1, 137  ) = [22.7273, 1.1293]
  cl(0:1, 138  ) = [23.3333, 1.1205]
  cl(0:1, 139  ) = [23.9394, 1.1117]
  cl(0:1, 140  ) = [24.5455, 1.1039]
  cl(0:1, 141  ) = [25.1515, 1.0961]
  cl(0:1, 142  ) = [25.7576, 1.0884]
  cl(0:1, 143  ) = [26.3636, 1.0815]
  cl(0:1, 144  ) = [26.9697, 1.0751]
  cl(0:1, 145  ) = [27.5758, 1.0688]
  cl(0:1, 146  ) = [28.1818, 1.0628]
  cl(0:1, 147  ) = [28.7879, 1.0574]
  cl(0:1, 148  ) = [29.3939, 1.052]
  cl(0:1, 149  ) = [30.0, 1.0466]
  cl(0:1, 150  ) = [33.0, 1.0231]
  cl(0:1, 151  ) = [36.0, 1.0008]
  cl(0:1, 152  ) = [39.0, 0.9776]
  cl(0:1, 153  ) = [42.0, 0.951]
  cl(0:1, 154  ) = [45.0, 0.9233]
  cl(0:1, 155  ) = [48.0, 0.8887]
  cl(0:1, 156  ) = [51.0, 0.85]
  cl(0:1, 157  ) = [54.0, 0.8031]
  cl(0:1, 158  ) = [57.0, 0.7563]
  cl(0:1, 159  ) = [60.0, 0.7095]
  cl(0:1, 160  ) = [63.0, 0.6472]
  cl(0:1, 161  ) = [66.0, 0.5848]
  cl(0:1, 162  ) = [69.0, 0.5224]
  cl(0:1, 163  ) = [72.0, 0.4527]
  cl(0:1, 164  ) = [75.0, 0.3793]
  cl(0:1, 165  ) = [78.0, 0.3059]
  cl(0:1, 166  ) = [81.0, 0.2313]
  cl(0:1, 167  ) = [84.0, 0.1542]
  cl(0:1, 168  ) = [87.0, 0.0771]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.054]
  cl(0:1, 171  ) = [96.0, -0.1079]
  cl(0:1, 172  ) = [99.0, -0.1619]
  cl(0:1, 173  ) = [102.0, -0.2142]
  cl(0:1, 174  ) = [105.0, -0.2655]
  cl(0:1, 175  ) = [108.0, -0.3169]
  cl(0:1, 176  ) = [111.0, -0.3657]
  cl(0:1, 177  ) = [114.0, -0.4094]
  cl(0:1, 178  ) = [117.0, -0.453]
  cl(0:1, 179  ) = [120.0, -0.4967]
  cl(0:1, 180  ) = [123.0, -0.5294]
  cl(0:1, 181  ) = [126.0, -0.5622]
  cl(0:1, 182  ) = [129.0, -0.595]
  cl(0:1, 183  ) = [132.0, -0.6205]
  cl(0:1, 184  ) = [135.0, -0.6423]
  cl(0:1, 185  ) = [138.0, -0.6641]
  cl(0:1, 186  ) = [141.0, -0.6841]
  cl(0:1, 187  ) = [144.0, -0.7002]
  cl(0:1, 188  ) = [147.0, -0.7164]
  cl(0:1, 189  ) = [150.0, -0.7326]
  cl(0:1, 190  ) = [153.0, -0.7333]
  cl(0:1, 191  ) = [156.0, -0.7309]
  cl(0:1, 192  ) = [159.0, -0.7281]
  cl(0:1, 193  ) = [162.0, -0.659]
  cl(0:1, 194  ) = [165.0, -0.5561]
  cl(0:1, 195  ) = [168.0, -0.4531]
  cl(0:1, 196  ) = [171.0, -0.346]
  cl(0:1, 197  ) = [174.0, -0.2307]
  cl(0:1, 198  ) = [177.0, -0.1148]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl23 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl23 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl23 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl23_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl23 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl23_nrel2p8.pro' 
  ENDIF

!print,y,cl23

END FUNCTION calc_cl23_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cl24_nrel2p8( y ) RESULT( cl24 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl24
INTEGER :: i

 cl24 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.1314]
  cl(0:1, 2  ) = [-174.0, 0.264]
  cl(0:1, 3  ) = [-171.0, 0.396]
  cl(0:1, 4  ) = [-168.0, 0.5064]
  cl(0:1, 5  ) = [-165.0, 0.606]
  cl(0:1, 6  ) = [-162.0, 0.7056]
  cl(0:1, 7  ) = [-159.0, 0.7677]
  cl(0:1, 8  ) = [-156.0, 0.7554]
  cl(0:1, 9  ) = [-153.0, 0.7427]
  cl(0:1, 10  ) = [-150.0, 0.7288]
  cl(0:1, 11  ) = [-147.0, 0.7131]
  cl(0:1, 12  ) = [-144.0, 0.6974]
  cl(0:1, 13  ) = [-141.0, 0.6816]
  cl(0:1, 14  ) = [-138.0, 0.662]
  cl(0:1, 15  ) = [-135.0, 0.6405]
  cl(0:1, 16  ) = [-132.0, 0.6189]
  cl(0:1, 17  ) = [-129.0, 0.5937]
  cl(0:1, 18  ) = [-126.0, 0.5611]
  cl(0:1, 19  ) = [-123.0, 0.5285]
  cl(0:1, 20  ) = [-120.0, 0.4959]
  cl(0:1, 21  ) = [-117.0, 0.4524]
  cl(0:1, 22  ) = [-114.0, 0.4089]
  cl(0:1, 23  ) = [-111.0, 0.3653]
  cl(0:1, 24  ) = [-108.0, 0.3166]
  cl(0:1, 25  ) = [-105.0, 0.2653]
  cl(0:1, 26  ) = [-102.0, 0.214]
  cl(0:1, 27  ) = [-99.0, 0.1618]
  cl(0:1, 28  ) = [-96.0, 0.1079]
  cl(0:1, 29  ) = [-93.0, 0.0539]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.0539]
  cl(0:1, 32  ) = [-84.0, -0.1079]
  cl(0:1, 33  ) = [-81.0, -0.1618]
  cl(0:1, 34  ) = [-78.0, -0.214]
  cl(0:1, 35  ) = [-75.0, -0.2653]
  cl(0:1, 36  ) = [-72.0, -0.3166]
  cl(0:1, 37  ) = [-69.0, -0.3653]
  cl(0:1, 38  ) = [-66.0, -0.4089]
  cl(0:1, 39  ) = [-63.0, -0.4524]
  cl(0:1, 40  ) = [-60.0, -0.4959]
  cl(0:1, 41  ) = [-57.0, -0.5285]
  cl(0:1, 42  ) = [-54.0, -0.5611]
  cl(0:1, 43  ) = [-51.0, -0.5937]
  cl(0:1, 44  ) = [-48.0, -0.6205]
  cl(0:1, 45  ) = [-45.0, -0.6445]
  cl(0:1, 46  ) = [-42.0, -0.6636]
  cl(0:1, 47  ) = [-39.0, -0.6819]
  cl(0:1, 48  ) = [-36.0, -0.6978]
  cl(0:1, 49  ) = [-33.0, -0.7129]
  cl(0:1, 50  ) = [-30.0, -0.7288]
  cl(0:1, 51  ) = [-29.3939, -0.7325]
  cl(0:1, 52  ) = [-28.7879, -0.7361]
  cl(0:1, 53  ) = [-28.1818, -0.7398]
  cl(0:1, 54  ) = [-27.5758, -0.7439]
  cl(0:1, 55  ) = [-26.9697, -0.7482]
  cl(0:1, 56  ) = [-26.3636, -0.7525]
  cl(0:1, 57  ) = [-25.7576, -0.7572]
  cl(0:1, 58  ) = [-25.1515, -0.7624]
  cl(0:1, 59  ) = [-24.5455, -0.7677]
  cl(0:1, 60  ) = [-23.9394, -0.7732]
  cl(0:1, 61  ) = [-23.3333, -0.7807]
  cl(0:1, 62  ) = [-22.7273, -0.7882]
  cl(0:1, 63  ) = [-22.1212, -0.7957]
  cl(0:1, 64  ) = [-21.5152, -0.8031]
  cl(0:1, 65  ) = [-20.9091, -0.8091]
  cl(0:1, 66  ) = [-20.303, -0.8154]
  cl(0:1, 67  ) = [-19.697, -0.8227]
  cl(0:1, 68  ) = [-19.0909, -0.8309]
  cl(0:1, 69  ) = [-18.4848, -0.8319]
  cl(0:1, 70  ) = [-17.8788, -0.8316]
  cl(0:1, 71  ) = [-17.2727, -0.831]
  cl(0:1, 72  ) = [-16.6667, -0.8253]
  cl(0:1, 73  ) = [-16.0606, -0.8153]
  cl(0:1, 74  ) = [-15.4545, -0.8072]
  cl(0:1, 75  ) = [-14.8485, -0.8005]
  cl(0:1, 76  ) = [-14.2424, -0.7973]
  cl(0:1, 77  ) = [-13.6364, -0.7954]
  cl(0:1, 78  ) = [-13.0303, -0.795]
  cl(0:1, 79  ) = [-12.4242, -0.7835]
  cl(0:1, 80  ) = [-11.8182, -0.7694]
  cl(0:1, 81  ) = [-11.2121, -0.7499]
  cl(0:1, 82  ) = [-10.6061, -0.7269]
  cl(0:1, 83  ) = [-10.0, -0.7019]
  cl(0:1, 84  ) = [-9.3939, -0.6738]
  cl(0:1, 85  ) = [-8.7879, -0.6366]
  cl(0:1, 86  ) = [-8.1818, -0.5819]
  cl(0:1, 87  ) = [-7.5758, -0.5052]
  cl(0:1, 88  ) = [-6.9697, -0.4186]
  cl(0:1, 89  ) = [-6.3636, -0.339]
  cl(0:1, 90  ) = [-5.7576, -0.2598]
  cl(0:1, 91  ) = [-5.1515, -0.181]
  cl(0:1, 92  ) = [-4.5455, -0.1031]
  cl(0:1, 93  ) = [-3.9394, -0.0258]
  cl(0:1, 94  ) = [-3.3333, 0.0506]
  cl(0:1, 95  ) = [-2.7273, 0.127]
  cl(0:1, 96  ) = [-2.1212, 0.2029]
  cl(0:1, 97  ) = [-1.5152, 0.2783]
  cl(0:1, 98  ) = [-0.9091, 0.3529]
  cl(0:1, 99  ) = [-0.303, 0.4263]
  cl(0:1, 100  ) = [0.303, 0.4998]
  cl(0:1, 101  ) = [0.9091, 0.5734]
  cl(0:1, 102  ) = [1.5152, 0.6468]
  cl(0:1, 103  ) = [2.1212, 0.7202]
  cl(0:1, 104  ) = [2.7273, 0.7931]
  cl(0:1, 105  ) = [3.3333, 0.8656]
  cl(0:1, 106  ) = [3.9394, 0.9376]
  cl(0:1, 107  ) = [4.5455, 1.0091]
  cl(0:1, 108  ) = [5.1515, 1.0801]
  cl(0:1, 109  ) = [5.7576, 1.1496]
  cl(0:1, 110  ) = [6.3636, 1.2162]
  cl(0:1, 111  ) = [6.9697, 1.2783]
  cl(0:1, 112  ) = [7.5758, 1.328]
  cl(0:1, 113  ) = [8.1818, 1.3711]
  cl(0:1, 114  ) = [8.7879, 1.4037]
  cl(0:1, 115  ) = [9.3939, 1.4171]
  cl(0:1, 116  ) = [10.0, 1.4165]
  cl(0:1, 117  ) = [10.6061, 1.3866]
  cl(0:1, 118  ) = [11.2121, 1.364]
  cl(0:1, 119  ) = [11.8182, 1.3569]
  cl(0:1, 120  ) = [12.4242, 1.3409]
  cl(0:1, 121  ) = [13.0303, 1.3222]
  cl(0:1, 122  ) = [13.6364, 1.2959]
  cl(0:1, 123  ) = [14.2424, 1.2755]
  cl(0:1, 124  ) = [14.8485, 1.2603]
  cl(0:1, 125  ) = [15.4545, 1.2482]
  cl(0:1, 126  ) = [16.0606, 1.2366]
  cl(0:1, 127  ) = [16.6667, 1.2211]
  cl(0:1, 128  ) = [17.2727, 1.2037]
  cl(0:1, 129  ) = [17.8788, 1.1837]
  cl(0:1, 130  ) = [18.4848, 1.174]
  cl(0:1, 131  ) = [19.0909, 1.1667]
  cl(0:1, 132  ) = [19.697, 1.158]
  cl(0:1, 133  ) = [20.303, 1.1483]
  cl(0:1, 134  ) = [20.9091, 1.141]
  cl(0:1, 135  ) = [21.5152, 1.1373]
  cl(0:1, 136  ) = [22.1212, 1.1309]
  cl(0:1, 137  ) = [22.7273, 1.122]
  cl(0:1, 138  ) = [23.3333, 1.1132]
  cl(0:1, 139  ) = [23.9394, 1.1044]
  cl(0:1, 140  ) = [24.5455, 1.0967]
  cl(0:1, 141  ) = [25.1515, 1.0892]
  cl(0:1, 142  ) = [25.7576, 1.0817]
  cl(0:1, 143  ) = [26.3636, 1.075]
  cl(0:1, 144  ) = [26.9697, 1.0688]
  cl(0:1, 145  ) = [27.5758, 1.0627]
  cl(0:1, 146  ) = [28.1818, 1.0569]
  cl(0:1, 147  ) = [28.7879, 1.0516]
  cl(0:1, 148  ) = [29.3939, 1.0464]
  cl(0:1, 149  ) = [30.0, 1.0412]
  cl(0:1, 150  ) = [33.0, 1.0185]
  cl(0:1, 151  ) = [36.0, 0.9968]
  cl(0:1, 152  ) = [39.0, 0.9741]
  cl(0:1, 153  ) = [42.0, 0.948]
  cl(0:1, 154  ) = [45.0, 0.9207]
  cl(0:1, 155  ) = [48.0, 0.8865]
  cl(0:1, 156  ) = [51.0, 0.8481]
  cl(0:1, 157  ) = [54.0, 0.8016]
  cl(0:1, 158  ) = [57.0, 0.755]
  cl(0:1, 159  ) = [60.0, 0.7085]
  cl(0:1, 160  ) = [63.0, 0.6463]
  cl(0:1, 161  ) = [66.0, 0.5841]
  cl(0:1, 162  ) = [69.0, 0.5219]
  cl(0:1, 163  ) = [72.0, 0.4523]
  cl(0:1, 164  ) = [75.0, 0.379]
  cl(0:1, 165  ) = [78.0, 0.3058]
  cl(0:1, 166  ) = [81.0, 0.2312]
  cl(0:1, 167  ) = [84.0, 0.1541]
  cl(0:1, 168  ) = [87.0, 0.0771]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.0539]
  cl(0:1, 171  ) = [96.0, -0.1079]
  cl(0:1, 172  ) = [99.0, -0.1618]
  cl(0:1, 173  ) = [102.0, -0.214]
  cl(0:1, 174  ) = [105.0, -0.2653]
  cl(0:1, 175  ) = [108.0, -0.3166]
  cl(0:1, 176  ) = [111.0, -0.3653]
  cl(0:1, 177  ) = [114.0, -0.4089]
  cl(0:1, 178  ) = [117.0, -0.4524]
  cl(0:1, 179  ) = [120.0, -0.4959]
  cl(0:1, 180  ) = [123.0, -0.5285]
  cl(0:1, 181  ) = [126.0, -0.5611]
  cl(0:1, 182  ) = [129.0, -0.5937]
  cl(0:1, 183  ) = [132.0, -0.6189]
  cl(0:1, 184  ) = [135.0, -0.6405]
  cl(0:1, 185  ) = [138.0, -0.662]
  cl(0:1, 186  ) = [141.0, -0.6816]
  cl(0:1, 187  ) = [144.0, -0.6974]
  cl(0:1, 188  ) = [147.0, -0.7131]
  cl(0:1, 189  ) = [150.0, -0.7288]
  cl(0:1, 190  ) = [153.0, -0.7383]
  cl(0:1, 191  ) = [156.0, -0.7461]
  cl(0:1, 192  ) = [159.0, -0.7537]
  cl(0:1, 193  ) = [162.0, -0.6881]
  cl(0:1, 194  ) = [165.0, -0.5857]
  cl(0:1, 195  ) = [168.0, -0.4832]
  cl(0:1, 196  ) = [171.0, -0.3733]
  cl(0:1, 197  ) = [174.0, -0.2489]
  cl(0:1, 198  ) = [177.0, -0.1239]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl24 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl24 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl24 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl24_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl24 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl24_nrel2p8.pro' 
  ENDIF

!print,y,cl24

END FUNCTION calc_cl24_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cl25_nrel2p8( y ) RESULT( cl25 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl25
INTEGER :: i

 cl25 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.1406]
  cl(0:1, 2  ) = [-174.0, 0.2826]
  cl(0:1, 3  ) = [-171.0, 0.4238]
  cl(0:1, 4  ) = [-168.0, 0.5349]
  cl(0:1, 5  ) = [-165.0, 0.6308]
  cl(0:1, 6  ) = [-162.0, 0.7267]
  cl(0:1, 7  ) = [-159.0, 0.7841]
  cl(0:1, 8  ) = [-156.0, 0.7645]
  cl(0:1, 9  ) = [-153.0, 0.7448]
  cl(0:1, 10  ) = [-150.0, 0.7248]
  cl(0:1, 11  ) = [-147.0, 0.7095]
  cl(0:1, 12  ) = [-144.0, 0.6943]
  cl(0:1, 13  ) = [-141.0, 0.679]
  cl(0:1, 14  ) = [-138.0, 0.6598]
  cl(0:1, 15  ) = [-135.0, 0.6385]
  cl(0:1, 16  ) = [-132.0, 0.6173]
  cl(0:1, 17  ) = [-129.0, 0.5923]
  cl(0:1, 18  ) = [-126.0, 0.5599]
  cl(0:1, 19  ) = [-123.0, 0.5275]
  cl(0:1, 20  ) = [-120.0, 0.4952]
  cl(0:1, 21  ) = [-117.0, 0.4518]
  cl(0:1, 22  ) = [-114.0, 0.4084]
  cl(0:1, 23  ) = [-111.0, 0.365]
  cl(0:1, 24  ) = [-108.0, 0.3164]
  cl(0:1, 25  ) = [-105.0, 0.2651]
  cl(0:1, 26  ) = [-102.0, 0.2139]
  cl(0:1, 27  ) = [-99.0, 0.1618]
  cl(0:1, 28  ) = [-96.0, 0.1078]
  cl(0:1, 29  ) = [-93.0, 0.0539]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.0539]
  cl(0:1, 32  ) = [-84.0, -0.1078]
  cl(0:1, 33  ) = [-81.0, -0.1618]
  cl(0:1, 34  ) = [-78.0, -0.2139]
  cl(0:1, 35  ) = [-75.0, -0.2651]
  cl(0:1, 36  ) = [-72.0, -0.3164]
  cl(0:1, 37  ) = [-69.0, -0.365]
  cl(0:1, 38  ) = [-66.0, -0.4084]
  cl(0:1, 39  ) = [-63.0, -0.4518]
  cl(0:1, 40  ) = [-60.0, -0.4952]
  cl(0:1, 41  ) = [-57.0, -0.5275]
  cl(0:1, 42  ) = [-54.0, -0.5599]
  cl(0:1, 43  ) = [-51.0, -0.5923]
  cl(0:1, 44  ) = [-48.0, -0.6189]
  cl(0:1, 45  ) = [-45.0, -0.6426]
  cl(0:1, 46  ) = [-42.0, -0.6614]
  cl(0:1, 47  ) = [-39.0, -0.6793]
  cl(0:1, 48  ) = [-36.0, -0.6948]
  cl(0:1, 49  ) = [-33.0, -0.7095]
  cl(0:1, 50  ) = [-30.0, -0.7248]
  cl(0:1, 51  ) = [-29.3939, -0.7283]
  cl(0:1, 52  ) = [-28.7879, -0.7319]
  cl(0:1, 53  ) = [-28.1818, -0.7354]
  cl(0:1, 54  ) = [-27.5758, -0.7393]
  cl(0:1, 55  ) = [-26.9697, -0.7435]
  cl(0:1, 56  ) = [-26.3636, -0.7476]
  cl(0:1, 57  ) = [-25.7576, -0.7522]
  cl(0:1, 58  ) = [-25.1515, -0.7572]
  cl(0:1, 59  ) = [-24.5455, -0.7623]
  cl(0:1, 60  ) = [-23.9394, -0.7676]
  cl(0:1, 61  ) = [-23.3333, -0.7745]
  cl(0:1, 62  ) = [-22.7273, -0.7814]
  cl(0:1, 63  ) = [-22.1212, -0.7883]
  cl(0:1, 64  ) = [-21.5152, -0.7956]
  cl(0:1, 65  ) = [-20.9091, -0.8026]
  cl(0:1, 66  ) = [-20.303, -0.8097]
  cl(0:1, 67  ) = [-19.697, -0.8176]
  cl(0:1, 68  ) = [-19.0909, -0.8265]
  cl(0:1, 69  ) = [-18.4848, -0.8261]
  cl(0:1, 70  ) = [-17.8788, -0.824]
  cl(0:1, 71  ) = [-17.2727, -0.8216]
  cl(0:1, 72  ) = [-16.6667, -0.8124]
  cl(0:1, 73  ) = [-16.0606, -0.7978]
  cl(0:1, 74  ) = [-15.4545, -0.7844]
  cl(0:1, 75  ) = [-14.8485, -0.7718]
  cl(0:1, 76  ) = [-14.2424, -0.7611]
  cl(0:1, 77  ) = [-13.6364, -0.751]
  cl(0:1, 78  ) = [-13.0303, -0.7414]
  cl(0:1, 79  ) = [-12.4242, -0.7261]
  cl(0:1, 80  ) = [-11.8182, -0.7095]
  cl(0:1, 81  ) = [-11.2121, -0.6901]
  cl(0:1, 82  ) = [-10.6061, -0.669]
  cl(0:1, 83  ) = [-10.0, -0.647]
  cl(0:1, 84  ) = [-9.3939, -0.6236]
  cl(0:1, 85  ) = [-8.7879, -0.5931]
  cl(0:1, 86  ) = [-8.1818, -0.5494]
  cl(0:1, 87  ) = [-7.5758, -0.4761]
  cl(0:1, 88  ) = [-6.9697, -0.3899]
  cl(0:1, 89  ) = [-6.3636, -0.3123]
  cl(0:1, 90  ) = [-5.7576, -0.2352]
  cl(0:1, 91  ) = [-5.1515, -0.1586]
  cl(0:1, 92  ) = [-4.5455, -0.0826]
  cl(0:1, 93  ) = [-3.9394, -0.0072]
  cl(0:1, 94  ) = [-3.3333, 0.0677]
  cl(0:1, 95  ) = [-2.7273, 0.1427]
  cl(0:1, 96  ) = [-2.1212, 0.2171]
  cl(0:1, 97  ) = [-1.5152, 0.2914]
  cl(0:1, 98  ) = [-0.9091, 0.3651]
  cl(0:1, 99  ) = [-0.303, 0.4381]
  cl(0:1, 100  ) = [0.303, 0.5111]
  cl(0:1, 101  ) = [0.9091, 0.584]
  cl(0:1, 102  ) = [1.5152, 0.6568]
  cl(0:1, 103  ) = [2.1212, 0.7294]
  cl(0:1, 104  ) = [2.7273, 0.8017]
  cl(0:1, 105  ) = [3.3333, 0.8737]
  cl(0:1, 106  ) = [3.9394, 0.9452]
  cl(0:1, 107  ) = [4.5455, 1.0162]
  cl(0:1, 108  ) = [5.1515, 1.0867]
  cl(0:1, 109  ) = [5.7576, 1.1559]
  cl(0:1, 110  ) = [6.3636, 1.2224]
  cl(0:1, 111  ) = [6.9697, 1.2838]
  cl(0:1, 112  ) = [7.5758, 1.3341]
  cl(0:1, 113  ) = [8.1818, 1.3792]
  cl(0:1, 114  ) = [8.7879, 1.4193]
  cl(0:1, 115  ) = [9.3939, 1.449]
  cl(0:1, 116  ) = [10.0, 1.4656]
  cl(0:1, 117  ) = [10.6061, 1.4648]
  cl(0:1, 118  ) = [11.2121, 1.4646]
  cl(0:1, 119  ) = [11.8182, 1.4679]
  cl(0:1, 120  ) = [12.4242, 1.4463]
  cl(0:1, 121  ) = [13.0303, 1.4155]
  cl(0:1, 122  ) = [13.6364, 1.3725]
  cl(0:1, 123  ) = [14.2424, 1.3354]
  cl(0:1, 124  ) = [14.8485, 1.3061]
  cl(0:1, 125  ) = [15.4545, 1.2855]
  cl(0:1, 126  ) = [16.0606, 1.2676]
  cl(0:1, 127  ) = [16.6667, 1.2477]
  cl(0:1, 128  ) = [17.2727, 1.2242]
  cl(0:1, 129  ) = [17.8788, 1.1961]
  cl(0:1, 130  ) = [18.4848, 1.1826]
  cl(0:1, 131  ) = [19.0909, 1.1724]
  cl(0:1, 132  ) = [19.697, 1.1608]
  cl(0:1, 133  ) = [20.303, 1.149]
  cl(0:1, 134  ) = [20.9091, 1.1395]
  cl(0:1, 135  ) = [21.5152, 1.1322]
  cl(0:1, 136  ) = [22.1212, 1.1232]
  cl(0:1, 137  ) = [22.7273, 1.1143]
  cl(0:1, 138  ) = [23.3333, 1.1054]
  cl(0:1, 139  ) = [23.9394, 1.0965]
  cl(0:1, 140  ) = [24.5455, 1.0891]
  cl(0:1, 141  ) = [25.1515, 1.0818]
  cl(0:1, 142  ) = [25.7576, 1.0745]
  cl(0:1, 143  ) = [26.3636, 1.068]
  cl(0:1, 144  ) = [26.9697, 1.0621]
  cl(0:1, 145  ) = [27.5758, 1.0562]
  cl(0:1, 146  ) = [28.1818, 1.0505]
  cl(0:1, 147  ) = [28.7879, 1.0455]
  cl(0:1, 148  ) = [29.3939, 1.0405]
  cl(0:1, 149  ) = [30.0, 1.0354]
  cl(0:1, 150  ) = [33.0, 1.0135]
  cl(0:1, 151  ) = [36.0, 0.9925]
  cl(0:1, 152  ) = [39.0, 0.9704]
  cl(0:1, 153  ) = [42.0, 0.9449]
  cl(0:1, 154  ) = [45.0, 0.918]
  cl(0:1, 155  ) = [48.0, 0.8842]
  cl(0:1, 156  ) = [51.0, 0.8461]
  cl(0:1, 157  ) = [54.0, 0.7999]
  cl(0:1, 158  ) = [57.0, 0.7536]
  cl(0:1, 159  ) = [60.0, 0.7074]
  cl(0:1, 160  ) = [63.0, 0.6454]
  cl(0:1, 161  ) = [66.0, 0.5834]
  cl(0:1, 162  ) = [69.0, 0.5214]
  cl(0:1, 163  ) = [72.0, 0.4519]
  cl(0:1, 164  ) = [75.0, 0.3787]
  cl(0:1, 165  ) = [78.0, 0.3056]
  cl(0:1, 166  ) = [81.0, 0.2311]
  cl(0:1, 167  ) = [84.0, 0.1541]
  cl(0:1, 168  ) = [87.0, 0.077]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.0539]
  cl(0:1, 171  ) = [96.0, -0.1078]
  cl(0:1, 172  ) = [99.0, -0.1618]
  cl(0:1, 173  ) = [102.0, -0.2139]
  cl(0:1, 174  ) = [105.0, -0.2651]
  cl(0:1, 175  ) = [108.0, -0.3164]
  cl(0:1, 176  ) = [111.0, -0.365]
  cl(0:1, 177  ) = [114.0, -0.4084]
  cl(0:1, 178  ) = [117.0, -0.4518]
  cl(0:1, 179  ) = [120.0, -0.4952]
  cl(0:1, 180  ) = [123.0, -0.5275]
  cl(0:1, 181  ) = [126.0, -0.5599]
  cl(0:1, 182  ) = [129.0, -0.5923]
  cl(0:1, 183  ) = [132.0, -0.6173]
  cl(0:1, 184  ) = [135.0, -0.6385]
  cl(0:1, 185  ) = [138.0, -0.6598]
  cl(0:1, 186  ) = [141.0, -0.679]
  cl(0:1, 187  ) = [144.0, -0.6943]
  cl(0:1, 188  ) = [147.0, -0.7095]
  cl(0:1, 189  ) = [150.0, -0.7248]
  cl(0:1, 190  ) = [153.0, -0.7426]
  cl(0:1, 191  ) = [156.0, -0.7599]
  cl(0:1, 192  ) = [159.0, -0.7772]
  cl(0:1, 193  ) = [162.0, -0.715]
  cl(0:1, 194  ) = [165.0, -0.6129]
  cl(0:1, 195  ) = [168.0, -0.5108]
  cl(0:1, 196  ) = [171.0, -0.3985]
  cl(0:1, 197  ) = [174.0, -0.2656]
  cl(0:1, 198  ) = [177.0, -0.1322]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl25 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl25 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl25 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl25_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl25 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl25_nrel2p8.pro' 
  ENDIF

!print,y,cl25

END FUNCTION calc_cl25_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cl26_nrel2p8( y ) RESULT( cl26 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl26
INTEGER :: i

 cl26 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.1471]
  cl(0:1, 2  ) = [-174.0, 0.2955]
  cl(0:1, 3  ) = [-171.0, 0.4433]
  cl(0:1, 4  ) = [-168.0, 0.5548]
  cl(0:1, 5  ) = [-165.0, 0.6481]
  cl(0:1, 6  ) = [-162.0, 0.7415]
  cl(0:1, 7  ) = [-159.0, 0.7955]
  cl(0:1, 8  ) = [-156.0, 0.7709]
  cl(0:1, 9  ) = [-153.0, 0.7463]
  cl(0:1, 10  ) = [-150.0, 0.7216]
  cl(0:1, 11  ) = [-147.0, 0.7067]
  cl(0:1, 12  ) = [-144.0, 0.6918]
  cl(0:1, 13  ) = [-141.0, 0.6769]
  cl(0:1, 14  ) = [-138.0, 0.658]
  cl(0:1, 15  ) = [-135.0, 0.637]
  cl(0:1, 16  ) = [-132.0, 0.6159]
  cl(0:1, 17  ) = [-129.0, 0.5912]
  cl(0:1, 18  ) = [-126.0, 0.559]
  cl(0:1, 19  ) = [-123.0, 0.5268]
  cl(0:1, 20  ) = [-120.0, 0.4945]
  cl(0:1, 21  ) = [-117.0, 0.4513]
  cl(0:1, 22  ) = [-114.0, 0.408]
  cl(0:1, 23  ) = [-111.0, 0.3647]
  cl(0:1, 24  ) = [-108.0, 0.3161]
  cl(0:1, 25  ) = [-105.0, 0.265]
  cl(0:1, 26  ) = [-102.0, 0.2138]
  cl(0:1, 27  ) = [-99.0, 0.1617]
  cl(0:1, 28  ) = [-96.0, 0.1078]
  cl(0:1, 29  ) = [-93.0, 0.0539]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.0539]
  cl(0:1, 32  ) = [-84.0, -0.1078]
  cl(0:1, 33  ) = [-81.0, -0.1617]
  cl(0:1, 34  ) = [-78.0, -0.2138]
  cl(0:1, 35  ) = [-75.0, -0.265]
  cl(0:1, 36  ) = [-72.0, -0.3161]
  cl(0:1, 37  ) = [-69.0, -0.3647]
  cl(0:1, 38  ) = [-66.0, -0.408]
  cl(0:1, 39  ) = [-63.0, -0.4513]
  cl(0:1, 40  ) = [-60.0, -0.4945]
  cl(0:1, 41  ) = [-57.0, -0.5268]
  cl(0:1, 42  ) = [-54.0, -0.559]
  cl(0:1, 43  ) = [-51.0, -0.5912]
  cl(0:1, 44  ) = [-48.0, -0.6176]
  cl(0:1, 45  ) = [-45.0, -0.6411]
  cl(0:1, 46  ) = [-42.0, -0.6596]
  cl(0:1, 47  ) = [-39.0, -0.6772]
  cl(0:1, 48  ) = [-36.0, -0.6924]
  cl(0:1, 49  ) = [-33.0, -0.7067]
  cl(0:1, 50  ) = [-30.0, -0.7216]
  cl(0:1, 51  ) = [-29.3939, -0.725]
  cl(0:1, 52  ) = [-28.7879, -0.7284]
  cl(0:1, 53  ) = [-28.1818, -0.7319]
  cl(0:1, 54  ) = [-27.5758, -0.7357]
  cl(0:1, 55  ) = [-26.9697, -0.7397]
  cl(0:1, 56  ) = [-26.3636, -0.7438]
  cl(0:1, 57  ) = [-25.7576, -0.7482]
  cl(0:1, 58  ) = [-25.1515, -0.7531]
  cl(0:1, 59  ) = [-24.5455, -0.7581]
  cl(0:1, 60  ) = [-23.9394, -0.7632]
  cl(0:1, 61  ) = [-23.3333, -0.7696]
  cl(0:1, 62  ) = [-22.7273, -0.7761]
  cl(0:1, 63  ) = [-22.1212, -0.7825]
  cl(0:1, 64  ) = [-21.5152, -0.7901]
  cl(0:1, 65  ) = [-20.9091, -0.798]
  cl(0:1, 66  ) = [-20.303, -0.8059]
  cl(0:1, 67  ) = [-19.697, -0.8147]
  cl(0:1, 68  ) = [-19.0909, -0.8245]
  cl(0:1, 69  ) = [-18.4848, -0.8234]
  cl(0:1, 70  ) = [-17.8788, -0.8203]
  cl(0:1, 71  ) = [-17.2727, -0.8168]
  cl(0:1, 72  ) = [-16.6667, -0.8055]
  cl(0:1, 73  ) = [-16.0606, -0.7877]
  cl(0:1, 74  ) = [-15.4545, -0.7703]
  cl(0:1, 75  ) = [-14.8485, -0.7531]
  cl(0:1, 76  ) = [-14.2424, -0.7365]
  cl(0:1, 77  ) = [-13.6364, -0.72]
  cl(0:1, 78  ) = [-13.0303, -0.7036]
  cl(0:1, 79  ) = [-12.4242, -0.6856]
  cl(0:1, 80  ) = [-11.8182, -0.6672]
  cl(0:1, 81  ) = [-11.2121, -0.6481]
  cl(0:1, 82  ) = [-10.6061, -0.6285]
  cl(0:1, 83  ) = [-10.0, -0.6086]
  cl(0:1, 84  ) = [-9.3939, -0.5885]
  cl(0:1, 85  ) = [-8.7879, -0.5627]
  cl(0:1, 86  ) = [-8.1818, -0.5265]
  cl(0:1, 87  ) = [-7.5758, -0.4557]
  cl(0:1, 88  ) = [-6.9697, -0.3698]
  cl(0:1, 89  ) = [-6.3636, -0.2936]
  cl(0:1, 90  ) = [-5.7576, -0.2181]
  cl(0:1, 91  ) = [-5.1515, -0.1431]
  cl(0:1, 92  ) = [-4.5455, -0.0685]
  cl(0:1, 93  ) = [-3.9394, 0.0056]
  cl(0:1, 94  ) = [-3.3333, 0.0794]
  cl(0:1, 95  ) = [-2.7273, 0.1532]
  cl(0:1, 96  ) = [-2.1212, 0.2266]
  cl(0:1, 97  ) = [-1.5152, 0.3001]
  cl(0:1, 98  ) = [-0.9091, 0.3731]
  cl(0:1, 99  ) = [-0.303, 0.4458]
  cl(0:1, 100  ) = [0.303, 0.5185]
  cl(0:1, 101  ) = [0.9091, 0.591]
  cl(0:1, 102  ) = [1.5152, 0.6632]
  cl(0:1, 103  ) = [2.1212, 0.7353]
  cl(0:1, 104  ) = [2.7273, 0.8071]
  cl(0:1, 105  ) = [3.3333, 0.8787]
  cl(0:1, 106  ) = [3.9394, 0.9498]
  cl(0:1, 107  ) = [4.5455, 1.0204]
  cl(0:1, 108  ) = [5.1515, 1.0906]
  cl(0:1, 109  ) = [5.7576, 1.1596]
  cl(0:1, 110  ) = [6.3636, 1.226]
  cl(0:1, 111  ) = [6.9697, 1.287]
  cl(0:1, 112  ) = [7.5758, 1.338]
  cl(0:1, 113  ) = [8.1818, 1.3847]
  cl(0:1, 114  ) = [8.7879, 1.4303]
  cl(0:1, 115  ) = [9.3939, 1.472]
  cl(0:1, 116  ) = [10.0, 1.5022]
  cl(0:1, 117  ) = [10.6061, 1.5237]
  cl(0:1, 118  ) = [11.2121, 1.5406]
  cl(0:1, 119  ) = [11.8182, 1.552]
  cl(0:1, 120  ) = [12.4242, 1.5244]
  cl(0:1, 121  ) = [13.0303, 1.482]
  cl(0:1, 122  ) = [13.6364, 1.4263]
  cl(0:1, 123  ) = [14.2424, 1.3774]
  cl(0:1, 124  ) = [14.8485, 1.3385]
  cl(0:1, 125  ) = [15.4545, 1.3119]
  cl(0:1, 126  ) = [16.0606, 1.2894]
  cl(0:1, 127  ) = [16.6667, 1.2663]
  cl(0:1, 128  ) = [17.2727, 1.2383]
  cl(0:1, 129  ) = [17.8788, 1.2043]
  cl(0:1, 130  ) = [18.4848, 1.188]
  cl(0:1, 131  ) = [19.0909, 1.1759]
  cl(0:1, 132  ) = [19.697, 1.1621]
  cl(0:1, 133  ) = [20.303, 1.1493]
  cl(0:1, 134  ) = [20.9091, 1.1381]
  cl(0:1, 135  ) = [21.5152, 1.1277]
  cl(0:1, 136  ) = [22.1212, 1.1171]
  cl(0:1, 137  ) = [22.7273, 1.1081]
  cl(0:1, 138  ) = [23.3333, 1.0992]
  cl(0:1, 139  ) = [23.9394, 1.0902]
  cl(0:1, 140  ) = [24.5455, 1.083]
  cl(0:1, 141  ) = [25.1515, 1.0759]
  cl(0:1, 142  ) = [25.7576, 1.0688]
  cl(0:1, 143  ) = [26.3636, 1.0625]
  cl(0:1, 144  ) = [26.9697, 1.0568]
  cl(0:1, 145  ) = [27.5758, 1.051]
  cl(0:1, 146  ) = [28.1818, 1.0455]
  cl(0:1, 147  ) = [28.7879, 1.0406]
  cl(0:1, 148  ) = [29.3939, 1.0357]
  cl(0:1, 149  ) = [30.0, 1.0308]
  cl(0:1, 150  ) = [33.0, 1.0096]
  cl(0:1, 151  ) = [36.0, 0.9891]
  cl(0:1, 152  ) = [39.0, 0.9675]
  cl(0:1, 153  ) = [42.0, 0.9423]
  cl(0:1, 154  ) = [45.0, 0.9159]
  cl(0:1, 155  ) = [48.0, 0.8823]
  cl(0:1, 156  ) = [51.0, 0.8446]
  cl(0:1, 157  ) = [54.0, 0.7985]
  cl(0:1, 158  ) = [57.0, 0.7525]
  cl(0:1, 159  ) = [60.0, 0.7065]
  cl(0:1, 160  ) = [63.0, 0.6446]
  cl(0:1, 161  ) = [66.0, 0.5828]
  cl(0:1, 162  ) = [69.0, 0.521]
  cl(0:1, 163  ) = [72.0, 0.4516]
  cl(0:1, 164  ) = [75.0, 0.3785]
  cl(0:1, 165  ) = [78.0, 0.3054]
  cl(0:1, 166  ) = [81.0, 0.231]
  cl(0:1, 167  ) = [84.0, 0.154]
  cl(0:1, 168  ) = [87.0, 0.077]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.0539]
  cl(0:1, 171  ) = [96.0, -0.1078]
  cl(0:1, 172  ) = [99.0, -0.1617]
  cl(0:1, 173  ) = [102.0, -0.2138]
  cl(0:1, 174  ) = [105.0, -0.265]
  cl(0:1, 175  ) = [108.0, -0.3161]
  cl(0:1, 176  ) = [111.0, -0.3647]
  cl(0:1, 177  ) = [114.0, -0.408]
  cl(0:1, 178  ) = [117.0, -0.4513]
  cl(0:1, 179  ) = [120.0, -0.4945]
  cl(0:1, 180  ) = [123.0, -0.5268]
  cl(0:1, 181  ) = [126.0, -0.559]
  cl(0:1, 182  ) = [129.0, -0.5912]
  cl(0:1, 183  ) = [132.0, -0.6159]
  cl(0:1, 184  ) = [135.0, -0.637]
  cl(0:1, 185  ) = [138.0, -0.658]
  cl(0:1, 186  ) = [141.0, -0.6769]
  cl(0:1, 187  ) = [144.0, -0.6918]
  cl(0:1, 188  ) = [147.0, -0.7067]
  cl(0:1, 189  ) = [150.0, -0.7216]
  cl(0:1, 190  ) = [153.0, -0.7457]
  cl(0:1, 191  ) = [156.0, -0.7697]
  cl(0:1, 192  ) = [159.0, -0.7937]
  cl(0:1, 193  ) = [162.0, -0.7338]
  cl(0:1, 194  ) = [165.0, -0.632]
  cl(0:1, 195  ) = [168.0, -0.5301]
  cl(0:1, 196  ) = [171.0, -0.416]
  cl(0:1, 197  ) = [174.0, -0.2774]
  cl(0:1, 198  ) = [177.0, -0.138]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl26 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl26 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl26 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl26_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl26 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl26_nrel2p8.pro' 
  ENDIF

!print,y,cl26

END FUNCTION calc_cl26_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cl27_nrel2p8( y ) RESULT( cl27 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl27
INTEGER :: i

 cl27 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.1495]
  cl(0:1, 2  ) = [-174.0, 0.3003]
  cl(0:1, 3  ) = [-171.0, 0.4505]
  cl(0:1, 4  ) = [-168.0, 0.5621]
  cl(0:1, 5  ) = [-165.0, 0.6546]
  cl(0:1, 6  ) = [-162.0, 0.747]
  cl(0:1, 7  ) = [-159.0, 0.7998]
  cl(0:1, 8  ) = [-156.0, 0.7733]
  cl(0:1, 9  ) = [-153.0, 0.7468]
  cl(0:1, 10  ) = [-150.0, 0.7203]
  cl(0:1, 11  ) = [-147.0, 0.7056]
  cl(0:1, 12  ) = [-144.0, 0.6908]
  cl(0:1, 13  ) = [-141.0, 0.6761]
  cl(0:1, 14  ) = [-138.0, 0.6572]
  cl(0:1, 15  ) = [-135.0, 0.6363]
  cl(0:1, 16  ) = [-132.0, 0.6154]
  cl(0:1, 17  ) = [-129.0, 0.5908]
  cl(0:1, 18  ) = [-126.0, 0.5586]
  cl(0:1, 19  ) = [-123.0, 0.5265]
  cl(0:1, 20  ) = [-120.0, 0.4943]
  cl(0:1, 21  ) = [-117.0, 0.451]
  cl(0:1, 22  ) = [-114.0, 0.4078]
  cl(0:1, 23  ) = [-111.0, 0.3645]
  cl(0:1, 24  ) = [-108.0, 0.316]
  cl(0:1, 25  ) = [-105.0, 0.2649]
  cl(0:1, 26  ) = [-102.0, 0.2137]
  cl(0:1, 27  ) = [-99.0, 0.1617]
  cl(0:1, 28  ) = [-96.0, 0.1078]
  cl(0:1, 29  ) = [-93.0, 0.0539]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.0539]
  cl(0:1, 32  ) = [-84.0, -0.1078]
  cl(0:1, 33  ) = [-81.0, -0.1617]
  cl(0:1, 34  ) = [-78.0, -0.2137]
  cl(0:1, 35  ) = [-75.0, -0.2649]
  cl(0:1, 36  ) = [-72.0, -0.316]
  cl(0:1, 37  ) = [-69.0, -0.3645]
  cl(0:1, 38  ) = [-66.0, -0.4078]
  cl(0:1, 39  ) = [-63.0, -0.451]
  cl(0:1, 40  ) = [-60.0, -0.4943]
  cl(0:1, 41  ) = [-57.0, -0.5265]
  cl(0:1, 42  ) = [-54.0, -0.5586]
  cl(0:1, 43  ) = [-51.0, -0.5908]
  cl(0:1, 44  ) = [-48.0, -0.6171]
  cl(0:1, 45  ) = [-45.0, -0.6405]
  cl(0:1, 46  ) = [-42.0, -0.6589]
  cl(0:1, 47  ) = [-39.0, -0.6764]
  cl(0:1, 48  ) = [-36.0, -0.6914]
  cl(0:1, 49  ) = [-33.0, -0.7056]
  cl(0:1, 50  ) = [-30.0, -0.7203]
  cl(0:1, 51  ) = [-29.3939, -0.7237]
  cl(0:1, 52  ) = [-28.7879, -0.7271]
  cl(0:1, 53  ) = [-28.1818, -0.7304]
  cl(0:1, 54  ) = [-27.5758, -0.7342]
  cl(0:1, 55  ) = [-26.9697, -0.7382]
  cl(0:1, 56  ) = [-26.3636, -0.7422]
  cl(0:1, 57  ) = [-25.7576, -0.7466]
  cl(0:1, 58  ) = [-25.1515, -0.7515]
  cl(0:1, 59  ) = [-24.5455, -0.7564]
  cl(0:1, 60  ) = [-23.9394, -0.7614]
  cl(0:1, 61  ) = [-23.3333, -0.7677]
  cl(0:1, 62  ) = [-22.7273, -0.7739]
  cl(0:1, 63  ) = [-22.1212, -0.7802]
  cl(0:1, 64  ) = [-21.5152, -0.788]
  cl(0:1, 65  ) = [-20.9091, -0.7963]
  cl(0:1, 66  ) = [-20.303, -0.8045]
  cl(0:1, 67  ) = [-19.697, -0.8137]
  cl(0:1, 68  ) = [-19.0909, -0.8239]
  cl(0:1, 69  ) = [-18.4848, -0.8226]
  cl(0:1, 70  ) = [-17.8788, -0.8192]
  cl(0:1, 71  ) = [-17.2727, -0.8154]
  cl(0:1, 72  ) = [-16.6667, -0.8033]
  cl(0:1, 73  ) = [-16.0606, -0.7843]
  cl(0:1, 74  ) = [-15.4545, -0.7653]
  cl(0:1, 75  ) = [-14.8485, -0.7463]
  cl(0:1, 76  ) = [-14.2424, -0.7274]
  cl(0:1, 77  ) = [-13.6364, -0.7084]
  cl(0:1, 78  ) = [-13.0303, -0.6894]
  cl(0:1, 79  ) = [-12.4242, -0.6704]
  cl(0:1, 80  ) = [-11.8182, -0.6514]
  cl(0:1, 81  ) = [-11.2121, -0.6324]
  cl(0:1, 82  ) = [-10.6061, -0.6134]
  cl(0:1, 83  ) = [-10.0, -0.5944]
  cl(0:1, 84  ) = [-9.3939, -0.5755]
  cl(0:1, 85  ) = [-8.7879, -0.5514]
  cl(0:1, 86  ) = [-8.1818, -0.5179]
  cl(0:1, 87  ) = [-7.5758, -0.448]
  cl(0:1, 88  ) = [-6.9697, -0.3623]
  cl(0:1, 89  ) = [-6.3636, -0.2866]
  cl(0:1, 90  ) = [-5.7576, -0.2117]
  cl(0:1, 91  ) = [-5.1515, -0.1373]
  cl(0:1, 92  ) = [-4.5455, -0.0632]
  cl(0:1, 93  ) = [-3.9394, 0.0103]
  cl(0:1, 94  ) = [-3.3333, 0.0837]
  cl(0:1, 95  ) = [-2.7273, 0.1571]
  cl(0:1, 96  ) = [-2.1212, 0.2301]
  cl(0:1, 97  ) = [-1.5152, 0.3033]
  cl(0:1, 98  ) = [-0.9091, 0.376]
  cl(0:1, 99  ) = [-0.303, 0.4486]
  cl(0:1, 100  ) = [0.303, 0.5212]
  cl(0:1, 101  ) = [0.9091, 0.5935]
  cl(0:1, 102  ) = [1.5152, 0.6655]
  cl(0:1, 103  ) = [2.1212, 0.7374]
  cl(0:1, 104  ) = [2.7273, 0.809]
  cl(0:1, 105  ) = [3.3333, 0.8804]
  cl(0:1, 106  ) = [3.9394, 0.9514]
  cl(0:1, 107  ) = [4.5455, 1.0219]
  cl(0:1, 108  ) = [5.1515, 1.0919]
  cl(0:1, 109  ) = [5.7576, 1.1608]
  cl(0:1, 110  ) = [6.3636, 1.2272]
  cl(0:1, 111  ) = [6.9697, 1.288]
  cl(0:1, 112  ) = [7.5758, 1.3393]
  cl(0:1, 113  ) = [8.1818, 1.3868]
  cl(0:1, 114  ) = [8.7879, 1.4344]
  cl(0:1, 115  ) = [9.3939, 1.4807]
  cl(0:1, 116  ) = [10.0, 1.5162]
  cl(0:1, 117  ) = [10.6061, 1.5465]
  cl(0:1, 118  ) = [11.2121, 1.5702]
  cl(0:1, 119  ) = [11.8182, 1.5847]
  cl(0:1, 120  ) = [12.4242, 1.5544]
  cl(0:1, 121  ) = [13.0303, 1.507]
  cl(0:1, 122  ) = [13.6364, 1.4464]
  cl(0:1, 123  ) = [14.2424, 1.393]
  cl(0:1, 124  ) = [14.8485, 1.3506]
  cl(0:1, 125  ) = [15.4545, 1.3218]
  cl(0:1, 126  ) = [16.0606, 1.2976]
  cl(0:1, 127  ) = [16.6667, 1.2733]
  cl(0:1, 128  ) = [17.2727, 1.2436]
  cl(0:1, 129  ) = [17.8788, 1.2073]
  cl(0:1, 130  ) = [18.4848, 1.1899]
  cl(0:1, 131  ) = [19.0909, 1.177]
  cl(0:1, 132  ) = [19.697, 1.1625]
  cl(0:1, 133  ) = [20.303, 1.1493]
  cl(0:1, 134  ) = [20.9091, 1.1375]
  cl(0:1, 135  ) = [21.5152, 1.1258]
  cl(0:1, 136  ) = [22.1212, 1.1146]
  cl(0:1, 137  ) = [22.7273, 1.1056]
  cl(0:1, 138  ) = [23.3333, 1.0967]
  cl(0:1, 139  ) = [23.9394, 1.0877]
  cl(0:1, 140  ) = [24.5455, 1.0805]
  cl(0:1, 141  ) = [25.1515, 1.0735]
  cl(0:1, 142  ) = [25.7576, 1.0665]
  cl(0:1, 143  ) = [26.3636, 1.0603]
  cl(0:1, 144  ) = [26.9697, 1.0546]
  cl(0:1, 145  ) = [27.5758, 1.0489]
  cl(0:1, 146  ) = [28.1818, 1.0435]
  cl(0:1, 147  ) = [28.7879, 1.0387]
  cl(0:1, 148  ) = [29.3939, 1.0338]
  cl(0:1, 149  ) = [30.0, 1.029]
  cl(0:1, 150  ) = [33.0, 1.008]
  cl(0:1, 151  ) = [36.0, 0.9878]
  cl(0:1, 152  ) = [39.0, 0.9663]
  cl(0:1, 153  ) = [42.0, 0.9413]
  cl(0:1, 154  ) = [45.0, 0.915]
  cl(0:1, 155  ) = [48.0, 0.8816]
  cl(0:1, 156  ) = [51.0, 0.8439]
  cl(0:1, 157  ) = [54.0, 0.798]
  cl(0:1, 158  ) = [57.0, 0.7521]
  cl(0:1, 159  ) = [60.0, 0.7061]
  cl(0:1, 160  ) = [63.0, 0.6444]
  cl(0:1, 161  ) = [66.0, 0.5826]
  cl(0:1, 162  ) = [69.0, 0.5208]
  cl(0:1, 163  ) = [72.0, 0.4515]
  cl(0:1, 164  ) = [75.0, 0.3784]
  cl(0:1, 165  ) = [78.0, 0.3054]
  cl(0:1, 166  ) = [81.0, 0.231]
  cl(0:1, 167  ) = [84.0, 0.154]
  cl(0:1, 168  ) = [87.0, 0.077]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.0539]
  cl(0:1, 171  ) = [96.0, -0.1078]
  cl(0:1, 172  ) = [99.0, -0.1617]
  cl(0:1, 173  ) = [102.0, -0.2137]
  cl(0:1, 174  ) = [105.0, -0.2649]
  cl(0:1, 175  ) = [108.0, -0.316]
  cl(0:1, 176  ) = [111.0, -0.3645]
  cl(0:1, 177  ) = [114.0, -0.4078]
  cl(0:1, 178  ) = [117.0, -0.451]
  cl(0:1, 179  ) = [120.0, -0.4943]
  cl(0:1, 180  ) = [123.0, -0.5265]
  cl(0:1, 181  ) = [126.0, -0.5586]
  cl(0:1, 182  ) = [129.0, -0.5908]
  cl(0:1, 183  ) = [132.0, -0.6154]
  cl(0:1, 184  ) = [135.0, -0.6363]
  cl(0:1, 185  ) = [138.0, -0.6572]
  cl(0:1, 186  ) = [141.0, -0.6761]
  cl(0:1, 187  ) = [144.0, -0.6908]
  cl(0:1, 188  ) = [147.0, -0.7056]
  cl(0:1, 189  ) = [150.0, -0.7203]
  cl(0:1, 190  ) = [153.0, -0.7468]
  cl(0:1, 191  ) = [156.0, -0.7733]
  cl(0:1, 192  ) = [159.0, -0.7998]
  cl(0:1, 193  ) = [162.0, -0.7408]
  cl(0:1, 194  ) = [165.0, -0.639]
  cl(0:1, 195  ) = [168.0, -0.5373]
  cl(0:1, 196  ) = [171.0, -0.4225]
  cl(0:1, 197  ) = [174.0, -0.2817]
  cl(0:1, 198  ) = [177.0, -0.1402]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl27 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl27 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl27 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl27_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl27 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl27_nrel2p8.pro' 
  ENDIF

!print,y,cl27

END FUNCTION calc_cl27_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cl28_nrel2p8( y ) RESULT( cl28 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl28
INTEGER :: i

 cl28 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.1495]
  cl(0:1, 2  ) = [-174.0, 0.3003]
  cl(0:1, 3  ) = [-171.0, 0.4505]
  cl(0:1, 4  ) = [-168.0, 0.5621]
  cl(0:1, 5  ) = [-165.0, 0.6546]
  cl(0:1, 6  ) = [-162.0, 0.747]
  cl(0:1, 7  ) = [-159.0, 0.7998]
  cl(0:1, 8  ) = [-156.0, 0.7733]
  cl(0:1, 9  ) = [-153.0, 0.7468]
  cl(0:1, 10  ) = [-150.0, 0.7203]
  cl(0:1, 11  ) = [-147.0, 0.7056]
  cl(0:1, 12  ) = [-144.0, 0.6908]
  cl(0:1, 13  ) = [-141.0, 0.6761]
  cl(0:1, 14  ) = [-138.0, 0.6572]
  cl(0:1, 15  ) = [-135.0, 0.6363]
  cl(0:1, 16  ) = [-132.0, 0.6154]
  cl(0:1, 17  ) = [-129.0, 0.5908]
  cl(0:1, 18  ) = [-126.0, 0.5586]
  cl(0:1, 19  ) = [-123.0, 0.5265]
  cl(0:1, 20  ) = [-120.0, 0.4943]
  cl(0:1, 21  ) = [-117.0, 0.451]
  cl(0:1, 22  ) = [-114.0, 0.4078]
  cl(0:1, 23  ) = [-111.0, 0.3645]
  cl(0:1, 24  ) = [-108.0, 0.316]
  cl(0:1, 25  ) = [-105.0, 0.2649]
  cl(0:1, 26  ) = [-102.0, 0.2137]
  cl(0:1, 27  ) = [-99.0, 0.1617]
  cl(0:1, 28  ) = [-96.0, 0.1078]
  cl(0:1, 29  ) = [-93.0, 0.0539]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.0539]
  cl(0:1, 32  ) = [-84.0, -0.1078]
  cl(0:1, 33  ) = [-81.0, -0.1617]
  cl(0:1, 34  ) = [-78.0, -0.2137]
  cl(0:1, 35  ) = [-75.0, -0.2649]
  cl(0:1, 36  ) = [-72.0, -0.316]
  cl(0:1, 37  ) = [-69.0, -0.3645]
  cl(0:1, 38  ) = [-66.0, -0.4078]
  cl(0:1, 39  ) = [-63.0, -0.451]
  cl(0:1, 40  ) = [-60.0, -0.4943]
  cl(0:1, 41  ) = [-57.0, -0.5265]
  cl(0:1, 42  ) = [-54.0, -0.5586]
  cl(0:1, 43  ) = [-51.0, -0.5908]
  cl(0:1, 44  ) = [-48.0, -0.6171]
  cl(0:1, 45  ) = [-45.0, -0.6405]
  cl(0:1, 46  ) = [-42.0, -0.6589]
  cl(0:1, 47  ) = [-39.0, -0.6764]
  cl(0:1, 48  ) = [-36.0, -0.6914]
  cl(0:1, 49  ) = [-33.0, -0.7056]
  cl(0:1, 50  ) = [-30.0, -0.7203]
  cl(0:1, 51  ) = [-29.3939, -0.7237]
  cl(0:1, 52  ) = [-28.7879, -0.7271]
  cl(0:1, 53  ) = [-28.1818, -0.7304]
  cl(0:1, 54  ) = [-27.5758, -0.7342]
  cl(0:1, 55  ) = [-26.9697, -0.7382]
  cl(0:1, 56  ) = [-26.3636, -0.7422]
  cl(0:1, 57  ) = [-25.7576, -0.7466]
  cl(0:1, 58  ) = [-25.1515, -0.7515]
  cl(0:1, 59  ) = [-24.5455, -0.7564]
  cl(0:1, 60  ) = [-23.9394, -0.7614]
  cl(0:1, 61  ) = [-23.3333, -0.7677]
  cl(0:1, 62  ) = [-22.7273, -0.7739]
  cl(0:1, 63  ) = [-22.1212, -0.7802]
  cl(0:1, 64  ) = [-21.5152, -0.788]
  cl(0:1, 65  ) = [-20.9091, -0.7963]
  cl(0:1, 66  ) = [-20.303, -0.8045]
  cl(0:1, 67  ) = [-19.697, -0.8137]
  cl(0:1, 68  ) = [-19.0909, -0.8239]
  cl(0:1, 69  ) = [-18.4848, -0.8226]
  cl(0:1, 70  ) = [-17.8788, -0.8192]
  cl(0:1, 71  ) = [-17.2727, -0.8154]
  cl(0:1, 72  ) = [-16.6667, -0.8033]
  cl(0:1, 73  ) = [-16.0606, -0.7843]
  cl(0:1, 74  ) = [-15.4545, -0.7653]
  cl(0:1, 75  ) = [-14.8485, -0.7463]
  cl(0:1, 76  ) = [-14.2424, -0.7274]
  cl(0:1, 77  ) = [-13.6364, -0.7084]
  cl(0:1, 78  ) = [-13.0303, -0.6894]
  cl(0:1, 79  ) = [-12.4242, -0.6704]
  cl(0:1, 80  ) = [-11.8182, -0.6514]
  cl(0:1, 81  ) = [-11.2121, -0.6324]
  cl(0:1, 82  ) = [-10.6061, -0.6134]
  cl(0:1, 83  ) = [-10.0, -0.5944]
  cl(0:1, 84  ) = [-9.3939, -0.5755]
  cl(0:1, 85  ) = [-8.7879, -0.5514]
  cl(0:1, 86  ) = [-8.1818, -0.5179]
  cl(0:1, 87  ) = [-7.5758, -0.448]
  cl(0:1, 88  ) = [-6.9697, -0.3623]
  cl(0:1, 89  ) = [-6.3636, -0.2866]
  cl(0:1, 90  ) = [-5.7576, -0.2117]
  cl(0:1, 91  ) = [-5.1515, -0.1373]
  cl(0:1, 92  ) = [-4.5455, -0.0632]
  cl(0:1, 93  ) = [-3.9394, 0.0103]
  cl(0:1, 94  ) = [-3.3333, 0.0837]
  cl(0:1, 95  ) = [-2.7273, 0.1571]
  cl(0:1, 96  ) = [-2.1212, 0.2301]
  cl(0:1, 97  ) = [-1.5152, 0.3033]
  cl(0:1, 98  ) = [-0.9091, 0.376]
  cl(0:1, 99  ) = [-0.303, 0.4486]
  cl(0:1, 100  ) = [0.303, 0.5212]
  cl(0:1, 101  ) = [0.9091, 0.5935]
  cl(0:1, 102  ) = [1.5152, 0.6655]
  cl(0:1, 103  ) = [2.1212, 0.7374]
  cl(0:1, 104  ) = [2.7273, 0.809]
  cl(0:1, 105  ) = [3.3333, 0.8804]
  cl(0:1, 106  ) = [3.9394, 0.9514]
  cl(0:1, 107  ) = [4.5455, 1.0219]
  cl(0:1, 108  ) = [5.1515, 1.0919]
  cl(0:1, 109  ) = [5.7576, 1.1608]
  cl(0:1, 110  ) = [6.3636, 1.2272]
  cl(0:1, 111  ) = [6.9697, 1.288]
  cl(0:1, 112  ) = [7.5758, 1.3393]
  cl(0:1, 113  ) = [8.1818, 1.3868]
  cl(0:1, 114  ) = [8.7879, 1.4344]
  cl(0:1, 115  ) = [9.3939, 1.4807]
  cl(0:1, 116  ) = [10.0, 1.5162]
  cl(0:1, 117  ) = [10.6061, 1.5465]
  cl(0:1, 118  ) = [11.2121, 1.5702]
  cl(0:1, 119  ) = [11.8182, 1.5847]
  cl(0:1, 120  ) = [12.4242, 1.5544]
  cl(0:1, 121  ) = [13.0303, 1.507]
  cl(0:1, 122  ) = [13.6364, 1.4464]
  cl(0:1, 123  ) = [14.2424, 1.393]
  cl(0:1, 124  ) = [14.8485, 1.3506]
  cl(0:1, 125  ) = [15.4545, 1.3218]
  cl(0:1, 126  ) = [16.0606, 1.2976]
  cl(0:1, 127  ) = [16.6667, 1.2733]
  cl(0:1, 128  ) = [17.2727, 1.2436]
  cl(0:1, 129  ) = [17.8788, 1.2073]
  cl(0:1, 130  ) = [18.4848, 1.1899]
  cl(0:1, 131  ) = [19.0909, 1.177]
  cl(0:1, 132  ) = [19.697, 1.1625]
  cl(0:1, 133  ) = [20.303, 1.1493]
  cl(0:1, 134  ) = [20.9091, 1.1375]
  cl(0:1, 135  ) = [21.5152, 1.1258]
  cl(0:1, 136  ) = [22.1212, 1.1146]
  cl(0:1, 137  ) = [22.7273, 1.1056]
  cl(0:1, 138  ) = [23.3333, 1.0967]
  cl(0:1, 139  ) = [23.9394, 1.0877]
  cl(0:1, 140  ) = [24.5455, 1.0805]
  cl(0:1, 141  ) = [25.1515, 1.0735]
  cl(0:1, 142  ) = [25.7576, 1.0665]
  cl(0:1, 143  ) = [26.3636, 1.0603]
  cl(0:1, 144  ) = [26.9697, 1.0546]
  cl(0:1, 145  ) = [27.5758, 1.0489]
  cl(0:1, 146  ) = [28.1818, 1.0435]
  cl(0:1, 147  ) = [28.7879, 1.0387]
  cl(0:1, 148  ) = [29.3939, 1.0338]
  cl(0:1, 149  ) = [30.0, 1.029]
  cl(0:1, 150  ) = [33.0, 1.008]
  cl(0:1, 151  ) = [36.0, 0.9878]
  cl(0:1, 152  ) = [39.0, 0.9663]
  cl(0:1, 153  ) = [42.0, 0.9413]
  cl(0:1, 154  ) = [45.0, 0.915]
  cl(0:1, 155  ) = [48.0, 0.8816]
  cl(0:1, 156  ) = [51.0, 0.8439]
  cl(0:1, 157  ) = [54.0, 0.798]
  cl(0:1, 158  ) = [57.0, 0.7521]
  cl(0:1, 159  ) = [60.0, 0.7061]
  cl(0:1, 160  ) = [63.0, 0.6444]
  cl(0:1, 161  ) = [66.0, 0.5826]
  cl(0:1, 162  ) = [69.0, 0.5208]
  cl(0:1, 163  ) = [72.0, 0.4515]
  cl(0:1, 164  ) = [75.0, 0.3784]
  cl(0:1, 165  ) = [78.0, 0.3054]
  cl(0:1, 166  ) = [81.0, 0.231]
  cl(0:1, 167  ) = [84.0, 0.154]
  cl(0:1, 168  ) = [87.0, 0.077]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.0539]
  cl(0:1, 171  ) = [96.0, -0.1078]
  cl(0:1, 172  ) = [99.0, -0.1617]
  cl(0:1, 173  ) = [102.0, -0.2137]
  cl(0:1, 174  ) = [105.0, -0.2649]
  cl(0:1, 175  ) = [108.0, -0.316]
  cl(0:1, 176  ) = [111.0, -0.3645]
  cl(0:1, 177  ) = [114.0, -0.4078]
  cl(0:1, 178  ) = [117.0, -0.451]
  cl(0:1, 179  ) = [120.0, -0.4943]
  cl(0:1, 180  ) = [123.0, -0.5265]
  cl(0:1, 181  ) = [126.0, -0.5586]
  cl(0:1, 182  ) = [129.0, -0.5908]
  cl(0:1, 183  ) = [132.0, -0.6154]
  cl(0:1, 184  ) = [135.0, -0.6363]
  cl(0:1, 185  ) = [138.0, -0.6572]
  cl(0:1, 186  ) = [141.0, -0.6761]
  cl(0:1, 187  ) = [144.0, -0.6908]
  cl(0:1, 188  ) = [147.0, -0.7056]
  cl(0:1, 189  ) = [150.0, -0.7203]
  cl(0:1, 190  ) = [153.0, -0.7468]
  cl(0:1, 191  ) = [156.0, -0.7733]
  cl(0:1, 192  ) = [159.0, -0.7998]
  cl(0:1, 193  ) = [162.0, -0.7408]
  cl(0:1, 194  ) = [165.0, -0.639]
  cl(0:1, 195  ) = [168.0, -0.5373]
  cl(0:1, 196  ) = [171.0, -0.4225]
  cl(0:1, 197  ) = [174.0, -0.2817]
  cl(0:1, 198  ) = [177.0, -0.1402]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl28 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl28 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl28 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl28_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl28 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl28_nrel2p8.pro' 
  ENDIF

!print,y,cl28

END FUNCTION calc_cl28_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cl29_nrel2p8( y ) RESULT( cl29 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl29
INTEGER :: i

 cl29 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.1495]
  cl(0:1, 2  ) = [-174.0, 0.3003]
  cl(0:1, 3  ) = [-171.0, 0.4505]
  cl(0:1, 4  ) = [-168.0, 0.5621]
  cl(0:1, 5  ) = [-165.0, 0.6546]
  cl(0:1, 6  ) = [-162.0, 0.747]
  cl(0:1, 7  ) = [-159.0, 0.7998]
  cl(0:1, 8  ) = [-156.0, 0.7733]
  cl(0:1, 9  ) = [-153.0, 0.7468]
  cl(0:1, 10  ) = [-150.0, 0.7203]
  cl(0:1, 11  ) = [-147.0, 0.7056]
  cl(0:1, 12  ) = [-144.0, 0.6908]
  cl(0:1, 13  ) = [-141.0, 0.6761]
  cl(0:1, 14  ) = [-138.0, 0.6572]
  cl(0:1, 15  ) = [-135.0, 0.6363]
  cl(0:1, 16  ) = [-132.0, 0.6154]
  cl(0:1, 17  ) = [-129.0, 0.5908]
  cl(0:1, 18  ) = [-126.0, 0.5586]
  cl(0:1, 19  ) = [-123.0, 0.5265]
  cl(0:1, 20  ) = [-120.0, 0.4943]
  cl(0:1, 21  ) = [-117.0, 0.451]
  cl(0:1, 22  ) = [-114.0, 0.4078]
  cl(0:1, 23  ) = [-111.0, 0.3645]
  cl(0:1, 24  ) = [-108.0, 0.316]
  cl(0:1, 25  ) = [-105.0, 0.2649]
  cl(0:1, 26  ) = [-102.0, 0.2137]
  cl(0:1, 27  ) = [-99.0, 0.1617]
  cl(0:1, 28  ) = [-96.0, 0.1078]
  cl(0:1, 29  ) = [-93.0, 0.0539]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.0539]
  cl(0:1, 32  ) = [-84.0, -0.1078]
  cl(0:1, 33  ) = [-81.0, -0.1617]
  cl(0:1, 34  ) = [-78.0, -0.2137]
  cl(0:1, 35  ) = [-75.0, -0.2649]
  cl(0:1, 36  ) = [-72.0, -0.316]
  cl(0:1, 37  ) = [-69.0, -0.3645]
  cl(0:1, 38  ) = [-66.0, -0.4078]
  cl(0:1, 39  ) = [-63.0, -0.451]
  cl(0:1, 40  ) = [-60.0, -0.4943]
  cl(0:1, 41  ) = [-57.0, -0.5265]
  cl(0:1, 42  ) = [-54.0, -0.5586]
  cl(0:1, 43  ) = [-51.0, -0.5908]
  cl(0:1, 44  ) = [-48.0, -0.6171]
  cl(0:1, 45  ) = [-45.0, -0.6405]
  cl(0:1, 46  ) = [-42.0, -0.6589]
  cl(0:1, 47  ) = [-39.0, -0.6764]
  cl(0:1, 48  ) = [-36.0, -0.6914]
  cl(0:1, 49  ) = [-33.0, -0.7056]
  cl(0:1, 50  ) = [-30.0, -0.7203]
  cl(0:1, 51  ) = [-29.3939, -0.7237]
  cl(0:1, 52  ) = [-28.7879, -0.7271]
  cl(0:1, 53  ) = [-28.1818, -0.7304]
  cl(0:1, 54  ) = [-27.5758, -0.7342]
  cl(0:1, 55  ) = [-26.9697, -0.7382]
  cl(0:1, 56  ) = [-26.3636, -0.7422]
  cl(0:1, 57  ) = [-25.7576, -0.7466]
  cl(0:1, 58  ) = [-25.1515, -0.7515]
  cl(0:1, 59  ) = [-24.5455, -0.7564]
  cl(0:1, 60  ) = [-23.9394, -0.7614]
  cl(0:1, 61  ) = [-23.3333, -0.7677]
  cl(0:1, 62  ) = [-22.7273, -0.7739]
  cl(0:1, 63  ) = [-22.1212, -0.7802]
  cl(0:1, 64  ) = [-21.5152, -0.788]
  cl(0:1, 65  ) = [-20.9091, -0.7963]
  cl(0:1, 66  ) = [-20.303, -0.8045]
  cl(0:1, 67  ) = [-19.697, -0.8137]
  cl(0:1, 68  ) = [-19.0909, -0.8239]
  cl(0:1, 69  ) = [-18.4848, -0.8226]
  cl(0:1, 70  ) = [-17.8788, -0.8192]
  cl(0:1, 71  ) = [-17.2727, -0.8154]
  cl(0:1, 72  ) = [-16.6667, -0.8033]
  cl(0:1, 73  ) = [-16.0606, -0.7843]
  cl(0:1, 74  ) = [-15.4545, -0.7653]
  cl(0:1, 75  ) = [-14.8485, -0.7463]
  cl(0:1, 76  ) = [-14.2424, -0.7274]
  cl(0:1, 77  ) = [-13.6364, -0.7084]
  cl(0:1, 78  ) = [-13.0303, -0.6894]
  cl(0:1, 79  ) = [-12.4242, -0.6704]
  cl(0:1, 80  ) = [-11.8182, -0.6514]
  cl(0:1, 81  ) = [-11.2121, -0.6324]
  cl(0:1, 82  ) = [-10.6061, -0.6134]
  cl(0:1, 83  ) = [-10.0, -0.5944]
  cl(0:1, 84  ) = [-9.3939, -0.5755]
  cl(0:1, 85  ) = [-8.7879, -0.5514]
  cl(0:1, 86  ) = [-8.1818, -0.5179]
  cl(0:1, 87  ) = [-7.5758, -0.448]
  cl(0:1, 88  ) = [-6.9697, -0.3623]
  cl(0:1, 89  ) = [-6.3636, -0.2866]
  cl(0:1, 90  ) = [-5.7576, -0.2117]
  cl(0:1, 91  ) = [-5.1515, -0.1373]
  cl(0:1, 92  ) = [-4.5455, -0.0632]
  cl(0:1, 93  ) = [-3.9394, 0.0103]
  cl(0:1, 94  ) = [-3.3333, 0.0837]
  cl(0:1, 95  ) = [-2.7273, 0.1571]
  cl(0:1, 96  ) = [-2.1212, 0.2301]
  cl(0:1, 97  ) = [-1.5152, 0.3033]
  cl(0:1, 98  ) = [-0.9091, 0.376]
  cl(0:1, 99  ) = [-0.303, 0.4486]
  cl(0:1, 100  ) = [0.303, 0.5212]
  cl(0:1, 101  ) = [0.9091, 0.5935]
  cl(0:1, 102  ) = [1.5152, 0.6655]
  cl(0:1, 103  ) = [2.1212, 0.7374]
  cl(0:1, 104  ) = [2.7273, 0.809]
  cl(0:1, 105  ) = [3.3333, 0.8804]
  cl(0:1, 106  ) = [3.9394, 0.9514]
  cl(0:1, 107  ) = [4.5455, 1.0219]
  cl(0:1, 108  ) = [5.1515, 1.0919]
  cl(0:1, 109  ) = [5.7576, 1.1608]
  cl(0:1, 110  ) = [6.3636, 1.2272]
  cl(0:1, 111  ) = [6.9697, 1.288]
  cl(0:1, 112  ) = [7.5758, 1.3393]
  cl(0:1, 113  ) = [8.1818, 1.3868]
  cl(0:1, 114  ) = [8.7879, 1.4344]
  cl(0:1, 115  ) = [9.3939, 1.4807]
  cl(0:1, 116  ) = [10.0, 1.5162]
  cl(0:1, 117  ) = [10.6061, 1.5465]
  cl(0:1, 118  ) = [11.2121, 1.5702]
  cl(0:1, 119  ) = [11.8182, 1.5847]
  cl(0:1, 120  ) = [12.4242, 1.5544]
  cl(0:1, 121  ) = [13.0303, 1.507]
  cl(0:1, 122  ) = [13.6364, 1.4464]
  cl(0:1, 123  ) = [14.2424, 1.393]
  cl(0:1, 124  ) = [14.8485, 1.3506]
  cl(0:1, 125  ) = [15.4545, 1.3218]
  cl(0:1, 126  ) = [16.0606, 1.2976]
  cl(0:1, 127  ) = [16.6667, 1.2733]
  cl(0:1, 128  ) = [17.2727, 1.2436]
  cl(0:1, 129  ) = [17.8788, 1.2073]
  cl(0:1, 130  ) = [18.4848, 1.1899]
  cl(0:1, 131  ) = [19.0909, 1.177]
  cl(0:1, 132  ) = [19.697, 1.1625]
  cl(0:1, 133  ) = [20.303, 1.1493]
  cl(0:1, 134  ) = [20.9091, 1.1375]
  cl(0:1, 135  ) = [21.5152, 1.1258]
  cl(0:1, 136  ) = [22.1212, 1.1146]
  cl(0:1, 137  ) = [22.7273, 1.1056]
  cl(0:1, 138  ) = [23.3333, 1.0967]
  cl(0:1, 139  ) = [23.9394, 1.0877]
  cl(0:1, 140  ) = [24.5455, 1.0805]
  cl(0:1, 141  ) = [25.1515, 1.0735]
  cl(0:1, 142  ) = [25.7576, 1.0665]
  cl(0:1, 143  ) = [26.3636, 1.0603]
  cl(0:1, 144  ) = [26.9697, 1.0546]
  cl(0:1, 145  ) = [27.5758, 1.0489]
  cl(0:1, 146  ) = [28.1818, 1.0435]
  cl(0:1, 147  ) = [28.7879, 1.0387]
  cl(0:1, 148  ) = [29.3939, 1.0338]
  cl(0:1, 149  ) = [30.0, 1.029]
  cl(0:1, 150  ) = [33.0, 1.008]
  cl(0:1, 151  ) = [36.0, 0.9878]
  cl(0:1, 152  ) = [39.0, 0.9663]
  cl(0:1, 153  ) = [42.0, 0.9413]
  cl(0:1, 154  ) = [45.0, 0.915]
  cl(0:1, 155  ) = [48.0, 0.8816]
  cl(0:1, 156  ) = [51.0, 0.8439]
  cl(0:1, 157  ) = [54.0, 0.798]
  cl(0:1, 158  ) = [57.0, 0.7521]
  cl(0:1, 159  ) = [60.0, 0.7061]
  cl(0:1, 160  ) = [63.0, 0.6444]
  cl(0:1, 161  ) = [66.0, 0.5826]
  cl(0:1, 162  ) = [69.0, 0.5208]
  cl(0:1, 163  ) = [72.0, 0.4515]
  cl(0:1, 164  ) = [75.0, 0.3784]
  cl(0:1, 165  ) = [78.0, 0.3054]
  cl(0:1, 166  ) = [81.0, 0.231]
  cl(0:1, 167  ) = [84.0, 0.154]
  cl(0:1, 168  ) = [87.0, 0.077]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.0539]
  cl(0:1, 171  ) = [96.0, -0.1078]
  cl(0:1, 172  ) = [99.0, -0.1617]
  cl(0:1, 173  ) = [102.0, -0.2137]
  cl(0:1, 174  ) = [105.0, -0.2649]
  cl(0:1, 175  ) = [108.0, -0.316]
  cl(0:1, 176  ) = [111.0, -0.3645]
  cl(0:1, 177  ) = [114.0, -0.4078]
  cl(0:1, 178  ) = [117.0, -0.451]
  cl(0:1, 179  ) = [120.0, -0.4943]
  cl(0:1, 180  ) = [123.0, -0.5265]
  cl(0:1, 181  ) = [126.0, -0.5586]
  cl(0:1, 182  ) = [129.0, -0.5908]
  cl(0:1, 183  ) = [132.0, -0.6154]
  cl(0:1, 184  ) = [135.0, -0.6363]
  cl(0:1, 185  ) = [138.0, -0.6572]
  cl(0:1, 186  ) = [141.0, -0.6761]
  cl(0:1, 187  ) = [144.0, -0.6908]
  cl(0:1, 188  ) = [147.0, -0.7056]
  cl(0:1, 189  ) = [150.0, -0.7203]
  cl(0:1, 190  ) = [153.0, -0.7468]
  cl(0:1, 191  ) = [156.0, -0.7733]
  cl(0:1, 192  ) = [159.0, -0.7998]
  cl(0:1, 193  ) = [162.0, -0.7408]
  cl(0:1, 194  ) = [165.0, -0.639]
  cl(0:1, 195  ) = [168.0, -0.5373]
  cl(0:1, 196  ) = [171.0, -0.4225]
  cl(0:1, 197  ) = [174.0, -0.2817]
  cl(0:1, 198  ) = [177.0, -0.1402]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl29 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl29 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl29 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl29_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl29 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl29_nrel2p8.pro' 
  ENDIF

!print,y,cl29

END FUNCTION calc_cl29_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cl30_nrel2p8( y ) RESULT( cl30 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cl

REAL :: cl30
INTEGER :: i

 cl30 = 0.0

!                alpha    CL  
  cl(0:1, 0  ) = [-180.0, 0.0]
  cl(0:1, 1  ) = [-177.0, 0.1495]
  cl(0:1, 2  ) = [-174.0, 0.3003]
  cl(0:1, 3  ) = [-171.0, 0.4505]
  cl(0:1, 4  ) = [-168.0, 0.5621]
  cl(0:1, 5  ) = [-165.0, 0.6546]
  cl(0:1, 6  ) = [-162.0, 0.747]
  cl(0:1, 7  ) = [-159.0, 0.7998]
  cl(0:1, 8  ) = [-156.0, 0.7733]
  cl(0:1, 9  ) = [-153.0, 0.7468]
  cl(0:1, 10  ) = [-150.0, 0.7203]
  cl(0:1, 11  ) = [-147.0, 0.7056]
  cl(0:1, 12  ) = [-144.0, 0.6908]
  cl(0:1, 13  ) = [-141.0, 0.6761]
  cl(0:1, 14  ) = [-138.0, 0.6572]
  cl(0:1, 15  ) = [-135.0, 0.6363]
  cl(0:1, 16  ) = [-132.0, 0.6154]
  cl(0:1, 17  ) = [-129.0, 0.5908]
  cl(0:1, 18  ) = [-126.0, 0.5586]
  cl(0:1, 19  ) = [-123.0, 0.5265]
  cl(0:1, 20  ) = [-120.0, 0.4943]
  cl(0:1, 21  ) = [-117.0, 0.451]
  cl(0:1, 22  ) = [-114.0, 0.4078]
  cl(0:1, 23  ) = [-111.0, 0.3645]
  cl(0:1, 24  ) = [-108.0, 0.316]
  cl(0:1, 25  ) = [-105.0, 0.2649]
  cl(0:1, 26  ) = [-102.0, 0.2137]
  cl(0:1, 27  ) = [-99.0, 0.1617]
  cl(0:1, 28  ) = [-96.0, 0.1078]
  cl(0:1, 29  ) = [-93.0, 0.0539]
  cl(0:1, 30  ) = [-90.0, 0.0]
  cl(0:1, 31  ) = [-87.0, -0.0539]
  cl(0:1, 32  ) = [-84.0, -0.1078]
  cl(0:1, 33  ) = [-81.0, -0.1617]
  cl(0:1, 34  ) = [-78.0, -0.2137]
  cl(0:1, 35  ) = [-75.0, -0.2649]
  cl(0:1, 36  ) = [-72.0, -0.316]
  cl(0:1, 37  ) = [-69.0, -0.3645]
  cl(0:1, 38  ) = [-66.0, -0.4078]
  cl(0:1, 39  ) = [-63.0, -0.451]
  cl(0:1, 40  ) = [-60.0, -0.4943]
  cl(0:1, 41  ) = [-57.0, -0.5265]
  cl(0:1, 42  ) = [-54.0, -0.5586]
  cl(0:1, 43  ) = [-51.0, -0.5908]
  cl(0:1, 44  ) = [-48.0, -0.6171]
  cl(0:1, 45  ) = [-45.0, -0.6405]
  cl(0:1, 46  ) = [-42.0, -0.6589]
  cl(0:1, 47  ) = [-39.0, -0.6764]
  cl(0:1, 48  ) = [-36.0, -0.6914]
  cl(0:1, 49  ) = [-33.0, -0.7056]
  cl(0:1, 50  ) = [-30.0, -0.7203]
  cl(0:1, 51  ) = [-29.3939, -0.7237]
  cl(0:1, 52  ) = [-28.7879, -0.7271]
  cl(0:1, 53  ) = [-28.1818, -0.7304]
  cl(0:1, 54  ) = [-27.5758, -0.7342]
  cl(0:1, 55  ) = [-26.9697, -0.7382]
  cl(0:1, 56  ) = [-26.3636, -0.7422]
  cl(0:1, 57  ) = [-25.7576, -0.7466]
  cl(0:1, 58  ) = [-25.1515, -0.7515]
  cl(0:1, 59  ) = [-24.5455, -0.7564]
  cl(0:1, 60  ) = [-23.9394, -0.7614]
  cl(0:1, 61  ) = [-23.3333, -0.7677]
  cl(0:1, 62  ) = [-22.7273, -0.7739]
  cl(0:1, 63  ) = [-22.1212, -0.7802]
  cl(0:1, 64  ) = [-21.5152, -0.788]
  cl(0:1, 65  ) = [-20.9091, -0.7963]
  cl(0:1, 66  ) = [-20.303, -0.8045]
  cl(0:1, 67  ) = [-19.697, -0.8137]
  cl(0:1, 68  ) = [-19.0909, -0.8239]
  cl(0:1, 69  ) = [-18.4848, -0.8226]
  cl(0:1, 70  ) = [-17.8788, -0.8192]
  cl(0:1, 71  ) = [-17.2727, -0.8154]
  cl(0:1, 72  ) = [-16.6667, -0.8033]
  cl(0:1, 73  ) = [-16.0606, -0.7843]
  cl(0:1, 74  ) = [-15.4545, -0.7653]
  cl(0:1, 75  ) = [-14.8485, -0.7463]
  cl(0:1, 76  ) = [-14.2424, -0.7274]
  cl(0:1, 77  ) = [-13.6364, -0.7084]
  cl(0:1, 78  ) = [-13.0303, -0.6894]
  cl(0:1, 79  ) = [-12.4242, -0.6704]
  cl(0:1, 80  ) = [-11.8182, -0.6514]
  cl(0:1, 81  ) = [-11.2121, -0.6324]
  cl(0:1, 82  ) = [-10.6061, -0.6134]
  cl(0:1, 83  ) = [-10.0, -0.5944]
  cl(0:1, 84  ) = [-9.3939, -0.5755]
  cl(0:1, 85  ) = [-8.7879, -0.5514]
  cl(0:1, 86  ) = [-8.1818, -0.5179]
  cl(0:1, 87  ) = [-7.5758, -0.448]
  cl(0:1, 88  ) = [-6.9697, -0.3623]
  cl(0:1, 89  ) = [-6.3636, -0.2866]
  cl(0:1, 90  ) = [-5.7576, -0.2117]
  cl(0:1, 91  ) = [-5.1515, -0.1373]
  cl(0:1, 92  ) = [-4.5455, -0.0632]
  cl(0:1, 93  ) = [-3.9394, 0.0103]
  cl(0:1, 94  ) = [-3.3333, 0.0837]
  cl(0:1, 95  ) = [-2.7273, 0.1571]
  cl(0:1, 96  ) = [-2.1212, 0.2301]
  cl(0:1, 97  ) = [-1.5152, 0.3033]
  cl(0:1, 98  ) = [-0.9091, 0.376]
  cl(0:1, 99  ) = [-0.303, 0.4486]
  cl(0:1, 100  ) = [0.303, 0.5212]
  cl(0:1, 101  ) = [0.9091, 0.5935]
  cl(0:1, 102  ) = [1.5152, 0.6655]
  cl(0:1, 103  ) = [2.1212, 0.7374]
  cl(0:1, 104  ) = [2.7273, 0.809]
  cl(0:1, 105  ) = [3.3333, 0.8804]
  cl(0:1, 106  ) = [3.9394, 0.9514]
  cl(0:1, 107  ) = [4.5455, 1.0219]
  cl(0:1, 108  ) = [5.1515, 1.0919]
  cl(0:1, 109  ) = [5.7576, 1.1608]
  cl(0:1, 110  ) = [6.3636, 1.2272]
  cl(0:1, 111  ) = [6.9697, 1.288]
  cl(0:1, 112  ) = [7.5758, 1.3393]
  cl(0:1, 113  ) = [8.1818, 1.3868]
  cl(0:1, 114  ) = [8.7879, 1.4344]
  cl(0:1, 115  ) = [9.3939, 1.4807]
  cl(0:1, 116  ) = [10.0, 1.5162]
  cl(0:1, 117  ) = [10.6061, 1.5465]
  cl(0:1, 118  ) = [11.2121, 1.5702]
  cl(0:1, 119  ) = [11.8182, 1.5847]
  cl(0:1, 120  ) = [12.4242, 1.5544]
  cl(0:1, 121  ) = [13.0303, 1.507]
  cl(0:1, 122  ) = [13.6364, 1.4464]
  cl(0:1, 123  ) = [14.2424, 1.393]
  cl(0:1, 124  ) = [14.8485, 1.3506]
  cl(0:1, 125  ) = [15.4545, 1.3218]
  cl(0:1, 126  ) = [16.0606, 1.2976]
  cl(0:1, 127  ) = [16.6667, 1.2733]
  cl(0:1, 128  ) = [17.2727, 1.2436]
  cl(0:1, 129  ) = [17.8788, 1.2073]
  cl(0:1, 130  ) = [18.4848, 1.1899]
  cl(0:1, 131  ) = [19.0909, 1.177]
  cl(0:1, 132  ) = [19.697, 1.1625]
  cl(0:1, 133  ) = [20.303, 1.1493]
  cl(0:1, 134  ) = [20.9091, 1.1375]
  cl(0:1, 135  ) = [21.5152, 1.1258]
  cl(0:1, 136  ) = [22.1212, 1.1146]
  cl(0:1, 137  ) = [22.7273, 1.1056]
  cl(0:1, 138  ) = [23.3333, 1.0967]
  cl(0:1, 139  ) = [23.9394, 1.0877]
  cl(0:1, 140  ) = [24.5455, 1.0805]
  cl(0:1, 141  ) = [25.1515, 1.0735]
  cl(0:1, 142  ) = [25.7576, 1.0665]
  cl(0:1, 143  ) = [26.3636, 1.0603]
  cl(0:1, 144  ) = [26.9697, 1.0546]
  cl(0:1, 145  ) = [27.5758, 1.0489]
  cl(0:1, 146  ) = [28.1818, 1.0435]
  cl(0:1, 147  ) = [28.7879, 1.0387]
  cl(0:1, 148  ) = [29.3939, 1.0338]
  cl(0:1, 149  ) = [30.0, 1.029]
  cl(0:1, 150  ) = [33.0, 1.008]
  cl(0:1, 151  ) = [36.0, 0.9878]
  cl(0:1, 152  ) = [39.0, 0.9663]
  cl(0:1, 153  ) = [42.0, 0.9413]
  cl(0:1, 154  ) = [45.0, 0.915]
  cl(0:1, 155  ) = [48.0, 0.8816]
  cl(0:1, 156  ) = [51.0, 0.8439]
  cl(0:1, 157  ) = [54.0, 0.798]
  cl(0:1, 158  ) = [57.0, 0.7521]
  cl(0:1, 159  ) = [60.0, 0.7061]
  cl(0:1, 160  ) = [63.0, 0.6444]
  cl(0:1, 161  ) = [66.0, 0.5826]
  cl(0:1, 162  ) = [69.0, 0.5208]
  cl(0:1, 163  ) = [72.0, 0.4515]
  cl(0:1, 164  ) = [75.0, 0.3784]
  cl(0:1, 165  ) = [78.0, 0.3054]
  cl(0:1, 166  ) = [81.0, 0.231]
  cl(0:1, 167  ) = [84.0, 0.154]
  cl(0:1, 168  ) = [87.0, 0.077]
  cl(0:1, 169  ) = [90.0, -0.0]
  cl(0:1, 170  ) = [93.0, -0.0539]
  cl(0:1, 171  ) = [96.0, -0.1078]
  cl(0:1, 172  ) = [99.0, -0.1617]
  cl(0:1, 173  ) = [102.0, -0.2137]
  cl(0:1, 174  ) = [105.0, -0.2649]
  cl(0:1, 175  ) = [108.0, -0.316]
  cl(0:1, 176  ) = [111.0, -0.3645]
  cl(0:1, 177  ) = [114.0, -0.4078]
  cl(0:1, 178  ) = [117.0, -0.451]
  cl(0:1, 179  ) = [120.0, -0.4943]
  cl(0:1, 180  ) = [123.0, -0.5265]
  cl(0:1, 181  ) = [126.0, -0.5586]
  cl(0:1, 182  ) = [129.0, -0.5908]
  cl(0:1, 183  ) = [132.0, -0.6154]
  cl(0:1, 184  ) = [135.0, -0.6363]
  cl(0:1, 185  ) = [138.0, -0.6572]
  cl(0:1, 186  ) = [141.0, -0.6761]
  cl(0:1, 187  ) = [144.0, -0.6908]
  cl(0:1, 188  ) = [147.0, -0.7056]
  cl(0:1, 189  ) = [150.0, -0.7203]
  cl(0:1, 190  ) = [153.0, -0.7468]
  cl(0:1, 191  ) = [156.0, -0.7733]
  cl(0:1, 192  ) = [159.0, -0.7998]
  cl(0:1, 193  ) = [162.0, -0.7408]
  cl(0:1, 194  ) = [165.0, -0.639]
  cl(0:1, 195  ) = [168.0, -0.5373]
  cl(0:1, 196  ) = [171.0, -0.4225]
  cl(0:1, 197  ) = [174.0, -0.2817]
  cl(0:1, 198  ) = [177.0, -0.1402]
  cl(0:1, 199  ) = [180.0, 0.0]

  IF ( y .EQ. cl(0,0) ) cl30 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl30 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN 
     cl30 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl30_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl30 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl30_nrel2p8.pro' 
  ENDIF

!print,y,cl30

END FUNCTION calc_cl30_nrel2p8
!_________________________________________________________________________







!=========================================================================

FUNCTION calc_cd_nrel2p8( x,y ) RESULT( cd )
!_________________________________________________________________________
!
! Calls appropriate airfoil section table depending upon radial distance
! from blade root. Data from NREL 2.8 MW
!
! calc_cd1_nrel2p8 uses data from Cylinder1.dat
! calc_cd2_nrel2p8 uses data from Cylinder2.dat
! calc_cd3_nrel2p8 uses data from DU40_A17.dat
! calc_cd4_nrel2p8 uses data from DU35_A17.dat
! calc_cd5_nrel2p8 uses data from DU30_A17.dat
! calc_cd6_nrel2p8 uses data from DU25_A17.dat
! calc_cd7_nrel2p8 uses data from DU21_A17.dat
! calc_cd8_nrel2p8 uses data from NACA64_A17.dat
!
! x = radial distance from hub center
! y = angle of attack
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: x,y

  INTEGER, PARAMETER :: ns = 30

  REAL, DIMENSION( 0:ns-1 ) :: r

  REAL :: cd
  INTEGER :: i

  cd = 0.0

  !print*,'FUNCTION calc_cd_nrel2p8'

  r(0) = 2.043
  r(1) = 4.162
  r(2) = 6.282
  r(3) = 8.401
  r(4) = 10.52
  r(5) = 12.639
  r(6) = 14.758
  r(7) = 16.878
  r(8) = 18.997
  r(9) = 21.116
  r(10) = 23.235
  r(11) = 25.354
  r(12) = 27.474
  r(13) = 29.593
  r(14) = 31.712
  r(15) = 33.831
  r(16) = 35.95
  r(17) = 38.07
  r(18) = 40.189
  r(19) = 42.308
  r(20) = 44.427
  r(21) = 46.546
  r(22) = 48.666
  r(23) = 50.785
  r(24) = 52.904
  r(25) = 55.023
  r(26) = 57.142
  r(27) = 59.262
  r(28) = 61.381
  r(29) = 63.5

     IF ( ( x .GT. r( 0) ) .AND. ( x .LE. r( 1) ) ) THEN
       cd = calc_cd1_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 1) ) .AND. ( x .LE. r(2) ) ) THEN
       cd = calc_cd2_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 2) ) .AND. ( x .LE. r(3) ) ) THEN
       cd = calc_cd3_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 3) ) .AND. ( x .LE. r(4) ) ) THEN
       cd = calc_cd4_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 4) ) .AND. ( x .LE. r(5) ) ) THEN
       cd = calc_cd5_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 5) ) .AND. ( x .LE. r(6) ) ) THEN
       cd = calc_cd6_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 6) ) .AND. ( x .LE. r(7) ) ) THEN
       cd = calc_cd7_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 7) ) .AND. ( x .LE. r(8) ) ) THEN
       cd = calc_cd8_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 8) ) .AND. ( x .LE. r(9) ) ) THEN
       cd = calc_cd9_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 9) ) .AND. ( x .LE. r(10) ) ) THEN
       cd = calc_cd10_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 10) ) .AND. ( x .LE. r(11) ) ) THEN
       cd = calc_cd11_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 11) ) .AND. ( x .LE. r(12) ) ) THEN
       cd = calc_cd12_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 12) ) .AND. ( x .LE. r(13) ) ) THEN
       cd = calc_cd13_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 13) ) .AND. ( x .LE. r(14) ) ) THEN
       cd = calc_cd14_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 14) ) .AND. ( x .LE. r(15) ) ) THEN
       cd = calc_cd15_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 15) ) .AND. ( x .LE. r(16) ) ) THEN
       cd = calc_cd16_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 16) ) .AND. ( x .LE. r(17) ) ) THEN
       cd = calc_cd17_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 17) ) .AND. ( x .LE. r(18) ) ) THEN
       cd = calc_cd18_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 18) ) .AND. ( x .LE. r(19) ) ) THEN
       cd = calc_cd19_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 19) ) .AND. ( x .LE. r(20) ) ) THEN
       cd = calc_cd20_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 20) ) .AND. ( x .LE. r(21) ) ) THEN
       cd = calc_cd21_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 21) ) .AND. ( x .LE. r(22) ) ) THEN
       cd = calc_cd22_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 22) ) .AND. ( x .LE. r(23) ) ) THEN
       cd = calc_cd23_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 23) ) .AND. ( x .LE. r(24) ) ) THEN
       cd = calc_cd24_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 24) ) .AND. ( x .LE. r(25) ) ) THEN
       cd = calc_cd25_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 25) ) .AND. ( x .LE. r(26) ) ) THEN
       cd = calc_cd26_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 26) ) .AND. ( x .LE. r(27) ) ) THEN
       cd = calc_cd27_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 27) ) .AND. ( x .LE. r(28) ) ) THEN
       cd = calc_cd28_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 28) ) .AND. ( x .LE. r(29) ) ) THEN
       cd = calc_cd29_nrel2p8(y)
     ELSEIF ( ( x .GT. r( 29) ) .AND. ( x .LE. r(30) ) ) THEN
       cd = calc_cd30_nrel2p8(y)
     ENDIF
  
  IF ( x .LT. r(0) ) THEN
     PRINT*,'—————————————'
     PRINT*,'radius less than blade root in calc_cd_nrel2p8.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
  
  IF ( x .GT. r(ns-1) ) THEN
     PRINT*,'—————————————'
     PRINT*,'radius greater than blade length in calc_cd_nrel2p8.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
!_________________________________________________________________________
  
END FUNCTION calc_cd_nrel2p8
!_________________________________________________________________________

FUNCTION calc_cd1_nrel2p8( y ) RESULT( cd1 )
!_________________________________________________________________________
!
! Data from NREL 2.8 MW: Cylinder1
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  REAL :: cd1

  cd1 = 0.5
!_________________________________________________________________________
  
END FUNCTION calc_cd1_nrel2p8
!_________________________________________________________________________


!_________________________________________________________________________
FUNCTION calc_cd2_nrel2p8( y ) RESULT( cd2 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd2
INTEGER :: i

 cd2 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.4983]
  cd(0:1, 1  ) = [-177.0, 0.4994]
  cd(0:1, 2  ) = [-174.0, 0.4994]
  cd(0:1, 3  ) = [-171.0, 0.4995]
  cd(0:1, 4  ) = [-168.0, 0.4995]
  cd(0:1, 5  ) = [-165.0, 0.4996]
  cd(0:1, 6  ) = [-162.0, 0.4997]
  cd(0:1, 7  ) = [-159.0, 0.4998]
  cd(0:1, 8  ) = [-156.0, 0.4998]
  cd(0:1, 9  ) = [-153.0, 0.4999]
  cd(0:1, 10  ) = [-150.0, 0.5]
  cd(0:1, 11  ) = [-147.0, 0.5015]
  cd(0:1, 12  ) = [-144.0, 0.5076]
  cd(0:1, 13  ) = [-141.0, 0.5137]
  cd(0:1, 14  ) = [-138.0, 0.52]
  cd(0:1, 15  ) = [-135.0, 0.5264]
  cd(0:1, 16  ) = [-132.0, 0.5328]
  cd(0:1, 17  ) = [-129.0, 0.539]
  cd(0:1, 18  ) = [-126.0, 0.5449]
  cd(0:1, 19  ) = [-123.0, 0.548]
  cd(0:1, 20  ) = [-120.0, 0.5504]
  cd(0:1, 21  ) = [-117.0, 0.5518]
  cd(0:1, 22  ) = [-114.0, 0.5533]
  cd(0:1, 23  ) = [-111.0, 0.5547]
  cd(0:1, 24  ) = [-108.0, 0.5553]
  cd(0:1, 25  ) = [-105.0, 0.5555]
  cd(0:1, 26  ) = [-102.0, 0.5557]
  cd(0:1, 27  ) = [-99.0, 0.5553]
  cd(0:1, 28  ) = [-96.0, 0.554]
  cd(0:1, 29  ) = [-93.0, 0.5527]
  cd(0:1, 30  ) = [-90.0, 0.5514]
  cd(0:1, 31  ) = [-87.0, 0.5527]
  cd(0:1, 32  ) = [-84.0, 0.554]
  cd(0:1, 33  ) = [-81.0, 0.5553]
  cd(0:1, 34  ) = [-78.0, 0.5557]
  cd(0:1, 35  ) = [-75.0, 0.5555]
  cd(0:1, 36  ) = [-72.0, 0.5553]
  cd(0:1, 37  ) = [-69.0, 0.5547]
  cd(0:1, 38  ) = [-66.0, 0.5533]
  cd(0:1, 39  ) = [-63.0, 0.5518]
  cd(0:1, 40  ) = [-60.0, 0.5504]
  cd(0:1, 41  ) = [-57.0, 0.548]
  cd(0:1, 42  ) = [-54.0, 0.5449]
  cd(0:1, 43  ) = [-51.0, 0.539]
  cd(0:1, 44  ) = [-48.0, 0.5328]
  cd(0:1, 45  ) = [-45.0, 0.5264]
  cd(0:1, 46  ) = [-42.0, 0.52]
  cd(0:1, 47  ) = [-39.0, 0.5137]
  cd(0:1, 48  ) = [-36.0, 0.5076]
  cd(0:1, 49  ) = [-33.0, 0.5015]
  cd(0:1, 50  ) = [-30.0, 0.5]
  cd(0:1, 51  ) = [-29.3939, 0.5]
  cd(0:1, 52  ) = [-28.7879, 0.4999]
  cd(0:1, 53  ) = [-28.1818, 0.4999]
  cd(0:1, 54  ) = [-27.5758, 0.4999]
  cd(0:1, 55  ) = [-26.9697, 0.4999]
  cd(0:1, 56  ) = [-26.3636, 0.4999]
  cd(0:1, 57  ) = [-25.7576, 0.4999]
  cd(0:1, 58  ) = [-25.1515, 0.4999]
  cd(0:1, 59  ) = [-24.5455, 0.4999]
  cd(0:1, 60  ) = [-23.9394, 0.4998]
  cd(0:1, 61  ) = [-23.3333, 0.4998]
  cd(0:1, 62  ) = [-22.7273, 0.4998]
  cd(0:1, 63  ) = [-22.1212, 0.4998]
  cd(0:1, 64  ) = [-21.5152, 0.4998]
  cd(0:1, 65  ) = [-20.9091, 0.4998]
  cd(0:1, 66  ) = [-20.303, 0.4997]
  cd(0:1, 67  ) = [-19.697, 0.4997]
  cd(0:1, 68  ) = [-19.0909, 0.4997]
  cd(0:1, 69  ) = [-18.4848, 0.4997]
  cd(0:1, 70  ) = [-17.8788, 0.4997]
  cd(0:1, 71  ) = [-17.2727, 0.4996]
  cd(0:1, 72  ) = [-16.6667, 0.4996]
  cd(0:1, 73  ) = [-16.0606, 0.4996]
  cd(0:1, 74  ) = [-15.4545, 0.4996]
  cd(0:1, 75  ) = [-14.8485, 0.4996]
  cd(0:1, 76  ) = [-14.2424, 0.4995]
  cd(0:1, 77  ) = [-13.6364, 0.4995]
  cd(0:1, 78  ) = [-13.0303, 0.4995]
  cd(0:1, 79  ) = [-12.4242, 0.4995]
  cd(0:1, 80  ) = [-11.8182, 0.4994]
  cd(0:1, 81  ) = [-11.2121, 0.4994]
  cd(0:1, 82  ) = [-10.6061, 0.4991]
  cd(0:1, 83  ) = [-10.0, 0.498]
  cd(0:1, 84  ) = [-9.3939, 0.4965]
  cd(0:1, 85  ) = [-8.7879, 0.495]
  cd(0:1, 86  ) = [-8.1818, 0.4935]
  cd(0:1, 87  ) = [-7.5758, 0.492]
  cd(0:1, 88  ) = [-6.9697, 0.4904]
  cd(0:1, 89  ) = [-6.3636, 0.4889]
  cd(0:1, 90  ) = [-5.7576, 0.4874]
  cd(0:1, 91  ) = [-5.1515, 0.4859]
  cd(0:1, 92  ) = [-4.5455, 0.4844]
  cd(0:1, 93  ) = [-3.9394, 0.4828]
  cd(0:1, 94  ) = [-3.3333, 0.4815]
  cd(0:1, 95  ) = [-2.7273, 0.4811]
  cd(0:1, 96  ) = [-2.1212, 0.4812]
  cd(0:1, 97  ) = [-1.5152, 0.4813]
  cd(0:1, 98  ) = [-0.9091, 0.4815]
  cd(0:1, 99  ) = [-0.303, 0.4816]
  cd(0:1, 100  ) = [0.303, 0.4817]
  cd(0:1, 101  ) = [0.9091, 0.4818]
  cd(0:1, 102  ) = [1.5152, 0.4819]
  cd(0:1, 103  ) = [2.1212, 0.4819]
  cd(0:1, 104  ) = [2.7273, 0.4819]
  cd(0:1, 105  ) = [3.3333, 0.4818]
  cd(0:1, 106  ) = [3.9394, 0.4818]
  cd(0:1, 107  ) = [4.5455, 0.4817]
  cd(0:1, 108  ) = [5.1515, 0.4817]
  cd(0:1, 109  ) = [5.7576, 0.4816]
  cd(0:1, 110  ) = [6.3636, 0.4816]
  cd(0:1, 111  ) = [6.9697, 0.4815]
  cd(0:1, 112  ) = [7.5758, 0.4815]
  cd(0:1, 113  ) = [8.1818, 0.4814]
  cd(0:1, 114  ) = [8.7879, 0.4815]
  cd(0:1, 115  ) = [9.3939, 0.4817]
  cd(0:1, 116  ) = [10.0, 0.482]
  cd(0:1, 117  ) = [10.6061, 0.4864]
  cd(0:1, 118  ) = [11.2121, 0.491]
  cd(0:1, 119  ) = [11.8182, 0.4946]
  cd(0:1, 120  ) = [12.4242, 0.4976]
  cd(0:1, 121  ) = [13.0303, 0.4994]
  cd(0:1, 122  ) = [13.6364, 0.4995]
  cd(0:1, 123  ) = [14.2424, 0.4995]
  cd(0:1, 124  ) = [14.8485, 0.4996]
  cd(0:1, 125  ) = [15.4545, 0.4996]
  cd(0:1, 126  ) = [16.0606, 0.4996]
  cd(0:1, 127  ) = [16.6667, 0.4996]
  cd(0:1, 128  ) = [17.2727, 0.4997]
  cd(0:1, 129  ) = [17.8788, 0.4997]
  cd(0:1, 130  ) = [18.4848, 0.4997]
  cd(0:1, 131  ) = [19.0909, 0.4997]
  cd(0:1, 132  ) = [19.697, 0.4997]
  cd(0:1, 133  ) = [20.303, 0.4997]
  cd(0:1, 134  ) = [20.9091, 0.4998]
  cd(0:1, 135  ) = [21.5152, 0.4998]
  cd(0:1, 136  ) = [22.1212, 0.4998]
  cd(0:1, 137  ) = [22.7273, 0.4998]
  cd(0:1, 138  ) = [23.3333, 0.4998]
  cd(0:1, 139  ) = [23.9394, 0.4998]
  cd(0:1, 140  ) = [24.5455, 0.4999]
  cd(0:1, 141  ) = [25.1515, 0.4999]
  cd(0:1, 142  ) = [25.7576, 0.4999]
  cd(0:1, 143  ) = [26.3636, 0.4999]
  cd(0:1, 144  ) = [26.9697, 0.4999]
  cd(0:1, 145  ) = [27.5758, 0.4999]
  cd(0:1, 146  ) = [28.1818, 0.4999]
  cd(0:1, 147  ) = [28.7879, 0.4999]
  cd(0:1, 148  ) = [29.3939, 0.5]
  cd(0:1, 149  ) = [30.0, 0.5]
  cd(0:1, 150  ) = [33.0, 0.5015]
  cd(0:1, 151  ) = [36.0, 0.5076]
  cd(0:1, 152  ) = [39.0, 0.5137]
  cd(0:1, 153  ) = [42.0, 0.52]
  cd(0:1, 154  ) = [45.0, 0.5264]
  cd(0:1, 155  ) = [48.0, 0.5328]
  cd(0:1, 156  ) = [51.0, 0.539]
  cd(0:1, 157  ) = [54.0, 0.5449]
  cd(0:1, 158  ) = [57.0, 0.548]
  cd(0:1, 159  ) = [60.0, 0.5504]
  cd(0:1, 160  ) = [63.0, 0.5518]
  cd(0:1, 161  ) = [66.0, 0.5533]
  cd(0:1, 162  ) = [69.0, 0.5547]
  cd(0:1, 163  ) = [72.0, 0.5553]
  cd(0:1, 164  ) = [75.0, 0.5555]
  cd(0:1, 165  ) = [78.0, 0.5557]
  cd(0:1, 166  ) = [81.0, 0.5553]
  cd(0:1, 167  ) = [84.0, 0.554]
  cd(0:1, 168  ) = [87.0, 0.5527]
  cd(0:1, 169  ) = [90.0, 0.5514]
  cd(0:1, 170  ) = [93.0, 0.5527]
  cd(0:1, 171  ) = [96.0, 0.554]
  cd(0:1, 172  ) = [99.0, 0.5553]
  cd(0:1, 173  ) = [102.0, 0.5557]
  cd(0:1, 174  ) = [105.0, 0.5555]
  cd(0:1, 175  ) = [108.0, 0.5553]
  cd(0:1, 176  ) = [111.0, 0.5547]
  cd(0:1, 177  ) = [114.0, 0.5533]
  cd(0:1, 178  ) = [117.0, 0.5518]
  cd(0:1, 179  ) = [120.0, 0.5504]
  cd(0:1, 180  ) = [123.0, 0.548]
  cd(0:1, 181  ) = [126.0, 0.5449]
  cd(0:1, 182  ) = [129.0, 0.539]
  cd(0:1, 183  ) = [132.0, 0.5328]
  cd(0:1, 184  ) = [135.0, 0.5264]
  cd(0:1, 185  ) = [138.0, 0.52]
  cd(0:1, 186  ) = [141.0, 0.5137]
  cd(0:1, 187  ) = [144.0, 0.5076]
  cd(0:1, 188  ) = [147.0, 0.5015]
  cd(0:1, 189  ) = [150.0, 0.5]
  cd(0:1, 190  ) = [153.0, 0.4999]
  cd(0:1, 191  ) = [156.0, 0.4998]
  cd(0:1, 192  ) = [159.0, 0.4998]
  cd(0:1, 193  ) = [162.0, 0.4997]
  cd(0:1, 194  ) = [165.0, 0.4996]
  cd(0:1, 195  ) = [168.0, 0.4995]
  cd(0:1, 196  ) = [171.0, 0.4995]
  cd(0:1, 197  ) = [174.0, 0.4994]
  cd(0:1, 198  ) = [177.0, 0.4994]
  cd(0:1, 199  ) = [180.0, 0.4983]

  IF ( y .EQ. cd(0,0) ) cd2 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd2 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd2 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd2_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd2 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd2_nrel2p8.pro' 
  ENDIF

!print,y,cd2

END FUNCTION calc_cd2_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd3_nrel2p8( y ) RESULT( cd3 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd3
INTEGER :: i

 cd3 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.444]
  cd(0:1, 1  ) = [-177.0, 0.4518]
  cd(0:1, 2  ) = [-174.0, 0.4548]
  cd(0:1, 3  ) = [-171.0, 0.4577]
  cd(0:1, 4  ) = [-168.0, 0.4626]
  cd(0:1, 5  ) = [-165.0, 0.4682]
  cd(0:1, 6  ) = [-162.0, 0.4735]
  cd(0:1, 7  ) = [-159.0, 0.4792]
  cd(0:1, 8  ) = [-156.0, 0.4857]
  cd(0:1, 9  ) = [-153.0, 0.4915]
  cd(0:1, 10  ) = [-150.0, 0.4965]
  cd(0:1, 11  ) = [-147.0, 0.5104]
  cd(0:1, 12  ) = [-144.0, 0.5515]
  cd(0:1, 13  ) = [-141.0, 0.5925]
  cd(0:1, 14  ) = [-138.0, 0.6349]
  cd(0:1, 15  ) = [-135.0, 0.6781]
  cd(0:1, 16  ) = [-132.0, 0.7212]
  cd(0:1, 17  ) = [-129.0, 0.7632]
  cd(0:1, 18  ) = [-126.0, 0.8028]
  cd(0:1, 19  ) = [-123.0, 0.8301]
  cd(0:1, 20  ) = [-120.0, 0.8543]
  cd(0:1, 21  ) = [-117.0, 0.8713]
  cd(0:1, 22  ) = [-114.0, 0.8884]
  cd(0:1, 23  ) = [-111.0, 0.9054]
  cd(0:1, 24  ) = [-108.0, 0.9157]
  cd(0:1, 25  ) = [-105.0, 0.9226]
  cd(0:1, 26  ) = [-102.0, 0.9295]
  cd(0:1, 27  ) = [-99.0, 0.9325]
  cd(0:1, 28  ) = [-96.0, 0.9275]
  cd(0:1, 29  ) = [-93.0, 0.9225]
  cd(0:1, 30  ) = [-90.0, 0.9175]
  cd(0:1, 31  ) = [-87.0, 0.9225]
  cd(0:1, 32  ) = [-84.0, 0.9275]
  cd(0:1, 33  ) = [-81.0, 0.9325]
  cd(0:1, 34  ) = [-78.0, 0.9295]
  cd(0:1, 35  ) = [-75.0, 0.9226]
  cd(0:1, 36  ) = [-72.0, 0.9157]
  cd(0:1, 37  ) = [-69.0, 0.9054]
  cd(0:1, 38  ) = [-66.0, 0.8884]
  cd(0:1, 39  ) = [-63.0, 0.8713]
  cd(0:1, 40  ) = [-60.0, 0.8543]
  cd(0:1, 41  ) = [-57.0, 0.8301]
  cd(0:1, 42  ) = [-54.0, 0.8028]
  cd(0:1, 43  ) = [-51.0, 0.7632]
  cd(0:1, 44  ) = [-48.0, 0.7212]
  cd(0:1, 45  ) = [-45.0, 0.6781]
  cd(0:1, 46  ) = [-42.0, 0.6349]
  cd(0:1, 47  ) = [-39.0, 0.5925]
  cd(0:1, 48  ) = [-36.0, 0.5515]
  cd(0:1, 49  ) = [-33.0, 0.5104]
  cd(0:1, 50  ) = [-30.0, 0.4965]
  cd(0:1, 51  ) = [-29.3939, 0.4956]
  cd(0:1, 52  ) = [-28.7879, 0.4946]
  cd(0:1, 53  ) = [-28.1818, 0.4936]
  cd(0:1, 54  ) = [-27.5758, 0.4925]
  cd(0:1, 55  ) = [-26.9697, 0.4914]
  cd(0:1, 56  ) = [-26.3636, 0.4903]
  cd(0:1, 57  ) = [-25.7576, 0.4892]
  cd(0:1, 58  ) = [-25.1515, 0.488]
  cd(0:1, 59  ) = [-24.5455, 0.4868]
  cd(0:1, 60  ) = [-23.9394, 0.4855]
  cd(0:1, 61  ) = [-23.3333, 0.4843]
  cd(0:1, 62  ) = [-22.7273, 0.483]
  cd(0:1, 63  ) = [-22.1212, 0.4817]
  cd(0:1, 64  ) = [-21.5152, 0.4803]
  cd(0:1, 65  ) = [-20.9091, 0.479]
  cd(0:1, 66  ) = [-20.303, 0.4776]
  cd(0:1, 67  ) = [-19.697, 0.4762]
  cd(0:1, 68  ) = [-19.0909, 0.4749]
  cd(0:1, 69  ) = [-18.4848, 0.4736]
  cd(0:1, 70  ) = [-17.8788, 0.4722]
  cd(0:1, 71  ) = [-17.2727, 0.4708]
  cd(0:1, 72  ) = [-16.6667, 0.4693]
  cd(0:1, 73  ) = [-16.0606, 0.4678]
  cd(0:1, 74  ) = [-15.4545, 0.4663]
  cd(0:1, 75  ) = [-14.8485, 0.4647]
  cd(0:1, 76  ) = [-14.2424, 0.4631]
  cd(0:1, 77  ) = [-13.6364, 0.4614]
  cd(0:1, 78  ) = [-13.0303, 0.4597]
  cd(0:1, 79  ) = [-12.4242, 0.458]
  cd(0:1, 80  ) = [-11.8182, 0.4562]
  cd(0:1, 81  ) = [-11.2121, 0.4543]
  cd(0:1, 82  ) = [-10.6061, 0.451]
  cd(0:1, 83  ) = [-10.0, 0.4445]
  cd(0:1, 84  ) = [-9.3939, 0.4356]
  cd(0:1, 85  ) = [-8.7879, 0.4266]
  cd(0:1, 86  ) = [-8.1818, 0.4175]
  cd(0:1, 87  ) = [-7.5758, 0.4082]
  cd(0:1, 88  ) = [-6.9697, 0.3989]
  cd(0:1, 89  ) = [-6.3636, 0.3893]
  cd(0:1, 90  ) = [-5.7576, 0.3797]
  cd(0:1, 91  ) = [-5.1515, 0.3699]
  cd(0:1, 92  ) = [-4.5455, 0.3599]
  cd(0:1, 93  ) = [-3.9394, 0.3497]
  cd(0:1, 94  ) = [-3.3333, 0.3406]
  cd(0:1, 95  ) = [-2.7273, 0.3379]
  cd(0:1, 96  ) = [-2.1212, 0.3387]
  cd(0:1, 97  ) = [-1.5152, 0.3395]
  cd(0:1, 98  ) = [-0.9091, 0.3404]
  cd(0:1, 99  ) = [-0.303, 0.3411]
  cd(0:1, 100  ) = [0.303, 0.342]
  cd(0:1, 101  ) = [0.9091, 0.3428]
  cd(0:1, 102  ) = [1.5152, 0.3436]
  cd(0:1, 103  ) = [2.1212, 0.3436]
  cd(0:1, 104  ) = [2.7273, 0.3433]
  cd(0:1, 105  ) = [3.3333, 0.343]
  cd(0:1, 106  ) = [3.9394, 0.3426]
  cd(0:1, 107  ) = [4.5455, 0.3423]
  cd(0:1, 108  ) = [5.1515, 0.3419]
  cd(0:1, 109  ) = [5.7576, 0.3416]
  cd(0:1, 110  ) = [6.3636, 0.3413]
  cd(0:1, 111  ) = [6.9697, 0.3409]
  cd(0:1, 112  ) = [7.5758, 0.3407]
  cd(0:1, 113  ) = [8.1818, 0.3403]
  cd(0:1, 114  ) = [8.7879, 0.3406]
  cd(0:1, 115  ) = [9.3939, 0.3423]
  cd(0:1, 116  ) = [10.0, 0.3444]
  cd(0:1, 117  ) = [10.6061, 0.3729]
  cd(0:1, 118  ) = [11.2121, 0.402]
  cd(0:1, 119  ) = [11.8182, 0.424]
  cd(0:1, 120  ) = [12.4242, 0.4423]
  cd(0:1, 121  ) = [13.0303, 0.4544]
  cd(0:1, 122  ) = [13.6364, 0.4584]
  cd(0:1, 123  ) = [14.2424, 0.4617]
  cd(0:1, 124  ) = [14.8485, 0.4647]
  cd(0:1, 125  ) = [15.4545, 0.4672]
  cd(0:1, 126  ) = [16.0606, 0.4694]
  cd(0:1, 127  ) = [16.6667, 0.4707]
  cd(0:1, 128  ) = [17.2727, 0.4719]
  cd(0:1, 129  ) = [17.8788, 0.473]
  cd(0:1, 130  ) = [18.4848, 0.4741]
  cd(0:1, 131  ) = [19.0909, 0.4752]
  cd(0:1, 132  ) = [19.697, 0.4763]
  cd(0:1, 133  ) = [20.303, 0.4776]
  cd(0:1, 134  ) = [20.9091, 0.479]
  cd(0:1, 135  ) = [21.5152, 0.4803]
  cd(0:1, 136  ) = [22.1212, 0.4817]
  cd(0:1, 137  ) = [22.7273, 0.483]
  cd(0:1, 138  ) = [23.3333, 0.4843]
  cd(0:1, 139  ) = [23.9394, 0.4855]
  cd(0:1, 140  ) = [24.5455, 0.4868]
  cd(0:1, 141  ) = [25.1515, 0.488]
  cd(0:1, 142  ) = [25.7576, 0.4892]
  cd(0:1, 143  ) = [26.3636, 0.4903]
  cd(0:1, 144  ) = [26.9697, 0.4914]
  cd(0:1, 145  ) = [27.5758, 0.4925]
  cd(0:1, 146  ) = [28.1818, 0.4936]
  cd(0:1, 147  ) = [28.7879, 0.4946]
  cd(0:1, 148  ) = [29.3939, 0.4956]
  cd(0:1, 149  ) = [30.0, 0.4965]
  cd(0:1, 150  ) = [33.0, 0.5104]
  cd(0:1, 151  ) = [36.0, 0.5515]
  cd(0:1, 152  ) = [39.0, 0.5925]
  cd(0:1, 153  ) = [42.0, 0.6349]
  cd(0:1, 154  ) = [45.0, 0.6781]
  cd(0:1, 155  ) = [48.0, 0.7212]
  cd(0:1, 156  ) = [51.0, 0.7632]
  cd(0:1, 157  ) = [54.0, 0.8028]
  cd(0:1, 158  ) = [57.0, 0.8301]
  cd(0:1, 159  ) = [60.0, 0.8543]
  cd(0:1, 160  ) = [63.0, 0.8713]
  cd(0:1, 161  ) = [66.0, 0.8884]
  cd(0:1, 162  ) = [69.0, 0.9054]
  cd(0:1, 163  ) = [72.0, 0.9157]
  cd(0:1, 164  ) = [75.0, 0.9226]
  cd(0:1, 165  ) = [78.0, 0.9295]
  cd(0:1, 166  ) = [81.0, 0.9325]
  cd(0:1, 167  ) = [84.0, 0.9275]
  cd(0:1, 168  ) = [87.0, 0.9225]
  cd(0:1, 169  ) = [90.0, 0.9175]
  cd(0:1, 170  ) = [93.0, 0.9225]
  cd(0:1, 171  ) = [96.0, 0.9275]
  cd(0:1, 172  ) = [99.0, 0.9325]
  cd(0:1, 173  ) = [102.0, 0.9295]
  cd(0:1, 174  ) = [105.0, 0.9226]
  cd(0:1, 175  ) = [108.0, 0.9157]
  cd(0:1, 176  ) = [111.0, 0.9054]
  cd(0:1, 177  ) = [114.0, 0.8884]
  cd(0:1, 178  ) = [117.0, 0.8713]
  cd(0:1, 179  ) = [120.0, 0.8543]
  cd(0:1, 180  ) = [123.0, 0.8301]
  cd(0:1, 181  ) = [126.0, 0.8028]
  cd(0:1, 182  ) = [129.0, 0.7632]
  cd(0:1, 183  ) = [132.0, 0.7212]
  cd(0:1, 184  ) = [135.0, 0.6781]
  cd(0:1, 185  ) = [138.0, 0.6349]
  cd(0:1, 186  ) = [141.0, 0.5925]
  cd(0:1, 187  ) = [144.0, 0.5515]
  cd(0:1, 188  ) = [147.0, 0.5104]
  cd(0:1, 189  ) = [150.0, 0.4965]
  cd(0:1, 190  ) = [153.0, 0.4915]
  cd(0:1, 191  ) = [156.0, 0.4857]
  cd(0:1, 192  ) = [159.0, 0.4792]
  cd(0:1, 193  ) = [162.0, 0.4735]
  cd(0:1, 194  ) = [165.0, 0.4682]
  cd(0:1, 195  ) = [168.0, 0.4626]
  cd(0:1, 196  ) = [171.0, 0.4577]
  cd(0:1, 197  ) = [174.0, 0.4548]
  cd(0:1, 198  ) = [177.0, 0.4518]
  cd(0:1, 199  ) = [180.0, 0.444]

  IF ( y .EQ. cd(0,0) ) cd3 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd3 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd3 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd3_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd3 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd3_nrel2p8.pro' 
  ENDIF

!print,y,cd3

END FUNCTION calc_cd3_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd4_nrel2p8( y ) RESULT( cd4 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd4
INTEGER :: i

 cd4 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.2867]
  cd(0:1, 1  ) = [-177.0, 0.2981]
  cd(0:1, 2  ) = [-174.0, 0.3076]
  cd(0:1, 3  ) = [-171.0, 0.3168]
  cd(0:1, 4  ) = [-168.0, 0.3342]
  cd(0:1, 5  ) = [-165.0, 0.3554]
  cd(0:1, 6  ) = [-162.0, 0.3759]
  cd(0:1, 7  ) = [-159.0, 0.3988]
  cd(0:1, 8  ) = [-156.0, 0.427]
  cd(0:1, 9  ) = [-153.0, 0.4539]
  cd(0:1, 10  ) = [-150.0, 0.4791]
  cd(0:1, 11  ) = [-147.0, 0.5146]
  cd(0:1, 12  ) = [-144.0, 0.572]
  cd(0:1, 13  ) = [-141.0, 0.6294]
  cd(0:1, 14  ) = [-138.0, 0.6887]
  cd(0:1, 15  ) = [-135.0, 0.749]
  cd(0:1, 16  ) = [-132.0, 0.8094]
  cd(0:1, 17  ) = [-129.0, 0.8681]
  cd(0:1, 18  ) = [-126.0, 0.9235]
  cd(0:1, 19  ) = [-123.0, 0.9746]
  cd(0:1, 20  ) = [-120.0, 1.0246]
  cd(0:1, 21  ) = [-117.0, 1.0629]
  cd(0:1, 22  ) = [-114.0, 1.1013]
  cd(0:1, 23  ) = [-111.0, 1.1397]
  cd(0:1, 24  ) = [-108.0, 1.1668]
  cd(0:1, 25  ) = [-105.0, 1.1881]
  cd(0:1, 26  ) = [-102.0, 1.2094]
  cd(0:1, 27  ) = [-99.0, 1.2239]
  cd(0:1, 28  ) = [-96.0, 1.2248]
  cd(0:1, 29  ) = [-93.0, 1.2257]
  cd(0:1, 30  ) = [-90.0, 1.2266]
  cd(0:1, 31  ) = [-87.0, 1.2257]
  cd(0:1, 32  ) = [-84.0, 1.2248]
  cd(0:1, 33  ) = [-81.0, 1.2239]
  cd(0:1, 34  ) = [-78.0, 1.2094]
  cd(0:1, 35  ) = [-75.0, 1.1881]
  cd(0:1, 36  ) = [-72.0, 1.1668]
  cd(0:1, 37  ) = [-69.0, 1.1397]
  cd(0:1, 38  ) = [-66.0, 1.1013]
  cd(0:1, 39  ) = [-63.0, 1.0629]
  cd(0:1, 40  ) = [-60.0, 1.0246]
  cd(0:1, 41  ) = [-57.0, 0.9746]
  cd(0:1, 42  ) = [-54.0, 0.9235]
  cd(0:1, 43  ) = [-51.0, 0.8681]
  cd(0:1, 44  ) = [-48.0, 0.8094]
  cd(0:1, 45  ) = [-45.0, 0.749]
  cd(0:1, 46  ) = [-42.0, 0.6887]
  cd(0:1, 47  ) = [-39.0, 0.6294]
  cd(0:1, 48  ) = [-36.0, 0.572]
  cd(0:1, 49  ) = [-33.0, 0.5146]
  cd(0:1, 50  ) = [-30.0, 0.4791]
  cd(0:1, 51  ) = [-29.3939, 0.4741]
  cd(0:1, 52  ) = [-28.7879, 0.4691]
  cd(0:1, 53  ) = [-28.1818, 0.464]
  cd(0:1, 54  ) = [-27.5758, 0.4589]
  cd(0:1, 55  ) = [-26.9697, 0.4536]
  cd(0:1, 56  ) = [-26.3636, 0.4483]
  cd(0:1, 57  ) = [-25.7576, 0.4429]
  cd(0:1, 58  ) = [-25.1515, 0.4375]
  cd(0:1, 59  ) = [-24.5455, 0.432]
  cd(0:1, 60  ) = [-23.9394, 0.4265]
  cd(0:1, 61  ) = [-23.3333, 0.4209]
  cd(0:1, 62  ) = [-22.7273, 0.4152]
  cd(0:1, 63  ) = [-22.1212, 0.4095]
  cd(0:1, 64  ) = [-21.5152, 0.4038]
  cd(0:1, 65  ) = [-20.9091, 0.398]
  cd(0:1, 66  ) = [-20.303, 0.3921]
  cd(0:1, 67  ) = [-19.697, 0.3867]
  cd(0:1, 68  ) = [-19.0909, 0.3817]
  cd(0:1, 69  ) = [-18.4848, 0.3766]
  cd(0:1, 70  ) = [-17.8788, 0.3715]
  cd(0:1, 71  ) = [-17.2727, 0.3662]
  cd(0:1, 72  ) = [-16.6667, 0.3609]
  cd(0:1, 73  ) = [-16.0606, 0.3555]
  cd(0:1, 74  ) = [-15.4545, 0.35]
  cd(0:1, 75  ) = [-14.8485, 0.3445]
  cd(0:1, 76  ) = [-14.2424, 0.3389]
  cd(0:1, 77  ) = [-13.6364, 0.3332]
  cd(0:1, 78  ) = [-13.0303, 0.3274]
  cd(0:1, 79  ) = [-12.4242, 0.3215]
  cd(0:1, 80  ) = [-11.8182, 0.3155]
  cd(0:1, 81  ) = [-11.2121, 0.3095]
  cd(0:1, 82  ) = [-10.6061, 0.3028]
  cd(0:1, 83  ) = [-10.0, 0.2949]
  cd(0:1, 84  ) = [-9.3939, 0.2855]
  cd(0:1, 85  ) = [-8.7879, 0.2758]
  cd(0:1, 86  ) = [-8.1818, 0.266]
  cd(0:1, 87  ) = [-7.5758, 0.2558]
  cd(0:1, 88  ) = [-6.9697, 0.2455]
  cd(0:1, 89  ) = [-6.3636, 0.2348]
  cd(0:1, 90  ) = [-5.7576, 0.2239]
  cd(0:1, 91  ) = [-5.1515, 0.2127]
  cd(0:1, 92  ) = [-4.5455, 0.2011]
  cd(0:1, 93  ) = [-3.9394, 0.1892]
  cd(0:1, 94  ) = [-3.3333, 0.1784]
  cd(0:1, 95  ) = [-2.7273, 0.1751]
  cd(0:1, 96  ) = [-2.1212, 0.1761]
  cd(0:1, 97  ) = [-1.5152, 0.1771]
  cd(0:1, 98  ) = [-0.9091, 0.178]
  cd(0:1, 99  ) = [-0.303, 0.1789]
  cd(0:1, 100  ) = [0.303, 0.1799]
  cd(0:1, 101  ) = [0.9091, 0.1809]
  cd(0:1, 102  ) = [1.5152, 0.1819]
  cd(0:1, 103  ) = [2.1212, 0.1819]
  cd(0:1, 104  ) = [2.7273, 0.1815]
  cd(0:1, 105  ) = [3.3333, 0.1811]
  cd(0:1, 106  ) = [3.9394, 0.1807]
  cd(0:1, 107  ) = [4.5455, 0.1803]
  cd(0:1, 108  ) = [5.1515, 0.18]
  cd(0:1, 109  ) = [5.7576, 0.1796]
  cd(0:1, 110  ) = [6.3636, 0.1792]
  cd(0:1, 111  ) = [6.9697, 0.1788]
  cd(0:1, 112  ) = [7.5758, 0.1785]
  cd(0:1, 113  ) = [8.1818, 0.1782]
  cd(0:1, 114  ) = [8.7879, 0.1785]
  cd(0:1, 115  ) = [9.3939, 0.1806]
  cd(0:1, 116  ) = [10.0, 0.183]
  cd(0:1, 117  ) = [10.6061, 0.2159]
  cd(0:1, 118  ) = [11.2121, 0.2481]
  cd(0:1, 119  ) = [11.8182, 0.272]
  cd(0:1, 120  ) = [12.4242, 0.2922]
  cd(0:1, 121  ) = [13.0303, 0.309]
  cd(0:1, 122  ) = [13.6364, 0.3215]
  cd(0:1, 123  ) = [14.2424, 0.3323]
  cd(0:1, 124  ) = [14.8485, 0.3422]
  cd(0:1, 125  ) = [15.4545, 0.3509]
  cd(0:1, 126  ) = [16.0606, 0.3587]
  cd(0:1, 127  ) = [16.6667, 0.3637]
  cd(0:1, 128  ) = [17.2727, 0.3685]
  cd(0:1, 129  ) = [17.8788, 0.3731]
  cd(0:1, 130  ) = [18.4848, 0.3777]
  cd(0:1, 131  ) = [19.0909, 0.3824]
  cd(0:1, 132  ) = [19.697, 0.3869]
  cd(0:1, 133  ) = [20.303, 0.3921]
  cd(0:1, 134  ) = [20.9091, 0.398]
  cd(0:1, 135  ) = [21.5152, 0.4038]
  cd(0:1, 136  ) = [22.1212, 0.4095]
  cd(0:1, 137  ) = [22.7273, 0.4152]
  cd(0:1, 138  ) = [23.3333, 0.4209]
  cd(0:1, 139  ) = [23.9394, 0.4265]
  cd(0:1, 140  ) = [24.5455, 0.432]
  cd(0:1, 141  ) = [25.1515, 0.4375]
  cd(0:1, 142  ) = [25.7576, 0.4429]
  cd(0:1, 143  ) = [26.3636, 0.4483]
  cd(0:1, 144  ) = [26.9697, 0.4536]
  cd(0:1, 145  ) = [27.5758, 0.4589]
  cd(0:1, 146  ) = [28.1818, 0.464]
  cd(0:1, 147  ) = [28.7879, 0.4691]
  cd(0:1, 148  ) = [29.3939, 0.4741]
  cd(0:1, 149  ) = [30.0, 0.4791]
  cd(0:1, 150  ) = [33.0, 0.5146]
  cd(0:1, 151  ) = [36.0, 0.572]
  cd(0:1, 152  ) = [39.0, 0.6294]
  cd(0:1, 153  ) = [42.0, 0.6887]
  cd(0:1, 154  ) = [45.0, 0.749]
  cd(0:1, 155  ) = [48.0, 0.8094]
  cd(0:1, 156  ) = [51.0, 0.8681]
  cd(0:1, 157  ) = [54.0, 0.9235]
  cd(0:1, 158  ) = [57.0, 0.9746]
  cd(0:1, 159  ) = [60.0, 1.0246]
  cd(0:1, 160  ) = [63.0, 1.0629]
  cd(0:1, 161  ) = [66.0, 1.1013]
  cd(0:1, 162  ) = [69.0, 1.1397]
  cd(0:1, 163  ) = [72.0, 1.1668]
  cd(0:1, 164  ) = [75.0, 1.1881]
  cd(0:1, 165  ) = [78.0, 1.2094]
  cd(0:1, 166  ) = [81.0, 1.2239]
  cd(0:1, 167  ) = [84.0, 1.2248]
  cd(0:1, 168  ) = [87.0, 1.2257]
  cd(0:1, 169  ) = [90.0, 1.2266]
  cd(0:1, 170  ) = [93.0, 1.2257]
  cd(0:1, 171  ) = [96.0, 1.2248]
  cd(0:1, 172  ) = [99.0, 1.2239]
  cd(0:1, 173  ) = [102.0, 1.2094]
  cd(0:1, 174  ) = [105.0, 1.1881]
  cd(0:1, 175  ) = [108.0, 1.1668]
  cd(0:1, 176  ) = [111.0, 1.1397]
  cd(0:1, 177  ) = [114.0, 1.1013]
  cd(0:1, 178  ) = [117.0, 1.0629]
  cd(0:1, 179  ) = [120.0, 1.0246]
  cd(0:1, 180  ) = [123.0, 0.9746]
  cd(0:1, 181  ) = [126.0, 0.9235]
  cd(0:1, 182  ) = [129.0, 0.8681]
  cd(0:1, 183  ) = [132.0, 0.8094]
  cd(0:1, 184  ) = [135.0, 0.749]
  cd(0:1, 185  ) = [138.0, 0.6887]
  cd(0:1, 186  ) = [141.0, 0.6294]
  cd(0:1, 187  ) = [144.0, 0.572]
  cd(0:1, 188  ) = [147.0, 0.5146]
  cd(0:1, 189  ) = [150.0, 0.4791]
  cd(0:1, 190  ) = [153.0, 0.4539]
  cd(0:1, 191  ) = [156.0, 0.427]
  cd(0:1, 192  ) = [159.0, 0.3988]
  cd(0:1, 193  ) = [162.0, 0.3759]
  cd(0:1, 194  ) = [165.0, 0.3554]
  cd(0:1, 195  ) = [168.0, 0.3342]
  cd(0:1, 196  ) = [171.0, 0.3168]
  cd(0:1, 197  ) = [174.0, 0.3076]
  cd(0:1, 198  ) = [177.0, 0.2981]
  cd(0:1, 199  ) = [180.0, 0.2867]

  IF ( y .EQ. cd(0,0) ) cd4 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd4 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd4 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd4_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd4 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd4_nrel2p8.pro' 
  ENDIF

!print,y,cd4

END FUNCTION calc_cd4_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd5_nrel2p8( y ) RESULT( cd5 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd5
INTEGER :: i

 cd5 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.1529]
  cd(0:1, 1  ) = [-177.0, 0.1638]
  cd(0:1, 2  ) = [-174.0, 0.1747]
  cd(0:1, 3  ) = [-171.0, 0.1857]
  cd(0:1, 4  ) = [-168.0, 0.2104]
  cd(0:1, 5  ) = [-165.0, 0.242]
  cd(0:1, 6  ) = [-162.0, 0.2737]
  cd(0:1, 7  ) = [-159.0, 0.3109]
  cd(0:1, 8  ) = [-156.0, 0.3592]
  cd(0:1, 9  ) = [-153.0, 0.4075]
  cd(0:1, 10  ) = [-150.0, 0.4559]
  cd(0:1, 11  ) = [-147.0, 0.5148]
  cd(0:1, 12  ) = [-144.0, 0.5733]
  cd(0:1, 13  ) = [-141.0, 0.6318]
  cd(0:1, 14  ) = [-138.0, 0.6923]
  cd(0:1, 15  ) = [-135.0, 0.7538]
  cd(0:1, 16  ) = [-132.0, 0.8153]
  cd(0:1, 17  ) = [-129.0, 0.8751]
  cd(0:1, 18  ) = [-126.0, 0.9317]
  cd(0:1, 19  ) = [-123.0, 0.9882]
  cd(0:1, 20  ) = [-120.0, 1.0447]
  cd(0:1, 21  ) = [-117.0, 1.0889]
  cd(0:1, 22  ) = [-114.0, 1.1331]
  cd(0:1, 23  ) = [-111.0, 1.1772]
  cd(0:1, 24  ) = [-108.0, 1.2091]
  cd(0:1, 25  ) = [-105.0, 1.2349]
  cd(0:1, 26  ) = [-102.0, 1.2607]
  cd(0:1, 27  ) = [-99.0, 1.2791]
  cd(0:1, 28  ) = [-96.0, 1.2827]
  cd(0:1, 29  ) = [-93.0, 1.2864]
  cd(0:1, 30  ) = [-90.0, 1.29]
  cd(0:1, 31  ) = [-87.0, 1.2864]
  cd(0:1, 32  ) = [-84.0, 1.2827]
  cd(0:1, 33  ) = [-81.0, 1.2791]
  cd(0:1, 34  ) = [-78.0, 1.2607]
  cd(0:1, 35  ) = [-75.0, 1.2349]
  cd(0:1, 36  ) = [-72.0, 1.2091]
  cd(0:1, 37  ) = [-69.0, 1.1772]
  cd(0:1, 38  ) = [-66.0, 1.1331]
  cd(0:1, 39  ) = [-63.0, 1.0889]
  cd(0:1, 40  ) = [-60.0, 1.0447]
  cd(0:1, 41  ) = [-57.0, 0.9882]
  cd(0:1, 42  ) = [-54.0, 0.9317]
  cd(0:1, 43  ) = [-51.0, 0.8751]
  cd(0:1, 44  ) = [-48.0, 0.8153]
  cd(0:1, 45  ) = [-45.0, 0.7538]
  cd(0:1, 46  ) = [-42.0, 0.6923]
  cd(0:1, 47  ) = [-39.0, 0.6318]
  cd(0:1, 48  ) = [-36.0, 0.5733]
  cd(0:1, 49  ) = [-33.0, 0.5148]
  cd(0:1, 50  ) = [-30.0, 0.4559]
  cd(0:1, 51  ) = [-29.3939, 0.4461]
  cd(0:1, 52  ) = [-28.7879, 0.4364]
  cd(0:1, 53  ) = [-28.1818, 0.4266]
  cd(0:1, 54  ) = [-27.5758, 0.4168]
  cd(0:1, 55  ) = [-26.9697, 0.4071]
  cd(0:1, 56  ) = [-26.3636, 0.3973]
  cd(0:1, 57  ) = [-25.7576, 0.3875]
  cd(0:1, 58  ) = [-25.1515, 0.3778]
  cd(0:1, 59  ) = [-24.5455, 0.368]
  cd(0:1, 60  ) = [-23.9394, 0.3582]
  cd(0:1, 61  ) = [-23.3333, 0.3485]
  cd(0:1, 62  ) = [-22.7273, 0.3387]
  cd(0:1, 63  ) = [-22.1212, 0.3289]
  cd(0:1, 64  ) = [-21.5152, 0.3192]
  cd(0:1, 65  ) = [-20.9091, 0.3094]
  cd(0:1, 66  ) = [-20.303, 0.2997]
  cd(0:1, 67  ) = [-19.697, 0.2909]
  cd(0:1, 68  ) = [-19.0909, 0.2833]
  cd(0:1, 69  ) = [-18.4848, 0.2756]
  cd(0:1, 70  ) = [-17.8788, 0.2679]
  cd(0:1, 71  ) = [-17.2727, 0.2602]
  cd(0:1, 72  ) = [-16.6667, 0.2526]
  cd(0:1, 73  ) = [-16.0606, 0.2449]
  cd(0:1, 74  ) = [-15.4545, 0.2372]
  cd(0:1, 75  ) = [-14.8485, 0.2296]
  cd(0:1, 76  ) = [-14.2424, 0.2219]
  cd(0:1, 77  ) = [-13.6364, 0.2142]
  cd(0:1, 78  ) = [-13.0303, 0.2066]
  cd(0:1, 79  ) = [-12.4242, 0.1989]
  cd(0:1, 80  ) = [-11.8182, 0.1912]
  cd(0:1, 81  ) = [-11.2121, 0.1836]
  cd(0:1, 82  ) = [-10.6061, 0.1759]
  cd(0:1, 83  ) = [-10.0, 0.1683]
  cd(0:1, 84  ) = [-9.3939, 0.1606]
  cd(0:1, 85  ) = [-8.7879, 0.1529]
  cd(0:1, 86  ) = [-8.1818, 0.1452]
  cd(0:1, 87  ) = [-7.5758, 0.1375]
  cd(0:1, 88  ) = [-6.9697, 0.1298]
  cd(0:1, 89  ) = [-6.3636, 0.1221]
  cd(0:1, 90  ) = [-5.7576, 0.1144]
  cd(0:1, 91  ) = [-5.1515, 0.1067]
  cd(0:1, 92  ) = [-4.5455, 0.0991]
  cd(0:1, 93  ) = [-3.9394, 0.0914]
  cd(0:1, 94  ) = [-3.3333, 0.0847]
  cd(0:1, 95  ) = [-2.7273, 0.0825]
  cd(0:1, 96  ) = [-2.1212, 0.0831]
  cd(0:1, 97  ) = [-1.5152, 0.0837]
  cd(0:1, 98  ) = [-0.9091, 0.0843]
  cd(0:1, 99  ) = [-0.303, 0.0849]
  cd(0:1, 100  ) = [0.303, 0.0855]
  cd(0:1, 101  ) = [0.9091, 0.086]
  cd(0:1, 102  ) = [1.5152, 0.0866]
  cd(0:1, 103  ) = [2.1212, 0.0867]
  cd(0:1, 104  ) = [2.7273, 0.0864]
  cd(0:1, 105  ) = [3.3333, 0.0862]
  cd(0:1, 106  ) = [3.9394, 0.086]
  cd(0:1, 107  ) = [4.5455, 0.0858]
  cd(0:1, 108  ) = [5.1515, 0.0856]
  cd(0:1, 109  ) = [5.7576, 0.0854]
  cd(0:1, 110  ) = [6.3636, 0.0852]
  cd(0:1, 111  ) = [6.9697, 0.085]
  cd(0:1, 112  ) = [7.5758, 0.0848]
  cd(0:1, 113  ) = [8.1818, 0.0846]
  cd(0:1, 114  ) = [8.7879, 0.0849]
  cd(0:1, 115  ) = [9.3939, 0.0862]
  cd(0:1, 116  ) = [10.0, 0.0877]
  cd(0:1, 117  ) = [10.6061, 0.1084]
  cd(0:1, 118  ) = [11.2121, 0.1307]
  cd(0:1, 119  ) = [11.8182, 0.1486]
  cd(0:1, 120  ) = [12.4242, 0.1655]
  cd(0:1, 121  ) = [13.0303, 0.1816]
  cd(0:1, 122  ) = [13.6364, 0.1964]
  cd(0:1, 123  ) = [14.2424, 0.2098]
  cd(0:1, 124  ) = [14.8485, 0.2226]
  cd(0:1, 125  ) = [15.4545, 0.2342]
  cd(0:1, 126  ) = [16.0606, 0.245]
  cd(0:1, 127  ) = [16.6667, 0.2529]
  cd(0:1, 128  ) = [17.2727, 0.2607]
  cd(0:1, 129  ) = [17.8788, 0.2683]
  cd(0:1, 130  ) = [18.4848, 0.2759]
  cd(0:1, 131  ) = [19.0909, 0.2834]
  cd(0:1, 132  ) = [19.697, 0.291]
  cd(0:1, 133  ) = [20.303, 0.2997]
  cd(0:1, 134  ) = [20.9091, 0.3094]
  cd(0:1, 135  ) = [21.5152, 0.3192]
  cd(0:1, 136  ) = [22.1212, 0.3289]
  cd(0:1, 137  ) = [22.7273, 0.3387]
  cd(0:1, 138  ) = [23.3333, 0.3485]
  cd(0:1, 139  ) = [23.9394, 0.3582]
  cd(0:1, 140  ) = [24.5455, 0.368]
  cd(0:1, 141  ) = [25.1515, 0.3778]
  cd(0:1, 142  ) = [25.7576, 0.3875]
  cd(0:1, 143  ) = [26.3636, 0.3973]
  cd(0:1, 144  ) = [26.9697, 0.4071]
  cd(0:1, 145  ) = [27.5758, 0.4168]
  cd(0:1, 146  ) = [28.1818, 0.4266]
  cd(0:1, 147  ) = [28.7879, 0.4364]
  cd(0:1, 148  ) = [29.3939, 0.4461]
  cd(0:1, 149  ) = [30.0, 0.4559]
  cd(0:1, 150  ) = [33.0, 0.5148]
  cd(0:1, 151  ) = [36.0, 0.5733]
  cd(0:1, 152  ) = [39.0, 0.6318]
  cd(0:1, 153  ) = [42.0, 0.6923]
  cd(0:1, 154  ) = [45.0, 0.7538]
  cd(0:1, 155  ) = [48.0, 0.8153]
  cd(0:1, 156  ) = [51.0, 0.8751]
  cd(0:1, 157  ) = [54.0, 0.9317]
  cd(0:1, 158  ) = [57.0, 0.9882]
  cd(0:1, 159  ) = [60.0, 1.0447]
  cd(0:1, 160  ) = [63.0, 1.0889]
  cd(0:1, 161  ) = [66.0, 1.1331]
  cd(0:1, 162  ) = [69.0, 1.1772]
  cd(0:1, 163  ) = [72.0, 1.2091]
  cd(0:1, 164  ) = [75.0, 1.2349]
  cd(0:1, 165  ) = [78.0, 1.2607]
  cd(0:1, 166  ) = [81.0, 1.2791]
  cd(0:1, 167  ) = [84.0, 1.2827]
  cd(0:1, 168  ) = [87.0, 1.2864]
  cd(0:1, 169  ) = [90.0, 1.29]
  cd(0:1, 170  ) = [93.0, 1.2864]
  cd(0:1, 171  ) = [96.0, 1.2827]
  cd(0:1, 172  ) = [99.0, 1.2791]
  cd(0:1, 173  ) = [102.0, 1.2607]
  cd(0:1, 174  ) = [105.0, 1.2349]
  cd(0:1, 175  ) = [108.0, 1.2091]
  cd(0:1, 176  ) = [111.0, 1.1772]
  cd(0:1, 177  ) = [114.0, 1.1331]
  cd(0:1, 178  ) = [117.0, 1.0889]
  cd(0:1, 179  ) = [120.0, 1.0447]
  cd(0:1, 180  ) = [123.0, 0.9882]
  cd(0:1, 181  ) = [126.0, 0.9317]
  cd(0:1, 182  ) = [129.0, 0.8751]
  cd(0:1, 183  ) = [132.0, 0.8153]
  cd(0:1, 184  ) = [135.0, 0.7538]
  cd(0:1, 185  ) = [138.0, 0.6923]
  cd(0:1, 186  ) = [141.0, 0.6318]
  cd(0:1, 187  ) = [144.0, 0.5733]
  cd(0:1, 188  ) = [147.0, 0.5148]
  cd(0:1, 189  ) = [150.0, 0.4559]
  cd(0:1, 190  ) = [153.0, 0.4075]
  cd(0:1, 191  ) = [156.0, 0.3592]
  cd(0:1, 192  ) = [159.0, 0.3109]
  cd(0:1, 193  ) = [162.0, 0.2737]
  cd(0:1, 194  ) = [165.0, 0.242]
  cd(0:1, 195  ) = [168.0, 0.2104]
  cd(0:1, 196  ) = [171.0, 0.1857]
  cd(0:1, 197  ) = [174.0, 0.1747]
  cd(0:1, 198  ) = [177.0, 0.1638]
  cd(0:1, 199  ) = [180.0, 0.1529]

  IF ( y .EQ. cd(0,0) ) cd5 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd5 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd5 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd5_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd5 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd5_nrel2p8.pro' 
  ENDIF

!print,y,cd5

END FUNCTION calc_cd5_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd6_nrel2p8( y ) RESULT( cd6 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd6
INTEGER :: i

 cd6 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.0531]
  cd(0:1, 1  ) = [-177.0, 0.057]
  cd(0:1, 2  ) = [-174.0, 0.061]
  cd(0:1, 3  ) = [-171.0, 0.065]
  cd(0:1, 4  ) = [-168.0, 0.0851]
  cd(0:1, 5  ) = [-165.0, 0.1133]
  cd(0:1, 6  ) = [-162.0, 0.1416]
  cd(0:1, 7  ) = [-159.0, 0.1781]
  cd(0:1, 8  ) = [-156.0, 0.2307]
  cd(0:1, 9  ) = [-153.0, 0.2836]
  cd(0:1, 10  ) = [-150.0, 0.3366]
  cd(0:1, 11  ) = [-147.0, 0.4013]
  cd(0:1, 12  ) = [-144.0, 0.4639]
  cd(0:1, 13  ) = [-141.0, 0.5265]
  cd(0:1, 14  ) = [-138.0, 0.5917]
  cd(0:1, 15  ) = [-135.0, 0.6582]
  cd(0:1, 16  ) = [-132.0, 0.7247]
  cd(0:1, 17  ) = [-129.0, 0.7898]
  cd(0:1, 18  ) = [-126.0, 0.8522]
  cd(0:1, 19  ) = [-123.0, 0.9145]
  cd(0:1, 20  ) = [-120.0, 0.9768]
  cd(0:1, 21  ) = [-117.0, 1.0274]
  cd(0:1, 22  ) = [-114.0, 1.0781]
  cd(0:1, 23  ) = [-111.0, 1.1287]
  cd(0:1, 24  ) = [-108.0, 1.1673]
  cd(0:1, 25  ) = [-105.0, 1.1999]
  cd(0:1, 26  ) = [-102.0, 1.2326]
  cd(0:1, 27  ) = [-99.0, 1.2579]
  cd(0:1, 28  ) = [-96.0, 1.2686]
  cd(0:1, 29  ) = [-93.0, 1.2793]
  cd(0:1, 30  ) = [-90.0, 1.29]
  cd(0:1, 31  ) = [-87.0, 1.2793]
  cd(0:1, 32  ) = [-84.0, 1.2686]
  cd(0:1, 33  ) = [-81.0, 1.2579]
  cd(0:1, 34  ) = [-78.0, 1.2326]
  cd(0:1, 35  ) = [-75.0, 1.1999]
  cd(0:1, 36  ) = [-72.0, 1.1673]
  cd(0:1, 37  ) = [-69.0, 1.1287]
  cd(0:1, 38  ) = [-66.0, 1.0781]
  cd(0:1, 39  ) = [-63.0, 1.0274]
  cd(0:1, 40  ) = [-60.0, 0.9768]
  cd(0:1, 41  ) = [-57.0, 0.9145]
  cd(0:1, 42  ) = [-54.0, 0.8522]
  cd(0:1, 43  ) = [-51.0, 0.7898]
  cd(0:1, 44  ) = [-48.0, 0.7247]
  cd(0:1, 45  ) = [-45.0, 0.6582]
  cd(0:1, 46  ) = [-42.0, 0.5917]
  cd(0:1, 47  ) = [-39.0, 0.5265]
  cd(0:1, 48  ) = [-36.0, 0.4639]
  cd(0:1, 49  ) = [-33.0, 0.4013]
  cd(0:1, 50  ) = [-30.0, 0.3366]
  cd(0:1, 51  ) = [-29.3939, 0.3259]
  cd(0:1, 52  ) = [-28.7879, 0.3151]
  cd(0:1, 53  ) = [-28.1818, 0.3044]
  cd(0:1, 54  ) = [-27.5758, 0.2937]
  cd(0:1, 55  ) = [-26.9697, 0.283]
  cd(0:1, 56  ) = [-26.3636, 0.2723]
  cd(0:1, 57  ) = [-25.7576, 0.2617]
  cd(0:1, 58  ) = [-25.1515, 0.251]
  cd(0:1, 59  ) = [-24.5455, 0.2403]
  cd(0:1, 60  ) = [-23.9394, 0.2297]
  cd(0:1, 61  ) = [-23.3333, 0.219]
  cd(0:1, 62  ) = [-22.7273, 0.2084]
  cd(0:1, 63  ) = [-22.1212, 0.1978]
  cd(0:1, 64  ) = [-21.5152, 0.1871]
  cd(0:1, 65  ) = [-20.9091, 0.1765]
  cd(0:1, 66  ) = [-20.303, 0.1659]
  cd(0:1, 67  ) = [-19.697, 0.1579]
  cd(0:1, 68  ) = [-19.0909, 0.1527]
  cd(0:1, 69  ) = [-18.4848, 0.1474]
  cd(0:1, 70  ) = [-17.8788, 0.1421]
  cd(0:1, 71  ) = [-17.2727, 0.1369]
  cd(0:1, 72  ) = [-16.6667, 0.1316]
  cd(0:1, 73  ) = [-16.0606, 0.1264]
  cd(0:1, 74  ) = [-15.4545, 0.1212]
  cd(0:1, 75  ) = [-14.8485, 0.1159]
  cd(0:1, 76  ) = [-14.2424, 0.1107]
  cd(0:1, 77  ) = [-13.6364, 0.1056]
  cd(0:1, 78  ) = [-13.0303, 0.1004]
  cd(0:1, 79  ) = [-12.4242, 0.0952]
  cd(0:1, 80  ) = [-11.8182, 0.0901]
  cd(0:1, 81  ) = [-11.2121, 0.0849]
  cd(0:1, 82  ) = [-10.6061, 0.0798]
  cd(0:1, 83  ) = [-10.0, 0.0747]
  cd(0:1, 84  ) = [-9.3939, 0.0716]
  cd(0:1, 85  ) = [-8.7879, 0.0685]
  cd(0:1, 86  ) = [-8.1818, 0.0655]
  cd(0:1, 87  ) = [-7.5758, 0.0635]
  cd(0:1, 88  ) = [-6.9697, 0.0623]
  cd(0:1, 89  ) = [-6.3636, 0.0606]
  cd(0:1, 90  ) = [-5.7576, 0.0586]
  cd(0:1, 91  ) = [-5.1515, 0.0562]
  cd(0:1, 92  ) = [-4.5455, 0.0537]
  cd(0:1, 93  ) = [-3.9394, 0.0511]
  cd(0:1, 94  ) = [-3.3333, 0.0485]
  cd(0:1, 95  ) = [-2.7273, 0.0476]
  cd(0:1, 96  ) = [-2.1212, 0.048]
  cd(0:1, 97  ) = [-1.5152, 0.0483]
  cd(0:1, 98  ) = [-0.9091, 0.0485]
  cd(0:1, 99  ) = [-0.303, 0.0488]
  cd(0:1, 100  ) = [0.303, 0.049]
  cd(0:1, 101  ) = [0.9091, 0.0492]
  cd(0:1, 102  ) = [1.5152, 0.0494]
  cd(0:1, 103  ) = [2.1212, 0.0494]
  cd(0:1, 104  ) = [2.7273, 0.0493]
  cd(0:1, 105  ) = [3.3333, 0.0492]
  cd(0:1, 106  ) = [3.9394, 0.0491]
  cd(0:1, 107  ) = [4.5455, 0.0489]
  cd(0:1, 108  ) = [5.1515, 0.0488]
  cd(0:1, 109  ) = [5.7576, 0.0487]
  cd(0:1, 110  ) = [6.3636, 0.0485]
  cd(0:1, 111  ) = [6.9697, 0.0483]
  cd(0:1, 112  ) = [7.5758, 0.048]
  cd(0:1, 113  ) = [8.1818, 0.0477]
  cd(0:1, 114  ) = [8.7879, 0.0476]
  cd(0:1, 115  ) = [9.3939, 0.0474]
  cd(0:1, 116  ) = [10.0, 0.047]
  cd(0:1, 117  ) = [10.6061, 0.0518]
  cd(0:1, 118  ) = [11.2121, 0.0581]
  cd(0:1, 119  ) = [11.8182, 0.0647]
  cd(0:1, 120  ) = [12.4242, 0.0727]
  cd(0:1, 121  ) = [13.0303, 0.08]
  cd(0:1, 122  ) = [13.6364, 0.0864]
  cd(0:1, 123  ) = [14.2424, 0.0922]
  cd(0:1, 124  ) = [14.8485, 0.0976]
  cd(0:1, 125  ) = [15.4545, 0.1023]
  cd(0:1, 126  ) = [16.0606, 0.1072]
  cd(0:1, 127  ) = [16.6667, 0.1155]
  cd(0:1, 128  ) = [17.2727, 0.1237]
  cd(0:1, 129  ) = [17.8788, 0.1319]
  cd(0:1, 130  ) = [18.4848, 0.1401]
  cd(0:1, 131  ) = [19.0909, 0.1483]
  cd(0:1, 132  ) = [19.697, 0.1565]
  cd(0:1, 133  ) = [20.303, 0.1659]
  cd(0:1, 134  ) = [20.9091, 0.1765]
  cd(0:1, 135  ) = [21.5152, 0.1871]
  cd(0:1, 136  ) = [22.1212, 0.1978]
  cd(0:1, 137  ) = [22.7273, 0.2084]
  cd(0:1, 138  ) = [23.3333, 0.219]
  cd(0:1, 139  ) = [23.9394, 0.2297]
  cd(0:1, 140  ) = [24.5455, 0.2403]
  cd(0:1, 141  ) = [25.1515, 0.251]
  cd(0:1, 142  ) = [25.7576, 0.2617]
  cd(0:1, 143  ) = [26.3636, 0.2723]
  cd(0:1, 144  ) = [26.9697, 0.283]
  cd(0:1, 145  ) = [27.5758, 0.2937]
  cd(0:1, 146  ) = [28.1818, 0.3044]
  cd(0:1, 147  ) = [28.7879, 0.3151]
  cd(0:1, 148  ) = [29.3939, 0.3259]
  cd(0:1, 149  ) = [30.0, 0.3366]
  cd(0:1, 150  ) = [33.0, 0.4013]
  cd(0:1, 151  ) = [36.0, 0.4639]
  cd(0:1, 152  ) = [39.0, 0.5265]
  cd(0:1, 153  ) = [42.0, 0.5917]
  cd(0:1, 154  ) = [45.0, 0.6582]
  cd(0:1, 155  ) = [48.0, 0.7247]
  cd(0:1, 156  ) = [51.0, 0.7898]
  cd(0:1, 157  ) = [54.0, 0.8522]
  cd(0:1, 158  ) = [57.0, 0.9145]
  cd(0:1, 159  ) = [60.0, 0.9768]
  cd(0:1, 160  ) = [63.0, 1.0274]
  cd(0:1, 161  ) = [66.0, 1.0781]
  cd(0:1, 162  ) = [69.0, 1.1287]
  cd(0:1, 163  ) = [72.0, 1.1673]
  cd(0:1, 164  ) = [75.0, 1.1999]
  cd(0:1, 165  ) = [78.0, 1.2326]
  cd(0:1, 166  ) = [81.0, 1.2579]
  cd(0:1, 167  ) = [84.0, 1.2686]
  cd(0:1, 168  ) = [87.0, 1.2793]
  cd(0:1, 169  ) = [90.0, 1.29]
  cd(0:1, 170  ) = [93.0, 1.2793]
  cd(0:1, 171  ) = [96.0, 1.2686]
  cd(0:1, 172  ) = [99.0, 1.2579]
  cd(0:1, 173  ) = [102.0, 1.2326]
  cd(0:1, 174  ) = [105.0, 1.1999]
  cd(0:1, 175  ) = [108.0, 1.1673]
  cd(0:1, 176  ) = [111.0, 1.1287]
  cd(0:1, 177  ) = [114.0, 1.0781]
  cd(0:1, 178  ) = [117.0, 1.0274]
  cd(0:1, 179  ) = [120.0, 0.9768]
  cd(0:1, 180  ) = [123.0, 0.9145]
  cd(0:1, 181  ) = [126.0, 0.8522]
  cd(0:1, 182  ) = [129.0, 0.7898]
  cd(0:1, 183  ) = [132.0, 0.7247]
  cd(0:1, 184  ) = [135.0, 0.6582]
  cd(0:1, 185  ) = [138.0, 0.5917]
  cd(0:1, 186  ) = [141.0, 0.5265]
  cd(0:1, 187  ) = [144.0, 0.4639]
  cd(0:1, 188  ) = [147.0, 0.4013]
  cd(0:1, 189  ) = [150.0, 0.3366]
  cd(0:1, 190  ) = [153.0, 0.2836]
  cd(0:1, 191  ) = [156.0, 0.2307]
  cd(0:1, 192  ) = [159.0, 0.1781]
  cd(0:1, 193  ) = [162.0, 0.1416]
  cd(0:1, 194  ) = [165.0, 0.1133]
  cd(0:1, 195  ) = [168.0, 0.0851]
  cd(0:1, 196  ) = [171.0, 0.065]
  cd(0:1, 197  ) = [174.0, 0.061]
  cd(0:1, 198  ) = [177.0, 0.057]
  cd(0:1, 199  ) = [180.0, 0.0531]

  IF ( y .EQ. cd(0,0) ) cd6 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd6 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd6 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd6_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd6 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd6_nrel2p8.pro' 
  ENDIF

!print,y,cd6

END FUNCTION calc_cd6_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd7_nrel2p8( y ) RESULT( cd7 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd7
INTEGER :: i

 cd7 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.0104]
  cd(0:1, 1  ) = [-177.0, 0.0105]
  cd(0:1, 2  ) = [-174.0, 0.0105]
  cd(0:1, 3  ) = [-171.0, 0.0106]
  cd(0:1, 4  ) = [-168.0, 0.0275]
  cd(0:1, 5  ) = [-165.0, 0.0527]
  cd(0:1, 6  ) = [-162.0, 0.078]
  cd(0:1, 7  ) = [-159.0, 0.1125]
  cd(0:1, 8  ) = [-156.0, 0.1653]
  cd(0:1, 9  ) = [-153.0, 0.2181]
  cd(0:1, 10  ) = [-150.0, 0.2709]
  cd(0:1, 11  ) = [-147.0, 0.3359]
  cd(0:1, 12  ) = [-144.0, 0.4009]
  cd(0:1, 13  ) = [-141.0, 0.4658]
  cd(0:1, 14  ) = [-138.0, 0.5337]
  cd(0:1, 15  ) = [-135.0, 0.6031]
  cd(0:1, 16  ) = [-132.0, 0.6725]
  cd(0:1, 17  ) = [-129.0, 0.7407]
  cd(0:1, 18  ) = [-126.0, 0.8063]
  cd(0:1, 19  ) = [-123.0, 0.872]
  cd(0:1, 20  ) = [-120.0, 0.9377]
  cd(0:1, 21  ) = [-117.0, 0.992]
  cd(0:1, 22  ) = [-114.0, 1.0464]
  cd(0:1, 23  ) = [-111.0, 1.1007]
  cd(0:1, 24  ) = [-108.0, 1.1432]
  cd(0:1, 25  ) = [-105.0, 1.1798]
  cd(0:1, 26  ) = [-102.0, 1.2164]
  cd(0:1, 27  ) = [-99.0, 1.2457]
  cd(0:1, 28  ) = [-96.0, 1.2604]
  cd(0:1, 29  ) = [-93.0, 1.2752]
  cd(0:1, 30  ) = [-90.0, 1.29]
  cd(0:1, 31  ) = [-87.0, 1.2752]
  cd(0:1, 32  ) = [-84.0, 1.2604]
  cd(0:1, 33  ) = [-81.0, 1.2457]
  cd(0:1, 34  ) = [-78.0, 1.2164]
  cd(0:1, 35  ) = [-75.0, 1.1798]
  cd(0:1, 36  ) = [-72.0, 1.1432]
  cd(0:1, 37  ) = [-69.0, 1.1007]
  cd(0:1, 38  ) = [-66.0, 1.0464]
  cd(0:1, 39  ) = [-63.0, 0.992]
  cd(0:1, 40  ) = [-60.0, 0.9377]
  cd(0:1, 41  ) = [-57.0, 0.872]
  cd(0:1, 42  ) = [-54.0, 0.8063]
  cd(0:1, 43  ) = [-51.0, 0.7407]
  cd(0:1, 44  ) = [-48.0, 0.6725]
  cd(0:1, 45  ) = [-45.0, 0.6031]
  cd(0:1, 46  ) = [-42.0, 0.5337]
  cd(0:1, 47  ) = [-39.0, 0.4658]
  cd(0:1, 48  ) = [-36.0, 0.4009]
  cd(0:1, 49  ) = [-33.0, 0.3359]
  cd(0:1, 50  ) = [-30.0, 0.2709]
  cd(0:1, 51  ) = [-29.3939, 0.2602]
  cd(0:1, 52  ) = [-28.7879, 0.2496]
  cd(0:1, 53  ) = [-28.1818, 0.2389]
  cd(0:1, 54  ) = [-27.5758, 0.2282]
  cd(0:1, 55  ) = [-26.9697, 0.2176]
  cd(0:1, 56  ) = [-26.3636, 0.2069]
  cd(0:1, 57  ) = [-25.7576, 0.1962]
  cd(0:1, 58  ) = [-25.1515, 0.1856]
  cd(0:1, 59  ) = [-24.5455, 0.1749]
  cd(0:1, 60  ) = [-23.9394, 0.1642]
  cd(0:1, 61  ) = [-23.3333, 0.1536]
  cd(0:1, 62  ) = [-22.7273, 0.1429]
  cd(0:1, 63  ) = [-22.1212, 0.1322]
  cd(0:1, 64  ) = [-21.5152, 0.1216]
  cd(0:1, 65  ) = [-20.9091, 0.1109]
  cd(0:1, 66  ) = [-20.303, 0.1002]
  cd(0:1, 67  ) = [-19.697, 0.0931]
  cd(0:1, 68  ) = [-19.0909, 0.0894]
  cd(0:1, 69  ) = [-18.4848, 0.0858]
  cd(0:1, 70  ) = [-17.8788, 0.0822]
  cd(0:1, 71  ) = [-17.2727, 0.0785]
  cd(0:1, 72  ) = [-16.6667, 0.0749]
  cd(0:1, 73  ) = [-16.0606, 0.0712]
  cd(0:1, 74  ) = [-15.4545, 0.0676]
  cd(0:1, 75  ) = [-14.8485, 0.0639]
  cd(0:1, 76  ) = [-14.2424, 0.0603]
  cd(0:1, 77  ) = [-13.6364, 0.0567]
  cd(0:1, 78  ) = [-13.0303, 0.053]
  cd(0:1, 79  ) = [-12.4242, 0.0494]
  cd(0:1, 80  ) = [-11.8182, 0.0457]
  cd(0:1, 81  ) = [-11.2121, 0.0421]
  cd(0:1, 82  ) = [-10.6061, 0.0385]
  cd(0:1, 83  ) = [-10.0, 0.0348]
  cd(0:1, 84  ) = [-9.3939, 0.0342]
  cd(0:1, 85  ) = [-8.7879, 0.0336]
  cd(0:1, 86  ) = [-8.1818, 0.033]
  cd(0:1, 87  ) = [-7.5758, 0.0327]
  cd(0:1, 88  ) = [-6.9697, 0.0325]
  cd(0:1, 89  ) = [-6.3636, 0.0322]
  cd(0:1, 90  ) = [-5.7576, 0.0318]
  cd(0:1, 91  ) = [-5.1515, 0.0313]
  cd(0:1, 92  ) = [-4.5455, 0.0308]
  cd(0:1, 93  ) = [-3.9394, 0.0303]
  cd(0:1, 94  ) = [-3.3333, 0.0296]
  cd(0:1, 95  ) = [-2.7273, 0.0293]
  cd(0:1, 96  ) = [-2.1212, 0.0294]
  cd(0:1, 97  ) = [-1.5152, 0.0294]
  cd(0:1, 98  ) = [-0.9091, 0.0294]
  cd(0:1, 99  ) = [-0.303, 0.0295]
  cd(0:1, 100  ) = [0.303, 0.0295]
  cd(0:1, 101  ) = [0.9091, 0.0295]
  cd(0:1, 102  ) = [1.5152, 0.0295]
  cd(0:1, 103  ) = [2.1212, 0.0296]
  cd(0:1, 104  ) = [2.7273, 0.0296]
  cd(0:1, 105  ) = [3.3333, 0.0296]
  cd(0:1, 106  ) = [3.9394, 0.0297]
  cd(0:1, 107  ) = [4.5455, 0.0296]
  cd(0:1, 108  ) = [5.1515, 0.0296]
  cd(0:1, 109  ) = [5.7576, 0.0297]
  cd(0:1, 110  ) = [6.3636, 0.0297]
  cd(0:1, 111  ) = [6.9697, 0.0297]
  cd(0:1, 112  ) = [7.5758, 0.0297]
  cd(0:1, 113  ) = [8.1818, 0.0296]
  cd(0:1, 114  ) = [8.7879, 0.0297]
  cd(0:1, 115  ) = [9.3939, 0.0296]
  cd(0:1, 116  ) = [10.0, 0.0295]
  cd(0:1, 117  ) = [10.6061, 0.0293]
  cd(0:1, 118  ) = [11.2121, 0.0294]
  cd(0:1, 119  ) = [11.8182, 0.0301]
  cd(0:1, 120  ) = [12.4242, 0.0333]
  cd(0:1, 121  ) = [13.0303, 0.0356]
  cd(0:1, 122  ) = [13.6364, 0.0373]
  cd(0:1, 123  ) = [14.2424, 0.0387]
  cd(0:1, 124  ) = [14.8485, 0.0397]
  cd(0:1, 125  ) = [15.4545, 0.0403]
  cd(0:1, 126  ) = [16.0606, 0.0415]
  cd(0:1, 127  ) = [16.6667, 0.0497]
  cd(0:1, 128  ) = [17.2727, 0.0579]
  cd(0:1, 129  ) = [17.8788, 0.0662]
  cd(0:1, 130  ) = [18.4848, 0.0744]
  cd(0:1, 131  ) = [19.0909, 0.0826]
  cd(0:1, 132  ) = [19.697, 0.0908]
  cd(0:1, 133  ) = [20.303, 0.1002]
  cd(0:1, 134  ) = [20.9091, 0.1109]
  cd(0:1, 135  ) = [21.5152, 0.1216]
  cd(0:1, 136  ) = [22.1212, 0.1322]
  cd(0:1, 137  ) = [22.7273, 0.1429]
  cd(0:1, 138  ) = [23.3333, 0.1536]
  cd(0:1, 139  ) = [23.9394, 0.1642]
  cd(0:1, 140  ) = [24.5455, 0.1749]
  cd(0:1, 141  ) = [25.1515, 0.1856]
  cd(0:1, 142  ) = [25.7576, 0.1962]
  cd(0:1, 143  ) = [26.3636, 0.2069]
  cd(0:1, 144  ) = [26.9697, 0.2176]
  cd(0:1, 145  ) = [27.5758, 0.2282]
  cd(0:1, 146  ) = [28.1818, 0.2389]
  cd(0:1, 147  ) = [28.7879, 0.2496]
  cd(0:1, 148  ) = [29.3939, 0.2602]
  cd(0:1, 149  ) = [30.0, 0.2709]
  cd(0:1, 150  ) = [33.0, 0.3359]
  cd(0:1, 151  ) = [36.0, 0.4009]
  cd(0:1, 152  ) = [39.0, 0.4658]
  cd(0:1, 153  ) = [42.0, 0.5337]
  cd(0:1, 154  ) = [45.0, 0.6031]
  cd(0:1, 155  ) = [48.0, 0.6725]
  cd(0:1, 156  ) = [51.0, 0.7407]
  cd(0:1, 157  ) = [54.0, 0.8063]
  cd(0:1, 158  ) = [57.0, 0.872]
  cd(0:1, 159  ) = [60.0, 0.9377]
  cd(0:1, 160  ) = [63.0, 0.992]
  cd(0:1, 161  ) = [66.0, 1.0464]
  cd(0:1, 162  ) = [69.0, 1.1007]
  cd(0:1, 163  ) = [72.0, 1.1432]
  cd(0:1, 164  ) = [75.0, 1.1798]
  cd(0:1, 165  ) = [78.0, 1.2164]
  cd(0:1, 166  ) = [81.0, 1.2457]
  cd(0:1, 167  ) = [84.0, 1.2604]
  cd(0:1, 168  ) = [87.0, 1.2752]
  cd(0:1, 169  ) = [90.0, 1.29]
  cd(0:1, 170  ) = [93.0, 1.2752]
  cd(0:1, 171  ) = [96.0, 1.2604]
  cd(0:1, 172  ) = [99.0, 1.2457]
  cd(0:1, 173  ) = [102.0, 1.2164]
  cd(0:1, 174  ) = [105.0, 1.1798]
  cd(0:1, 175  ) = [108.0, 1.1432]
  cd(0:1, 176  ) = [111.0, 1.1007]
  cd(0:1, 177  ) = [114.0, 1.0464]
  cd(0:1, 178  ) = [117.0, 0.992]
  cd(0:1, 179  ) = [120.0, 0.9377]
  cd(0:1, 180  ) = [123.0, 0.872]
  cd(0:1, 181  ) = [126.0, 0.8063]
  cd(0:1, 182  ) = [129.0, 0.7407]
  cd(0:1, 183  ) = [132.0, 0.6725]
  cd(0:1, 184  ) = [135.0, 0.6031]
  cd(0:1, 185  ) = [138.0, 0.5337]
  cd(0:1, 186  ) = [141.0, 0.4658]
  cd(0:1, 187  ) = [144.0, 0.4009]
  cd(0:1, 188  ) = [147.0, 0.3359]
  cd(0:1, 189  ) = [150.0, 0.2709]
  cd(0:1, 190  ) = [153.0, 0.2181]
  cd(0:1, 191  ) = [156.0, 0.1653]
  cd(0:1, 192  ) = [159.0, 0.1125]
  cd(0:1, 193  ) = [162.0, 0.078]
  cd(0:1, 194  ) = [165.0, 0.0527]
  cd(0:1, 195  ) = [168.0, 0.0275]
  cd(0:1, 196  ) = [171.0, 0.0106]
  cd(0:1, 197  ) = [174.0, 0.0105]
  cd(0:1, 198  ) = [177.0, 0.0105]
  cd(0:1, 199  ) = [180.0, 0.0104]

  IF ( y .EQ. cd(0,0) ) cd7 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd7 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd7 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd7_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd7 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd7_nrel2p8.pro' 
  ENDIF

!print,y,cd7

END FUNCTION calc_cd7_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd8_nrel2p8( y ) RESULT( cd8 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd8
INTEGER :: i

 cd8 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.0234]
  cd(0:1, 1  ) = [-177.0, 0.0234]
  cd(0:1, 2  ) = [-174.0, 0.0267]
  cd(0:1, 3  ) = [-171.0, 0.0366]
  cd(0:1, 4  ) = [-168.0, 0.0584]
  cd(0:1, 5  ) = [-165.0, 0.0862]
  cd(0:1, 6  ) = [-162.0, 0.1141]
  cd(0:1, 7  ) = [-159.0, 0.1506]
  cd(0:1, 8  ) = [-156.0, 0.2047]
  cd(0:1, 9  ) = [-153.0, 0.2587]
  cd(0:1, 10  ) = [-150.0, 0.3128]
  cd(0:1, 11  ) = [-147.0, 0.3791]
  cd(0:1, 12  ) = [-144.0, 0.4455]
  cd(0:1, 13  ) = [-141.0, 0.5119]
  cd(0:1, 14  ) = [-138.0, 0.5812]
  cd(0:1, 15  ) = [-135.0, 0.652]
  cd(0:1, 16  ) = [-132.0, 0.7227]
  cd(0:1, 17  ) = [-129.0, 0.7921]
  cd(0:1, 18  ) = [-126.0, 0.8588]
  cd(0:1, 19  ) = [-123.0, 0.9255]
  cd(0:1, 20  ) = [-120.0, 0.9922]
  cd(0:1, 21  ) = [-117.0, 1.0469]
  cd(0:1, 22  ) = [-114.0, 1.1017]
  cd(0:1, 23  ) = [-111.0, 1.1564]
  cd(0:1, 24  ) = [-108.0, 1.1987]
  cd(0:1, 25  ) = [-105.0, 1.2349]
  cd(0:1, 26  ) = [-102.0, 1.2711]
  cd(0:1, 27  ) = [-99.0, 1.2997]
  cd(0:1, 28  ) = [-96.0, 1.3132]
  cd(0:1, 29  ) = [-93.0, 1.3267]
  cd(0:1, 30  ) = [-90.0, 1.3401]
  cd(0:1, 31  ) = [-87.0, 1.3267]
  cd(0:1, 32  ) = [-84.0, 1.3132]
  cd(0:1, 33  ) = [-81.0, 1.2997]
  cd(0:1, 34  ) = [-78.0, 1.2711]
  cd(0:1, 35  ) = [-75.0, 1.2349]
  cd(0:1, 36  ) = [-72.0, 1.1987]
  cd(0:1, 37  ) = [-69.0, 1.1564]
  cd(0:1, 38  ) = [-66.0, 1.1017]
  cd(0:1, 39  ) = [-63.0, 1.0469]
  cd(0:1, 40  ) = [-60.0, 0.9922]
  cd(0:1, 41  ) = [-57.0, 0.9255]
  cd(0:1, 42  ) = [-54.0, 0.8588]
  cd(0:1, 43  ) = [-51.0, 0.7921]
  cd(0:1, 44  ) = [-48.0, 0.7227]
  cd(0:1, 45  ) = [-45.0, 0.652]
  cd(0:1, 46  ) = [-42.0, 0.5812]
  cd(0:1, 47  ) = [-39.0, 0.5116]
  cd(0:1, 48  ) = [-36.0, 0.4445]
  cd(0:1, 49  ) = [-33.0, 0.3782]
  cd(0:1, 50  ) = [-30.0, 0.3128]
  cd(0:1, 51  ) = [-29.3939, 0.3014]
  cd(0:1, 52  ) = [-28.7879, 0.2901]
  cd(0:1, 53  ) = [-28.1818, 0.2788]
  cd(0:1, 54  ) = [-27.5758, 0.2676]
  cd(0:1, 55  ) = [-26.9697, 0.2565]
  cd(0:1, 56  ) = [-26.3636, 0.2454]
  cd(0:1, 57  ) = [-25.7576, 0.2343]
  cd(0:1, 58  ) = [-25.1515, 0.2234]
  cd(0:1, 59  ) = [-24.5455, 0.2124]
  cd(0:1, 60  ) = [-23.9394, 0.2015]
  cd(0:1, 61  ) = [-23.3333, 0.1908]
  cd(0:1, 62  ) = [-22.7273, 0.1801]
  cd(0:1, 63  ) = [-22.1212, 0.1694]
  cd(0:1, 64  ) = [-21.5152, 0.1588]
  cd(0:1, 65  ) = [-20.9091, 0.1484]
  cd(0:1, 66  ) = [-20.303, 0.1379]
  cd(0:1, 67  ) = [-19.697, 0.1299]
  cd(0:1, 68  ) = [-19.0909, 0.1245]
  cd(0:1, 69  ) = [-18.4848, 0.1191]
  cd(0:1, 70  ) = [-17.8788, 0.1137]
  cd(0:1, 71  ) = [-17.2727, 0.1083]
  cd(0:1, 72  ) = [-16.6667, 0.1023]
  cd(0:1, 73  ) = [-16.0606, 0.0961]
  cd(0:1, 74  ) = [-15.4545, 0.0904]
  cd(0:1, 75  ) = [-14.8485, 0.0848]
  cd(0:1, 76  ) = [-14.2424, 0.0797]
  cd(0:1, 77  ) = [-13.6364, 0.0746]
  cd(0:1, 78  ) = [-13.0303, 0.0695]
  cd(0:1, 79  ) = [-12.4242, 0.0644]
  cd(0:1, 80  ) = [-11.8182, 0.0592]
  cd(0:1, 81  ) = [-11.2121, 0.0538]
  cd(0:1, 82  ) = [-10.6061, 0.0483]
  cd(0:1, 83  ) = [-10.0, 0.0427]
  cd(0:1, 84  ) = [-9.3939, 0.0398]
  cd(0:1, 85  ) = [-8.7879, 0.0369]
  cd(0:1, 86  ) = [-8.1818, 0.0337]
  cd(0:1, 87  ) = [-7.5758, 0.0312]
  cd(0:1, 88  ) = [-6.9697, 0.0289]
  cd(0:1, 89  ) = [-6.3636, 0.0267]
  cd(0:1, 90  ) = [-5.7576, 0.0248]
  cd(0:1, 91  ) = [-5.1515, 0.0234]
  cd(0:1, 92  ) = [-4.5455, 0.0221]
  cd(0:1, 93  ) = [-3.9394, 0.021]
  cd(0:1, 94  ) = [-3.3333, 0.0205]
  cd(0:1, 95  ) = [-2.7273, 0.02]
  cd(0:1, 96  ) = [-2.1212, 0.0196]
  cd(0:1, 97  ) = [-1.5152, 0.0194]
  cd(0:1, 98  ) = [-0.9091, 0.0193]
  cd(0:1, 99  ) = [-0.303, 0.0193]
  cd(0:1, 100  ) = [0.303, 0.0193]
  cd(0:1, 101  ) = [0.9091, 0.0193]
  cd(0:1, 102  ) = [1.5152, 0.0193]
  cd(0:1, 103  ) = [2.1212, 0.0195]
  cd(0:1, 104  ) = [2.7273, 0.0196]
  cd(0:1, 105  ) = [3.3333, 0.0197]
  cd(0:1, 106  ) = [3.9394, 0.0199]
  cd(0:1, 107  ) = [4.5455, 0.0202]
  cd(0:1, 108  ) = [5.1515, 0.0204]
  cd(0:1, 109  ) = [5.7576, 0.0207]
  cd(0:1, 110  ) = [6.3636, 0.021]
  cd(0:1, 111  ) = [6.9697, 0.0214]
  cd(0:1, 112  ) = [7.5758, 0.0218]
  cd(0:1, 113  ) = [8.1818, 0.0223]
  cd(0:1, 114  ) = [8.7879, 0.023]
  cd(0:1, 115  ) = [9.3939, 0.0243]
  cd(0:1, 116  ) = [10.0, 0.0258]
  cd(0:1, 117  ) = [10.6061, 0.0279]
  cd(0:1, 118  ) = [11.2121, 0.0299]
  cd(0:1, 119  ) = [11.8182, 0.0326]
  cd(0:1, 120  ) = [12.4242, 0.0377]
  cd(0:1, 121  ) = [13.0303, 0.0426]
  cd(0:1, 122  ) = [13.6364, 0.0477]
  cd(0:1, 123  ) = [14.2424, 0.0526]
  cd(0:1, 124  ) = [14.8485, 0.0577]
  cd(0:1, 125  ) = [15.4545, 0.0629]
  cd(0:1, 126  ) = [16.0606, 0.0686]
  cd(0:1, 127  ) = [16.6667, 0.0789]
  cd(0:1, 128  ) = [17.2727, 0.0892]
  cd(0:1, 129  ) = [17.8788, 0.0995]
  cd(0:1, 130  ) = [18.4848, 0.1097]
  cd(0:1, 131  ) = [19.0909, 0.1197]
  cd(0:1, 132  ) = [19.697, 0.1283]
  cd(0:1, 133  ) = [20.303, 0.1379]
  cd(0:1, 134  ) = [20.9091, 0.1484]
  cd(0:1, 135  ) = [21.5152, 0.1588]
  cd(0:1, 136  ) = [22.1212, 0.1694]
  cd(0:1, 137  ) = [22.7273, 0.1801]
  cd(0:1, 138  ) = [23.3333, 0.1908]
  cd(0:1, 139  ) = [23.9394, 0.2015]
  cd(0:1, 140  ) = [24.5455, 0.2124]
  cd(0:1, 141  ) = [25.1515, 0.2234]
  cd(0:1, 142  ) = [25.7576, 0.2343]
  cd(0:1, 143  ) = [26.3636, 0.2454]
  cd(0:1, 144  ) = [26.9697, 0.2565]
  cd(0:1, 145  ) = [27.5758, 0.2676]
  cd(0:1, 146  ) = [28.1818, 0.2788]
  cd(0:1, 147  ) = [28.7879, 0.2901]
  cd(0:1, 148  ) = [29.3939, 0.3014]
  cd(0:1, 149  ) = [30.0, 0.3128]
  cd(0:1, 150  ) = [33.0, 0.3782]
  cd(0:1, 151  ) = [36.0, 0.4445]
  cd(0:1, 152  ) = [39.0, 0.5116]
  cd(0:1, 153  ) = [42.0, 0.5812]
  cd(0:1, 154  ) = [45.0, 0.652]
  cd(0:1, 155  ) = [48.0, 0.7227]
  cd(0:1, 156  ) = [51.0, 0.7921]
  cd(0:1, 157  ) = [54.0, 0.8588]
  cd(0:1, 158  ) = [57.0, 0.9255]
  cd(0:1, 159  ) = [60.0, 0.9922]
  cd(0:1, 160  ) = [63.0, 1.0469]
  cd(0:1, 161  ) = [66.0, 1.1017]
  cd(0:1, 162  ) = [69.0, 1.1564]
  cd(0:1, 163  ) = [72.0, 1.1987]
  cd(0:1, 164  ) = [75.0, 1.2349]
  cd(0:1, 165  ) = [78.0, 1.2711]
  cd(0:1, 166  ) = [81.0, 1.2997]
  cd(0:1, 167  ) = [84.0, 1.3132]
  cd(0:1, 168  ) = [87.0, 1.3267]
  cd(0:1, 169  ) = [90.0, 1.3401]
  cd(0:1, 170  ) = [93.0, 1.3267]
  cd(0:1, 171  ) = [96.0, 1.3132]
  cd(0:1, 172  ) = [99.0, 1.2997]
  cd(0:1, 173  ) = [102.0, 1.2711]
  cd(0:1, 174  ) = [105.0, 1.2349]
  cd(0:1, 175  ) = [108.0, 1.1987]
  cd(0:1, 176  ) = [111.0, 1.1564]
  cd(0:1, 177  ) = [114.0, 1.1017]
  cd(0:1, 178  ) = [117.0, 1.0469]
  cd(0:1, 179  ) = [120.0, 0.9922]
  cd(0:1, 180  ) = [123.0, 0.9255]
  cd(0:1, 181  ) = [126.0, 0.8588]
  cd(0:1, 182  ) = [129.0, 0.7921]
  cd(0:1, 183  ) = [132.0, 0.7227]
  cd(0:1, 184  ) = [135.0, 0.652]
  cd(0:1, 185  ) = [138.0, 0.5812]
  cd(0:1, 186  ) = [141.0, 0.5119]
  cd(0:1, 187  ) = [144.0, 0.4455]
  cd(0:1, 188  ) = [147.0, 0.3791]
  cd(0:1, 189  ) = [150.0, 0.3128]
  cd(0:1, 190  ) = [153.0, 0.2587]
  cd(0:1, 191  ) = [156.0, 0.2047]
  cd(0:1, 192  ) = [159.0, 0.1506]
  cd(0:1, 193  ) = [162.0, 0.1142]
  cd(0:1, 194  ) = [165.0, 0.0866]
  cd(0:1, 195  ) = [168.0, 0.059]
  cd(0:1, 196  ) = [171.0, 0.0372]
  cd(0:1, 197  ) = [174.0, 0.0271]
  cd(0:1, 198  ) = [177.0, 0.0236]
  cd(0:1, 199  ) = [180.0, 0.0234]

  IF ( y .EQ. cd(0,0) ) cd8 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd8 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd8 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd8_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd8 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd8_nrel2p8.pro' 
  ENDIF

!print,y,cd8

END FUNCTION calc_cd8_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd9_nrel2p8( y ) RESULT( cd9 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd9
INTEGER :: i

 cd9 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.0437]
  cd(0:1, 1  ) = [-177.0, 0.0437]
  cd(0:1, 2  ) = [-174.0, 0.0519]
  cd(0:1, 3  ) = [-171.0, 0.0768]
  cd(0:1, 4  ) = [-168.0, 0.1062]
  cd(0:1, 5  ) = [-165.0, 0.1379]
  cd(0:1, 6  ) = [-162.0, 0.1696]
  cd(0:1, 7  ) = [-159.0, 0.2094]
  cd(0:1, 8  ) = [-156.0, 0.2653]
  cd(0:1, 9  ) = [-153.0, 0.3212]
  cd(0:1, 10  ) = [-150.0, 0.3771]
  cd(0:1, 11  ) = [-147.0, 0.4457]
  cd(0:1, 12  ) = [-144.0, 0.5142]
  cd(0:1, 13  ) = [-141.0, 0.5827]
  cd(0:1, 14  ) = [-138.0, 0.6541]
  cd(0:1, 15  ) = [-135.0, 0.7268]
  cd(0:1, 16  ) = [-132.0, 0.7996]
  cd(0:1, 17  ) = [-129.0, 0.8709]
  cd(0:1, 18  ) = [-126.0, 0.939]
  cd(0:1, 19  ) = [-123.0, 1.0072]
  cd(0:1, 20  ) = [-120.0, 1.0754]
  cd(0:1, 21  ) = [-117.0, 1.1306]
  cd(0:1, 22  ) = [-114.0, 1.1859]
  cd(0:1, 23  ) = [-111.0, 1.2411]
  cd(0:1, 24  ) = [-108.0, 1.2832]
  cd(0:1, 25  ) = [-105.0, 1.3187]
  cd(0:1, 26  ) = [-102.0, 1.3543]
  cd(0:1, 27  ) = [-99.0, 1.3818]
  cd(0:1, 28  ) = [-96.0, 1.3932]
  cd(0:1, 29  ) = [-93.0, 1.4046]
  cd(0:1, 30  ) = [-90.0, 1.416]
  cd(0:1, 31  ) = [-87.0, 1.4046]
  cd(0:1, 32  ) = [-84.0, 1.3932]
  cd(0:1, 33  ) = [-81.0, 1.3818]
  cd(0:1, 34  ) = [-78.0, 1.3543]
  cd(0:1, 35  ) = [-75.0, 1.3187]
  cd(0:1, 36  ) = [-72.0, 1.2832]
  cd(0:1, 37  ) = [-69.0, 1.2411]
  cd(0:1, 38  ) = [-66.0, 1.1859]
  cd(0:1, 39  ) = [-63.0, 1.1306]
  cd(0:1, 40  ) = [-60.0, 1.0754]
  cd(0:1, 41  ) = [-57.0, 1.0072]
  cd(0:1, 42  ) = [-54.0, 0.939]
  cd(0:1, 43  ) = [-51.0, 0.8709]
  cd(0:1, 44  ) = [-48.0, 0.7997]
  cd(0:1, 45  ) = [-45.0, 0.7269]
  cd(0:1, 46  ) = [-42.0, 0.6541]
  cd(0:1, 47  ) = [-39.0, 0.5819]
  cd(0:1, 48  ) = [-36.0, 0.5116]
  cd(0:1, 49  ) = [-33.0, 0.4433]
  cd(0:1, 50  ) = [-30.0, 0.3771]
  cd(0:1, 51  ) = [-29.3939, 0.3649]
  cd(0:1, 52  ) = [-28.7879, 0.3526]
  cd(0:1, 53  ) = [-28.1818, 0.3403]
  cd(0:1, 54  ) = [-27.5758, 0.3283]
  cd(0:1, 55  ) = [-26.9697, 0.3165]
  cd(0:1, 56  ) = [-26.3636, 0.3046]
  cd(0:1, 57  ) = [-25.7576, 0.293]
  cd(0:1, 58  ) = [-25.1515, 0.2817]
  cd(0:1, 59  ) = [-24.5455, 0.2703]
  cd(0:1, 60  ) = [-23.9394, 0.259]
  cd(0:1, 61  ) = [-23.3333, 0.2482]
  cd(0:1, 62  ) = [-22.7273, 0.2374]
  cd(0:1, 63  ) = [-22.1212, 0.2266]
  cd(0:1, 64  ) = [-21.5152, 0.2163]
  cd(0:1, 65  ) = [-20.9091, 0.2061]
  cd(0:1, 66  ) = [-20.303, 0.1959]
  cd(0:1, 67  ) = [-19.697, 0.1867]
  cd(0:1, 68  ) = [-19.0909, 0.1786]
  cd(0:1, 69  ) = [-18.4848, 0.1705]
  cd(0:1, 70  ) = [-17.8788, 0.1625]
  cd(0:1, 71  ) = [-17.2727, 0.1544]
  cd(0:1, 72  ) = [-16.6667, 0.1451]
  cd(0:1, 73  ) = [-16.0606, 0.1358]
  cd(0:1, 74  ) = [-15.4545, 0.1271]
  cd(0:1, 75  ) = [-14.8485, 0.1188]
  cd(0:1, 76  ) = [-14.2424, 0.1111]
  cd(0:1, 77  ) = [-13.6364, 0.1035]
  cd(0:1, 78  ) = [-13.0303, 0.096]
  cd(0:1, 79  ) = [-12.4242, 0.0884]
  cd(0:1, 80  ) = [-11.8182, 0.0807]
  cd(0:1, 81  ) = [-11.2121, 0.0726]
  cd(0:1, 82  ) = [-10.6061, 0.0643]
  cd(0:1, 83  ) = [-10.0, 0.0559]
  cd(0:1, 84  ) = [-9.3939, 0.0491]
  cd(0:1, 85  ) = [-8.7879, 0.0425]
  cd(0:1, 86  ) = [-8.1818, 0.0351]
  cd(0:1, 87  ) = [-7.5758, 0.0307]
  cd(0:1, 88  ) = [-6.9697, 0.0273]
  cd(0:1, 89  ) = [-6.3636, 0.024]
  cd(0:1, 90  ) = [-5.7576, 0.0211]
  cd(0:1, 91  ) = [-5.1515, 0.019]
  cd(0:1, 92  ) = [-4.5455, 0.0171]
  cd(0:1, 93  ) = [-3.9394, 0.0154]
  cd(0:1, 94  ) = [-3.3333, 0.0148]
  cd(0:1, 95  ) = [-2.7273, 0.0142]
  cd(0:1, 96  ) = [-2.1212, 0.0134]
  cd(0:1, 97  ) = [-1.5152, 0.0131]
  cd(0:1, 98  ) = [-0.9091, 0.0129]
  cd(0:1, 99  ) = [-0.303, 0.0128]
  cd(0:1, 100  ) = [0.303, 0.0127]
  cd(0:1, 101  ) = [0.9091, 0.0127]
  cd(0:1, 102  ) = [1.5152, 0.0129]
  cd(0:1, 103  ) = [2.1212, 0.0131]
  cd(0:1, 104  ) = [2.7273, 0.0132]
  cd(0:1, 105  ) = [3.3333, 0.0135]
  cd(0:1, 106  ) = [3.9394, 0.0138]
  cd(0:1, 107  ) = [4.5455, 0.0143]
  cd(0:1, 108  ) = [5.1515, 0.0147]
  cd(0:1, 109  ) = [5.7576, 0.0152]
  cd(0:1, 110  ) = [6.3636, 0.0157]
  cd(0:1, 111  ) = [6.9697, 0.0163]
  cd(0:1, 112  ) = [7.5758, 0.0172]
  cd(0:1, 113  ) = [8.1818, 0.0181]
  cd(0:1, 114  ) = [8.7879, 0.0192]
  cd(0:1, 115  ) = [9.3939, 0.0214]
  cd(0:1, 116  ) = [10.0, 0.024]
  cd(0:1, 117  ) = [10.6061, 0.0274]
  cd(0:1, 118  ) = [11.2121, 0.0312]
  cd(0:1, 119  ) = [11.8182, 0.0368]
  cd(0:1, 120  ) = [12.4242, 0.045]
  cd(0:1, 121  ) = [13.0303, 0.0538]
  cd(0:1, 122  ) = [13.6364, 0.064]
  cd(0:1, 123  ) = [14.2424, 0.0747]
  cd(0:1, 124  ) = [14.8485, 0.086]
  cd(0:1, 125  ) = [15.4545, 0.0981]
  cd(0:1, 126  ) = [16.0606, 0.1106]
  cd(0:1, 127  ) = [16.6667, 0.1241]
  cd(0:1, 128  ) = [17.2727, 0.1375]
  cd(0:1, 129  ) = [17.8788, 0.151]
  cd(0:1, 130  ) = [18.4848, 0.1643]
  cd(0:1, 131  ) = [19.0909, 0.177]
  cd(0:1, 132  ) = [19.697, 0.1862]
  cd(0:1, 133  ) = [20.303, 0.1959]
  cd(0:1, 134  ) = [20.9091, 0.2061]
  cd(0:1, 135  ) = [21.5152, 0.2163]
  cd(0:1, 136  ) = [22.1212, 0.2266]
  cd(0:1, 137  ) = [22.7273, 0.2374]
  cd(0:1, 138  ) = [23.3333, 0.2482]
  cd(0:1, 139  ) = [23.9394, 0.259]
  cd(0:1, 140  ) = [24.5455, 0.2703]
  cd(0:1, 141  ) = [25.1515, 0.2817]
  cd(0:1, 142  ) = [25.7576, 0.293]
  cd(0:1, 143  ) = [26.3636, 0.3046]
  cd(0:1, 144  ) = [26.9697, 0.3165]
  cd(0:1, 145  ) = [27.5758, 0.3283]
  cd(0:1, 146  ) = [28.1818, 0.3403]
  cd(0:1, 147  ) = [28.7879, 0.3526]
  cd(0:1, 148  ) = [29.3939, 0.3649]
  cd(0:1, 149  ) = [30.0, 0.3771]
  cd(0:1, 150  ) = [33.0, 0.4433]
  cd(0:1, 151  ) = [36.0, 0.5116]
  cd(0:1, 152  ) = [39.0, 0.5819]
  cd(0:1, 153  ) = [42.0, 0.6541]
  cd(0:1, 154  ) = [45.0, 0.7269]
  cd(0:1, 155  ) = [48.0, 0.7997]
  cd(0:1, 156  ) = [51.0, 0.8709]
  cd(0:1, 157  ) = [54.0, 0.939]
  cd(0:1, 158  ) = [57.0, 1.0072]
  cd(0:1, 159  ) = [60.0, 1.0754]
  cd(0:1, 160  ) = [63.0, 1.1306]
  cd(0:1, 161  ) = [66.0, 1.1859]
  cd(0:1, 162  ) = [69.0, 1.2411]
  cd(0:1, 163  ) = [72.0, 1.2832]
  cd(0:1, 164  ) = [75.0, 1.3187]
  cd(0:1, 165  ) = [78.0, 1.3543]
  cd(0:1, 166  ) = [81.0, 1.3818]
  cd(0:1, 167  ) = [84.0, 1.3932]
  cd(0:1, 168  ) = [87.0, 1.4046]
  cd(0:1, 169  ) = [90.0, 1.416]
  cd(0:1, 170  ) = [93.0, 1.4046]
  cd(0:1, 171  ) = [96.0, 1.3932]
  cd(0:1, 172  ) = [99.0, 1.3818]
  cd(0:1, 173  ) = [102.0, 1.3543]
  cd(0:1, 174  ) = [105.0, 1.3187]
  cd(0:1, 175  ) = [108.0, 1.2832]
  cd(0:1, 176  ) = [111.0, 1.2411]
  cd(0:1, 177  ) = [114.0, 1.1859]
  cd(0:1, 178  ) = [117.0, 1.1306]
  cd(0:1, 179  ) = [120.0, 1.0754]
  cd(0:1, 180  ) = [123.0, 1.0072]
  cd(0:1, 181  ) = [126.0, 0.939]
  cd(0:1, 182  ) = [129.0, 0.8709]
  cd(0:1, 183  ) = [132.0, 0.7996]
  cd(0:1, 184  ) = [135.0, 0.7268]
  cd(0:1, 185  ) = [138.0, 0.6541]
  cd(0:1, 186  ) = [141.0, 0.5827]
  cd(0:1, 187  ) = [144.0, 0.5142]
  cd(0:1, 188  ) = [147.0, 0.4457]
  cd(0:1, 189  ) = [150.0, 0.3771]
  cd(0:1, 190  ) = [153.0, 0.3212]
  cd(0:1, 191  ) = [156.0, 0.2653]
  cd(0:1, 192  ) = [159.0, 0.2094]
  cd(0:1, 193  ) = [162.0, 0.17]
  cd(0:1, 194  ) = [165.0, 0.1389]
  cd(0:1, 195  ) = [168.0, 0.1077]
  cd(0:1, 196  ) = [171.0, 0.0785]
  cd(0:1, 197  ) = [174.0, 0.0531]
  cd(0:1, 198  ) = [177.0, 0.0442]
  cd(0:1, 199  ) = [180.0, 0.0437]

  IF ( y .EQ. cd(0,0) ) cd9 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd9 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd9 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd9_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd9 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd9_nrel2p8.pro' 
  ENDIF

!print,y,cd9

END FUNCTION calc_cd9_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd10_nrel2p8( y ) RESULT( cd10 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd10
INTEGER :: i

 cd10 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.0531]
  cd(0:1, 1  ) = [-177.0, 0.0531]
  cd(0:1, 2  ) = [-174.0, 0.0638]
  cd(0:1, 3  ) = [-171.0, 0.0956]
  cd(0:1, 4  ) = [-168.0, 0.1286]
  cd(0:1, 5  ) = [-165.0, 0.1621]
  cd(0:1, 6  ) = [-162.0, 0.1956]
  cd(0:1, 7  ) = [-159.0, 0.2369]
  cd(0:1, 8  ) = [-156.0, 0.2937]
  cd(0:1, 9  ) = [-153.0, 0.3505]
  cd(0:1, 10  ) = [-150.0, 0.4073]
  cd(0:1, 11  ) = [-147.0, 0.4768]
  cd(0:1, 12  ) = [-144.0, 0.5463]
  cd(0:1, 13  ) = [-141.0, 0.6159]
  cd(0:1, 14  ) = [-138.0, 0.6882]
  cd(0:1, 15  ) = [-135.0, 0.7619]
  cd(0:1, 16  ) = [-132.0, 0.8356]
  cd(0:1, 17  ) = [-129.0, 0.9077]
  cd(0:1, 18  ) = [-126.0, 0.9766]
  cd(0:1, 19  ) = [-123.0, 1.0454]
  cd(0:1, 20  ) = [-120.0, 1.1143]
  cd(0:1, 21  ) = [-117.0, 1.1698]
  cd(0:1, 22  ) = [-114.0, 1.2253]
  cd(0:1, 23  ) = [-111.0, 1.2808]
  cd(0:1, 24  ) = [-108.0, 1.3228]
  cd(0:1, 25  ) = [-105.0, 1.358]
  cd(0:1, 26  ) = [-102.0, 1.3932]
  cd(0:1, 27  ) = [-99.0, 1.4202]
  cd(0:1, 28  ) = [-96.0, 1.4306]
  cd(0:1, 29  ) = [-93.0, 1.4411]
  cd(0:1, 30  ) = [-90.0, 1.4516]
  cd(0:1, 31  ) = [-87.0, 1.4411]
  cd(0:1, 32  ) = [-84.0, 1.4306]
  cd(0:1, 33  ) = [-81.0, 1.4202]
  cd(0:1, 34  ) = [-78.0, 1.3932]
  cd(0:1, 35  ) = [-75.0, 1.358]
  cd(0:1, 36  ) = [-72.0, 1.3228]
  cd(0:1, 37  ) = [-69.0, 1.2808]
  cd(0:1, 38  ) = [-66.0, 1.2253]
  cd(0:1, 39  ) = [-63.0, 1.1698]
  cd(0:1, 40  ) = [-60.0, 1.1143]
  cd(0:1, 41  ) = [-57.0, 1.0454]
  cd(0:1, 42  ) = [-54.0, 0.9766]
  cd(0:1, 43  ) = [-51.0, 0.9077]
  cd(0:1, 44  ) = [-48.0, 0.8357]
  cd(0:1, 45  ) = [-45.0, 0.762]
  cd(0:1, 46  ) = [-42.0, 0.6882]
  cd(0:1, 47  ) = [-39.0, 0.6148]
  cd(0:1, 48  ) = [-36.0, 0.543]
  cd(0:1, 49  ) = [-33.0, 0.4738]
  cd(0:1, 50  ) = [-30.0, 0.4073]
  cd(0:1, 51  ) = [-29.3939, 0.3946]
  cd(0:1, 52  ) = [-28.7879, 0.3818]
  cd(0:1, 53  ) = [-28.1818, 0.3691]
  cd(0:1, 54  ) = [-27.5758, 0.3567]
  cd(0:1, 55  ) = [-26.9697, 0.3446]
  cd(0:1, 56  ) = [-26.3636, 0.3324]
  cd(0:1, 57  ) = [-25.7576, 0.3205]
  cd(0:1, 58  ) = [-25.1515, 0.3089]
  cd(0:1, 59  ) = [-24.5455, 0.2974]
  cd(0:1, 60  ) = [-23.9394, 0.286]
  cd(0:1, 61  ) = [-23.3333, 0.2751]
  cd(0:1, 62  ) = [-22.7273, 0.2643]
  cd(0:1, 63  ) = [-22.1212, 0.2534]
  cd(0:1, 64  ) = [-21.5152, 0.2432]
  cd(0:1, 65  ) = [-20.9091, 0.2331]
  cd(0:1, 66  ) = [-20.303, 0.223]
  cd(0:1, 67  ) = [-19.697, 0.2133]
  cd(0:1, 68  ) = [-19.0909, 0.204]
  cd(0:1, 69  ) = [-18.4848, 0.1946]
  cd(0:1, 70  ) = [-17.8788, 0.1853]
  cd(0:1, 71  ) = [-17.2727, 0.1759]
  cd(0:1, 72  ) = [-16.6667, 0.1662]
  cd(0:1, 73  ) = [-16.0606, 0.1564]
  cd(0:1, 74  ) = [-15.4545, 0.1469]
  cd(0:1, 75  ) = [-14.8485, 0.1374]
  cd(0:1, 76  ) = [-14.2424, 0.1282]
  cd(0:1, 77  ) = [-13.6364, 0.1191]
  cd(0:1, 78  ) = [-13.0303, 0.1099]
  cd(0:1, 79  ) = [-12.4242, 0.1007]
  cd(0:1, 80  ) = [-11.8182, 0.0915]
  cd(0:1, 81  ) = [-11.2121, 0.0822]
  cd(0:1, 82  ) = [-10.6061, 0.0728]
  cd(0:1, 83  ) = [-10.0, 0.0633]
  cd(0:1, 84  ) = [-9.3939, 0.0542]
  cd(0:1, 85  ) = [-8.7879, 0.0451]
  cd(0:1, 86  ) = [-8.1818, 0.0358]
  cd(0:1, 87  ) = [-7.5758, 0.0301]
  cd(0:1, 88  ) = [-6.9697, 0.026]
  cd(0:1, 89  ) = [-6.3636, 0.0222]
  cd(0:1, 90  ) = [-5.7576, 0.019]
  cd(0:1, 91  ) = [-5.1515, 0.0167]
  cd(0:1, 92  ) = [-4.5455, 0.0147]
  cd(0:1, 93  ) = [-3.9394, 0.013]
  cd(0:1, 94  ) = [-3.3333, 0.0124]
  cd(0:1, 95  ) = [-2.7273, 0.0117]
  cd(0:1, 96  ) = [-2.1212, 0.011]
  cd(0:1, 97  ) = [-1.5152, 0.0106]
  cd(0:1, 98  ) = [-0.9091, 0.0104]
  cd(0:1, 99  ) = [-0.303, 0.0103]
  cd(0:1, 100  ) = [0.303, 0.0102]
  cd(0:1, 101  ) = [0.9091, 0.0102]
  cd(0:1, 102  ) = [1.5152, 0.0104]
  cd(0:1, 103  ) = [2.1212, 0.0106]
  cd(0:1, 104  ) = [2.7273, 0.0108]
  cd(0:1, 105  ) = [3.3333, 0.0111]
  cd(0:1, 106  ) = [3.9394, 0.0114]
  cd(0:1, 107  ) = [4.5455, 0.0119]
  cd(0:1, 108  ) = [5.1515, 0.0124]
  cd(0:1, 109  ) = [5.7576, 0.0128]
  cd(0:1, 110  ) = [6.3636, 0.0134]
  cd(0:1, 111  ) = [6.9697, 0.0141]
  cd(0:1, 112  ) = [7.5758, 0.015]
  cd(0:1, 113  ) = [8.1818, 0.016]
  cd(0:1, 114  ) = [8.7879, 0.0172]
  cd(0:1, 115  ) = [9.3939, 0.0196]
  cd(0:1, 116  ) = [10.0, 0.0226]
  cd(0:1, 117  ) = [10.6061, 0.0267]
  cd(0:1, 118  ) = [11.2121, 0.0318]
  cd(0:1, 119  ) = [11.8182, 0.0388]
  cd(0:1, 120  ) = [12.4242, 0.0483]
  cd(0:1, 121  ) = [13.0303, 0.0591]
  cd(0:1, 122  ) = [13.6364, 0.0717]
  cd(0:1, 123  ) = [14.2424, 0.085]
  cd(0:1, 124  ) = [14.8485, 0.0993]
  cd(0:1, 125  ) = [15.4545, 0.1146]
  cd(0:1, 126  ) = [16.0606, 0.1302]
  cd(0:1, 127  ) = [16.6667, 0.1452]
  cd(0:1, 128  ) = [17.2727, 0.1602]
  cd(0:1, 129  ) = [17.8788, 0.1751]
  cd(0:1, 130  ) = [18.4848, 0.1898]
  cd(0:1, 131  ) = [19.0909, 0.2038]
  cd(0:1, 132  ) = [19.697, 0.2132]
  cd(0:1, 133  ) = [20.303, 0.223]
  cd(0:1, 134  ) = [20.9091, 0.2331]
  cd(0:1, 135  ) = [21.5152, 0.2432]
  cd(0:1, 136  ) = [22.1212, 0.2534]
  cd(0:1, 137  ) = [22.7273, 0.2643]
  cd(0:1, 138  ) = [23.3333, 0.2751]
  cd(0:1, 139  ) = [23.9394, 0.286]
  cd(0:1, 140  ) = [24.5455, 0.2974]
  cd(0:1, 141  ) = [25.1515, 0.3089]
  cd(0:1, 142  ) = [25.7576, 0.3205]
  cd(0:1, 143  ) = [26.3636, 0.3324]
  cd(0:1, 144  ) = [26.9697, 0.3446]
  cd(0:1, 145  ) = [27.5758, 0.3567]
  cd(0:1, 146  ) = [28.1818, 0.3691]
  cd(0:1, 147  ) = [28.7879, 0.3818]
  cd(0:1, 148  ) = [29.3939, 0.3946]
  cd(0:1, 149  ) = [30.0, 0.4073]
  cd(0:1, 150  ) = [33.0, 0.4738]
  cd(0:1, 151  ) = [36.0, 0.543]
  cd(0:1, 152  ) = [39.0, 0.6148]
  cd(0:1, 153  ) = [42.0, 0.6882]
  cd(0:1, 154  ) = [45.0, 0.762]
  cd(0:1, 155  ) = [48.0, 0.8357]
  cd(0:1, 156  ) = [51.0, 0.9077]
  cd(0:1, 157  ) = [54.0, 0.9766]
  cd(0:1, 158  ) = [57.0, 1.0454]
  cd(0:1, 159  ) = [60.0, 1.1143]
  cd(0:1, 160  ) = [63.0, 1.1698]
  cd(0:1, 161  ) = [66.0, 1.2253]
  cd(0:1, 162  ) = [69.0, 1.2808]
  cd(0:1, 163  ) = [72.0, 1.3228]
  cd(0:1, 164  ) = [75.0, 1.358]
  cd(0:1, 165  ) = [78.0, 1.3932]
  cd(0:1, 166  ) = [81.0, 1.4202]
  cd(0:1, 167  ) = [84.0, 1.4306]
  cd(0:1, 168  ) = [87.0, 1.4411]
  cd(0:1, 169  ) = [90.0, 1.4516]
  cd(0:1, 170  ) = [93.0, 1.4411]
  cd(0:1, 171  ) = [96.0, 1.4306]
  cd(0:1, 172  ) = [99.0, 1.4202]
  cd(0:1, 173  ) = [102.0, 1.3932]
  cd(0:1, 174  ) = [105.0, 1.358]
  cd(0:1, 175  ) = [108.0, 1.3228]
  cd(0:1, 176  ) = [111.0, 1.2808]
  cd(0:1, 177  ) = [114.0, 1.2253]
  cd(0:1, 178  ) = [117.0, 1.1698]
  cd(0:1, 179  ) = [120.0, 1.1143]
  cd(0:1, 180  ) = [123.0, 1.0454]
  cd(0:1, 181  ) = [126.0, 0.9766]
  cd(0:1, 182  ) = [129.0, 0.9077]
  cd(0:1, 183  ) = [132.0, 0.8356]
  cd(0:1, 184  ) = [135.0, 0.7619]
  cd(0:1, 185  ) = [138.0, 0.6882]
  cd(0:1, 186  ) = [141.0, 0.6159]
  cd(0:1, 187  ) = [144.0, 0.5463]
  cd(0:1, 188  ) = [147.0, 0.4768]
  cd(0:1, 189  ) = [150.0, 0.4073]
  cd(0:1, 190  ) = [153.0, 0.3505]
  cd(0:1, 191  ) = [156.0, 0.2937]
  cd(0:1, 192  ) = [159.0, 0.2369]
  cd(0:1, 193  ) = [162.0, 0.1961]
  cd(0:1, 194  ) = [165.0, 0.1633]
  cd(0:1, 195  ) = [168.0, 0.1305]
  cd(0:1, 196  ) = [171.0, 0.0978]
  cd(0:1, 197  ) = [174.0, 0.0652]
  cd(0:1, 198  ) = [177.0, 0.0539]
  cd(0:1, 199  ) = [180.0, 0.0531]

  IF ( y .EQ. cd(0,0) ) cd10 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd10 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd10 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd10_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd10 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd10_nrel2p8.pro' 
  ENDIF

!print,y,cd10

END FUNCTION calc_cd10_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd11_nrel2p8( y ) RESULT( cd11 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd11
INTEGER :: i

 cd11 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.0531]
  cd(0:1, 1  ) = [-177.0, 0.0531]
  cd(0:1, 2  ) = [-174.0, 0.0637]
  cd(0:1, 3  ) = [-171.0, 0.0956]
  cd(0:1, 4  ) = [-168.0, 0.1283]
  cd(0:1, 5  ) = [-165.0, 0.1616]
  cd(0:1, 6  ) = [-162.0, 0.1948]
  cd(0:1, 7  ) = [-159.0, 0.2358]
  cd(0:1, 8  ) = [-156.0, 0.2926]
  cd(0:1, 9  ) = [-153.0, 0.3493]
  cd(0:1, 10  ) = [-150.0, 0.406]
  cd(0:1, 11  ) = [-147.0, 0.4757]
  cd(0:1, 12  ) = [-144.0, 0.5453]
  cd(0:1, 13  ) = [-141.0, 0.615]
  cd(0:1, 14  ) = [-138.0, 0.6874]
  cd(0:1, 15  ) = [-135.0, 0.7613]
  cd(0:1, 16  ) = [-132.0, 0.8352]
  cd(0:1, 17  ) = [-129.0, 0.9074]
  cd(0:1, 18  ) = [-126.0, 0.9764]
  cd(0:1, 19  ) = [-123.0, 1.0454]
  cd(0:1, 20  ) = [-120.0, 1.1145]
  cd(0:1, 21  ) = [-117.0, 1.1701]
  cd(0:1, 22  ) = [-114.0, 1.2258]
  cd(0:1, 23  ) = [-111.0, 1.2814]
  cd(0:1, 24  ) = [-108.0, 1.3235]
  cd(0:1, 25  ) = [-105.0, 1.3589]
  cd(0:1, 26  ) = [-102.0, 1.3942]
  cd(0:1, 27  ) = [-99.0, 1.4213]
  cd(0:1, 28  ) = [-96.0, 1.4319]
  cd(0:1, 29  ) = [-93.0, 1.4425]
  cd(0:1, 30  ) = [-90.0, 1.4531]
  cd(0:1, 31  ) = [-87.0, 1.4425]
  cd(0:1, 32  ) = [-84.0, 1.4319]
  cd(0:1, 33  ) = [-81.0, 1.4213]
  cd(0:1, 34  ) = [-78.0, 1.3942]
  cd(0:1, 35  ) = [-75.0, 1.3589]
  cd(0:1, 36  ) = [-72.0, 1.3235]
  cd(0:1, 37  ) = [-69.0, 1.2814]
  cd(0:1, 38  ) = [-66.0, 1.2258]
  cd(0:1, 39  ) = [-63.0, 1.1701]
  cd(0:1, 40  ) = [-60.0, 1.1145]
  cd(0:1, 41  ) = [-57.0, 1.0454]
  cd(0:1, 42  ) = [-54.0, 0.9764]
  cd(0:1, 43  ) = [-51.0, 0.9074]
  cd(0:1, 44  ) = [-48.0, 0.8352]
  cd(0:1, 45  ) = [-45.0, 0.7614]
  cd(0:1, 46  ) = [-42.0, 0.6875]
  cd(0:1, 47  ) = [-39.0, 0.6139]
  cd(0:1, 48  ) = [-36.0, 0.5419]
  cd(0:1, 49  ) = [-33.0, 0.4726]
  cd(0:1, 50  ) = [-30.0, 0.406]
  cd(0:1, 51  ) = [-29.3939, 0.3932]
  cd(0:1, 52  ) = [-28.7879, 0.3804]
  cd(0:1, 53  ) = [-28.1818, 0.3677]
  cd(0:1, 54  ) = [-27.5758, 0.3553]
  cd(0:1, 55  ) = [-26.9697, 0.3431]
  cd(0:1, 56  ) = [-26.3636, 0.3309]
  cd(0:1, 57  ) = [-25.7576, 0.319]
  cd(0:1, 58  ) = [-25.1515, 0.3074]
  cd(0:1, 59  ) = [-24.5455, 0.2959]
  cd(0:1, 60  ) = [-23.9394, 0.2844]
  cd(0:1, 61  ) = [-23.3333, 0.2735]
  cd(0:1, 62  ) = [-22.7273, 0.2627]
  cd(0:1, 63  ) = [-22.1212, 0.2518]
  cd(0:1, 64  ) = [-21.5152, 0.2419]
  cd(0:1, 65  ) = [-20.9091, 0.2322]
  cd(0:1, 66  ) = [-20.303, 0.2225]
  cd(0:1, 67  ) = [-19.697, 0.2131]
  cd(0:1, 68  ) = [-19.0909, 0.204]
  cd(0:1, 69  ) = [-18.4848, 0.195]
  cd(0:1, 70  ) = [-17.8788, 0.186]
  cd(0:1, 71  ) = [-17.2727, 0.1768]
  cd(0:1, 72  ) = [-16.6667, 0.1686]
  cd(0:1, 73  ) = [-16.0606, 0.1605]
  cd(0:1, 74  ) = [-15.4545, 0.1517]
  cd(0:1, 75  ) = [-14.8485, 0.1426]
  cd(0:1, 76  ) = [-14.2424, 0.1328]
  cd(0:1, 77  ) = [-13.6364, 0.1228]
  cd(0:1, 78  ) = [-13.0303, 0.1128]
  cd(0:1, 79  ) = [-12.4242, 0.1029]
  cd(0:1, 80  ) = [-11.8182, 0.0931]
  cd(0:1, 81  ) = [-11.2121, 0.0837]
  cd(0:1, 82  ) = [-10.6061, 0.0746]
  cd(0:1, 83  ) = [-10.0, 0.0655]
  cd(0:1, 84  ) = [-9.3939, 0.0555]
  cd(0:1, 85  ) = [-8.7879, 0.0452]
  cd(0:1, 86  ) = [-8.1818, 0.0356]
  cd(0:1, 87  ) = [-7.5758, 0.0292]
  cd(0:1, 88  ) = [-6.9697, 0.0244]
  cd(0:1, 89  ) = [-6.3636, 0.0207]
  cd(0:1, 90  ) = [-5.7576, 0.0177]
  cd(0:1, 91  ) = [-5.1515, 0.0156]
  cd(0:1, 92  ) = [-4.5455, 0.0139]
  cd(0:1, 93  ) = [-3.9394, 0.0124]
  cd(0:1, 94  ) = [-3.3333, 0.0119]
  cd(0:1, 95  ) = [-2.7273, 0.0114]
  cd(0:1, 96  ) = [-2.1212, 0.0108]
  cd(0:1, 97  ) = [-1.5152, 0.0105]
  cd(0:1, 98  ) = [-0.9091, 0.0103]
  cd(0:1, 99  ) = [-0.303, 0.0102]
  cd(0:1, 100  ) = [0.303, 0.0102]
  cd(0:1, 101  ) = [0.9091, 0.0102]
  cd(0:1, 102  ) = [1.5152, 0.0103]
  cd(0:1, 103  ) = [2.1212, 0.0105]
  cd(0:1, 104  ) = [2.7273, 0.0107]
  cd(0:1, 105  ) = [3.3333, 0.011]
  cd(0:1, 106  ) = [3.9394, 0.0113]
  cd(0:1, 107  ) = [4.5455, 0.0116]
  cd(0:1, 108  ) = [5.1515, 0.012]
  cd(0:1, 109  ) = [5.7576, 0.0124]
  cd(0:1, 110  ) = [6.3636, 0.0129]
  cd(0:1, 111  ) = [6.9697, 0.0135]
  cd(0:1, 112  ) = [7.5758, 0.0142]
  cd(0:1, 113  ) = [8.1818, 0.0151]
  cd(0:1, 114  ) = [8.7879, 0.0161]
  cd(0:1, 115  ) = [9.3939, 0.0181]
  cd(0:1, 116  ) = [10.0, 0.0209]
  cd(0:1, 117  ) = [10.6061, 0.0254]
  cd(0:1, 118  ) = [11.2121, 0.0308]
  cd(0:1, 119  ) = [11.8182, 0.0375]
  cd(0:1, 120  ) = [12.4242, 0.0477]
  cd(0:1, 121  ) = [13.0303, 0.0594]
  cd(0:1, 122  ) = [13.6364, 0.0718]
  cd(0:1, 123  ) = [14.2424, 0.0849]
  cd(0:1, 124  ) = [14.8485, 0.0989]
  cd(0:1, 125  ) = [15.4545, 0.1139]
  cd(0:1, 126  ) = [16.0606, 0.1292]
  cd(0:1, 127  ) = [16.6667, 0.1438]
  cd(0:1, 128  ) = [17.2727, 0.1584]
  cd(0:1, 129  ) = [17.8788, 0.173]
  cd(0:1, 130  ) = [18.4848, 0.1876]
  cd(0:1, 131  ) = [19.0909, 0.2015]
  cd(0:1, 132  ) = [19.697, 0.2112]
  cd(0:1, 133  ) = [20.303, 0.2211]
  cd(0:1, 134  ) = [20.9091, 0.2313]
  cd(0:1, 135  ) = [21.5152, 0.2415]
  cd(0:1, 136  ) = [22.1212, 0.2518]
  cd(0:1, 137  ) = [22.7273, 0.2627]
  cd(0:1, 138  ) = [23.3333, 0.2735]
  cd(0:1, 139  ) = [23.9394, 0.2844]
  cd(0:1, 140  ) = [24.5455, 0.2959]
  cd(0:1, 141  ) = [25.1515, 0.3074]
  cd(0:1, 142  ) = [25.7576, 0.319]
  cd(0:1, 143  ) = [26.3636, 0.3309]
  cd(0:1, 144  ) = [26.9697, 0.3431]
  cd(0:1, 145  ) = [27.5758, 0.3553]
  cd(0:1, 146  ) = [28.1818, 0.3677]
  cd(0:1, 147  ) = [28.7879, 0.3804]
  cd(0:1, 148  ) = [29.3939, 0.3932]
  cd(0:1, 149  ) = [30.0, 0.406]
  cd(0:1, 150  ) = [33.0, 0.4726]
  cd(0:1, 151  ) = [36.0, 0.5419]
  cd(0:1, 152  ) = [39.0, 0.6139]
  cd(0:1, 153  ) = [42.0, 0.6875]
  cd(0:1, 154  ) = [45.0, 0.7614]
  cd(0:1, 155  ) = [48.0, 0.8352]
  cd(0:1, 156  ) = [51.0, 0.9074]
  cd(0:1, 157  ) = [54.0, 0.9764]
  cd(0:1, 158  ) = [57.0, 1.0454]
  cd(0:1, 159  ) = [60.0, 1.1145]
  cd(0:1, 160  ) = [63.0, 1.1701]
  cd(0:1, 161  ) = [66.0, 1.2258]
  cd(0:1, 162  ) = [69.0, 1.2814]
  cd(0:1, 163  ) = [72.0, 1.3235]
  cd(0:1, 164  ) = [75.0, 1.3589]
  cd(0:1, 165  ) = [78.0, 1.3942]
  cd(0:1, 166  ) = [81.0, 1.4213]
  cd(0:1, 167  ) = [84.0, 1.4319]
  cd(0:1, 168  ) = [87.0, 1.4425]
  cd(0:1, 169  ) = [90.0, 1.4531]
  cd(0:1, 170  ) = [93.0, 1.4425]
  cd(0:1, 171  ) = [96.0, 1.4319]
  cd(0:1, 172  ) = [99.0, 1.4213]
  cd(0:1, 173  ) = [102.0, 1.3942]
  cd(0:1, 174  ) = [105.0, 1.3589]
  cd(0:1, 175  ) = [108.0, 1.3235]
  cd(0:1, 176  ) = [111.0, 1.2814]
  cd(0:1, 177  ) = [114.0, 1.2258]
  cd(0:1, 178  ) = [117.0, 1.1701]
  cd(0:1, 179  ) = [120.0, 1.1145]
  cd(0:1, 180  ) = [123.0, 1.0454]
  cd(0:1, 181  ) = [126.0, 0.9764]
  cd(0:1, 182  ) = [129.0, 0.9074]
  cd(0:1, 183  ) = [132.0, 0.8352]
  cd(0:1, 184  ) = [135.0, 0.7613]
  cd(0:1, 185  ) = [138.0, 0.6874]
  cd(0:1, 186  ) = [141.0, 0.615]
  cd(0:1, 187  ) = [144.0, 0.5453]
  cd(0:1, 188  ) = [147.0, 0.4757]
  cd(0:1, 189  ) = [150.0, 0.406]
  cd(0:1, 190  ) = [153.0, 0.3494]
  cd(0:1, 191  ) = [156.0, 0.2928]
  cd(0:1, 192  ) = [159.0, 0.2362]
  cd(0:1, 193  ) = [162.0, 0.1956]
  cd(0:1, 194  ) = [165.0, 0.163]
  cd(0:1, 195  ) = [168.0, 0.1304]
  cd(0:1, 196  ) = [171.0, 0.0978]
  cd(0:1, 197  ) = [174.0, 0.0652]
  cd(0:1, 198  ) = [177.0, 0.0538]
  cd(0:1, 199  ) = [180.0, 0.0531]

  IF ( y .EQ. cd(0,0) ) cd11 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd11 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd11 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd11_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd11 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd11_nrel2p8.pro' 
  ENDIF

!print,y,cd11

END FUNCTION calc_cd11_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd12_nrel2p8( y ) RESULT( cd12 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd12
INTEGER :: i

 cd12 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.0517]
  cd(0:1, 1  ) = [-177.0, 0.0517]
  cd(0:1, 2  ) = [-174.0, 0.0621]
  cd(0:1, 3  ) = [-171.0, 0.0931]
  cd(0:1, 4  ) = [-168.0, 0.1249]
  cd(0:1, 5  ) = [-165.0, 0.157]
  cd(0:1, 6  ) = [-162.0, 0.1891]
  cd(0:1, 7  ) = [-159.0, 0.2294]
  cd(0:1, 8  ) = [-156.0, 0.2857]
  cd(0:1, 9  ) = [-153.0, 0.3421]
  cd(0:1, 10  ) = [-150.0, 0.3985]
  cd(0:1, 11  ) = [-147.0, 0.4684]
  cd(0:1, 12  ) = [-144.0, 0.5384]
  cd(0:1, 13  ) = [-141.0, 0.6083]
  cd(0:1, 14  ) = [-138.0, 0.681]
  cd(0:1, 15  ) = [-135.0, 0.7552]
  cd(0:1, 16  ) = [-132.0, 0.8294]
  cd(0:1, 17  ) = [-129.0, 0.902]
  cd(0:1, 18  ) = [-126.0, 0.9714]
  cd(0:1, 19  ) = [-123.0, 1.0408]
  cd(0:1, 20  ) = [-120.0, 1.1102]
  cd(0:1, 21  ) = [-117.0, 1.1662]
  cd(0:1, 22  ) = [-114.0, 1.2223]
  cd(0:1, 23  ) = [-111.0, 1.2783]
  cd(0:1, 24  ) = [-108.0, 1.3209]
  cd(0:1, 25  ) = [-105.0, 1.3567]
  cd(0:1, 26  ) = [-102.0, 1.3925]
  cd(0:1, 27  ) = [-99.0, 1.42]
  cd(0:1, 28  ) = [-96.0, 1.431]
  cd(0:1, 29  ) = [-93.0, 1.442]
  cd(0:1, 30  ) = [-90.0, 1.4531]
  cd(0:1, 31  ) = [-87.0, 1.442]
  cd(0:1, 32  ) = [-84.0, 1.431]
  cd(0:1, 33  ) = [-81.0, 1.42]
  cd(0:1, 34  ) = [-78.0, 1.3925]
  cd(0:1, 35  ) = [-75.0, 1.3567]
  cd(0:1, 36  ) = [-72.0, 1.3209]
  cd(0:1, 37  ) = [-69.0, 1.2783]
  cd(0:1, 38  ) = [-66.0, 1.2223]
  cd(0:1, 39  ) = [-63.0, 1.1662]
  cd(0:1, 40  ) = [-60.0, 1.1102]
  cd(0:1, 41  ) = [-57.0, 1.0408]
  cd(0:1, 42  ) = [-54.0, 0.9714]
  cd(0:1, 43  ) = [-51.0, 0.902]
  cd(0:1, 44  ) = [-48.0, 0.8294]
  cd(0:1, 45  ) = [-45.0, 0.7553]
  cd(0:1, 46  ) = [-42.0, 0.6811]
  cd(0:1, 47  ) = [-39.0, 0.6072]
  cd(0:1, 48  ) = [-36.0, 0.535]
  cd(0:1, 49  ) = [-33.0, 0.4654]
  cd(0:1, 50  ) = [-30.0, 0.3985]
  cd(0:1, 51  ) = [-29.3939, 0.3857]
  cd(0:1, 52  ) = [-28.7879, 0.3729]
  cd(0:1, 53  ) = [-28.1818, 0.3601]
  cd(0:1, 54  ) = [-27.5758, 0.3477]
  cd(0:1, 55  ) = [-26.9697, 0.3354]
  cd(0:1, 56  ) = [-26.3636, 0.3232]
  cd(0:1, 57  ) = [-25.7576, 0.3112]
  cd(0:1, 58  ) = [-25.1515, 0.2996]
  cd(0:1, 59  ) = [-24.5455, 0.288]
  cd(0:1, 60  ) = [-23.9394, 0.2765]
  cd(0:1, 61  ) = [-23.3333, 0.2656]
  cd(0:1, 62  ) = [-22.7273, 0.2547]
  cd(0:1, 63  ) = [-22.1212, 0.2438]
  cd(0:1, 64  ) = [-21.5152, 0.2348]
  cd(0:1, 65  ) = [-20.9091, 0.2261]
  cd(0:1, 66  ) = [-20.303, 0.2175]
  cd(0:1, 67  ) = [-19.697, 0.2091]
  cd(0:1, 68  ) = [-19.0909, 0.201]
  cd(0:1, 69  ) = [-18.4848, 0.1932]
  cd(0:1, 70  ) = [-17.8788, 0.1853]
  cd(0:1, 71  ) = [-17.2727, 0.1768]
  cd(0:1, 72  ) = [-16.6667, 0.1697]
  cd(0:1, 73  ) = [-16.0606, 0.1628]
  cd(0:1, 74  ) = [-15.4545, 0.1547]
  cd(0:1, 75  ) = [-14.8485, 0.146]
  cd(0:1, 76  ) = [-14.2424, 0.1357]
  cd(0:1, 77  ) = [-13.6364, 0.1252]
  cd(0:1, 78  ) = [-13.0303, 0.1145]
  cd(0:1, 79  ) = [-12.4242, 0.1041]
  cd(0:1, 80  ) = [-11.8182, 0.0939]
  cd(0:1, 81  ) = [-11.2121, 0.0846]
  cd(0:1, 82  ) = [-10.6061, 0.0757]
  cd(0:1, 83  ) = [-10.0, 0.067]
  cd(0:1, 84  ) = [-9.3939, 0.0563]
  cd(0:1, 85  ) = [-8.7879, 0.0452]
  cd(0:1, 86  ) = [-8.1818, 0.0347]
  cd(0:1, 87  ) = [-7.5758, 0.0278]
  cd(0:1, 88  ) = [-6.9697, 0.0228]
  cd(0:1, 89  ) = [-6.3636, 0.0195]
  cd(0:1, 90  ) = [-5.7576, 0.0168]
  cd(0:1, 91  ) = [-5.1515, 0.015]
  cd(0:1, 92  ) = [-4.5455, 0.0135]
  cd(0:1, 93  ) = [-3.9394, 0.0122]
  cd(0:1, 94  ) = [-3.3333, 0.0117]
  cd(0:1, 95  ) = [-2.7273, 0.0112]
  cd(0:1, 96  ) = [-2.1212, 0.0107]
  cd(0:1, 97  ) = [-1.5152, 0.0105]
  cd(0:1, 98  ) = [-0.9091, 0.0103]
  cd(0:1, 99  ) = [-0.303, 0.0103]
  cd(0:1, 100  ) = [0.303, 0.0102]
  cd(0:1, 101  ) = [0.9091, 0.0103]
  cd(0:1, 102  ) = [1.5152, 0.0104]
  cd(0:1, 103  ) = [2.1212, 0.0105]
  cd(0:1, 104  ) = [2.7273, 0.0107]
  cd(0:1, 105  ) = [3.3333, 0.011]
  cd(0:1, 106  ) = [3.9394, 0.0112]
  cd(0:1, 107  ) = [4.5455, 0.0116]
  cd(0:1, 108  ) = [5.1515, 0.0119]
  cd(0:1, 109  ) = [5.7576, 0.0123]
  cd(0:1, 110  ) = [6.3636, 0.0127]
  cd(0:1, 111  ) = [6.9697, 0.0132]
  cd(0:1, 112  ) = [7.5758, 0.0138]
  cd(0:1, 113  ) = [8.1818, 0.0145]
  cd(0:1, 114  ) = [8.7879, 0.0153]
  cd(0:1, 115  ) = [9.3939, 0.017]
  cd(0:1, 116  ) = [10.0, 0.0193]
  cd(0:1, 117  ) = [10.6061, 0.0232]
  cd(0:1, 118  ) = [11.2121, 0.028]
  cd(0:1, 119  ) = [11.8182, 0.0337]
  cd(0:1, 120  ) = [12.4242, 0.0453]
  cd(0:1, 121  ) = [13.0303, 0.0595]
  cd(0:1, 122  ) = [13.6364, 0.0712]
  cd(0:1, 123  ) = [14.2424, 0.0835]
  cd(0:1, 124  ) = [14.8485, 0.0963]
  cd(0:1, 125  ) = [15.4545, 0.1099]
  cd(0:1, 126  ) = [16.0606, 0.1237]
  cd(0:1, 127  ) = [16.6667, 0.1371]
  cd(0:1, 128  ) = [17.2727, 0.1505]
  cd(0:1, 129  ) = [17.8788, 0.1641]
  cd(0:1, 130  ) = [18.4848, 0.178]
  cd(0:1, 131  ) = [19.0909, 0.1915]
  cd(0:1, 132  ) = [19.697, 0.2018]
  cd(0:1, 133  ) = [20.303, 0.2122]
  cd(0:1, 134  ) = [20.9091, 0.2227]
  cd(0:1, 135  ) = [21.5152, 0.2332]
  cd(0:1, 136  ) = [22.1212, 0.2438]
  cd(0:1, 137  ) = [22.7273, 0.2547]
  cd(0:1, 138  ) = [23.3333, 0.2656]
  cd(0:1, 139  ) = [23.9394, 0.2765]
  cd(0:1, 140  ) = [24.5455, 0.288]
  cd(0:1, 141  ) = [25.1515, 0.2996]
  cd(0:1, 142  ) = [25.7576, 0.3112]
  cd(0:1, 143  ) = [26.3636, 0.3232]
  cd(0:1, 144  ) = [26.9697, 0.3354]
  cd(0:1, 145  ) = [27.5758, 0.3477]
  cd(0:1, 146  ) = [28.1818, 0.3601]
  cd(0:1, 147  ) = [28.7879, 0.3729]
  cd(0:1, 148  ) = [29.3939, 0.3857]
  cd(0:1, 149  ) = [30.0, 0.3985]
  cd(0:1, 150  ) = [33.0, 0.4654]
  cd(0:1, 151  ) = [36.0, 0.535]
  cd(0:1, 152  ) = [39.0, 0.6072]
  cd(0:1, 153  ) = [42.0, 0.6811]
  cd(0:1, 154  ) = [45.0, 0.7553]
  cd(0:1, 155  ) = [48.0, 0.8294]
  cd(0:1, 156  ) = [51.0, 0.902]
  cd(0:1, 157  ) = [54.0, 0.9714]
  cd(0:1, 158  ) = [57.0, 1.0408]
  cd(0:1, 159  ) = [60.0, 1.1102]
  cd(0:1, 160  ) = [63.0, 1.1662]
  cd(0:1, 161  ) = [66.0, 1.2223]
  cd(0:1, 162  ) = [69.0, 1.2783]
  cd(0:1, 163  ) = [72.0, 1.3209]
  cd(0:1, 164  ) = [75.0, 1.3567]
  cd(0:1, 165  ) = [78.0, 1.3925]
  cd(0:1, 166  ) = [81.0, 1.42]
  cd(0:1, 167  ) = [84.0, 1.431]
  cd(0:1, 168  ) = [87.0, 1.442]
  cd(0:1, 169  ) = [90.0, 1.4531]
  cd(0:1, 170  ) = [93.0, 1.442]
  cd(0:1, 171  ) = [96.0, 1.431]
  cd(0:1, 172  ) = [99.0, 1.42]
  cd(0:1, 173  ) = [102.0, 1.3925]
  cd(0:1, 174  ) = [105.0, 1.3567]
  cd(0:1, 175  ) = [108.0, 1.3209]
  cd(0:1, 176  ) = [111.0, 1.2783]
  cd(0:1, 177  ) = [114.0, 1.2223]
  cd(0:1, 178  ) = [117.0, 1.1662]
  cd(0:1, 179  ) = [120.0, 1.1102]
  cd(0:1, 180  ) = [123.0, 1.0408]
  cd(0:1, 181  ) = [126.0, 0.9714]
  cd(0:1, 182  ) = [129.0, 0.902]
  cd(0:1, 183  ) = [132.0, 0.8294]
  cd(0:1, 184  ) = [135.0, 0.7552]
  cd(0:1, 185  ) = [138.0, 0.681]
  cd(0:1, 186  ) = [141.0, 0.6083]
  cd(0:1, 187  ) = [144.0, 0.5384]
  cd(0:1, 188  ) = [147.0, 0.4684]
  cd(0:1, 189  ) = [150.0, 0.3985]
  cd(0:1, 190  ) = [153.0, 0.3426]
  cd(0:1, 191  ) = [156.0, 0.2867]
  cd(0:1, 192  ) = [159.0, 0.2308]
  cd(0:1, 193  ) = [162.0, 0.1909]
  cd(0:1, 194  ) = [165.0, 0.1591]
  cd(0:1, 195  ) = [168.0, 0.1273]
  cd(0:1, 196  ) = [171.0, 0.0955]
  cd(0:1, 197  ) = [174.0, 0.0636]
  cd(0:1, 198  ) = [177.0, 0.0525]
  cd(0:1, 199  ) = [180.0, 0.0517]

  IF ( y .EQ. cd(0,0) ) cd12 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd12 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd12 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd12_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd12 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd12_nrel2p8.pro' 
  ENDIF

!print,y,cd12

END FUNCTION calc_cd12_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd13_nrel2p8( y ) RESULT( cd13 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd13
INTEGER :: i

 cd13 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.0502]
  cd(0:1, 1  ) = [-177.0, 0.0502]
  cd(0:1, 2  ) = [-174.0, 0.0602]
  cd(0:1, 3  ) = [-171.0, 0.0904]
  cd(0:1, 4  ) = [-168.0, 0.121]
  cd(0:1, 5  ) = [-165.0, 0.1519]
  cd(0:1, 6  ) = [-162.0, 0.1827]
  cd(0:1, 7  ) = [-159.0, 0.222]
  cd(0:1, 8  ) = [-156.0, 0.278]
  cd(0:1, 9  ) = [-153.0, 0.334]
  cd(0:1, 10  ) = [-150.0, 0.3901]
  cd(0:1, 11  ) = [-147.0, 0.4603]
  cd(0:1, 12  ) = [-144.0, 0.5305]
  cd(0:1, 13  ) = [-141.0, 0.6007]
  cd(0:1, 14  ) = [-138.0, 0.6738]
  cd(0:1, 15  ) = [-135.0, 0.7483]
  cd(0:1, 16  ) = [-132.0, 0.8229]
  cd(0:1, 17  ) = [-129.0, 0.8958]
  cd(0:1, 18  ) = [-126.0, 0.9657]
  cd(0:1, 19  ) = [-123.0, 1.0355]
  cd(0:1, 20  ) = [-120.0, 1.1053]
  cd(0:1, 21  ) = [-117.0, 1.1618]
  cd(0:1, 22  ) = [-114.0, 1.2183]
  cd(0:1, 23  ) = [-111.0, 1.2748]
  cd(0:1, 24  ) = [-108.0, 1.3179]
  cd(0:1, 25  ) = [-105.0, 1.3541]
  cd(0:1, 26  ) = [-102.0, 1.3904]
  cd(0:1, 27  ) = [-99.0, 1.4185]
  cd(0:1, 28  ) = [-96.0, 1.43]
  cd(0:1, 29  ) = [-93.0, 1.4415]
  cd(0:1, 30  ) = [-90.0, 1.4531]
  cd(0:1, 31  ) = [-87.0, 1.4415]
  cd(0:1, 32  ) = [-84.0, 1.43]
  cd(0:1, 33  ) = [-81.0, 1.4185]
  cd(0:1, 34  ) = [-78.0, 1.3904]
  cd(0:1, 35  ) = [-75.0, 1.3541]
  cd(0:1, 36  ) = [-72.0, 1.3179]
  cd(0:1, 37  ) = [-69.0, 1.2748]
  cd(0:1, 38  ) = [-66.0, 1.2183]
  cd(0:1, 39  ) = [-63.0, 1.1618]
  cd(0:1, 40  ) = [-60.0, 1.1053]
  cd(0:1, 41  ) = [-57.0, 1.0355]
  cd(0:1, 42  ) = [-54.0, 0.9657]
  cd(0:1, 43  ) = [-51.0, 0.8958]
  cd(0:1, 44  ) = [-48.0, 0.8229]
  cd(0:1, 45  ) = [-45.0, 0.7484]
  cd(0:1, 46  ) = [-42.0, 0.6738]
  cd(0:1, 47  ) = [-39.0, 0.5996]
  cd(0:1, 48  ) = [-36.0, 0.527]
  cd(0:1, 49  ) = [-33.0, 0.4571]
  cd(0:1, 50  ) = [-30.0, 0.3901]
  cd(0:1, 51  ) = [-29.3939, 0.3772]
  cd(0:1, 52  ) = [-28.7879, 0.3643]
  cd(0:1, 53  ) = [-28.1818, 0.3514]
  cd(0:1, 54  ) = [-27.5758, 0.339]
  cd(0:1, 55  ) = [-26.9697, 0.3267]
  cd(0:1, 56  ) = [-26.3636, 0.3144]
  cd(0:1, 57  ) = [-25.7576, 0.3024]
  cd(0:1, 58  ) = [-25.1515, 0.2908]
  cd(0:1, 59  ) = [-24.5455, 0.2791]
  cd(0:1, 60  ) = [-23.9394, 0.2676]
  cd(0:1, 61  ) = [-23.3333, 0.2566]
  cd(0:1, 62  ) = [-22.7273, 0.2457]
  cd(0:1, 63  ) = [-22.1212, 0.2348]
  cd(0:1, 64  ) = [-21.5152, 0.2267]
  cd(0:1, 65  ) = [-20.9091, 0.2193]
  cd(0:1, 66  ) = [-20.303, 0.2119]
  cd(0:1, 67  ) = [-19.697, 0.2047]
  cd(0:1, 68  ) = [-19.0909, 0.1976]
  cd(0:1, 69  ) = [-18.4848, 0.191]
  cd(0:1, 70  ) = [-17.8788, 0.1843]
  cd(0:1, 71  ) = [-17.2727, 0.1767]
  cd(0:1, 72  ) = [-16.6667, 0.1704]
  cd(0:1, 73  ) = [-16.0606, 0.1644]
  cd(0:1, 74  ) = [-15.4545, 0.1569]
  cd(0:1, 75  ) = [-14.8485, 0.1484]
  cd(0:1, 76  ) = [-14.2424, 0.1378]
  cd(0:1, 77  ) = [-13.6364, 0.1269]
  cd(0:1, 78  ) = [-13.0303, 0.1158]
  cd(0:1, 79  ) = [-12.4242, 0.1049]
  cd(0:1, 80  ) = [-11.8182, 0.0945]
  cd(0:1, 81  ) = [-11.2121, 0.0852]
  cd(0:1, 82  ) = [-10.6061, 0.0764]
  cd(0:1, 83  ) = [-10.0, 0.068]
  cd(0:1, 84  ) = [-9.3939, 0.0568]
  cd(0:1, 85  ) = [-8.7879, 0.0451]
  cd(0:1, 86  ) = [-8.1818, 0.0336]
  cd(0:1, 87  ) = [-7.5758, 0.0263]
  cd(0:1, 88  ) = [-6.9697, 0.0214]
  cd(0:1, 89  ) = [-6.3636, 0.0185]
  cd(0:1, 90  ) = [-5.7576, 0.0162]
  cd(0:1, 91  ) = [-5.1515, 0.0145]
  cd(0:1, 92  ) = [-4.5455, 0.0132]
  cd(0:1, 93  ) = [-3.9394, 0.012]
  cd(0:1, 94  ) = [-3.3333, 0.0116]
  cd(0:1, 95  ) = [-2.7273, 0.0111]
  cd(0:1, 96  ) = [-2.1212, 0.0107]
  cd(0:1, 97  ) = [-1.5152, 0.0105]
  cd(0:1, 98  ) = [-0.9091, 0.0103]
  cd(0:1, 99  ) = [-0.303, 0.0103]
  cd(0:1, 100  ) = [0.303, 0.0103]
  cd(0:1, 101  ) = [0.9091, 0.0104]
  cd(0:1, 102  ) = [1.5152, 0.0105]
  cd(0:1, 103  ) = [2.1212, 0.0106]
  cd(0:1, 104  ) = [2.7273, 0.0108]
  cd(0:1, 105  ) = [3.3333, 0.011]
  cd(0:1, 106  ) = [3.9394, 0.0112]
  cd(0:1, 107  ) = [4.5455, 0.0115]
  cd(0:1, 108  ) = [5.1515, 0.0118]
  cd(0:1, 109  ) = [5.7576, 0.0122]
  cd(0:1, 110  ) = [6.3636, 0.0126]
  cd(0:1, 111  ) = [6.9697, 0.013]
  cd(0:1, 112  ) = [7.5758, 0.0135]
  cd(0:1, 113  ) = [8.1818, 0.0141]
  cd(0:1, 114  ) = [8.7879, 0.0147]
  cd(0:1, 115  ) = [9.3939, 0.016]
  cd(0:1, 116  ) = [10.0, 0.0178]
  cd(0:1, 117  ) = [10.6061, 0.021]
  cd(0:1, 118  ) = [11.2121, 0.0248]
  cd(0:1, 119  ) = [11.8182, 0.0293]
  cd(0:1, 120  ) = [12.4242, 0.0427]
  cd(0:1, 121  ) = [13.0303, 0.0595]
  cd(0:1, 122  ) = [13.6364, 0.0706]
  cd(0:1, 123  ) = [14.2424, 0.0818]
  cd(0:1, 124  ) = [14.8485, 0.0934]
  cd(0:1, 125  ) = [15.4545, 0.1054]
  cd(0:1, 126  ) = [16.0606, 0.1175]
  cd(0:1, 127  ) = [16.6667, 0.1295]
  cd(0:1, 128  ) = [17.2727, 0.1416]
  cd(0:1, 129  ) = [17.8788, 0.1539]
  cd(0:1, 130  ) = [18.4848, 0.1671]
  cd(0:1, 131  ) = [19.0909, 0.1802]
  cd(0:1, 132  ) = [19.697, 0.1912]
  cd(0:1, 133  ) = [20.303, 0.2021]
  cd(0:1, 134  ) = [20.9091, 0.213]
  cd(0:1, 135  ) = [21.5152, 0.2239]
  cd(0:1, 136  ) = [22.1212, 0.2348]
  cd(0:1, 137  ) = [22.7273, 0.2457]
  cd(0:1, 138  ) = [23.3333, 0.2566]
  cd(0:1, 139  ) = [23.9394, 0.2676]
  cd(0:1, 140  ) = [24.5455, 0.2791]
  cd(0:1, 141  ) = [25.1515, 0.2908]
  cd(0:1, 142  ) = [25.7576, 0.3024]
  cd(0:1, 143  ) = [26.3636, 0.3144]
  cd(0:1, 144  ) = [26.9697, 0.3267]
  cd(0:1, 145  ) = [27.5758, 0.339]
  cd(0:1, 146  ) = [28.1818, 0.3514]
  cd(0:1, 147  ) = [28.7879, 0.3643]
  cd(0:1, 148  ) = [29.3939, 0.3772]
  cd(0:1, 149  ) = [30.0, 0.3901]
  cd(0:1, 150  ) = [33.0, 0.4571]
  cd(0:1, 151  ) = [36.0, 0.527]
  cd(0:1, 152  ) = [39.0, 0.5996]
  cd(0:1, 153  ) = [42.0, 0.6738]
  cd(0:1, 154  ) = [45.0, 0.7484]
  cd(0:1, 155  ) = [48.0, 0.8229]
  cd(0:1, 156  ) = [51.0, 0.8958]
  cd(0:1, 157  ) = [54.0, 0.9657]
  cd(0:1, 158  ) = [57.0, 1.0355]
  cd(0:1, 159  ) = [60.0, 1.1053]
  cd(0:1, 160  ) = [63.0, 1.1618]
  cd(0:1, 161  ) = [66.0, 1.2183]
  cd(0:1, 162  ) = [69.0, 1.2748]
  cd(0:1, 163  ) = [72.0, 1.3179]
  cd(0:1, 164  ) = [75.0, 1.3541]
  cd(0:1, 165  ) = [78.0, 1.3904]
  cd(0:1, 166  ) = [81.0, 1.4185]
  cd(0:1, 167  ) = [84.0, 1.43]
  cd(0:1, 168  ) = [87.0, 1.4415]
  cd(0:1, 169  ) = [90.0, 1.4531]
  cd(0:1, 170  ) = [93.0, 1.4415]
  cd(0:1, 171  ) = [96.0, 1.43]
  cd(0:1, 172  ) = [99.0, 1.4185]
  cd(0:1, 173  ) = [102.0, 1.3904]
  cd(0:1, 174  ) = [105.0, 1.3541]
  cd(0:1, 175  ) = [108.0, 1.3179]
  cd(0:1, 176  ) = [111.0, 1.2748]
  cd(0:1, 177  ) = [114.0, 1.2183]
  cd(0:1, 178  ) = [117.0, 1.1618]
  cd(0:1, 179  ) = [120.0, 1.1053]
  cd(0:1, 180  ) = [123.0, 1.0355]
  cd(0:1, 181  ) = [126.0, 0.9657]
  cd(0:1, 182  ) = [129.0, 0.8958]
  cd(0:1, 183  ) = [132.0, 0.8229]
  cd(0:1, 184  ) = [135.0, 0.7483]
  cd(0:1, 185  ) = [138.0, 0.6738]
  cd(0:1, 186  ) = [141.0, 0.6007]
  cd(0:1, 187  ) = [144.0, 0.5305]
  cd(0:1, 188  ) = [147.0, 0.4603]
  cd(0:1, 189  ) = [150.0, 0.3901]
  cd(0:1, 190  ) = [153.0, 0.3349]
  cd(0:1, 191  ) = [156.0, 0.2797]
  cd(0:1, 192  ) = [159.0, 0.2246]
  cd(0:1, 193  ) = [162.0, 0.1856]
  cd(0:1, 194  ) = [165.0, 0.1546]
  cd(0:1, 195  ) = [168.0, 0.1237]
  cd(0:1, 196  ) = [171.0, 0.0928]
  cd(0:1, 197  ) = [174.0, 0.0619]
  cd(0:1, 198  ) = [177.0, 0.051]
  cd(0:1, 199  ) = [180.0, 0.0502]

  IF ( y .EQ. cd(0,0) ) cd13 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd13 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd13 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd13_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd13 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd13_nrel2p8.pro' 
  ENDIF

!print,y,cd13

END FUNCTION calc_cd13_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd14_nrel2p8( y ) RESULT( cd14 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd14
INTEGER :: i

 cd14 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.0487]
  cd(0:1, 1  ) = [-177.0, 0.0487]
  cd(0:1, 2  ) = [-174.0, 0.0585]
  cd(0:1, 3  ) = [-171.0, 0.0877]
  cd(0:1, 4  ) = [-168.0, 0.1173]
  cd(0:1, 5  ) = [-165.0, 0.147]
  cd(0:1, 6  ) = [-162.0, 0.1766]
  cd(0:1, 7  ) = [-159.0, 0.215]
  cd(0:1, 8  ) = [-156.0, 0.2707]
  cd(0:1, 9  ) = [-153.0, 0.3263]
  cd(0:1, 10  ) = [-150.0, 0.382]
  cd(0:1, 11  ) = [-147.0, 0.4525]
  cd(0:1, 12  ) = [-144.0, 0.523]
  cd(0:1, 13  ) = [-141.0, 0.5934]
  cd(0:1, 14  ) = [-138.0, 0.6669]
  cd(0:1, 15  ) = [-135.0, 0.7418]
  cd(0:1, 16  ) = [-132.0, 0.8167]
  cd(0:1, 17  ) = [-129.0, 0.89]
  cd(0:1, 18  ) = [-126.0, 0.9602]
  cd(0:1, 19  ) = [-123.0, 1.0304]
  cd(0:1, 20  ) = [-120.0, 1.1006]
  cd(0:1, 21  ) = [-117.0, 1.1576]
  cd(0:1, 22  ) = [-114.0, 1.2145]
  cd(0:1, 23  ) = [-111.0, 1.2715]
  cd(0:1, 24  ) = [-108.0, 1.315]
  cd(0:1, 25  ) = [-105.0, 1.3517]
  cd(0:1, 26  ) = [-102.0, 1.3885]
  cd(0:1, 27  ) = [-99.0, 1.417]
  cd(0:1, 28  ) = [-96.0, 1.429]
  cd(0:1, 29  ) = [-93.0, 1.441]
  cd(0:1, 30  ) = [-90.0, 1.4531]
  cd(0:1, 31  ) = [-87.0, 1.441]
  cd(0:1, 32  ) = [-84.0, 1.429]
  cd(0:1, 33  ) = [-81.0, 1.417]
  cd(0:1, 34  ) = [-78.0, 1.3885]
  cd(0:1, 35  ) = [-75.0, 1.3517]
  cd(0:1, 36  ) = [-72.0, 1.315]
  cd(0:1, 37  ) = [-69.0, 1.2715]
  cd(0:1, 38  ) = [-66.0, 1.2145]
  cd(0:1, 39  ) = [-63.0, 1.1576]
  cd(0:1, 40  ) = [-60.0, 1.1006]
  cd(0:1, 41  ) = [-57.0, 1.0304]
  cd(0:1, 42  ) = [-54.0, 0.9602]
  cd(0:1, 43  ) = [-51.0, 0.89]
  cd(0:1, 44  ) = [-48.0, 0.8167]
  cd(0:1, 45  ) = [-45.0, 0.7418]
  cd(0:1, 46  ) = [-42.0, 0.6669]
  cd(0:1, 47  ) = [-39.0, 0.5924]
  cd(0:1, 48  ) = [-36.0, 0.5195]
  cd(0:1, 49  ) = [-33.0, 0.4493]
  cd(0:1, 50  ) = [-30.0, 0.382]
  cd(0:1, 51  ) = [-29.3939, 0.3691]
  cd(0:1, 52  ) = [-28.7879, 0.3561]
  cd(0:1, 53  ) = [-28.1818, 0.3432]
  cd(0:1, 54  ) = [-27.5758, 0.3307]
  cd(0:1, 55  ) = [-26.9697, 0.3184]
  cd(0:1, 56  ) = [-26.3636, 0.3061]
  cd(0:1, 57  ) = [-25.7576, 0.294]
  cd(0:1, 58  ) = [-25.1515, 0.2823]
  cd(0:1, 59  ) = [-24.5455, 0.2706]
  cd(0:1, 60  ) = [-23.9394, 0.259]
  cd(0:1, 61  ) = [-23.3333, 0.2481]
  cd(0:1, 62  ) = [-22.7273, 0.2371]
  cd(0:1, 63  ) = [-22.1212, 0.2262]
  cd(0:1, 64  ) = [-21.5152, 0.219]
  cd(0:1, 65  ) = [-20.9091, 0.2128]
  cd(0:1, 66  ) = [-20.303, 0.2065]
  cd(0:1, 67  ) = [-19.697, 0.2004]
  cd(0:1, 68  ) = [-19.0909, 0.1944]
  cd(0:1, 69  ) = [-18.4848, 0.1889]
  cd(0:1, 70  ) = [-17.8788, 0.1833]
  cd(0:1, 71  ) = [-17.2727, 0.1765]
  cd(0:1, 72  ) = [-16.6667, 0.1709]
  cd(0:1, 73  ) = [-16.0606, 0.1656]
  cd(0:1, 74  ) = [-15.4545, 0.1584]
  cd(0:1, 75  ) = [-14.8485, 0.1502]
  cd(0:1, 76  ) = [-14.2424, 0.1393]
  cd(0:1, 77  ) = [-13.6364, 0.1281]
  cd(0:1, 78  ) = [-13.0303, 0.1166]
  cd(0:1, 79  ) = [-12.4242, 0.1055]
  cd(0:1, 80  ) = [-11.8182, 0.0949]
  cd(0:1, 81  ) = [-11.2121, 0.0856]
  cd(0:1, 82  ) = [-10.6061, 0.077]
  cd(0:1, 83  ) = [-10.0, 0.0688]
  cd(0:1, 84  ) = [-9.3939, 0.0573]
  cd(0:1, 85  ) = [-8.7879, 0.0449]
  cd(0:1, 86  ) = [-8.1818, 0.0323]
  cd(0:1, 87  ) = [-7.5758, 0.0248]
  cd(0:1, 88  ) = [-6.9697, 0.0201]
  cd(0:1, 89  ) = [-6.3636, 0.0177]
  cd(0:1, 90  ) = [-5.7576, 0.0157]
  cd(0:1, 91  ) = [-5.1515, 0.0141]
  cd(0:1, 92  ) = [-4.5455, 0.0129]
  cd(0:1, 93  ) = [-3.9394, 0.0119]
  cd(0:1, 94  ) = [-3.3333, 0.0115]
  cd(0:1, 95  ) = [-2.7273, 0.0111]
  cd(0:1, 96  ) = [-2.1212, 0.0107]
  cd(0:1, 97  ) = [-1.5152, 0.0105]
  cd(0:1, 98  ) = [-0.9091, 0.0103]
  cd(0:1, 99  ) = [-0.303, 0.0103]
  cd(0:1, 100  ) = [0.303, 0.0104]
  cd(0:1, 101  ) = [0.9091, 0.0105]
  cd(0:1, 102  ) = [1.5152, 0.0105]
  cd(0:1, 103  ) = [2.1212, 0.0106]
  cd(0:1, 104  ) = [2.7273, 0.0108]
  cd(0:1, 105  ) = [3.3333, 0.011]
  cd(0:1, 106  ) = [3.9394, 0.0112]
  cd(0:1, 107  ) = [4.5455, 0.0115]
  cd(0:1, 108  ) = [5.1515, 0.0117]
  cd(0:1, 109  ) = [5.7576, 0.0121]
  cd(0:1, 110  ) = [6.3636, 0.0124]
  cd(0:1, 111  ) = [6.9697, 0.0128]
  cd(0:1, 112  ) = [7.5758, 0.0133]
  cd(0:1, 113  ) = [8.1818, 0.0138]
  cd(0:1, 114  ) = [8.7879, 0.0142]
  cd(0:1, 115  ) = [9.3939, 0.0152]
  cd(0:1, 116  ) = [10.0, 0.0166]
  cd(0:1, 117  ) = [10.6061, 0.0189]
  cd(0:1, 118  ) = [11.2121, 0.0217]
  cd(0:1, 119  ) = [11.8182, 0.0252]
  cd(0:1, 120  ) = [12.4242, 0.0401]
  cd(0:1, 121  ) = [13.0303, 0.0595]
  cd(0:1, 122  ) = [13.6364, 0.0699]
  cd(0:1, 123  ) = [14.2424, 0.0803]
  cd(0:1, 124  ) = [14.8485, 0.0905]
  cd(0:1, 125  ) = [15.4545, 0.101]
  cd(0:1, 126  ) = [16.0606, 0.1116]
  cd(0:1, 127  ) = [16.6667, 0.1223]
  cd(0:1, 128  ) = [17.2727, 0.1331]
  cd(0:1, 129  ) = [17.8788, 0.1443]
  cd(0:1, 130  ) = [18.4848, 0.1568]
  cd(0:1, 131  ) = [19.0909, 0.1694]
  cd(0:1, 132  ) = [19.697, 0.181]
  cd(0:1, 133  ) = [20.303, 0.1925]
  cd(0:1, 134  ) = [20.9091, 0.2037]
  cd(0:1, 135  ) = [21.5152, 0.215]
  cd(0:1, 136  ) = [22.1212, 0.2262]
  cd(0:1, 137  ) = [22.7273, 0.2371]
  cd(0:1, 138  ) = [23.3333, 0.2481]
  cd(0:1, 139  ) = [23.9394, 0.259]
  cd(0:1, 140  ) = [24.5455, 0.2706]
  cd(0:1, 141  ) = [25.1515, 0.2823]
  cd(0:1, 142  ) = [25.7576, 0.294]
  cd(0:1, 143  ) = [26.3636, 0.3061]
  cd(0:1, 144  ) = [26.9697, 0.3184]
  cd(0:1, 145  ) = [27.5758, 0.3307]
  cd(0:1, 146  ) = [28.1818, 0.3432]
  cd(0:1, 147  ) = [28.7879, 0.3561]
  cd(0:1, 148  ) = [29.3939, 0.3691]
  cd(0:1, 149  ) = [30.0, 0.382]
  cd(0:1, 150  ) = [33.0, 0.4493]
  cd(0:1, 151  ) = [36.0, 0.5195]
  cd(0:1, 152  ) = [39.0, 0.5924]
  cd(0:1, 153  ) = [42.0, 0.6669]
  cd(0:1, 154  ) = [45.0, 0.7418]
  cd(0:1, 155  ) = [48.0, 0.8167]
  cd(0:1, 156  ) = [51.0, 0.89]
  cd(0:1, 157  ) = [54.0, 0.9602]
  cd(0:1, 158  ) = [57.0, 1.0304]
  cd(0:1, 159  ) = [60.0, 1.1006]
  cd(0:1, 160  ) = [63.0, 1.1576]
  cd(0:1, 161  ) = [66.0, 1.2145]
  cd(0:1, 162  ) = [69.0, 1.2715]
  cd(0:1, 163  ) = [72.0, 1.315]
  cd(0:1, 164  ) = [75.0, 1.3517]
  cd(0:1, 165  ) = [78.0, 1.3885]
  cd(0:1, 166  ) = [81.0, 1.417]
  cd(0:1, 167  ) = [84.0, 1.429]
  cd(0:1, 168  ) = [87.0, 1.441]
  cd(0:1, 169  ) = [90.0, 1.4531]
  cd(0:1, 170  ) = [93.0, 1.441]
  cd(0:1, 171  ) = [96.0, 1.429]
  cd(0:1, 172  ) = [99.0, 1.417]
  cd(0:1, 173  ) = [102.0, 1.3885]
  cd(0:1, 174  ) = [105.0, 1.3517]
  cd(0:1, 175  ) = [108.0, 1.315]
  cd(0:1, 176  ) = [111.0, 1.2715]
  cd(0:1, 177  ) = [114.0, 1.2145]
  cd(0:1, 178  ) = [117.0, 1.1576]
  cd(0:1, 179  ) = [120.0, 1.1006]
  cd(0:1, 180  ) = [123.0, 1.0304]
  cd(0:1, 181  ) = [126.0, 0.9602]
  cd(0:1, 182  ) = [129.0, 0.89]
  cd(0:1, 183  ) = [132.0, 0.8167]
  cd(0:1, 184  ) = [135.0, 0.7418]
  cd(0:1, 185  ) = [138.0, 0.6669]
  cd(0:1, 186  ) = [141.0, 0.5934]
  cd(0:1, 187  ) = [144.0, 0.523]
  cd(0:1, 188  ) = [147.0, 0.4525]
  cd(0:1, 189  ) = [150.0, 0.382]
  cd(0:1, 190  ) = [153.0, 0.3276]
  cd(0:1, 191  ) = [156.0, 0.2731]
  cd(0:1, 192  ) = [159.0, 0.2187]
  cd(0:1, 193  ) = [162.0, 0.1805]
  cd(0:1, 194  ) = [165.0, 0.1504]
  cd(0:1, 195  ) = [168.0, 0.1203]
  cd(0:1, 196  ) = [171.0, 0.0903]
  cd(0:1, 197  ) = [174.0, 0.0602]
  cd(0:1, 198  ) = [177.0, 0.0496]
  cd(0:1, 199  ) = [180.0, 0.0487]

  IF ( y .EQ. cd(0,0) ) cd14 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd14 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd14 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd14_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd14 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd14_nrel2p8.pro' 
  ENDIF

!print,y,cd14

END FUNCTION calc_cd14_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd15_nrel2p8( y ) RESULT( cd15 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd15
INTEGER :: i

 cd15 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.0475]
  cd(0:1, 1  ) = [-177.0, 0.0475]
  cd(0:1, 2  ) = [-174.0, 0.057]
  cd(0:1, 3  ) = [-171.0, 0.0855]
  cd(0:1, 4  ) = [-168.0, 0.1141]
  cd(0:1, 5  ) = [-165.0, 0.1428]
  cd(0:1, 6  ) = [-162.0, 0.1714]
  cd(0:1, 7  ) = [-159.0, 0.209]
  cd(0:1, 8  ) = [-156.0, 0.2644]
  cd(0:1, 9  ) = [-153.0, 0.3197]
  cd(0:1, 10  ) = [-150.0, 0.3751]
  cd(0:1, 11  ) = [-147.0, 0.4458]
  cd(0:1, 12  ) = [-144.0, 0.5165]
  cd(0:1, 13  ) = [-141.0, 0.5873]
  cd(0:1, 14  ) = [-138.0, 0.661]
  cd(0:1, 15  ) = [-135.0, 0.7362]
  cd(0:1, 16  ) = [-132.0, 0.8113]
  cd(0:1, 17  ) = [-129.0, 0.885]
  cd(0:1, 18  ) = [-126.0, 0.9555]
  cd(0:1, 19  ) = [-123.0, 1.0261]
  cd(0:1, 20  ) = [-120.0, 1.0966]
  cd(0:1, 21  ) = [-117.0, 1.154]
  cd(0:1, 22  ) = [-114.0, 1.2113]
  cd(0:1, 23  ) = [-111.0, 1.2686]
  cd(0:1, 24  ) = [-108.0, 1.3125]
  cd(0:1, 25  ) = [-105.0, 1.3497]
  cd(0:1, 26  ) = [-102.0, 1.3868]
  cd(0:1, 27  ) = [-99.0, 1.4158]
  cd(0:1, 28  ) = [-96.0, 1.4282]
  cd(0:1, 29  ) = [-93.0, 1.4406]
  cd(0:1, 30  ) = [-90.0, 1.4531]
  cd(0:1, 31  ) = [-87.0, 1.4406]
  cd(0:1, 32  ) = [-84.0, 1.4282]
  cd(0:1, 33  ) = [-81.0, 1.4158]
  cd(0:1, 34  ) = [-78.0, 1.3868]
  cd(0:1, 35  ) = [-75.0, 1.3497]
  cd(0:1, 36  ) = [-72.0, 1.3125]
  cd(0:1, 37  ) = [-69.0, 1.2686]
  cd(0:1, 38  ) = [-66.0, 1.2113]
  cd(0:1, 39  ) = [-63.0, 1.154]
  cd(0:1, 40  ) = [-60.0, 1.0966]
  cd(0:1, 41  ) = [-57.0, 1.0261]
  cd(0:1, 42  ) = [-54.0, 0.9555]
  cd(0:1, 43  ) = [-51.0, 0.885]
  cd(0:1, 44  ) = [-48.0, 0.8114]
  cd(0:1, 45  ) = [-45.0, 0.7362]
  cd(0:1, 46  ) = [-42.0, 0.661]
  cd(0:1, 47  ) = [-39.0, 0.5862]
  cd(0:1, 48  ) = [-36.0, 0.5131]
  cd(0:1, 49  ) = [-33.0, 0.4427]
  cd(0:1, 50  ) = [-30.0, 0.3751]
  cd(0:1, 51  ) = [-29.3939, 0.3621]
  cd(0:1, 52  ) = [-28.7879, 0.3492]
  cd(0:1, 53  ) = [-28.1818, 0.3362]
  cd(0:1, 54  ) = [-27.5758, 0.3237]
  cd(0:1, 55  ) = [-26.9697, 0.3113]
  cd(0:1, 56  ) = [-26.3636, 0.2989]
  cd(0:1, 57  ) = [-25.7576, 0.2868]
  cd(0:1, 58  ) = [-25.1515, 0.2751]
  cd(0:1, 59  ) = [-24.5455, 0.2634]
  cd(0:1, 60  ) = [-23.9394, 0.2518]
  cd(0:1, 61  ) = [-23.3333, 0.2408]
  cd(0:1, 62  ) = [-22.7273, 0.2298]
  cd(0:1, 63  ) = [-22.1212, 0.2188]
  cd(0:1, 64  ) = [-21.5152, 0.2124]
  cd(0:1, 65  ) = [-20.9091, 0.2072]
  cd(0:1, 66  ) = [-20.303, 0.202]
  cd(0:1, 67  ) = [-19.697, 0.1968]
  cd(0:1, 68  ) = [-19.0909, 0.1916]
  cd(0:1, 69  ) = [-18.4848, 0.1869]
  cd(0:1, 70  ) = [-17.8788, 0.1822]
  cd(0:1, 71  ) = [-17.2727, 0.1764]
  cd(0:1, 72  ) = [-16.6667, 0.1713]
  cd(0:1, 73  ) = [-16.0606, 0.1664]
  cd(0:1, 74  ) = [-15.4545, 0.1596]
  cd(0:1, 75  ) = [-14.8485, 0.1515]
  cd(0:1, 76  ) = [-14.2424, 0.1405]
  cd(0:1, 77  ) = [-13.6364, 0.129]
  cd(0:1, 78  ) = [-13.0303, 0.1173]
  cd(0:1, 79  ) = [-12.4242, 0.1059]
  cd(0:1, 80  ) = [-11.8182, 0.0952]
  cd(0:1, 81  ) = [-11.2121, 0.0859]
  cd(0:1, 82  ) = [-10.6061, 0.0774]
  cd(0:1, 83  ) = [-10.0, 0.0694]
  cd(0:1, 84  ) = [-9.3939, 0.0576]
  cd(0:1, 85  ) = [-8.7879, 0.0448]
  cd(0:1, 86  ) = [-8.1818, 0.0308]
  cd(0:1, 87  ) = [-7.5758, 0.0232]
  cd(0:1, 88  ) = [-6.9697, 0.0187]
  cd(0:1, 89  ) = [-6.3636, 0.0168]
  cd(0:1, 90  ) = [-5.7576, 0.0151]
  cd(0:1, 91  ) = [-5.1515, 0.0137]
  cd(0:1, 92  ) = [-4.5455, 0.0127]
  cd(0:1, 93  ) = [-3.9394, 0.0118]
  cd(0:1, 94  ) = [-3.3333, 0.0113]
  cd(0:1, 95  ) = [-2.7273, 0.011]
  cd(0:1, 96  ) = [-2.1212, 0.0107]
  cd(0:1, 97  ) = [-1.5152, 0.0105]
  cd(0:1, 98  ) = [-0.9091, 0.0103]
  cd(0:1, 99  ) = [-0.303, 0.0104]
  cd(0:1, 100  ) = [0.303, 0.0105]
  cd(0:1, 101  ) = [0.9091, 0.0106]
  cd(0:1, 102  ) = [1.5152, 0.0106]
  cd(0:1, 103  ) = [2.1212, 0.0106]
  cd(0:1, 104  ) = [2.7273, 0.0108]
  cd(0:1, 105  ) = [3.3333, 0.011]
  cd(0:1, 106  ) = [3.9394, 0.0112]
  cd(0:1, 107  ) = [4.5455, 0.0114]
  cd(0:1, 108  ) = [5.1515, 0.0117]
  cd(0:1, 109  ) = [5.7576, 0.012]
  cd(0:1, 110  ) = [6.3636, 0.0123]
  cd(0:1, 111  ) = [6.9697, 0.0127]
  cd(0:1, 112  ) = [7.5758, 0.0131]
  cd(0:1, 113  ) = [8.1818, 0.0135]
  cd(0:1, 114  ) = [8.7879, 0.0139]
  cd(0:1, 115  ) = [9.3939, 0.0147]
  cd(0:1, 116  ) = [10.0, 0.0156]
  cd(0:1, 117  ) = [10.6061, 0.0172]
  cd(0:1, 118  ) = [11.2121, 0.0191]
  cd(0:1, 119  ) = [11.8182, 0.0216]
  cd(0:1, 120  ) = [12.4242, 0.038]
  cd(0:1, 121  ) = [13.0303, 0.0596]
  cd(0:1, 122  ) = [13.6364, 0.0694]
  cd(0:1, 123  ) = [14.2424, 0.0789]
  cd(0:1, 124  ) = [14.8485, 0.0881]
  cd(0:1, 125  ) = [15.4545, 0.0973]
  cd(0:1, 126  ) = [16.0606, 0.1066]
  cd(0:1, 127  ) = [16.6667, 0.1161]
  cd(0:1, 128  ) = [17.2727, 0.1259]
  cd(0:1, 129  ) = [17.8788, 0.136]
  cd(0:1, 130  ) = [18.4848, 0.1479]
  cd(0:1, 131  ) = [19.0909, 0.1602]
  cd(0:1, 132  ) = [19.697, 0.1724]
  cd(0:1, 133  ) = [20.303, 0.1842]
  cd(0:1, 134  ) = [20.9091, 0.1958]
  cd(0:1, 135  ) = [21.5152, 0.2073]
  cd(0:1, 136  ) = [22.1212, 0.2188]
  cd(0:1, 137  ) = [22.7273, 0.2298]
  cd(0:1, 138  ) = [23.3333, 0.2408]
  cd(0:1, 139  ) = [23.9394, 0.2518]
  cd(0:1, 140  ) = [24.5455, 0.2634]
  cd(0:1, 141  ) = [25.1515, 0.2751]
  cd(0:1, 142  ) = [25.7576, 0.2868]
  cd(0:1, 143  ) = [26.3636, 0.2989]
  cd(0:1, 144  ) = [26.9697, 0.3113]
  cd(0:1, 145  ) = [27.5758, 0.3237]
  cd(0:1, 146  ) = [28.1818, 0.3362]
  cd(0:1, 147  ) = [28.7879, 0.3492]
  cd(0:1, 148  ) = [29.3939, 0.3621]
  cd(0:1, 149  ) = [30.0, 0.3751]
  cd(0:1, 150  ) = [33.0, 0.4427]
  cd(0:1, 151  ) = [36.0, 0.5131]
  cd(0:1, 152  ) = [39.0, 0.5862]
  cd(0:1, 153  ) = [42.0, 0.661]
  cd(0:1, 154  ) = [45.0, 0.7362]
  cd(0:1, 155  ) = [48.0, 0.8114]
  cd(0:1, 156  ) = [51.0, 0.885]
  cd(0:1, 157  ) = [54.0, 0.9555]
  cd(0:1, 158  ) = [57.0, 1.0261]
  cd(0:1, 159  ) = [60.0, 1.0966]
  cd(0:1, 160  ) = [63.0, 1.154]
  cd(0:1, 161  ) = [66.0, 1.2113]
  cd(0:1, 162  ) = [69.0, 1.2686]
  cd(0:1, 163  ) = [72.0, 1.3125]
  cd(0:1, 164  ) = [75.0, 1.3497]
  cd(0:1, 165  ) = [78.0, 1.3868]
  cd(0:1, 166  ) = [81.0, 1.4158]
  cd(0:1, 167  ) = [84.0, 1.4282]
  cd(0:1, 168  ) = [87.0, 1.4406]
  cd(0:1, 169  ) = [90.0, 1.4531]
  cd(0:1, 170  ) = [93.0, 1.4406]
  cd(0:1, 171  ) = [96.0, 1.4282]
  cd(0:1, 172  ) = [99.0, 1.4158]
  cd(0:1, 173  ) = [102.0, 1.3868]
  cd(0:1, 174  ) = [105.0, 1.3497]
  cd(0:1, 175  ) = [108.0, 1.3125]
  cd(0:1, 176  ) = [111.0, 1.2686]
  cd(0:1, 177  ) = [114.0, 1.2113]
  cd(0:1, 178  ) = [117.0, 1.154]
  cd(0:1, 179  ) = [120.0, 1.0966]
  cd(0:1, 180  ) = [123.0, 1.0261]
  cd(0:1, 181  ) = [126.0, 0.9555]
  cd(0:1, 182  ) = [129.0, 0.885]
  cd(0:1, 183  ) = [132.0, 0.8113]
  cd(0:1, 184  ) = [135.0, 0.7362]
  cd(0:1, 185  ) = [138.0, 0.661]
  cd(0:1, 186  ) = [141.0, 0.5873]
  cd(0:1, 187  ) = [144.0, 0.5165]
  cd(0:1, 188  ) = [147.0, 0.4458]
  cd(0:1, 189  ) = [150.0, 0.3751]
  cd(0:1, 190  ) = [153.0, 0.3213]
  cd(0:1, 191  ) = [156.0, 0.2675]
  cd(0:1, 192  ) = [159.0, 0.2137]
  cd(0:1, 193  ) = [162.0, 0.1762]
  cd(0:1, 194  ) = [165.0, 0.1468]
  cd(0:1, 195  ) = [168.0, 0.1174]
  cd(0:1, 196  ) = [171.0, 0.0881]
  cd(0:1, 197  ) = [174.0, 0.0587]
  cd(0:1, 198  ) = [177.0, 0.0484]
  cd(0:1, 199  ) = [180.0, 0.0475]

  IF ( y .EQ. cd(0,0) ) cd15 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd15 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd15 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd15_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd15 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd15_nrel2p8.pro' 
  ENDIF

!print,y,cd15

END FUNCTION calc_cd15_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd16_nrel2p8( y ) RESULT( cd16 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd16
INTEGER :: i

 cd16 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.0468]
  cd(0:1, 1  ) = [-177.0, 0.0468]
  cd(0:1, 2  ) = [-174.0, 0.0561]
  cd(0:1, 3  ) = [-171.0, 0.0842]
  cd(0:1, 4  ) = [-168.0, 0.1122]
  cd(0:1, 5  ) = [-165.0, 0.1403]
  cd(0:1, 6  ) = [-162.0, 0.1684]
  cd(0:1, 7  ) = [-159.0, 0.2055]
  cd(0:1, 8  ) = [-156.0, 0.2607]
  cd(0:1, 9  ) = [-153.0, 0.3159]
  cd(0:1, 10  ) = [-150.0, 0.3711]
  cd(0:1, 11  ) = [-147.0, 0.4419]
  cd(0:1, 12  ) = [-144.0, 0.5128]
  cd(0:1, 13  ) = [-141.0, 0.5836]
  cd(0:1, 14  ) = [-138.0, 0.6575]
  cd(0:1, 15  ) = [-135.0, 0.7329]
  cd(0:1, 16  ) = [-132.0, 0.8082]
  cd(0:1, 17  ) = [-129.0, 0.8821]
  cd(0:1, 18  ) = [-126.0, 0.9528]
  cd(0:1, 19  ) = [-123.0, 1.0236]
  cd(0:1, 20  ) = [-120.0, 1.0943]
  cd(0:1, 21  ) = [-117.0, 1.1519]
  cd(0:1, 22  ) = [-114.0, 1.2094]
  cd(0:1, 23  ) = [-111.0, 1.267]
  cd(0:1, 24  ) = [-108.0, 1.3111]
  cd(0:1, 25  ) = [-105.0, 1.3485]
  cd(0:1, 26  ) = [-102.0, 1.3859]
  cd(0:1, 27  ) = [-99.0, 1.415]
  cd(0:1, 28  ) = [-96.0, 1.4277]
  cd(0:1, 29  ) = [-93.0, 1.4404]
  cd(0:1, 30  ) = [-90.0, 1.4531]
  cd(0:1, 31  ) = [-87.0, 1.4404]
  cd(0:1, 32  ) = [-84.0, 1.4277]
  cd(0:1, 33  ) = [-81.0, 1.415]
  cd(0:1, 34  ) = [-78.0, 1.3859]
  cd(0:1, 35  ) = [-75.0, 1.3485]
  cd(0:1, 36  ) = [-72.0, 1.3111]
  cd(0:1, 37  ) = [-69.0, 1.267]
  cd(0:1, 38  ) = [-66.0, 1.2094]
  cd(0:1, 39  ) = [-63.0, 1.1519]
  cd(0:1, 40  ) = [-60.0, 1.0943]
  cd(0:1, 41  ) = [-57.0, 1.0236]
  cd(0:1, 42  ) = [-54.0, 0.9528]
  cd(0:1, 43  ) = [-51.0, 0.8821]
  cd(0:1, 44  ) = [-48.0, 0.8082]
  cd(0:1, 45  ) = [-45.0, 0.7329]
  cd(0:1, 46  ) = [-42.0, 0.6575]
  cd(0:1, 47  ) = [-39.0, 0.5826]
  cd(0:1, 48  ) = [-36.0, 0.5093]
  cd(0:1, 49  ) = [-33.0, 0.4388]
  cd(0:1, 50  ) = [-30.0, 0.3711]
  cd(0:1, 51  ) = [-29.3939, 0.3581]
  cd(0:1, 52  ) = [-28.7879, 0.3451]
  cd(0:1, 53  ) = [-28.1818, 0.3321]
  cd(0:1, 54  ) = [-27.5758, 0.3195]
  cd(0:1, 55  ) = [-26.9697, 0.3071]
  cd(0:1, 56  ) = [-26.3636, 0.2948]
  cd(0:1, 57  ) = [-25.7576, 0.2826]
  cd(0:1, 58  ) = [-25.1515, 0.2709]
  cd(0:1, 59  ) = [-24.5455, 0.2592]
  cd(0:1, 60  ) = [-23.9394, 0.2475]
  cd(0:1, 61  ) = [-23.3333, 0.2365]
  cd(0:1, 62  ) = [-22.7273, 0.2255]
  cd(0:1, 63  ) = [-22.1212, 0.2145]
  cd(0:1, 64  ) = [-21.5152, 0.2085]
  cd(0:1, 65  ) = [-20.9091, 0.2039]
  cd(0:1, 66  ) = [-20.303, 0.1993]
  cd(0:1, 67  ) = [-19.697, 0.1946]
  cd(0:1, 68  ) = [-19.0909, 0.19]
  cd(0:1, 69  ) = [-18.4848, 0.1855]
  cd(0:1, 70  ) = [-17.8788, 0.181]
  cd(0:1, 71  ) = [-17.2727, 0.1761]
  cd(0:1, 72  ) = [-16.6667, 0.1715]
  cd(0:1, 73  ) = [-16.0606, 0.1668]
  cd(0:1, 74  ) = [-15.4545, 0.1601]
  cd(0:1, 75  ) = [-14.8485, 0.1522]
  cd(0:1, 76  ) = [-14.2424, 0.141]
  cd(0:1, 77  ) = [-13.6364, 0.1294]
  cd(0:1, 78  ) = [-13.0303, 0.1176]
  cd(0:1, 79  ) = [-12.4242, 0.1061]
  cd(0:1, 80  ) = [-11.8182, 0.0953]
  cd(0:1, 81  ) = [-11.2121, 0.086]
  cd(0:1, 82  ) = [-10.6061, 0.0776]
  cd(0:1, 83  ) = [-10.0, 0.0697]
  cd(0:1, 84  ) = [-9.3939, 0.0577]
  cd(0:1, 85  ) = [-8.7879, 0.0445]
  cd(0:1, 86  ) = [-8.1818, 0.0291]
  cd(0:1, 87  ) = [-7.5758, 0.0214]
  cd(0:1, 88  ) = [-6.9697, 0.0173]
  cd(0:1, 89  ) = [-6.3636, 0.0158]
  cd(0:1, 90  ) = [-5.7576, 0.0144]
  cd(0:1, 91  ) = [-5.1515, 0.0131]
  cd(0:1, 92  ) = [-4.5455, 0.0123]
  cd(0:1, 93  ) = [-3.9394, 0.0115]
  cd(0:1, 94  ) = [-3.3333, 0.0111]
  cd(0:1, 95  ) = [-2.7273, 0.0108]
  cd(0:1, 96  ) = [-2.1212, 0.0106]
  cd(0:1, 97  ) = [-1.5152, 0.0104]
  cd(0:1, 98  ) = [-0.9091, 0.0103]
  cd(0:1, 99  ) = [-0.303, 0.0104]
  cd(0:1, 100  ) = [0.303, 0.0105]
  cd(0:1, 101  ) = [0.9091, 0.0106]
  cd(0:1, 102  ) = [1.5152, 0.0106]
  cd(0:1, 103  ) = [2.1212, 0.0106]
  cd(0:1, 104  ) = [2.7273, 0.0108]
  cd(0:1, 105  ) = [3.3333, 0.011]
  cd(0:1, 106  ) = [3.9394, 0.0111]
  cd(0:1, 107  ) = [4.5455, 0.0113]
  cd(0:1, 108  ) = [5.1515, 0.0115]
  cd(0:1, 109  ) = [5.7576, 0.0118]
  cd(0:1, 110  ) = [6.3636, 0.0121]
  cd(0:1, 111  ) = [6.9697, 0.0124]
  cd(0:1, 112  ) = [7.5758, 0.0128]
  cd(0:1, 113  ) = [8.1818, 0.0132]
  cd(0:1, 114  ) = [8.7879, 0.0137]
  cd(0:1, 115  ) = [9.3939, 0.0144]
  cd(0:1, 116  ) = [10.0, 0.0151]
  cd(0:1, 117  ) = [10.6061, 0.0162]
  cd(0:1, 118  ) = [11.2121, 0.0176]
  cd(0:1, 119  ) = [11.8182, 0.0195]
  cd(0:1, 120  ) = [12.4242, 0.0367]
  cd(0:1, 121  ) = [13.0303, 0.0596]
  cd(0:1, 122  ) = [13.6364, 0.0691]
  cd(0:1, 123  ) = [14.2424, 0.0782]
  cd(0:1, 124  ) = [14.8485, 0.0867]
  cd(0:1, 125  ) = [15.4545, 0.0952]
  cd(0:1, 126  ) = [16.0606, 0.1037]
  cd(0:1, 127  ) = [16.6667, 0.1125]
  cd(0:1, 128  ) = [17.2727, 0.1216]
  cd(0:1, 129  ) = [17.8788, 0.1312]
  cd(0:1, 130  ) = [18.4848, 0.1427]
  cd(0:1, 131  ) = [19.0909, 0.1548]
  cd(0:1, 132  ) = [19.697, 0.1673]
  cd(0:1, 133  ) = [20.303, 0.1794]
  cd(0:1, 134  ) = [20.9091, 0.1911]
  cd(0:1, 135  ) = [21.5152, 0.2029]
  cd(0:1, 136  ) = [22.1212, 0.2145]
  cd(0:1, 137  ) = [22.7273, 0.2255]
  cd(0:1, 138  ) = [23.3333, 0.2365]
  cd(0:1, 139  ) = [23.9394, 0.2475]
  cd(0:1, 140  ) = [24.5455, 0.2592]
  cd(0:1, 141  ) = [25.1515, 0.2709]
  cd(0:1, 142  ) = [25.7576, 0.2826]
  cd(0:1, 143  ) = [26.3636, 0.2948]
  cd(0:1, 144  ) = [26.9697, 0.3071]
  cd(0:1, 145  ) = [27.5758, 0.3195]
  cd(0:1, 146  ) = [28.1818, 0.3321]
  cd(0:1, 147  ) = [28.7879, 0.3451]
  cd(0:1, 148  ) = [29.3939, 0.3581]
  cd(0:1, 149  ) = [30.0, 0.3711]
  cd(0:1, 150  ) = [33.0, 0.4388]
  cd(0:1, 151  ) = [36.0, 0.5093]
  cd(0:1, 152  ) = [39.0, 0.5826]
  cd(0:1, 153  ) = [42.0, 0.6575]
  cd(0:1, 154  ) = [45.0, 0.7329]
  cd(0:1, 155  ) = [48.0, 0.8082]
  cd(0:1, 156  ) = [51.0, 0.8821]
  cd(0:1, 157  ) = [54.0, 0.9528]
  cd(0:1, 158  ) = [57.0, 1.0236]
  cd(0:1, 159  ) = [60.0, 1.0943]
  cd(0:1, 160  ) = [63.0, 1.1519]
  cd(0:1, 161  ) = [66.0, 1.2094]
  cd(0:1, 162  ) = [69.0, 1.267]
  cd(0:1, 163  ) = [72.0, 1.3111]
  cd(0:1, 164  ) = [75.0, 1.3485]
  cd(0:1, 165  ) = [78.0, 1.3859]
  cd(0:1, 166  ) = [81.0, 1.415]
  cd(0:1, 167  ) = [84.0, 1.4277]
  cd(0:1, 168  ) = [87.0, 1.4404]
  cd(0:1, 169  ) = [90.0, 1.4531]
  cd(0:1, 170  ) = [93.0, 1.4404]
  cd(0:1, 171  ) = [96.0, 1.4277]
  cd(0:1, 172  ) = [99.0, 1.415]
  cd(0:1, 173  ) = [102.0, 1.3859]
  cd(0:1, 174  ) = [105.0, 1.3485]
  cd(0:1, 175  ) = [108.0, 1.3111]
  cd(0:1, 176  ) = [111.0, 1.267]
  cd(0:1, 177  ) = [114.0, 1.2094]
  cd(0:1, 178  ) = [117.0, 1.1519]
  cd(0:1, 179  ) = [120.0, 1.0943]
  cd(0:1, 180  ) = [123.0, 1.0236]
  cd(0:1, 181  ) = [126.0, 0.9528]
  cd(0:1, 182  ) = [129.0, 0.8821]
  cd(0:1, 183  ) = [132.0, 0.8082]
  cd(0:1, 184  ) = [135.0, 0.7329]
  cd(0:1, 185  ) = [138.0, 0.6575]
  cd(0:1, 186  ) = [141.0, 0.5836]
  cd(0:1, 187  ) = [144.0, 0.5128]
  cd(0:1, 188  ) = [147.0, 0.4419]
  cd(0:1, 189  ) = [150.0, 0.3711]
  cd(0:1, 190  ) = [153.0, 0.3176]
  cd(0:1, 191  ) = [156.0, 0.2642]
  cd(0:1, 192  ) = [159.0, 0.2107]
  cd(0:1, 193  ) = [162.0, 0.1736]
  cd(0:1, 194  ) = [165.0, 0.1447]
  cd(0:1, 195  ) = [168.0, 0.1158]
  cd(0:1, 196  ) = [171.0, 0.0868]
  cd(0:1, 197  ) = [174.0, 0.0579]
  cd(0:1, 198  ) = [177.0, 0.0476]
  cd(0:1, 199  ) = [180.0, 0.0468]

  IF ( y .EQ. cd(0,0) ) cd16 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd16 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd16 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd16_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd16 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd16_nrel2p8.pro' 
  ENDIF

!print,y,cd16

END FUNCTION calc_cd16_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd17_nrel2p8( y ) RESULT( cd17 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd17
INTEGER :: i

 cd17 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.0475]
  cd(0:1, 1  ) = [-177.0, 0.0475]
  cd(0:1, 2  ) = [-174.0, 0.057]
  cd(0:1, 3  ) = [-171.0, 0.0855]
  cd(0:1, 4  ) = [-168.0, 0.114]
  cd(0:1, 5  ) = [-165.0, 0.1425]
  cd(0:1, 6  ) = [-162.0, 0.171]
  cd(0:1, 7  ) = [-159.0, 0.2083]
  cd(0:1, 8  ) = [-156.0, 0.2634]
  cd(0:1, 9  ) = [-153.0, 0.3184]
  cd(0:1, 10  ) = [-150.0, 0.3735]
  cd(0:1, 11  ) = [-147.0, 0.4442]
  cd(0:1, 12  ) = [-144.0, 0.515]
  cd(0:1, 13  ) = [-141.0, 0.5858]
  cd(0:1, 14  ) = [-138.0, 0.6596]
  cd(0:1, 15  ) = [-135.0, 0.7348]
  cd(0:1, 16  ) = [-132.0, 0.8101]
  cd(0:1, 17  ) = [-129.0, 0.8838]
  cd(0:1, 18  ) = [-126.0, 0.9544]
  cd(0:1, 19  ) = [-123.0, 1.0251]
  cd(0:1, 20  ) = [-120.0, 1.0957]
  cd(0:1, 21  ) = [-117.0, 1.1531]
  cd(0:1, 22  ) = [-114.0, 1.2105]
  cd(0:1, 23  ) = [-111.0, 1.268]
  cd(0:1, 24  ) = [-108.0, 1.3119]
  cd(0:1, 25  ) = [-105.0, 1.3492]
  cd(0:1, 26  ) = [-102.0, 1.3865]
  cd(0:1, 27  ) = [-99.0, 1.4155]
  cd(0:1, 28  ) = [-96.0, 1.428]
  cd(0:1, 29  ) = [-93.0, 1.4405]
  cd(0:1, 30  ) = [-90.0, 1.4531]
  cd(0:1, 31  ) = [-87.0, 1.4405]
  cd(0:1, 32  ) = [-84.0, 1.428]
  cd(0:1, 33  ) = [-81.0, 1.4155]
  cd(0:1, 34  ) = [-78.0, 1.3865]
  cd(0:1, 35  ) = [-75.0, 1.3492]
  cd(0:1, 36  ) = [-72.0, 1.3119]
  cd(0:1, 37  ) = [-69.0, 1.268]
  cd(0:1, 38  ) = [-66.0, 1.2105]
  cd(0:1, 39  ) = [-63.0, 1.1531]
  cd(0:1, 40  ) = [-60.0, 1.0957]
  cd(0:1, 41  ) = [-57.0, 1.0251]
  cd(0:1, 42  ) = [-54.0, 0.9544]
  cd(0:1, 43  ) = [-51.0, 0.8838]
  cd(0:1, 44  ) = [-48.0, 0.8101]
  cd(0:1, 45  ) = [-45.0, 0.7349]
  cd(0:1, 46  ) = [-42.0, 0.6596]
  cd(0:1, 47  ) = [-39.0, 0.5847]
  cd(0:1, 48  ) = [-36.0, 0.5115]
  cd(0:1, 49  ) = [-33.0, 0.4411]
  cd(0:1, 50  ) = [-30.0, 0.3735]
  cd(0:1, 51  ) = [-29.3939, 0.3605]
  cd(0:1, 52  ) = [-28.7879, 0.3475]
  cd(0:1, 53  ) = [-28.1818, 0.3345]
  cd(0:1, 54  ) = [-27.5758, 0.322]
  cd(0:1, 55  ) = [-26.9697, 0.3096]
  cd(0:1, 56  ) = [-26.3636, 0.2972]
  cd(0:1, 57  ) = [-25.7576, 0.2851]
  cd(0:1, 58  ) = [-25.1515, 0.2734]
  cd(0:1, 59  ) = [-24.5455, 0.2617]
  cd(0:1, 60  ) = [-23.9394, 0.25]
  cd(0:1, 61  ) = [-23.3333, 0.239]
  cd(0:1, 62  ) = [-22.7273, 0.228]
  cd(0:1, 63  ) = [-22.1212, 0.217]
  cd(0:1, 64  ) = [-21.5152, 0.2107]
  cd(0:1, 65  ) = [-20.9091, 0.2056]
  cd(0:1, 66  ) = [-20.303, 0.2005]
  cd(0:1, 67  ) = [-19.697, 0.1954]
  cd(0:1, 68  ) = [-19.0909, 0.1903]
  cd(0:1, 69  ) = [-18.4848, 0.1849]
  cd(0:1, 70  ) = [-17.8788, 0.1796]
  cd(0:1, 71  ) = [-17.2727, 0.1753]
  cd(0:1, 72  ) = [-16.6667, 0.1705]
  cd(0:1, 73  ) = [-16.0606, 0.1654]
  cd(0:1, 74  ) = [-15.4545, 0.1584]
  cd(0:1, 75  ) = [-14.8485, 0.1502]
  cd(0:1, 76  ) = [-14.2424, 0.139]
  cd(0:1, 77  ) = [-13.6364, 0.1274]
  cd(0:1, 78  ) = [-13.0303, 0.1155]
  cd(0:1, 79  ) = [-12.4242, 0.1038]
  cd(0:1, 80  ) = [-11.8182, 0.0926]
  cd(0:1, 81  ) = [-11.2121, 0.0831]
  cd(0:1, 82  ) = [-10.6061, 0.0746]
  cd(0:1, 83  ) = [-10.0, 0.0667]
  cd(0:1, 84  ) = [-9.3939, 0.0551]
  cd(0:1, 85  ) = [-8.7879, 0.0423]
  cd(0:1, 86  ) = [-8.1818, 0.0265]
  cd(0:1, 87  ) = [-7.5758, 0.0194]
  cd(0:1, 88  ) = [-6.9697, 0.0157]
  cd(0:1, 89  ) = [-6.3636, 0.0144]
  cd(0:1, 90  ) = [-5.7576, 0.0133]
  cd(0:1, 91  ) = [-5.1515, 0.0123]
  cd(0:1, 92  ) = [-4.5455, 0.0116]
  cd(0:1, 93  ) = [-3.9394, 0.011]
  cd(0:1, 94  ) = [-3.3333, 0.0107]
  cd(0:1, 95  ) = [-2.7273, 0.0104]
  cd(0:1, 96  ) = [-2.1212, 0.0104]
  cd(0:1, 97  ) = [-1.5152, 0.0102]
  cd(0:1, 98  ) = [-0.9091, 0.0102]
  cd(0:1, 99  ) = [-0.303, 0.0102]
  cd(0:1, 100  ) = [0.303, 0.0103]
  cd(0:1, 101  ) = [0.9091, 0.0104]
  cd(0:1, 102  ) = [1.5152, 0.0104]
  cd(0:1, 103  ) = [2.1212, 0.0105]
  cd(0:1, 104  ) = [2.7273, 0.0106]
  cd(0:1, 105  ) = [3.3333, 0.0108]
  cd(0:1, 106  ) = [3.9394, 0.0109]
  cd(0:1, 107  ) = [4.5455, 0.011]
  cd(0:1, 108  ) = [5.1515, 0.0112]
  cd(0:1, 109  ) = [5.7576, 0.0114]
  cd(0:1, 110  ) = [6.3636, 0.0117]
  cd(0:1, 111  ) = [6.9697, 0.012]
  cd(0:1, 112  ) = [7.5758, 0.0123]
  cd(0:1, 113  ) = [8.1818, 0.0128]
  cd(0:1, 114  ) = [8.7879, 0.0137]
  cd(0:1, 115  ) = [9.3939, 0.0151]
  cd(0:1, 116  ) = [10.0, 0.0168]
  cd(0:1, 117  ) = [10.6061, 0.0187]
  cd(0:1, 118  ) = [11.2121, 0.0206]
  cd(0:1, 119  ) = [11.8182, 0.0228]
  cd(0:1, 120  ) = [12.4242, 0.0392]
  cd(0:1, 121  ) = [13.0303, 0.0611]
  cd(0:1, 122  ) = [13.6364, 0.0705]
  cd(0:1, 123  ) = [14.2424, 0.0797]
  cd(0:1, 124  ) = [14.8485, 0.0884]
  cd(0:1, 125  ) = [15.4545, 0.0969]
  cd(0:1, 126  ) = [16.0606, 0.1054]
  cd(0:1, 127  ) = [16.6667, 0.114]
  cd(0:1, 128  ) = [17.2727, 0.1231]
  cd(0:1, 129  ) = [17.8788, 0.1328]
  cd(0:1, 130  ) = [18.4848, 0.1444]
  cd(0:1, 131  ) = [19.0909, 0.1565]
  cd(0:1, 132  ) = [19.697, 0.1689]
  cd(0:1, 133  ) = [20.303, 0.1811]
  cd(0:1, 134  ) = [20.9091, 0.1931]
  cd(0:1, 135  ) = [21.5152, 0.2051]
  cd(0:1, 136  ) = [22.1212, 0.2169]
  cd(0:1, 137  ) = [22.7273, 0.2279]
  cd(0:1, 138  ) = [23.3333, 0.239]
  cd(0:1, 139  ) = [23.9394, 0.25]
  cd(0:1, 140  ) = [24.5455, 0.2617]
  cd(0:1, 141  ) = [25.1515, 0.2734]
  cd(0:1, 142  ) = [25.7576, 0.2851]
  cd(0:1, 143  ) = [26.3636, 0.2972]
  cd(0:1, 144  ) = [26.9697, 0.3096]
  cd(0:1, 145  ) = [27.5758, 0.322]
  cd(0:1, 146  ) = [28.1818, 0.3345]
  cd(0:1, 147  ) = [28.7879, 0.3475]
  cd(0:1, 148  ) = [29.3939, 0.3605]
  cd(0:1, 149  ) = [30.0, 0.3735]
  cd(0:1, 150  ) = [33.0, 0.4411]
  cd(0:1, 151  ) = [36.0, 0.5115]
  cd(0:1, 152  ) = [39.0, 0.5847]
  cd(0:1, 153  ) = [42.0, 0.6596]
  cd(0:1, 154  ) = [45.0, 0.7349]
  cd(0:1, 155  ) = [48.0, 0.8101]
  cd(0:1, 156  ) = [51.0, 0.8838]
  cd(0:1, 157  ) = [54.0, 0.9544]
  cd(0:1, 158  ) = [57.0, 1.0251]
  cd(0:1, 159  ) = [60.0, 1.0957]
  cd(0:1, 160  ) = [63.0, 1.1531]
  cd(0:1, 161  ) = [66.0, 1.2105]
  cd(0:1, 162  ) = [69.0, 1.268]
  cd(0:1, 163  ) = [72.0, 1.3119]
  cd(0:1, 164  ) = [75.0, 1.3492]
  cd(0:1, 165  ) = [78.0, 1.3865]
  cd(0:1, 166  ) = [81.0, 1.4155]
  cd(0:1, 167  ) = [84.0, 1.428]
  cd(0:1, 168  ) = [87.0, 1.4405]
  cd(0:1, 169  ) = [90.0, 1.4531]
  cd(0:1, 170  ) = [93.0, 1.4405]
  cd(0:1, 171  ) = [96.0, 1.428]
  cd(0:1, 172  ) = [99.0, 1.4155]
  cd(0:1, 173  ) = [102.0, 1.3865]
  cd(0:1, 174  ) = [105.0, 1.3492]
  cd(0:1, 175  ) = [108.0, 1.3119]
  cd(0:1, 176  ) = [111.0, 1.268]
  cd(0:1, 177  ) = [114.0, 1.2105]
  cd(0:1, 178  ) = [117.0, 1.1531]
  cd(0:1, 179  ) = [120.0, 1.0957]
  cd(0:1, 180  ) = [123.0, 1.0251]
  cd(0:1, 181  ) = [126.0, 0.9544]
  cd(0:1, 182  ) = [129.0, 0.8838]
  cd(0:1, 183  ) = [132.0, 0.8101]
  cd(0:1, 184  ) = [135.0, 0.7348]
  cd(0:1, 185  ) = [138.0, 0.6596]
  cd(0:1, 186  ) = [141.0, 0.5858]
  cd(0:1, 187  ) = [144.0, 0.515]
  cd(0:1, 188  ) = [147.0, 0.4442]
  cd(0:1, 189  ) = [150.0, 0.3735]
  cd(0:1, 190  ) = [153.0, 0.3201]
  cd(0:1, 191  ) = [156.0, 0.2668]
  cd(0:1, 192  ) = [159.0, 0.2135]
  cd(0:1, 193  ) = [162.0, 0.1762]
  cd(0:1, 194  ) = [165.0, 0.1468]
  cd(0:1, 195  ) = [168.0, 0.1174]
  cd(0:1, 196  ) = [171.0, 0.0881]
  cd(0:1, 197  ) = [174.0, 0.0587]
  cd(0:1, 198  ) = [177.0, 0.0484]
  cd(0:1, 199  ) = [180.0, 0.0475]

  IF ( y .EQ. cd(0,0) ) cd17 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd17 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd17 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd17_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd17 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd17_nrel2p8.pro' 
  ENDIF

!print,y,cd17

END FUNCTION calc_cd17_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd18_nrel2p8( y ) RESULT( cd18 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd18
INTEGER :: i

 cd18 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.0502]
  cd(0:1, 1  ) = [-177.0, 0.0502]
  cd(0:1, 2  ) = [-174.0, 0.0603]
  cd(0:1, 3  ) = [-171.0, 0.0904]
  cd(0:1, 4  ) = [-168.0, 0.1206]
  cd(0:1, 5  ) = [-165.0, 0.1507]
  cd(0:1, 6  ) = [-162.0, 0.1809]
  cd(0:1, 7  ) = [-159.0, 0.2191]
  cd(0:1, 8  ) = [-156.0, 0.2737]
  cd(0:1, 9  ) = [-153.0, 0.3282]
  cd(0:1, 10  ) = [-150.0, 0.3827]
  cd(0:1, 11  ) = [-147.0, 0.4532]
  cd(0:1, 12  ) = [-144.0, 0.5236]
  cd(0:1, 13  ) = [-141.0, 0.5941]
  cd(0:1, 14  ) = [-138.0, 0.6675]
  cd(0:1, 15  ) = [-135.0, 0.7424]
  cd(0:1, 16  ) = [-132.0, 0.8172]
  cd(0:1, 17  ) = [-129.0, 0.8905]
  cd(0:1, 18  ) = [-126.0, 0.9607]
  cd(0:1, 19  ) = [-123.0, 1.0309]
  cd(0:1, 20  ) = [-120.0, 1.101]
  cd(0:1, 21  ) = [-117.0, 1.158]
  cd(0:1, 22  ) = [-114.0, 1.2149]
  cd(0:1, 23  ) = [-111.0, 1.2718]
  cd(0:1, 24  ) = [-108.0, 1.3152]
  cd(0:1, 25  ) = [-105.0, 1.352]
  cd(0:1, 26  ) = [-102.0, 1.3887]
  cd(0:1, 27  ) = [-99.0, 1.4171]
  cd(0:1, 28  ) = [-96.0, 1.4291]
  cd(0:1, 29  ) = [-93.0, 1.4411]
  cd(0:1, 30  ) = [-90.0, 1.4531]
  cd(0:1, 31  ) = [-87.0, 1.4411]
  cd(0:1, 32  ) = [-84.0, 1.4291]
  cd(0:1, 33  ) = [-81.0, 1.4171]
  cd(0:1, 34  ) = [-78.0, 1.3887]
  cd(0:1, 35  ) = [-75.0, 1.352]
  cd(0:1, 36  ) = [-72.0, 1.3152]
  cd(0:1, 37  ) = [-69.0, 1.2718]
  cd(0:1, 38  ) = [-66.0, 1.2149]
  cd(0:1, 39  ) = [-63.0, 1.158]
  cd(0:1, 40  ) = [-60.0, 1.101]
  cd(0:1, 41  ) = [-57.0, 1.0309]
  cd(0:1, 42  ) = [-54.0, 0.9607]
  cd(0:1, 43  ) = [-51.0, 0.8905]
  cd(0:1, 44  ) = [-48.0, 0.8172]
  cd(0:1, 45  ) = [-45.0, 0.7424]
  cd(0:1, 46  ) = [-42.0, 0.6675]
  cd(0:1, 47  ) = [-39.0, 0.593]
  cd(0:1, 48  ) = [-36.0, 0.5202]
  cd(0:1, 49  ) = [-33.0, 0.45]
  cd(0:1, 50  ) = [-30.0, 0.3827]
  cd(0:1, 51  ) = [-29.3939, 0.3698]
  cd(0:1, 52  ) = [-28.7879, 0.3569]
  cd(0:1, 53  ) = [-28.1818, 0.344]
  cd(0:1, 54  ) = [-27.5758, 0.3315]
  cd(0:1, 55  ) = [-26.9697, 0.3192]
  cd(0:1, 56  ) = [-26.3636, 0.3068]
  cd(0:1, 57  ) = [-25.7576, 0.2948]
  cd(0:1, 58  ) = [-25.1515, 0.2831]
  cd(0:1, 59  ) = [-24.5455, 0.2714]
  cd(0:1, 60  ) = [-23.9394, 0.2598]
  cd(0:1, 61  ) = [-23.3333, 0.2488]
  cd(0:1, 62  ) = [-22.7273, 0.2378]
  cd(0:1, 63  ) = [-22.1212, 0.2269]
  cd(0:1, 64  ) = [-21.5152, 0.2191]
  cd(0:1, 65  ) = [-20.9091, 0.2122]
  cd(0:1, 66  ) = [-20.303, 0.2053]
  cd(0:1, 67  ) = [-19.697, 0.1984]
  cd(0:1, 68  ) = [-19.0909, 0.1915]
  cd(0:1, 69  ) = [-18.4848, 0.1845]
  cd(0:1, 70  ) = [-17.8788, 0.1781]
  cd(0:1, 71  ) = [-17.2727, 0.1727]
  cd(0:1, 72  ) = [-16.6667, 0.1664]
  cd(0:1, 73  ) = [-16.0606, 0.1598]
  cd(0:1, 74  ) = [-15.4545, 0.1516]
  cd(0:1, 75  ) = [-14.8485, 0.1424]
  cd(0:1, 76  ) = [-14.2424, 0.1311]
  cd(0:1, 77  ) = [-13.6364, 0.1195]
  cd(0:1, 78  ) = [-13.0303, 0.1076]
  cd(0:1, 79  ) = [-12.4242, 0.0949]
  cd(0:1, 80  ) = [-11.8182, 0.0826]
  cd(0:1, 81  ) = [-11.2121, 0.0724]
  cd(0:1, 82  ) = [-10.6061, 0.0637]
  cd(0:1, 83  ) = [-10.0, 0.0556]
  cd(0:1, 84  ) = [-9.3939, 0.0456]
  cd(0:1, 85  ) = [-8.7879, 0.0348]
  cd(0:1, 86  ) = [-8.1818, 0.0222]
  cd(0:1, 87  ) = [-7.5758, 0.0167]
  cd(0:1, 88  ) = [-6.9697, 0.0138]
  cd(0:1, 89  ) = [-6.3636, 0.0128]
  cd(0:1, 90  ) = [-5.7576, 0.0119]
  cd(0:1, 91  ) = [-5.1515, 0.0111]
  cd(0:1, 92  ) = [-4.5455, 0.0106]
  cd(0:1, 93  ) = [-3.9394, 0.0102]
  cd(0:1, 94  ) = [-3.3333, 0.0099]
  cd(0:1, 95  ) = [-2.7273, 0.0097]
  cd(0:1, 96  ) = [-2.1212, 0.0096]
  cd(0:1, 97  ) = [-1.5152, 0.0096]
  cd(0:1, 98  ) = [-0.9091, 0.0095]
  cd(0:1, 99  ) = [-0.303, 0.0096]
  cd(0:1, 100  ) = [0.303, 0.0097]
  cd(0:1, 101  ) = [0.9091, 0.0097]
  cd(0:1, 102  ) = [1.5152, 0.0097]
  cd(0:1, 103  ) = [2.1212, 0.0098]
  cd(0:1, 104  ) = [2.7273, 0.01]
  cd(0:1, 105  ) = [3.3333, 0.0101]
  cd(0:1, 106  ) = [3.9394, 0.0102]
  cd(0:1, 107  ) = [4.5455, 0.0104]
  cd(0:1, 108  ) = [5.1515, 0.0105]
  cd(0:1, 109  ) = [5.7576, 0.0108]
  cd(0:1, 110  ) = [6.3636, 0.011]
  cd(0:1, 111  ) = [6.9697, 0.0113]
  cd(0:1, 112  ) = [7.5758, 0.0117]
  cd(0:1, 113  ) = [8.1818, 0.0124]
  cd(0:1, 114  ) = [8.7879, 0.0138]
  cd(0:1, 115  ) = [9.3939, 0.0176]
  cd(0:1, 116  ) = [10.0, 0.0229]
  cd(0:1, 117  ) = [10.6061, 0.0277]
  cd(0:1, 118  ) = [11.2121, 0.032]
  cd(0:1, 119  ) = [11.8182, 0.035]
  cd(0:1, 120  ) = [12.4242, 0.0485]
  cd(0:1, 121  ) = [13.0303, 0.0662]
  cd(0:1, 122  ) = [13.6364, 0.0758]
  cd(0:1, 123  ) = [14.2424, 0.0853]
  cd(0:1, 124  ) = [14.8485, 0.0947]
  cd(0:1, 125  ) = [15.4545, 0.1034]
  cd(0:1, 126  ) = [16.0606, 0.1119]
  cd(0:1, 127  ) = [16.6667, 0.1201]
  cd(0:1, 128  ) = [17.2727, 0.1291]
  cd(0:1, 129  ) = [17.8788, 0.1391]
  cd(0:1, 130  ) = [18.4848, 0.151]
  cd(0:1, 131  ) = [19.0909, 0.1633]
  cd(0:1, 132  ) = [19.697, 0.1755]
  cd(0:1, 133  ) = [20.303, 0.188]
  cd(0:1, 134  ) = [20.9091, 0.2009]
  cd(0:1, 135  ) = [21.5152, 0.2138]
  cd(0:1, 136  ) = [22.1212, 0.2263]
  cd(0:1, 137  ) = [22.7273, 0.2375]
  cd(0:1, 138  ) = [23.3333, 0.2486]
  cd(0:1, 139  ) = [23.9394, 0.2598]
  cd(0:1, 140  ) = [24.5455, 0.2714]
  cd(0:1, 141  ) = [25.1515, 0.2831]
  cd(0:1, 142  ) = [25.7576, 0.2948]
  cd(0:1, 143  ) = [26.3636, 0.3068]
  cd(0:1, 144  ) = [26.9697, 0.3192]
  cd(0:1, 145  ) = [27.5758, 0.3315]
  cd(0:1, 146  ) = [28.1818, 0.344]
  cd(0:1, 147  ) = [28.7879, 0.3569]
  cd(0:1, 148  ) = [29.3939, 0.3698]
  cd(0:1, 149  ) = [30.0, 0.3827]
  cd(0:1, 150  ) = [33.0, 0.45]
  cd(0:1, 151  ) = [36.0, 0.5202]
  cd(0:1, 152  ) = [39.0, 0.593]
  cd(0:1, 153  ) = [42.0, 0.6675]
  cd(0:1, 154  ) = [45.0, 0.7424]
  cd(0:1, 155  ) = [48.0, 0.8172]
  cd(0:1, 156  ) = [51.0, 0.8905]
  cd(0:1, 157  ) = [54.0, 0.9607]
  cd(0:1, 158  ) = [57.0, 1.0309]
  cd(0:1, 159  ) = [60.0, 1.101]
  cd(0:1, 160  ) = [63.0, 1.158]
  cd(0:1, 161  ) = [66.0, 1.2149]
  cd(0:1, 162  ) = [69.0, 1.2718]
  cd(0:1, 163  ) = [72.0, 1.3152]
  cd(0:1, 164  ) = [75.0, 1.352]
  cd(0:1, 165  ) = [78.0, 1.3887]
  cd(0:1, 166  ) = [81.0, 1.4171]
  cd(0:1, 167  ) = [84.0, 1.4291]
  cd(0:1, 168  ) = [87.0, 1.4411]
  cd(0:1, 169  ) = [90.0, 1.4531]
  cd(0:1, 170  ) = [93.0, 1.4411]
  cd(0:1, 171  ) = [96.0, 1.4291]
  cd(0:1, 172  ) = [99.0, 1.4171]
  cd(0:1, 173  ) = [102.0, 1.3887]
  cd(0:1, 174  ) = [105.0, 1.352]
  cd(0:1, 175  ) = [108.0, 1.3152]
  cd(0:1, 176  ) = [111.0, 1.2718]
  cd(0:1, 177  ) = [114.0, 1.2149]
  cd(0:1, 178  ) = [117.0, 1.158]
  cd(0:1, 179  ) = [120.0, 1.101]
  cd(0:1, 180  ) = [123.0, 1.0309]
  cd(0:1, 181  ) = [126.0, 0.9607]
  cd(0:1, 182  ) = [129.0, 0.8905]
  cd(0:1, 183  ) = [132.0, 0.8172]
  cd(0:1, 184  ) = [135.0, 0.7424]
  cd(0:1, 185  ) = [138.0, 0.6675]
  cd(0:1, 186  ) = [141.0, 0.5941]
  cd(0:1, 187  ) = [144.0, 0.5236]
  cd(0:1, 188  ) = [147.0, 0.4532]
  cd(0:1, 189  ) = [150.0, 0.3827]
  cd(0:1, 190  ) = [153.0, 0.3298]
  cd(0:1, 191  ) = [156.0, 0.2769]
  cd(0:1, 192  ) = [159.0, 0.224]
  cd(0:1, 193  ) = [162.0, 0.1857]
  cd(0:1, 194  ) = [165.0, 0.1548]
  cd(0:1, 195  ) = [168.0, 0.1238]
  cd(0:1, 196  ) = [171.0, 0.0929]
  cd(0:1, 197  ) = [174.0, 0.0619]
  cd(0:1, 198  ) = [177.0, 0.051]
  cd(0:1, 199  ) = [180.0, 0.0502]

  IF ( y .EQ. cd(0,0) ) cd18 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd18 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd18 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd18_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd18 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd18_nrel2p8.pro' 
  ENDIF

!print,y,cd18

END FUNCTION calc_cd18_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd19_nrel2p8( y ) RESULT( cd19 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd19
INTEGER :: i

 cd19 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.0536]
  cd(0:1, 1  ) = [-177.0, 0.0536]
  cd(0:1, 2  ) = [-174.0, 0.0643]
  cd(0:1, 3  ) = [-171.0, 0.0965]
  cd(0:1, 4  ) = [-168.0, 0.1287]
  cd(0:1, 5  ) = [-165.0, 0.1608]
  cd(0:1, 6  ) = [-162.0, 0.193]
  cd(0:1, 7  ) = [-159.0, 0.2324]
  cd(0:1, 8  ) = [-156.0, 0.2863]
  cd(0:1, 9  ) = [-153.0, 0.3402]
  cd(0:1, 10  ) = [-150.0, 0.3941]
  cd(0:1, 11  ) = [-147.0, 0.4642]
  cd(0:1, 12  ) = [-144.0, 0.5343]
  cd(0:1, 13  ) = [-141.0, 0.6043]
  cd(0:1, 14  ) = [-138.0, 0.6773]
  cd(0:1, 15  ) = [-135.0, 0.7516]
  cd(0:1, 16  ) = [-132.0, 0.826]
  cd(0:1, 17  ) = [-129.0, 0.8988]
  cd(0:1, 18  ) = [-126.0, 0.9684]
  cd(0:1, 19  ) = [-123.0, 1.038]
  cd(0:1, 20  ) = [-120.0, 1.1076]
  cd(0:1, 21  ) = [-117.0, 1.1639]
  cd(0:1, 22  ) = [-114.0, 1.2202]
  cd(0:1, 23  ) = [-111.0, 1.2765]
  cd(0:1, 24  ) = [-108.0, 1.3193]
  cd(0:1, 25  ) = [-105.0, 1.3554]
  cd(0:1, 26  ) = [-102.0, 1.3914]
  cd(0:1, 27  ) = [-99.0, 1.4192]
  cd(0:1, 28  ) = [-96.0, 1.4305]
  cd(0:1, 29  ) = [-93.0, 1.4418]
  cd(0:1, 30  ) = [-90.0, 1.4531]
  cd(0:1, 31  ) = [-87.0, 1.4418]
  cd(0:1, 32  ) = [-84.0, 1.4305]
  cd(0:1, 33  ) = [-81.0, 1.4192]
  cd(0:1, 34  ) = [-78.0, 1.3914]
  cd(0:1, 35  ) = [-75.0, 1.3554]
  cd(0:1, 36  ) = [-72.0, 1.3193]
  cd(0:1, 37  ) = [-69.0, 1.2765]
  cd(0:1, 38  ) = [-66.0, 1.2202]
  cd(0:1, 39  ) = [-63.0, 1.1639]
  cd(0:1, 40  ) = [-60.0, 1.1076]
  cd(0:1, 41  ) = [-57.0, 1.038]
  cd(0:1, 42  ) = [-54.0, 0.9684]
  cd(0:1, 43  ) = [-51.0, 0.8988]
  cd(0:1, 44  ) = [-48.0, 0.8261]
  cd(0:1, 45  ) = [-45.0, 0.7517]
  cd(0:1, 46  ) = [-42.0, 0.6773]
  cd(0:1, 47  ) = [-39.0, 0.6033]
  cd(0:1, 48  ) = [-36.0, 0.5309]
  cd(0:1, 49  ) = [-33.0, 0.4611]
  cd(0:1, 50  ) = [-30.0, 0.3941]
  cd(0:1, 51  ) = [-29.3939, 0.3813]
  cd(0:1, 52  ) = [-28.7879, 0.3684]
  cd(0:1, 53  ) = [-28.1818, 0.3556]
  cd(0:1, 54  ) = [-27.5758, 0.3431]
  cd(0:1, 55  ) = [-26.9697, 0.3309]
  cd(0:1, 56  ) = [-26.3636, 0.3186]
  cd(0:1, 57  ) = [-25.7576, 0.3066]
  cd(0:1, 58  ) = [-25.1515, 0.295]
  cd(0:1, 59  ) = [-24.5455, 0.2834]
  cd(0:1, 60  ) = [-23.9394, 0.2718]
  cd(0:1, 61  ) = [-23.3333, 0.2609]
  cd(0:1, 62  ) = [-22.7273, 0.2499]
  cd(0:1, 63  ) = [-22.1212, 0.239]
  cd(0:1, 64  ) = [-21.5152, 0.2295]
  cd(0:1, 65  ) = [-20.9091, 0.2203]
  cd(0:1, 66  ) = [-20.303, 0.2112]
  cd(0:1, 67  ) = [-19.697, 0.2021]
  cd(0:1, 68  ) = [-19.0909, 0.193]
  cd(0:1, 69  ) = [-18.4848, 0.1842]
  cd(0:1, 70  ) = [-17.8788, 0.1763]
  cd(0:1, 71  ) = [-17.2727, 0.1686]
  cd(0:1, 72  ) = [-16.6667, 0.1603]
  cd(0:1, 73  ) = [-16.0606, 0.1516]
  cd(0:1, 74  ) = [-15.4545, 0.1419]
  cd(0:1, 75  ) = [-14.8485, 0.1317]
  cd(0:1, 76  ) = [-14.2424, 0.1203]
  cd(0:1, 77  ) = [-13.6364, 0.1088]
  cd(0:1, 78  ) = [-13.0303, 0.0972]
  cd(0:1, 79  ) = [-12.4242, 0.0836]
  cd(0:1, 80  ) = [-11.8182, 0.0703]
  cd(0:1, 81  ) = [-11.2121, 0.0593]
  cd(0:1, 82  ) = [-10.6061, 0.0502]
  cd(0:1, 83  ) = [-10.0, 0.042]
  cd(0:1, 84  ) = [-9.3939, 0.0339]
  cd(0:1, 85  ) = [-8.7879, 0.0259]
  cd(0:1, 86  ) = [-8.1818, 0.0177]
  cd(0:1, 87  ) = [-7.5758, 0.0141]
  cd(0:1, 88  ) = [-6.9697, 0.0121]
  cd(0:1, 89  ) = [-6.3636, 0.0113]
  cd(0:1, 90  ) = [-5.7576, 0.0104]
  cd(0:1, 91  ) = [-5.1515, 0.0099]
  cd(0:1, 92  ) = [-4.5455, 0.0096]
  cd(0:1, 93  ) = [-3.9394, 0.0093]
  cd(0:1, 94  ) = [-3.3333, 0.009]
  cd(0:1, 95  ) = [-2.7273, 0.0088]
  cd(0:1, 96  ) = [-2.1212, 0.0087]
  cd(0:1, 97  ) = [-1.5152, 0.0086]
  cd(0:1, 98  ) = [-0.9091, 0.0086]
  cd(0:1, 99  ) = [-0.303, 0.0087]
  cd(0:1, 100  ) = [0.303, 0.0087]
  cd(0:1, 101  ) = [0.9091, 0.0088]
  cd(0:1, 102  ) = [1.5152, 0.0088]
  cd(0:1, 103  ) = [2.1212, 0.0089]
  cd(0:1, 104  ) = [2.7273, 0.0091]
  cd(0:1, 105  ) = [3.3333, 0.0092]
  cd(0:1, 106  ) = [3.9394, 0.0094]
  cd(0:1, 107  ) = [4.5455, 0.0095]
  cd(0:1, 108  ) = [5.1515, 0.0097]
  cd(0:1, 109  ) = [5.7576, 0.01]
  cd(0:1, 110  ) = [6.3636, 0.0103]
  cd(0:1, 111  ) = [6.9697, 0.0106]
  cd(0:1, 112  ) = [7.5758, 0.0111]
  cd(0:1, 113  ) = [8.1818, 0.0121]
  cd(0:1, 114  ) = [8.7879, 0.0139]
  cd(0:1, 115  ) = [9.3939, 0.0207]
  cd(0:1, 116  ) = [10.0, 0.0304]
  cd(0:1, 117  ) = [10.6061, 0.0389]
  cd(0:1, 118  ) = [11.2121, 0.0459]
  cd(0:1, 119  ) = [11.8182, 0.05]
  cd(0:1, 120  ) = [12.4242, 0.06]
  cd(0:1, 121  ) = [13.0303, 0.0724]
  cd(0:1, 122  ) = [13.6364, 0.0823]
  cd(0:1, 123  ) = [14.2424, 0.0922]
  cd(0:1, 124  ) = [14.8485, 0.1024]
  cd(0:1, 125  ) = [15.4545, 0.1114]
  cd(0:1, 126  ) = [16.0606, 0.1199]
  cd(0:1, 127  ) = [16.6667, 0.1276]
  cd(0:1, 128  ) = [17.2727, 0.1365]
  cd(0:1, 129  ) = [17.8788, 0.147]
  cd(0:1, 130  ) = [18.4848, 0.1591]
  cd(0:1, 131  ) = [19.0909, 0.1716]
  cd(0:1, 132  ) = [19.697, 0.1835]
  cd(0:1, 133  ) = [20.303, 0.1965]
  cd(0:1, 134  ) = [20.9091, 0.2105]
  cd(0:1, 135  ) = [21.5152, 0.2245]
  cd(0:1, 136  ) = [22.1212, 0.2379]
  cd(0:1, 137  ) = [22.7273, 0.2492]
  cd(0:1, 138  ) = [23.3333, 0.2605]
  cd(0:1, 139  ) = [23.9394, 0.2718]
  cd(0:1, 140  ) = [24.5455, 0.2834]
  cd(0:1, 141  ) = [25.1515, 0.295]
  cd(0:1, 142  ) = [25.7576, 0.3066]
  cd(0:1, 143  ) = [26.3636, 0.3186]
  cd(0:1, 144  ) = [26.9697, 0.3309]
  cd(0:1, 145  ) = [27.5758, 0.3431]
  cd(0:1, 146  ) = [28.1818, 0.3556]
  cd(0:1, 147  ) = [28.7879, 0.3684]
  cd(0:1, 148  ) = [29.3939, 0.3813]
  cd(0:1, 149  ) = [30.0, 0.3941]
  cd(0:1, 150  ) = [33.0, 0.4611]
  cd(0:1, 151  ) = [36.0, 0.5309]
  cd(0:1, 152  ) = [39.0, 0.6033]
  cd(0:1, 153  ) = [42.0, 0.6773]
  cd(0:1, 154  ) = [45.0, 0.7517]
  cd(0:1, 155  ) = [48.0, 0.8261]
  cd(0:1, 156  ) = [51.0, 0.8988]
  cd(0:1, 157  ) = [54.0, 0.9684]
  cd(0:1, 158  ) = [57.0, 1.038]
  cd(0:1, 159  ) = [60.0, 1.1076]
  cd(0:1, 160  ) = [63.0, 1.1639]
  cd(0:1, 161  ) = [66.0, 1.2202]
  cd(0:1, 162  ) = [69.0, 1.2765]
  cd(0:1, 163  ) = [72.0, 1.3193]
  cd(0:1, 164  ) = [75.0, 1.3554]
  cd(0:1, 165  ) = [78.0, 1.3914]
  cd(0:1, 166  ) = [81.0, 1.4192]
  cd(0:1, 167  ) = [84.0, 1.4305]
  cd(0:1, 168  ) = [87.0, 1.4418]
  cd(0:1, 169  ) = [90.0, 1.4531]
  cd(0:1, 170  ) = [93.0, 1.4418]
  cd(0:1, 171  ) = [96.0, 1.4305]
  cd(0:1, 172  ) = [99.0, 1.4192]
  cd(0:1, 173  ) = [102.0, 1.3914]
  cd(0:1, 174  ) = [105.0, 1.3554]
  cd(0:1, 175  ) = [108.0, 1.3193]
  cd(0:1, 176  ) = [111.0, 1.2765]
  cd(0:1, 177  ) = [114.0, 1.2202]
  cd(0:1, 178  ) = [117.0, 1.1639]
  cd(0:1, 179  ) = [120.0, 1.1076]
  cd(0:1, 180  ) = [123.0, 1.038]
  cd(0:1, 181  ) = [126.0, 0.9684]
  cd(0:1, 182  ) = [129.0, 0.8988]
  cd(0:1, 183  ) = [132.0, 0.826]
  cd(0:1, 184  ) = [135.0, 0.7516]
  cd(0:1, 185  ) = [138.0, 0.6773]
  cd(0:1, 186  ) = [141.0, 0.6043]
  cd(0:1, 187  ) = [144.0, 0.5343]
  cd(0:1, 188  ) = [147.0, 0.4642]
  cd(0:1, 189  ) = [150.0, 0.3941]
  cd(0:1, 190  ) = [153.0, 0.3417]
  cd(0:1, 191  ) = [156.0, 0.2893]
  cd(0:1, 192  ) = [159.0, 0.2369]
  cd(0:1, 193  ) = [162.0, 0.1975]
  cd(0:1, 194  ) = [165.0, 0.1646]
  cd(0:1, 195  ) = [168.0, 0.1317]
  cd(0:1, 196  ) = [171.0, 0.0988]
  cd(0:1, 197  ) = [174.0, 0.0658]
  cd(0:1, 198  ) = [177.0, 0.0544]
  cd(0:1, 199  ) = [180.0, 0.0536]

  IF ( y .EQ. cd(0,0) ) cd19 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd19 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd19 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd19_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd19 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd19_nrel2p8.pro' 
  ENDIF

!print,y,cd19

END FUNCTION calc_cd19_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd20_nrel2p8( y ) RESULT( cd20 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd20
INTEGER :: i

 cd20 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.0558]
  cd(0:1, 1  ) = [-177.0, 0.0558]
  cd(0:1, 2  ) = [-174.0, 0.067]
  cd(0:1, 3  ) = [-171.0, 0.1005]
  cd(0:1, 4  ) = [-168.0, 0.134]
  cd(0:1, 5  ) = [-165.0, 0.1674]
  cd(0:1, 6  ) = [-162.0, 0.2009]
  cd(0:1, 7  ) = [-159.0, 0.2411]
  cd(0:1, 8  ) = [-156.0, 0.2946]
  cd(0:1, 9  ) = [-153.0, 0.3481]
  cd(0:1, 10  ) = [-150.0, 0.4016]
  cd(0:1, 11  ) = [-147.0, 0.4714]
  cd(0:1, 12  ) = [-144.0, 0.5412]
  cd(0:1, 13  ) = [-141.0, 0.611]
  cd(0:1, 14  ) = [-138.0, 0.6836]
  cd(0:1, 15  ) = [-135.0, 0.7577]
  cd(0:1, 16  ) = [-132.0, 0.8317]
  cd(0:1, 17  ) = [-129.0, 0.9042]
  cd(0:1, 18  ) = [-126.0, 0.9734]
  cd(0:1, 19  ) = [-123.0, 1.0427]
  cd(0:1, 20  ) = [-120.0, 1.1119]
  cd(0:1, 21  ) = [-117.0, 1.1678]
  cd(0:1, 22  ) = [-114.0, 1.2237]
  cd(0:1, 23  ) = [-111.0, 1.2796]
  cd(0:1, 24  ) = [-108.0, 1.322]
  cd(0:1, 25  ) = [-105.0, 1.3576]
  cd(0:1, 26  ) = [-102.0, 1.3932]
  cd(0:1, 27  ) = [-99.0, 1.4205]
  cd(0:1, 28  ) = [-96.0, 1.4314]
  cd(0:1, 29  ) = [-93.0, 1.4422]
  cd(0:1, 30  ) = [-90.0, 1.4531]
  cd(0:1, 31  ) = [-87.0, 1.4422]
  cd(0:1, 32  ) = [-84.0, 1.4314]
  cd(0:1, 33  ) = [-81.0, 1.4205]
  cd(0:1, 34  ) = [-78.0, 1.3932]
  cd(0:1, 35  ) = [-75.0, 1.3576]
  cd(0:1, 36  ) = [-72.0, 1.322]
  cd(0:1, 37  ) = [-69.0, 1.2796]
  cd(0:1, 38  ) = [-66.0, 1.2237]
  cd(0:1, 39  ) = [-63.0, 1.1678]
  cd(0:1, 40  ) = [-60.0, 1.1119]
  cd(0:1, 41  ) = [-57.0, 1.0427]
  cd(0:1, 42  ) = [-54.0, 0.9734]
  cd(0:1, 43  ) = [-51.0, 0.9042]
  cd(0:1, 44  ) = [-48.0, 0.8318]
  cd(0:1, 45  ) = [-45.0, 0.7578]
  cd(0:1, 46  ) = [-42.0, 0.6837]
  cd(0:1, 47  ) = [-39.0, 0.6099]
  cd(0:1, 48  ) = [-36.0, 0.5378]
  cd(0:1, 49  ) = [-33.0, 0.4683]
  cd(0:1, 50  ) = [-30.0, 0.4016]
  cd(0:1, 51  ) = [-29.3939, 0.3888]
  cd(0:1, 52  ) = [-28.7879, 0.376]
  cd(0:1, 53  ) = [-28.1818, 0.3631]
  cd(0:1, 54  ) = [-27.5758, 0.3508]
  cd(0:1, 55  ) = [-26.9697, 0.3385]
  cd(0:1, 56  ) = [-26.3636, 0.3263]
  cd(0:1, 57  ) = [-25.7576, 0.3144]
  cd(0:1, 58  ) = [-25.1515, 0.3028]
  cd(0:1, 59  ) = [-24.5455, 0.2912]
  cd(0:1, 60  ) = [-23.9394, 0.2797]
  cd(0:1, 61  ) = [-23.3333, 0.2688]
  cd(0:1, 62  ) = [-22.7273, 0.2578]
  cd(0:1, 63  ) = [-22.1212, 0.2469]
  cd(0:1, 64  ) = [-21.5152, 0.2362]
  cd(0:1, 65  ) = [-20.9091, 0.2256]
  cd(0:1, 66  ) = [-20.303, 0.2151]
  cd(0:1, 67  ) = [-19.697, 0.2045]
  cd(0:1, 68  ) = [-19.0909, 0.1939]
  cd(0:1, 69  ) = [-18.4848, 0.1837]
  cd(0:1, 70  ) = [-17.8788, 0.174]
  cd(0:1, 71  ) = [-17.2727, 0.1642]
  cd(0:1, 72  ) = [-16.6667, 0.1541]
  cd(0:1, 73  ) = [-16.0606, 0.1438]
  cd(0:1, 74  ) = [-15.4545, 0.1331]
  cd(0:1, 75  ) = [-14.8485, 0.1223]
  cd(0:1, 76  ) = [-14.2424, 0.1112]
  cd(0:1, 77  ) = [-13.6364, 0.1]
  cd(0:1, 78  ) = [-13.0303, 0.0888]
  cd(0:1, 79  ) = [-12.4242, 0.0753]
  cd(0:1, 80  ) = [-11.8182, 0.062]
  cd(0:1, 81  ) = [-11.2121, 0.0507]
  cd(0:1, 82  ) = [-10.6061, 0.0414]
  cd(0:1, 83  ) = [-10.0, 0.0331]
  cd(0:1, 84  ) = [-9.3939, 0.0263]
  cd(0:1, 85  ) = [-8.7879, 0.02]
  cd(0:1, 86  ) = [-8.1818, 0.0148]
  cd(0:1, 87  ) = [-7.5758, 0.0123]
  cd(0:1, 88  ) = [-6.9697, 0.0109]
  cd(0:1, 89  ) = [-6.3636, 0.0101]
  cd(0:1, 90  ) = [-5.7576, 0.0093]
  cd(0:1, 91  ) = [-5.1515, 0.0089]
  cd(0:1, 92  ) = [-4.5455, 0.0087]
  cd(0:1, 93  ) = [-3.9394, 0.0085]
  cd(0:1, 94  ) = [-3.3333, 0.0083]
  cd(0:1, 95  ) = [-2.7273, 0.0081]
  cd(0:1, 96  ) = [-2.1212, 0.0079]
  cd(0:1, 97  ) = [-1.5152, 0.0079]
  cd(0:1, 98  ) = [-0.9091, 0.0078]
  cd(0:1, 99  ) = [-0.303, 0.0079]
  cd(0:1, 100  ) = [0.303, 0.008]
  cd(0:1, 101  ) = [0.9091, 0.008]
  cd(0:1, 102  ) = [1.5152, 0.008]
  cd(0:1, 103  ) = [2.1212, 0.0081]
  cd(0:1, 104  ) = [2.7273, 0.0083]
  cd(0:1, 105  ) = [3.3333, 0.0085]
  cd(0:1, 106  ) = [3.9394, 0.0086]
  cd(0:1, 107  ) = [4.5455, 0.0088]
  cd(0:1, 108  ) = [5.1515, 0.009]
  cd(0:1, 109  ) = [5.7576, 0.0093]
  cd(0:1, 110  ) = [6.3636, 0.0097]
  cd(0:1, 111  ) = [6.9697, 0.01]
  cd(0:1, 112  ) = [7.5758, 0.0107]
  cd(0:1, 113  ) = [8.1818, 0.0119]
  cd(0:1, 114  ) = [8.7879, 0.0139]
  cd(0:1, 115  ) = [9.3939, 0.0228]
  cd(0:1, 116  ) = [10.0, 0.0353]
  cd(0:1, 117  ) = [10.6061, 0.0462]
  cd(0:1, 118  ) = [11.2121, 0.055]
  cd(0:1, 119  ) = [11.8182, 0.0598]
  cd(0:1, 120  ) = [12.4242, 0.0675]
  cd(0:1, 121  ) = [13.0303, 0.0765]
  cd(0:1, 122  ) = [13.6364, 0.0865]
  cd(0:1, 123  ) = [14.2424, 0.0968]
  cd(0:1, 124  ) = [14.8485, 0.1074]
  cd(0:1, 125  ) = [15.4545, 0.1166]
  cd(0:1, 126  ) = [16.0606, 0.1251]
  cd(0:1, 127  ) = [16.6667, 0.1324]
  cd(0:1, 128  ) = [17.2727, 0.1413]
  cd(0:1, 129  ) = [17.8788, 0.1521]
  cd(0:1, 130  ) = [18.4848, 0.1645]
  cd(0:1, 131  ) = [19.0909, 0.1771]
  cd(0:1, 132  ) = [19.697, 0.1888]
  cd(0:1, 133  ) = [20.303, 0.202]
  cd(0:1, 134  ) = [20.9091, 0.2167]
  cd(0:1, 135  ) = [21.5152, 0.2314]
  cd(0:1, 136  ) = [22.1212, 0.2455]
  cd(0:1, 137  ) = [22.7273, 0.2569]
  cd(0:1, 138  ) = [23.3333, 0.2683]
  cd(0:1, 139  ) = [23.9394, 0.2796]
  cd(0:1, 140  ) = [24.5455, 0.2912]
  cd(0:1, 141  ) = [25.1515, 0.3028]
  cd(0:1, 142  ) = [25.7576, 0.3144]
  cd(0:1, 143  ) = [26.3636, 0.3263]
  cd(0:1, 144  ) = [26.9697, 0.3385]
  cd(0:1, 145  ) = [27.5758, 0.3508]
  cd(0:1, 146  ) = [28.1818, 0.3631]
  cd(0:1, 147  ) = [28.7879, 0.376]
  cd(0:1, 148  ) = [29.3939, 0.3888]
  cd(0:1, 149  ) = [30.0, 0.4016]
  cd(0:1, 150  ) = [33.0, 0.4683]
  cd(0:1, 151  ) = [36.0, 0.5378]
  cd(0:1, 152  ) = [39.0, 0.6099]
  cd(0:1, 153  ) = [42.0, 0.6837]
  cd(0:1, 154  ) = [45.0, 0.7578]
  cd(0:1, 155  ) = [48.0, 0.8318]
  cd(0:1, 156  ) = [51.0, 0.9042]
  cd(0:1, 157  ) = [54.0, 0.9734]
  cd(0:1, 158  ) = [57.0, 1.0427]
  cd(0:1, 159  ) = [60.0, 1.1119]
  cd(0:1, 160  ) = [63.0, 1.1678]
  cd(0:1, 161  ) = [66.0, 1.2237]
  cd(0:1, 162  ) = [69.0, 1.2796]
  cd(0:1, 163  ) = [72.0, 1.322]
  cd(0:1, 164  ) = [75.0, 1.3576]
  cd(0:1, 165  ) = [78.0, 1.3932]
  cd(0:1, 166  ) = [81.0, 1.4205]
  cd(0:1, 167  ) = [84.0, 1.4314]
  cd(0:1, 168  ) = [87.0, 1.4422]
  cd(0:1, 169  ) = [90.0, 1.4531]
  cd(0:1, 170  ) = [93.0, 1.4422]
  cd(0:1, 171  ) = [96.0, 1.4314]
  cd(0:1, 172  ) = [99.0, 1.4205]
  cd(0:1, 173  ) = [102.0, 1.3932]
  cd(0:1, 174  ) = [105.0, 1.3576]
  cd(0:1, 175  ) = [108.0, 1.322]
  cd(0:1, 176  ) = [111.0, 1.2796]
  cd(0:1, 177  ) = [114.0, 1.2237]
  cd(0:1, 178  ) = [117.0, 1.1678]
  cd(0:1, 179  ) = [120.0, 1.1119]
  cd(0:1, 180  ) = [123.0, 1.0427]
  cd(0:1, 181  ) = [126.0, 0.9734]
  cd(0:1, 182  ) = [129.0, 0.9042]
  cd(0:1, 183  ) = [132.0, 0.8317]
  cd(0:1, 184  ) = [135.0, 0.7577]
  cd(0:1, 185  ) = [138.0, 0.6836]
  cd(0:1, 186  ) = [141.0, 0.611]
  cd(0:1, 187  ) = [144.0, 0.5412]
  cd(0:1, 188  ) = [147.0, 0.4714]
  cd(0:1, 189  ) = [150.0, 0.4016]
  cd(0:1, 190  ) = [153.0, 0.3495]
  cd(0:1, 191  ) = [156.0, 0.2974]
  cd(0:1, 192  ) = [159.0, 0.2454]
  cd(0:1, 193  ) = [162.0, 0.2052]
  cd(0:1, 194  ) = [165.0, 0.171]
  cd(0:1, 195  ) = [168.0, 0.1368]
  cd(0:1, 196  ) = [171.0, 0.1026]
  cd(0:1, 197  ) = [174.0, 0.0684]
  cd(0:1, 198  ) = [177.0, 0.0565]
  cd(0:1, 199  ) = [180.0, 0.0558]

  IF ( y .EQ. cd(0,0) ) cd20 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd20 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd20 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd20_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd20 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd20_nrel2p8.pro' 
  ENDIF

!print,y,cd20

END FUNCTION calc_cd20_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd21_nrel2p8( y ) RESULT( cd21 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd21
INTEGER :: i

 cd21 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.0561]
  cd(0:1, 1  ) = [-177.0, 0.0561]
  cd(0:1, 2  ) = [-174.0, 0.0673]
  cd(0:1, 3  ) = [-171.0, 0.101]
  cd(0:1, 4  ) = [-168.0, 0.1346]
  cd(0:1, 5  ) = [-165.0, 0.1683]
  cd(0:1, 6  ) = [-162.0, 0.202]
  cd(0:1, 7  ) = [-159.0, 0.2423]
  cd(0:1, 8  ) = [-156.0, 0.2958]
  cd(0:1, 9  ) = [-153.0, 0.3492]
  cd(0:1, 10  ) = [-150.0, 0.4027]
  cd(0:1, 11  ) = [-147.0, 0.4724]
  cd(0:1, 12  ) = [-144.0, 0.5422]
  cd(0:1, 13  ) = [-141.0, 0.612]
  cd(0:1, 14  ) = [-138.0, 0.6846]
  cd(0:1, 15  ) = [-135.0, 0.7586]
  cd(0:1, 16  ) = [-132.0, 0.8326]
  cd(0:1, 17  ) = [-129.0, 0.905]
  cd(0:1, 18  ) = [-126.0, 0.9742]
  cd(0:1, 19  ) = [-123.0, 1.0434]
  cd(0:1, 20  ) = [-120.0, 1.1125]
  cd(0:1, 21  ) = [-117.0, 1.1684]
  cd(0:1, 22  ) = [-114.0, 1.2242]
  cd(0:1, 23  ) = [-111.0, 1.28]
  cd(0:1, 24  ) = [-108.0, 1.3223]
  cd(0:1, 25  ) = [-105.0, 1.3579]
  cd(0:1, 26  ) = [-102.0, 1.3934]
  cd(0:1, 27  ) = [-99.0, 1.4207]
  cd(0:1, 28  ) = [-96.0, 1.4315]
  cd(0:1, 29  ) = [-93.0, 1.4423]
  cd(0:1, 30  ) = [-90.0, 1.4531]
  cd(0:1, 31  ) = [-87.0, 1.4423]
  cd(0:1, 32  ) = [-84.0, 1.4315]
  cd(0:1, 33  ) = [-81.0, 1.4207]
  cd(0:1, 34  ) = [-78.0, 1.3934]
  cd(0:1, 35  ) = [-75.0, 1.3579]
  cd(0:1, 36  ) = [-72.0, 1.3223]
  cd(0:1, 37  ) = [-69.0, 1.28]
  cd(0:1, 38  ) = [-66.0, 1.2242]
  cd(0:1, 39  ) = [-63.0, 1.1684]
  cd(0:1, 40  ) = [-60.0, 1.1125]
  cd(0:1, 41  ) = [-57.0, 1.0434]
  cd(0:1, 42  ) = [-54.0, 0.9742]
  cd(0:1, 43  ) = [-51.0, 0.905]
  cd(0:1, 44  ) = [-48.0, 0.8326]
  cd(0:1, 45  ) = [-45.0, 0.7587]
  cd(0:1, 46  ) = [-42.0, 0.6846]
  cd(0:1, 47  ) = [-39.0, 0.6109]
  cd(0:1, 48  ) = [-36.0, 0.5388]
  cd(0:1, 49  ) = [-33.0, 0.4693]
  cd(0:1, 50  ) = [-30.0, 0.4027]
  cd(0:1, 51  ) = [-29.3939, 0.3899]
  cd(0:1, 52  ) = [-28.7879, 0.3771]
  cd(0:1, 53  ) = [-28.1818, 0.3643]
  cd(0:1, 54  ) = [-27.5758, 0.3519]
  cd(0:1, 55  ) = [-26.9697, 0.3397]
  cd(0:1, 56  ) = [-26.3636, 0.3274]
  cd(0:1, 57  ) = [-25.7576, 0.3155]
  cd(0:1, 58  ) = [-25.1515, 0.3039]
  cd(0:1, 59  ) = [-24.5455, 0.2924]
  cd(0:1, 60  ) = [-23.9394, 0.2808]
  cd(0:1, 61  ) = [-23.3333, 0.2699]
  cd(0:1, 62  ) = [-22.7273, 0.259]
  cd(0:1, 63  ) = [-22.1212, 0.248]
  cd(0:1, 64  ) = [-21.5152, 0.2372]
  cd(0:1, 65  ) = [-20.9091, 0.2263]
  cd(0:1, 66  ) = [-20.303, 0.2155]
  cd(0:1, 67  ) = [-19.697, 0.2047]
  cd(0:1, 68  ) = [-19.0909, 0.1938]
  cd(0:1, 69  ) = [-18.4848, 0.1828]
  cd(0:1, 70  ) = [-17.8788, 0.1715]
  cd(0:1, 71  ) = [-17.2727, 0.1603]
  cd(0:1, 72  ) = [-16.6667, 0.1493]
  cd(0:1, 73  ) = [-16.0606, 0.1384]
  cd(0:1, 74  ) = [-15.4545, 0.1275]
  cd(0:1, 75  ) = [-14.8485, 0.1168]
  cd(0:1, 76  ) = [-14.2424, 0.1061]
  cd(0:1, 77  ) = [-13.6364, 0.0955]
  cd(0:1, 78  ) = [-13.0303, 0.0848]
  cd(0:1, 79  ) = [-12.4242, 0.0723]
  cd(0:1, 80  ) = [-11.8182, 0.0603]
  cd(0:1, 81  ) = [-11.2121, 0.0492]
  cd(0:1, 82  ) = [-10.6061, 0.0399]
  cd(0:1, 83  ) = [-10.0, 0.0316]
  cd(0:1, 84  ) = [-9.3939, 0.025]
  cd(0:1, 85  ) = [-8.7879, 0.019]
  cd(0:1, 86  ) = [-8.1818, 0.0141]
  cd(0:1, 87  ) = [-7.5758, 0.0117]
  cd(0:1, 88  ) = [-6.9697, 0.0103]
  cd(0:1, 89  ) = [-6.3636, 0.0094]
  cd(0:1, 90  ) = [-5.7576, 0.0087]
  cd(0:1, 91  ) = [-5.1515, 0.0084]
  cd(0:1, 92  ) = [-4.5455, 0.0082]
  cd(0:1, 93  ) = [-3.9394, 0.008]
  cd(0:1, 94  ) = [-3.3333, 0.0078]
  cd(0:1, 95  ) = [-2.7273, 0.0076]
  cd(0:1, 96  ) = [-2.1212, 0.0075]
  cd(0:1, 97  ) = [-1.5152, 0.0074]
  cd(0:1, 98  ) = [-0.9091, 0.0074]
  cd(0:1, 99  ) = [-0.303, 0.0075]
  cd(0:1, 100  ) = [0.303, 0.0075]
  cd(0:1, 101  ) = [0.9091, 0.0075]
  cd(0:1, 102  ) = [1.5152, 0.0076]
  cd(0:1, 103  ) = [2.1212, 0.0077]
  cd(0:1, 104  ) = [2.7273, 0.0079]
  cd(0:1, 105  ) = [3.3333, 0.008]
  cd(0:1, 106  ) = [3.9394, 0.0082]
  cd(0:1, 107  ) = [4.5455, 0.0084]
  cd(0:1, 108  ) = [5.1515, 0.0086]
  cd(0:1, 109  ) = [5.7576, 0.0089]
  cd(0:1, 110  ) = [6.3636, 0.0093]
  cd(0:1, 111  ) = [6.9697, 0.0097]
  cd(0:1, 112  ) = [7.5758, 0.0107]
  cd(0:1, 113  ) = [8.1818, 0.0119]
  cd(0:1, 114  ) = [8.7879, 0.014]
  cd(0:1, 115  ) = [9.3939, 0.0231]
  cd(0:1, 116  ) = [10.0, 0.036]
  cd(0:1, 117  ) = [10.6061, 0.0473]
  cd(0:1, 118  ) = [11.2121, 0.0564]
  cd(0:1, 119  ) = [11.8182, 0.0614]
  cd(0:1, 120  ) = [12.4242, 0.0687]
  cd(0:1, 121  ) = [13.0303, 0.0771]
  cd(0:1, 122  ) = [13.6364, 0.0872]
  cd(0:1, 123  ) = [14.2424, 0.0976]
  cd(0:1, 124  ) = [14.8485, 0.1083]
  cd(0:1, 125  ) = [15.4545, 0.1175]
  cd(0:1, 126  ) = [16.0606, 0.126]
  cd(0:1, 127  ) = [16.6667, 0.1333]
  cd(0:1, 128  ) = [17.2727, 0.1421]
  cd(0:1, 129  ) = [17.8788, 0.1529]
  cd(0:1, 130  ) = [18.4848, 0.1653]
  cd(0:1, 131  ) = [19.0909, 0.1779]
  cd(0:1, 132  ) = [19.697, 0.1896]
  cd(0:1, 133  ) = [20.303, 0.2028]
  cd(0:1, 134  ) = [20.9091, 0.2176]
  cd(0:1, 135  ) = [21.5152, 0.2325]
  cd(0:1, 136  ) = [22.1212, 0.2466]
  cd(0:1, 137  ) = [22.7273, 0.258]
  cd(0:1, 138  ) = [23.3333, 0.2694]
  cd(0:1, 139  ) = [23.9394, 0.2808]
  cd(0:1, 140  ) = [24.5455, 0.2924]
  cd(0:1, 141  ) = [25.1515, 0.3039]
  cd(0:1, 142  ) = [25.7576, 0.3155]
  cd(0:1, 143  ) = [26.3636, 0.3274]
  cd(0:1, 144  ) = [26.9697, 0.3397]
  cd(0:1, 145  ) = [27.5758, 0.3519]
  cd(0:1, 146  ) = [28.1818, 0.3643]
  cd(0:1, 147  ) = [28.7879, 0.3771]
  cd(0:1, 148  ) = [29.3939, 0.3899]
  cd(0:1, 149  ) = [30.0, 0.4027]
  cd(0:1, 150  ) = [33.0, 0.4693]
  cd(0:1, 151  ) = [36.0, 0.5388]
  cd(0:1, 152  ) = [39.0, 0.6109]
  cd(0:1, 153  ) = [42.0, 0.6846]
  cd(0:1, 154  ) = [45.0, 0.7587]
  cd(0:1, 155  ) = [48.0, 0.8326]
  cd(0:1, 156  ) = [51.0, 0.905]
  cd(0:1, 157  ) = [54.0, 0.9742]
  cd(0:1, 158  ) = [57.0, 1.0434]
  cd(0:1, 159  ) = [60.0, 1.1125]
  cd(0:1, 160  ) = [63.0, 1.1684]
  cd(0:1, 161  ) = [66.0, 1.2242]
  cd(0:1, 162  ) = [69.0, 1.28]
  cd(0:1, 163  ) = [72.0, 1.3223]
  cd(0:1, 164  ) = [75.0, 1.3579]
  cd(0:1, 165  ) = [78.0, 1.3934]
  cd(0:1, 166  ) = [81.0, 1.4207]
  cd(0:1, 167  ) = [84.0, 1.4315]
  cd(0:1, 168  ) = [87.0, 1.4423]
  cd(0:1, 169  ) = [90.0, 1.4531]
  cd(0:1, 170  ) = [93.0, 1.4423]
  cd(0:1, 171  ) = [96.0, 1.4315]
  cd(0:1, 172  ) = [99.0, 1.4207]
  cd(0:1, 173  ) = [102.0, 1.3934]
  cd(0:1, 174  ) = [105.0, 1.3579]
  cd(0:1, 175  ) = [108.0, 1.3223]
  cd(0:1, 176  ) = [111.0, 1.28]
  cd(0:1, 177  ) = [114.0, 1.2242]
  cd(0:1, 178  ) = [117.0, 1.1684]
  cd(0:1, 179  ) = [120.0, 1.1125]
  cd(0:1, 180  ) = [123.0, 1.0434]
  cd(0:1, 181  ) = [126.0, 0.9742]
  cd(0:1, 182  ) = [129.0, 0.905]
  cd(0:1, 183  ) = [132.0, 0.8326]
  cd(0:1, 184  ) = [135.0, 0.7586]
  cd(0:1, 185  ) = [138.0, 0.6846]
  cd(0:1, 186  ) = [141.0, 0.612]
  cd(0:1, 187  ) = [144.0, 0.5422]
  cd(0:1, 188  ) = [147.0, 0.4724]
  cd(0:1, 189  ) = [150.0, 0.4027]
  cd(0:1, 190  ) = [153.0, 0.3506]
  cd(0:1, 191  ) = [156.0, 0.2986]
  cd(0:1, 192  ) = [159.0, 0.2465]
  cd(0:1, 193  ) = [162.0, 0.2062]
  cd(0:1, 194  ) = [165.0, 0.1718]
  cd(0:1, 195  ) = [168.0, 0.1374]
  cd(0:1, 196  ) = [171.0, 0.1031]
  cd(0:1, 197  ) = [174.0, 0.0687]
  cd(0:1, 198  ) = [177.0, 0.0568]
  cd(0:1, 199  ) = [180.0, 0.0561]

  IF ( y .EQ. cd(0,0) ) cd21 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd21 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd21 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd21_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd21 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd21_nrel2p8.pro' 
  ENDIF

!print,y,cd21

END FUNCTION calc_cd21_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd22_nrel2p8( y ) RESULT( cd22 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd22
INTEGER :: i

 cd22 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.0543]
  cd(0:1, 1  ) = [-177.0, 0.0543]
  cd(0:1, 2  ) = [-174.0, 0.0652]
  cd(0:1, 3  ) = [-171.0, 0.0977]
  cd(0:1, 4  ) = [-168.0, 0.1308]
  cd(0:1, 5  ) = [-165.0, 0.1641]
  cd(0:1, 6  ) = [-162.0, 0.1974]
  cd(0:1, 7  ) = [-159.0, 0.2376]
  cd(0:1, 8  ) = [-156.0, 0.2915]
  cd(0:1, 9  ) = [-153.0, 0.3454]
  cd(0:1, 10  ) = [-150.0, 0.3993]
  cd(0:1, 11  ) = [-147.0, 0.4692]
  cd(0:1, 12  ) = [-144.0, 0.5391]
  cd(0:1, 13  ) = [-141.0, 0.6089]
  cd(0:1, 14  ) = [-138.0, 0.6817]
  cd(0:1, 15  ) = [-135.0, 0.7558]
  cd(0:1, 16  ) = [-132.0, 0.83]
  cd(0:1, 17  ) = [-129.0, 0.9026]
  cd(0:1, 18  ) = [-126.0, 0.9719]
  cd(0:1, 19  ) = [-123.0, 1.0413]
  cd(0:1, 20  ) = [-120.0, 1.1106]
  cd(0:1, 21  ) = [-117.0, 1.1666]
  cd(0:1, 22  ) = [-114.0, 1.2226]
  cd(0:1, 23  ) = [-111.0, 1.2786]
  cd(0:1, 24  ) = [-108.0, 1.3211]
  cd(0:1, 25  ) = [-105.0, 1.3569]
  cd(0:1, 26  ) = [-102.0, 1.3926]
  cd(0:1, 27  ) = [-99.0, 1.4201]
  cd(0:1, 28  ) = [-96.0, 1.4311]
  cd(0:1, 29  ) = [-93.0, 1.4421]
  cd(0:1, 30  ) = [-90.0, 1.4531]
  cd(0:1, 31  ) = [-87.0, 1.4421]
  cd(0:1, 32  ) = [-84.0, 1.4311]
  cd(0:1, 33  ) = [-81.0, 1.4201]
  cd(0:1, 34  ) = [-78.0, 1.3926]
  cd(0:1, 35  ) = [-75.0, 1.3569]
  cd(0:1, 36  ) = [-72.0, 1.3211]
  cd(0:1, 37  ) = [-69.0, 1.2786]
  cd(0:1, 38  ) = [-66.0, 1.2226]
  cd(0:1, 39  ) = [-63.0, 1.1666]
  cd(0:1, 40  ) = [-60.0, 1.1106]
  cd(0:1, 41  ) = [-57.0, 1.0413]
  cd(0:1, 42  ) = [-54.0, 0.9719]
  cd(0:1, 43  ) = [-51.0, 0.9026]
  cd(0:1, 44  ) = [-48.0, 0.83]
  cd(0:1, 45  ) = [-45.0, 0.756]
  cd(0:1, 46  ) = [-42.0, 0.6817]
  cd(0:1, 47  ) = [-39.0, 0.6079]
  cd(0:1, 48  ) = [-36.0, 0.5357]
  cd(0:1, 49  ) = [-33.0, 0.4661]
  cd(0:1, 50  ) = [-30.0, 0.3993]
  cd(0:1, 51  ) = [-29.3939, 0.3865]
  cd(0:1, 52  ) = [-28.7879, 0.3737]
  cd(0:1, 53  ) = [-28.1818, 0.3608]
  cd(0:1, 54  ) = [-27.5758, 0.3484]
  cd(0:1, 55  ) = [-26.9697, 0.3362]
  cd(0:1, 56  ) = [-26.3636, 0.324]
  cd(0:1, 57  ) = [-25.7576, 0.312]
  cd(0:1, 58  ) = [-25.1515, 0.3004]
  cd(0:1, 59  ) = [-24.5455, 0.2888]
  cd(0:1, 60  ) = [-23.9394, 0.2773]
  cd(0:1, 61  ) = [-23.3333, 0.2664]
  cd(0:1, 62  ) = [-22.7273, 0.2554]
  cd(0:1, 63  ) = [-22.1212, 0.2445]
  cd(0:1, 64  ) = [-21.5152, 0.2336]
  cd(0:1, 65  ) = [-20.9091, 0.2227]
  cd(0:1, 66  ) = [-20.303, 0.2118]
  cd(0:1, 67  ) = [-19.697, 0.2009]
  cd(0:1, 68  ) = [-19.0909, 0.1901]
  cd(0:1, 69  ) = [-18.4848, 0.1788]
  cd(0:1, 70  ) = [-17.8788, 0.1669]
  cd(0:1, 71  ) = [-17.2727, 0.1554]
  cd(0:1, 72  ) = [-16.6667, 0.1442]
  cd(0:1, 73  ) = [-16.0606, 0.1333]
  cd(0:1, 74  ) = [-15.4545, 0.1227]
  cd(0:1, 75  ) = [-14.8485, 0.1123]
  cd(0:1, 76  ) = [-14.2424, 0.1021]
  cd(0:1, 77  ) = [-13.6364, 0.092]
  cd(0:1, 78  ) = [-13.0303, 0.0819]
  cd(0:1, 79  ) = [-12.4242, 0.0706]
  cd(0:1, 80  ) = [-11.8182, 0.06]
  cd(0:1, 81  ) = [-11.2121, 0.0493]
  cd(0:1, 82  ) = [-10.6061, 0.04]
  cd(0:1, 83  ) = [-10.0, 0.0317]
  cd(0:1, 84  ) = [-9.3939, 0.025]
  cd(0:1, 85  ) = [-8.7879, 0.019]
  cd(0:1, 86  ) = [-8.1818, 0.0139]
  cd(0:1, 87  ) = [-7.5758, 0.0112]
  cd(0:1, 88  ) = [-6.9697, 0.0098]
  cd(0:1, 89  ) = [-6.3636, 0.0088]
  cd(0:1, 90  ) = [-5.7576, 0.0082]
  cd(0:1, 91  ) = [-5.1515, 0.0079]
  cd(0:1, 92  ) = [-4.5455, 0.0077]
  cd(0:1, 93  ) = [-3.9394, 0.0076]
  cd(0:1, 94  ) = [-3.3333, 0.0074]
  cd(0:1, 95  ) = [-2.7273, 0.0073]
  cd(0:1, 96  ) = [-2.1212, 0.0072]
  cd(0:1, 97  ) = [-1.5152, 0.0071]
  cd(0:1, 98  ) = [-0.9091, 0.0071]
  cd(0:1, 99  ) = [-0.303, 0.0072]
  cd(0:1, 100  ) = [0.303, 0.0072]
  cd(0:1, 101  ) = [0.9091, 0.0072]
  cd(0:1, 102  ) = [1.5152, 0.0072]
  cd(0:1, 103  ) = [2.1212, 0.0074]
  cd(0:1, 104  ) = [2.7273, 0.0075]
  cd(0:1, 105  ) = [3.3333, 0.0077]
  cd(0:1, 106  ) = [3.9394, 0.0078]
  cd(0:1, 107  ) = [4.5455, 0.008]
  cd(0:1, 108  ) = [5.1515, 0.0082]
  cd(0:1, 109  ) = [5.7576, 0.0085]
  cd(0:1, 110  ) = [6.3636, 0.0089]
  cd(0:1, 111  ) = [6.9697, 0.0095]
  cd(0:1, 112  ) = [7.5758, 0.0107]
  cd(0:1, 113  ) = [8.1818, 0.012]
  cd(0:1, 114  ) = [8.7879, 0.014]
  cd(0:1, 115  ) = [9.3939, 0.0224]
  cd(0:1, 116  ) = [10.0, 0.0343]
  cd(0:1, 117  ) = [10.6061, 0.0448]
  cd(0:1, 118  ) = [11.2121, 0.0533]
  cd(0:1, 119  ) = [11.8182, 0.0588]
  cd(0:1, 120  ) = [12.4242, 0.0669]
  cd(0:1, 121  ) = [13.0303, 0.0759]
  cd(0:1, 122  ) = [13.6364, 0.0867]
  cd(0:1, 123  ) = [14.2424, 0.0974]
  cd(0:1, 124  ) = [14.8485, 0.1079]
  cd(0:1, 125  ) = [15.4545, 0.117]
  cd(0:1, 126  ) = [16.0606, 0.1254]
  cd(0:1, 127  ) = [16.6667, 0.1328]
  cd(0:1, 128  ) = [17.2727, 0.1416]
  cd(0:1, 129  ) = [17.8788, 0.1521]
  cd(0:1, 130  ) = [18.4848, 0.1641]
  cd(0:1, 131  ) = [19.0909, 0.1763]
  cd(0:1, 132  ) = [19.697, 0.1877]
  cd(0:1, 133  ) = [20.303, 0.2006]
  cd(0:1, 134  ) = [20.9091, 0.215]
  cd(0:1, 135  ) = [21.5152, 0.2294]
  cd(0:1, 136  ) = [22.1212, 0.2431]
  cd(0:1, 137  ) = [22.7273, 0.2545]
  cd(0:1, 138  ) = [23.3333, 0.2659]
  cd(0:1, 139  ) = [23.9394, 0.2773]
  cd(0:1, 140  ) = [24.5455, 0.2888]
  cd(0:1, 141  ) = [25.1515, 0.3004]
  cd(0:1, 142  ) = [25.7576, 0.312]
  cd(0:1, 143  ) = [26.3636, 0.324]
  cd(0:1, 144  ) = [26.9697, 0.3362]
  cd(0:1, 145  ) = [27.5758, 0.3484]
  cd(0:1, 146  ) = [28.1818, 0.3608]
  cd(0:1, 147  ) = [28.7879, 0.3737]
  cd(0:1, 148  ) = [29.3939, 0.3865]
  cd(0:1, 149  ) = [30.0, 0.3993]
  cd(0:1, 150  ) = [33.0, 0.4661]
  cd(0:1, 151  ) = [36.0, 0.5357]
  cd(0:1, 152  ) = [39.0, 0.6079]
  cd(0:1, 153  ) = [42.0, 0.6817]
  cd(0:1, 154  ) = [45.0, 0.756]
  cd(0:1, 155  ) = [48.0, 0.83]
  cd(0:1, 156  ) = [51.0, 0.9026]
  cd(0:1, 157  ) = [54.0, 0.9719]
  cd(0:1, 158  ) = [57.0, 1.0413]
  cd(0:1, 159  ) = [60.0, 1.1106]
  cd(0:1, 160  ) = [63.0, 1.1666]
  cd(0:1, 161  ) = [66.0, 1.2226]
  cd(0:1, 162  ) = [69.0, 1.2786]
  cd(0:1, 163  ) = [72.0, 1.3211]
  cd(0:1, 164  ) = [75.0, 1.3569]
  cd(0:1, 165  ) = [78.0, 1.3926]
  cd(0:1, 166  ) = [81.0, 1.4201]
  cd(0:1, 167  ) = [84.0, 1.4311]
  cd(0:1, 168  ) = [87.0, 1.4421]
  cd(0:1, 169  ) = [90.0, 1.4531]
  cd(0:1, 170  ) = [93.0, 1.4421]
  cd(0:1, 171  ) = [96.0, 1.4311]
  cd(0:1, 172  ) = [99.0, 1.4201]
  cd(0:1, 173  ) = [102.0, 1.3926]
  cd(0:1, 174  ) = [105.0, 1.3569]
  cd(0:1, 175  ) = [108.0, 1.3211]
  cd(0:1, 176  ) = [111.0, 1.2786]
  cd(0:1, 177  ) = [114.0, 1.2226]
  cd(0:1, 178  ) = [117.0, 1.1666]
  cd(0:1, 179  ) = [120.0, 1.1106]
  cd(0:1, 180  ) = [123.0, 1.0413]
  cd(0:1, 181  ) = [126.0, 0.9719]
  cd(0:1, 182  ) = [129.0, 0.9026]
  cd(0:1, 183  ) = [132.0, 0.83]
  cd(0:1, 184  ) = [135.0, 0.7558]
  cd(0:1, 185  ) = [138.0, 0.6817]
  cd(0:1, 186  ) = [141.0, 0.6089]
  cd(0:1, 187  ) = [144.0, 0.5391]
  cd(0:1, 188  ) = [147.0, 0.4692]
  cd(0:1, 189  ) = [150.0, 0.3993]
  cd(0:1, 190  ) = [153.0, 0.3466]
  cd(0:1, 191  ) = [156.0, 0.294]
  cd(0:1, 192  ) = [159.0, 0.2413]
  cd(0:1, 193  ) = [162.0, 0.2012]
  cd(0:1, 194  ) = [165.0, 0.1674]
  cd(0:1, 195  ) = [168.0, 0.1336]
  cd(0:1, 196  ) = [171.0, 0.0999]
  cd(0:1, 197  ) = [174.0, 0.0666]
  cd(0:1, 198  ) = [177.0, 0.055]
  cd(0:1, 199  ) = [180.0, 0.0543]

  IF ( y .EQ. cd(0,0) ) cd22 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd22 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd22 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd22_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd22 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd22_nrel2p8.pro' 
  ENDIF

!print,y,cd22

END FUNCTION calc_cd22_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd23_nrel2p8( y ) RESULT( cd23 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd23
INTEGER :: i

 cd23 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.0508]
  cd(0:1, 1  ) = [-177.0, 0.0508]
  cd(0:1, 2  ) = [-174.0, 0.0609]
  cd(0:1, 3  ) = [-171.0, 0.0914]
  cd(0:1, 4  ) = [-168.0, 0.1232]
  cd(0:1, 5  ) = [-165.0, 0.1557]
  cd(0:1, 6  ) = [-162.0, 0.1882]
  cd(0:1, 7  ) = [-159.0, 0.2281]
  cd(0:1, 8  ) = [-156.0, 0.2829]
  cd(0:1, 9  ) = [-153.0, 0.3377]
  cd(0:1, 10  ) = [-150.0, 0.3924]
  cd(0:1, 11  ) = [-147.0, 0.4625]
  cd(0:1, 12  ) = [-144.0, 0.5327]
  cd(0:1, 13  ) = [-141.0, 0.6028]
  cd(0:1, 14  ) = [-138.0, 0.6758]
  cd(0:1, 15  ) = [-135.0, 0.7502]
  cd(0:1, 16  ) = [-132.0, 0.8247]
  cd(0:1, 17  ) = [-129.0, 0.8976]
  cd(0:1, 18  ) = [-126.0, 0.9673]
  cd(0:1, 19  ) = [-123.0, 1.0369]
  cd(0:1, 20  ) = [-120.0, 1.1066]
  cd(0:1, 21  ) = [-117.0, 1.163]
  cd(0:1, 22  ) = [-114.0, 1.2194]
  cd(0:1, 23  ) = [-111.0, 1.2758]
  cd(0:1, 24  ) = [-108.0, 1.3187]
  cd(0:1, 25  ) = [-105.0, 1.3548]
  cd(0:1, 26  ) = [-102.0, 1.391]
  cd(0:1, 27  ) = [-99.0, 1.4189]
  cd(0:1, 28  ) = [-96.0, 1.4303]
  cd(0:1, 29  ) = [-93.0, 1.4417]
  cd(0:1, 30  ) = [-90.0, 1.4531]
  cd(0:1, 31  ) = [-87.0, 1.4417]
  cd(0:1, 32  ) = [-84.0, 1.4303]
  cd(0:1, 33  ) = [-81.0, 1.4189]
  cd(0:1, 34  ) = [-78.0, 1.391]
  cd(0:1, 35  ) = [-75.0, 1.3548]
  cd(0:1, 36  ) = [-72.0, 1.3187]
  cd(0:1, 37  ) = [-69.0, 1.2758]
  cd(0:1, 38  ) = [-66.0, 1.2194]
  cd(0:1, 39  ) = [-63.0, 1.163]
  cd(0:1, 40  ) = [-60.0, 1.1066]
  cd(0:1, 41  ) = [-57.0, 1.0369]
  cd(0:1, 42  ) = [-54.0, 0.9673]
  cd(0:1, 43  ) = [-51.0, 0.8976]
  cd(0:1, 44  ) = [-48.0, 0.8247]
  cd(0:1, 45  ) = [-45.0, 0.7503]
  cd(0:1, 46  ) = [-42.0, 0.6758]
  cd(0:1, 47  ) = [-39.0, 0.6017]
  cd(0:1, 48  ) = [-36.0, 0.5293]
  cd(0:1, 49  ) = [-33.0, 0.4594]
  cd(0:1, 50  ) = [-30.0, 0.3924]
  cd(0:1, 51  ) = [-29.3939, 0.3796]
  cd(0:1, 52  ) = [-28.7879, 0.3667]
  cd(0:1, 53  ) = [-28.1818, 0.3538]
  cd(0:1, 54  ) = [-27.5758, 0.3414]
  cd(0:1, 55  ) = [-26.9697, 0.3291]
  cd(0:1, 56  ) = [-26.3636, 0.3169]
  cd(0:1, 57  ) = [-25.7576, 0.3049]
  cd(0:1, 58  ) = [-25.1515, 0.2932]
  cd(0:1, 59  ) = [-24.5455, 0.2816]
  cd(0:1, 60  ) = [-23.9394, 0.2701]
  cd(0:1, 61  ) = [-23.3333, 0.2591]
  cd(0:1, 62  ) = [-22.7273, 0.2481]
  cd(0:1, 63  ) = [-22.1212, 0.2371]
  cd(0:1, 64  ) = [-21.5152, 0.2262]
  cd(0:1, 65  ) = [-20.9091, 0.2153]
  cd(0:1, 66  ) = [-20.303, 0.2044]
  cd(0:1, 67  ) = [-19.697, 0.1935]
  cd(0:1, 68  ) = [-19.0909, 0.1828]
  cd(0:1, 69  ) = [-18.4848, 0.1718]
  cd(0:1, 70  ) = [-17.8788, 0.1602]
  cd(0:1, 71  ) = [-17.2727, 0.1493]
  cd(0:1, 72  ) = [-16.6667, 0.1386]
  cd(0:1, 73  ) = [-16.0606, 0.1282]
  cd(0:1, 74  ) = [-15.4545, 0.118]
  cd(0:1, 75  ) = [-14.8485, 0.1081]
  cd(0:1, 76  ) = [-14.2424, 0.0985]
  cd(0:1, 77  ) = [-13.6364, 0.0889]
  cd(0:1, 78  ) = [-13.0303, 0.0794]
  cd(0:1, 79  ) = [-12.4242, 0.0692]
  cd(0:1, 80  ) = [-11.8182, 0.0598]
  cd(0:1, 81  ) = [-11.2121, 0.0495]
  cd(0:1, 82  ) = [-10.6061, 0.0403]
  cd(0:1, 83  ) = [-10.0, 0.0321]
  cd(0:1, 84  ) = [-9.3939, 0.0253]
  cd(0:1, 85  ) = [-8.7879, 0.0191]
  cd(0:1, 86  ) = [-8.1818, 0.0138]
  cd(0:1, 87  ) = [-7.5758, 0.0108]
  cd(0:1, 88  ) = [-6.9697, 0.0094]
  cd(0:1, 89  ) = [-6.3636, 0.0083]
  cd(0:1, 90  ) = [-5.7576, 0.0078]
  cd(0:1, 91  ) = [-5.1515, 0.0075]
  cd(0:1, 92  ) = [-4.5455, 0.0073]
  cd(0:1, 93  ) = [-3.9394, 0.0072]
  cd(0:1, 94  ) = [-3.3333, 0.0071]
  cd(0:1, 95  ) = [-2.7273, 0.0069]
  cd(0:1, 96  ) = [-2.1212, 0.0069]
  cd(0:1, 97  ) = [-1.5152, 0.0068]
  cd(0:1, 98  ) = [-0.9091, 0.0068]
  cd(0:1, 99  ) = [-0.303, 0.0069]
  cd(0:1, 100  ) = [0.303, 0.0069]
  cd(0:1, 101  ) = [0.9091, 0.0069]
  cd(0:1, 102  ) = [1.5152, 0.007]
  cd(0:1, 103  ) = [2.1212, 0.0071]
  cd(0:1, 104  ) = [2.7273, 0.0072]
  cd(0:1, 105  ) = [3.3333, 0.0074]
  cd(0:1, 106  ) = [3.9394, 0.0075]
  cd(0:1, 107  ) = [4.5455, 0.0077]
  cd(0:1, 108  ) = [5.1515, 0.0079]
  cd(0:1, 109  ) = [5.7576, 0.0082]
  cd(0:1, 110  ) = [6.3636, 0.0086]
  cd(0:1, 111  ) = [6.9697, 0.0093]
  cd(0:1, 112  ) = [7.5758, 0.0107]
  cd(0:1, 113  ) = [8.1818, 0.012]
  cd(0:1, 114  ) = [8.7879, 0.014]
  cd(0:1, 115  ) = [9.3939, 0.0211]
  cd(0:1, 116  ) = [10.0, 0.0308]
  cd(0:1, 117  ) = [10.6061, 0.0396]
  cd(0:1, 118  ) = [11.2121, 0.0469]
  cd(0:1, 119  ) = [11.8182, 0.0534]
  cd(0:1, 120  ) = [12.4242, 0.0631]
  cd(0:1, 121  ) = [13.0303, 0.0734]
  cd(0:1, 122  ) = [13.6364, 0.0855]
  cd(0:1, 123  ) = [14.2424, 0.0968]
  cd(0:1, 124  ) = [14.8485, 0.1068]
  cd(0:1, 125  ) = [15.4545, 0.1157]
  cd(0:1, 126  ) = [16.0606, 0.1241]
  cd(0:1, 127  ) = [16.6667, 0.1316]
  cd(0:1, 128  ) = [17.2727, 0.1402]
  cd(0:1, 129  ) = [17.8788, 0.1503]
  cd(0:1, 130  ) = [18.4848, 0.1614]
  cd(0:1, 131  ) = [19.0909, 0.1727]
  cd(0:1, 132  ) = [19.697, 0.1837]
  cd(0:1, 133  ) = [20.303, 0.1959]
  cd(0:1, 134  ) = [20.9091, 0.2095]
  cd(0:1, 135  ) = [21.5152, 0.223]
  cd(0:1, 136  ) = [22.1212, 0.2361]
  cd(0:1, 137  ) = [22.7273, 0.2474]
  cd(0:1, 138  ) = [23.3333, 0.2587]
  cd(0:1, 139  ) = [23.9394, 0.27]
  cd(0:1, 140  ) = [24.5455, 0.2816]
  cd(0:1, 141  ) = [25.1515, 0.2932]
  cd(0:1, 142  ) = [25.7576, 0.3049]
  cd(0:1, 143  ) = [26.3636, 0.3169]
  cd(0:1, 144  ) = [26.9697, 0.3291]
  cd(0:1, 145  ) = [27.5758, 0.3414]
  cd(0:1, 146  ) = [28.1818, 0.3538]
  cd(0:1, 147  ) = [28.7879, 0.3667]
  cd(0:1, 148  ) = [29.3939, 0.3796]
  cd(0:1, 149  ) = [30.0, 0.3924]
  cd(0:1, 150  ) = [33.0, 0.4594]
  cd(0:1, 151  ) = [36.0, 0.5293]
  cd(0:1, 152  ) = [39.0, 0.6017]
  cd(0:1, 153  ) = [42.0, 0.6758]
  cd(0:1, 154  ) = [45.0, 0.7503]
  cd(0:1, 155  ) = [48.0, 0.8247]
  cd(0:1, 156  ) = [51.0, 0.8976]
  cd(0:1, 157  ) = [54.0, 0.9673]
  cd(0:1, 158  ) = [57.0, 1.0369]
  cd(0:1, 159  ) = [60.0, 1.1066]
  cd(0:1, 160  ) = [63.0, 1.163]
  cd(0:1, 161  ) = [66.0, 1.2194]
  cd(0:1, 162  ) = [69.0, 1.2758]
  cd(0:1, 163  ) = [72.0, 1.3187]
  cd(0:1, 164  ) = [75.0, 1.3548]
  cd(0:1, 165  ) = [78.0, 1.391]
  cd(0:1, 166  ) = [81.0, 1.4189]
  cd(0:1, 167  ) = [84.0, 1.4303]
  cd(0:1, 168  ) = [87.0, 1.4417]
  cd(0:1, 169  ) = [90.0, 1.4531]
  cd(0:1, 170  ) = [93.0, 1.4417]
  cd(0:1, 171  ) = [96.0, 1.4303]
  cd(0:1, 172  ) = [99.0, 1.4189]
  cd(0:1, 173  ) = [102.0, 1.391]
  cd(0:1, 174  ) = [105.0, 1.3548]
  cd(0:1, 175  ) = [108.0, 1.3187]
  cd(0:1, 176  ) = [111.0, 1.2758]
  cd(0:1, 177  ) = [114.0, 1.2194]
  cd(0:1, 178  ) = [117.0, 1.163]
  cd(0:1, 179  ) = [120.0, 1.1066]
  cd(0:1, 180  ) = [123.0, 1.0369]
  cd(0:1, 181  ) = [126.0, 0.9673]
  cd(0:1, 182  ) = [129.0, 0.8976]
  cd(0:1, 183  ) = [132.0, 0.8247]
  cd(0:1, 184  ) = [135.0, 0.7502]
  cd(0:1, 185  ) = [138.0, 0.6758]
  cd(0:1, 186  ) = [141.0, 0.6028]
  cd(0:1, 187  ) = [144.0, 0.5327]
  cd(0:1, 188  ) = [147.0, 0.4625]
  cd(0:1, 189  ) = [150.0, 0.3924]
  cd(0:1, 190  ) = [153.0, 0.3386]
  cd(0:1, 191  ) = [156.0, 0.2848]
  cd(0:1, 192  ) = [159.0, 0.231]
  cd(0:1, 193  ) = [162.0, 0.1912]
  cd(0:1, 194  ) = [165.0, 0.1585]
  cd(0:1, 195  ) = [168.0, 0.1258]
  cd(0:1, 196  ) = [171.0, 0.0936]
  cd(0:1, 197  ) = [174.0, 0.0624]
  cd(0:1, 198  ) = [177.0, 0.0515]
  cd(0:1, 199  ) = [180.0, 0.0508]

  IF ( y .EQ. cd(0,0) ) cd23 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd23 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd23 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd23_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd23 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd23_nrel2p8.pro' 
  ENDIF

!print,y,cd23

END FUNCTION calc_cd23_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd24_nrel2p8( y ) RESULT( cd24 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd24
INTEGER :: i

 cd24 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.0463]
  cd(0:1, 1  ) = [-177.0, 0.0463]
  cd(0:1, 2  ) = [-174.0, 0.0556]
  cd(0:1, 3  ) = [-171.0, 0.0834]
  cd(0:1, 4  ) = [-168.0, 0.1136]
  cd(0:1, 5  ) = [-165.0, 0.145]
  cd(0:1, 6  ) = [-162.0, 0.1764]
  cd(0:1, 7  ) = [-159.0, 0.216]
  cd(0:1, 8  ) = [-156.0, 0.2718]
  cd(0:1, 9  ) = [-153.0, 0.3277]
  cd(0:1, 10  ) = [-150.0, 0.3835]
  cd(0:1, 11  ) = [-147.0, 0.454]
  cd(0:1, 12  ) = [-144.0, 0.5244]
  cd(0:1, 13  ) = [-141.0, 0.5948]
  cd(0:1, 14  ) = [-138.0, 0.6682]
  cd(0:1, 15  ) = [-135.0, 0.743]
  cd(0:1, 16  ) = [-132.0, 0.8179]
  cd(0:1, 17  ) = [-129.0, 0.8911]
  cd(0:1, 18  ) = [-126.0, 0.9612]
  cd(0:1, 19  ) = [-123.0, 1.0314]
  cd(0:1, 20  ) = [-120.0, 1.1015]
  cd(0:1, 21  ) = [-117.0, 1.1584]
  cd(0:1, 22  ) = [-114.0, 1.2153]
  cd(0:1, 23  ) = [-111.0, 1.2721]
  cd(0:1, 24  ) = [-108.0, 1.3155]
  cd(0:1, 25  ) = [-105.0, 1.3522]
  cd(0:1, 26  ) = [-102.0, 1.3889]
  cd(0:1, 27  ) = [-99.0, 1.4173]
  cd(0:1, 28  ) = [-96.0, 1.4292]
  cd(0:1, 29  ) = [-93.0, 1.4411]
  cd(0:1, 30  ) = [-90.0, 1.4531]
  cd(0:1, 31  ) = [-87.0, 1.4411]
  cd(0:1, 32  ) = [-84.0, 1.4292]
  cd(0:1, 33  ) = [-81.0, 1.4173]
  cd(0:1, 34  ) = [-78.0, 1.3889]
  cd(0:1, 35  ) = [-75.0, 1.3522]
  cd(0:1, 36  ) = [-72.0, 1.3155]
  cd(0:1, 37  ) = [-69.0, 1.2721]
  cd(0:1, 38  ) = [-66.0, 1.2153]
  cd(0:1, 39  ) = [-63.0, 1.1584]
  cd(0:1, 40  ) = [-60.0, 1.1015]
  cd(0:1, 41  ) = [-57.0, 1.0314]
  cd(0:1, 42  ) = [-54.0, 0.9612]
  cd(0:1, 43  ) = [-51.0, 0.8911]
  cd(0:1, 44  ) = [-48.0, 0.8179]
  cd(0:1, 45  ) = [-45.0, 0.7431]
  cd(0:1, 46  ) = [-42.0, 0.6682]
  cd(0:1, 47  ) = [-39.0, 0.5938]
  cd(0:1, 48  ) = [-36.0, 0.521]
  cd(0:1, 49  ) = [-33.0, 0.4508]
  cd(0:1, 50  ) = [-30.0, 0.3835]
  cd(0:1, 51  ) = [-29.3939, 0.3706]
  cd(0:1, 52  ) = [-28.7879, 0.3577]
  cd(0:1, 53  ) = [-28.1818, 0.3448]
  cd(0:1, 54  ) = [-27.5758, 0.3323]
  cd(0:1, 55  ) = [-26.9697, 0.32]
  cd(0:1, 56  ) = [-26.3636, 0.3077]
  cd(0:1, 57  ) = [-25.7576, 0.2956]
  cd(0:1, 58  ) = [-25.1515, 0.2839]
  cd(0:1, 59  ) = [-24.5455, 0.2723]
  cd(0:1, 60  ) = [-23.9394, 0.2607]
  cd(0:1, 61  ) = [-23.3333, 0.2497]
  cd(0:1, 62  ) = [-22.7273, 0.2387]
  cd(0:1, 63  ) = [-22.1212, 0.2277]
  cd(0:1, 64  ) = [-21.5152, 0.2168]
  cd(0:1, 65  ) = [-20.9091, 0.206]
  cd(0:1, 66  ) = [-20.303, 0.1951]
  cd(0:1, 67  ) = [-19.697, 0.1845]
  cd(0:1, 68  ) = [-19.0909, 0.174]
  cd(0:1, 69  ) = [-18.4848, 0.1635]
  cd(0:1, 70  ) = [-17.8788, 0.1528]
  cd(0:1, 71  ) = [-17.2727, 0.1429]
  cd(0:1, 72  ) = [-16.6667, 0.1331]
  cd(0:1, 73  ) = [-16.0606, 0.1234]
  cd(0:1, 74  ) = [-15.4545, 0.1139]
  cd(0:1, 75  ) = [-14.8485, 0.1046]
  cd(0:1, 76  ) = [-14.2424, 0.0955]
  cd(0:1, 77  ) = [-13.6364, 0.0865]
  cd(0:1, 78  ) = [-13.0303, 0.0775]
  cd(0:1, 79  ) = [-12.4242, 0.0682]
  cd(0:1, 80  ) = [-11.8182, 0.0597]
  cd(0:1, 81  ) = [-11.2121, 0.05]
  cd(0:1, 82  ) = [-10.6061, 0.0411]
  cd(0:1, 83  ) = [-10.0, 0.0329]
  cd(0:1, 84  ) = [-9.3939, 0.0259]
  cd(0:1, 85  ) = [-8.7879, 0.0194]
  cd(0:1, 86  ) = [-8.1818, 0.0137]
  cd(0:1, 87  ) = [-7.5758, 0.0106]
  cd(0:1, 88  ) = [-6.9697, 0.0091]
  cd(0:1, 89  ) = [-6.3636, 0.0079]
  cd(0:1, 90  ) = [-5.7576, 0.0074]
  cd(0:1, 91  ) = [-5.1515, 0.0071]
  cd(0:1, 92  ) = [-4.5455, 0.007]
  cd(0:1, 93  ) = [-3.9394, 0.0068]
  cd(0:1, 94  ) = [-3.3333, 0.0068]
  cd(0:1, 95  ) = [-2.7273, 0.0067]
  cd(0:1, 96  ) = [-2.1212, 0.0066]
  cd(0:1, 97  ) = [-1.5152, 0.0066]
  cd(0:1, 98  ) = [-0.9091, 0.0066]
  cd(0:1, 99  ) = [-0.303, 0.0066]
  cd(0:1, 100  ) = [0.303, 0.0067]
  cd(0:1, 101  ) = [0.9091, 0.0067]
  cd(0:1, 102  ) = [1.5152, 0.0068]
  cd(0:1, 103  ) = [2.1212, 0.0068]
  cd(0:1, 104  ) = [2.7273, 0.007]
  cd(0:1, 105  ) = [3.3333, 0.0071]
  cd(0:1, 106  ) = [3.9394, 0.0072]
  cd(0:1, 107  ) = [4.5455, 0.0074]
  cd(0:1, 108  ) = [5.1515, 0.0076]
  cd(0:1, 109  ) = [5.7576, 0.0079]
  cd(0:1, 110  ) = [6.3636, 0.0083]
  cd(0:1, 111  ) = [6.9697, 0.0091]
  cd(0:1, 112  ) = [7.5758, 0.0107]
  cd(0:1, 113  ) = [8.1818, 0.0122]
  cd(0:1, 114  ) = [8.7879, 0.014]
  cd(0:1, 115  ) = [9.3939, 0.0192]
  cd(0:1, 116  ) = [10.0, 0.0262]
  cd(0:1, 117  ) = [10.6061, 0.0327]
  cd(0:1, 118  ) = [11.2121, 0.0383]
  cd(0:1, 119  ) = [11.8182, 0.0461]
  cd(0:1, 120  ) = [12.4242, 0.0579]
  cd(0:1, 121  ) = [13.0303, 0.07]
  cd(0:1, 122  ) = [13.6364, 0.0837]
  cd(0:1, 123  ) = [14.2424, 0.0956]
  cd(0:1, 124  ) = [14.8485, 0.105]
  cd(0:1, 125  ) = [15.4545, 0.1137]
  cd(0:1, 126  ) = [16.0606, 0.122]
  cd(0:1, 127  ) = [16.6667, 0.1297]
  cd(0:1, 128  ) = [17.2727, 0.1382]
  cd(0:1, 129  ) = [17.8788, 0.1476]
  cd(0:1, 130  ) = [18.4848, 0.1577]
  cd(0:1, 131  ) = [19.0909, 0.1679]
  cd(0:1, 132  ) = [19.697, 0.1784]
  cd(0:1, 133  ) = [20.303, 0.1899]
  cd(0:1, 134  ) = [20.9091, 0.2023]
  cd(0:1, 135  ) = [21.5152, 0.2148]
  cd(0:1, 136  ) = [22.1212, 0.227]
  cd(0:1, 137  ) = [22.7273, 0.2382]
  cd(0:1, 138  ) = [23.3333, 0.2494]
  cd(0:1, 139  ) = [23.9394, 0.2607]
  cd(0:1, 140  ) = [24.5455, 0.2723]
  cd(0:1, 141  ) = [25.1515, 0.2839]
  cd(0:1, 142  ) = [25.7576, 0.2956]
  cd(0:1, 143  ) = [26.3636, 0.3077]
  cd(0:1, 144  ) = [26.9697, 0.32]
  cd(0:1, 145  ) = [27.5758, 0.3323]
  cd(0:1, 146  ) = [28.1818, 0.3448]
  cd(0:1, 147  ) = [28.7879, 0.3577]
  cd(0:1, 148  ) = [29.3939, 0.3706]
  cd(0:1, 149  ) = [30.0, 0.3835]
  cd(0:1, 150  ) = [33.0, 0.4508]
  cd(0:1, 151  ) = [36.0, 0.521]
  cd(0:1, 152  ) = [39.0, 0.5938]
  cd(0:1, 153  ) = [42.0, 0.6682]
  cd(0:1, 154  ) = [45.0, 0.7431]
  cd(0:1, 155  ) = [48.0, 0.8179]
  cd(0:1, 156  ) = [51.0, 0.8911]
  cd(0:1, 157  ) = [54.0, 0.9612]
  cd(0:1, 158  ) = [57.0, 1.0314]
  cd(0:1, 159  ) = [60.0, 1.1015]
  cd(0:1, 160  ) = [63.0, 1.1584]
  cd(0:1, 161  ) = [66.0, 1.2153]
  cd(0:1, 162  ) = [69.0, 1.2721]
  cd(0:1, 163  ) = [72.0, 1.3155]
  cd(0:1, 164  ) = [75.0, 1.3522]
  cd(0:1, 165  ) = [78.0, 1.3889]
  cd(0:1, 166  ) = [81.0, 1.4173]
  cd(0:1, 167  ) = [84.0, 1.4292]
  cd(0:1, 168  ) = [87.0, 1.4411]
  cd(0:1, 169  ) = [90.0, 1.4531]
  cd(0:1, 170  ) = [93.0, 1.4411]
  cd(0:1, 171  ) = [96.0, 1.4292]
  cd(0:1, 172  ) = [99.0, 1.4173]
  cd(0:1, 173  ) = [102.0, 1.3889]
  cd(0:1, 174  ) = [105.0, 1.3522]
  cd(0:1, 175  ) = [108.0, 1.3155]
  cd(0:1, 176  ) = [111.0, 1.2721]
  cd(0:1, 177  ) = [114.0, 1.2153]
  cd(0:1, 178  ) = [117.0, 1.1584]
  cd(0:1, 179  ) = [120.0, 1.1015]
  cd(0:1, 180  ) = [123.0, 1.0314]
  cd(0:1, 181  ) = [126.0, 0.9612]
  cd(0:1, 182  ) = [129.0, 0.8911]
  cd(0:1, 183  ) = [132.0, 0.8179]
  cd(0:1, 184  ) = [135.0, 0.743]
  cd(0:1, 185  ) = [138.0, 0.6682]
  cd(0:1, 186  ) = [141.0, 0.5948]
  cd(0:1, 187  ) = [144.0, 0.5244]
  cd(0:1, 188  ) = [147.0, 0.454]
  cd(0:1, 189  ) = [150.0, 0.3835]
  cd(0:1, 190  ) = [153.0, 0.3283]
  cd(0:1, 191  ) = [156.0, 0.273]
  cd(0:1, 192  ) = [159.0, 0.2178]
  cd(0:1, 193  ) = [162.0, 0.1785]
  cd(0:1, 194  ) = [165.0, 0.1473]
  cd(0:1, 195  ) = [168.0, 0.1161]
  cd(0:1, 196  ) = [171.0, 0.0858]
  cd(0:1, 197  ) = [174.0, 0.0572]
  cd(0:1, 198  ) = [177.0, 0.0471]
  cd(0:1, 199  ) = [180.0, 0.0463]

  IF ( y .EQ. cd(0,0) ) cd24 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd24 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd24 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd24_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd24 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd24_nrel2p8.pro' 
  ENDIF

!print,y,cd24

END FUNCTION calc_cd24_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd25_nrel2p8( y ) RESULT( cd25 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd25
INTEGER :: i

 cd25 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.042]
  cd(0:1, 1  ) = [-177.0, 0.042]
  cd(0:1, 2  ) = [-174.0, 0.0504]
  cd(0:1, 3  ) = [-171.0, 0.0756]
  cd(0:1, 4  ) = [-168.0, 0.1042]
  cd(0:1, 5  ) = [-165.0, 0.1345]
  cd(0:1, 6  ) = [-162.0, 0.1648]
  cd(0:1, 7  ) = [-159.0, 0.204]
  cd(0:1, 8  ) = [-156.0, 0.2609]
  cd(0:1, 9  ) = [-153.0, 0.3178]
  cd(0:1, 10  ) = [-150.0, 0.3747]
  cd(0:1, 11  ) = [-147.0, 0.4454]
  cd(0:1, 12  ) = [-144.0, 0.5162]
  cd(0:1, 13  ) = [-141.0, 0.5869]
  cd(0:1, 14  ) = [-138.0, 0.6606]
  cd(0:1, 15  ) = [-135.0, 0.7358]
  cd(0:1, 16  ) = [-132.0, 0.811]
  cd(0:1, 17  ) = [-129.0, 0.8847]
  cd(0:1, 18  ) = [-126.0, 0.9553]
  cd(0:1, 19  ) = [-123.0, 1.0258]
  cd(0:1, 20  ) = [-120.0, 1.0964]
  cd(0:1, 21  ) = [-117.0, 1.1538]
  cd(0:1, 22  ) = [-114.0, 1.2111]
  cd(0:1, 23  ) = [-111.0, 1.2685]
  cd(0:1, 24  ) = [-108.0, 1.3124]
  cd(0:1, 25  ) = [-105.0, 1.3496]
  cd(0:1, 26  ) = [-102.0, 1.3867]
  cd(0:1, 27  ) = [-99.0, 1.4157]
  cd(0:1, 28  ) = [-96.0, 1.4281]
  cd(0:1, 29  ) = [-93.0, 1.4406]
  cd(0:1, 30  ) = [-90.0, 1.4531]
  cd(0:1, 31  ) = [-87.0, 1.4406]
  cd(0:1, 32  ) = [-84.0, 1.4281]
  cd(0:1, 33  ) = [-81.0, 1.4157]
  cd(0:1, 34  ) = [-78.0, 1.3867]
  cd(0:1, 35  ) = [-75.0, 1.3496]
  cd(0:1, 36  ) = [-72.0, 1.3124]
  cd(0:1, 37  ) = [-69.0, 1.2685]
  cd(0:1, 38  ) = [-66.0, 1.2111]
  cd(0:1, 39  ) = [-63.0, 1.1538]
  cd(0:1, 40  ) = [-60.0, 1.0964]
  cd(0:1, 41  ) = [-57.0, 1.0258]
  cd(0:1, 42  ) = [-54.0, 0.9553]
  cd(0:1, 43  ) = [-51.0, 0.8847]
  cd(0:1, 44  ) = [-48.0, 0.811]
  cd(0:1, 45  ) = [-45.0, 0.7359]
  cd(0:1, 46  ) = [-42.0, 0.6606]
  cd(0:1, 47  ) = [-39.0, 0.5858]
  cd(0:1, 48  ) = [-36.0, 0.5127]
  cd(0:1, 49  ) = [-33.0, 0.4423]
  cd(0:1, 50  ) = [-30.0, 0.3747]
  cd(0:1, 51  ) = [-29.3939, 0.3617]
  cd(0:1, 52  ) = [-28.7879, 0.3488]
  cd(0:1, 53  ) = [-28.1818, 0.3358]
  cd(0:1, 54  ) = [-27.5758, 0.3233]
  cd(0:1, 55  ) = [-26.9697, 0.3109]
  cd(0:1, 56  ) = [-26.3636, 0.2985]
  cd(0:1, 57  ) = [-25.7576, 0.2864]
  cd(0:1, 58  ) = [-25.1515, 0.2747]
  cd(0:1, 59  ) = [-24.5455, 0.263]
  cd(0:1, 60  ) = [-23.9394, 0.2513]
  cd(0:1, 61  ) = [-23.3333, 0.2403]
  cd(0:1, 62  ) = [-22.7273, 0.2293]
  cd(0:1, 63  ) = [-22.1212, 0.2183]
  cd(0:1, 64  ) = [-21.5152, 0.2076]
  cd(0:1, 65  ) = [-20.9091, 0.1969]
  cd(0:1, 66  ) = [-20.303, 0.1863]
  cd(0:1, 67  ) = [-19.697, 0.1759]
  cd(0:1, 68  ) = [-19.0909, 0.1658]
  cd(0:1, 69  ) = [-18.4848, 0.1558]
  cd(0:1, 70  ) = [-17.8788, 0.1461]
  cd(0:1, 71  ) = [-17.2727, 0.1371]
  cd(0:1, 72  ) = [-16.6667, 0.1282]
  cd(0:1, 73  ) = [-16.0606, 0.1193]
  cd(0:1, 74  ) = [-15.4545, 0.1106]
  cd(0:1, 75  ) = [-14.8485, 0.1019]
  cd(0:1, 76  ) = [-14.2424, 0.0933]
  cd(0:1, 77  ) = [-13.6364, 0.0848]
  cd(0:1, 78  ) = [-13.0303, 0.0763]
  cd(0:1, 79  ) = [-12.4242, 0.0678]
  cd(0:1, 80  ) = [-11.8182, 0.0596]
  cd(0:1, 81  ) = [-11.2121, 0.0507]
  cd(0:1, 82  ) = [-10.6061, 0.0422]
  cd(0:1, 83  ) = [-10.0, 0.0341]
  cd(0:1, 84  ) = [-9.3939, 0.0266]
  cd(0:1, 85  ) = [-8.7879, 0.0197]
  cd(0:1, 86  ) = [-8.1818, 0.0136]
  cd(0:1, 87  ) = [-7.5758, 0.0104]
  cd(0:1, 88  ) = [-6.9697, 0.009]
  cd(0:1, 89  ) = [-6.3636, 0.0077]
  cd(0:1, 90  ) = [-5.7576, 0.0072]
  cd(0:1, 91  ) = [-5.1515, 0.0069]
  cd(0:1, 92  ) = [-4.5455, 0.0067]
  cd(0:1, 93  ) = [-3.9394, 0.0066]
  cd(0:1, 94  ) = [-3.3333, 0.0065]
  cd(0:1, 95  ) = [-2.7273, 0.0065]
  cd(0:1, 96  ) = [-2.1212, 0.0064]
  cd(0:1, 97  ) = [-1.5152, 0.0064]
  cd(0:1, 98  ) = [-0.9091, 0.0064]
  cd(0:1, 99  ) = [-0.303, 0.0065]
  cd(0:1, 100  ) = [0.303, 0.0065]
  cd(0:1, 101  ) = [0.9091, 0.0065]
  cd(0:1, 102  ) = [1.5152, 0.0066]
  cd(0:1, 103  ) = [2.1212, 0.0067]
  cd(0:1, 104  ) = [2.7273, 0.0068]
  cd(0:1, 105  ) = [3.3333, 0.0069]
  cd(0:1, 106  ) = [3.9394, 0.007]
  cd(0:1, 107  ) = [4.5455, 0.0072]
  cd(0:1, 108  ) = [5.1515, 0.0074]
  cd(0:1, 109  ) = [5.7576, 0.0077]
  cd(0:1, 110  ) = [6.3636, 0.0081]
  cd(0:1, 111  ) = [6.9697, 0.009]
  cd(0:1, 112  ) = [7.5758, 0.0107]
  cd(0:1, 113  ) = [8.1818, 0.0123]
  cd(0:1, 114  ) = [8.7879, 0.014]
  cd(0:1, 115  ) = [9.3939, 0.0174]
  cd(0:1, 116  ) = [10.0, 0.0215]
  cd(0:1, 117  ) = [10.6061, 0.0256]
  cd(0:1, 118  ) = [11.2121, 0.0296]
  cd(0:1, 119  ) = [11.8182, 0.0384]
  cd(0:1, 120  ) = [12.4242, 0.0525]
  cd(0:1, 121  ) = [13.0303, 0.0666]
  cd(0:1, 122  ) = [13.6364, 0.0816]
  cd(0:1, 123  ) = [14.2424, 0.0942]
  cd(0:1, 124  ) = [14.8485, 0.103]
  cd(0:1, 125  ) = [15.4545, 0.1114]
  cd(0:1, 126  ) = [16.0606, 0.1196]
  cd(0:1, 127  ) = [16.6667, 0.1276]
  cd(0:1, 128  ) = [17.2727, 0.1359]
  cd(0:1, 129  ) = [17.8788, 0.1447]
  cd(0:1, 130  ) = [18.4848, 0.1538]
  cd(0:1, 131  ) = [19.0909, 0.1631]
  cd(0:1, 132  ) = [19.697, 0.1731]
  cd(0:1, 133  ) = [20.303, 0.1838]
  cd(0:1, 134  ) = [20.9091, 0.1952]
  cd(0:1, 135  ) = [21.5152, 0.2066]
  cd(0:1, 136  ) = [22.1212, 0.2179]
  cd(0:1, 137  ) = [22.7273, 0.2291]
  cd(0:1, 138  ) = [23.3333, 0.2402]
  cd(0:1, 139  ) = [23.9394, 0.2513]
  cd(0:1, 140  ) = [24.5455, 0.263]
  cd(0:1, 141  ) = [25.1515, 0.2747]
  cd(0:1, 142  ) = [25.7576, 0.2864]
  cd(0:1, 143  ) = [26.3636, 0.2985]
  cd(0:1, 144  ) = [26.9697, 0.3109]
  cd(0:1, 145  ) = [27.5758, 0.3233]
  cd(0:1, 146  ) = [28.1818, 0.3358]
  cd(0:1, 147  ) = [28.7879, 0.3488]
  cd(0:1, 148  ) = [29.3939, 0.3617]
  cd(0:1, 149  ) = [30.0, 0.3747]
  cd(0:1, 150  ) = [33.0, 0.4423]
  cd(0:1, 151  ) = [36.0, 0.5127]
  cd(0:1, 152  ) = [39.0, 0.5858]
  cd(0:1, 153  ) = [42.0, 0.6606]
  cd(0:1, 154  ) = [45.0, 0.7359]
  cd(0:1, 155  ) = [48.0, 0.811]
  cd(0:1, 156  ) = [51.0, 0.8847]
  cd(0:1, 157  ) = [54.0, 0.9553]
  cd(0:1, 158  ) = [57.0, 1.0258]
  cd(0:1, 159  ) = [60.0, 1.0964]
  cd(0:1, 160  ) = [63.0, 1.1538]
  cd(0:1, 161  ) = [66.0, 1.2111]
  cd(0:1, 162  ) = [69.0, 1.2685]
  cd(0:1, 163  ) = [72.0, 1.3124]
  cd(0:1, 164  ) = [75.0, 1.3496]
  cd(0:1, 165  ) = [78.0, 1.3867]
  cd(0:1, 166  ) = [81.0, 1.4157]
  cd(0:1, 167  ) = [84.0, 1.4281]
  cd(0:1, 168  ) = [87.0, 1.4406]
  cd(0:1, 169  ) = [90.0, 1.4531]
  cd(0:1, 170  ) = [93.0, 1.4406]
  cd(0:1, 171  ) = [96.0, 1.4281]
  cd(0:1, 172  ) = [99.0, 1.4157]
  cd(0:1, 173  ) = [102.0, 1.3867]
  cd(0:1, 174  ) = [105.0, 1.3496]
  cd(0:1, 175  ) = [108.0, 1.3124]
  cd(0:1, 176  ) = [111.0, 1.2685]
  cd(0:1, 177  ) = [114.0, 1.2111]
  cd(0:1, 178  ) = [117.0, 1.1538]
  cd(0:1, 179  ) = [120.0, 1.0964]
  cd(0:1, 180  ) = [123.0, 1.0258]
  cd(0:1, 181  ) = [126.0, 0.9553]
  cd(0:1, 182  ) = [129.0, 0.8847]
  cd(0:1, 183  ) = [132.0, 0.811]
  cd(0:1, 184  ) = [135.0, 0.7358]
  cd(0:1, 185  ) = [138.0, 0.6606]
  cd(0:1, 186  ) = [141.0, 0.5869]
  cd(0:1, 187  ) = [144.0, 0.5162]
  cd(0:1, 188  ) = [147.0, 0.4454]
  cd(0:1, 189  ) = [150.0, 0.3747]
  cd(0:1, 190  ) = [153.0, 0.3181]
  cd(0:1, 191  ) = [156.0, 0.2615]
  cd(0:1, 192  ) = [159.0, 0.2049]
  cd(0:1, 193  ) = [162.0, 0.1662]
  cd(0:1, 194  ) = [165.0, 0.1365]
  cd(0:1, 195  ) = [168.0, 0.1067]
  cd(0:1, 196  ) = [171.0, 0.0782]
  cd(0:1, 197  ) = [174.0, 0.0521]
  cd(0:1, 198  ) = [177.0, 0.0429]
  cd(0:1, 199  ) = [180.0, 0.042]

  IF ( y .EQ. cd(0,0) ) cd25 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd25 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd25 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd25_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd25 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd25_nrel2p8.pro' 
  ENDIF

!print,y,cd25

END FUNCTION calc_cd25_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd26_nrel2p8( y ) RESULT( cd26 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd26
INTEGER :: i

 cd26 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.0389]
  cd(0:1, 1  ) = [-177.0, 0.0389]
  cd(0:1, 2  ) = [-174.0, 0.0467]
  cd(0:1, 3  ) = [-171.0, 0.07]
  cd(0:1, 4  ) = [-168.0, 0.0974]
  cd(0:1, 5  ) = [-165.0, 0.1268]
  cd(0:1, 6  ) = [-162.0, 0.1563]
  cd(0:1, 7  ) = [-159.0, 0.1951]
  cd(0:1, 8  ) = [-156.0, 0.2527]
  cd(0:1, 9  ) = [-153.0, 0.3104]
  cd(0:1, 10  ) = [-150.0, 0.368]
  cd(0:1, 11  ) = [-147.0, 0.439]
  cd(0:1, 12  ) = [-144.0, 0.51]
  cd(0:1, 13  ) = [-141.0, 0.5809]
  cd(0:1, 14  ) = [-138.0, 0.6549]
  cd(0:1, 15  ) = [-135.0, 0.7304]
  cd(0:1, 16  ) = [-132.0, 0.8059]
  cd(0:1, 17  ) = [-129.0, 0.8799]
  cd(0:1, 18  ) = [-126.0, 0.9508]
  cd(0:1, 19  ) = [-123.0, 1.0217]
  cd(0:1, 20  ) = [-120.0, 1.0926]
  cd(0:1, 21  ) = [-117.0, 1.1503]
  cd(0:1, 22  ) = [-114.0, 1.208]
  cd(0:1, 23  ) = [-111.0, 1.2657]
  cd(0:1, 24  ) = [-108.0, 1.31]
  cd(0:1, 25  ) = [-105.0, 1.3476]
  cd(0:1, 26  ) = [-102.0, 1.3852]
  cd(0:1, 27  ) = [-99.0, 1.4145]
  cd(0:1, 28  ) = [-96.0, 1.4273]
  cd(0:1, 29  ) = [-93.0, 1.4402]
  cd(0:1, 30  ) = [-90.0, 1.4531]
  cd(0:1, 31  ) = [-87.0, 1.4402]
  cd(0:1, 32  ) = [-84.0, 1.4273]
  cd(0:1, 33  ) = [-81.0, 1.4145]
  cd(0:1, 34  ) = [-78.0, 1.3852]
  cd(0:1, 35  ) = [-75.0, 1.3476]
  cd(0:1, 36  ) = [-72.0, 1.31]
  cd(0:1, 37  ) = [-69.0, 1.2657]
  cd(0:1, 38  ) = [-66.0, 1.208]
  cd(0:1, 39  ) = [-63.0, 1.1503]
  cd(0:1, 40  ) = [-60.0, 1.0925]
  cd(0:1, 41  ) = [-57.0, 1.0217]
  cd(0:1, 42  ) = [-54.0, 0.9508]
  cd(0:1, 43  ) = [-51.0, 0.8799]
  cd(0:1, 44  ) = [-48.0, 0.8059]
  cd(0:1, 45  ) = [-45.0, 0.7304]
  cd(0:1, 46  ) = [-42.0, 0.6549]
  cd(0:1, 47  ) = [-39.0, 0.5798]
  cd(0:1, 48  ) = [-36.0, 0.5065]
  cd(0:1, 49  ) = [-33.0, 0.4358]
  cd(0:1, 50  ) = [-30.0, 0.368]
  cd(0:1, 51  ) = [-29.3939, 0.355]
  cd(0:1, 52  ) = [-28.7879, 0.342]
  cd(0:1, 53  ) = [-28.1818, 0.329]
  cd(0:1, 54  ) = [-27.5758, 0.3164]
  cd(0:1, 55  ) = [-26.9697, 0.304]
  cd(0:1, 56  ) = [-26.3636, 0.2916]
  cd(0:1, 57  ) = [-25.7576, 0.2795]
  cd(0:1, 58  ) = [-25.1515, 0.2677]
  cd(0:1, 59  ) = [-24.5455, 0.256]
  cd(0:1, 60  ) = [-23.9394, 0.2443]
  cd(0:1, 61  ) = [-23.3333, 0.2333]
  cd(0:1, 62  ) = [-22.7273, 0.2222]
  cd(0:1, 63  ) = [-22.1212, 0.2112]
  cd(0:1, 64  ) = [-21.5152, 0.2007]
  cd(0:1, 65  ) = [-20.9091, 0.1903]
  cd(0:1, 66  ) = [-20.303, 0.1799]
  cd(0:1, 67  ) = [-19.697, 0.1698]
  cd(0:1, 68  ) = [-19.0909, 0.16]
  cd(0:1, 69  ) = [-18.4848, 0.1505]
  cd(0:1, 70  ) = [-17.8788, 0.1413]
  cd(0:1, 71  ) = [-17.2727, 0.1331]
  cd(0:1, 72  ) = [-16.6667, 0.1248]
  cd(0:1, 73  ) = [-16.0606, 0.1166]
  cd(0:1, 74  ) = [-15.4545, 0.1084]
  cd(0:1, 75  ) = [-14.8485, 0.1002]
  cd(0:1, 76  ) = [-14.2424, 0.092]
  cd(0:1, 77  ) = [-13.6364, 0.0839]
  cd(0:1, 78  ) = [-13.0303, 0.0757]
  cd(0:1, 79  ) = [-12.4242, 0.0676]
  cd(0:1, 80  ) = [-11.8182, 0.0596]
  cd(0:1, 81  ) = [-11.2121, 0.0513]
  cd(0:1, 82  ) = [-10.6061, 0.0432]
  cd(0:1, 83  ) = [-10.0, 0.0352]
  cd(0:1, 84  ) = [-9.3939, 0.0273]
  cd(0:1, 85  ) = [-8.7879, 0.02]
  cd(0:1, 86  ) = [-8.1818, 0.0136]
  cd(0:1, 87  ) = [-7.5758, 0.0104]
  cd(0:1, 88  ) = [-6.9697, 0.0089]
  cd(0:1, 89  ) = [-6.3636, 0.0075]
  cd(0:1, 90  ) = [-5.7576, 0.0071]
  cd(0:1, 91  ) = [-5.1515, 0.0068]
  cd(0:1, 92  ) = [-4.5455, 0.0066]
  cd(0:1, 93  ) = [-3.9394, 0.0065]
  cd(0:1, 94  ) = [-3.3333, 0.0064]
  cd(0:1, 95  ) = [-2.7273, 0.0063]
  cd(0:1, 96  ) = [-2.1212, 0.0063]
  cd(0:1, 97  ) = [-1.5152, 0.0063]
  cd(0:1, 98  ) = [-0.9091, 0.0063]
  cd(0:1, 99  ) = [-0.303, 0.0064]
  cd(0:1, 100  ) = [0.303, 0.0064]
  cd(0:1, 101  ) = [0.9091, 0.0064]
  cd(0:1, 102  ) = [1.5152, 0.0065]
  cd(0:1, 103  ) = [2.1212, 0.0066]
  cd(0:1, 104  ) = [2.7273, 0.0067]
  cd(0:1, 105  ) = [3.3333, 0.0068]
  cd(0:1, 106  ) = [3.9394, 0.0069]
  cd(0:1, 107  ) = [4.5455, 0.0071]
  cd(0:1, 108  ) = [5.1515, 0.0072]
  cd(0:1, 109  ) = [5.7576, 0.0075]
  cd(0:1, 110  ) = [6.3636, 0.008]
  cd(0:1, 111  ) = [6.9697, 0.009]
  cd(0:1, 112  ) = [7.5758, 0.0107]
  cd(0:1, 113  ) = [8.1818, 0.0125]
  cd(0:1, 114  ) = [8.7879, 0.014]
  cd(0:1, 115  ) = [9.3939, 0.0159]
  cd(0:1, 116  ) = [10.0, 0.0179]
  cd(0:1, 117  ) = [10.6061, 0.0203]
  cd(0:1, 118  ) = [11.2121, 0.023]
  cd(0:1, 119  ) = [11.8182, 0.0325]
  cd(0:1, 120  ) = [12.4242, 0.0482]
  cd(0:1, 121  ) = [13.0303, 0.0639]
  cd(0:1, 122  ) = [13.6364, 0.0799]
  cd(0:1, 123  ) = [14.2424, 0.0928]
  cd(0:1, 124  ) = [14.8485, 0.1012]
  cd(0:1, 125  ) = [15.4545, 0.1094]
  cd(0:1, 126  ) = [16.0606, 0.1176]
  cd(0:1, 127  ) = [16.6667, 0.1258]
  cd(0:1, 128  ) = [17.2727, 0.134]
  cd(0:1, 129  ) = [17.8788, 0.1423]
  cd(0:1, 130  ) = [18.4848, 0.1507]
  cd(0:1, 131  ) = [19.0909, 0.1593]
  cd(0:1, 132  ) = [19.697, 0.1691]
  cd(0:1, 133  ) = [20.303, 0.1793]
  cd(0:1, 134  ) = [20.9091, 0.1898]
  cd(0:1, 135  ) = [21.5152, 0.2004]
  cd(0:1, 136  ) = [22.1212, 0.2111]
  cd(0:1, 137  ) = [22.7273, 0.2222]
  cd(0:1, 138  ) = [23.3333, 0.2332]
  cd(0:1, 139  ) = [23.9394, 0.2443]
  cd(0:1, 140  ) = [24.5455, 0.256]
  cd(0:1, 141  ) = [25.1515, 0.2677]
  cd(0:1, 142  ) = [25.7576, 0.2795]
  cd(0:1, 143  ) = [26.3636, 0.2916]
  cd(0:1, 144  ) = [26.9697, 0.304]
  cd(0:1, 145  ) = [27.5758, 0.3164]
  cd(0:1, 146  ) = [28.1818, 0.329]
  cd(0:1, 147  ) = [28.7879, 0.342]
  cd(0:1, 148  ) = [29.3939, 0.355]
  cd(0:1, 149  ) = [30.0, 0.368]
  cd(0:1, 150  ) = [33.0, 0.4358]
  cd(0:1, 151  ) = [36.0, 0.5065]
  cd(0:1, 152  ) = [39.0, 0.5798]
  cd(0:1, 153  ) = [42.0, 0.6549]
  cd(0:1, 154  ) = [45.0, 0.7304]
  cd(0:1, 155  ) = [48.0, 0.8059]
  cd(0:1, 156  ) = [51.0, 0.8799]
  cd(0:1, 157  ) = [54.0, 0.9508]
  cd(0:1, 158  ) = [57.0, 1.0217]
  cd(0:1, 159  ) = [60.0, 1.0925]
  cd(0:1, 160  ) = [63.0, 1.1503]
  cd(0:1, 161  ) = [66.0, 1.208]
  cd(0:1, 162  ) = [69.0, 1.2657]
  cd(0:1, 163  ) = [72.0, 1.31]
  cd(0:1, 164  ) = [75.0, 1.3476]
  cd(0:1, 165  ) = [78.0, 1.3852]
  cd(0:1, 166  ) = [81.0, 1.4145]
  cd(0:1, 167  ) = [84.0, 1.4273]
  cd(0:1, 168  ) = [87.0, 1.4402]
  cd(0:1, 169  ) = [90.0, 1.4531]
  cd(0:1, 170  ) = [93.0, 1.4402]
  cd(0:1, 171  ) = [96.0, 1.4273]
  cd(0:1, 172  ) = [99.0, 1.4145]
  cd(0:1, 173  ) = [102.0, 1.3852]
  cd(0:1, 174  ) = [105.0, 1.3476]
  cd(0:1, 175  ) = [108.0, 1.31]
  cd(0:1, 176  ) = [111.0, 1.2657]
  cd(0:1, 177  ) = [114.0, 1.208]
  cd(0:1, 178  ) = [117.0, 1.1503]
  cd(0:1, 179  ) = [120.0, 1.0926]
  cd(0:1, 180  ) = [123.0, 1.0217]
  cd(0:1, 181  ) = [126.0, 0.9508]
  cd(0:1, 182  ) = [129.0, 0.8799]
  cd(0:1, 183  ) = [132.0, 0.8059]
  cd(0:1, 184  ) = [135.0, 0.7304]
  cd(0:1, 185  ) = [138.0, 0.6549]
  cd(0:1, 186  ) = [141.0, 0.5809]
  cd(0:1, 187  ) = [144.0, 0.51]
  cd(0:1, 188  ) = [147.0, 0.439]
  cd(0:1, 189  ) = [150.0, 0.368]
  cd(0:1, 190  ) = [153.0, 0.3105]
  cd(0:1, 191  ) = [156.0, 0.2529]
  cd(0:1, 192  ) = [159.0, 0.1953]
  cd(0:1, 193  ) = [162.0, 0.1571]
  cd(0:1, 194  ) = [165.0, 0.1284]
  cd(0:1, 195  ) = [168.0, 0.0998]
  cd(0:1, 196  ) = [171.0, 0.0727]
  cd(0:1, 197  ) = [174.0, 0.0485]
  cd(0:1, 198  ) = [177.0, 0.0398]
  cd(0:1, 199  ) = [180.0, 0.0389]

  IF ( y .EQ. cd(0,0) ) cd26 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd26 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd26 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd26_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd26 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd26_nrel2p8.pro' 
  ENDIF

!print,y,cd26

END FUNCTION calc_cd26_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd27_nrel2p8( y ) RESULT( cd27 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd27
INTEGER :: i

 cd27 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.0377]
  cd(0:1, 1  ) = [-177.0, 0.0377]
  cd(0:1, 2  ) = [-174.0, 0.0452]
  cd(0:1, 3  ) = [-171.0, 0.0679]
  cd(0:1, 4  ) = [-168.0, 0.0948]
  cd(0:1, 5  ) = [-165.0, 0.1239]
  cd(0:1, 6  ) = [-162.0, 0.153]
  cd(0:1, 7  ) = [-159.0, 0.1917]
  cd(0:1, 8  ) = [-156.0, 0.2496]
  cd(0:1, 9  ) = [-153.0, 0.3075]
  cd(0:1, 10  ) = [-150.0, 0.3654]
  cd(0:1, 11  ) = [-147.0, 0.4365]
  cd(0:1, 12  ) = [-144.0, 0.5076]
  cd(0:1, 13  ) = [-141.0, 0.5786]
  cd(0:1, 14  ) = [-138.0, 0.6527]
  cd(0:1, 15  ) = [-135.0, 0.7283]
  cd(0:1, 16  ) = [-132.0, 0.8039]
  cd(0:1, 17  ) = [-129.0, 0.878]
  cd(0:1, 18  ) = [-126.0, 0.949]
  cd(0:1, 19  ) = [-123.0, 1.02]
  cd(0:1, 20  ) = [-120.0, 1.0911]
  cd(0:1, 21  ) = [-117.0, 1.1489]
  cd(0:1, 22  ) = [-114.0, 1.2068]
  cd(0:1, 23  ) = [-111.0, 1.2647]
  cd(0:1, 24  ) = [-108.0, 1.3091]
  cd(0:1, 25  ) = [-105.0, 1.3468]
  cd(0:1, 26  ) = [-102.0, 1.3845]
  cd(0:1, 27  ) = [-99.0, 1.414]
  cd(0:1, 28  ) = [-96.0, 1.427]
  cd(0:1, 29  ) = [-93.0, 1.44]
  cd(0:1, 30  ) = [-90.0, 1.4531]
  cd(0:1, 31  ) = [-87.0, 1.44]
  cd(0:1, 32  ) = [-84.0, 1.427]
  cd(0:1, 33  ) = [-81.0, 1.414]
  cd(0:1, 34  ) = [-78.0, 1.3845]
  cd(0:1, 35  ) = [-75.0, 1.3468]
  cd(0:1, 36  ) = [-72.0, 1.3091]
  cd(0:1, 37  ) = [-69.0, 1.2647]
  cd(0:1, 38  ) = [-66.0, 1.2068]
  cd(0:1, 39  ) = [-63.0, 1.1489]
  cd(0:1, 40  ) = [-60.0, 1.0911]
  cd(0:1, 41  ) = [-57.0, 1.02]
  cd(0:1, 42  ) = [-54.0, 0.949]
  cd(0:1, 43  ) = [-51.0, 0.878]
  cd(0:1, 44  ) = [-48.0, 0.8039]
  cd(0:1, 45  ) = [-45.0, 0.7283]
  cd(0:1, 46  ) = [-42.0, 0.6527]
  cd(0:1, 47  ) = [-39.0, 0.5775]
  cd(0:1, 48  ) = [-36.0, 0.5041]
  cd(0:1, 49  ) = [-33.0, 0.4333]
  cd(0:1, 50  ) = [-30.0, 0.3654]
  cd(0:1, 51  ) = [-29.3939, 0.3524]
  cd(0:1, 52  ) = [-28.7879, 0.3394]
  cd(0:1, 53  ) = [-28.1818, 0.3264]
  cd(0:1, 54  ) = [-27.5758, 0.3138]
  cd(0:1, 55  ) = [-26.9697, 0.3014]
  cd(0:1, 56  ) = [-26.3636, 0.289]
  cd(0:1, 57  ) = [-25.7576, 0.2768]
  cd(0:1, 58  ) = [-25.1515, 0.265]
  cd(0:1, 59  ) = [-24.5455, 0.2533]
  cd(0:1, 60  ) = [-23.9394, 0.2416]
  cd(0:1, 61  ) = [-23.3333, 0.2305]
  cd(0:1, 62  ) = [-22.7273, 0.2195]
  cd(0:1, 63  ) = [-22.1212, 0.2085]
  cd(0:1, 64  ) = [-21.5152, 0.198]
  cd(0:1, 65  ) = [-20.9091, 0.1878]
  cd(0:1, 66  ) = [-20.303, 0.1775]
  cd(0:1, 67  ) = [-19.697, 0.1675]
  cd(0:1, 68  ) = [-19.0909, 0.1579]
  cd(0:1, 69  ) = [-18.4848, 0.1485]
  cd(0:1, 70  ) = [-17.8788, 0.1396]
  cd(0:1, 71  ) = [-17.2727, 0.1316]
  cd(0:1, 72  ) = [-16.6667, 0.1236]
  cd(0:1, 73  ) = [-16.0606, 0.1156]
  cd(0:1, 74  ) = [-15.4545, 0.1076]
  cd(0:1, 75  ) = [-14.8485, 0.0996]
  cd(0:1, 76  ) = [-14.2424, 0.0916]
  cd(0:1, 77  ) = [-13.6364, 0.0836]
  cd(0:1, 78  ) = [-13.0303, 0.0756]
  cd(0:1, 79  ) = [-12.4242, 0.0676]
  cd(0:1, 80  ) = [-11.8182, 0.0596]
  cd(0:1, 81  ) = [-11.2121, 0.0516]
  cd(0:1, 82  ) = [-10.6061, 0.0436]
  cd(0:1, 83  ) = [-10.0, 0.0356]
  cd(0:1, 84  ) = [-9.3939, 0.0276]
  cd(0:1, 85  ) = [-8.7879, 0.0201]
  cd(0:1, 86  ) = [-8.1818, 0.0136]
  cd(0:1, 87  ) = [-7.5758, 0.0104]
  cd(0:1, 88  ) = [-6.9697, 0.0089]
  cd(0:1, 89  ) = [-6.3636, 0.0075]
  cd(0:1, 90  ) = [-5.7576, 0.0071]
  cd(0:1, 91  ) = [-5.1515, 0.0068]
  cd(0:1, 92  ) = [-4.5455, 0.0065]
  cd(0:1, 93  ) = [-3.9394, 0.0064]
  cd(0:1, 94  ) = [-3.3333, 0.0064]
  cd(0:1, 95  ) = [-2.7273, 0.0063]
  cd(0:1, 96  ) = [-2.1212, 0.0063]
  cd(0:1, 97  ) = [-1.5152, 0.0063]
  cd(0:1, 98  ) = [-0.9091, 0.0063]
  cd(0:1, 99  ) = [-0.303, 0.0063]
  cd(0:1, 100  ) = [0.303, 0.0064]
  cd(0:1, 101  ) = [0.9091, 0.0064]
  cd(0:1, 102  ) = [1.5152, 0.0065]
  cd(0:1, 103  ) = [2.1212, 0.0065]
  cd(0:1, 104  ) = [2.7273, 0.0066]
  cd(0:1, 105  ) = [3.3333, 0.0068]
  cd(0:1, 106  ) = [3.9394, 0.0069]
  cd(0:1, 107  ) = [4.5455, 0.007]
  cd(0:1, 108  ) = [5.1515, 0.0072]
  cd(0:1, 109  ) = [5.7576, 0.0075]
  cd(0:1, 110  ) = [6.3636, 0.008]
  cd(0:1, 111  ) = [6.9697, 0.0089]
  cd(0:1, 112  ) = [7.5758, 0.0107]
  cd(0:1, 113  ) = [8.1818, 0.0125]
  cd(0:1, 114  ) = [8.7879, 0.014]
  cd(0:1, 115  ) = [9.3939, 0.0154]
  cd(0:1, 116  ) = [10.0, 0.0165]
  cd(0:1, 117  ) = [10.6061, 0.0182]
  cd(0:1, 118  ) = [11.2121, 0.0203]
  cd(0:1, 119  ) = [11.8182, 0.0301]
  cd(0:1, 120  ) = [12.4242, 0.0465]
  cd(0:1, 121  ) = [13.0303, 0.0628]
  cd(0:1, 122  ) = [13.6364, 0.0792]
  cd(0:1, 123  ) = [14.2424, 0.0923]
  cd(0:1, 124  ) = [14.8485, 0.1005]
  cd(0:1, 125  ) = [15.4545, 0.1086]
  cd(0:1, 126  ) = [16.0606, 0.1168]
  cd(0:1, 127  ) = [16.6667, 0.125]
  cd(0:1, 128  ) = [17.2727, 0.1332]
  cd(0:1, 129  ) = [17.8788, 0.1414]
  cd(0:1, 130  ) = [18.4848, 0.1495]
  cd(0:1, 131  ) = [19.0909, 0.1579]
  cd(0:1, 132  ) = [19.697, 0.1675]
  cd(0:1, 133  ) = [20.303, 0.1775]
  cd(0:1, 134  ) = [20.9091, 0.1878]
  cd(0:1, 135  ) = [21.5152, 0.198]
  cd(0:1, 136  ) = [22.1212, 0.2085]
  cd(0:1, 137  ) = [22.7273, 0.2195]
  cd(0:1, 138  ) = [23.3333, 0.2305]
  cd(0:1, 139  ) = [23.9394, 0.2416]
  cd(0:1, 140  ) = [24.5455, 0.2533]
  cd(0:1, 141  ) = [25.1515, 0.265]
  cd(0:1, 142  ) = [25.7576, 0.2768]
  cd(0:1, 143  ) = [26.3636, 0.289]
  cd(0:1, 144  ) = [26.9697, 0.3014]
  cd(0:1, 145  ) = [27.5758, 0.3138]
  cd(0:1, 146  ) = [28.1818, 0.3264]
  cd(0:1, 147  ) = [28.7879, 0.3394]
  cd(0:1, 148  ) = [29.3939, 0.3524]
  cd(0:1, 149  ) = [30.0, 0.3654]
  cd(0:1, 150  ) = [33.0, 0.4333]
  cd(0:1, 151  ) = [36.0, 0.5041]
  cd(0:1, 152  ) = [39.0, 0.5775]
  cd(0:1, 153  ) = [42.0, 0.6527]
  cd(0:1, 154  ) = [45.0, 0.7283]
  cd(0:1, 155  ) = [48.0, 0.8039]
  cd(0:1, 156  ) = [51.0, 0.878]
  cd(0:1, 157  ) = [54.0, 0.949]
  cd(0:1, 158  ) = [57.0, 1.02]
  cd(0:1, 159  ) = [60.0, 1.0911]
  cd(0:1, 160  ) = [63.0, 1.1489]
  cd(0:1, 161  ) = [66.0, 1.2068]
  cd(0:1, 162  ) = [69.0, 1.2647]
  cd(0:1, 163  ) = [72.0, 1.3091]
  cd(0:1, 164  ) = [75.0, 1.3468]
  cd(0:1, 165  ) = [78.0, 1.3845]
  cd(0:1, 166  ) = [81.0, 1.414]
  cd(0:1, 167  ) = [84.0, 1.427]
  cd(0:1, 168  ) = [87.0, 1.44]
  cd(0:1, 169  ) = [90.0, 1.4531]
  cd(0:1, 170  ) = [93.0, 1.44]
  cd(0:1, 171  ) = [96.0, 1.427]
  cd(0:1, 172  ) = [99.0, 1.414]
  cd(0:1, 173  ) = [102.0, 1.3845]
  cd(0:1, 174  ) = [105.0, 1.3468]
  cd(0:1, 175  ) = [108.0, 1.3091]
  cd(0:1, 176  ) = [111.0, 1.2647]
  cd(0:1, 177  ) = [114.0, 1.2068]
  cd(0:1, 178  ) = [117.0, 1.1489]
  cd(0:1, 179  ) = [120.0, 1.0911]
  cd(0:1, 180  ) = [123.0, 1.02]
  cd(0:1, 181  ) = [126.0, 0.949]
  cd(0:1, 182  ) = [129.0, 0.878]
  cd(0:1, 183  ) = [132.0, 0.8039]
  cd(0:1, 184  ) = [135.0, 0.7283]
  cd(0:1, 185  ) = [138.0, 0.6527]
  cd(0:1, 186  ) = [141.0, 0.5786]
  cd(0:1, 187  ) = [144.0, 0.5076]
  cd(0:1, 188  ) = [147.0, 0.4365]
  cd(0:1, 189  ) = [150.0, 0.3654]
  cd(0:1, 190  ) = [153.0, 0.3075]
  cd(0:1, 191  ) = [156.0, 0.2496]
  cd(0:1, 192  ) = [159.0, 0.1917]
  cd(0:1, 193  ) = [162.0, 0.1536]
  cd(0:1, 194  ) = [165.0, 0.1254]
  cd(0:1, 195  ) = [168.0, 0.0972]
  cd(0:1, 196  ) = [171.0, 0.0706]
  cd(0:1, 197  ) = [174.0, 0.0471]
  cd(0:1, 198  ) = [177.0, 0.0386]
  cd(0:1, 199  ) = [180.0, 0.0377]

  IF ( y .EQ. cd(0,0) ) cd27 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd27 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd27 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd27_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd27 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd27_nrel2p8.pro' 
  ENDIF

!print,y,cd27

END FUNCTION calc_cd27_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd28_nrel2p8( y ) RESULT( cd28 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd28
INTEGER :: i

 cd28 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.0377]
  cd(0:1, 1  ) = [-177.0, 0.0377]
  cd(0:1, 2  ) = [-174.0, 0.0452]
  cd(0:1, 3  ) = [-171.0, 0.0679]
  cd(0:1, 4  ) = [-168.0, 0.0948]
  cd(0:1, 5  ) = [-165.0, 0.1239]
  cd(0:1, 6  ) = [-162.0, 0.153]
  cd(0:1, 7  ) = [-159.0, 0.1917]
  cd(0:1, 8  ) = [-156.0, 0.2496]
  cd(0:1, 9  ) = [-153.0, 0.3075]
  cd(0:1, 10  ) = [-150.0, 0.3654]
  cd(0:1, 11  ) = [-147.0, 0.4365]
  cd(0:1, 12  ) = [-144.0, 0.5076]
  cd(0:1, 13  ) = [-141.0, 0.5786]
  cd(0:1, 14  ) = [-138.0, 0.6527]
  cd(0:1, 15  ) = [-135.0, 0.7283]
  cd(0:1, 16  ) = [-132.0, 0.8039]
  cd(0:1, 17  ) = [-129.0, 0.878]
  cd(0:1, 18  ) = [-126.0, 0.949]
  cd(0:1, 19  ) = [-123.0, 1.02]
  cd(0:1, 20  ) = [-120.0, 1.0911]
  cd(0:1, 21  ) = [-117.0, 1.1489]
  cd(0:1, 22  ) = [-114.0, 1.2068]
  cd(0:1, 23  ) = [-111.0, 1.2647]
  cd(0:1, 24  ) = [-108.0, 1.3091]
  cd(0:1, 25  ) = [-105.0, 1.3468]
  cd(0:1, 26  ) = [-102.0, 1.3845]
  cd(0:1, 27  ) = [-99.0, 1.414]
  cd(0:1, 28  ) = [-96.0, 1.427]
  cd(0:1, 29  ) = [-93.0, 1.44]
  cd(0:1, 30  ) = [-90.0, 1.4531]
  cd(0:1, 31  ) = [-87.0, 1.44]
  cd(0:1, 32  ) = [-84.0, 1.427]
  cd(0:1, 33  ) = [-81.0, 1.414]
  cd(0:1, 34  ) = [-78.0, 1.3845]
  cd(0:1, 35  ) = [-75.0, 1.3468]
  cd(0:1, 36  ) = [-72.0, 1.3091]
  cd(0:1, 37  ) = [-69.0, 1.2647]
  cd(0:1, 38  ) = [-66.0, 1.2068]
  cd(0:1, 39  ) = [-63.0, 1.1489]
  cd(0:1, 40  ) = [-60.0, 1.0911]
  cd(0:1, 41  ) = [-57.0, 1.02]
  cd(0:1, 42  ) = [-54.0, 0.949]
  cd(0:1, 43  ) = [-51.0, 0.878]
  cd(0:1, 44  ) = [-48.0, 0.8039]
  cd(0:1, 45  ) = [-45.0, 0.7283]
  cd(0:1, 46  ) = [-42.0, 0.6527]
  cd(0:1, 47  ) = [-39.0, 0.5775]
  cd(0:1, 48  ) = [-36.0, 0.5041]
  cd(0:1, 49  ) = [-33.0, 0.4333]
  cd(0:1, 50  ) = [-30.0, 0.3654]
  cd(0:1, 51  ) = [-29.3939, 0.3524]
  cd(0:1, 52  ) = [-28.7879, 0.3394]
  cd(0:1, 53  ) = [-28.1818, 0.3264]
  cd(0:1, 54  ) = [-27.5758, 0.3138]
  cd(0:1, 55  ) = [-26.9697, 0.3014]
  cd(0:1, 56  ) = [-26.3636, 0.289]
  cd(0:1, 57  ) = [-25.7576, 0.2768]
  cd(0:1, 58  ) = [-25.1515, 0.265]
  cd(0:1, 59  ) = [-24.5455, 0.2533]
  cd(0:1, 60  ) = [-23.9394, 0.2416]
  cd(0:1, 61  ) = [-23.3333, 0.2305]
  cd(0:1, 62  ) = [-22.7273, 0.2195]
  cd(0:1, 63  ) = [-22.1212, 0.2085]
  cd(0:1, 64  ) = [-21.5152, 0.198]
  cd(0:1, 65  ) = [-20.9091, 0.1878]
  cd(0:1, 66  ) = [-20.303, 0.1775]
  cd(0:1, 67  ) = [-19.697, 0.1675]
  cd(0:1, 68  ) = [-19.0909, 0.1579]
  cd(0:1, 69  ) = [-18.4848, 0.1485]
  cd(0:1, 70  ) = [-17.8788, 0.1396]
  cd(0:1, 71  ) = [-17.2727, 0.1316]
  cd(0:1, 72  ) = [-16.6667, 0.1236]
  cd(0:1, 73  ) = [-16.0606, 0.1156]
  cd(0:1, 74  ) = [-15.4545, 0.1076]
  cd(0:1, 75  ) = [-14.8485, 0.0996]
  cd(0:1, 76  ) = [-14.2424, 0.0916]
  cd(0:1, 77  ) = [-13.6364, 0.0836]
  cd(0:1, 78  ) = [-13.0303, 0.0756]
  cd(0:1, 79  ) = [-12.4242, 0.0676]
  cd(0:1, 80  ) = [-11.8182, 0.0596]
  cd(0:1, 81  ) = [-11.2121, 0.0516]
  cd(0:1, 82  ) = [-10.6061, 0.0436]
  cd(0:1, 83  ) = [-10.0, 0.0356]
  cd(0:1, 84  ) = [-9.3939, 0.0276]
  cd(0:1, 85  ) = [-8.7879, 0.0201]
  cd(0:1, 86  ) = [-8.1818, 0.0136]
  cd(0:1, 87  ) = [-7.5758, 0.0104]
  cd(0:1, 88  ) = [-6.9697, 0.0089]
  cd(0:1, 89  ) = [-6.3636, 0.0075]
  cd(0:1, 90  ) = [-5.7576, 0.0071]
  cd(0:1, 91  ) = [-5.1515, 0.0068]
  cd(0:1, 92  ) = [-4.5455, 0.0065]
  cd(0:1, 93  ) = [-3.9394, 0.0064]
  cd(0:1, 94  ) = [-3.3333, 0.0064]
  cd(0:1, 95  ) = [-2.7273, 0.0063]
  cd(0:1, 96  ) = [-2.1212, 0.0063]
  cd(0:1, 97  ) = [-1.5152, 0.0063]
  cd(0:1, 98  ) = [-0.9091, 0.0063]
  cd(0:1, 99  ) = [-0.303, 0.0063]
  cd(0:1, 100  ) = [0.303, 0.0064]
  cd(0:1, 101  ) = [0.9091, 0.0064]
  cd(0:1, 102  ) = [1.5152, 0.0065]
  cd(0:1, 103  ) = [2.1212, 0.0065]
  cd(0:1, 104  ) = [2.7273, 0.0066]
  cd(0:1, 105  ) = [3.3333, 0.0068]
  cd(0:1, 106  ) = [3.9394, 0.0069]
  cd(0:1, 107  ) = [4.5455, 0.007]
  cd(0:1, 108  ) = [5.1515, 0.0072]
  cd(0:1, 109  ) = [5.7576, 0.0075]
  cd(0:1, 110  ) = [6.3636, 0.008]
  cd(0:1, 111  ) = [6.9697, 0.0089]
  cd(0:1, 112  ) = [7.5758, 0.0107]
  cd(0:1, 113  ) = [8.1818, 0.0125]
  cd(0:1, 114  ) = [8.7879, 0.014]
  cd(0:1, 115  ) = [9.3939, 0.0154]
  cd(0:1, 116  ) = [10.0, 0.0165]
  cd(0:1, 117  ) = [10.6061, 0.0182]
  cd(0:1, 118  ) = [11.2121, 0.0203]
  cd(0:1, 119  ) = [11.8182, 0.0301]
  cd(0:1, 120  ) = [12.4242, 0.0465]
  cd(0:1, 121  ) = [13.0303, 0.0628]
  cd(0:1, 122  ) = [13.6364, 0.0792]
  cd(0:1, 123  ) = [14.2424, 0.0923]
  cd(0:1, 124  ) = [14.8485, 0.1005]
  cd(0:1, 125  ) = [15.4545, 0.1086]
  cd(0:1, 126  ) = [16.0606, 0.1168]
  cd(0:1, 127  ) = [16.6667, 0.125]
  cd(0:1, 128  ) = [17.2727, 0.1332]
  cd(0:1, 129  ) = [17.8788, 0.1414]
  cd(0:1, 130  ) = [18.4848, 0.1495]
  cd(0:1, 131  ) = [19.0909, 0.1579]
  cd(0:1, 132  ) = [19.697, 0.1675]
  cd(0:1, 133  ) = [20.303, 0.1775]
  cd(0:1, 134  ) = [20.9091, 0.1878]
  cd(0:1, 135  ) = [21.5152, 0.198]
  cd(0:1, 136  ) = [22.1212, 0.2085]
  cd(0:1, 137  ) = [22.7273, 0.2195]
  cd(0:1, 138  ) = [23.3333, 0.2305]
  cd(0:1, 139  ) = [23.9394, 0.2416]
  cd(0:1, 140  ) = [24.5455, 0.2533]
  cd(0:1, 141  ) = [25.1515, 0.265]
  cd(0:1, 142  ) = [25.7576, 0.2768]
  cd(0:1, 143  ) = [26.3636, 0.289]
  cd(0:1, 144  ) = [26.9697, 0.3014]
  cd(0:1, 145  ) = [27.5758, 0.3138]
  cd(0:1, 146  ) = [28.1818, 0.3264]
  cd(0:1, 147  ) = [28.7879, 0.3394]
  cd(0:1, 148  ) = [29.3939, 0.3524]
  cd(0:1, 149  ) = [30.0, 0.3654]
  cd(0:1, 150  ) = [33.0, 0.4333]
  cd(0:1, 151  ) = [36.0, 0.5041]
  cd(0:1, 152  ) = [39.0, 0.5775]
  cd(0:1, 153  ) = [42.0, 0.6527]
  cd(0:1, 154  ) = [45.0, 0.7283]
  cd(0:1, 155  ) = [48.0, 0.8039]
  cd(0:1, 156  ) = [51.0, 0.878]
  cd(0:1, 157  ) = [54.0, 0.949]
  cd(0:1, 158  ) = [57.0, 1.02]
  cd(0:1, 159  ) = [60.0, 1.0911]
  cd(0:1, 160  ) = [63.0, 1.1489]
  cd(0:1, 161  ) = [66.0, 1.2068]
  cd(0:1, 162  ) = [69.0, 1.2647]
  cd(0:1, 163  ) = [72.0, 1.3091]
  cd(0:1, 164  ) = [75.0, 1.3468]
  cd(0:1, 165  ) = [78.0, 1.3845]
  cd(0:1, 166  ) = [81.0, 1.414]
  cd(0:1, 167  ) = [84.0, 1.427]
  cd(0:1, 168  ) = [87.0, 1.44]
  cd(0:1, 169  ) = [90.0, 1.4531]
  cd(0:1, 170  ) = [93.0, 1.44]
  cd(0:1, 171  ) = [96.0, 1.427]
  cd(0:1, 172  ) = [99.0, 1.414]
  cd(0:1, 173  ) = [102.0, 1.3845]
  cd(0:1, 174  ) = [105.0, 1.3468]
  cd(0:1, 175  ) = [108.0, 1.3091]
  cd(0:1, 176  ) = [111.0, 1.2647]
  cd(0:1, 177  ) = [114.0, 1.2068]
  cd(0:1, 178  ) = [117.0, 1.1489]
  cd(0:1, 179  ) = [120.0, 1.0911]
  cd(0:1, 180  ) = [123.0, 1.02]
  cd(0:1, 181  ) = [126.0, 0.949]
  cd(0:1, 182  ) = [129.0, 0.878]
  cd(0:1, 183  ) = [132.0, 0.8039]
  cd(0:1, 184  ) = [135.0, 0.7283]
  cd(0:1, 185  ) = [138.0, 0.6527]
  cd(0:1, 186  ) = [141.0, 0.5786]
  cd(0:1, 187  ) = [144.0, 0.5076]
  cd(0:1, 188  ) = [147.0, 0.4365]
  cd(0:1, 189  ) = [150.0, 0.3654]
  cd(0:1, 190  ) = [153.0, 0.3075]
  cd(0:1, 191  ) = [156.0, 0.2496]
  cd(0:1, 192  ) = [159.0, 0.1917]
  cd(0:1, 193  ) = [162.0, 0.1536]
  cd(0:1, 194  ) = [165.0, 0.1254]
  cd(0:1, 195  ) = [168.0, 0.0972]
  cd(0:1, 196  ) = [171.0, 0.0706]
  cd(0:1, 197  ) = [174.0, 0.0471]
  cd(0:1, 198  ) = [177.0, 0.0386]
  cd(0:1, 199  ) = [180.0, 0.0377]

  IF ( y .EQ. cd(0,0) ) cd28 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd28 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd28 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd28_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd28 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd28_nrel2p8.pro' 
  ENDIF

!print,y,cd28

END FUNCTION calc_cd28_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd29_nrel2p8( y ) RESULT( cd29 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd29
INTEGER :: i

 cd29 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.0377]
  cd(0:1, 1  ) = [-177.0, 0.0377]
  cd(0:1, 2  ) = [-174.0, 0.0452]
  cd(0:1, 3  ) = [-171.0, 0.0679]
  cd(0:1, 4  ) = [-168.0, 0.0948]
  cd(0:1, 5  ) = [-165.0, 0.1239]
  cd(0:1, 6  ) = [-162.0, 0.153]
  cd(0:1, 7  ) = [-159.0, 0.1917]
  cd(0:1, 8  ) = [-156.0, 0.2496]
  cd(0:1, 9  ) = [-153.0, 0.3075]
  cd(0:1, 10  ) = [-150.0, 0.3654]
  cd(0:1, 11  ) = [-147.0, 0.4365]
  cd(0:1, 12  ) = [-144.0, 0.5076]
  cd(0:1, 13  ) = [-141.0, 0.5786]
  cd(0:1, 14  ) = [-138.0, 0.6527]
  cd(0:1, 15  ) = [-135.0, 0.7283]
  cd(0:1, 16  ) = [-132.0, 0.8039]
  cd(0:1, 17  ) = [-129.0, 0.878]
  cd(0:1, 18  ) = [-126.0, 0.949]
  cd(0:1, 19  ) = [-123.0, 1.02]
  cd(0:1, 20  ) = [-120.0, 1.0911]
  cd(0:1, 21  ) = [-117.0, 1.1489]
  cd(0:1, 22  ) = [-114.0, 1.2068]
  cd(0:1, 23  ) = [-111.0, 1.2647]
  cd(0:1, 24  ) = [-108.0, 1.3091]
  cd(0:1, 25  ) = [-105.0, 1.3468]
  cd(0:1, 26  ) = [-102.0, 1.3845]
  cd(0:1, 27  ) = [-99.0, 1.414]
  cd(0:1, 28  ) = [-96.0, 1.427]
  cd(0:1, 29  ) = [-93.0, 1.44]
  cd(0:1, 30  ) = [-90.0, 1.4531]
  cd(0:1, 31  ) = [-87.0, 1.44]
  cd(0:1, 32  ) = [-84.0, 1.427]
  cd(0:1, 33  ) = [-81.0, 1.414]
  cd(0:1, 34  ) = [-78.0, 1.3845]
  cd(0:1, 35  ) = [-75.0, 1.3468]
  cd(0:1, 36  ) = [-72.0, 1.3091]
  cd(0:1, 37  ) = [-69.0, 1.2647]
  cd(0:1, 38  ) = [-66.0, 1.2068]
  cd(0:1, 39  ) = [-63.0, 1.1489]
  cd(0:1, 40  ) = [-60.0, 1.0911]
  cd(0:1, 41  ) = [-57.0, 1.02]
  cd(0:1, 42  ) = [-54.0, 0.949]
  cd(0:1, 43  ) = [-51.0, 0.878]
  cd(0:1, 44  ) = [-48.0, 0.8039]
  cd(0:1, 45  ) = [-45.0, 0.7283]
  cd(0:1, 46  ) = [-42.0, 0.6527]
  cd(0:1, 47  ) = [-39.0, 0.5775]
  cd(0:1, 48  ) = [-36.0, 0.5041]
  cd(0:1, 49  ) = [-33.0, 0.4333]
  cd(0:1, 50  ) = [-30.0, 0.3654]
  cd(0:1, 51  ) = [-29.3939, 0.3524]
  cd(0:1, 52  ) = [-28.7879, 0.3394]
  cd(0:1, 53  ) = [-28.1818, 0.3264]
  cd(0:1, 54  ) = [-27.5758, 0.3138]
  cd(0:1, 55  ) = [-26.9697, 0.3014]
  cd(0:1, 56  ) = [-26.3636, 0.289]
  cd(0:1, 57  ) = [-25.7576, 0.2768]
  cd(0:1, 58  ) = [-25.1515, 0.265]
  cd(0:1, 59  ) = [-24.5455, 0.2533]
  cd(0:1, 60  ) = [-23.9394, 0.2416]
  cd(0:1, 61  ) = [-23.3333, 0.2305]
  cd(0:1, 62  ) = [-22.7273, 0.2195]
  cd(0:1, 63  ) = [-22.1212, 0.2085]
  cd(0:1, 64  ) = [-21.5152, 0.198]
  cd(0:1, 65  ) = [-20.9091, 0.1878]
  cd(0:1, 66  ) = [-20.303, 0.1775]
  cd(0:1, 67  ) = [-19.697, 0.1675]
  cd(0:1, 68  ) = [-19.0909, 0.1579]
  cd(0:1, 69  ) = [-18.4848, 0.1485]
  cd(0:1, 70  ) = [-17.8788, 0.1396]
  cd(0:1, 71  ) = [-17.2727, 0.1316]
  cd(0:1, 72  ) = [-16.6667, 0.1236]
  cd(0:1, 73  ) = [-16.0606, 0.1156]
  cd(0:1, 74  ) = [-15.4545, 0.1076]
  cd(0:1, 75  ) = [-14.8485, 0.0996]
  cd(0:1, 76  ) = [-14.2424, 0.0916]
  cd(0:1, 77  ) = [-13.6364, 0.0836]
  cd(0:1, 78  ) = [-13.0303, 0.0756]
  cd(0:1, 79  ) = [-12.4242, 0.0676]
  cd(0:1, 80  ) = [-11.8182, 0.0596]
  cd(0:1, 81  ) = [-11.2121, 0.0516]
  cd(0:1, 82  ) = [-10.6061, 0.0436]
  cd(0:1, 83  ) = [-10.0, 0.0356]
  cd(0:1, 84  ) = [-9.3939, 0.0276]
  cd(0:1, 85  ) = [-8.7879, 0.0201]
  cd(0:1, 86  ) = [-8.1818, 0.0136]
  cd(0:1, 87  ) = [-7.5758, 0.0104]
  cd(0:1, 88  ) = [-6.9697, 0.0089]
  cd(0:1, 89  ) = [-6.3636, 0.0075]
  cd(0:1, 90  ) = [-5.7576, 0.0071]
  cd(0:1, 91  ) = [-5.1515, 0.0068]
  cd(0:1, 92  ) = [-4.5455, 0.0065]
  cd(0:1, 93  ) = [-3.9394, 0.0064]
  cd(0:1, 94  ) = [-3.3333, 0.0064]
  cd(0:1, 95  ) = [-2.7273, 0.0063]
  cd(0:1, 96  ) = [-2.1212, 0.0063]
  cd(0:1, 97  ) = [-1.5152, 0.0063]
  cd(0:1, 98  ) = [-0.9091, 0.0063]
  cd(0:1, 99  ) = [-0.303, 0.0063]
  cd(0:1, 100  ) = [0.303, 0.0064]
  cd(0:1, 101  ) = [0.9091, 0.0064]
  cd(0:1, 102  ) = [1.5152, 0.0065]
  cd(0:1, 103  ) = [2.1212, 0.0065]
  cd(0:1, 104  ) = [2.7273, 0.0066]
  cd(0:1, 105  ) = [3.3333, 0.0068]
  cd(0:1, 106  ) = [3.9394, 0.0069]
  cd(0:1, 107  ) = [4.5455, 0.007]
  cd(0:1, 108  ) = [5.1515, 0.0072]
  cd(0:1, 109  ) = [5.7576, 0.0075]
  cd(0:1, 110  ) = [6.3636, 0.008]
  cd(0:1, 111  ) = [6.9697, 0.0089]
  cd(0:1, 112  ) = [7.5758, 0.0107]
  cd(0:1, 113  ) = [8.1818, 0.0125]
  cd(0:1, 114  ) = [8.7879, 0.014]
  cd(0:1, 115  ) = [9.3939, 0.0154]
  cd(0:1, 116  ) = [10.0, 0.0165]
  cd(0:1, 117  ) = [10.6061, 0.0182]
  cd(0:1, 118  ) = [11.2121, 0.0203]
  cd(0:1, 119  ) = [11.8182, 0.0301]
  cd(0:1, 120  ) = [12.4242, 0.0465]
  cd(0:1, 121  ) = [13.0303, 0.0628]
  cd(0:1, 122  ) = [13.6364, 0.0792]
  cd(0:1, 123  ) = [14.2424, 0.0923]
  cd(0:1, 124  ) = [14.8485, 0.1005]
  cd(0:1, 125  ) = [15.4545, 0.1086]
  cd(0:1, 126  ) = [16.0606, 0.1168]
  cd(0:1, 127  ) = [16.6667, 0.125]
  cd(0:1, 128  ) = [17.2727, 0.1332]
  cd(0:1, 129  ) = [17.8788, 0.1414]
  cd(0:1, 130  ) = [18.4848, 0.1495]
  cd(0:1, 131  ) = [19.0909, 0.1579]
  cd(0:1, 132  ) = [19.697, 0.1675]
  cd(0:1, 133  ) = [20.303, 0.1775]
  cd(0:1, 134  ) = [20.9091, 0.1878]
  cd(0:1, 135  ) = [21.5152, 0.198]
  cd(0:1, 136  ) = [22.1212, 0.2085]
  cd(0:1, 137  ) = [22.7273, 0.2195]
  cd(0:1, 138  ) = [23.3333, 0.2305]
  cd(0:1, 139  ) = [23.9394, 0.2416]
  cd(0:1, 140  ) = [24.5455, 0.2533]
  cd(0:1, 141  ) = [25.1515, 0.265]
  cd(0:1, 142  ) = [25.7576, 0.2768]
  cd(0:1, 143  ) = [26.3636, 0.289]
  cd(0:1, 144  ) = [26.9697, 0.3014]
  cd(0:1, 145  ) = [27.5758, 0.3138]
  cd(0:1, 146  ) = [28.1818, 0.3264]
  cd(0:1, 147  ) = [28.7879, 0.3394]
  cd(0:1, 148  ) = [29.3939, 0.3524]
  cd(0:1, 149  ) = [30.0, 0.3654]
  cd(0:1, 150  ) = [33.0, 0.4333]
  cd(0:1, 151  ) = [36.0, 0.5041]
  cd(0:1, 152  ) = [39.0, 0.5775]
  cd(0:1, 153  ) = [42.0, 0.6527]
  cd(0:1, 154  ) = [45.0, 0.7283]
  cd(0:1, 155  ) = [48.0, 0.8039]
  cd(0:1, 156  ) = [51.0, 0.878]
  cd(0:1, 157  ) = [54.0, 0.949]
  cd(0:1, 158  ) = [57.0, 1.02]
  cd(0:1, 159  ) = [60.0, 1.0911]
  cd(0:1, 160  ) = [63.0, 1.1489]
  cd(0:1, 161  ) = [66.0, 1.2068]
  cd(0:1, 162  ) = [69.0, 1.2647]
  cd(0:1, 163  ) = [72.0, 1.3091]
  cd(0:1, 164  ) = [75.0, 1.3468]
  cd(0:1, 165  ) = [78.0, 1.3845]
  cd(0:1, 166  ) = [81.0, 1.414]
  cd(0:1, 167  ) = [84.0, 1.427]
  cd(0:1, 168  ) = [87.0, 1.44]
  cd(0:1, 169  ) = [90.0, 1.4531]
  cd(0:1, 170  ) = [93.0, 1.44]
  cd(0:1, 171  ) = [96.0, 1.427]
  cd(0:1, 172  ) = [99.0, 1.414]
  cd(0:1, 173  ) = [102.0, 1.3845]
  cd(0:1, 174  ) = [105.0, 1.3468]
  cd(0:1, 175  ) = [108.0, 1.3091]
  cd(0:1, 176  ) = [111.0, 1.2647]
  cd(0:1, 177  ) = [114.0, 1.2068]
  cd(0:1, 178  ) = [117.0, 1.1489]
  cd(0:1, 179  ) = [120.0, 1.0911]
  cd(0:1, 180  ) = [123.0, 1.02]
  cd(0:1, 181  ) = [126.0, 0.949]
  cd(0:1, 182  ) = [129.0, 0.878]
  cd(0:1, 183  ) = [132.0, 0.8039]
  cd(0:1, 184  ) = [135.0, 0.7283]
  cd(0:1, 185  ) = [138.0, 0.6527]
  cd(0:1, 186  ) = [141.0, 0.5786]
  cd(0:1, 187  ) = [144.0, 0.5076]
  cd(0:1, 188  ) = [147.0, 0.4365]
  cd(0:1, 189  ) = [150.0, 0.3654]
  cd(0:1, 190  ) = [153.0, 0.3075]
  cd(0:1, 191  ) = [156.0, 0.2496]
  cd(0:1, 192  ) = [159.0, 0.1917]
  cd(0:1, 193  ) = [162.0, 0.1536]
  cd(0:1, 194  ) = [165.0, 0.1254]
  cd(0:1, 195  ) = [168.0, 0.0972]
  cd(0:1, 196  ) = [171.0, 0.0706]
  cd(0:1, 197  ) = [174.0, 0.0471]
  cd(0:1, 198  ) = [177.0, 0.0386]
  cd(0:1, 199  ) = [180.0, 0.0377]

  IF ( y .EQ. cd(0,0) ) cd29 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd29 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd29 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd29_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd29 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd29_nrel2p8.pro' 
  ENDIF

!print,y,cd29

END FUNCTION calc_cd29_nrel2p8
!_________________________________________________________________________








!_________________________________________________________________________
FUNCTION calc_cd30_nrel2p8( y ) RESULT( cd30 )
!_________________________________________________________________________

REAL, INTENT( IN ) :: y

INTEGER, PARAMETER :: na = 200

REAL, DIMENSION( 0:1,0:na-1 ) :: cd

REAL :: cd30
INTEGER :: i

 cd30 = 0.0

!                alpha    CD  
  cd(0:1, 0  ) = [-180.0, 0.0377]
  cd(0:1, 1  ) = [-177.0, 0.0377]
  cd(0:1, 2  ) = [-174.0, 0.0452]
  cd(0:1, 3  ) = [-171.0, 0.0679]
  cd(0:1, 4  ) = [-168.0, 0.0948]
  cd(0:1, 5  ) = [-165.0, 0.1239]
  cd(0:1, 6  ) = [-162.0, 0.153]
  cd(0:1, 7  ) = [-159.0, 0.1917]
  cd(0:1, 8  ) = [-156.0, 0.2496]
  cd(0:1, 9  ) = [-153.0, 0.3075]
  cd(0:1, 10  ) = [-150.0, 0.3654]
  cd(0:1, 11  ) = [-147.0, 0.4365]
  cd(0:1, 12  ) = [-144.0, 0.5076]
  cd(0:1, 13  ) = [-141.0, 0.5786]
  cd(0:1, 14  ) = [-138.0, 0.6527]
  cd(0:1, 15  ) = [-135.0, 0.7283]
  cd(0:1, 16  ) = [-132.0, 0.8039]
  cd(0:1, 17  ) = [-129.0, 0.878]
  cd(0:1, 18  ) = [-126.0, 0.949]
  cd(0:1, 19  ) = [-123.0, 1.02]
  cd(0:1, 20  ) = [-120.0, 1.0911]
  cd(0:1, 21  ) = [-117.0, 1.1489]
  cd(0:1, 22  ) = [-114.0, 1.2068]
  cd(0:1, 23  ) = [-111.0, 1.2647]
  cd(0:1, 24  ) = [-108.0, 1.3091]
  cd(0:1, 25  ) = [-105.0, 1.3468]
  cd(0:1, 26  ) = [-102.0, 1.3845]
  cd(0:1, 27  ) = [-99.0, 1.414]
  cd(0:1, 28  ) = [-96.0, 1.427]
  cd(0:1, 29  ) = [-93.0, 1.44]
  cd(0:1, 30  ) = [-90.0, 1.4531]
  cd(0:1, 31  ) = [-87.0, 1.44]
  cd(0:1, 32  ) = [-84.0, 1.427]
  cd(0:1, 33  ) = [-81.0, 1.414]
  cd(0:1, 34  ) = [-78.0, 1.3845]
  cd(0:1, 35  ) = [-75.0, 1.3468]
  cd(0:1, 36  ) = [-72.0, 1.3091]
  cd(0:1, 37  ) = [-69.0, 1.2647]
  cd(0:1, 38  ) = [-66.0, 1.2068]
  cd(0:1, 39  ) = [-63.0, 1.1489]
  cd(0:1, 40  ) = [-60.0, 1.0911]
  cd(0:1, 41  ) = [-57.0, 1.02]
  cd(0:1, 42  ) = [-54.0, 0.949]
  cd(0:1, 43  ) = [-51.0, 0.878]
  cd(0:1, 44  ) = [-48.0, 0.8039]
  cd(0:1, 45  ) = [-45.0, 0.7283]
  cd(0:1, 46  ) = [-42.0, 0.6527]
  cd(0:1, 47  ) = [-39.0, 0.5775]
  cd(0:1, 48  ) = [-36.0, 0.5041]
  cd(0:1, 49  ) = [-33.0, 0.4333]
  cd(0:1, 50  ) = [-30.0, 0.3654]
  cd(0:1, 51  ) = [-29.3939, 0.3524]
  cd(0:1, 52  ) = [-28.7879, 0.3394]
  cd(0:1, 53  ) = [-28.1818, 0.3264]
  cd(0:1, 54  ) = [-27.5758, 0.3138]
  cd(0:1, 55  ) = [-26.9697, 0.3014]
  cd(0:1, 56  ) = [-26.3636, 0.289]
  cd(0:1, 57  ) = [-25.7576, 0.2768]
  cd(0:1, 58  ) = [-25.1515, 0.265]
  cd(0:1, 59  ) = [-24.5455, 0.2533]
  cd(0:1, 60  ) = [-23.9394, 0.2416]
  cd(0:1, 61  ) = [-23.3333, 0.2305]
  cd(0:1, 62  ) = [-22.7273, 0.2195]
  cd(0:1, 63  ) = [-22.1212, 0.2085]
  cd(0:1, 64  ) = [-21.5152, 0.198]
  cd(0:1, 65  ) = [-20.9091, 0.1878]
  cd(0:1, 66  ) = [-20.303, 0.1775]
  cd(0:1, 67  ) = [-19.697, 0.1675]
  cd(0:1, 68  ) = [-19.0909, 0.1579]
  cd(0:1, 69  ) = [-18.4848, 0.1485]
  cd(0:1, 70  ) = [-17.8788, 0.1396]
  cd(0:1, 71  ) = [-17.2727, 0.1316]
  cd(0:1, 72  ) = [-16.6667, 0.1236]
  cd(0:1, 73  ) = [-16.0606, 0.1156]
  cd(0:1, 74  ) = [-15.4545, 0.1076]
  cd(0:1, 75  ) = [-14.8485, 0.0996]
  cd(0:1, 76  ) = [-14.2424, 0.0916]
  cd(0:1, 77  ) = [-13.6364, 0.0836]
  cd(0:1, 78  ) = [-13.0303, 0.0756]
  cd(0:1, 79  ) = [-12.4242, 0.0676]
  cd(0:1, 80  ) = [-11.8182, 0.0596]
  cd(0:1, 81  ) = [-11.2121, 0.0516]
  cd(0:1, 82  ) = [-10.6061, 0.0436]
  cd(0:1, 83  ) = [-10.0, 0.0356]
  cd(0:1, 84  ) = [-9.3939, 0.0276]
  cd(0:1, 85  ) = [-8.7879, 0.0201]
  cd(0:1, 86  ) = [-8.1818, 0.0136]
  cd(0:1, 87  ) = [-7.5758, 0.0104]
  cd(0:1, 88  ) = [-6.9697, 0.0089]
  cd(0:1, 89  ) = [-6.3636, 0.0075]
  cd(0:1, 90  ) = [-5.7576, 0.0071]
  cd(0:1, 91  ) = [-5.1515, 0.0068]
  cd(0:1, 92  ) = [-4.5455, 0.0065]
  cd(0:1, 93  ) = [-3.9394, 0.0064]
  cd(0:1, 94  ) = [-3.3333, 0.0064]
  cd(0:1, 95  ) = [-2.7273, 0.0063]
  cd(0:1, 96  ) = [-2.1212, 0.0063]
  cd(0:1, 97  ) = [-1.5152, 0.0063]
  cd(0:1, 98  ) = [-0.9091, 0.0063]
  cd(0:1, 99  ) = [-0.303, 0.0063]
  cd(0:1, 100  ) = [0.303, 0.0064]
  cd(0:1, 101  ) = [0.9091, 0.0064]
  cd(0:1, 102  ) = [1.5152, 0.0065]
  cd(0:1, 103  ) = [2.1212, 0.0065]
  cd(0:1, 104  ) = [2.7273, 0.0066]
  cd(0:1, 105  ) = [3.3333, 0.0068]
  cd(0:1, 106  ) = [3.9394, 0.0069]
  cd(0:1, 107  ) = [4.5455, 0.007]
  cd(0:1, 108  ) = [5.1515, 0.0072]
  cd(0:1, 109  ) = [5.7576, 0.0075]
  cd(0:1, 110  ) = [6.3636, 0.008]
  cd(0:1, 111  ) = [6.9697, 0.0089]
  cd(0:1, 112  ) = [7.5758, 0.0107]
  cd(0:1, 113  ) = [8.1818, 0.0125]
  cd(0:1, 114  ) = [8.7879, 0.014]
  cd(0:1, 115  ) = [9.3939, 0.0154]
  cd(0:1, 116  ) = [10.0, 0.0165]
  cd(0:1, 117  ) = [10.6061, 0.0182]
  cd(0:1, 118  ) = [11.2121, 0.0203]
  cd(0:1, 119  ) = [11.8182, 0.0301]
  cd(0:1, 120  ) = [12.4242, 0.0465]
  cd(0:1, 121  ) = [13.0303, 0.0628]
  cd(0:1, 122  ) = [13.6364, 0.0792]
  cd(0:1, 123  ) = [14.2424, 0.0923]
  cd(0:1, 124  ) = [14.8485, 0.1005]
  cd(0:1, 125  ) = [15.4545, 0.1086]
  cd(0:1, 126  ) = [16.0606, 0.1168]
  cd(0:1, 127  ) = [16.6667, 0.125]
  cd(0:1, 128  ) = [17.2727, 0.1332]
  cd(0:1, 129  ) = [17.8788, 0.1414]
  cd(0:1, 130  ) = [18.4848, 0.1495]
  cd(0:1, 131  ) = [19.0909, 0.1579]
  cd(0:1, 132  ) = [19.697, 0.1675]
  cd(0:1, 133  ) = [20.303, 0.1775]
  cd(0:1, 134  ) = [20.9091, 0.1878]
  cd(0:1, 135  ) = [21.5152, 0.198]
  cd(0:1, 136  ) = [22.1212, 0.2085]
  cd(0:1, 137  ) = [22.7273, 0.2195]
  cd(0:1, 138  ) = [23.3333, 0.2305]
  cd(0:1, 139  ) = [23.9394, 0.2416]
  cd(0:1, 140  ) = [24.5455, 0.2533]
  cd(0:1, 141  ) = [25.1515, 0.265]
  cd(0:1, 142  ) = [25.7576, 0.2768]
  cd(0:1, 143  ) = [26.3636, 0.289]
  cd(0:1, 144  ) = [26.9697, 0.3014]
  cd(0:1, 145  ) = [27.5758, 0.3138]
  cd(0:1, 146  ) = [28.1818, 0.3264]
  cd(0:1, 147  ) = [28.7879, 0.3394]
  cd(0:1, 148  ) = [29.3939, 0.3524]
  cd(0:1, 149  ) = [30.0, 0.3654]
  cd(0:1, 150  ) = [33.0, 0.4333]
  cd(0:1, 151  ) = [36.0, 0.5041]
  cd(0:1, 152  ) = [39.0, 0.5775]
  cd(0:1, 153  ) = [42.0, 0.6527]
  cd(0:1, 154  ) = [45.0, 0.7283]
  cd(0:1, 155  ) = [48.0, 0.8039]
  cd(0:1, 156  ) = [51.0, 0.878]
  cd(0:1, 157  ) = [54.0, 0.949]
  cd(0:1, 158  ) = [57.0, 1.02]
  cd(0:1, 159  ) = [60.0, 1.0911]
  cd(0:1, 160  ) = [63.0, 1.1489]
  cd(0:1, 161  ) = [66.0, 1.2068]
  cd(0:1, 162  ) = [69.0, 1.2647]
  cd(0:1, 163  ) = [72.0, 1.3091]
  cd(0:1, 164  ) = [75.0, 1.3468]
  cd(0:1, 165  ) = [78.0, 1.3845]
  cd(0:1, 166  ) = [81.0, 1.414]
  cd(0:1, 167  ) = [84.0, 1.427]
  cd(0:1, 168  ) = [87.0, 1.44]
  cd(0:1, 169  ) = [90.0, 1.4531]
  cd(0:1, 170  ) = [93.0, 1.44]
  cd(0:1, 171  ) = [96.0, 1.427]
  cd(0:1, 172  ) = [99.0, 1.414]
  cd(0:1, 173  ) = [102.0, 1.3845]
  cd(0:1, 174  ) = [105.0, 1.3468]
  cd(0:1, 175  ) = [108.0, 1.3091]
  cd(0:1, 176  ) = [111.0, 1.2647]
  cd(0:1, 177  ) = [114.0, 1.2068]
  cd(0:1, 178  ) = [117.0, 1.1489]
  cd(0:1, 179  ) = [120.0, 1.0911]
  cd(0:1, 180  ) = [123.0, 1.02]
  cd(0:1, 181  ) = [126.0, 0.949]
  cd(0:1, 182  ) = [129.0, 0.878]
  cd(0:1, 183  ) = [132.0, 0.8039]
  cd(0:1, 184  ) = [135.0, 0.7283]
  cd(0:1, 185  ) = [138.0, 0.6527]
  cd(0:1, 186  ) = [141.0, 0.5786]
  cd(0:1, 187  ) = [144.0, 0.5076]
  cd(0:1, 188  ) = [147.0, 0.4365]
  cd(0:1, 189  ) = [150.0, 0.3654]
  cd(0:1, 190  ) = [153.0, 0.3075]
  cd(0:1, 191  ) = [156.0, 0.2496]
  cd(0:1, 192  ) = [159.0, 0.1917]
  cd(0:1, 193  ) = [162.0, 0.1536]
  cd(0:1, 194  ) = [165.0, 0.1254]
  cd(0:1, 195  ) = [168.0, 0.0972]
  cd(0:1, 196  ) = [171.0, 0.0706]
  cd(0:1, 197  ) = [174.0, 0.0471]
  cd(0:1, 198  ) = [177.0, 0.0386]
  cd(0:1, 199  ) = [180.0, 0.0377]

  IF ( y .EQ. cd(0,0) ) cd30 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd30 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO 
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN 
     cd30 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd30_nrel2p8.pro' 
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd30 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd30_nrel2p8.pro' 
  ENDIF

!print,y,cd30

END FUNCTION calc_cd30_nrel2p8
!_________________________________________________________________________


END MODULE module_gad_nrel2p8

