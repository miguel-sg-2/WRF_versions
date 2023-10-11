MODULE module_gad_ge28

REAL, PARAMETER :: pi=3.1415926535897932384626433

CONTAINS

!===============================================================================
!
!  GE 2.8 MW Turbine
!
!===============================================================================

!=========================================================================

FUNCTION calc_rpm_ge28( x ) result( rpm )

!_________________________________________________________________________
!
! Data from GE 2.8 MW
! x =  wind speed
!_________________________________________________________________________


  REAL, INTENT( in ) :: x

  INTEGER, PARAMETER :: nws = 30

  REAL, DIMENSION( 0:nws-1 ) :: ws
  REAL, DIMENSION( 0:nws-1 ) :: rp

  REAL :: rpm
  INTEGER :: i

  rpm = 0.0

  rp(0)  = 8.0000000
  rp(1)  = 8.0000000
  rp(2)  = 8.0000000
  rp(3)  = 8.0000000
  rp(4)  = 8.0000000
  rp(5)  = 8.0000000
  rp(6)  = 8.2030167
  rp(7)  = 8.5075629
  rp(8)  = 8.6721525
  rp(9)  = 8.6956999
  rp(10) = 8.8133503
  rp(11) = 9.0714221
  rp(12) = 9.4682128
  rp(13) = 10.0011045
  rp(14) = 10.6665813
  rp(15) = 11.4602527
  rp(16) = 12.0388027
  rp(17) = 12.0388027
  rp(18) = 12.0388027
  rp(19) = 12.0388027
  rp(20) = 12.0388027
  rp(21) = 12.0388027
  rp(22) = 12.0388027
  rp(23) = 12.0388027
  rp(24) = 12.0388027
  rp(25) = 12.0388027
  rp(26) = 12.0388027
  rp(27) = 12.0388027
  rp(28) = 12.0388027
  rp(29) = 12.0388027

  ws(0)  = 3.0000000
  ws(1)  = 3.8896500
  ws(2)  = 4.6840070
  ws(3)  = 5.3778302
  ws(4)  = 5.9665421
  ws(5)  = 6.4462585
  ws(6)  = 6.8138144
  ws(7)  = 7.0667849
  ws(8)  = 7.2035009
  ws(9)  = 7.2230604
  ws(10) = 7.3207864
  ws(11) = 7.5351531
  ws(12) = 7.8647462
  ws(13) = 8.3073913
  ws(14) = 8.8601679
  ws(15) = 9.5194289
  ws(16) = 10.0165397
  ws(17) = 10.2808249
  ws(18) = 11.1393325
  ws(19) = 12.0892874
  ws(20) = 13.1244224
  ws(21) = 14.2379079
  ws(22) = 15.4223976
  ws(23) = 16.6700767
  ws(24) = 17.9727135
  ws(25) = 19.3217137
  ws(26) = 20.7081770
  ws(27) = 22.1229562
  ws(28) = 23.5567170
  ws(29) = 25.0000000

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

END FUNCTION calc_rpm_ge28
!_________________________________________________________________________
 
!=========================================================================

FUNCTION calc_pitch_ge28( x ) RESULT( pitch )

!_________________________________________________________________________
!
! Data from GE 2.8 MW
! x = wind speeds
!_________________________________________________________________________
 
  REAL, INTENT( IN ) :: x

  INTEGER, PARAMETER :: nws = 30

  REAL, DIMENSION( 0:nws-1 ) :: ws 
  REAL, DIMENSION( 0:nws-1 ) :: p

  REAL :: pitch
  INTEGER :: i

  pitch = 0.0

  p(0)  = 4.037784497
  p(1)  = 3.769398804
  p(2)  = 3.238069184
  p(3)  = 2.625164065
  p(4)  = 2.195868330
  p(5)  = 1.422637312
  p(6)  = 0.000000000
  p(7)  = 0.000000000
  p(8)  = 0.000000000
  p(9)  = 0.000000000
  p(10) = 0.000000000
  p(11) = 0.000000000
  p(12) = 0.000000000
  p(13) = 0.000000000
  p(14) = 2.187293904
  p(15) = 3.978096219
  p(16) = 5.008093294
  p(17) = 5.907198538
  p(18) = 8.131366750
  p(19) = 10.062099355
  p(20) = 11.881933888
  p(21) = 13.616212039
  p(22) = 15.331339346
  p(23) = 17.002430079
  p(24) = 18.643015097
  p(25) = 20.281932870
  p(26) = 21.893865676
  p(27) = 23.469671173
  p(28) = 24.944939580
  p(29) = 26.439972041
  
  ws(0)  = 3.0000000
  ws(1)  = 3.8896500
  ws(2)  = 4.6840070
  ws(3)  = 5.3778302
  ws(4)  = 5.9665421
  ws(5)  = 6.4462585
  ws(6)  = 6.8138144
  ws(7)  = 7.0667849
  ws(8)  = 7.2035009
  ws(9)  = 7.2230604
  ws(10) = 7.3207864
  ws(11) = 7.5351531
  ws(12) = 7.8647462
  ws(13) = 8.3073913
  ws(14) = 8.8601679
  ws(15) = 9.5194289
  ws(16) = 10.0165397
  ws(17) = 10.2808249
  ws(18) = 11.1393325
  ws(19) = 12.0892874
  ws(20) = 13.1244224
  ws(21) = 14.2379079
  ws(22) = 15.4223976
  ws(23) = 16.6700767
  ws(24) = 17.9727135
  ws(25) = 19.3217137
  ws(26) = 20.7081770
  ws(27) = 22.1229562
  ws(28) = 23.5567170
  ws(29) = 25.0000000


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

END FUNCTION calc_pitch_ge28

!=========================================================================

FUNCTION calc_fnft_ge28( v0, rho, blades, bladelen, diameter,           &
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

        xideg = calc_twist_ge28( radius/dscale ) + pitchdeg
        xi    = xideg*pi/180.0
!
!    --Cord length 

        cord  = cscale * calc_chord_ge28( radius/dscale )
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
     
           cl = calc_cl_ge28( radius/dscale, betadeg )
           cd = calc_cd_ge28( radius/dscale, betadeg )
  
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
  
       cl = calc_cl_ge28( radius/dscale, betadeg )
       cd = calc_cd_ge28( radius/dscale, betadeg )

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

END FUNCTION calc_fnft_ge28

!=========================================================================

FUNCTION calc_twist_ge28( x ) RESULT( twist )
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
  
  r(0)  = 2.00000000
  r(1)  = 4.11919952
  r(2)  = 6.23839904
  r(3)  = 8.35759855
  r(4)  = 10.47679807
  r(5)  = 12.59599759
  r(6)  = 14.71519711
  r(7)  = 16.83439663
  r(8)  = 18.95359614
  r(9)  = 21.07279566
  r(10) = 23.19199518
  r(11) = 25.31119470
  r(12) = 27.43039421
  r(13) = 29.54959373
  r(14) = 31.66879325
  r(15) = 33.78799277
  r(16) = 35.90719229
  r(17) = 38.02639180
  r(18) = 40.14559132
  r(19) = 42.26479084
  r(20) = 44.38399036
  r(21) = 46.50318988
  r(22) = 48.62238939
  r(23) = 50.74158891
  r(24) = 52.86078843
  r(25) = 54.97998795
  r(26) = 57.09918747
  r(27) = 59.21838698
  r(28) = 61.33758650
  r(29) = 63.45678602
 
  t(0)  = 19.99622705
  t(1)  = 19.21007989
  t(2)  = 18.19444736
  t(3)  = 16.99344970
  t(4)  = 15.65120711
  t(5)  = 13.98238987
  t(6)  = 11.84224242
  t(7)  = 9.61510861
  t(8)  = 7.69288880
  t(9)  = 6.26844777
  t(10) = 5.00464673
  t(11) = 3.89964290
  t(12) = 2.97844748
  t(13) = 2.23039859
  t(14) = 1.53663005
  t(15) = 0.94655895
  t(16) = 0.53107290
  t(17) = 0.33570186
  t(18) = 0.20451956
  t(19) = 0.10457652
  t(20) = 0.04462178
  t(21) = 0.03463063
  t(22) = 0.09012440
  t(23) = 0.17975683
  t(24) = 0.26007871
  t(25) = 0.28256807
  t(26) = -0.06648126
  t(27) = -0.85518072
  t(28) = -1.94698049
  t(29) = -3.20533072
  
  DO i = 1,ns-1
 
     IF ( ( x .GT. r(i-1) ) .AND. ( x .LE. r(i) ) ) THEN
        twist = t(i-1) + (x - r(i-1))*( ( t(i) - t(i-1) )/( r(i) - r(i-1) ) )
        goto 1
     ENDIF

1 CONTINUE

     IF ( x .LT. r(0) ) THEN
        PRINT*,'—————————————'
        PRINT*,'radius < blade root in calc_twist_ge28.pro'
	PRINT*, 'radius: ', x
        PRINT*,'—————————————'
        STOP
     ENDIF
     
     IF ( x .GT. r(ns-1) ) THEN
        PRINT*,'—————————————'
        PRINT*,'radius >  blade length in calc_twist_ge28.pro'
	PRINT*,'radius: ', x
        PRINT*,'—————————————'
        STOP
     ENDIF 
     
  ENDDO
  
!PRINT*,x,twist
  
!_________________________________________________________________________
  
END FUNCTION calc_twist_ge28
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_chord_ge28( x ) RESULT( chord )
!_________________________________________________________________________
!
! Data from GE 2.8 MW
! x = radial distance from hub center
!_________________________________________________________________________
 

  REAL, INTENT( IN ) :: x

  INTEGER, PARAMETER :: ns = 30

  REAL, DIMENSION( 0:ns-1 ) :: r
  REAL, DIMENSION( 0:ns-1 ) :: c

  REAL :: chord 
  INTEGER :: i

  chord = 0.0
  
  r(0)  = 2.00000000
  r(1)  = 4.11919952
  r(2)  = 6.23839904
  r(3)  = 8.35759855
  r(4)  = 10.47679807
  r(5)  = 12.59599759
  r(6)  = 14.71519711
  r(7)  = 16.83439663
  r(8)  = 18.95359614
  r(9)  = 21.07279566
  r(10) = 23.19199518
  r(11) = 25.31119470
  r(12) = 27.43039421
  r(13) = 29.54959373
  r(14) = 31.66879325
  r(15) = 33.78799277
  r(16) = 35.90719229
  r(17) = 38.02639180
  r(18) = 40.14559132
  r(19) = 42.26479084
  r(20) = 44.38399036
  r(21) = 46.50318988
  r(22) = 48.62238939
  r(23) = 50.74158891
  r(24) = 52.86078843
  r(25) = 54.97998795
  r(26) = 57.09918747
  r(27) = 59.21838698
  r(28) = 61.33758650
  r(29) = 63.45678602

  c(0)  = 2.60000000
  c(1)  = 2.92868930
  c(2)  = 3.23713623
  c(3)  = 3.52262149
  c(4)  = 3.78242576
  c(5)  = 4.04207625
  c(6)  = 4.31424240
  c(7)  = 4.54068957
  c(8)  = 4.66220635
  c(9)  = 4.60446019
  c(10) = 4.34831906
  c(11) = 3.99691584
  c(12) = 3.65807541
  c(13) = 3.40937494
  c(14) = 3.18027493
  c(15) = 2.96652488
  c(16) = 2.77788674
  c(17) = 2.61824468
  c(18) = 2.45671407
  c(19) = 2.31214652
  c(20) = 2.21496747
  c(21) = 2.19148818
  c(22) = 2.19255230
  c(23) = 2.19427103
  c(24) = 2.19581123
  c(25) = 2.19284084
  c(26) = 1.98956474
  c(27) = 1.53365234
  c(28) = 0.90912398
  c(29) = 0.20000000


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
     PRINT*,'radius > blade length in calc_chord_ge28.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
  
1 CONTINUE
  
!PRINT*,x,chord
   
!_________________________________________________________________________
  
END FUNCTION calc_chord_ge28
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_cl_ge28( x,y ) RESULT( cl )
!_________________________________________________________________________
!
! Calls appropriate airfoil section table depending upon radial distance
! from blade root. Data from GE 2.8 MW
!
! calc_cl1_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl2_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl3_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl4_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl5_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl6_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl7_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl8_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl9_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl10_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl11_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl12_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl13_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl14_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl15_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl16_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl17_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl18_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl19_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl20_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl21_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl22_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl23_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl24_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl25_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl26_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl27_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl28_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl29_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cl30_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!
! x = radial distance from hub center
! y = angle of attack
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: x,y

  INTEGER, PARAMETER :: ns = 30

  REAL, DIMENSION( 0:ns-1 ) :: r

  REAL :: cl

  INTEGER :: i

  cl = 0.0
  
  r(0)  = 2.00000000
  r(1)  = 4.11919952
  r(2)  = 6.23839904
  r(3)  = 8.35759855
  r(4)  = 10.47679807
  r(5)  = 12.59599759
  r(6)  = 14.71519711
  r(7)  = 16.83439663
  r(8)  = 18.95359614
  r(9)  = 21.07279566
  r(10) = 23.19199518
  r(11) = 25.31119470
  r(12) = 27.43039421
  r(13) = 29.54959373
  r(14) = 31.66879325
  r(15) = 33.78799277
  r(16) = 35.90719229
  r(17) = 38.02639180
  r(18) = 40.14559132
  r(19) = 42.26479084
  r(20) = 44.38399036
  r(21) = 46.50318988
  r(22) = 48.62238939
  r(23) = 50.74158891
  r(24) = 52.86078843
  r(25) = 54.97998795
  r(26) = 57.09918747
  r(27) = 59.21838698
  r(28) = 61.33758650
  r(29) = 63.45678602
  
  IF ( ( x .GT. r( 0) ) .AND. ( x .LE. r( 1) ) ) THEN
       cl = calc_cl1_ge28(y)
     ELSEIF ( ( x .GT. r( 1) ) .AND. ( x .LE. r( 2) ) ) THEN
       cl = calc_cl2_ge28(y)
     ELSEIF ( ( x .GT. r( 2) ) .AND. ( x .LE. r( 3) ) ) THEN
       cl = calc_cl3_ge28(y)
     ELSEIF ( ( x .GT. r( 3) ) .AND. ( x .LE. r( 4) ) ) THEN
       cl = calc_cl4_ge28(y)
     ELSEIF ( ( x .GT. r( 4) ) .AND. ( x .LE. r( 5) ) ) THEN
       cl = calc_cl5_ge28(y)
     ELSEIF ( ( x .GT. r( 5) ) .AND. ( x .LE. r( 6) ) ) THEN
       cl = calc_cl6_ge28(y)
     ELSEIF ( ( x .GT. r( 6) ) .AND. ( x .LE. r( 7) ) ) THEN
       cl = calc_cl7_ge28(y)
     ELSEIF ( ( x .GT. r( 7) ) .AND. ( x .LE. r( 8) ) ) THEN
       cl = calc_cl8_ge28(y)  
     ELSEIF ( ( x .GT. r( 8) ) .AND. ( x .LE. r( 9) ) ) THEN
       cl = calc_cl9_ge28(y)  
     ELSEIF ( ( x .GT. r( 9) ) .AND. ( x .LE. r(10) ) ) THEN
       cl = calc_cl10_ge28(y)  
     ELSEIF ( ( x .GT. r(10) ) .AND. ( x .LE. r(11) ) ) THEN
       cl = calc_cl11_ge28(y)
     ELSEIF ( ( x .GT. r(11) ) .AND. ( x .LE. r(12) ) ) THEN
       cl = calc_cl12_ge28(y)
     ELSEIF ( ( x .GT. r(12) ) .AND. ( x .LE. r(13) ) ) THEN
       cl = calc_cl13_ge28(y)
     ELSEIF ( ( x .GT. r(13) ) .AND. ( x .LE. r(14) ) ) THEN
       cl = calc_cl14_ge28(y)
     ELSEIF ( ( x .GT. r(14) ) .AND. ( x .LE. r(15) ) ) THEN
       cl = calc_cl15_ge28(y)
     ELSEIF ( ( x .GT. r(15) ) .AND. ( x .LE. r(16) ) ) THEN
       cl = calc_cl16_ge28(y)
     ELSEIF ( ( x .GT. r(16) ) .AND. ( x .LE. r(17) ) ) THEN
       cl = calc_cl17_ge28(y)
     ELSEIF ( ( x .GT. r(17) ) .AND. ( x .LE. r(18) ) ) THEN
       cl = calc_cl18_ge28(y)  
     ELSEIF ( ( x .GT. r(18) ) .AND. ( x .LE. r(19) ) ) THEN
       cl = calc_cl19_ge28(y)  
     ELSEIF ( ( x .GT. r(19) ) .AND. ( x .LE. r(20) ) ) THEN
       cl = calc_cl20_ge28(y)  
     ELSEIF ( ( x .GT. r(20) ) .AND. ( x .LE. r(21) ) ) THEN
       cl = calc_cl21_ge28(y)
     ELSEIF ( ( x .GT. r(21) ) .AND. ( x .LE. r(22) ) ) THEN
       cl = calc_cl22_ge28(y)
     ELSEIF ( ( x .GT. r(22) ) .AND. ( x .LE. r(23) ) ) THEN
       cl = calc_cl23_ge28(y)
     ELSEIF ( ( x .GT. r(23) ) .AND. ( x .LE. r(24) ) ) THEN
       cl = calc_cl24_ge28(y)
     ELSEIF ( ( x .GT. r(24) ) .AND. ( x .LE. r(25) ) ) THEN
       cl = calc_cl25_ge28(y)
     ELSEIF ( ( x .GT. r(25) ) .AND. ( x .LE. r(26) ) ) THEN
       cl = calc_cl26_ge28(y)
     ELSEIF ( ( x .GT. r(26) ) .AND. ( x .LE. r(27) ) ) THEN
       cl = calc_cl27_ge28(y)
     ELSEIF ( ( x .GT. r(27) ) .AND. ( x .LE. r(28) ) ) THEN
       cl = calc_cl28_ge28(y)  
     ELSEIF ( ( x .GT. r(28) ) .AND. ( x .LE. r(29) ) ) THEN
       cl = calc_cl29_ge28(y)  
     ELSEIF ( ( x .GT. r(29) ) .AND. ( x .LE. r(30) ) ) THEN
       cl = calc_cl30_ge28(y)  
  ENDIF
  
  IF ( x .LT. r(0) ) THEN 
     PRINT*,'—————————————'
     PRINT*,'radius less than blade root in calc_cl_ge28.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
  IF ( x .GT. r(ns-1) ) THEN 
     PRINT*,'—————————————'
     PRINT*,'radius greater than blade length in calc_cl_ge28.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
  
!_________________________________________________________________________
  
END FUNCTION calc_cl_ge28
!_________________________________________________________________________


FUNCTION calc_cl1_ge28( y ) RESULT( cl1 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________

  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl1
  INTEGER :: i

  cl1 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.0000000]
  cl(0:1, 2  ) = [-174.0000000,0.0000000]
  cl(0:1, 3  ) = [-171.0000000,0.0000000]
  cl(0:1, 4  ) = [-168.0000000,0.0000000]
  cl(0:1, 5  ) = [-165.0000000,0.0000000]
  cl(0:1, 6  ) = [-162.0000000,0.0000000]
  cl(0:1, 7  ) = [-159.0000000,0.0000000]
  cl(0:1, 8  ) = [-156.0000000,0.0000000]
  cl(0:1, 9  ) = [-153.0000000,0.0000000]
  cl(0:1, 10 ) = [-150.0000000,0.0000000]
  cl(0:1, 11 ) = [-147.0000000,0.0000000]
  cl(0:1, 12 ) = [-144.0000000,0.0000000]
  cl(0:1, 13 ) = [-141.0000000,0.0000000]
  cl(0:1, 14 ) = [-138.0000000,0.0000000]
  cl(0:1, 15 ) = [-135.0000000,0.0000000]
  cl(0:1, 16 ) = [-132.0000000,0.0000000]
  cl(0:1, 17 ) = [-129.0000000,0.0000000]
  cl(0:1, 18 ) = [-126.0000000,0.0000000]
  cl(0:1, 19 ) = [-123.0000000,0.0000000]
  cl(0:1, 20 ) = [-120.0000000,0.0000000]
  cl(0:1, 21 ) = [-117.0000000,0.0000000]
  cl(0:1, 22 ) = [-114.0000000,0.0000000]
  cl(0:1, 23 ) = [-111.0000000,0.0000000]
  cl(0:1, 24 ) = [-108.0000000,0.0000000]
  cl(0:1, 25 ) = [-105.0000000,0.0000000]
  cl(0:1, 26 ) = [-102.0000000,0.0000000]
  cl(0:1, 27 ) = [-99.0000000,0.0000000]
  cl(0:1, 28 ) = [-96.0000000,0.0000000]
  cl(0:1, 29 ) = [-93.0000000,0.0000000]
  cl(0:1, 30 ) = [-90.0000000,0.0000000]
  cl(0:1, 31 ) = [-87.0000000,0.0000000]
  cl(0:1, 32 ) = [-84.0000000,0.0000000]
  cl(0:1, 33 ) = [-81.0000000,0.0000000]
  cl(0:1, 34 ) = [-78.0000000,0.0000000]
  cl(0:1, 35 ) = [-75.0000000,0.0000000]
  cl(0:1, 36 ) = [-72.0000000,0.0000000]
  cl(0:1, 37 ) = [-69.0000000,0.0000000]
  cl(0:1, 38 ) = [-66.0000000,0.0000000]
  cl(0:1, 39 ) = [-63.0000000,0.0000000]
  cl(0:1, 40 ) = [-60.0000000,0.0000000]
  cl(0:1, 41 ) = [-57.0000000,0.0000000]
  cl(0:1, 42 ) = [-54.0000000,0.0000000]
  cl(0:1, 43 ) = [-51.0000000,0.0000000]
  cl(0:1, 44 ) = [-48.0000000,0.0000000]
  cl(0:1, 45 ) = [-45.0000000,0.0000000]
  cl(0:1, 46 ) = [-42.0000000,0.0000000]
  cl(0:1, 47 ) = [-39.0000000,0.0000000]
  cl(0:1, 48 ) = [-36.0000000,0.0000000]
  cl(0:1, 49 ) = [-33.0000000,0.0000000]
  cl(0:1, 50 ) = [-30.0000000,0.0000000]
  cl(0:1, 51 ) = [-29.3939394,0.0000000]
  cl(0:1, 52 ) = [-28.7878788,0.0000000]
  cl(0:1, 53 ) = [-28.1818182,0.0000000]
  cl(0:1, 54 ) = [-27.5757576,0.0000000]
  cl(0:1, 55 ) = [-26.9696970,0.0000000]
  cl(0:1, 56 ) = [-26.3636364,0.0000000]
  cl(0:1, 57 ) = [-25.7575758,0.0000000]
  cl(0:1, 58 ) = [-25.1515152,0.0000000]
  cl(0:1, 59 ) = [-24.5454545,0.0000000]
  cl(0:1, 60 ) = [-23.9393939,0.0000000]
  cl(0:1, 61 ) = [-23.3333333,0.0000000]
  cl(0:1, 62 ) = [-22.7272727,0.0000000]
  cl(0:1, 63 ) = [-22.1212121,0.0000000]
  cl(0:1, 64 ) = [-21.5151515,0.0000000]
  cl(0:1, 65 ) = [-20.9090909,0.0000000]
  cl(0:1, 66 ) = [-20.3030303,0.0000000]
  cl(0:1, 67 ) = [-19.6969697,0.0000000]
  cl(0:1, 68 ) = [-19.0909091,0.0000000]
  cl(0:1, 69 ) = [-18.4848485,0.0000000]
  cl(0:1, 70 ) = [-17.8787879,0.0000000]
  cl(0:1, 71 ) = [-17.2727273,0.0000000]
  cl(0:1, 72 ) = [-16.6666667,0.0000000]
  cl(0:1, 73 ) = [-16.0606061,0.0000000]
  cl(0:1, 74 ) = [-15.4545455,0.0000000]
  cl(0:1, 75 ) = [-14.8484848,0.0000000]
  cl(0:1, 76 ) = [-14.2424242,0.0000000]
  cl(0:1, 77 ) = [-13.6363636,0.0000000]
  cl(0:1, 78 ) = [-13.0303030,0.0000000]
  cl(0:1, 79 ) = [-12.4242424,0.0000000]
  cl(0:1, 80 ) = [-11.8181818,0.0000000]
  cl(0:1, 81 ) = [-11.2121212,0.0000000]
  cl(0:1, 82 ) = [-10.6060606,0.0000000]
  cl(0:1, 83 ) = [-10.0000000,0.0000000]
  cl(0:1, 84 ) = [-9.3939394,0.0000000]
  cl(0:1, 85 ) = [-8.7878788,0.0000000]
  cl(0:1, 86 ) = [-8.1818182,0.0000000]
  cl(0:1, 87 ) = [-7.5757576,0.0000000]
  cl(0:1, 88 ) = [-6.9696970,0.0000000]
  cl(0:1, 89 ) = [-6.3636364,0.0000000]
  cl(0:1, 90 ) = [-5.7575758,0.0000000]
  cl(0:1, 91 ) = [-5.1515152,0.0000000]
  cl(0:1, 92 ) = [-4.5454545,0.0000000]
  cl(0:1, 93 ) = [-3.9393939,0.0000000]
  cl(0:1, 94 ) = [-3.3333333,0.0000000]
  cl(0:1, 95 ) = [-2.7272727,0.0000000]
  cl(0:1, 96 ) = [-2.1212121,0.0000000]
  cl(0:1, 97 ) = [-1.5151515,0.0000000]
  cl(0:1, 98 ) = [-0.9090909,0.0000000]
  cl(0:1, 99 ) = [-0.3030303,0.0000000]
  cl(0:1, 100) = [0.3030303,0.0000000]
  cl(0:1, 101) = [0.9090909,0.0000000]
  cl(0:1, 102) = [1.5151515,0.0000000]
  cl(0:1, 103) = [2.1212121,0.0000000]
  cl(0:1, 104) = [2.7272727,0.0000000]
  cl(0:1, 105) = [3.3333333,0.0000000]
  cl(0:1, 106) = [3.9393939,0.0000000]
  cl(0:1, 107) = [4.5454545,0.0000000]
  cl(0:1, 108) = [5.1515152,0.0000000]
  cl(0:1, 109) = [5.7575758,0.0000000]
  cl(0:1, 110) = [6.3636364,0.0000000]
  cl(0:1, 111) = [6.9696970,0.0000000]
  cl(0:1, 112) = [7.5757576,0.0000000]
  cl(0:1, 113) = [8.1818182,0.0000000]
  cl(0:1, 114) = [8.7878788,0.0000000]
  cl(0:1, 115) = [9.3939394,0.0000000]
  cl(0:1, 116) = [10.0000000,0.0000000]
  cl(0:1, 117) = [10.6060606,0.0000000]
  cl(0:1, 118) = [11.2121212,0.0000000]
  cl(0:1, 119) = [11.8181818,0.0000000]
  cl(0:1, 120) = [12.4242424,0.0000000]
  cl(0:1, 121) = [13.0303030,0.0000000]
  cl(0:1, 122) = [13.6363636,0.0000000]
  cl(0:1, 123) = [14.2424242,0.0000000]
  cl(0:1, 124) = [14.8484848,0.0000000]
  cl(0:1, 125) = [15.4545455,0.0000000]
  cl(0:1, 126) = [16.0606061,0.0000000]
  cl(0:1, 127) = [16.6666667,0.0000000]
  cl(0:1, 128) = [17.2727273,0.0000000]
  cl(0:1, 129) = [17.8787879,0.0000000]
  cl(0:1, 130) = [18.4848485,0.0000000]
  cl(0:1, 131) = [19.0909091,0.0000000]
  cl(0:1, 132) = [19.6969697,0.0000000]
  cl(0:1, 133) = [20.3030303,0.0000000]
  cl(0:1, 134) = [20.9090909,0.0000000]
  cl(0:1, 135) = [21.5151515,0.0000000]
  cl(0:1, 136) = [22.1212121,0.0000000]
  cl(0:1, 137) = [22.7272727,0.0000000]
  cl(0:1, 138) = [23.3333333,0.0000000]
  cl(0:1, 139) = [23.9393939,0.0000000]
  cl(0:1, 140) = [24.5454545,0.0000000]
  cl(0:1, 141) = [25.1515152,0.0000000]
  cl(0:1, 142) = [25.7575758,0.0000000]
  cl(0:1, 143) = [26.3636364,0.0000000]
  cl(0:1, 144) = [26.9696970,0.0000000]
  cl(0:1, 145) = [27.5757576,0.0000000]
  cl(0:1, 146) = [28.1818182,0.0000000]
  cl(0:1, 147) = [28.7878788,0.0000000]
  cl(0:1, 148) = [29.3939394,0.0000000]
  cl(0:1, 149) = [30.0000000,0.0000000]
  cl(0:1, 150) = [33.0000000,0.0000000]
  cl(0:1, 151) = [36.0000000,0.0000000]
  cl(0:1, 152) = [39.0000000,0.0000000]
  cl(0:1, 153) = [42.0000000,0.0000000]
  cl(0:1, 154) = [45.0000000,0.0000000]
  cl(0:1, 155) = [48.0000000,0.0000000]
  cl(0:1, 156) = [51.0000000,0.0000000]
  cl(0:1, 157) = [54.0000000,0.0000000]
  cl(0:1, 158) = [57.0000000,0.0000000]
  cl(0:1, 159) = [60.0000000,0.0000000]
  cl(0:1, 160) = [63.0000000,0.0000000]
  cl(0:1, 161) = [66.0000000,0.0000000]
  cl(0:1, 162) = [69.0000000,0.0000000]
  cl(0:1, 163) = [72.0000000,0.0000000]
  cl(0:1, 164) = [75.0000000,0.0000000]
  cl(0:1, 165) = [78.0000000,0.0000000]
  cl(0:1, 166) = [81.0000000,0.0000000]
  cl(0:1, 167) = [84.0000000,0.0000000]
  cl(0:1, 168) = [87.0000000,0.0000000]
  cl(0:1, 169) = [90.0000000,0.0000000]
  cl(0:1, 170) = [93.0000000,0.0000000]
  cl(0:1, 171) = [96.0000000,0.0000000]
  cl(0:1, 172) = [99.0000000,0.0000000]
  cl(0:1, 173) = [102.0000000,0.0000000]
  cl(0:1, 174) = [105.0000000,0.0000000]
  cl(0:1, 175) = [108.0000000,0.0000000]
  cl(0:1, 176) = [111.0000000,0.0000000]
  cl(0:1, 177) = [114.0000000,0.0000000]
  cl(0:1, 178) = [117.0000000,0.0000000]
  cl(0:1, 179) = [120.0000000,0.0000000]
  cl(0:1, 180) = [123.0000000,0.0000000]
  cl(0:1, 181) = [126.0000000,0.0000000]
  cl(0:1, 182) = [129.0000000,0.0000000]
  cl(0:1, 183) = [132.0000000,0.0000000]
  cl(0:1, 184) = [135.0000000,0.0000000]
  cl(0:1, 185) = [138.0000000,0.0000000]
  cl(0:1, 186) = [141.0000000,0.0000000]
  cl(0:1, 187) = [144.0000000,0.0000000]
  cl(0:1, 188) = [147.0000000,0.0000000]
  cl(0:1, 189) = [150.0000000,0.0000000]
  cl(0:1, 190) = [153.0000000,0.0000000]
  cl(0:1, 191) = [156.0000000,0.0000000]
  cl(0:1, 192) = [159.0000000,0.0000000]
  cl(0:1, 193) = [162.0000000,0.0000000]
  cl(0:1, 194) = [165.0000000,0.0000000]
  cl(0:1, 195) = [168.0000000,0.0000000]
  cl(0:1, 196) = [171.0000000,0.0000000]
  cl(0:1, 197) = [174.0000000,0.0000000]
  cl(0:1, 198) = [177.0000000,0.0000000]
  cl(0:1, 199) = [180.0000000,0.0000000]

  IF ( y .EQ. cl(0,0) ) cl1 = cl(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cl(0,i-1) ) .AND. (y .LE. cl(0,i) ) ) THEN
        cl1 = cl(1,i-1) + (y - cl(0,i-1))*( ( cl(1,i) - cl(1,i-1) )/( cl(0,i) - cl(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO
1 CONTINUE

  IF ( y .LT. cl(0,0) ) THEN
     cl1 = cl(1,0)
!     PRINT*,'alpha < tabulated range in calc_cl1_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl1 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl1_ge28.pro'
  ENDIF

!print,y,cl1



!_________________________________________________________________________
  
END FUNCTION calc_cl1_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl2_ge28( y ) RESULT( cl2 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl2
  INTEGER :: i

  cl2 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.0062292]
  cl(0:1, 2  ) = [-174.0000000,0.0125154]
  cl(0:1, 3  ) = [-171.0000000,0.0187732]
  cl(0:1, 4  ) = [-168.0000000,0.0234094]
  cl(0:1, 5  ) = [-165.0000000,0.0272361]
  cl(0:1, 6  ) = [-162.0000000,0.0310637]
  cl(0:1, 7  ) = [-159.0000000,0.0336494]
  cl(0:1, 8  ) = [-156.0000000,0.0337511]
  cl(0:1, 9  ) = [-153.0000000,0.0338548]
  cl(0:1, 10 ) = [-150.0000000,0.0339611]
  cl(0:1, 11 ) = [-147.0000000,0.0340022]
  cl(0:1, 12 ) = [-144.0000000,0.0340443]
  cl(0:1, 13 ) = [-141.0000000,0.0340875]
  cl(0:1, 14 ) = [-138.0000000,0.0336791]
  cl(0:1, 15 ) = [-135.0000000,0.0330448]
  cl(0:1, 16 ) = [-132.0000000,0.0324110]
  cl(0:1, 17 ) = [-129.0000000,0.0314967]
  cl(0:1, 18 ) = [-126.0000000,0.0300204]
  cl(0:1, 19 ) = [-123.0000000,0.0285443]
  cl(0:1, 20 ) = [-120.0000000,0.0270684]
  cl(0:1, 21 ) = [-117.0000000,0.0248208]
  cl(0:1, 22 ) = [-114.0000000,0.0225733]
  cl(0:1, 23 ) = [-111.0000000,0.0203259]
  cl(0:1, 24 ) = [-108.0000000,0.0177045]
  cl(0:1, 25 ) = [-105.0000000,0.0148960]
  cl(0:1, 26 ) = [-102.0000000,0.0120877]
  cl(0:1, 27 ) = [-99.0000000,0.0091939]
  cl(0:1, 28 ) = [-96.0000000,0.0061293]
  cl(0:1, 29 ) = [-93.0000000,0.0030646]
  cl(0:1, 30 ) = [-90.0000000,0.0000000]
  cl(0:1, 31 ) = [-87.0000000,-0.0030646]
  cl(0:1, 32 ) = [-84.0000000,-0.0061293]
  cl(0:1, 33 ) = [-81.0000000,-0.0091939]
  cl(0:1, 34 ) = [-78.0000000,-0.0120877]
  cl(0:1, 35 ) = [-75.0000000,-0.0148961]
  cl(0:1, 36 ) = [-72.0000000,-0.0177044]
  cl(0:1, 37 ) = [-69.0000000,-0.0203258]
  cl(0:1, 38 ) = [-66.0000000,-0.0225733]
  cl(0:1, 39 ) = [-63.0000000,-0.0248208]
  cl(0:1, 40 ) = [-60.0000000,-0.0270684]
  cl(0:1, 41 ) = [-57.0000000,-0.0285443]
  cl(0:1, 42 ) = [-54.0000000,-0.0300204]
  cl(0:1, 43 ) = [-51.0000000,-0.0314967]
  cl(0:1, 44 ) = [-48.0000000,-0.0324110]
  cl(0:1, 45 ) = [-45.0000000,-0.0330448]
  cl(0:1, 46 ) = [-42.0000000,-0.0336791]
  cl(0:1, 47 ) = [-39.0000000,-0.0340875]
  cl(0:1, 48 ) = [-36.0000000,-0.0340443]
  cl(0:1, 49 ) = [-33.0000000,-0.0340022]
  cl(0:1, 50 ) = [-30.0000000,-0.0339611]
  cl(0:1, 51 ) = [-29.3939394,-0.0339394]
  cl(0:1, 52 ) = [-28.7878788,-0.0339178]
  cl(0:1, 53 ) = [-28.1818182,-0.0338964]
  cl(0:1, 54 ) = [-27.5757576,-0.0338750]
  cl(0:1, 55 ) = [-26.9696970,-0.0338538]
  cl(0:1, 56 ) = [-26.3636364,-0.0338326]
  cl(0:1, 57 ) = [-25.7575758,-0.0338116]
  cl(0:1, 58 ) = [-25.1515152,-0.0337906]
  cl(0:1, 59 ) = [-24.5454545,-0.0337698]
  cl(0:1, 60 ) = [-23.9393939,-0.0337490]
  cl(0:1, 61 ) = [-23.3333333,-0.0337283]
  cl(0:1, 62 ) = [-22.7272727,-0.0337077]
  cl(0:1, 63 ) = [-22.1212121,-0.0336872]
  cl(0:1, 64 ) = [-21.5151515,-0.0336667]
  cl(0:1, 65 ) = [-20.9090909,-0.0336464]
  cl(0:1, 66 ) = [-20.3030303,-0.0336261]
  cl(0:1, 67 ) = [-19.6969697,-0.0329942]
  cl(0:1, 68 ) = [-19.0909091,-0.0317510]
  cl(0:1, 69 ) = [-18.4848485,-0.0305082]
  cl(0:1, 70 ) = [-17.8787879,-0.0292658]
  cl(0:1, 71 ) = [-17.2727273,-0.0280239]
  cl(0:1, 72 ) = [-16.6666667,-0.0267824]
  cl(0:1, 73 ) = [-16.0606061,-0.0255413]
  cl(0:1, 74 ) = [-15.4545455,-0.0243008]
  cl(0:1, 75 ) = [-14.8484848,-0.0230609]
  cl(0:1, 76 ) = [-14.2424242,-0.0218214]
  cl(0:1, 77 ) = [-13.6363636,-0.0205833]
  cl(0:1, 78 ) = [-13.0303030,-0.0193443]
  cl(0:1, 79 ) = [-12.4242424,-0.0181075]
  cl(0:1, 80 ) = [-11.8181818,-0.0168699]
  cl(0:1, 81 ) = [-11.2121212,-0.0156338]
  cl(0:1, 82 ) = [-10.6060606,-0.0143986]
  cl(0:1, 83 ) = [-10.0000000,-0.0131630]
  cl(0:1, 84 ) = [-9.3939394,-0.0111477]
  cl(0:1, 85 ) = [-8.7878788,-0.0091329]
  cl(0:1, 86 ) = [-8.1818182,-0.0071215]
  cl(0:1, 87 ) = [-7.5757576,-0.0051127]
  cl(0:1, 88 ) = [-6.9696970,-0.0031070]
  cl(0:1, 89 ) = [-6.3636364,-0.0011031]
  cl(0:1, 90 ) = [-5.7575758,-0.0003007]
  cl(0:1, 91 ) = [-5.1515152,-0.0002216]
  cl(0:1, 92 ) = [-4.5454545,-0.0001496]
  cl(0:1, 93 ) = [-3.9393939,-0.0000869]
  cl(0:1, 94 ) = [-3.3333333,-0.0000306]
  cl(0:1, 95 ) = [-2.7272727,0.0024988]
  cl(0:1, 96 ) = [-2.1212121,0.0097023]
  cl(0:1, 97 ) = [-1.5151515,0.0169064]
  cl(0:1, 98 ) = [-0.9090909,0.0215688]
  cl(0:1, 99 ) = [-0.3030303,0.0242405]
  cl(0:1, 100) = [0.3030303,0.0269132]
  cl(0:1, 101) = [0.9090909,0.0295857]
  cl(0:1, 102) = [1.5151515,0.0322399]
  cl(0:1, 103) = [2.1212121,0.0366676]
  cl(0:1, 104) = [2.7272727,0.0419698]
  cl(0:1, 105) = [3.3333333,0.0472717]
  cl(0:1, 106) = [3.9393939,0.0525753]
  cl(0:1, 107) = [4.5454545,0.0578766]
  cl(0:1, 108) = [5.1515152,0.0631804]
  cl(0:1, 109) = [5.7575758,0.0678668]
  cl(0:1, 110) = [6.3636364,0.0715042]
  cl(0:1, 111) = [6.9696970,0.0749999]
  cl(0:1, 112) = [7.5757576,0.0785534]
  cl(0:1, 113) = [8.1818182,0.0827222]
  cl(0:1, 114) = [8.7878788,0.0859674]
  cl(0:1, 115) = [9.3939394,0.0863939]
  cl(0:1, 116) = [10.0000000,0.0861476]
  cl(0:1, 117) = [10.6060606,0.0114641]
  cl(0:1, 118) = [11.2121212,0.0017711]
  cl(0:1, 119) = [11.8181818,0.0016361]
  cl(0:1, 120) = [12.4242424,0.0538023]
  cl(0:1, 121) = [13.0303030,0.0707103]
  cl(0:1, 122) = [13.6363636,0.0681632]
  cl(0:1, 123) = [14.2424242,0.0649593]
  cl(0:1, 124) = [14.8484848,0.0600226]
  cl(0:1, 125) = [15.4545455,0.0542932]
  cl(0:1, 126) = [16.0606061,0.0489400]
  cl(0:1, 127) = [16.6666667,0.0490265]
  cl(0:1, 128) = [17.2727273,0.0489767]
  cl(0:1, 129) = [17.8787879,0.0487602]
  cl(0:1, 130) = [18.4848485,0.0485449]
  cl(0:1, 131) = [19.0909091,0.0483301]
  cl(0:1, 132) = [19.6969697,0.0481155]
  cl(0:1, 133) = [20.3030303,0.0480233]
  cl(0:1, 134) = [20.9090909,0.0480535]
  cl(0:1, 135) = [21.5151515,0.0480838]
  cl(0:1, 136) = [22.1212121,0.0481143]
  cl(0:1, 137) = [22.7272727,0.0481448]
  cl(0:1, 138) = [23.3333333,0.0481755]
  cl(0:1, 139) = [23.9393939,0.0482062]
  cl(0:1, 140) = [24.5454545,0.0482371]
  cl(0:1, 141) = [25.1515152,0.0482681]
  cl(0:1, 142) = [25.7575758,0.0482993]
  cl(0:1, 143) = [26.3636364,0.0483306]
  cl(0:1, 144) = [26.9696970,0.0483620]
  cl(0:1, 145) = [27.5757576,0.0483936]
  cl(0:1, 146) = [28.1818182,0.0484253]
  cl(0:1, 147) = [28.7878788,0.0484572]
  cl(0:1, 148) = [29.3939394,0.0484892]
  cl(0:1, 149) = [30.0000000,0.0485214]
  cl(0:1, 150) = [33.0000000,0.0485784]
  cl(0:1, 151) = [36.0000000,0.0486367]
  cl(0:1, 152) = [39.0000000,0.0486965]
  cl(0:1, 153) = [42.0000000,0.0481075]
  cl(0:1, 154) = [45.0000000,0.0471938]
  cl(0:1, 155) = [48.0000000,0.0462808]
  cl(0:1, 156) = [51.0000000,0.0449697]
  cl(0:1, 157) = [54.0000000,0.0428613]
  cl(0:1, 158) = [57.0000000,0.0407532]
  cl(0:1, 159) = [60.0000000,0.0386454]
  cl(0:1, 160) = [63.0000000,0.0354442]
  cl(0:1, 161) = [66.0000000,0.0322430]
  cl(0:1, 162) = [69.0000000,0.0290419]
  cl(0:1, 163) = [72.0000000,0.0252998]
  cl(0:1, 164) = [75.0000000,0.0212870]
  cl(0:1, 165) = [78.0000000,0.0172742]
  cl(0:1, 166) = [81.0000000,0.0131392]
  cl(0:1, 167) = [84.0000000,0.0087595]
  cl(0:1, 168) = [87.0000000,0.0043797]
  cl(0:1, 169) = [90.0000000,0.0000000]
  cl(0:1, 170) = [93.0000000,-0.0030646]
  cl(0:1, 171) = [96.0000000,-0.0061293]
  cl(0:1, 172) = [99.0000000,-0.0091939]
  cl(0:1, 173) = [102.0000000,-0.0120877]
  cl(0:1, 174) = [105.0000000,-0.0148960]
  cl(0:1, 175) = [108.0000000,-0.0177045]
  cl(0:1, 176) = [111.0000000,-0.0203259]
  cl(0:1, 177) = [114.0000000,-0.0225733]
  cl(0:1, 178) = [117.0000000,-0.0248208]
  cl(0:1, 179) = [120.0000000,-0.0270684]
  cl(0:1, 180) = [123.0000000,-0.0285443]
  cl(0:1, 181) = [126.0000000,-0.0300204]
  cl(0:1, 182) = [129.0000000,-0.0314967]
  cl(0:1, 183) = [132.0000000,-0.0324110]
  cl(0:1, 184) = [135.0000000,-0.0330448]
  cl(0:1, 185) = [138.0000000,-0.0336791]
  cl(0:1, 186) = [141.0000000,-0.0340875]
  cl(0:1, 187) = [144.0000000,-0.0340443]
  cl(0:1, 188) = [147.0000000,-0.0340022]
  cl(0:1, 189) = [150.0000000,-0.0339611]
  cl(0:1, 190) = [153.0000000,-0.0338548]
  cl(0:1, 191) = [156.0000000,-0.0337511]
  cl(0:1, 192) = [159.0000000,-0.0336494]
  cl(0:1, 193) = [162.0000000,-0.0310637]
  cl(0:1, 194) = [165.0000000,-0.0272361]
  cl(0:1, 195) = [168.0000000,-0.0234094]
  cl(0:1, 196) = [171.0000000,-0.0187732]
  cl(0:1, 197) = [174.0000000,-0.0125154]
  cl(0:1, 198) = [177.0000000,-0.0062292]
  cl(0:1, 199) = [180.0000000,0.0000000]


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
!     PRINT*,'alpha < tabulated range in calc_cl2_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl2 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl2_ge28.pro'
  ENDIF

!print,y,cl2


!_________________________________________________________________________
  
END FUNCTION calc_cl2_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl3_ge28( y ) RESULT( cl3 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl3
  INTEGER :: i

  cl3 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.00000000,0.00000000]
  cl(0:1, 1  ) = [-177.00000000,0.05282189]
  cl(0:1, 2  ) = [-174.00000000,0.10612769]
  cl(0:1, 3  ) = [-171.00000000,0.15919186]
  cl(0:1, 4  ) = [-168.00000000,0.19744762]
  cl(0:1, 5  ) = [-165.00000000,0.22836369]
  cl(0:1, 6  ) = [-162.00000000,0.25933904]
  cl(0:1, 7  ) = [-159.00000000,0.27982034]
  cl(0:1, 8  ) = [-156.00000000,0.27931146]
  cl(0:1, 9  ) = [-153.00000000,0.27892675]
  cl(0:1, 10 ) = [-150.00000000,0.27868683]
  cl(0:1, 11 ) = [-147.00000000,0.27848262]
  cl(0:1, 12 ) = [-144.00000000,0.27833300]
  cl(0:1, 13 ) = [-141.00000000,0.27824348]
  cl(0:1, 14 ) = [-138.00000000,0.27463820]
  cl(0:1, 15 ) = [-135.00000000,0.26927183]
  cl(0:1, 16 ) = [-132.00000000,0.26392992]
  cl(0:1, 17 ) = [-129.00000000,0.25635334]
  cl(0:1, 18 ) = [-126.00000000,0.24426710]
  cl(0:1, 19 ) = [-123.00000000,0.23219003]
  cl(0:1, 20 ) = [-120.00000000,0.22012403]
  cl(0:1, 21 ) = [-117.00000000,0.20182277]
  cl(0:1, 22 ) = [-114.00000000,0.18352483]
  cl(0:1, 23 ) = [-111.00000000,0.16523149]
  cl(0:1, 24 ) = [-108.00000000,0.14391169]
  cl(0:1, 25 ) = [-105.00000000,0.12107749]
  cl(0:1, 26 ) = [-102.00000000,0.09824512]
  cl(0:1, 27 ) = [-99.00000000,0.07472256]
  cl(0:1, 28 ) = [-96.00000000,0.04981493]
  cl(0:1, 29 ) = [-93.00000000,0.02490750]
  cl(0:1, 30 ) = [-90.00000000,0.00000016]
  cl(0:1, 31 ) = [-87.00000000,-0.02490747]
  cl(0:1, 32 ) = [-84.00000000,-0.04981500]
  cl(0:1, 33 ) = [-81.00000000,-0.07472234]
  cl(0:1, 34 ) = [-78.00000000,-0.09824501]
  cl(0:1, 35 ) = [-75.00000000,-0.12107764]
  cl(0:1, 36 ) = [-72.00000000,-0.14391158]
  cl(0:1, 37 ) = [-69.00000000,-0.16523134]
  cl(0:1, 38 ) = [-66.00000000,-0.18352488]
  cl(0:1, 39 ) = [-63.00000000,-0.20182289]
  cl(0:1, 40 ) = [-60.00000000,-0.22012411]
  cl(0:1, 41 ) = [-57.00000000,-0.23219011]
  cl(0:1, 42 ) = [-54.00000000,-0.24426718]
  cl(0:1, 43 ) = [-51.00000000,-0.25635342]
  cl(0:1, 44 ) = [-48.00000000,-0.26392991]
  cl(0:1, 45 ) = [-45.00000000,-0.26927176]
  cl(0:1, 46 ) = [-42.00000000,-0.27463820]
  cl(0:1, 47 ) = [-39.00000000,-0.27824348]
  cl(0:1, 48 ) = [-36.00000000,-0.27833300]
  cl(0:1, 49 ) = [-33.00000000,-0.27848262]
  cl(0:1, 50 ) = [-30.00000000,-0.27868683]
  cl(0:1, 51 ) = [-29.39393939,-0.27872248]
  cl(0:1, 52 ) = [-28.78787879,-0.27876483]
  cl(0:1, 53 ) = [-28.18181818,-0.27881367]
  cl(0:1, 54 ) = [-27.57575758,-0.27886877]
  cl(0:1, 55 ) = [-26.96969697,-0.27892995]
  cl(0:1, 56 ) = [-26.36363636,-0.27899702]
  cl(0:1, 57 ) = [-25.75757576,-0.27906979]
  cl(0:1, 58 ) = [-25.15151515,-0.27914808]
  cl(0:1, 59 ) = [-24.54545455,-0.27923173]
  cl(0:1, 60 ) = [-23.93939394,-0.27932057]
  cl(0:1, 61 ) = [-23.33333333,-0.27941445]
  cl(0:1, 62 ) = [-22.72727273,-0.27951323]
  cl(0:1, 63 ) = [-22.12121212,-0.27961675]
  cl(0:1, 64 ) = [-21.51515152,-0.27972489]
  cl(0:1, 65 ) = [-20.90909091,-0.27983751]
  cl(0:1, 66 ) = [-20.30303030,-0.27995448]
  cl(0:1, 67 ) = [-19.69696970,-0.27517146]
  cl(0:1, 68 ) = [-19.09090909,-0.26550261]
  cl(0:1, 69 ) = [-18.48484848,-0.25585781]
  cl(0:1, 70 ) = [-17.87878788,-0.24623759]
  cl(0:1, 71 ) = [-17.27272727,-0.23664264]
  cl(0:1, 72 ) = [-16.66666667,-0.22707455]
  cl(0:1, 73 ) = [-16.06060606,-0.21753455]
  cl(0:1, 74 ) = [-15.45454545,-0.20802444]
  cl(0:1, 75 ) = [-14.84848485,-0.19854503]
  cl(0:1, 76 ) = [-14.24242424,-0.18909746]
  cl(0:1, 77 ) = [-13.63636364,-0.17968912]
  cl(0:1, 78 ) = [-13.03030303,-0.17030593]
  cl(0:1, 79 ) = [-12.42424242,-0.16097074]
  cl(0:1, 80 ) = [-11.81818182,-0.15166542]
  cl(0:1, 81 ) = [-11.21212121,-0.14240583]
  cl(0:1, 82 ) = [-10.60606061,-0.13319099]
  cl(0:1, 83 ) = [-10.00000000,-0.12401468]
  cl(0:1, 84 ) = [-9.39393939,-0.10843649]
  cl(0:1, 85 ) = [-8.78787879,-0.09295413]
  cl(0:1, 86 ) = [-8.18181818,-0.07760269]
  cl(0:1, 87 ) = [-7.57575758,-0.06239708]
  cl(0:1, 88 ) = [-6.96969697,-0.04736853]
  cl(0:1, 89 ) = [-6.36363636,-0.03254218]
  cl(0:1, 90 ) = [-5.75757576,-0.02328835]
  cl(0:1, 91 ) = [-5.15151515,-0.01754638]
  cl(0:1, 92 ) = [-4.54545455,-0.01222317]
  cl(0:1, 93 ) = [-3.93939394,-0.00744825]
  cl(0:1, 94 ) = [-3.33333333,-0.00291659]
  cl(0:1, 95 ) = [-2.72727273,0.01685073]
  cl(0:1, 96 ) = [-2.12121212,0.06542682]
  cl(0:1, 97 ) = [-1.51515152,0.11400734]
  cl(0:1, 98 ) = [-0.90909091,0.15131373]
  cl(0:1, 99 ) = [-0.30303030,0.17979311]
  cl(0:1, 100) = [0.30303030,0.20827723]
  cl(0:1, 101) = [0.90909091,0.23676112]
  cl(0:1, 102) = [1.51515152,0.26209912]
  cl(0:1, 103) = [2.12121212,0.29821494]
  cl(0:1, 104) = [2.72727273,0.34126531]
  cl(0:1, 105) = [3.33333333,0.38432194]
  cl(0:1, 106) = [3.93939394,0.42739932]
  cl(0:1, 107) = [4.54545455,0.47046275]
  cl(0:1, 108) = [5.15151515,0.51355061]
  cl(0:1, 109) = [5.75757576,0.55169045]
  cl(0:1, 110) = [6.36363636,0.58150212]
  cl(0:1, 111) = [6.96969697,0.61030156]
  cl(0:1, 112) = [7.57575758,0.63964083]
  cl(0:1, 113) = [8.18181818,0.67371692]
  cl(0:1, 114) = [8.78787879,0.70057501]
  cl(0:1, 115) = [9.39393939,0.70598052]
  cl(0:1, 116) = [10.00000000,0.70687742]
  cl(0:1, 117) = [10.60606061,0.22404354]
  cl(0:1, 118) = [11.21212121,0.14026466]
  cl(0:1, 119) = [11.81818182,0.13138506]
  cl(0:1, 120) = [12.42424242,0.44603801]
  cl(0:1, 121) = [13.03030303,0.55599837]
  cl(0:1, 122) = [13.63636364,0.54418813]
  cl(0:1, 123) = [14.24242424,0.52794294]
  cl(0:1, 124) = [14.84848485,0.48968865]
  cl(0:1, 125) = [15.45454545,0.44873759]
  cl(0:1, 126) = [16.06060606,0.41294383]
  cl(0:1, 127) = [16.66666667,0.41250741]
  cl(0:1, 128) = [17.27272727,0.41114032]
  cl(0:1, 129) = [17.87878788,0.40861762]
  cl(0:1, 130) = [18.48484848,0.40611488]
  cl(0:1, 131) = [19.09090909,0.40362742]
  cl(0:1, 132) = [19.69696970,0.40115415]
  cl(0:1, 133) = [20.30303030,0.39984145]
  cl(0:1, 134) = [20.90909091,0.39968267]
  cl(0:1, 135) = [21.51515152,0.39953014]
  cl(0:1, 136) = [22.12121212,0.39938405]
  cl(0:1, 137) = [22.72727273,0.39924458]
  cl(0:1, 138) = [23.33333333,0.39911195]
  cl(0:1, 139) = [23.93939394,0.39898635]
  cl(0:1, 140) = [24.54545455,0.39886799]
  cl(0:1, 141) = [25.15151515,0.39875710]
  cl(0:1, 142) = [25.75757576,0.39865391]
  cl(0:1, 143) = [26.36363636,0.39855867]
  cl(0:1, 144) = [26.96969697,0.39847162]
  cl(0:1, 145) = [27.57575758,0.39839303]
  cl(0:1, 146) = [28.18181818,0.39832318]
  cl(0:1, 147) = [28.78787879,0.39826234]
  cl(0:1, 148) = [29.39393939,0.39821083]
  cl(0:1, 149) = [30.00000000,0.39816895]
  cl(0:1, 150) = [33.00000000,0.39786163]
  cl(0:1, 151) = [36.00000000,0.39763250]
  cl(0:1, 152) = [39.00000000,0.39748945]
  cl(0:1, 153) = [42.00000000,0.39229636]
  cl(0:1, 154) = [45.00000000,0.38457256]
  cl(0:1, 155) = [48.00000000,0.37688267]
  cl(0:1, 156) = [51.00000000,0.36602053]
  cl(0:1, 157) = [54.00000000,0.34875835]
  cl(0:1, 158) = [57.00000000,0.33150920]
  cl(0:1, 159) = [60.00000000,0.31427578]
  cl(0:1, 160) = [63.00000000,0.28820645]
  cl(0:1, 161) = [66.00000000,0.26214218]
  cl(0:1, 162) = [69.00000000,0.23608495]
  cl(0:1, 163) = [72.00000000,0.20564977]
  cl(0:1, 164) = [75.00000000,0.17302375]
  cl(0:1, 165) = [78.00000000,0.14039952]
  cl(0:1, 166) = [81.00000000,0.10678697]
  cl(0:1, 167) = [84.00000000,0.07119147]
  cl(0:1, 168) = [87.00000000,0.03559569]
  cl(0:1, 169) = [90.00000000,-0.00000016]
  cl(0:1, 170) = [93.00000000,-0.02490750]
  cl(0:1, 171) = [96.00000000,-0.04981493]
  cl(0:1, 172) = [99.00000000,-0.07472256]
  cl(0:1, 173) = [102.00000000,-0.09824512]
  cl(0:1, 174) = [105.00000000,-0.12107749]
  cl(0:1, 175) = [108.00000000,-0.14391169]
  cl(0:1, 176) = [111.00000000,-0.16523149]
  cl(0:1, 177) = [114.00000000,-0.18352483]
  cl(0:1, 178) = [117.00000000,-0.20182277]
  cl(0:1, 179) = [120.00000000,-0.22012403]
  cl(0:1, 180) = [123.00000000,-0.23219003]
  cl(0:1, 181) = [126.00000000,-0.24426710]
  cl(0:1, 182) = [129.00000000,-0.25635334]
  cl(0:1, 183) = [132.00000000,-0.26392992]
  cl(0:1, 184) = [135.00000000,-0.26927183]
  cl(0:1, 185) = [138.00000000,-0.27463820]
  cl(0:1, 186) = [141.00000000,-0.27824348]
  cl(0:1, 187) = [144.00000000,-0.27833300]
  cl(0:1, 188) = [147.00000000,-0.27848262]
  cl(0:1, 189) = [150.00000000,-0.27868683]
  cl(0:1, 190) = [153.00000000,-0.27892675]
  cl(0:1, 191) = [156.00000000,-0.27931146]
  cl(0:1, 192) = [159.00000000,-0.27982034]
  cl(0:1, 193) = [162.00000000,-0.25933904]
  cl(0:1, 194) = [165.00000000,-0.22836369]
  cl(0:1, 195) = [168.00000000,-0.19744762]
  cl(0:1, 196) = [171.00000000,-0.15919186]
  cl(0:1, 197) = [174.00000000,-0.10612769]
  cl(0:1, 198) = [177.00000000,-0.05282189]
  cl(0:1, 199) = [180.00000000,0.00000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl3_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl3 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl3_ge28.pro'
  ENDIF

!print,y,cl3

!_________________________________________________________________________
  
END FUNCTION calc_cl3_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl4_ge28( y ) RESULT( cl4 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl4
  INTEGER :: i

  cl4 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.00000000,0.00000000]
  cl(0:1, 1  ) = [-177.00000000,0.10399339]
  cl(0:1, 2  ) = [-174.00000000,0.20893950]
  cl(0:1, 3  ) = [-171.00000000,0.31340990]
  cl(0:1, 4  ) = [-168.00000000,0.38505444]
  cl(0:1, 5  ) = [-165.00000000,0.44041212]
  cl(0:1, 6  ) = [-162.00000000,0.49588550]
  cl(0:1, 7  ) = [-159.00000000,0.53062628]
  cl(0:1, 8  ) = [-156.00000000,0.52389776]
  cl(0:1, 9  ) = [-153.00000000,0.51741172]
  cl(0:1, 10 ) = [-150.00000000,0.51120842]
  cl(0:1, 11 ) = [-147.00000000,0.50772263]
  cl(0:1, 12 ) = [-144.00000000,0.50434346]
  cl(0:1, 13 ) = [-141.00000000,0.50108163]
  cl(0:1, 14 ) = [-138.00000000,0.49245373]
  cl(0:1, 15 ) = [-135.00000000,0.48113650]
  cl(0:1, 16 ) = [-132.00000000,0.46986704]
  cl(0:1, 17 ) = [-129.00000000,0.45493848]
  cl(0:1, 18 ) = [-126.00000000,0.43261298]
  cl(0:1, 19 ) = [-123.00000000,0.41030538]
  cl(0:1, 20 ) = [-120.00000000,0.38801940]
  cl(0:1, 21 ) = [-117.00000000,0.35532823]
  cl(0:1, 22 ) = [-114.00000000,0.32264352]
  cl(0:1, 23 ) = [-111.00000000,0.28996778]
  cl(0:1, 24 ) = [-108.00000000,0.25226140]
  cl(0:1, 25 ) = [-105.00000000,0.21203742]
  cl(0:1, 26 ) = [-102.00000000,0.17181698]
  cl(0:1, 27 ) = [-99.00000000,0.13050571]
  cl(0:1, 28 ) = [-96.00000000,0.08700362]
  cl(0:1, 29 ) = [-93.00000000,0.04350186]
  cl(0:1, 30 ) = [-90.00000000,0.00000027]
  cl(0:1, 31 ) = [-87.00000000,-0.04350182]
  cl(0:1, 32 ) = [-84.00000000,-0.08700374]
  cl(0:1, 33 ) = [-81.00000000,-0.13050533]
  cl(0:1, 34 ) = [-78.00000000,-0.17181679]
  cl(0:1, 35 ) = [-75.00000000,-0.21203769]
  cl(0:1, 36 ) = [-72.00000000,-0.25226120]
  cl(0:1, 37 ) = [-69.00000000,-0.28996750]
  cl(0:1, 38 ) = [-66.00000000,-0.32264361]
  cl(0:1, 39 ) = [-63.00000000,-0.35532844]
  cl(0:1, 40 ) = [-60.00000000,-0.38801955]
  cl(0:1, 41 ) = [-57.00000000,-0.41030553]
  cl(0:1, 42 ) = [-54.00000000,-0.43261312]
  cl(0:1, 43 ) = [-51.00000000,-0.45493863]
  cl(0:1, 44 ) = [-48.00000000,-0.46986702]
  cl(0:1, 45 ) = [-45.00000000,-0.48113636]
  cl(0:1, 46 ) = [-42.00000000,-0.49245372]
  cl(0:1, 47 ) = [-39.00000000,-0.50108163]
  cl(0:1, 48 ) = [-36.00000000,-0.50434347]
  cl(0:1, 49 ) = [-33.00000000,-0.50772261]
  cl(0:1, 50 ) = [-30.00000000,-0.51120846]
  cl(0:1, 51 ) = [-29.39393939,-0.51243660]
  cl(0:1, 52 ) = [-28.78787879,-0.51367782]
  cl(0:1, 53 ) = [-28.18181818,-0.51493170]
  cl(0:1, 54 ) = [-27.57575758,-0.51619786]
  cl(0:1, 55 ) = [-26.96969697,-0.51747589]
  cl(0:1, 56 ) = [-26.36363636,-0.51876542]
  cl(0:1, 57 ) = [-25.75757576,-0.52006606]
  cl(0:1, 58 ) = [-25.15151515,-0.52137748]
  cl(0:1, 59 ) = [-24.54545455,-0.52269936]
  cl(0:1, 60 ) = [-23.93939394,-0.52403138]
  cl(0:1, 61 ) = [-23.33333333,-0.52537325]
  cl(0:1, 62 ) = [-22.72727273,-0.52672467]
  cl(0:1, 63 ) = [-22.12121212,-0.52808537]
  cl(0:1, 64 ) = [-21.51515152,-0.52945510]
  cl(0:1, 65 ) = [-20.90909091,-0.53083359]
  cl(0:1, 66 ) = [-20.30303030,-0.53222057]
  cl(0:1, 67 ) = [-19.69696970,-0.52504185]
  cl(0:1, 68 ) = [-19.09090909,-0.50932503]
  cl(0:1, 69 ) = [-18.48484848,-0.49365503]
  cl(0:1, 70 ) = [-17.87878788,-0.47803304]
  cl(0:1, 71 ) = [-17.27272727,-0.46246054]
  cl(0:1, 72 ) = [-16.66666667,-0.44694050]
  cl(0:1, 73 ) = [-16.06060606,-0.43147532]
  cl(0:1, 74 ) = [-15.45454545,-0.41606835]
  cl(0:1, 75 ) = [-14.84848485,-0.40072135]
  cl(0:1, 76 ) = [-14.24242424,-0.38543670]
  cl(0:1, 77 ) = [-13.63636364,-0.37022454]
  cl(0:1, 78 ) = [-13.03030303,-0.35506937]
  cl(0:1, 79 ) = [-12.42424242,-0.33999973]
  cl(0:1, 80 ) = [-11.81818182,-0.32499563]
  cl(0:1, 81 ) = [-11.21212121,-0.31007756]
  cl(0:1, 82 ) = [-10.60606061,-0.29524547]
  cl(0:1, 83 ) = [-10.00000000,-0.28049480]
  cl(0:1, 84 ) = [-9.39393939,-0.25337837]
  cl(0:1, 85 ) = [-8.78787879,-0.22645562]
  cl(0:1, 86 ) = [-8.18181818,-0.19978035]
  cl(0:1, 87 ) = [-7.57575758,-0.17338984]
  cl(0:1, 88 ) = [-6.96969697,-0.14734518]
  cl(0:1, 89 ) = [-6.36363636,-0.12170511]
  cl(0:1, 90 ) = [-5.75757576,-0.09845835]
  cl(0:1, 91 ) = [-5.15151515,-0.07698047]
  cl(0:1, 92 ) = [-4.54545455,-0.05631829]
  cl(0:1, 93 ) = [-3.93939394,-0.03673022]
  cl(0:1, 94 ) = [-3.33333333,-0.01629095]
  cl(0:1, 95 ) = [-2.72727273,0.02356756]
  cl(0:1, 96 ) = [-2.12121212,0.09150643]
  cl(0:1, 97 ) = [-1.51515152,0.15945150]
  cl(0:1, 98 ) = [-0.90909091,0.22340360]
  cl(0:1, 99 ) = [-0.30303030,0.28423789]
  cl(0:1, 100) = [0.30303030,0.34507437]
  cl(0:1, 101) = [0.90909091,0.40591315]
  cl(0:1, 102) = [1.51515152,0.46039156]
  cl(0:1, 103) = [2.12121212,0.52615944]
  cl(0:1, 104) = [2.72727273,0.60083773]
  cl(0:1, 105) = [3.33333333,0.67552995]
  cl(0:1, 106) = [3.93939394,0.75025590]
  cl(0:1, 107) = [4.54545455,0.82496620]
  cl(0:1, 108) = [5.15151515,0.89971259]
  cl(0:1, 109) = [5.75757576,0.96796866]
  cl(0:1, 110) = [6.36363636,1.02536320]
  cl(0:1, 111) = [6.96969697,1.08152199]
  cl(0:1, 112) = [7.57575758,1.13835718]
  cl(0:1, 113) = [8.18181818,1.20034525]
  cl(0:1, 114) = [8.78787879,1.25229032]
  cl(0:1, 115) = [9.39393939,1.27694825]
  cl(0:1, 116) = [10.00000000,1.29634873]
  cl(0:1, 117) = [10.60606061,0.76142814]
  cl(0:1, 118) = [11.21212121,0.61571838]
  cl(0:1, 119) = [11.81818182,0.58964596]
  cl(0:1, 120) = [12.42424242,0.84007845]
  cl(0:1, 121) = [13.03030303,0.93654037]
  cl(0:1, 122) = [13.63636364,0.93079318]
  cl(0:1, 123) = [14.24242424,0.92153560]
  cl(0:1, 124) = [14.84848485,0.87992021]
  cl(0:1, 125) = [15.45454545,0.83911217]
  cl(0:1, 126) = [16.06060606,0.80586601]
  cl(0:1, 127) = [16.66666667,0.80084732]
  cl(0:1, 128) = [17.27272727,0.79472781]
  cl(0:1, 129) = [17.87878788,0.78722752]
  cl(0:1, 130) = [18.48484848,0.77976097]
  cl(0:1, 131) = [19.09090909,0.77232295]
  cl(0:1, 132) = [19.69696970,0.76491251]
  cl(0:1, 133) = [20.30303030,0.76022791]
  cl(0:1, 134) = [20.90909091,0.75825617]
  cl(0:1, 135) = [21.51515152,0.75629665]
  cl(0:1, 136) = [22.12121212,0.75434971]
  cl(0:1, 137) = [22.72727273,0.75241576]
  cl(0:1, 138) = [23.33333333,0.75049514]
  cl(0:1, 139) = [23.93939394,0.74858825]
  cl(0:1, 140) = [24.54545455,0.74669551]
  cl(0:1, 141) = [25.15151515,0.74481736]
  cl(0:1, 142) = [25.75757576,0.74295425]
  cl(0:1, 143) = [26.36363636,0.74110662]
  cl(0:1, 144) = [26.96969697,0.73927498]
  cl(0:1, 145) = [27.57575758,0.73745986]
  cl(0:1, 146) = [28.18181818,0.73566182]
  cl(0:1, 147) = [28.78787879,0.73388143]
  cl(0:1, 148) = [29.39393939,0.73211924]
  cl(0:1, 149) = [30.00000000,0.73037587]
  cl(0:1, 150) = [33.00000000,0.72536248]
  cl(0:1, 151) = [36.00000000,0.72050191]
  cl(0:1, 152) = [39.00000000,0.71580937]
  cl(0:1, 153) = [42.00000000,0.70343319]
  cl(0:1, 154) = [45.00000000,0.68720409]
  cl(0:1, 155) = [48.00000000,0.67104122]
  cl(0:1, 156) = [51.00000000,0.64967205]
  cl(0:1, 157) = [54.00000000,0.61778181]
  cl(0:1, 158) = [57.00000000,0.58591703]
  cl(0:1, 159) = [60.00000000,0.55408295]
  cl(0:1, 160) = [63.00000000,0.50747421]
  cl(0:1, 161) = [66.00000000,0.46087538]
  cl(0:1, 162) = [69.00000000,0.41429029]
  cl(0:1, 163) = [72.00000000,0.36045550]
  cl(0:1, 164) = [75.00000000,0.30299218]
  cl(0:1, 165) = [78.00000000,0.24553243]
  cl(0:1, 166) = [81.00000000,0.18650713]
  cl(0:1, 167) = [84.00000000,0.12433836]
  cl(0:1, 168) = [87.00000000,0.06216910]
  cl(0:1, 169) = [90.00000000,-0.00000027]
  cl(0:1, 170) = [93.00000000,-0.04350186]
  cl(0:1, 171) = [96.00000000,-0.08700362]
  cl(0:1, 172) = [99.00000000,-0.13050571]
  cl(0:1, 173) = [102.00000000,-0.17181698]
  cl(0:1, 174) = [105.00000000,-0.21203742]
  cl(0:1, 175) = [108.00000000,-0.25226140]
  cl(0:1, 176) = [111.00000000,-0.28996778]
  cl(0:1, 177) = [114.00000000,-0.32264352]
  cl(0:1, 178) = [117.00000000,-0.35532823]
  cl(0:1, 179) = [120.00000000,-0.38801940]
  cl(0:1, 180) = [123.00000000,-0.41030538]
  cl(0:1, 181) = [126.00000000,-0.43261298]
  cl(0:1, 182) = [129.00000000,-0.45493848]
  cl(0:1, 183) = [132.00000000,-0.46986704]
  cl(0:1, 184) = [135.00000000,-0.48113650]
  cl(0:1, 185) = [138.00000000,-0.49245373]
  cl(0:1, 186) = [141.00000000,-0.50108163]
  cl(0:1, 187) = [144.00000000,-0.50434346]
  cl(0:1, 188) = [147.00000000,-0.50772263]
  cl(0:1, 189) = [150.00000000,-0.51120842]
  cl(0:1, 190) = [153.00000000,-0.51741172]
  cl(0:1, 191) = [156.00000000,-0.52389776]
  cl(0:1, 192) = [159.00000000,-0.53062628]
  cl(0:1, 193) = [162.00000000,-0.49588550]
  cl(0:1, 194) = [165.00000000,-0.44041212]
  cl(0:1, 195) = [168.00000000,-0.38505444]
  cl(0:1, 196) = [171.00000000,-0.31340990]
  cl(0:1, 197) = [174.00000000,-0.20893950]
  cl(0:1, 198) = [177.00000000,-0.10399339]
  cl(0:1, 199) = [180.00000000,0.00000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl4_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl4 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl4_ge28.pro'
  ENDIF

!print,y,cl4

!_________________________________________________________________________
  
END FUNCTION calc_cl4_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl5_ge28( y ) RESULT( cl5 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl5
  INTEGER :: i

  cl5 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.000000000,0.000000000]
  cl(0:1, 1  ) = [-177.000000000,0.131704480]
  cl(0:1, 2  ) = [-174.000000000,0.264615542]
  cl(0:1, 3  ) = [-171.000000000,0.396924138]
  cl(0:1, 4  ) = [-168.000000000,0.483692710]
  cl(0:1, 5  ) = [-165.000000000,0.547678400]
  cl(0:1, 6  ) = [-162.000000000,0.611661937]
  cl(0:1, 7  ) = [-159.000000000,0.649147891]
  cl(0:1, 8  ) = [-156.000000000,0.633627994]
  cl(0:1, 9  ) = [-153.000000000,0.618102771]
  cl(0:1, 10 ) = [-150.000000000,0.602571441]
  cl(0:1, 11 ) = [-147.000000000,0.593851743]
  cl(0:1, 12 ) = [-144.000000000,0.585129730]
  cl(0:1, 13 ) = [-141.000000000,0.576405141]
  cl(0:1, 14 ) = [-138.000000000,0.562958435]
  cl(0:1, 15 ) = [-135.000000000,0.547150816]
  cl(0:1, 16 ) = [-132.000000000,0.531341995]
  cl(0:1, 17 ) = [-129.000000000,0.511898674]
  cl(0:1, 18 ) = [-126.000000000,0.485188015]
  cl(0:1, 19 ) = [-123.000000000,0.458477183]
  cl(0:1, 20 ) = [-120.000000000,0.431765995]
  cl(0:1, 21 ) = [-117.000000000,0.394568157]
  cl(0:1, 22 ) = [-114.000000000,0.357370305]
  cl(0:1, 23 ) = [-111.000000000,0.320172552]
  cl(0:1, 24 ) = [-108.000000000,0.277962126]
  cl(0:1, 25 ) = [-105.000000000,0.233245373]
  cl(0:1, 26 ) = [-102.000000000,0.188529066]
  cl(0:1, 27 ) = [-99.000000000,0.142846269]
  cl(0:1, 28 ) = [-96.000000000,0.095230642]
  cl(0:1, 29 ) = [-93.000000000,0.047615379]
  cl(0:1, 30 ) = [-90.000000000,0.000000297]
  cl(0:1, 31 ) = [-87.000000000,-0.047615330]
  cl(0:1, 32 ) = [-84.000000000,-0.095230775]
  cl(0:1, 33 ) = [-81.000000000,-0.142845857]
  cl(0:1, 34 ) = [-78.000000000,-0.188528850]
  cl(0:1, 35 ) = [-75.000000000,-0.233245669]
  cl(0:1, 36 ) = [-72.000000000,-0.277961910]
  cl(0:1, 37 ) = [-69.000000000,-0.320172230]
  cl(0:1, 38 ) = [-66.000000000,-0.357370409]
  cl(0:1, 39 ) = [-63.000000000,-0.394568403]
  cl(0:1, 40 ) = [-60.000000000,-0.431766171]
  cl(0:1, 41 ) = [-57.000000000,-0.458477360]
  cl(0:1, 42 ) = [-54.000000000,-0.485188192]
  cl(0:1, 43 ) = [-51.000000000,-0.511898851]
  cl(0:1, 44 ) = [-48.000000000,-0.531341959]
  cl(0:1, 45 ) = [-45.000000000,-0.547150558]
  cl(0:1, 46 ) = [-42.000000000,-0.562958388]
  cl(0:1, 47 ) = [-39.000000000,-0.576405141]
  cl(0:1, 48 ) = [-36.000000000,-0.585129736]
  cl(0:1, 49 ) = [-33.000000000,-0.593851683]
  cl(0:1, 50 ) = [-30.000000000,-0.602571528]
  cl(0:1, 51 ) = [-29.393939394,-0.605709743]
  cl(0:1, 52 ) = [-28.787878788,-0.608847691]
  cl(0:1, 53 ) = [-28.181818182,-0.611985380]
  cl(0:1, 54 ) = [-27.575757576,-0.615122859]
  cl(0:1, 55 ) = [-26.969696970,-0.618260110]
  cl(0:1, 56 ) = [-26.363636364,-0.621397123]
  cl(0:1, 57 ) = [-25.757575758,-0.624533844]
  cl(0:1, 58 ) = [-25.151515152,-0.627670245]
  cl(0:1, 59 ) = [-24.545454545,-0.630806424]
  cl(0:1, 60 ) = [-23.939393939,-0.633942377]
  cl(0:1, 61 ) = [-23.333333333,-0.637078020]
  cl(0:1, 62 ) = [-22.727272727,-0.640213455]
  cl(0:1, 63 ) = [-22.121212121,-0.643348688]
  cl(0:1, 64 ) = [-21.515151515,-0.646483667]
  cl(0:1, 65 ) = [-20.909090909,-0.649618442]
  cl(0:1, 66 ) = [-20.303030303,-0.652753033]
  cl(0:1, 67 ) = [-19.696969697,-0.646281639]
  cl(0:1, 68 ) = [-19.090909091,-0.630204218]
  cl(0:1, 69 ) = [-18.484848485,-0.614126582]
  cl(0:1, 70 ) = [-17.878787879,-0.598047857]
  cl(0:1, 71 ) = [-17.272727273,-0.581967303]
  cl(0:1, 72 ) = [-16.666666667,-0.565885606]
  cl(0:1, 73 ) = [-16.060606061,-0.549802714]
  cl(0:1, 74 ) = [-15.454545455,-0.533719399]
  cl(0:1, 75 ) = [-14.848484848,-0.517634634]
  cl(0:1, 76 ) = [-14.242424242,-0.501547802]
  cl(0:1, 77 ) = [-13.636363636,-0.485463674]
  cl(0:1, 78 ) = [-13.030303030,-0.469370079]
  cl(0:1, 79 ) = [-12.424242424,-0.453283193]
  cl(0:1, 80 ) = [-11.818181818,-0.437187367]
  cl(0:1, 81 ) = [-11.212121212,-0.421093768]
  cl(0:1, 82 ) = [-10.606060606,-0.404998617]
  cl(0:1, 83 ) = [-10.000000000,-0.388896565]
  cl(0:1, 84 ) = [-9.393939394,-0.356980582]
  cl(0:1, 85 ) = [-8.787878788,-0.325052844]
  cl(0:1, 86 ) = [-8.181818182,-0.293123808]
  cl(0:1, 87 ) = [-7.575757576,-0.261189541]
  cl(0:1, 88 ) = [-6.969696970,-0.229253316]
  cl(0:1, 89 ) = [-6.363636364,-0.197299114]
  cl(0:1, 90 ) = [-5.757575758,-0.165337094]
  cl(0:1, 91 ) = [-5.151515152,-0.133363372]
  cl(0:1, 92 ) = [-4.545454545,-0.101365854]
  cl(0:1, 93 ) = [-3.939393939,-0.069356784]
  cl(0:1, 94 ) = [-3.333333333,-0.033180756]
  cl(0:1, 95 ) = [-2.727272727,0.023972738]
  cl(0:1, 96 ) = [-2.121212121,0.093248633]
  cl(0:1, 97 ) = [-1.515151515,0.162528583]
  cl(0:1, 98 ) = [-0.909090909,0.231797235]
  cl(0:1, 99 ) = [-0.303030303,0.301068247]
  cl(0:1, 100) = [0.303030303,0.370338484]
  cl(0:1, 101) = [0.909090909,0.439610300]
  cl(0:1, 102) = [1.515151515,0.509192580]
  cl(0:1, 103) = [2.121212121,0.586406064]
  cl(0:1, 104) = [2.727272727,0.667209758]
  cl(0:1, 105) = [3.333333333,0.748006311]
  cl(0:1, 106) = [3.939393939,0.828814560]
  cl(0:1, 107) = [4.545454545,0.909607714]
  cl(0:1, 108) = [5.151515152,0.990415751]
  cl(0:1, 109) = [5.757575758,1.068404471]
  cl(0:1, 110) = [6.363636364,1.141604003]
  cl(0:1, 111) = [6.969696970,1.214137217]
  cl(0:1, 112) = [7.575757576,1.286646093]
  cl(0:1, 113) = [8.181818182,1.359073673]
  cl(0:1, 114) = [8.787878788,1.424988755]
  cl(0:1, 115) = [9.393939394,1.477900440]
  cl(0:1, 116) = [10.000000000,1.527619510]
  cl(0:1, 117) = [10.606060606,1.210371112]
  cl(0:1, 118) = [11.212121212,1.067452521]
  cl(0:1, 119) = [11.818181818,1.040476822]
  cl(0:1, 120) = [12.424242424,1.020501393]
  cl(0:1, 121) = [13.030303030,1.009568824]
  cl(0:1, 122) = [13.636363636,1.007437786]
  cl(0:1, 123) = [14.242424242,1.007574756]
  cl(0:1, 124) = [14.848484848,1.008997151]
  cl(0:1, 125) = [15.454545455,1.011489579]
  cl(0:1, 126) = [16.060606061,1.012889006]
  cl(0:1, 127) = [16.666666667,1.001952528]
  cl(0:1, 128) = [17.272727273,0.990372353]
  cl(0:1, 129) = [17.878787879,0.978006308]
  cl(0:1, 130) = [18.484848485,0.965644507]
  cl(0:1, 131) = [19.090909091,0.953284332]
  cl(0:1, 132) = [19.696969697,0.940924883]
  cl(0:1, 133) = [20.303030303,0.932508485]
  cl(0:1, 134) = [20.909090909,0.928035612]
  cl(0:1, 135) = [21.515151515,0.923562474]
  cl(0:1, 136) = [22.121212121,0.919089043]
  cl(0:1, 137) = [22.727272727,0.914615248]
  cl(0:1, 138) = [23.333333333,0.910141162]
  cl(0:1, 139) = [23.939393939,0.905666775]
  cl(0:1, 140) = [24.545454545,0.901191945]
  cl(0:1, 141) = [25.151515152,0.896716790]
  cl(0:1, 142) = [25.757575758,0.892241315]
  cl(0:1, 143) = [26.363636364,0.887765381]
  cl(0:1, 144) = [26.969696970,0.883289028]
  cl(0:1, 145) = [27.575757576,0.878812333]
  cl(0:1, 146) = [28.181818182,0.874335310]
  cl(0:1, 147) = [28.787878788,0.869857983]
  cl(0:1, 148) = [29.393939394,0.865380285]
  cl(0:1, 149) = [30.000000000,0.860902203]
  cl(0:1, 150) = [33.000000000,0.848397959]
  cl(0:1, 151) = [36.000000000,0.835890708]
  cl(0:1, 152) = [39.000000000,0.823379662]
  cl(0:1, 153) = [42.000000000,0.804154955]
  cl(0:1, 154) = [45.000000000,0.781573458]
  cl(0:1, 155) = [48.000000000,0.758990916]
  cl(0:1, 156) = [51.000000000,0.731215840]
  cl(0:1, 157) = [54.000000000,0.693057700]
  cl(0:1, 158) = [57.000000000,0.654899315]
  cl(0:1, 159) = [60.000000000,0.616740426]
  cl(0:1, 160) = [63.000000000,0.563629557]
  cl(0:1, 161) = [66.000000000,0.510518346]
  cl(0:1, 162) = [69.000000000,0.457406845]
  cl(0:1, 163) = [72.000000000,0.397127599]
  cl(0:1, 164) = [75.000000000,0.333264710]
  cl(0:1, 165) = [78.000000000,0.269401000]
  cl(0:1, 166) = [81.000000000,0.204142653]
  cl(0:1, 167) = [84.000000000,0.136095394]
  cl(0:1, 168) = [87.000000000,0.068047614]
  cl(0:1, 169) = [90.000000000,-0.000000297]
  cl(0:1, 170) = [93.000000000,-0.047615379]
  cl(0:1, 171) = [96.000000000,-0.095230642]
  cl(0:1, 172) = [99.000000000,-0.142846269]
  cl(0:1, 173) = [102.000000000,-0.188529066]
  cl(0:1, 174) = [105.000000000,-0.233245373]
  cl(0:1, 175) = [108.000000000,-0.277962126]
  cl(0:1, 176) = [111.000000000,-0.320172552]
  cl(0:1, 177) = [114.000000000,-0.357370305]
  cl(0:1, 178) = [117.000000000,-0.394568157]
  cl(0:1, 179) = [120.000000000,-0.431765995]
  cl(0:1, 180) = [123.000000000,-0.458477183]
  cl(0:1, 181) = [126.000000000,-0.485188015]
  cl(0:1, 182) = [129.000000000,-0.511898674]
  cl(0:1, 183) = [132.000000000,-0.531341995]
  cl(0:1, 184) = [135.000000000,-0.547150816]
  cl(0:1, 185) = [138.000000000,-0.562958435]
  cl(0:1, 186) = [141.000000000,-0.576405141]
  cl(0:1, 187) = [144.000000000,-0.585129730]
  cl(0:1, 188) = [147.000000000,-0.593851743]
  cl(0:1, 189) = [150.000000000,-0.602571441]
  cl(0:1, 190) = [153.000000000,-0.618102771]
  cl(0:1, 191) = [156.000000000,-0.633627994]
  cl(0:1, 192) = [159.000000000,-0.649147891]
  cl(0:1, 193) = [162.000000000,-0.611662753]
  cl(0:1, 194) = [165.000000000,-0.547687467]
  cl(0:1, 195) = [168.000000000,-0.483692710]
  cl(0:1, 196) = [171.000000000,-0.396924138]
  cl(0:1, 197) = [174.000000000,-0.264615542]
  cl(0:1, 198) = [177.000000000,-0.131704480]
  cl(0:1, 199) = [180.000000000,0.000000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl5_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl5 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl5_ge28.pro'
  ENDIF

!print,y,cl5


!_________________________________________________________________________
  
END FUNCTION calc_cl5_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl6_ge28( y ) RESULT( cl6 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl6
  INTEGER :: i

  cl6 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.00000000,0.00000000]
  cl(0:1, 1  ) = [-177.00000000,0.14324499]
  cl(0:1, 2  ) = [-174.00000000,0.28780229]
  cl(0:1, 3  ) = [-171.00000000,0.43170433]
  cl(0:1, 4  ) = [-168.00000000,0.52349971]
  cl(0:1, 5  ) = [-165.00000000,0.58338443]
  cl(0:1, 6  ) = [-162.00000000,0.64305380]
  cl(0:1, 7  ) = [-159.00000000,0.67845107]
  cl(0:1, 8  ) = [-156.00000000,0.65889304]
  cl(0:1, 9  ) = [-153.00000000,0.63936102]
  cl(0:1, 10 ) = [-150.00000000,0.61986330]
  cl(0:1, 11 ) = [-147.00000000,0.60900947]
  cl(0:1, 12 ) = [-144.00000000,0.59817115]
  cl(0:1, 13 ) = [-141.00000000,0.58735123]
  cl(0:1, 14 ) = [-138.00000000,0.57233073]
  cl(0:1, 15 ) = [-135.00000000,0.55520953]
  cl(0:1, 16 ) = [-132.00000000,0.53809737]
  cl(0:1, 17 ) = [-129.00000000,0.51751132]
  cl(0:1, 18 ) = [-126.00000000,0.48996173]
  cl(0:1, 19 ) = [-123.00000000,0.46241617]
  cl(0:1, 20 ) = [-120.00000000,0.43487552]
  cl(0:1, 21 ) = [-117.00000000,0.39714125]
  cl(0:1, 22 ) = [-114.00000000,0.35940861]
  cl(0:1, 23 ) = [-111.00000000,0.32167837]
  cl(0:1, 24 ) = [-108.00000000,0.27908806]
  cl(0:1, 25 ) = [-105.00000000,0.23406715]
  cl(0:1, 26 ) = [-102.00000000,0.18904755]
  cl(0:1, 27 ) = [-99.00000000,0.14313170]
  cl(0:1, 28 ) = [-96.00000000,0.09542093]
  cl(0:1, 29 ) = [-93.00000000,0.04771052]
  cl(0:1, 30 ) = [-90.00000000,0.00000030]
  cl(0:1, 31 ) = [-87.00000000,-0.04771047]
  cl(0:1, 32 ) = [-84.00000000,-0.09542107]
  cl(0:1, 33 ) = [-81.00000000,-0.14313129]
  cl(0:1, 34 ) = [-78.00000000,-0.18904733]
  cl(0:1, 35 ) = [-75.00000000,-0.23406745]
  cl(0:1, 36 ) = [-72.00000000,-0.27908784]
  cl(0:1, 37 ) = [-69.00000000,-0.32167804]
  cl(0:1, 38 ) = [-66.00000000,-0.35940872]
  cl(0:1, 39 ) = [-63.00000000,-0.39714150]
  cl(0:1, 40 ) = [-60.00000000,-0.43487571]
  cl(0:1, 41 ) = [-57.00000000,-0.46241635]
  cl(0:1, 42 ) = [-54.00000000,-0.48996191]
  cl(0:1, 43 ) = [-51.00000000,-0.51751150]
  cl(0:1, 44 ) = [-48.00000000,-0.53808652]
  cl(0:1, 45 ) = [-45.00000000,-0.55517590]
  cl(0:1, 46 ) = [-42.00000000,-0.57231398]
  cl(0:1, 47 ) = [-39.00000000,-0.58734712]
  cl(0:1, 48 ) = [-36.00000000,-0.59816118]
  cl(0:1, 49 ) = [-33.00000000,-0.60900845]
  cl(0:1, 50 ) = [-30.00000000,-0.61986341]
  cl(0:1, 51 ) = [-29.39393939,-0.62384676]
  cl(0:1, 52 ) = [-28.78787879,-0.62783878]
  cl(0:1, 53 ) = [-28.18181818,-0.63183964]
  cl(0:1, 54 ) = [-27.57575758,-0.63583627]
  cl(0:1, 55 ) = [-26.96969697,-0.63983391]
  cl(0:1, 56 ) = [-26.36363636,-0.64383781]
  cl(0:1, 57 ) = [-25.75757576,-0.64783373]
  cl(0:1, 58 ) = [-25.15151515,-0.65181188]
  cl(0:1, 59 ) = [-24.54545455,-0.65579240]
  cl(0:1, 60 ) = [-23.93939394,-0.65976933]
  cl(0:1, 61 ) = [-23.33333333,-0.66369375]
  cl(0:1, 62 ) = [-22.72727273,-0.66761728]
  cl(0:1, 63 ) = [-22.12121212,-0.67153993]
  cl(0:1, 64 ) = [-21.51515152,-0.67539136]
  cl(0:1, 65 ) = [-20.90909091,-0.67922506]
  cl(0:1, 66 ) = [-20.30303030,-0.68305859]
  cl(0:1, 67 ) = [-19.69696970,-0.67717937]
  cl(0:1, 68 ) = [-19.09090909,-0.66158881]
  cl(0:1, 69 ) = [-18.48484848,-0.64599897]
  cl(0:1, 70 ) = [-17.87878788,-0.63040795]
  cl(0:1, 71 ) = [-17.27272727,-0.61481408]
  cl(0:1, 72 ) = [-16.66666667,-0.59921723]
  cl(0:1, 73 ) = [-16.06060606,-0.58361658]
  cl(0:1, 74 ) = [-15.45454545,-0.56801217]
  cl(0:1, 75 ) = [-14.84848485,-0.55240234]
  cl(0:1, 76 ) = [-14.24242424,-0.53678584]
  cl(0:1, 77 ) = [-13.63636364,-0.52116495]
  cl(0:1, 78 ) = [-13.03030303,-0.50553227]
  cl(0:1, 79 ) = [-12.42424242,-0.48989616]
  cl(0:1, 80 ) = [-11.81818182,-0.47424723]
  cl(0:1, 81 ) = [-11.21212121,-0.45859143]
  cl(0:1, 82 ) = [-10.60606061,-0.44292501]
  cl(0:1, 83 ) = [-10.00000000,-0.42724542]
  cl(0:1, 84 ) = [-9.39393939,-0.39445007]
  cl(0:1, 85 ) = [-8.78787879,-0.36170976]
  cl(0:1, 86 ) = [-8.18181818,-0.32900477]
  cl(0:1, 87 ) = [-7.57575758,-0.29671946]
  cl(0:1, 88 ) = [-6.96969697,-0.26460765]
  cl(0:1, 89 ) = [-6.36363636,-0.23258053]
  cl(0:1, 90 ) = [-5.75757576,-0.20063317]
  cl(0:1, 91 ) = [-5.15151515,-0.16886611]
  cl(0:1, 92 ) = [-4.54545455,-0.13746193]
  cl(0:1, 93 ) = [-3.93939394,-0.10675845]
  cl(0:1, 94 ) = [-3.33333333,-0.07626855]
  cl(0:1, 95 ) = [-2.72727273,-0.02304656]
  cl(0:1, 96 ) = [-2.12121212,0.05608352]
  cl(0:1, 97 ) = [-1.51515152,0.13397326]
  cl(0:1, 98 ) = [-0.90909091,0.21102809]
  cl(0:1, 99 ) = [-0.30303030,0.28726137]
  cl(0:1, 100) = [0.30303030,0.36268347]
  cl(0:1, 101) = [0.90909091,0.43719956]
  cl(0:1, 102) = [1.51515152,0.51361065]
  cl(0:1, 103) = [2.12121212,0.59381952]
  cl(0:1, 104) = [2.72727273,0.67438674]
  cl(0:1, 105) = [3.33333333,0.75494504]
  cl(0:1, 106) = [3.93939394,0.83550909]
  cl(0:1, 107) = [4.54545455,0.91606432]
  cl(0:1, 108) = [5.15151515,0.99662579]
  cl(0:1, 109) = [5.75757576,1.07656302]
  cl(0:1, 110) = [6.36363636,1.15542943]
  cl(0:1, 111) = [6.96969697,1.23410924]
  cl(0:1, 112) = [7.57575758,1.31245194]
  cl(0:1, 113) = [8.18181818,1.38759944]
  cl(0:1, 114) = [8.78787879,1.45873319]
  cl(0:1, 115) = [9.39393939,1.52667303]
  cl(0:1, 116) = [10.00000000,1.59364180]
  cl(0:1, 117) = [10.60606061,1.53266180]
  cl(0:1, 118) = [11.21212121,1.50662714]
  cl(0:1, 119) = [11.81818182,1.52196088]
  cl(0:1, 120) = [12.42424242,1.06125738]
  cl(0:1, 121) = [13.03030303,0.98662365]
  cl(0:1, 122) = [13.63636364,0.99541390]
  cl(0:1, 123) = [14.24242424,1.00924723]
  cl(0:1, 124) = [14.84848485,1.02661322]
  cl(0:1, 125) = [15.45454545,1.05138532]
  cl(0:1, 126) = [16.06060606,1.07964132]
  cl(0:1, 127) = [16.66666667,1.06432412]
  cl(0:1, 128) = [17.27272727,1.04867809]
  cl(0:1, 129) = [17.87878788,1.03269241]
  cl(0:1, 130) = [18.48484848,1.01640008]
  cl(0:1, 131) = [19.09090909,1.00063944]
  cl(0:1, 132) = [19.69696970,0.98593842]
  cl(0:1, 133) = [20.30303030,0.97586006]
  cl(0:1, 134) = [20.90909091,0.97038552]
  cl(0:1, 135) = [21.51515152,0.96491074]
  cl(0:1, 136) = [22.12121212,0.95941054]
  cl(0:1, 137) = [22.72727273,0.95380841]
  cl(0:1, 138) = [23.33333333,0.94820501]
  cl(0:1, 139) = [23.93939394,0.94260033]
  cl(0:1, 140) = [24.54545455,0.93692046]
  cl(0:1, 141) = [25.15151515,0.93123545]
  cl(0:1, 142) = [25.75757576,0.92555386]
  cl(0:1, 143) = [26.36363636,0.91984685]
  cl(0:1, 144) = [26.96969697,0.91412844]
  cl(0:1, 145) = [27.57575758,0.90841902]
  cl(0:1, 146) = [28.18181818,0.90271106]
  cl(0:1, 147) = [28.78787879,0.89699706]
  cl(0:1, 148) = [29.39393939,0.89129575]
  cl(0:1, 149) = [30.00000000,0.88560689]
  cl(0:1, 150) = [33.00000000,0.87004662]
  cl(0:1, 151) = [36.00000000,0.85449776]
  cl(0:1, 152) = [39.00000000,0.83899689]
  cl(0:1, 153) = [42.00000000,0.81752230]
  cl(0:1, 154) = [45.00000000,0.79306655]
  cl(0:1, 155) = [48.00000000,0.76867993]
  cl(0:1, 156) = [51.00000000,0.73930372]
  cl(0:1, 157) = [54.00000000,0.69994445]
  cl(0:1, 158) = [57.00000000,0.66059091]
  cl(0:1, 159) = [60.00000000,0.62124434]
  cl(0:1, 160) = [63.00000000,0.56734224]
  cl(0:1, 161) = [66.00000000,0.51344241]
  cl(0:1, 162) = [69.00000000,0.45954593]
  cl(0:1, 163) = [72.00000000,0.39872008]
  cl(0:1, 164) = [75.00000000,0.33442875]
  cl(0:1, 165) = [78.00000000,0.27013776]
  cl(0:1, 166) = [81.00000000,0.20455043]
  cl(0:1, 167) = [84.00000000,0.13636725]
  cl(0:1, 168) = [87.00000000,0.06818354]
  cl(0:1, 169) = [90.00000000,-0.00000030]
  cl(0:1, 170) = [93.00000000,-0.04771052]
  cl(0:1, 171) = [96.00000000,-0.09542093]
  cl(0:1, 172) = [99.00000000,-0.14313170]
  cl(0:1, 173) = [102.00000000,-0.18904755]
  cl(0:1, 174) = [105.00000000,-0.23406715]
  cl(0:1, 175) = [108.00000000,-0.27908806]
  cl(0:1, 176) = [111.00000000,-0.32167837]
  cl(0:1, 177) = [114.00000000,-0.35940861]
  cl(0:1, 178) = [117.00000000,-0.39714125]
  cl(0:1, 179) = [120.00000000,-0.43487552]
  cl(0:1, 180) = [123.00000000,-0.46241617]
  cl(0:1, 181) = [126.00000000,-0.48996173]
  cl(0:1, 182) = [129.00000000,-0.51751132]
  cl(0:1, 183) = [132.00000000,-0.53809737]
  cl(0:1, 184) = [135.00000000,-0.55520953]
  cl(0:1, 185) = [138.00000000,-0.57233073]
  cl(0:1, 186) = [141.00000000,-0.58735123]
  cl(0:1, 187) = [144.00000000,-0.59817115]
  cl(0:1, 188) = [147.00000000,-0.60900947]
  cl(0:1, 189) = [150.00000000,-0.61986330]
  cl(0:1, 190) = [153.00000000,-0.63936102]
  cl(0:1, 191) = [156.00000000,-0.65889304]
  cl(0:1, 192) = [159.00000000,-0.67845107]
  cl(0:1, 193) = [162.00000000,-0.64350175]
  cl(0:1, 194) = [165.00000000,-0.58835932]
  cl(0:1, 195) = [168.00000000,-0.52349971]
  cl(0:1, 196) = [171.00000000,-0.43170433]
  cl(0:1, 197) = [174.00000000,-0.28780229]
  cl(0:1, 198) = [177.00000000,-0.14324499]
  cl(0:1, 199) = [180.00000000,0.00000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl6_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl6 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl6_ge28.pro'
  ENDIF

!print,y,cl6

!_________________________________________________________________________
  
END FUNCTION calc_cl6_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl7_ge28( y ) RESULT( cl7 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl7
  INTEGER :: i

  cl7 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.1471570]
  cl(0:1, 2  ) = [-174.0000000,0.2956622]
  cl(0:1, 3  ) = [-171.0000000,0.4434942]
  cl(0:1, 4  ) = [-168.0000000,0.5368487]
  cl(0:1, 5  ) = [-165.0000000,0.6015601]
  cl(0:1, 6  ) = [-162.0000000,0.6662238]
  cl(0:1, 7  ) = [-159.0000000,0.7023166]
  cl(0:1, 8  ) = [-156.0000000,0.6797261]
  cl(0:1, 9  ) = [-153.0000000,0.6571493]
  cl(0:1, 10 ) = [-150.0000000,0.6345897]
  cl(0:1, 11 ) = [-147.0000000,0.6220610]
  cl(0:1, 12 ) = [-144.0000000,0.6095393]
  cl(0:1, 13 ) = [-141.0000000,0.5970257]
  cl(0:1, 14 ) = [-138.0000000,0.5807041]
  cl(0:1, 15 ) = [-135.0000000,0.5624781]
  cl(0:1, 16 ) = [-132.0000000,0.5442556]
  cl(0:1, 17 ) = [-129.0000000,0.5226785]
  cl(0:1, 18 ) = [-126.0000000,0.4943859]
  cl(0:1, 19 ) = [-123.0000000,0.4660950]
  cl(0:1, 20 ) = [-120.0000000,0.4378060]
  cl(0:1, 21 ) = [-117.0000000,0.3995774]
  cl(0:1, 22 ) = [-114.0000000,0.3613495]
  cl(0:1, 23 ) = [-111.0000000,0.3231227]
  cl(0:1, 24 ) = [-108.0000000,0.2801733]
  cl(0:1, 25 ) = [-105.0000000,0.2348625]
  cl(0:1, 26 ) = [-102.0000000,0.1895524]
  cl(0:1, 27 ) = [-99.0000000,0.1434114]
  cl(0:1, 28 ) = [-96.0000000,0.0956074]
  cl(0:1, 29 ) = [-93.0000000,0.0478037]
  cl(0:1, 30 ) = [-90.0000000,0.0000003]
  cl(0:1, 31 ) = [-87.0000000,-0.0478037]
  cl(0:1, 32 ) = [-84.0000000,-0.0956075]
  cl(0:1, 33 ) = [-81.0000000,-0.1434110]
  cl(0:1, 34 ) = [-78.0000000,-0.1895522]
  cl(0:1, 35 ) = [-75.0000000,-0.2348628]
  cl(0:1, 36 ) = [-72.0000000,-0.2801731]
  cl(0:1, 37 ) = [-69.0000000,-0.3231223]
  cl(0:1, 38 ) = [-66.0000000,-0.3613496]
  cl(0:1, 39 ) = [-63.0000000,-0.3995777]
  cl(0:1, 40 ) = [-60.0000000,-0.4378062]
  cl(0:1, 41 ) = [-57.0000000,-0.4660952]
  cl(0:1, 42 ) = [-54.0000000,-0.4943861]
  cl(0:1, 43 ) = [-51.0000000,-0.5226787]
  cl(0:1, 44 ) = [-48.0000000,-0.5442530]
  cl(0:1, 45 ) = [-45.0000000,-0.5624698]
  cl(0:1, 46 ) = [-42.0000000,-0.5807000]
  cl(0:1, 47 ) = [-39.0000000,-0.5970248]
  cl(0:1, 48 ) = [-36.0000000,-0.6095370]
  cl(0:1, 49 ) = [-33.0000000,-0.6220607]
  cl(0:1, 50 ) = [-30.0000000,-0.6345898]
  cl(0:1, 51 ) = [-29.3939394,-0.6391571]
  cl(0:1, 52 ) = [-28.7878788,-0.6437270]
  cl(0:1, 53 ) = [-28.1818182,-0.6482993]
  cl(0:1, 54 ) = [-27.5757576,-0.6528711]
  cl(0:1, 55 ) = [-26.9696970,-0.6574436]
  cl(0:1, 56 ) = [-26.3636364,-0.6620179]
  cl(0:1, 57 ) = [-25.7575758,-0.6665906]
  cl(0:1, 58 ) = [-25.1515152,-0.6711594]
  cl(0:1, 59 ) = [-24.5454545,-0.6757290]
  cl(0:1, 60 ) = [-23.9393939,-0.6802980]
  cl(0:1, 61 ) = [-23.3333333,-0.6848548]
  cl(0:1, 62 ) = [-22.7272727,-0.6894116]
  cl(0:1, 63 ) = [-22.1212121,-0.6939686]
  cl(0:1, 64 ) = [-21.5151515,-0.6985087]
  cl(0:1, 65 ) = [-20.9090909,-0.7030449]
  cl(0:1, 66 ) = [-20.3030303,-0.7075813]
  cl(0:1, 67 ) = [-19.6969697,-0.7024341]
  cl(0:1, 68 ) = [-19.0909091,-0.6876050]
  cl(0:1, 69 ) = [-18.4848485,-0.6727781]
  cl(0:1, 70 ) = [-17.8787879,-0.6579523]
  cl(0:1, 71 ) = [-17.2727273,-0.6431270]
  cl(0:1, 72 ) = [-16.6666667,-0.6283025]
  cl(0:1, 73 ) = [-16.0606061,-0.6134789]
  cl(0:1, 74 ) = [-15.4545455,-0.5986568]
  cl(0:1, 75 ) = [-14.8484848,-0.5838351]
  cl(0:1, 76 ) = [-14.2424242,-0.5690134]
  cl(0:1, 77 ) = [-13.6363636,-0.5541928]
  cl(0:1, 78 ) = [-13.0303030,-0.5393713]
  cl(0:1, 79 ) = [-12.4242424,-0.5245515]
  cl(0:1, 80 ) = [-11.8181818,-0.5097309]
  cl(0:1, 81 ) = [-11.2121212,-0.4949118]
  cl(0:1, 82 ) = [-10.6060606,-0.4800919]
  cl(0:1, 83 ) = [-10.0000000,-0.4652726]
  cl(0:1, 84 ) = [-9.3939394,-0.4330152]
  cl(0:1, 85 ) = [-8.7878788,-0.4007766]
  cl(0:1, 86 ) = [-8.1818182,-0.3685504]
  cl(0:1, 87 ) = [-7.5757576,-0.3364343]
  cl(0:1, 88 ) = [-6.9696970,-0.3043750]
  cl(0:1, 89 ) = [-6.3636364,-0.2723497]
  cl(0:1, 90 ) = [-5.7575758,-0.2403584]
  cl(0:1, 91 ) = [-5.1515152,-0.2084323]
  cl(0:1, 92 ) = [-4.5454545,-0.1766100]
  cl(0:1, 93 ) = [-3.9393939,-0.1449976]
  cl(0:1, 94 ) = [-3.3333333,-0.1138636]
  cl(0:1, 95 ) = [-2.7272727,-0.0599893]
  cl(0:1, 96 ) = [-2.1212121,0.0227565]
  cl(0:1, 97 ) = [-1.5151515,0.1052000]
  cl(0:1, 98 ) = [-0.9090909,0.1874431]
  cl(0:1, 99 ) = [-0.3030303,0.2694925]
  cl(0:1, 100) = [0.3030303,0.3513412]
  cl(0:1, 101) = [0.9090909,0.4329771]
  cl(0:1, 102) = [1.5151515,0.5148186]
  cl(0:1, 103) = [2.1212121,0.5958760]
  cl(0:1, 104) = [2.7272727,0.6763641]
  cl(0:1, 105) = [3.3333333,0.7568464]
  cl(0:1, 106) = [3.9393939,0.8373354]
  cl(0:1, 107) = [4.5454545,0.9178193]
  cl(0:1, 108) = [5.1515152,0.9983085]
  cl(0:1, 109) = [5.7575758,1.0787851]
  cl(0:1, 110) = [6.3636364,1.1592572]
  cl(0:1, 111) = [6.9696970,1.2397241]
  cl(0:1, 112) = [7.5757576,1.3197965]
  cl(0:1, 113) = [8.1818182,1.3957435]
  cl(0:1, 114) = [8.7878788,1.4684528]
  cl(0:1, 115) = [9.3939394,1.5411367]
  cl(0:1, 116) = [10.0000000,1.6138066]
  cl(0:1, 117) = [10.6060606,1.6683597]
  cl(0:1, 118) = [11.2121212,1.7069274]
  cl(0:1, 119) = [11.8181818,1.7458763]
  cl(0:1, 120) = [12.4242424,1.0978272]
  cl(0:1, 121) = [13.0303030,0.9734028]
  cl(0:1, 122) = [13.6363636,0.9884858]
  cl(0:1, 123) = [14.2424242,1.0109065]
  cl(0:1, 124) = [14.8484848,1.0428284]
  cl(0:1, 125) = [15.4545455,1.0826904]
  cl(0:1, 126) = [16.0606061,1.1206944]
  cl(0:1, 127) = [16.6666667,1.1042863]
  cl(0:1, 128) = [17.2727273,1.0878429]
  cl(0:1, 129) = [17.8787879,1.0713701]
  cl(0:1, 130) = [18.4848485,1.0548246]
  cl(0:1, 131) = [19.0909091,1.0384077]
  cl(0:1, 132) = [19.6969697,1.0222461]
  cl(0:1, 133) = [20.3030303,1.0109270]
  cl(0:1, 134) = [20.9090909,1.0044458]
  cl(0:1, 135) = [21.5151515,0.9979650]
  cl(0:1, 136) = [22.1212121,0.9914784]
  cl(0:1, 137) = [22.7272727,0.9849679]
  cl(0:1, 138) = [23.3333333,0.9784575]
  cl(0:1, 139) = [23.9393939,0.9719472]
  cl(0:1, 140) = [24.5454545,0.9654193]
  cl(0:1, 141) = [25.1515152,0.9588905]
  cl(0:1, 142) = [25.7575758,0.9523631]
  cl(0:1, 143) = [26.3636364,0.9458300]
  cl(0:1, 144) = [26.9696970,0.9392946]
  cl(0:1, 145) = [27.5757576,0.9327618]
  cl(0:1, 146) = [28.1818182,0.9262300]
  cl(0:1, 147) = [28.7878788,0.9196974]
  cl(0:1, 148) = [29.3939394,0.9131684]
  cl(0:1, 149) = [30.0000000,0.9066430]
  cl(0:1, 150) = [33.0000000,0.8886887]
  cl(0:1, 151) = [36.0000000,0.8707422]
  cl(0:1, 152) = [39.0000000,0.8528123]
  cl(0:1, 153) = [42.0000000,0.8295053]
  cl(0:1, 154) = [45.0000000,0.8035105]
  cl(0:1, 155) = [48.0000000,0.7775346]
  cl(0:1, 156) = [51.0000000,0.7467448]
  cl(0:1, 157) = [54.0000000,0.7063226]
  cl(0:1, 158) = [57.0000000,0.6659028]
  cl(0:1, 159) = [60.0000000,0.6254858]
  cl(0:1, 160) = [63.0000000,0.5708556]
  cl(0:1, 161) = [66.0000000,0.5162262]
  cl(0:1, 162) = [69.0000000,0.4615980]
  cl(0:1, 163) = [72.0000000,0.4002555]
  cl(0:1, 164) = [75.0000000,0.3355555]
  cl(0:1, 165) = [78.0000000,0.2708552]
  cl(0:1, 166) = [81.0000000,0.2049499]
  cl(0:1, 167) = [84.0000000,0.1366336]
  cl(0:1, 168) = [87.0000000,0.0683167]
  cl(0:1, 169) = [90.0000000,-0.0000003]
  cl(0:1, 170) = [93.0000000,-0.0478037]
  cl(0:1, 171) = [96.0000000,-0.0956074]
  cl(0:1, 172) = [99.0000000,-0.1434114]
  cl(0:1, 173) = [102.0000000,-0.1895524]
  cl(0:1, 174) = [105.0000000,-0.2348625]
  cl(0:1, 175) = [108.0000000,-0.2801733]
  cl(0:1, 176) = [111.0000000,-0.3231227]
  cl(0:1, 177) = [114.0000000,-0.3613495]
  cl(0:1, 178) = [117.0000000,-0.3995774]
  cl(0:1, 179) = [120.0000000,-0.4378060]
  cl(0:1, 180) = [123.0000000,-0.4660950]
  cl(0:1, 181) = [126.0000000,-0.4943859]
  cl(0:1, 182) = [129.0000000,-0.5226785]
  cl(0:1, 183) = [132.0000000,-0.5442556]
  cl(0:1, 184) = [135.0000000,-0.5624781]
  cl(0:1, 185) = [138.0000000,-0.5807041]
  cl(0:1, 186) = [141.0000000,-0.5970257]
  cl(0:1, 187) = [144.0000000,-0.6095393]
  cl(0:1, 188) = [147.0000000,-0.6220610]
  cl(0:1, 189) = [150.0000000,-0.6345897]
  cl(0:1, 190) = [153.0000000,-0.6571493]
  cl(0:1, 191) = [156.0000000,-0.6797261]
  cl(0:1, 192) = [159.0000000,-0.7023166]
  cl(0:1, 193) = [162.0000000,-0.6663315]
  cl(0:1, 194) = [165.0000000,-0.6027568]
  cl(0:1, 195) = [168.0000000,-0.5368487]
  cl(0:1, 196) = [171.0000000,-0.4434942]
  cl(0:1, 197) = [174.0000000,-0.2956622]
  cl(0:1, 198) = [177.0000000,-0.1471570]
  cl(0:1, 199) = [180.0000000,0.0000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl7_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl7 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl7_ge28.pro'
  ENDIF

!print,y,cl7

!_________________________________________________________________________
  
END FUNCTION calc_cl7_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl8_ge28( y ) RESULT( cl8 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl8
  INTEGER :: i

  cl8 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.1430315]
  cl(0:1, 2  ) = [-174.0000000,0.2879909]
  cl(0:1, 3  ) = [-171.0000000,0.4319871]
  cl(0:1, 4  ) = [-168.0000000,0.5315458]
  cl(0:1, 5  ) = [-165.0000000,0.6112685]
  cl(0:1, 6  ) = [-162.0000000,0.6946001]
  cl(0:1, 7  ) = [-159.0000000,0.7408245]
  cl(0:1, 8  ) = [-156.0000000,0.7161397]
  cl(0:1, 9  ) = [-153.0000000,0.6914239]
  cl(0:1, 10 ) = [-150.0000000,0.6666701]
  cl(0:1, 11 ) = [-147.0000000,0.6529009]
  cl(0:1, 12 ) = [-144.0000000,0.6391160]
  cl(0:1, 13 ) = [-141.0000000,0.6253130]
  cl(0:1, 14 ) = [-138.0000000,0.6077281]
  cl(0:1, 15 ) = [-135.0000000,0.5882529]
  cl(0:1, 16 ) = [-132.0000000,0.5687692]
  cl(0:1, 17 ) = [-129.0000000,0.5458465]
  cl(0:1, 18 ) = [-126.0000000,0.5160599]
  cl(0:1, 19 ) = [-123.0000000,0.4862701]
  cl(0:1, 20 ) = [-120.0000000,0.4564763]
  cl(0:1, 21 ) = [-117.0000000,0.4164896]
  cl(0:1, 22 ) = [-114.0000000,0.3765018]
  cl(0:1, 23 ) = [-111.0000000,0.3365126]
  cl(0:1, 24 ) = [-108.0000000,0.2916930]
  cl(0:1, 25 ) = [-105.0000000,0.2444587]
  cl(0:1, 26 ) = [-102.0000000,0.1972242]
  cl(0:1, 27 ) = [-99.0000000,0.1491605]
  cl(0:1, 28 ) = [-96.0000000,0.0994402]
  cl(0:1, 29 ) = [-93.0000000,0.0497202]
  cl(0:1, 30 ) = [-90.0000000,0.0000003]
  cl(0:1, 31 ) = [-87.0000000,-0.0497202]
  cl(0:1, 32 ) = [-84.0000000,-0.0994404]
  cl(0:1, 33 ) = [-81.0000000,-0.1491601]
  cl(0:1, 34 ) = [-78.0000000,-0.1972240]
  cl(0:1, 35 ) = [-75.0000000,-0.2444590]
  cl(0:1, 36 ) = [-72.0000000,-0.2916928]
  cl(0:1, 37 ) = [-69.0000000,-0.3365123]
  cl(0:1, 38 ) = [-66.0000000,-0.3765019]
  cl(0:1, 39 ) = [-63.0000000,-0.4164899]
  cl(0:1, 40 ) = [-60.0000000,-0.4564765]
  cl(0:1, 41 ) = [-57.0000000,-0.4862703]
  cl(0:1, 42 ) = [-54.0000000,-0.5160601]
  cl(0:1, 43 ) = [-51.0000000,-0.5458467]
  cl(0:1, 44 ) = [-48.0000000,-0.5692899]
  cl(0:1, 45 ) = [-45.0000000,-0.5895577]
  cl(0:1, 46 ) = [-42.0000000,-0.6082518]
  cl(0:1, 47 ) = [-39.0000000,-0.6254169]
  cl(0:1, 48 ) = [-36.0000000,-0.6393169]
  cl(0:1, 49 ) = [-33.0000000,-0.6529164]
  cl(0:1, 50 ) = [-30.0000000,-0.6666702]
  cl(0:1, 51 ) = [-29.3939394,-0.6710474]
  cl(0:1, 52 ) = [-28.7878788,-0.6754191]
  cl(0:1, 53 ) = [-28.1818182,-0.6797854]
  cl(0:1, 54 ) = [-27.5757576,-0.6842812]
  cl(0:1, 55 ) = [-26.9696970,-0.6888307]
  cl(0:1, 56 ) = [-26.3636364,-0.6933762]
  cl(0:1, 57 ) = [-25.7575758,-0.6980379]
  cl(0:1, 58 ) = [-25.1515152,-0.7028774]
  cl(0:1, 59 ) = [-24.5454545,-0.7077149]
  cl(0:1, 60 ) = [-23.9393939,-0.7125952]
  cl(0:1, 61 ) = [-23.3333333,-0.7178763]
  cl(0:1, 62 ) = [-22.7272727,-0.7231572]
  cl(0:1, 63 ) = [-22.1212121,-0.7284379]
  cl(0:1, 64 ) = [-21.5151515,-0.7317134]
  cl(0:1, 65 ) = [-20.9090909,-0.7342458]
  cl(0:1, 66 ) = [-20.3030303,-0.7380656]
  cl(0:1, 67 ) = [-19.6969697,-0.7328088]
  cl(0:1, 68 ) = [-19.0909091,-0.7183620]
  cl(0:1, 69 ) = [-18.4848485,-0.7041896]
  cl(0:1, 70 ) = [-17.8787879,-0.6901705]
  cl(0:1, 71 ) = [-17.2727273,-0.6762451]
  cl(0:1, 72 ) = [-16.6666667,-0.6623814]
  cl(0:1, 73 ) = [-16.0606061,-0.6485603]
  cl(0:1, 74 ) = [-15.4545455,-0.6349695]
  cl(0:1, 75 ) = [-14.8484848,-0.6215085]
  cl(0:1, 76 ) = [-14.2424242,-0.6084084]
  cl(0:1, 77 ) = [-13.6363636,-0.5954472]
  cl(0:1, 78 ) = [-13.0303030,-0.5826463]
  cl(0:1, 79 ) = [-12.4242424,-0.5700513]
  cl(0:1, 80 ) = [-11.8181818,-0.5576620]
  cl(0:1, 81 ) = [-11.2121212,-0.5455211]
  cl(0:1, 82 ) = [-10.6060606,-0.5337706]
  cl(0:1, 83 ) = [-10.0000000,-0.5225363]
  cl(0:1, 84 ) = [-9.3939394,-0.4986630]
  cl(0:1, 85 ) = [-8.7878788,-0.4748966]
  cl(0:1, 86 ) = [-8.1818182,-0.4501847]
  cl(0:1, 87 ) = [-7.5757576,-0.4163209]
  cl(0:1, 88 ) = [-6.9696970,-0.3785339]
  cl(0:1, 89 ) = [-6.3636364,-0.3413084]
  cl(0:1, 90 ) = [-5.7575758,-0.3032921]
  cl(0:1, 91 ) = [-5.1515152,-0.2639178]
  cl(0:1, 92 ) = [-4.5454545,-0.2228248]
  cl(0:1, 93 ) = [-3.9393939,-0.1806688]
  cl(0:1, 94 ) = [-3.3333333,-0.1341766]
  cl(0:1, 95 ) = [-2.7272727,-0.0719454]
  cl(0:1, 96 ) = [-2.1212121,0.0070761]
  cl(0:1, 97 ) = [-1.5151515,0.0873290]
  cl(0:1, 98 ) = [-0.9090909,0.1682146]
  cl(0:1, 99 ) = [-0.3030303,0.2498476]
  cl(0:1, 100) = [0.3030303,0.3320767]
  cl(0:1, 101) = [0.9090909,0.4149679]
  cl(0:1, 102) = [1.5151515,0.4975034]
  cl(0:1, 103) = [2.1212121,0.5780170]
  cl(0:1, 104) = [2.7272727,0.6579825]
  cl(0:1, 105) = [3.3333333,0.7376348]
  cl(0:1, 106) = [3.9393939,0.8170422]
  cl(0:1, 107) = [4.5454545,0.8957736]
  cl(0:1, 108) = [5.1515152,0.9742026]
  cl(0:1, 109) = [5.7575758,1.0519261]
  cl(0:1, 110) = [6.3636364,1.1286485]
  cl(0:1, 111) = [6.9696970,1.2046968]
  cl(0:1, 112) = [7.5757576,1.2785239]
  cl(0:1, 113) = [8.1818182,1.3484362]
  cl(0:1, 114) = [8.7878788,1.4138926]
  cl(0:1, 115) = [9.3939394,1.4769311]
  cl(0:1, 116) = [10.0000000,1.5386615]
  cl(0:1, 117) = [10.6060606,1.5817563]
  cl(0:1, 118) = [11.2121212,1.6085343]
  cl(0:1, 119) = [11.8181818,1.6261812]
  cl(0:1, 120) = [12.4242424,1.1708112]
  cl(0:1, 121) = [13.0303030,1.0705727]
  cl(0:1, 122) = [13.6363636,1.0732379]
  cl(0:1, 123) = [14.2424242,1.0820979]
  cl(0:1, 124) = [14.8484848,1.1035499]
  cl(0:1, 125) = [15.4545455,1.1309010]
  cl(0:1, 126) = [16.0606061,1.1529033]
  cl(0:1, 127) = [16.6666667,1.1388878]
  cl(0:1, 128) = [17.2727273,1.1256987]
  cl(0:1, 129) = [17.8787879,1.1134900]
  cl(0:1, 130) = [18.4848485,1.1041227]
  cl(0:1, 131) = [19.0909091,1.0941762]
  cl(0:1, 132) = [19.6969697,1.0781567]
  cl(0:1, 133) = [20.3030303,1.0659092]
  cl(0:1, 134) = [20.9090909,1.0574446]
  cl(0:1, 135) = [21.5151515,1.0489792]
  cl(0:1, 136) = [22.1212121,1.0406974]
  cl(0:1, 137) = [22.7272727,1.0331525]
  cl(0:1, 138) = [23.3333333,1.0256073]
  cl(0:1, 139) = [23.9393939,1.0180618]
  cl(0:1, 140) = [24.5454545,1.0110891]
  cl(0:1, 141) = [25.1515152,1.0041774]
  cl(0:1, 142) = [25.7575758,0.9972629]
  cl(0:1, 143) = [26.3636364,0.9906023]
  cl(0:1, 144) = [26.9696970,0.9841077]
  cl(0:1, 145) = [27.5757576,0.9776073]
  cl(0:1, 146) = [28.1818182,0.9711835]
  cl(0:1, 147) = [28.7878788,0.9649451]
  cl(0:1, 148) = [29.3939394,0.9586989]
  cl(0:1, 149) = [30.0000000,0.9524449]
  cl(0:1, 150) = [33.0000000,0.9327571]
  cl(0:1, 151) = [36.0000000,0.9132906]
  cl(0:1, 152) = [39.0000000,0.8933938]
  cl(0:1, 153) = [42.0000000,0.8688866]
  cl(0:1, 154) = [45.0000000,0.8422230]
  cl(0:1, 155) = [48.0000000,0.8133111]
  cl(0:1, 156) = [51.0000000,0.7798482]
  cl(0:1, 157) = [54.0000000,0.7372926]
  cl(0:1, 158) = [57.0000000,0.6947327]
  cl(0:1, 159) = [60.0000000,0.6521673]
  cl(0:1, 160) = [63.0000000,0.5950215]
  cl(0:1, 161) = [66.0000000,0.5378737]
  cl(0:1, 162) = [69.0000000,0.4807231]
  cl(0:1, 163) = [72.0000000,0.4167029]
  cl(0:1, 164) = [75.0000000,0.3492486]
  cl(0:1, 165) = [78.0000000,0.2817928]
  cl(0:1, 166) = [81.0000000,0.2131393]
  cl(0:1, 167) = [84.0000000,0.1420933]
  cl(0:1, 168) = [87.0000000,0.0710466]
  cl(0:1, 169) = [90.0000000,-0.0000003]
  cl(0:1, 170) = [93.0000000,-0.0497202]
  cl(0:1, 171) = [96.0000000,-0.0994402]
  cl(0:1, 172) = [99.0000000,-0.1491605]
  cl(0:1, 173) = [102.0000000,-0.1972242]
  cl(0:1, 174) = [105.0000000,-0.2444587]
  cl(0:1, 175) = [108.0000000,-0.2916930]
  cl(0:1, 176) = [111.0000000,-0.3365126]
  cl(0:1, 177) = [114.0000000,-0.3765018]
  cl(0:1, 178) = [117.0000000,-0.4164896]
  cl(0:1, 179) = [120.0000000,-0.4564763]
  cl(0:1, 180) = [123.0000000,-0.4862701]
  cl(0:1, 181) = [126.0000000,-0.5160599]
  cl(0:1, 182) = [129.0000000,-0.5458465]
  cl(0:1, 183) = [132.0000000,-0.5687692]
  cl(0:1, 184) = [135.0000000,-0.5882529]
  cl(0:1, 185) = [138.0000000,-0.6077281]
  cl(0:1, 186) = [141.0000000,-0.6253130]
  cl(0:1, 187) = [144.0000000,-0.6391160]
  cl(0:1, 188) = [147.0000000,-0.6529009]
  cl(0:1, 189) = [150.0000000,-0.6666701]
  cl(0:1, 190) = [153.0000000,-0.6914239]
  cl(0:1, 191) = [156.0000000,-0.7161397]
  cl(0:1, 192) = [159.0000000,-0.7408245]
  cl(0:1, 193) = [162.0000000,-0.6925233]
  cl(0:1, 194) = [165.0000000,-0.6040902]
  cl(0:1, 195) = [168.0000000,-0.5257035]
  cl(0:1, 196) = [171.0000000,-0.4240606]
  cl(0:1, 197) = [174.0000000,-0.2827066]
  cl(0:1, 198) = [177.0000000,-0.1403747]
  cl(0:1, 199) = [180.0000000,0.0000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl8_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl8 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl8_ge28.pro'
  ENDIF

!print,y,cl8

!_________________________________________________________________________
  
END FUNCTION calc_cl8_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl9_ge28( y ) RESULT( cl9 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl9
  INTEGER :: i

  cl9 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.1354399]
  cl(0:1, 2  ) = [-174.0000000,0.2736818]
  cl(0:1, 3  ) = [-171.0000000,0.4105232]
  cl(0:1, 4  ) = [-168.0000000,0.5205402]
  cl(0:1, 5  ) = [-165.0000000,0.6174166]
  cl(0:1, 6  ) = [-162.0000000,0.7203667]
  cl(0:1, 7  ) = [-159.0000000,0.7799030]
  cl(0:1, 8  ) = [-156.0000000,0.7540950]
  cl(0:1, 9  ) = [-153.0000000,0.7282730]
  cl(0:1, 10 ) = [-150.0000000,0.7024339]
  cl(0:1, 11 ) = [-147.0000000,0.6880570]
  cl(0:1, 12 ) = [-144.0000000,0.6736728]
  cl(0:1, 13 ) = [-141.0000000,0.6592802]
  cl(0:1, 14 ) = [-138.0000000,0.6408702]
  cl(0:1, 15 ) = [-135.0000000,0.6204519]
  cl(0:1, 16 ) = [-132.0000000,0.6000292]
  cl(0:1, 17 ) = [-129.0000000,0.5759567]
  cl(0:1, 18 ) = [-126.0000000,0.5445909]
  cl(0:1, 19 ) = [-123.0000000,0.5132236]
  cl(0:1, 20 ) = [-120.0000000,0.4818544]
  cl(0:1, 21 ) = [-117.0000000,0.4396815]
  cl(0:1, 22 ) = [-114.0000000,0.3975081]
  cl(0:1, 23 ) = [-111.0000000,0.3553343]
  cl(0:1, 24 ) = [-108.0000000,0.3080379]
  cl(0:1, 25 ) = [-105.0000000,0.2581804]
  cl(0:1, 26 ) = [-102.0000000,0.2083231]
  cl(0:1, 27 ) = [-99.0000000,0.1575762]
  cl(0:1, 28 ) = [-96.0000000,0.1050508]
  cl(0:1, 29 ) = [-93.0000000,0.0525256]
  cl(0:1, 30 ) = [-90.0000000,0.0000003]
  cl(0:1, 31 ) = [-87.0000000,-0.0525255]
  cl(0:1, 32 ) = [-84.0000000,-0.1050510]
  cl(0:1, 33 ) = [-81.0000000,-0.1575757]
  cl(0:1, 34 ) = [-78.0000000,-0.2083229]
  cl(0:1, 35 ) = [-75.0000000,-0.2581807]
  cl(0:1, 36 ) = [-72.0000000,-0.3080377]
  cl(0:1, 37 ) = [-69.0000000,-0.3553339]
  cl(0:1, 38 ) = [-66.0000000,-0.3975082]
  cl(0:1, 39 ) = [-63.0000000,-0.4396818]
  cl(0:1, 40 ) = [-60.0000000,-0.4818546]
  cl(0:1, 41 ) = [-57.0000000,-0.5132238]
  cl(0:1, 42 ) = [-54.0000000,-0.5445911]
  cl(0:1, 43 ) = [-51.0000000,-0.5759569]
  cl(0:1, 44 ) = [-48.0000000,-0.6013263]
  cl(0:1, 45 ) = [-45.0000000,-0.6236960]
  cl(0:1, 46 ) = [-42.0000000,-0.6421687]
  cl(0:1, 47 ) = [-39.0000000,-0.6595370]
  cl(0:1, 48 ) = [-36.0000000,-0.6741670]
  cl(0:1, 49 ) = [-33.0000000,-0.6880951]
  cl(0:1, 50 ) = [-30.0000000,-0.7024341]
  cl(0:1, 51 ) = [-29.3939394,-0.7061305]
  cl(0:1, 52 ) = [-28.7878788,-0.7098245]
  cl(0:1, 53 ) = [-28.1818182,-0.7135160]
  cl(0:1, 54 ) = [-27.5757576,-0.7175304]
  cl(0:1, 55 ) = [-26.9696970,-0.7216822]
  cl(0:1, 56 ) = [-26.3636364,-0.7258323]
  cl(0:1, 57 ) = [-25.7575758,-0.7302678]
  cl(0:1, 58 ) = [-25.1515152,-0.7351330]
  cl(0:1, 59 ) = [-24.5454545,-0.7399973]
  cl(0:1, 60 ) = [-23.9393939,-0.7449667]
  cl(0:1, 61 ) = [-23.3333333,-0.7508892]
  cl(0:1, 62 ) = [-22.7272727,-0.7568116]
  cl(0:1, 63 ) = [-22.1212121,-0.7627339]
  cl(0:1, 64 ) = [-21.5151515,-0.7655432]
  cl(0:1, 65 ) = [-20.9090909,-0.7671594]
  cl(0:1, 66 ) = [-20.3030303,-0.7709896]
  cl(0:1, 67 ) = [-19.6969697,-0.7664089]
  cl(0:1, 68 ) = [-19.0909091,-0.7532243]
  cl(0:1, 69 ) = [-18.4848485,-0.7405152]
  cl(0:1, 70 ) = [-17.8787879,-0.7280734]
  cl(0:1, 71 ) = [-17.2727273,-0.7157964]
  cl(0:1, 72 ) = [-16.6666667,-0.7036284]
  cl(0:1, 73 ) = [-16.0606061,-0.6915361]
  cl(0:1, 74 ) = [-15.4545455,-0.6798414]
  cl(0:1, 75 ) = [-14.8484848,-0.6683720]
  cl(0:1, 76 ) = [-14.2424242,-0.6575253]
  cl(0:1, 77 ) = [-13.6363636,-0.6469193]
  cl(0:1, 78 ) = [-13.0303030,-0.6365902]
  cl(0:1, 79 ) = [-12.4242424,-0.6266165]
  cl(0:1, 80 ) = [-11.8181818,-0.6169976]
  cl(0:1, 81 ) = [-11.2121212,-0.6078064]
  cl(0:1, 82 ) = [-10.6060606,-0.5992886]
  cl(0:1, 83 ) = [-10.0000000,-0.5916579]
  cl(0:1, 84 ) = [-9.3939394,-0.5790582]
  cl(0:1, 85 ) = [-8.7878788,-0.5667005]
  cl(0:1, 86 ) = [-8.1818182,-0.5527671]
  cl(0:1, 87 ) = [-7.5757576,-0.5161525]
  cl(0:1, 88 ) = [-6.9696970,-0.4697571]
  cl(0:1, 89 ) = [-6.3636364,-0.4231579]
  cl(0:1, 90 ) = [-5.7575758,-0.3747658]
  cl(0:1, 91 ) = [-5.1515152,-0.3234137]
  cl(0:1, 92 ) = [-4.5454545,-0.2685476]
  cl(0:1, 93 ) = [-3.9393939,-0.2119943]
  cl(0:1, 94 ) = [-3.3333333,-0.1484159]
  cl(0:1, 95 ) = [-2.7272727,-0.0789769]
  cl(0:1, 96 ) = [-2.1212121,-0.0034648]
  cl(0:1, 97 ) = [-1.5151515,0.0738291]
  cl(0:1, 98 ) = [-0.9090909,0.1518298]
  cl(0:1, 99 ) = [-0.3030303,0.2308223]
  cl(0:1, 100) = [0.3030303,0.3104637]
  cl(0:1, 101) = [0.9090909,0.3907903]
  cl(0:1, 102) = [1.5151515,0.4712842]
  cl(0:1, 103) = [2.1212121,0.5509631]
  cl(0:1, 104) = [2.7272727,0.6301389]
  cl(0:1, 105) = [3.3333333,0.7085363]
  cl(0:1, 106) = [3.9393939,0.7863079]
  cl(0:1, 107) = [4.5454545,0.8623883]
  cl(0:1, 108) = [5.1515152,0.9377009]
  cl(0:1, 109) = [5.7575758,1.0112507]
  cl(0:1, 110) = [6.3636364,1.0822762]
  cl(0:1, 111) = [6.9696970,1.1516126]
  cl(0:1, 112) = [7.5757576,1.2159641]
  cl(0:1, 113) = [8.1818182,1.2767330]
  cl(0:1, 114) = [8.7878788,1.3311912]
  cl(0:1, 115) = [9.3939394,1.3795643]
  cl(0:1, 116) = [10.0000000,1.4246560]
  cl(0:1, 117) = [10.6060606,1.4485668]
  cl(0:1, 118) = [11.2121212,1.4564349]
  cl(0:1, 119) = [11.8181818,1.4414551]
  cl(0:1, 120) = [12.4242424,1.2581161]
  cl(0:1, 121) = [13.0303030,1.2178355]
  cl(0:1, 122) = [13.6363636,1.2016046]
  cl(0:1, 123) = [14.2424242,1.1881319]
  cl(0:1, 124) = [14.8484848,1.1802742]
  cl(0:1, 125) = [15.4545455,1.1787778]
  cl(0:1, 126) = [16.0606061,1.1768932]
  cl(0:1, 127) = [16.6666667,1.1658104]
  cl(0:1, 128) = [17.2727273,1.1566566]
  cl(0:1, 129) = [17.8787879,1.1498473]
  cl(0:1, 130) = [18.4848485,1.1498536]
  cl(0:1, 131) = [19.0909091,1.1489729]
  cl(0:1, 132) = [19.6969697,1.1339629]
  cl(0:1, 133) = [20.3030303,1.1211345]
  cl(0:1, 134) = [20.9090909,1.1104929]
  cl(0:1, 135) = [21.5151515,1.0998509]
  cl(0:1, 136) = [22.1212121,1.0896449]
  cl(0:1, 137) = [22.7272727,1.0811841]
  cl(0:1, 138) = [23.3333333,1.0727231]
  cl(0:1, 139) = [23.9393939,1.0642620]
  cl(0:1, 140) = [24.5454545,1.0571625]
  cl(0:1, 141) = [25.1515152,1.0502131]
  cl(0:1, 142) = [25.7575758,1.0432625]
  cl(0:1, 143) = [26.3636364,1.0369255]
  cl(0:1, 144) = [26.9696970,1.0309962]
  cl(0:1, 145) = [27.5757576,1.0250643]
  cl(0:1, 146) = [28.1818182,1.0193289]
  cl(0:1, 147) = [28.7878788,1.0140553]
  cl(0:1, 148) = [29.3939394,1.0087781]
  cl(0:1, 149) = [30.0000000,1.0034974]
  cl(0:1, 150) = [33.0000000,0.9829987]
  cl(0:1, 151) = [36.0000000,0.9630897]
  cl(0:1, 152) = [39.0000000,0.9421760]
  cl(0:1, 153) = [42.0000000,0.9173694]
  cl(0:1, 154) = [45.0000000,0.8909948]
  cl(0:1, 155) = [48.0000000,0.8590526]
  cl(0:1, 156) = [51.0000000,0.8228200]
  cl(0:1, 157) = [54.0000000,0.7780103]
  cl(0:1, 158) = [57.0000000,0.7331990]
  cl(0:1, 159) = [60.0000000,0.6883853]
  cl(0:1, 160) = [63.0000000,0.6281302]
  cl(0:1, 161) = [66.0000000,0.5678742]
  cl(0:1, 162) = [69.0000000,0.5076169]
  cl(0:1, 163) = [72.0000000,0.4400530]
  cl(0:1, 164) = [75.0000000,0.3688364]
  cl(0:1, 165) = [78.0000000,0.2976186]
  cl(0:1, 166) = [81.0000000,0.2251258]
  cl(0:1, 167) = [84.0000000,0.1500844]
  cl(0:1, 168) = [87.0000000,0.0750422]
  cl(0:1, 169) = [90.0000000,-0.0000003]
  cl(0:1, 170) = [93.0000000,-0.0525256]
  cl(0:1, 171) = [96.0000000,-0.1050508]
  cl(0:1, 172) = [99.0000000,-0.1575762]
  cl(0:1, 173) = [102.0000000,-0.2083231]
  cl(0:1, 174) = [105.0000000,-0.2581804]
  cl(0:1, 175) = [108.0000000,-0.3080379]
  cl(0:1, 176) = [111.0000000,-0.3553343]
  cl(0:1, 177) = [114.0000000,-0.3975081]
  cl(0:1, 178) = [117.0000000,-0.4396815]
  cl(0:1, 179) = [120.0000000,-0.4818544]
  cl(0:1, 180) = [123.0000000,-0.5132236]
  cl(0:1, 181) = [126.0000000,-0.5445909]
  cl(0:1, 182) = [129.0000000,-0.5759567]
  cl(0:1, 183) = [132.0000000,-0.6000292]
  cl(0:1, 184) = [135.0000000,-0.6204519]
  cl(0:1, 185) = [138.0000000,-0.6408702]
  cl(0:1, 186) = [141.0000000,-0.6592802]
  cl(0:1, 187) = [144.0000000,-0.6736728]
  cl(0:1, 188) = [147.0000000,-0.6880570]
  cl(0:1, 189) = [150.0000000,-0.7024339]
  cl(0:1, 190) = [153.0000000,-0.7282730]
  cl(0:1, 191) = [156.0000000,-0.7540950]
  cl(0:1, 192) = [159.0000000,-0.7799030]
  cl(0:1, 193) = [162.0000000,-0.7156237]
  cl(0:1, 194) = [165.0000000,-0.6046682]
  cl(0:1, 195) = [168.0000000,-0.5046331]
  cl(0:1, 196) = [171.0000000,-0.3902999]
  cl(0:1, 197) = [174.0000000,-0.2601996]
  cl(0:1, 198) = [177.0000000,-0.1287009]
  cl(0:1, 199) = [180.0000000,0.0000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl9_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl9 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl9_ge28.pro'
  ENDIF

!print,y,cl9

!_________________________________________________________________________
  
END FUNCTION calc_cl9_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl10_ge28( y ) RESULT( cl10 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl10
  INTEGER :: i

  cl10 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.1294950]
  cl(0:1, 2  ) = [-174.0000000,0.2621965]
  cl(0:1, 3  ) = [-171.0000000,0.3932951]
  cl(0:1, 4  ) = [-168.0000000,0.5102333]
  cl(0:1, 5  ) = [-165.0000000,0.6197854]
  cl(0:1, 6  ) = [-162.0000000,0.7313958]
  cl(0:1, 7  ) = [-159.0000000,0.7970428]
  cl(0:1, 8  ) = [-156.0000000,0.7708323]
  cl(0:1, 9  ) = [-153.0000000,0.7446209]
  cl(0:1, 10 ) = [-150.0000000,0.7184087]
  cl(0:1, 11 ) = [-147.0000000,0.7038239]
  cl(0:1, 12 ) = [-144.0000000,0.6892385]
  cl(0:1, 13 ) = [-141.0000000,0.6746523]
  cl(0:1, 14 ) = [-138.0000000,0.6559218]
  cl(0:1, 15 ) = [-135.0000000,0.6351191]
  cl(0:1, 16 ) = [-132.0000000,0.6143155]
  cl(0:1, 17 ) = [-129.0000000,0.5897584]
  cl(0:1, 18 ) = [-126.0000000,0.5576944]
  cl(0:1, 19 ) = [-123.0000000,0.5256301]
  cl(0:1, 20 ) = [-120.0000000,0.4935658]
  cl(0:1, 21 ) = [-117.0000000,0.4503977]
  cl(0:1, 22 ) = [-114.0000000,0.4072297]
  cl(0:1, 23 ) = [-111.0000000,0.3640620]
  cl(0:1, 24 ) = [-108.0000000,0.3156271]
  cl(0:1, 25 ) = [-105.0000000,0.2645584]
  cl(0:1, 26 ) = [-102.0000000,0.2134903]
  cl(0:1, 27 ) = [-99.0000000,0.1615005]
  cl(0:1, 28 ) = [-96.0000000,0.1076671]
  cl(0:1, 29 ) = [-93.0000000,0.0538337]
  cl(0:1, 30 ) = [-90.0000000,0.0000003]
  cl(0:1, 31 ) = [-87.0000000,-0.0538337]
  cl(0:1, 32 ) = [-84.0000000,-0.1076672]
  cl(0:1, 33 ) = [-81.0000000,-0.1615000]
  cl(0:1, 34 ) = [-78.0000000,-0.2134901]
  cl(0:1, 35 ) = [-75.0000000,-0.2645588]
  cl(0:1, 36 ) = [-72.0000000,-0.3156268]
  cl(0:1, 37 ) = [-69.0000000,-0.3640617]
  cl(0:1, 38 ) = [-66.0000000,-0.4072298]
  cl(0:1, 39 ) = [-63.0000000,-0.4503980]
  cl(0:1, 40 ) = [-60.0000000,-0.4935660]
  cl(0:1, 41 ) = [-57.0000000,-0.5256304]
  cl(0:1, 42 ) = [-54.0000000,-0.5576946]
  cl(0:1, 43 ) = [-51.0000000,-0.5897586]
  cl(0:1, 44 ) = [-48.0000000,-0.6159754]
  cl(0:1, 45 ) = [-45.0000000,-0.6392686]
  cl(0:1, 46 ) = [-42.0000000,-0.6575817]
  cl(0:1, 47 ) = [-39.0000000,-0.6749804]
  cl(0:1, 48 ) = [-36.0000000,-0.6898692]
  cl(0:1, 49 ) = [-33.0000000,-0.7038725]
  cl(0:1, 50 ) = [-30.0000000,-0.7184088]
  cl(0:1, 51 ) = [-29.3939394,-0.7217627]
  cl(0:1, 52 ) = [-28.7878788,-0.7251165]
  cl(0:1, 53 ) = [-28.1818182,-0.7284701]
  cl(0:1, 54 ) = [-27.5757576,-0.7322369]
  cl(0:1, 55 ) = [-26.9696970,-0.7361806]
  cl(0:1, 56 ) = [-26.3636364,-0.7401242]
  cl(0:1, 57 ) = [-25.7575758,-0.7444321]
  cl(0:1, 58 ) = [-25.1515152,-0.7492861]
  cl(0:1, 59 ) = [-24.5454545,-0.7541402]
  cl(0:1, 60 ) = [-23.9393939,-0.7591283]
  cl(0:1, 61 ) = [-23.3333333,-0.7653237]
  cl(0:1, 62 ) = [-22.7272727,-0.7715192]
  cl(0:1, 63 ) = [-22.1212121,-0.7777146]
  cl(0:1, 64 ) = [-21.5151515,-0.7839782]
  cl(0:1, 65 ) = [-20.9090909,-0.7901175]
  cl(0:1, 66 ) = [-20.3030303,-0.7970121]
  cl(0:1, 67 ) = [-19.6969697,-0.7954157]
  cl(0:1, 68 ) = [-19.0909091,-0.7852633]
  cl(0:1, 69 ) = [-18.4848485,-0.7752733]
  cl(0:1, 70 ) = [-17.8787879,-0.7653749]
  cl(0:1, 71 ) = [-17.2727273,-0.7555331]
  cl(0:1, 72 ) = [-16.6666667,-0.7457288]
  cl(0:1, 73 ) = [-16.0606061,-0.7359506]
  cl(0:1, 74 ) = [-15.4545455,-0.7263085]
  cl(0:1, 75 ) = [-14.8484848,-0.7167434]
  cl(0:1, 76 ) = [-14.2424242,-0.7073906]
  cl(0:1, 77 ) = [-13.6363636,-0.6981207]
  cl(0:1, 78 ) = [-13.0303030,-0.6889443]
  cl(0:1, 79 ) = [-12.4242424,-0.6798903]
  cl(0:1, 80 ) = [-11.8181818,-0.6709563]
  cl(0:1, 81 ) = [-11.2121212,-0.6621688]
  cl(0:1, 82 ) = [-10.6060606,-0.6536113]
  cl(0:1, 83 ) = [-10.0000000,-0.6453562]
  cl(0:1, 84 ) = [-9.3939394,-0.6365933]
  cl(0:1, 85 ) = [-8.7878788,-0.6279183]
  cl(0:1, 86 ) = [-8.1818182,-0.6187114]
  cl(0:1, 87 ) = [-7.5757576,-0.5802162]
  cl(0:1, 88 ) = [-6.9696970,-0.5289534]
  cl(0:1, 89 ) = [-6.3636364,-0.4738625]
  cl(0:1, 90 ) = [-5.7575758,-0.4165822]
  cl(0:1, 91 ) = [-5.1515152,-0.3559335]
  cl(0:1, 92 ) = [-4.5454545,-0.2914721]
  cl(0:1, 93 ) = [-3.9393939,-0.2253049]
  cl(0:1, 94 ) = [-3.3333333,-0.1541040]
  cl(0:1, 95 ) = [-2.7272727,-0.0816248]
  cl(0:1, 96 ) = [-2.1212121,-0.0076212]
  cl(0:1, 97 ) = [-1.5151515,0.0683135]
  cl(0:1, 98 ) = [-0.9090909,0.1449169]
  cl(0:1, 99 ) = [-0.3030303,0.2225546]
  cl(0:1, 100) = [0.3030303,0.3007952]
  cl(0:1, 101) = [0.9090909,0.3796520]
  cl(0:1, 102) = [1.5151515,0.4590041]
  cl(0:1, 103) = [2.1212121,0.5382920]
  cl(0:1, 104) = [2.7272727,0.6170980]
  cl(0:1, 105) = [3.3333333,0.6949077]
  cl(0:1, 106) = [3.9393939,0.7719131]
  cl(0:1, 107) = [4.5454545,0.8467519]
  cl(0:1, 108) = [5.1515152,0.9206049]
  cl(0:1, 109) = [5.7575758,0.9921999]
  cl(0:1, 110) = [6.3636364,1.0605572]
  cl(0:1, 111) = [6.9696970,1.1267499]
  cl(0:1, 112) = [7.5757576,1.1866634]
  cl(0:1, 113) = [8.1818182,1.2431499]
  cl(0:1, 114) = [8.7878788,1.2924569]
  cl(0:1, 115) = [9.3939394,1.3339613]
  cl(0:1, 116) = [10.0000000,1.3712601]
  cl(0:1, 117) = [10.6060606,1.3861858]
  cl(0:1, 118) = [11.2121212,1.3851973]
  cl(0:1, 119) = [11.8181818,1.3549363]
  cl(0:1, 120) = [12.4242424,1.3124619]
  cl(0:1, 121) = [13.0303030,1.2868079]
  cl(0:1, 122) = [13.6363636,1.2617268]
  cl(0:1, 123) = [14.2424242,1.2376910]
  cl(0:1, 124) = [14.8484848,1.2152877]
  cl(0:1, 125) = [15.4545455,1.1996819]
  cl(0:1, 126) = [16.0606061,1.1866391]
  cl(0:1, 127) = [16.6666667,1.1768863]
  cl(0:1, 128) = [17.2727273,1.1695668]
  cl(0:1, 129) = [17.8787879,1.1652210]
  cl(0:1, 130) = [18.4848485,1.1695253]
  cl(0:1, 131) = [19.0909091,1.1728454]
  cl(0:1, 132) = [19.6969697,1.1583557]
  cl(0:1, 133) = [20.3030303,1.1453051]
  cl(0:1, 134) = [20.9090909,1.1336937]
  cl(0:1, 135) = [21.5151515,1.1220823]
  cl(0:1, 136) = [22.1212121,1.1110232]
  cl(0:1, 137) = [22.7272727,1.1021726]
  cl(0:1, 138) = [23.3333333,1.0933220]
  cl(0:1, 139) = [23.9393939,1.0844713]
  cl(0:1, 140) = [24.5454545,1.0773454]
  cl(0:1, 141) = [25.1515152,1.0704111]
  cl(0:1, 142) = [25.7575758,1.0634767]
  cl(0:1, 143) = [26.3636364,1.0573224]
  cl(0:1, 144) = [26.9696970,1.0516881]
  cl(0:1, 145) = [27.5757576,1.0460538]
  cl(0:1, 146) = [28.1818182,1.0406726]
  cl(0:1, 147) = [28.7878788,1.0358820]
  cl(0:1, 148) = [29.3939394,1.0310913]
  cl(0:1, 149) = [30.0000000,1.0263005]
  cl(0:1, 150) = [33.0000000,1.0055317]
  cl(0:1, 151) = [36.0000000,0.9855279]
  cl(0:1, 152) = [39.0000000,0.9642567]
  cl(0:1, 153) = [42.0000000,0.9394017]
  cl(0:1, 154) = [45.0000000,0.9132416]
  cl(0:1, 155) = [48.0000000,0.8799658]
  cl(0:1, 156) = [51.0000000,0.8425133]
  cl(0:1, 157) = [54.0000000,0.7967072]
  cl(0:1, 158) = [57.0000000,0.7509014]
  cl(0:1, 159) = [60.0000000,0.7050956]
  cl(0:1, 160) = [63.0000000,0.6434267]
  cl(0:1, 161) = [66.0000000,0.5817575]
  cl(0:1, 162) = [69.0000000,0.5200884]
  cl(0:1, 163) = [72.0000000,0.4508958]
  cl(0:1, 164) = [75.0000000,0.3779416]
  cl(0:1, 165) = [78.0000000,0.3049866]
  cl(0:1, 166) = [81.0000000,0.2307151]
  cl(0:1, 167) = [84.0000000,0.1538107]
  cl(0:1, 168) = [87.0000000,0.0769054]
  cl(0:1, 169) = [90.0000000,-0.0000003]
  cl(0:1, 170) = [93.0000000,-0.0538337]
  cl(0:1, 171) = [96.0000000,-0.1076671]
  cl(0:1, 172) = [99.0000000,-0.1615005]
  cl(0:1, 173) = [102.0000000,-0.2134903]
  cl(0:1, 174) = [105.0000000,-0.2645584]
  cl(0:1, 175) = [108.0000000,-0.3156271]
  cl(0:1, 176) = [111.0000000,-0.3640620]
  cl(0:1, 177) = [114.0000000,-0.4072297]
  cl(0:1, 178) = [117.0000000,-0.4503977]
  cl(0:1, 179) = [120.0000000,-0.4935658]
  cl(0:1, 180) = [123.0000000,-0.5256301]
  cl(0:1, 181) = [126.0000000,-0.5576944]
  cl(0:1, 182) = [129.0000000,-0.5897584]
  cl(0:1, 183) = [132.0000000,-0.6143155]
  cl(0:1, 184) = [135.0000000,-0.6351191]
  cl(0:1, 185) = [138.0000000,-0.6559218]
  cl(0:1, 186) = [141.0000000,-0.6746523]
  cl(0:1, 187) = [144.0000000,-0.6892385]
  cl(0:1, 188) = [147.0000000,-0.7038239]
  cl(0:1, 189) = [150.0000000,-0.7184087]
  cl(0:1, 190) = [153.0000000,-0.7446209]
  cl(0:1, 191) = [156.0000000,-0.7708323]
  cl(0:1, 192) = [159.0000000,-0.7970428]
  cl(0:1, 193) = [162.0000000,-0.7254433]
  cl(0:1, 194) = [165.0000000,-0.6048632]
  cl(0:1, 195) = [168.0000000,-0.4873462]
  cl(0:1, 196) = [171.0000000,-0.3667531]
  cl(0:1, 197) = [174.0000000,-0.2445019]
  cl(0:1, 198) = [177.0000000,-0.1207296]
  cl(0:1, 199) = [180.0000000,0.0000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl10_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl10 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl10_ge28.pro'
  ENDIF

!print,y,cl10

!_________________________________________________________________________
  
END FUNCTION calc_cl10_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl11_ge28( y ) RESULT( cl11 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl11
  INTEGER :: i

  cl11 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.1249662]
  cl(0:1, 2  ) = [-174.0000000,0.2529986]
  cl(0:1, 3  ) = [-171.0000000,0.3794983]
  cl(0:1, 4  ) = [-168.0000000,0.4987576]
  cl(0:1, 5  ) = [-165.0000000,0.6152205]
  cl(0:1, 6  ) = [-162.0000000,0.7270771]
  cl(0:1, 7  ) = [-159.0000000,0.7932310]
  cl(0:1, 8  ) = [-156.0000000,0.7682341]
  cl(0:1, 9  ) = [-153.0000000,0.7433564]
  cl(0:1, 10 ) = [-150.0000000,0.7184785]
  cl(0:1, 11 ) = [-147.0000000,0.7039559]
  cl(0:1, 12 ) = [-144.0000000,0.6894330]
  cl(0:1, 13 ) = [-141.0000000,0.6749097]
  cl(0:1, 14 ) = [-138.0000000,0.6562195]
  cl(0:1, 15 ) = [-135.0000000,0.6354459]
  cl(0:1, 16 ) = [-132.0000000,0.6146715]
  cl(0:1, 17 ) = [-129.0000000,0.5901342]
  cl(0:1, 18 ) = [-126.0000000,0.5580711]
  cl(0:1, 19 ) = [-123.0000000,0.5260078]
  cl(0:1, 20 ) = [-120.0000000,0.4939445]
  cl(0:1, 21 ) = [-117.0000000,0.4507542]
  cl(0:1, 22 ) = [-114.0000000,0.4075641]
  cl(0:1, 23 ) = [-111.0000000,0.3643743]
  cl(0:1, 24 ) = [-108.0000000,0.3159054]
  cl(0:1, 25 ) = [-105.0000000,0.2647970]
  cl(0:1, 26 ) = [-102.0000000,0.2136890]
  cl(0:1, 27 ) = [-99.0000000,0.1616554]
  cl(0:1, 28 ) = [-96.0000000,0.1077704]
  cl(0:1, 29 ) = [-93.0000000,0.0538854]
  cl(0:1, 30 ) = [-90.0000000,0.0000003]
  cl(0:1, 31 ) = [-87.0000000,-0.0538853]
  cl(0:1, 32 ) = [-84.0000000,-0.1077705]
  cl(0:1, 33 ) = [-81.0000000,-0.1616549]
  cl(0:1, 34 ) = [-78.0000000,-0.2136888]
  cl(0:1, 35 ) = [-75.0000000,-0.2647973]
  cl(0:1, 36 ) = [-72.0000000,-0.3159052]
  cl(0:1, 37 ) = [-69.0000000,-0.3643739]
  cl(0:1, 38 ) = [-66.0000000,-0.4075643]
  cl(0:1, 39 ) = [-63.0000000,-0.4507545]
  cl(0:1, 40 ) = [-60.0000000,-0.4939447]
  cl(0:1, 41 ) = [-57.0000000,-0.5260080]
  cl(0:1, 42 ) = [-54.0000000,-0.5580713]
  cl(0:1, 43 ) = [-51.0000000,-0.5901344]
  cl(0:1, 44 ) = [-48.0000000,-0.6163497]
  cl(0:1, 45 ) = [-45.0000000,-0.6396412]
  cl(0:1, 46 ) = [-42.0000000,-0.6578977]
  cl(0:1, 47 ) = [-39.0000000,-0.6752451]
  cl(0:1, 48 ) = [-36.0000000,-0.6900831]
  cl(0:1, 49 ) = [-33.0000000,-0.7040171]
  cl(0:1, 50 ) = [-30.0000000,-0.7184786]
  cl(0:1, 51 ) = [-29.3939394,-0.7217984]
  cl(0:1, 52 ) = [-28.7878788,-0.7251181]
  cl(0:1, 53 ) = [-28.1818182,-0.7284378]
  cl(0:1, 54 ) = [-27.5757576,-0.7321726]
  cl(0:1, 55 ) = [-26.9696970,-0.7360853]
  cl(0:1, 56 ) = [-26.3636364,-0.7399980]
  cl(0:1, 57 ) = [-25.7575758,-0.7442769]
  cl(0:1, 58 ) = [-25.1515152,-0.7491050]
  cl(0:1, 59 ) = [-24.5454545,-0.7539332]
  cl(0:1, 60 ) = [-23.9393939,-0.7588962]
  cl(0:1, 61 ) = [-23.3333333,-0.7650736]
  cl(0:1, 62 ) = [-22.7272727,-0.7712509]
  cl(0:1, 63 ) = [-22.1212121,-0.7774283]
  cl(0:1, 64 ) = [-21.5151515,-0.7902629]
  cl(0:1, 65 ) = [-20.9090909,-0.8059060]
  cl(0:1, 66 ) = [-20.3030303,-0.8194892]
  cl(0:1, 67 ) = [-19.6969697,-0.8246780]
  cl(0:1, 68 ) = [-19.0909091,-0.8216200]
  cl(0:1, 69 ) = [-18.4848485,-0.8181974]
  cl(0:1, 70 ) = [-17.8787879,-0.8145695]
  cl(0:1, 71 ) = [-17.2727273,-0.8108148]
  cl(0:1, 72 ) = [-16.6666667,-0.8069761]
  cl(0:1, 73 ) = [-16.0606061,-0.8030789]
  cl(0:1, 74 ) = [-15.4545455,-0.7970007]
  cl(0:1, 75 ) = [-14.8484848,-0.7895766]
  cl(0:1, 76 ) = [-14.2424242,-0.7787813]
  cl(0:1, 77 ) = [-13.6363636,-0.7673574]
  cl(0:1, 78 ) = [-13.0303030,-0.7554226]
  cl(0:1, 79 ) = [-12.4242424,-0.7429692]
  cl(0:1, 80 ) = [-11.8181818,-0.7302446]
  cl(0:1, 81 ) = [-11.2121212,-0.7172315]
  cl(0:1, 82 ) = [-10.6060606,-0.7035774]
  cl(0:1, 83 ) = [-10.0000000,-0.6891051]
  cl(0:1, 84 ) = [-9.3939394,-0.6762278]
  cl(0:1, 85 ) = [-8.7878788,-0.6641416]
  cl(0:1, 86 ) = [-8.1818182,-0.6542169]
  cl(0:1, 87 ) = [-7.5757576,-0.6146250]
  cl(0:1, 88 ) = [-6.9696970,-0.5618419]
  cl(0:1, 89 ) = [-6.3636364,-0.4991896]
  cl(0:1, 90 ) = [-5.7575758,-0.4346088]
  cl(0:1, 91 ) = [-5.1515152,-0.3673484]
  cl(0:1, 92 ) = [-4.5454545,-0.2972208]
  cl(0:1, 93 ) = [-3.9393939,-0.2258271]
  cl(0:1, 94 ) = [-3.3333333,-0.1540505]
  cl(0:1, 95 ) = [-2.7272727,-0.0812319]
  cl(0:1, 96 ) = [-2.1212121,-0.0071498]
  cl(0:1, 97 ) = [-1.5151515,0.0687827]
  cl(0:1, 98 ) = [-0.9090909,0.1453699]
  cl(0:1, 99 ) = [-0.3030303,0.2230288]
  cl(0:1, 100) = [0.3030303,0.3012373]
  cl(0:1, 101) = [0.9090909,0.3800064]
  cl(0:1, 102) = [1.5151515,0.4592466]
  cl(0:1, 103) = [2.1212121,0.5384362]
  cl(0:1, 104) = [2.7272727,0.6170864]
  cl(0:1, 105) = [3.3333333,0.6947666]
  cl(0:1, 106) = [3.9393939,0.7716653]
  cl(0:1, 107) = [4.5454545,0.8464241]
  cl(0:1, 108) = [5.1515152,0.9201987]
  cl(0:1, 109) = [5.7575758,0.9917111]
  cl(0:1, 110) = [6.3636364,1.0600225]
  cl(0:1, 111) = [6.9696970,1.1262005]
  cl(0:1, 112) = [7.5757576,1.1862308]
  cl(0:1, 113) = [8.1818182,1.2430486]
  cl(0:1, 114) = [8.7878788,1.2933945]
  cl(0:1, 115) = [9.3939394,1.3364061]
  cl(0:1, 116) = [10.0000000,1.3746951]
  cl(0:1, 117) = [10.6060606,1.3912854]
  cl(0:1, 118) = [11.2121212,1.3924765]
  cl(0:1, 119) = [11.8181818,1.3650687]
  cl(0:1, 120) = [12.4242424,1.3398855]
  cl(0:1, 121) = [13.0303030,1.2851739]
  cl(0:1, 122) = [13.6363636,1.2564302]
  cl(0:1, 123) = [14.2424242,1.2294110]
  cl(0:1, 124) = [14.8484848,1.2058736]
  cl(0:1, 125) = [15.4545455,1.1893677]
  cl(0:1, 126) = [16.0606061,1.1756590]
  cl(0:1, 127) = [16.6666667,1.1660364]
  cl(0:1, 128) = [17.2727273,1.1590129]
  cl(0:1, 129) = [17.8787879,1.1551661]
  cl(0:1, 130) = [18.4848485,1.1603533]
  cl(0:1, 131) = [19.0909091,1.1648212]
  cl(0:1, 132) = [19.6969697,1.1524119]
  cl(0:1, 133) = [20.3030303,1.1410815]
  cl(0:1, 134) = [20.9090909,1.1308302]
  cl(0:1, 135) = [21.5151515,1.1205790]
  cl(0:1, 136) = [22.1212121,1.1106131]
  cl(0:1, 137) = [22.7272727,1.1017883]
  cl(0:1, 138) = [23.3333333,1.0929636]
  cl(0:1, 139) = [23.9393939,1.0841389]
  cl(0:1, 140) = [24.5454545,1.0770488]
  cl(0:1, 141) = [25.1515152,1.0701514]
  cl(0:1, 142) = [25.7575758,1.0632539]
  cl(0:1, 143) = [26.3636364,1.0571409]
  cl(0:1, 144) = [26.9696970,1.0515509]
  cl(0:1, 145) = [27.5757576,1.0459609]
  cl(0:1, 146) = [28.1818182,1.0406253]
  cl(0:1, 147) = [28.7878788,1.0358834]
  cl(0:1, 148) = [29.3939394,1.0311415]
  cl(0:1, 149) = [30.0000000,1.0263997]
  cl(0:1, 150) = [33.0000000,1.0057380]
  cl(0:1, 151) = [36.0000000,0.9858337]
  cl(0:1, 152) = [39.0000000,0.9646357]
  cl(0:1, 153) = [42.0000000,0.9398538]
  cl(0:1, 154) = [45.0000000,0.9137739]
  cl(0:1, 155) = [48.0000000,0.8804999]
  cl(0:1, 156) = [51.0000000,0.8430491]
  cl(0:1, 157) = [54.0000000,0.7972444]
  cl(0:1, 158) = [57.0000000,0.7514399]
  cl(0:1, 159) = [60.0000000,0.7056357]
  cl(0:1, 160) = [63.0000000,0.6439354]
  cl(0:1, 161) = [66.0000000,0.5822350]
  cl(0:1, 162) = [69.0000000,0.5205346]
  cl(0:1, 163) = [72.0000000,0.4512935]
  cl(0:1, 164) = [75.0000000,0.3782821]
  cl(0:1, 165) = [78.0000000,0.3052699]
  cl(0:1, 166) = [81.0000000,0.2309357]
  cl(0:1, 167) = [84.0000000,0.1539578]
  cl(0:1, 168) = [87.0000000,0.0769789]
  cl(0:1, 169) = [90.0000000,-0.0000003]
  cl(0:1, 170) = [93.0000000,-0.0538854]
  cl(0:1, 171) = [96.0000000,-0.1077704]
  cl(0:1, 172) = [99.0000000,-0.1616554]
  cl(0:1, 173) = [102.0000000,-0.2136890]
  cl(0:1, 174) = [105.0000000,-0.2647970]
  cl(0:1, 175) = [108.0000000,-0.3159054]
  cl(0:1, 176) = [111.0000000,-0.3643743]
  cl(0:1, 177) = [114.0000000,-0.4075641]
  cl(0:1, 178) = [117.0000000,-0.4507542]
  cl(0:1, 179) = [120.0000000,-0.4939445]
  cl(0:1, 180) = [123.0000000,-0.5260078]
  cl(0:1, 181) = [126.0000000,-0.5580711]
  cl(0:1, 182) = [129.0000000,-0.5901342]
  cl(0:1, 183) = [132.0000000,-0.6146715]
  cl(0:1, 184) = [135.0000000,-0.6354459]
  cl(0:1, 185) = [138.0000000,-0.6562195]
  cl(0:1, 186) = [141.0000000,-0.6749097]
  cl(0:1, 187) = [144.0000000,-0.6894330]
  cl(0:1, 188) = [147.0000000,-0.7039559]
  cl(0:1, 189) = [150.0000000,-0.7184785]
  cl(0:1, 190) = [153.0000000,-0.7426292]
  cl(0:1, 191) = [156.0000000,-0.7667799]
  cl(0:1, 192) = [159.0000000,-0.7910516]
  cl(0:1, 193) = [162.0000000,-0.7193074]
  cl(0:1, 194) = [165.0000000,-0.5994226]
  cl(0:1, 195) = [168.0000000,-0.4729059]
  cl(0:1, 196) = [171.0000000,-0.3527389]
  cl(0:1, 197) = [174.0000000,-0.2351591]
  cl(0:1, 198) = [177.0000000,-0.1162164]
  cl(0:1, 199) = [180.0000000,0.0000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl11_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl11 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl11_ge28.pro'
  ENDIF

!print,y,cl11

!_________________________________________________________________________
  
END FUNCTION calc_cl11_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl12_ge28( y ) RESULT( cl12 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl12
  INTEGER :: i

  cl12 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.1206102]
  cl(0:1, 2  ) = [-174.0000000,0.2438462]
  cl(0:1, 3  ) = [-171.0000000,0.3657697]
  cl(0:1, 4  ) = [-168.0000000,0.4840692]
  cl(0:1, 5  ) = [-165.0000000,0.6016375]
  cl(0:1, 6  ) = [-162.0000000,0.7131536]
  cl(0:1, 7  ) = [-159.0000000,0.7800690]
  cl(0:1, 8  ) = [-156.0000000,0.7586813]
  cl(0:1, 9  ) = [-153.0000000,0.7377160]
  cl(0:1, 10 ) = [-150.0000000,0.7167506]
  cl(0:1, 11 ) = [-147.0000000,0.7024308]
  cl(0:1, 12 ) = [-144.0000000,0.6881108]
  cl(0:1, 13 ) = [-141.0000000,0.6737902]
  cl(0:1, 14 ) = [-138.0000000,0.6552538]
  cl(0:1, 15 ) = [-135.0000000,0.6346093]
  cl(0:1, 16 ) = [-132.0000000,0.6139641]
  cl(0:1, 17 ) = [-129.0000000,0.5895418]
  cl(0:1, 18 ) = [-126.0000000,0.5575653]
  cl(0:1, 19 ) = [-123.0000000,0.5255887]
  cl(0:1, 20 ) = [-120.0000000,0.4936120]
  cl(0:1, 21 ) = [-117.0000000,0.4504785]
  cl(0:1, 22 ) = [-114.0000000,0.4073452]
  cl(0:1, 23 ) = [-111.0000000,0.3642120]
  cl(0:1, 24 ) = [-108.0000000,0.3157837]
  cl(0:1, 25 ) = [-105.0000000,0.2647077]
  cl(0:1, 26 ) = [-102.0000000,0.2136321]
  cl(0:1, 27 ) = [-99.0000000,0.1616236]
  cl(0:1, 28 ) = [-96.0000000,0.1077492]
  cl(0:1, 29 ) = [-93.0000000,0.0538748]
  cl(0:1, 30 ) = [-90.0000000,0.0000003]
  cl(0:1, 31 ) = [-87.0000000,-0.0538747]
  cl(0:1, 32 ) = [-84.0000000,-0.1077493]
  cl(0:1, 33 ) = [-81.0000000,-0.1616232]
  cl(0:1, 34 ) = [-78.0000000,-0.2136319]
  cl(0:1, 35 ) = [-75.0000000,-0.2647080]
  cl(0:1, 36 ) = [-72.0000000,-0.3157834]
  cl(0:1, 37 ) = [-69.0000000,-0.3642116]
  cl(0:1, 38 ) = [-66.0000000,-0.4073453]
  cl(0:1, 39 ) = [-63.0000000,-0.4504788]
  cl(0:1, 40 ) = [-60.0000000,-0.4936122]
  cl(0:1, 41 ) = [-57.0000000,-0.5255889]
  cl(0:1, 42 ) = [-54.0000000,-0.5575655]
  cl(0:1, 43 ) = [-51.0000000,-0.5895420]
  cl(0:1, 44 ) = [-48.0000000,-0.6156511]
  cl(0:1, 45 ) = [-45.0000000,-0.6388267]
  cl(0:1, 46 ) = [-42.0000000,-0.6569408]
  cl(0:1, 47 ) = [-39.0000000,-0.6741382]
  cl(0:1, 48 ) = [-36.0000000,-0.6888004]
  cl(0:1, 49 ) = [-33.0000000,-0.7025275]
  cl(0:1, 50 ) = [-30.0000000,-0.7167507]
  cl(0:1, 51 ) = [-29.3939394,-0.7200144]
  cl(0:1, 52 ) = [-28.7878788,-0.7232780]
  cl(0:1, 53 ) = [-28.1818182,-0.7265416]
  cl(0:1, 54 ) = [-27.5757576,-0.7302151]
  cl(0:1, 55 ) = [-26.9696970,-0.7340642]
  cl(0:1, 56 ) = [-26.3636364,-0.7379133]
  cl(0:1, 57 ) = [-25.7575758,-0.7421248]
  cl(0:1, 58 ) = [-25.1515152,-0.7468801]
  cl(0:1, 59 ) = [-24.5454545,-0.7516353]
  cl(0:1, 60 ) = [-23.9393939,-0.7565243]
  cl(0:1, 61 ) = [-23.3333333,-0.7626167]
  cl(0:1, 62 ) = [-22.7272727,-0.7687092]
  cl(0:1, 63 ) = [-22.1212121,-0.7748016]
  cl(0:1, 64 ) = [-21.5151515,-0.7938963]
  cl(0:1, 65 ) = [-20.9090909,-0.8195888]
  cl(0:1, 66 ) = [-20.3030303,-0.8417559]
  cl(0:1, 67 ) = [-19.6969697,-0.8571330]
  cl(0:1, 68 ) = [-19.0909091,-0.8659135]
  cl(0:1, 69 ) = [-18.4848485,-0.8742150]
  cl(0:1, 70 ) = [-17.8787879,-0.8822469]
  cl(0:1, 71 ) = [-17.2727273,-0.8901125]
  cl(0:1, 72 ) = [-16.6666667,-0.8978677]
  cl(0:1, 73 ) = [-16.0606061,-0.9055460]
  cl(0:1, 74 ) = [-15.4545455,-0.9055002]
  cl(0:1, 75 ) = [-14.8484848,-0.9006505]
  cl(0:1, 76 ) = [-14.2424242,-0.8838819]
  cl(0:1, 77 ) = [-13.6363636,-0.8651353]
  cl(0:1, 78 ) = [-13.0303030,-0.8449492]
  cl(0:1, 79 ) = [-12.4242424,-0.8234381]
  cl(0:1, 80 ) = [-11.8181818,-0.8015783]
  cl(0:1, 81 ) = [-11.2121212,-0.7794361]
  cl(0:1, 82 ) = [-10.6060606,-0.7561254]
  cl(0:1, 83 ) = [-10.0000000,-0.7314042]
  cl(0:1, 84 ) = [-9.3939394,-0.7109082]
  cl(0:1, 85 ) = [-8.7878788,-0.6938955]
  cl(0:1, 86 ) = [-8.1818182,-0.6823058]
  cl(0:1, 87 ) = [-7.5757576,-0.6418097]
  cl(0:1, 88 ) = [-6.9696970,-0.5881439]
  cl(0:1, 89 ) = [-6.3636364,-0.5185954]
  cl(0:1, 90 ) = [-5.7575758,-0.4477231]
  cl(0:1, 91 ) = [-5.1515152,-0.3751430]
  cl(0:1, 92 ) = [-4.5454545,-0.3007872]
  cl(0:1, 93 ) = [-3.9393939,-0.2257502]
  cl(0:1, 94 ) = [-3.3333333,-0.1530974]
  cl(0:1, 95 ) = [-2.7272727,-0.0795719]
  cl(0:1, 96 ) = [-2.1212121,-0.0050354]
  cl(0:1, 97 ) = [-1.5151515,0.0710741]
  cl(0:1, 98 ) = [-0.9090909,0.1478005]
  cl(0:1, 99 ) = [-0.3030303,0.2257121]
  cl(0:1, 100) = [0.3030303,0.3040127]
  cl(0:1, 101) = [0.9090909,0.3827113]
  cl(0:1, 102) = [1.5151515,0.4617569]
  cl(0:1, 103) = [2.1212121,0.5406850]
  cl(0:1, 104) = [2.7272727,0.6188862]
  cl(0:1, 105) = [3.3333333,0.6962278]
  cl(0:1, 106) = [3.9393939,0.7728834]
  cl(0:1, 107) = [4.5454545,0.8475519]
  cl(0:1, 108) = [5.1515152,0.9212716]
  cl(0:1, 109) = [5.7575758,0.9927843]
  cl(0:1, 110) = [6.3636364,1.0613147]
  cl(0:1, 111) = [6.9696970,1.1278742]
  cl(0:1, 112) = [7.5757576,1.1888568]
  cl(0:1, 113) = [8.1818182,1.2472084]
  cl(0:1, 114) = [8.7878788,1.3010916]
  cl(0:1, 115) = [9.3939394,1.3491027]
  cl(0:1, 116) = [10.0000000,1.3912221]
  cl(0:1, 117) = [10.6060606,1.4137469]
  cl(0:1, 118) = [11.2121212,1.4223506]
  cl(0:1, 119) = [11.8181818,1.4050995]
  cl(0:1, 120) = [12.4242424,1.3609846]
  cl(0:1, 121) = [13.0303030,1.2712199]
  cl(0:1, 122) = [13.6363636,1.2333169]
  cl(0:1, 123) = [14.2424242,1.1992998]
  cl(0:1, 124) = [14.8484848,1.1742920]
  cl(0:1, 125) = [15.4545455,1.1569316]
  cl(0:1, 126) = [16.0606061,1.1426602]
  cl(0:1, 127) = [16.6666667,1.1332541]
  cl(0:1, 128) = [17.2727273,1.1268663]
  cl(0:1, 129) = [17.8787879,1.1241677]
  cl(0:1, 130) = [18.4848485,1.1313927]
  cl(0:1, 131) = [19.0909091,1.1386773]
  cl(0:1, 132) = [19.6969697,1.1322387]
  cl(0:1, 133) = [20.3030303,1.1259257]
  cl(0:1, 134) = [20.9090909,1.1197383]
  cl(0:1, 135) = [21.5151515,1.1135509]
  cl(0:1, 136) = [22.1212121,1.1068602]
  cl(0:1, 137) = [22.7272727,1.0981569]
  cl(0:1, 138) = [23.3333333,1.0894536]
  cl(0:1, 139) = [23.9393939,1.0807503]
  cl(0:1, 140) = [24.5454545,1.0737659]
  cl(0:1, 141) = [25.1515152,1.0669725]
  cl(0:1, 142) = [25.7575758,1.0601791]
  cl(0:1, 143) = [26.3636364,1.0541621]
  cl(0:1, 144) = [26.9696970,1.0486629]
  cl(0:1, 145) = [27.5757576,1.0431636]
  cl(0:1, 146) = [28.1818182,1.0379158]
  cl(0:1, 147) = [28.7878788,1.0332545]
  cl(0:1, 148) = [29.3939394,1.0285931]
  cl(0:1, 149) = [30.0000000,1.0239318]
  cl(0:1, 150) = [33.0000000,1.0036098]
  cl(0:1, 151) = [36.0000000,0.9840013]
  cl(0:1, 152) = [39.0000000,0.9630546]
  cl(0:1, 153) = [42.0000000,0.9384869]
  cl(0:1, 154) = [45.0000000,0.9126102]
  cl(0:1, 155) = [48.0000000,0.8795018]
  cl(0:1, 156) = [51.0000000,0.8422027]
  cl(0:1, 157) = [54.0000000,0.7965217]
  cl(0:1, 158) = [57.0000000,0.7508410]
  cl(0:1, 159) = [60.0000000,0.7051605]
  cl(0:1, 160) = [63.0000000,0.6435413]
  cl(0:1, 161) = [66.0000000,0.5819220]
  cl(0:1, 162) = [69.0000000,0.5203027]
  cl(0:1, 163) = [72.0000000,0.4511195]
  cl(0:1, 164) = [75.0000000,0.3781546]
  cl(0:1, 165) = [78.0000000,0.3051886]
  cl(0:1, 166) = [81.0000000,0.2308904]
  cl(0:1, 167) = [84.0000000,0.1539276]
  cl(0:1, 168) = [87.0000000,0.0769638]
  cl(0:1, 169) = [90.0000000,-0.0000003]
  cl(0:1, 170) = [93.0000000,-0.0538748]
  cl(0:1, 171) = [96.0000000,-0.1077492]
  cl(0:1, 172) = [99.0000000,-0.1616236]
  cl(0:1, 173) = [102.0000000,-0.2136321]
  cl(0:1, 174) = [105.0000000,-0.2647077]
  cl(0:1, 175) = [108.0000000,-0.3157837]
  cl(0:1, 176) = [111.0000000,-0.3642120]
  cl(0:1, 177) = [114.0000000,-0.4073452]
  cl(0:1, 178) = [117.0000000,-0.4504785]
  cl(0:1, 179) = [120.0000000,-0.4936120]
  cl(0:1, 180) = [123.0000000,-0.5255887]
  cl(0:1, 181) = [126.0000000,-0.5575653]
  cl(0:1, 182) = [129.0000000,-0.5895418]
  cl(0:1, 183) = [132.0000000,-0.6139641]
  cl(0:1, 184) = [135.0000000,-0.6346093]
  cl(0:1, 185) = [138.0000000,-0.6552538]
  cl(0:1, 186) = [141.0000000,-0.6737902]
  cl(0:1, 187) = [144.0000000,-0.6881108]
  cl(0:1, 188) = [147.0000000,-0.7024308]
  cl(0:1, 189) = [150.0000000,-0.7167506]
  cl(0:1, 190) = [153.0000000,-0.7348789]
  cl(0:1, 191) = [156.0000000,-0.7530070]
  cl(0:1, 192) = [159.0000000,-0.7715645]
  cl(0:1, 193) = [162.0000000,-0.7002562]
  cl(0:1, 194) = [165.0000000,-0.5835466]
  cl(0:1, 195) = [168.0000000,-0.4581257]
  cl(0:1, 196) = [171.0000000,-0.3410453]
  cl(0:1, 197) = [174.0000000,-0.2273634]
  cl(0:1, 198) = [177.0000000,-0.1125416]
  cl(0:1, 199) = [180.0000000,0.0000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl12_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl12 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl12_ge28.pro'
  ENDIF

!print,y,cl12

!_________________________________________________________________________
  
END FUNCTION calc_cl12_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl13_ge28( y ) RESULT( cl13 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl13
  INTEGER :: i

  cl13 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.1168147]
  cl(0:1, 2  ) = [-174.0000000,0.2357573]
  cl(0:1, 3  ) = [-171.0000000,0.3536365]
  cl(0:1, 4  ) = [-168.0000000,0.4698298]
  cl(0:1, 5  ) = [-165.0000000,0.5861004]
  cl(0:1, 6  ) = [-162.0000000,0.6972070]
  cl(0:1, 7  ) = [-159.0000000,0.7650533]
  cl(0:1, 8  ) = [-156.0000000,0.7478402]
  cl(0:1, 9  ) = [-153.0000000,0.7313151]
  cl(0:1, 10 ) = [-150.0000000,0.7147897]
  cl(0:1, 11 ) = [-147.0000000,0.7007001]
  cl(0:1, 12 ) = [-144.0000000,0.6866102]
  cl(0:1, 13 ) = [-141.0000000,0.6725199]
  cl(0:1, 14 ) = [-138.0000000,0.6541578]
  cl(0:1, 15 ) = [-135.0000000,0.6336600]
  cl(0:1, 16 ) = [-132.0000000,0.6131613]
  cl(0:1, 17 ) = [-129.0000000,0.5888694]
  cl(0:1, 18 ) = [-126.0000000,0.5569913]
  cl(0:1, 19 ) = [-123.0000000,0.5251130]
  cl(0:1, 20 ) = [-120.0000000,0.4932347]
  cl(0:1, 21 ) = [-117.0000000,0.4501656]
  cl(0:1, 22 ) = [-114.0000000,0.4070967]
  cl(0:1, 23 ) = [-111.0000000,0.3640278]
  cl(0:1, 24 ) = [-108.0000000,0.3156455]
  cl(0:1, 25 ) = [-105.0000000,0.2646064]
  cl(0:1, 26 ) = [-102.0000000,0.2135676]
  cl(0:1, 27 ) = [-99.0000000,0.1615876]
  cl(0:1, 28 ) = [-96.0000000,0.1077251]
  cl(0:1, 29 ) = [-93.0000000,0.0538627]
  cl(0:1, 30 ) = [-90.0000000,0.0000003]
  cl(0:1, 31 ) = [-87.0000000,-0.0538627]
  cl(0:1, 32 ) = [-84.0000000,-0.1077253]
  cl(0:1, 33 ) = [-81.0000000,-0.1615871]
  cl(0:1, 34 ) = [-78.0000000,-0.2135673]
  cl(0:1, 35 ) = [-75.0000000,-0.2646067]
  cl(0:1, 36 ) = [-72.0000000,-0.3156452]
  cl(0:1, 37 ) = [-69.0000000,-0.3640274]
  cl(0:1, 38 ) = [-66.0000000,-0.4070968]
  cl(0:1, 39 ) = [-63.0000000,-0.4501659]
  cl(0:1, 40 ) = [-60.0000000,-0.4932349]
  cl(0:1, 41 ) = [-57.0000000,-0.5251132]
  cl(0:1, 42 ) = [-54.0000000,-0.5569915]
  cl(0:1, 43 ) = [-51.0000000,-0.5888696]
  cl(0:1, 44 ) = [-48.0000000,-0.6148583]
  cl(0:1, 45 ) = [-45.0000000,-0.6379023]
  cl(0:1, 46 ) = [-42.0000000,-0.6558548]
  cl(0:1, 47 ) = [-39.0000000,-0.6728821]
  cl(0:1, 48 ) = [-36.0000000,-0.6873447]
  cl(0:1, 49 ) = [-33.0000000,-0.7008370]
  cl(0:1, 50 ) = [-30.0000000,-0.7147898]
  cl(0:1, 51 ) = [-29.3939394,-0.7179898]
  cl(0:1, 52 ) = [-28.7878788,-0.7211898]
  cl(0:1, 53 ) = [-28.1818182,-0.7243898]
  cl(0:1, 54 ) = [-27.5757576,-0.7279936]
  cl(0:1, 55 ) = [-26.9696970,-0.7317705]
  cl(0:1, 56 ) = [-26.3636364,-0.7355474]
  cl(0:1, 57 ) = [-25.7575758,-0.7396826]
  cl(0:1, 58 ) = [-25.1515152,-0.7443551]
  cl(0:1, 59 ) = [-24.5454545,-0.7490276]
  cl(0:1, 60 ) = [-23.9393939,-0.7538324]
  cl(0:1, 61 ) = [-23.3333333,-0.7598285]
  cl(0:1, 62 ) = [-22.7272727,-0.7658247]
  cl(0:1, 63 ) = [-22.1212121,-0.7718208]
  cl(0:1, 64 ) = [-21.5151515,-0.7964435]
  cl(0:1, 65 ) = [-20.9090909,-0.8308499]
  cl(0:1, 66 ) = [-20.3030303,-0.8612398]
  cl(0:1, 67 ) = [-19.6969697,-0.8868514]
  cl(0:1, 68 ) = [-19.0909091,-0.9078495]
  cl(0:1, 69 ) = [-18.4848485,-0.9284387]
  cl(0:1, 70 ) = [-17.8787879,-0.9487980]
  cl(0:1, 71 ) = [-17.2727273,-0.9690157]
  cl(0:1, 72 ) = [-16.6666667,-0.9891392]
  cl(0:1, 73 ) = [-16.0606061,-1.0091972]
  cl(0:1, 74 ) = [-15.4545455,-1.0154076]
  cl(0:1, 75 ) = [-14.8484848,-1.0129843]
  cl(0:1, 76 ) = [-14.2424242,-0.9892038]
  cl(0:1, 77 ) = [-13.6363636,-0.9620147]
  cl(0:1, 78 ) = [-13.0303030,-0.9324493]
  cl(0:1, 79 ) = [-12.4242424,-0.9007925]
  cl(0:1, 80 ) = [-11.8181818,-0.8688487]
  cl(0:1, 81 ) = [-11.2121212,-0.8368086]
  cl(0:1, 82 ) = [-10.6060606,-0.8032842]
  cl(0:1, 83 ) = [-10.0000000,-0.7680970]
  cl(0:1, 84 ) = [-9.3939394,-0.7397376]
  cl(0:1, 85 ) = [-8.7878788,-0.7177916]
  cl(0:1, 86 ) = [-8.1818182,-0.7044576]
  cl(0:1, 87 ) = [-7.5757576,-0.6632328]
  cl(0:1, 88 ) = [-6.9696970,-0.6089914]
  cl(0:1, 89 ) = [-6.3636364,-0.5336520]
  cl(0:1, 90 ) = [-5.7575758,-0.4576458]
  cl(0:1, 91 ) = [-5.1515152,-0.3808782]
  cl(0:1, 92 ) = [-4.5454545,-0.3033286]
  cl(0:1, 93 ) = [-3.9393939,-0.2256389]
  cl(0:1, 94 ) = [-3.3333333,-0.1517591]
  cl(0:1, 95 ) = [-2.7272727,-0.0772804]
  cl(0:1, 96 ) = [-2.1212121,-0.0021417]
  cl(0:1, 97 ) = [-1.5151515,0.0742011]
  cl(0:1, 98 ) = [-0.9090909,0.1511092]
  cl(0:1, 99 ) = [-0.3030303,0.2293440]
  cl(0:1, 100) = [0.3030303,0.3077592]
  cl(0:1, 101) = [0.9090909,0.3863636]
  cl(0:1, 102) = [1.5151515,0.4651551]
  cl(0:1, 103) = [2.1212121,0.5437424]
  cl(0:1, 104) = [2.7272727,0.6213556]
  cl(0:1, 105) = [3.3333333,0.6982477]
  cl(0:1, 106) = [3.9393939,0.7745773]
  cl(0:1, 107) = [4.5454545,0.8491236]
  cl(0:1, 108) = [5.1515152,0.9227688]
  cl(0:1, 109) = [5.7575758,0.9942818]
  cl(0:1, 110) = [6.3636364,1.0631053]
  cl(0:1, 111) = [6.9696970,1.1301679]
  cl(0:1, 112) = [7.5757576,1.1923572]
  cl(0:1, 113) = [8.1818182,1.2525345]
  cl(0:1, 114) = [8.7878788,1.3102282]
  cl(0:1, 115) = [9.3939394,1.3635114]
  cl(0:1, 116) = [10.0000000,1.4099778]
  cl(0:1, 117) = [10.6060606,1.4392373]
  cl(0:1, 118) = [11.2121212,1.4562533]
  cl(0:1, 119) = [11.8181818,1.4505284]
  cl(0:1, 120) = [12.4242424,1.3773893]
  cl(0:1, 121) = [13.0303030,1.2540205]
  cl(0:1, 122) = [13.6363636,1.2064269]
  cl(0:1, 123) = [14.2424242,1.1651282]
  cl(0:1, 124) = [14.8484848,1.1384517]
  cl(0:1, 125) = [15.4545455,1.1201215]
  cl(0:1, 126) = [16.0606061,1.1052116]
  cl(0:1, 127) = [16.6666667,1.0960511]
  cl(0:1, 128) = [17.2727273,1.0903847]
  cl(0:1, 129) = [17.8787879,1.0889892]
  cl(0:1, 130) = [18.4848485,1.0985269]
  cl(0:1, 131) = [19.0909091,1.1090080]
  cl(0:1, 132) = [19.6969697,1.1093453]
  cl(0:1, 133) = [20.3030303,1.1087262]
  cl(0:1, 134) = [20.9090909,1.1071506]
  cl(0:1, 135) = [21.5151515,1.1055751]
  cl(0:1, 136) = [22.1212121,1.1026012]
  cl(0:1, 137) = [22.7272727,1.0940358]
  cl(0:1, 138) = [23.3333333,1.0854703]
  cl(0:1, 139) = [23.9393939,1.0769048]
  cl(0:1, 140) = [24.5454545,1.0700404]
  cl(0:1, 141) = [25.1515152,1.0633650]
  cl(0:1, 142) = [25.7575758,1.0566896]
  cl(0:1, 143) = [26.3636364,1.0507817]
  cl(0:1, 144) = [26.9696970,1.0453855]
  cl(0:1, 145) = [27.5757576,1.0399892]
  cl(0:1, 146) = [28.1818182,1.0348409]
  cl(0:1, 147) = [28.7878788,1.0302710]
  cl(0:1, 148) = [29.3939394,1.0257011]
  cl(0:1, 149) = [30.0000000,1.0211312]
  cl(0:1, 150) = [33.0000000,1.0011946]
  cl(0:1, 151) = [36.0000000,0.9819217]
  cl(0:1, 152) = [39.0000000,0.9612603]
  cl(0:1, 153) = [42.0000000,0.9369357]
  cl(0:1, 154) = [45.0000000,0.9112896]
  cl(0:1, 155) = [48.0000000,0.8783691]
  cl(0:1, 156) = [51.0000000,0.8412420]
  cl(0:1, 157) = [54.0000000,0.7957015]
  cl(0:1, 158) = [57.0000000,0.7501613]
  cl(0:1, 159) = [60.0000000,0.7046213]
  cl(0:1, 160) = [63.0000000,0.6430941]
  cl(0:1, 161) = [66.0000000,0.5815668]
  cl(0:1, 162) = [69.0000000,0.5200394]
  cl(0:1, 163) = [72.0000000,0.4509221]
  cl(0:1, 164) = [75.0000000,0.3780098]
  cl(0:1, 165) = [78.0000000,0.3050964]
  cl(0:1, 166) = [81.0000000,0.2308389]
  cl(0:1, 167) = [84.0000000,0.1538933]
  cl(0:1, 168) = [87.0000000,0.0769466]
  cl(0:1, 169) = [90.0000000,-0.0000003]
  cl(0:1, 170) = [93.0000000,-0.0538627]
  cl(0:1, 171) = [96.0000000,-0.1077251]
  cl(0:1, 172) = [99.0000000,-0.1615876]
  cl(0:1, 173) = [102.0000000,-0.2135676]
  cl(0:1, 174) = [105.0000000,-0.2646064]
  cl(0:1, 175) = [108.0000000,-0.3156455]
  cl(0:1, 176) = [111.0000000,-0.3640278]
  cl(0:1, 177) = [114.0000000,-0.4070967]
  cl(0:1, 178) = [117.0000000,-0.4501656]
  cl(0:1, 179) = [120.0000000,-0.4932347]
  cl(0:1, 180) = [123.0000000,-0.5251130]
  cl(0:1, 181) = [126.0000000,-0.5569913]
  cl(0:1, 182) = [129.0000000,-0.5888694]
  cl(0:1, 183) = [132.0000000,-0.6131613]
  cl(0:1, 184) = [135.0000000,-0.6336600]
  cl(0:1, 185) = [138.0000000,-0.6541578]
  cl(0:1, 186) = [141.0000000,-0.6725199]
  cl(0:1, 187) = [144.0000000,-0.6866102]
  cl(0:1, 188) = [147.0000000,-0.7007001]
  cl(0:1, 189) = [150.0000000,-0.7147897]
  cl(0:1, 190) = [153.0000000,-0.7260834]
  cl(0:1, 191) = [156.0000000,-0.7373769]
  cl(0:1, 192) = [159.0000000,-0.7493696]
  cl(0:1, 193) = [162.0000000,-0.6784876]
  cl(0:1, 194) = [165.0000000,-0.5654061]
  cl(0:1, 195) = [168.0000000,-0.4448942]
  cl(0:1, 196) = [171.0000000,-0.3314965]
  cl(0:1, 197) = [174.0000000,-0.2209976]
  cl(0:1, 198) = [177.0000000,-0.1095755]
  cl(0:1, 199) = [180.0000000,0.0000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl13_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl13 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl13_ge28.pro'
  ENDIF

!print,y,cl13

!_________________________________________________________________________
  
END FUNCTION calc_cl13_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl14_ge28( y ) RESULT( cl14 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl14
  INTEGER :: i

  cl14 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.1136020]
  cl(0:1, 2  ) = [-174.0000000,0.2288584]
  cl(0:1, 3  ) = [-171.0000000,0.3432883]
  cl(0:1, 4  ) = [-168.0000000,0.4570708]
  cl(0:1, 5  ) = [-165.0000000,0.5711291]
  cl(0:1, 6  ) = [-162.0000000,0.6818112]
  cl(0:1, 7  ) = [-159.0000000,0.7506445]
  cl(0:1, 8  ) = [-156.0000000,0.7375238]
  cl(0:1, 9  ) = [-153.0000000,0.7252238]
  cl(0:1, 10 ) = [-150.0000000,0.7129237]
  cl(0:1, 11 ) = [-147.0000000,0.6990531]
  cl(0:1, 12 ) = [-144.0000000,0.6851823]
  cl(0:1, 13 ) = [-141.0000000,0.6713110]
  cl(0:1, 14 ) = [-138.0000000,0.6531149]
  cl(0:1, 15 ) = [-135.0000000,0.6327565]
  cl(0:1, 16 ) = [-132.0000000,0.6123973]
  cl(0:1, 17 ) = [-129.0000000,0.5882296]
  cl(0:1, 18 ) = [-126.0000000,0.5564451]
  cl(0:1, 19 ) = [-123.0000000,0.5246604]
  cl(0:1, 20 ) = [-120.0000000,0.4928756]
  cl(0:1, 21 ) = [-117.0000000,0.4498679]
  cl(0:1, 22 ) = [-114.0000000,0.4068602]
  cl(0:1, 23 ) = [-111.0000000,0.3638525]
  cl(0:1, 24 ) = [-108.0000000,0.3155140]
  cl(0:1, 25 ) = [-105.0000000,0.2645100]
  cl(0:1, 26 ) = [-102.0000000,0.2135061]
  cl(0:1, 27 ) = [-99.0000000,0.1615532]
  cl(0:1, 28 ) = [-96.0000000,0.1077023]
  cl(0:1, 29 ) = [-93.0000000,0.0538513]
  cl(0:1, 30 ) = [-90.0000000,0.0000003]
  cl(0:1, 31 ) = [-87.0000000,-0.0538512]
  cl(0:1, 32 ) = [-84.0000000,-0.1077024]
  cl(0:1, 33 ) = [-81.0000000,-0.1615528]
  cl(0:1, 34 ) = [-78.0000000,-0.2135059]
  cl(0:1, 35 ) = [-75.0000000,-0.2645103]
  cl(0:1, 36 ) = [-72.0000000,-0.3155137]
  cl(0:1, 37 ) = [-69.0000000,-0.3638521]
  cl(0:1, 38 ) = [-66.0000000,-0.4068603]
  cl(0:1, 39 ) = [-63.0000000,-0.4498682]
  cl(0:1, 40 ) = [-60.0000000,-0.4928758]
  cl(0:1, 41 ) = [-57.0000000,-0.5246606]
  cl(0:1, 42 ) = [-54.0000000,-0.5564453]
  cl(0:1, 43 ) = [-51.0000000,-0.5882298]
  cl(0:1, 44 ) = [-48.0000000,-0.6141038]
  cl(0:1, 45 ) = [-45.0000000,-0.6370226]
  cl(0:1, 46 ) = [-42.0000000,-0.6548214]
  cl(0:1, 47 ) = [-39.0000000,-0.6716868]
  cl(0:1, 48 ) = [-36.0000000,-0.6859594]
  cl(0:1, 49 ) = [-33.0000000,-0.6992283]
  cl(0:1, 50 ) = [-30.0000000,-0.7129238]
  cl(0:1, 51 ) = [-29.3939394,-0.7160632]
  cl(0:1, 52 ) = [-28.7878788,-0.7192027]
  cl(0:1, 53 ) = [-28.1818182,-0.7223421]
  cl(0:1, 54 ) = [-27.5757576,-0.7258796]
  cl(0:1, 55 ) = [-26.9696970,-0.7295879]
  cl(0:1, 56 ) = [-26.3636364,-0.7332961]
  cl(0:1, 57 ) = [-25.7575758,-0.7373586]
  cl(0:1, 58 ) = [-25.1515152,-0.7419523]
  cl(0:1, 59 ) = [-24.5454545,-0.7465460]
  cl(0:1, 60 ) = [-23.9393939,-0.7512708]
  cl(0:1, 61 ) = [-23.3333333,-0.7571753]
  cl(0:1, 62 ) = [-22.7272727,-0.7630797]
  cl(0:1, 63 ) = [-22.1212121,-0.7689842]
  cl(0:1, 64 ) = [-21.5151515,-0.7985587]
  cl(0:1, 65 ) = [-20.9090909,-0.8399770]
  cl(0:1, 66 ) = [-20.3030303,-0.8776261]
  cl(0:1, 67 ) = [-19.6969697,-0.9124823]
  cl(0:1, 68 ) = [-19.0909091,-0.9446534]
  cl(0:1, 69 ) = [-18.4848485,-0.9765567]
  cl(0:1, 70 ) = [-17.8787879,-1.0083099]
  cl(0:1, 71 ) = [-17.2727273,-1.0399710]
  cl(0:1, 72 ) = [-16.6666667,-1.0715705]
  cl(0:1, 73 ) = [-16.0606061,-1.1031272]
  cl(0:1, 74 ) = [-15.4545455,-1.1150712]
  cl(0:1, 75 ) = [-14.8484848,-1.1147727]
  cl(0:1, 76 ) = [-14.2424242,-1.0842326]
  cl(0:1, 77 ) = [-13.6363636,-1.0489603]
  cl(0:1, 78 ) = [-13.0303030,-1.0104637]
  cl(0:1, 79 ) = [-12.4242424,-0.9692019]
  cl(0:1, 80 ) = [-11.8181818,-0.9277674]
  cl(0:1, 81 ) = [-11.2121212,-0.8864806]
  cl(0:1, 82 ) = [-10.6060606,-0.8435143]
  cl(0:1, 83 ) = [-10.0000000,-0.7988563]
  cl(0:1, 84 ) = [-9.3939394,-0.7634132]
  cl(0:1, 85 ) = [-8.7878788,-0.7368552]
  cl(0:1, 86 ) = [-8.1818182,-0.7219152]
  cl(0:1, 87 ) = [-7.5757576,-0.6801078]
  cl(0:1, 88 ) = [-6.9696970,-0.6254773]
  cl(0:1, 89 ) = [-6.3636364,-0.5453854]
  cl(0:1, 90 ) = [-5.7575758,-0.4652409]
  cl(0:1, 91 ) = [-5.1515152,-0.3851773]
  cl(0:1, 92 ) = [-4.5454545,-0.3051860]
  cl(0:1, 93 ) = [-3.9393939,-0.2254966]
  cl(0:1, 94 ) = [-3.3333333,-0.1500959]
  cl(0:1, 95 ) = [-2.7272727,-0.0744808]
  cl(0:1, 96 ) = [-2.1212121,0.0013624]
  cl(0:1, 97 ) = [-1.5151515,0.0779763]
  cl(0:1, 98 ) = [-0.9090909,0.1550932]
  cl(0:1, 99 ) = [-0.3030303,0.2336910]
  cl(0:1, 100) = [0.3030303,0.3122304]
  cl(0:1, 101) = [0.9090909,0.3907236]
  cl(0:1, 102) = [1.5151515,0.4692234]
  cl(0:1, 103) = [2.1212121,0.5474190]
  cl(0:1, 104) = [2.7272727,0.6243535]
  cl(0:1, 105) = [3.3333333,0.7007190]
  cl(0:1, 106) = [3.9393939,0.7766621]
  cl(0:1, 107) = [4.5454545,0.8510625]
  cl(0:1, 108) = [5.1515152,0.9246183]
  cl(0:1, 109) = [5.7575758,0.9961310]
  cl(0:1, 110) = [6.3636364,1.0653012]
  cl(0:1, 111) = [6.9696970,1.1329492]
  cl(0:1, 112) = [7.5757576,1.1964783]
  cl(0:1, 113) = [8.1818182,1.2585221]
  cl(0:1, 114) = [8.7878788,1.3195326]
  cl(0:1, 115) = [9.3939394,1.3772228]
  cl(0:1, 116) = [10.0000000,1.4278259]
  cl(0:1, 117) = [10.6060606,1.4634942]
  cl(0:1, 118) = [11.2121212,1.4885153]
  cl(0:1, 119) = [11.8181818,1.4937590]
  cl(0:1, 120) = [12.4242424,1.3901918]
  cl(0:1, 121) = [13.0303030,1.2355829]
  cl(0:1, 122) = [13.6363636,1.1798362]
  cl(0:1, 123) = [14.2424242,1.1326103]
  cl(0:1, 124) = [14.8484848,1.1043457]
  cl(0:1, 125) = [15.4545455,1.0850927]
  cl(0:1, 126) = [16.0606061,1.0695752]
  cl(0:1, 127) = [16.6666667,1.0606483]
  cl(0:1, 128) = [17.2727273,1.0556686]
  cl(0:1, 129) = [17.8787879,1.0555130]
  cl(0:1, 130) = [18.4848485,1.0672515]
  cl(0:1, 131) = [19.0909091,1.0807744]
  cl(0:1, 132) = [19.6969697,1.0875597]
  cl(0:1, 133) = [20.3030303,1.0923589]
  cl(0:1, 134) = [20.9090909,1.0951721]
  cl(0:1, 135) = [21.5151515,1.0979852]
  cl(0:1, 136) = [22.1212121,1.0985484]
  cl(0:1, 137) = [22.7272727,1.0901141]
  cl(0:1, 138) = [23.3333333,1.0816797]
  cl(0:1, 139) = [23.9393939,1.0732454]
  cl(0:1, 140) = [24.5454545,1.0664952]
  cl(0:1, 141) = [25.1515152,1.0599321]
  cl(0:1, 142) = [25.7575758,1.0533690]
  cl(0:1, 143) = [26.3636364,1.0475648]
  cl(0:1, 144) = [26.9696970,1.0422666]
  cl(0:1, 145) = [27.5757576,1.0369684]
  cl(0:1, 146) = [28.1818182,1.0319148]
  cl(0:1, 147) = [28.7878788,1.0274319]
  cl(0:1, 148) = [29.3939394,1.0229490]
  cl(0:1, 149) = [30.0000000,1.0184661]
  cl(0:1, 150) = [33.0000000,0.9988963]
  cl(0:1, 151) = [36.0000000,0.9799428]
  cl(0:1, 152) = [39.0000000,0.9595528]
  cl(0:1, 153) = [42.0000000,0.9354596]
  cl(0:1, 154) = [45.0000000,0.9100328]
  cl(0:1, 155) = [48.0000000,0.8772912]
  cl(0:1, 156) = [51.0000000,0.8403279]
  cl(0:1, 157) = [54.0000000,0.7949210]
  cl(0:1, 158) = [57.0000000,0.7495146]
  cl(0:1, 159) = [60.0000000,0.7041082]
  cl(0:1, 160) = [63.0000000,0.6426686]
  cl(0:1, 161) = [66.0000000,0.5812288]
  cl(0:1, 162) = [69.0000000,0.5197890]
  cl(0:1, 163) = [72.0000000,0.4507342]
  cl(0:1, 164) = [75.0000000,0.3778721]
  cl(0:1, 165) = [78.0000000,0.3050087]
  cl(0:1, 166) = [81.0000000,0.2307900]
  cl(0:1, 167) = [84.0000000,0.1538606]
  cl(0:1, 168) = [87.0000000,0.0769303]
  cl(0:1, 169) = [90.0000000,-0.0000003]
  cl(0:1, 170) = [93.0000000,-0.0538513]
  cl(0:1, 171) = [96.0000000,-0.1077023]
  cl(0:1, 172) = [99.0000000,-0.1615532]
  cl(0:1, 173) = [102.0000000,-0.2135061]
  cl(0:1, 174) = [105.0000000,-0.2645100]
  cl(0:1, 175) = [108.0000000,-0.3155140]
  cl(0:1, 176) = [111.0000000,-0.3638525]
  cl(0:1, 177) = [114.0000000,-0.4068602]
  cl(0:1, 178) = [117.0000000,-0.4498679]
  cl(0:1, 179) = [120.0000000,-0.4928756]
  cl(0:1, 180) = [123.0000000,-0.5246604]
  cl(0:1, 181) = [126.0000000,-0.5564451]
  cl(0:1, 182) = [129.0000000,-0.5882296]
  cl(0:1, 183) = [132.0000000,-0.6123973]
  cl(0:1, 184) = [135.0000000,-0.6327565]
  cl(0:1, 185) = [138.0000000,-0.6531149]
  cl(0:1, 186) = [141.0000000,-0.6713110]
  cl(0:1, 187) = [144.0000000,-0.6851823]
  cl(0:1, 188) = [147.0000000,-0.6990531]
  cl(0:1, 189) = [150.0000000,-0.7129237]
  cl(0:1, 190) = [153.0000000,-0.7177135]
  cl(0:1, 191) = [156.0000000,-0.7225032]
  cl(0:1, 192) = [159.0000000,-0.7281271]
  cl(0:1, 193) = [162.0000000,-0.6575469]
  cl(0:1, 194) = [165.0000000,-0.5479556]
  cl(0:1, 195) = [168.0000000,-0.4335088]
  cl(0:1, 196) = [171.0000000,-0.3237110]
  cl(0:1, 197) = [174.0000000,-0.2158073]
  cl(0:1, 198) = [177.0000000,-0.1071734]
  cl(0:1, 199) = [180.0000000,0.0000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl14_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl14 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl14_ge28.pro'
  ENDIF

!print,y,cl14

!_________________________________________________________________________
  
END FUNCTION calc_cl14_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl15_ge28( y ) RESULT( cl15 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl15
  INTEGER :: i

  cl15 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.1110002]
  cl(0:1, 2  ) = [-174.0000000,0.2232495]
  cl(0:1, 3  ) = [-171.0000000,0.3348750]
  cl(0:1, 4  ) = [-168.0000000,0.4463519]
  cl(0:1, 5  ) = [-165.0000000,0.5579991]
  cl(0:1, 6  ) = [-162.0000000,0.6682566]
  cl(0:1, 7  ) = [-159.0000000,0.7381120]
  cl(0:1, 8  ) = [-156.0000000,0.7287014]
  cl(0:1, 9  ) = [-153.0000000,0.7200148]
  cl(0:1, 10 ) = [-150.0000000,0.7113280]
  cl(0:1, 11 ) = [-147.0000000,0.6976447]
  cl(0:1, 12 ) = [-144.0000000,0.6839612]
  cl(0:1, 13 ) = [-141.0000000,0.6702771]
  cl(0:1, 14 ) = [-138.0000000,0.6522230]
  cl(0:1, 15 ) = [-135.0000000,0.6319839]
  cl(0:1, 16 ) = [-132.0000000,0.6117440]
  cl(0:1, 17 ) = [-129.0000000,0.5876825]
  cl(0:1, 18 ) = [-126.0000000,0.5559780]
  cl(0:1, 19 ) = [-123.0000000,0.5242733]
  cl(0:1, 20 ) = [-120.0000000,0.4925686]
  cl(0:1, 21 ) = [-117.0000000,0.4496133]
  cl(0:1, 22 ) = [-114.0000000,0.4066580]
  cl(0:1, 23 ) = [-111.0000000,0.3637026]
  cl(0:1, 24 ) = [-108.0000000,0.3154015]
  cl(0:1, 25 ) = [-105.0000000,0.2644275]
  cl(0:1, 26 ) = [-102.0000000,0.2134536]
  cl(0:1, 27 ) = [-99.0000000,0.1615239]
  cl(0:1, 28 ) = [-96.0000000,0.1076827]
  cl(0:1, 29 ) = [-93.0000000,0.0538415]
  cl(0:1, 30 ) = [-90.0000000,0.0000003]
  cl(0:1, 31 ) = [-87.0000000,-0.0538415]
  cl(0:1, 32 ) = [-84.0000000,-0.1076828]
  cl(0:1, 33 ) = [-81.0000000,-0.1615234]
  cl(0:1, 34 ) = [-78.0000000,-0.2134533]
  cl(0:1, 35 ) = [-75.0000000,-0.2644279]
  cl(0:1, 36 ) = [-72.0000000,-0.3154013]
  cl(0:1, 37 ) = [-69.0000000,-0.3637022]
  cl(0:1, 38 ) = [-66.0000000,-0.4066581]
  cl(0:1, 39 ) = [-63.0000000,-0.4496136]
  cl(0:1, 40 ) = [-60.0000000,-0.4925688]
  cl(0:1, 41 ) = [-57.0000000,-0.5242735]
  cl(0:1, 42 ) = [-54.0000000,-0.5559782]
  cl(0:1, 43 ) = [-51.0000000,-0.5876827]
  cl(0:1, 44 ) = [-48.0000000,-0.6134586]
  cl(0:1, 45 ) = [-45.0000000,-0.6362704]
  cl(0:1, 46 ) = [-42.0000000,-0.6539377]
  cl(0:1, 47 ) = [-39.0000000,-0.6706645]
  cl(0:1, 48 ) = [-36.0000000,-0.6847748]
  cl(0:1, 49 ) = [-33.0000000,-0.6978525]
  cl(0:1, 50 ) = [-30.0000000,-0.7113281]
  cl(0:1, 51 ) = [-29.3939394,-0.7144157]
  cl(0:1, 52 ) = [-28.7878788,-0.7175033]
  cl(0:1, 53 ) = [-28.1818182,-0.7205909]
  cl(0:1, 54 ) = [-27.5757576,-0.7240718]
  cl(0:1, 55 ) = [-26.9696970,-0.7277213]
  cl(0:1, 56 ) = [-26.3636364,-0.7313708]
  cl(0:1, 57 ) = [-25.7575758,-0.7353711]
  cl(0:1, 58 ) = [-25.1515152,-0.7398975]
  cl(0:1, 59 ) = [-24.5454545,-0.7444239]
  cl(0:1, 60 ) = [-23.9393939,-0.7490802]
  cl(0:1, 61 ) = [-23.3333333,-0.7549063]
  cl(0:1, 62 ) = [-22.7272727,-0.7607324]
  cl(0:1, 63 ) = [-22.1212121,-0.7665584]
  cl(0:1, 64 ) = [-21.5151515,-0.8006876]
  cl(0:1, 65 ) = [-20.9090909,-0.8470116]
  cl(0:1, 66 ) = [-20.3030303,-0.8905943]
  cl(0:1, 67 ) = [-19.6969697,-0.9331165]
  cl(0:1, 68 ) = [-19.0909091,-0.9746225]
  cl(0:1, 69 ) = [-18.4848485,-1.0160177]
  cl(0:1, 70 ) = [-17.8787879,-1.0573512]
  cl(0:1, 71 ) = [-17.2727273,-1.0986475]
  cl(0:1, 72 ) = [-16.6666667,-1.1399185]
  cl(0:1, 73 ) = [-16.0606061,-1.1811720]
  cl(0:1, 74 ) = [-15.4545455,-1.1979127]
  cl(0:1, 75 ) = [-14.8484848,-1.1993419]
  cl(0:1, 76 ) = [-14.2424242,-1.1629790]
  cl(0:1, 77 ) = [-13.6363636,-1.1207704]
  cl(0:1, 78 ) = [-13.0303030,-1.0746332]
  cl(0:1, 79 ) = [-12.4242424,-1.0251816]
  cl(0:1, 80 ) = [-11.8181818,-0.9756819]
  cl(0:1, 81 ) = [-11.2121212,-0.9265711]
  cl(0:1, 82 ) = [-10.6060606,-0.8756649]
  cl(0:1, 83 ) = [-10.0000000,-0.8232371]
  cl(0:1, 84 ) = [-9.3939394,-0.7821113]
  cl(0:1, 85 ) = [-8.7878788,-0.7513752]
  cl(0:1, 86 ) = [-8.1818182,-0.7350881]
  cl(0:1, 87 ) = [-7.5757576,-0.6928361]
  cl(0:1, 88 ) = [-6.9696970,-0.6379499]
  cl(0:1, 89 ) = [-6.3636364,-0.5541613]
  cl(0:1, 90 ) = [-5.7575758,-0.4708401]
  cl(0:1, 91 ) = [-5.1515152,-0.3882920]
  cl(0:1, 92 ) = [-4.5454545,-0.3065024]
  cl(0:1, 93 ) = [-3.9393939,-0.2253108]
  cl(0:1, 94 ) = [-3.3333333,-0.1479882]
  cl(0:1, 95 ) = [-2.7272727,-0.0709984]
  cl(0:1, 96 ) = [-2.1212121,0.0056782]
  cl(0:1, 97 ) = [-1.5151515,0.0826103]
  cl(0:1, 98 ) = [-0.9090909,0.1599690]
  cl(0:1, 99 ) = [-0.3030303,0.2389744]
  cl(0:1, 100) = [0.3030303,0.3176469]
  cl(0:1, 101) = [0.9090909,0.3960074]
  cl(0:1, 102) = [1.5151515,0.4741694]
  cl(0:1, 103) = [2.1212121,0.5519119]
  cl(0:1, 104) = [2.7272727,0.6280566]
  cl(0:1, 105) = [3.3333333,0.7037981]
  cl(0:1, 106) = [3.9393939,0.7792765]
  cl(0:1, 107) = [4.5454545,0.8534998]
  cl(0:1, 108) = [5.1515152,0.9269466]
  cl(0:1, 109) = [5.7575758,0.9984584]
  cl(0:1, 110) = [6.3636364,1.0680443]
  cl(0:1, 111) = [6.9696970,1.1363802]
  cl(0:1, 112) = [7.5757576,1.2013915]
  cl(0:1, 113) = [8.1818182,1.2652587]
  cl(0:1, 114) = [8.7878788,1.3285617]
  cl(0:1, 115) = [9.3939394,1.3889485]
  cl(0:1, 116) = [10.0000000,1.4430891]
  cl(0:1, 117) = [10.6060606,1.4842380]
  cl(0:1, 118) = [11.2121212,1.5161050]
  cl(0:1, 119) = [11.8181818,1.5307287]
  cl(0:1, 120) = [12.4242424,1.3997785]
  cl(0:1, 121) = [13.0303030,1.2161754]
  cl(0:1, 122) = [13.6363636,1.1553350]
  cl(0:1, 123) = [14.2424242,1.1048017]
  cl(0:1, 124) = [14.8484848,1.0751791]
  cl(0:1, 125) = [15.4545455,1.0551370]
  cl(0:1, 126) = [16.0606061,1.0390998]
  cl(0:1, 127) = [16.6666667,1.0303729]
  cl(0:1, 128) = [17.2727273,1.0259802]
  cl(0:1, 129) = [17.8787879,1.0268851]
  cl(0:1, 130) = [18.4848485,1.0405056]
  cl(0:1, 131) = [19.0909091,1.0566297]
  cl(0:1, 132) = [19.6969697,1.0689292]
  cl(0:1, 133) = [20.3030303,1.0783621]
  cl(0:1, 134) = [20.9090909,1.0849283]
  cl(0:1, 135) = [21.5151515,1.0914946]
  cl(0:1, 136) = [22.1212121,1.0950825]
  cl(0:1, 137) = [22.7272727,1.0867603]
  cl(0:1, 138) = [23.3333333,1.0784381]
  cl(0:1, 139) = [23.9393939,1.0701159]
  cl(0:1, 140) = [24.5454545,1.0634634]
  cl(0:1, 141) = [25.1515152,1.0569963]
  cl(0:1, 142) = [25.7575758,1.0505293]
  cl(0:1, 143) = [26.3636364,1.0448138]
  cl(0:1, 144) = [26.9696970,1.0395995]
  cl(0:1, 145) = [27.5757576,1.0343851]
  cl(0:1, 146) = [28.1818182,1.0294125]
  cl(0:1, 147) = [28.7878788,1.0250040]
  cl(0:1, 148) = [29.3939394,1.0205954]
  cl(0:1, 149) = [30.0000000,1.0161869]
  cl(0:1, 150) = [33.0000000,0.9969308]
  cl(0:1, 151) = [36.0000000,0.9782505]
  cl(0:1, 152) = [39.0000000,0.9580926]
  cl(0:1, 153) = [42.0000000,0.9341972]
  cl(0:1, 154) = [45.0000000,0.9089581]
  cl(0:1, 155) = [48.0000000,0.8763694]
  cl(0:1, 156) = [51.0000000,0.8395461]
  cl(0:1, 157) = [54.0000000,0.7942536]
  cl(0:1, 158) = [57.0000000,0.7489614]
  cl(0:1, 159) = [60.0000000,0.7036695]
  cl(0:1, 160) = [63.0000000,0.6423046]
  cl(0:1, 161) = [66.0000000,0.5809397]
  cl(0:1, 162) = [69.0000000,0.5195748]
  cl(0:1, 163) = [72.0000000,0.4505735]
  cl(0:1, 164) = [75.0000000,0.3777543]
  cl(0:1, 165) = [78.0000000,0.3049336]
  cl(0:1, 166) = [81.0000000,0.2307481]
  cl(0:1, 167) = [84.0000000,0.1538327]
  cl(0:1, 168) = [87.0000000,0.0769164]
  cl(0:1, 169) = [90.0000000,-0.0000003]
  cl(0:1, 170) = [93.0000000,-0.0538415]
  cl(0:1, 171) = [96.0000000,-0.1076827]
  cl(0:1, 172) = [99.0000000,-0.1615239]
  cl(0:1, 173) = [102.0000000,-0.2134536]
  cl(0:1, 174) = [105.0000000,-0.2644275]
  cl(0:1, 175) = [108.0000000,-0.3154015]
  cl(0:1, 176) = [111.0000000,-0.3637026]
  cl(0:1, 177) = [114.0000000,-0.4066580]
  cl(0:1, 178) = [117.0000000,-0.4496133]
  cl(0:1, 179) = [120.0000000,-0.4925686]
  cl(0:1, 180) = [123.0000000,-0.5242733]
  cl(0:1, 181) = [126.0000000,-0.5559780]
  cl(0:1, 182) = [129.0000000,-0.5876825]
  cl(0:1, 183) = [132.0000000,-0.6117440]
  cl(0:1, 184) = [135.0000000,-0.6319839]
  cl(0:1, 185) = [138.0000000,-0.6522230]
  cl(0:1, 186) = [141.0000000,-0.6702771]
  cl(0:1, 187) = [144.0000000,-0.6839612]
  cl(0:1, 188) = [147.0000000,-0.6976447]
  cl(0:1, 189) = [150.0000000,-0.7113280]
  cl(0:1, 190) = [153.0000000,-0.7105558]
  cl(0:1, 191) = [156.0000000,-0.7097835]
  cl(0:1, 192) = [159.0000000,-0.7097473]
  cl(0:1, 193) = [162.0000000,-0.6392427]
  cl(0:1, 194) = [165.0000000,-0.5327021]
  cl(0:1, 195) = [168.0000000,-0.4241657]
  cl(0:1, 196) = [171.0000000,-0.3175404]
  cl(0:1, 197) = [174.0000000,-0.2116936]
  cl(0:1, 198) = [177.0000000,-0.1052755]
  cl(0:1, 199) = [180.0000000,0.0000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl15_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl15 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl15_ge28.pro'
  ENDIF

!print,y,cl15

!_________________________________________________________________________
  
END FUNCTION calc_cl15_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl16_ge28( y ) RESULT( cl16 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl16
  INTEGER :: i

  cl16 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.1094089]
  cl(0:1, 2  ) = [-174.0000000,0.2198279]
  cl(0:1, 3  ) = [-171.0000000,0.3297426]
  cl(0:1, 4  ) = [-168.0000000,0.4396529]
  cl(0:1, 5  ) = [-165.0000000,0.5495689]
  cl(0:1, 6  ) = [-162.0000000,0.6594363]
  cl(0:1, 7  ) = [-159.0000000,0.7303005]
  cl(0:1, 8  ) = [-156.0000000,0.7235448]
  cl(0:1, 9  ) = [-153.0000000,0.7169701]
  cl(0:1, 10 ) = [-150.0000000,0.7103953]
  cl(0:1, 11 ) = [-147.0000000,0.6968215]
  cl(0:1, 12 ) = [-144.0000000,0.6832474]
  cl(0:1, 13 ) = [-141.0000000,0.6696729]
  cl(0:1, 14 ) = [-138.0000000,0.6517017]
  cl(0:1, 15 ) = [-135.0000000,0.6315323]
  cl(0:1, 16 ) = [-132.0000000,0.6113621]
  cl(0:1, 17 ) = [-129.0000000,0.5873627]
  cl(0:1, 18 ) = [-126.0000000,0.5557050]
  cl(0:1, 19 ) = [-123.0000000,0.5240471]
  cl(0:1, 20 ) = [-120.0000000,0.4923891]
  cl(0:1, 21 ) = [-117.0000000,0.4494644]
  cl(0:1, 22 ) = [-114.0000000,0.4065398]
  cl(0:1, 23 ) = [-111.0000000,0.3636150]
  cl(0:1, 24 ) = [-108.0000000,0.3153358]
  cl(0:1, 25 ) = [-105.0000000,0.2643793]
  cl(0:1, 26 ) = [-102.0000000,0.2134229]
  cl(0:1, 27 ) = [-99.0000000,0.1615067]
  cl(0:1, 28 ) = [-96.0000000,0.1076713]
  cl(0:1, 29 ) = [-93.0000000,0.0538358]
  cl(0:1, 30 ) = [-90.0000000,0.0000003]
  cl(0:1, 31 ) = [-87.0000000,-0.0538357]
  cl(0:1, 32 ) = [-84.0000000,-0.1076714]
  cl(0:1, 33 ) = [-81.0000000,-0.1615063]
  cl(0:1, 34 ) = [-78.0000000,-0.2134226]
  cl(0:1, 35 ) = [-75.0000000,-0.2643797]
  cl(0:1, 36 ) = [-72.0000000,-0.3153356]
  cl(0:1, 37 ) = [-69.0000000,-0.3636146]
  cl(0:1, 38 ) = [-66.0000000,-0.4065399]
  cl(0:1, 39 ) = [-63.0000000,-0.4494647]
  cl(0:1, 40 ) = [-60.0000000,-0.4923893]
  cl(0:1, 41 ) = [-57.0000000,-0.5240473]
  cl(0:1, 42 ) = [-54.0000000,-0.5557052]
  cl(0:1, 43 ) = [-51.0000000,-0.5873629]
  cl(0:1, 44 ) = [-48.0000000,-0.6130815]
  cl(0:1, 45 ) = [-45.0000000,-0.6358307]
  cl(0:1, 46 ) = [-42.0000000,-0.6534212]
  cl(0:1, 47 ) = [-39.0000000,-0.6700671]
  cl(0:1, 48 ) = [-36.0000000,-0.6840823]
  cl(0:1, 49 ) = [-33.0000000,-0.6970484]
  cl(0:1, 50 ) = [-30.0000000,-0.7103953]
  cl(0:1, 51 ) = [-29.3939394,-0.7134527]
  cl(0:1, 52 ) = [-28.7878788,-0.7165100]
  cl(0:1, 53 ) = [-28.1818182,-0.7195674]
  cl(0:1, 54 ) = [-27.5757576,-0.7230152]
  cl(0:1, 55 ) = [-26.9696970,-0.7266303]
  cl(0:1, 56 ) = [-26.3636364,-0.7302455]
  cl(0:1, 57 ) = [-25.7575758,-0.7342094]
  cl(0:1, 58 ) = [-25.1515152,-0.7386964]
  cl(0:1, 59 ) = [-24.5454545,-0.7431835]
  cl(0:1, 60 ) = [-23.9393939,-0.7477998]
  cl(0:1, 61 ) = [-23.3333333,-0.7535801]
  cl(0:1, 62 ) = [-22.7272727,-0.7593603]
  cl(0:1, 63 ) = [-22.1212121,-0.7651406]
  cl(0:1, 64 ) = [-21.5151515,-0.8033027]
  cl(0:1, 65 ) = [-20.9090909,-0.8508240]
  cl(0:1, 66 ) = [-20.3030303,-0.8977684]
  cl(0:1, 67 ) = [-19.6969697,-0.9446782]
  cl(0:1, 68 ) = [-19.0909091,-0.9915551]
  cl(0:1, 69 ) = [-18.4848485,-1.0384268]
  cl(0:1, 70 ) = [-17.8787879,-1.0852965]
  cl(0:1, 71 ) = [-17.2727273,-1.1321660]
  cl(0:1, 72 ) = [-16.6666667,-1.1790347]
  cl(0:1, 73 ) = [-16.0606061,-1.2259027]
  cl(0:1, 74 ) = [-15.4545455,-1.2454058]
  cl(0:1, 75 ) = [-14.8484848,-1.2478100]
  cl(0:1, 76 ) = [-14.2424242,-1.2080274]
  cl(0:1, 77 ) = [-13.6363636,-1.1617552]
  cl(0:1, 78 ) = [-13.0303030,-1.1111511]
  cl(0:1, 79 ) = [-12.4242424,-1.0569219]
  cl(0:1, 80 ) = [-11.8181818,-1.0027281]
  cl(0:1, 81 ) = [-11.2121212,-0.9490766]
  cl(0:1, 82 ) = [-10.6060606,-0.8935818]
  cl(0:1, 83 ) = [-10.0000000,-0.8369859]
  cl(0:1, 84 ) = [-9.3939394,-0.7931664]
  cl(0:1, 85 ) = [-8.7878788,-0.7591698]
  cl(0:1, 86 ) = [-8.1818182,-0.7421055]
  cl(0:1, 87 ) = [-7.5757576,-0.6996146]
  cl(0:1, 88 ) = [-6.9696970,-0.6446088]
  cl(0:1, 89 ) = [-6.3636364,-0.5588023]
  cl(0:1, 90 ) = [-5.7575758,-0.4737650]
  cl(0:1, 91 ) = [-5.1515152,-0.3898944]
  cl(0:1, 92 ) = [-4.5454545,-0.3071663]
  cl(0:1, 93 ) = [-3.9393939,-0.2250542]
  cl(0:1, 94 ) = [-3.3333333,-0.1451727]
  cl(0:1, 95 ) = [-2.7272727,-0.0664482]
  cl(0:1, 96 ) = [-2.1212121,0.0112495]
  cl(0:1, 97 ) = [-1.5151515,0.0885670]
  cl(0:1, 98 ) = [-0.9090909,0.1662133]
  cl(0:1, 99 ) = [-0.3030303,0.2456818]
  cl(0:1, 100) = [0.3030303,0.3244938]
  cl(0:1, 101) = [0.9090909,0.4026896]
  cl(0:1, 102) = [1.5151515,0.4804506]
  cl(0:1, 103) = [2.1212121,0.5576550]
  cl(0:1, 104) = [2.7272727,0.6328541]
  cl(0:1, 105) = [3.3333333,0.7078291]
  cl(0:1, 106) = [3.9393939,0.7827261]
  cl(0:1, 107) = [4.5454545,0.8567250]
  cl(0:1, 108) = [5.1515152,0.9300328]
  cl(0:1, 109) = [5.7575758,1.0015427]
  cl(0:1, 110) = [6.3636364,1.0716467]
  cl(0:1, 111) = [6.9696970,1.1408178]
  cl(0:1, 112) = [7.5757576,1.2074731]
  cl(0:1, 113) = [8.1818182,1.2729309]
  cl(0:1, 114) = [8.7878788,1.3363172]
  cl(0:1, 115) = [9.3939394,1.3958021]
  cl(0:1, 116) = [10.0000000,1.4520104]
  cl(0:1, 117) = [10.6060606,1.4963627]
  cl(0:1, 118) = [11.2121212,1.5322311]
  cl(0:1, 119) = [11.8181818,1.5523372]
  cl(0:1, 120) = [12.4242424,1.4048531]
  cl(0:1, 121) = [13.0303030,1.1964194]
  cl(0:1, 122) = [13.6363636,1.1369435]
  cl(0:1, 123) = [14.2424242,1.0885477]
  cl(0:1, 124) = [14.8484848,1.0581314]
  cl(0:1, 125) = [15.4545455,1.0376281]
  cl(0:1, 126) = [16.0606061,1.0212871]
  cl(0:1, 127) = [16.6666667,1.0126770]
  cl(0:1, 128) = [17.2727273,1.0086275]
  cl(0:1, 129) = [17.8787879,1.0101522]
  cl(0:1, 130) = [18.4848485,1.0248727]
  cl(0:1, 131) = [19.0909091,1.0425173]
  cl(0:1, 132) = [19.6969697,1.0580397]
  cl(0:1, 133) = [20.3030303,1.0701810]
  cl(0:1, 134) = [20.9090909,1.0789409]
  cl(0:1, 135) = [21.5151515,1.0877008]
  cl(0:1, 136) = [22.1212121,1.0930567]
  cl(0:1, 137) = [22.7272727,1.0848001]
  cl(0:1, 138) = [23.3333333,1.0765434]
  cl(0:1, 139) = [23.9393939,1.0682868]
  cl(0:1, 140) = [24.5454545,1.0616913]
  cl(0:1, 141) = [25.1515152,1.0552804]
  cl(0:1, 142) = [25.7575758,1.0488695]
  cl(0:1, 143) = [26.3636364,1.0432059]
  cl(0:1, 144) = [26.9696970,1.0380405]
  cl(0:1, 145) = [27.5757576,1.0328752]
  cl(0:1, 146) = [28.1818182,1.0279499]
  cl(0:1, 147) = [28.7878788,1.0235849]
  cl(0:1, 148) = [29.3939394,1.0192198]
  cl(0:1, 149) = [30.0000000,1.0148548]
  cl(0:1, 150) = [33.0000000,0.9957820]
  cl(0:1, 151) = [36.0000000,0.9772613]
  cl(0:1, 152) = [39.0000000,0.9572392]
  cl(0:1, 153) = [42.0000000,0.9334594]
  cl(0:1, 154) = [45.0000000,0.9083300]
  cl(0:1, 155) = [48.0000000,0.8758306]
  cl(0:1, 156) = [51.0000000,0.8390892]
  cl(0:1, 157) = [54.0000000,0.7938635]
  cl(0:1, 158) = [57.0000000,0.7486382]
  cl(0:1, 159) = [60.0000000,0.7034130]
  cl(0:1, 160) = [63.0000000,0.6420919]
  cl(0:1, 161) = [66.0000000,0.5807707]
  cl(0:1, 162) = [69.0000000,0.5194495]
  cl(0:1, 163) = [72.0000000,0.4504796]
  cl(0:1, 164) = [75.0000000,0.3776854]
  cl(0:1, 165) = [78.0000000,0.3048898]
  cl(0:1, 166) = [81.0000000,0.2307236]
  cl(0:1, 167) = [84.0000000,0.1538164]
  cl(0:1, 168) = [87.0000000,0.0769082]
  cl(0:1, 169) = [90.0000000,-0.0000003]
  cl(0:1, 170) = [93.0000000,-0.0538358]
  cl(0:1, 171) = [96.0000000,-0.1076713]
  cl(0:1, 172) = [99.0000000,-0.1615067]
  cl(0:1, 173) = [102.0000000,-0.2134229]
  cl(0:1, 174) = [105.0000000,-0.2643793]
  cl(0:1, 175) = [108.0000000,-0.3153358]
  cl(0:1, 176) = [111.0000000,-0.3636150]
  cl(0:1, 177) = [114.0000000,-0.4065398]
  cl(0:1, 178) = [117.0000000,-0.4494644]
  cl(0:1, 179) = [120.0000000,-0.4923891]
  cl(0:1, 180) = [123.0000000,-0.5240471]
  cl(0:1, 181) = [126.0000000,-0.5557050]
  cl(0:1, 182) = [129.0000000,-0.5873627]
  cl(0:1, 183) = [132.0000000,-0.6113621]
  cl(0:1, 184) = [135.0000000,-0.6315323]
  cl(0:1, 185) = [138.0000000,-0.6517017]
  cl(0:1, 186) = [141.0000000,-0.6696729]
  cl(0:1, 187) = [144.0000000,-0.6832474]
  cl(0:1, 188) = [147.0000000,-0.6968215]
  cl(0:1, 189) = [150.0000000,-0.7103953]
  cl(0:1, 190) = [153.0000000,-0.7063721]
  cl(0:1, 191) = [156.0000000,-0.7023489]
  cl(0:1, 192) = [159.0000000,-0.6985102]
  cl(0:1, 193) = [162.0000000,-0.6276283]
  cl(0:1, 194) = [165.0000000,-0.5230233]
  cl(0:1, 195) = [168.0000000,-0.4183494]
  cl(0:1, 196) = [171.0000000,-0.3137420]
  cl(0:1, 197) = [174.0000000,-0.2091612]
  cl(0:1, 198) = [177.0000000,-0.1041009]
  cl(0:1, 199) = [180.0000000,0.0000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl16_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl16 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl16_ge28.pro'
  ENDIF

!print,y,cl16

!_________________________________________________________________________
  
END FUNCTION calc_cl16_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl17_ge28( y ) RESULT( cl17 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl17
  INTEGER :: i

  cl17 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.00000000,0.00000000]
  cl(0:1, 1  ) = [-177.00000000,0.10902617]
  cl(0:1, 2  ) = [-174.00000000,0.21905012]
  cl(0:1, 3  ) = [-171.00000000,0.32857572]
  cl(0:1, 4  ) = [-168.00000000,0.43810691]
  cl(0:1, 5  ) = [-165.00000000,0.54764405]
  cl(0:1, 6  ) = [-162.00000000,0.65718538]
  cl(0:1, 7  ) = [-159.00000000,0.72898656]
  cl(0:1, 8  ) = [-156.00000000,0.72360431]
  cl(0:1, 9  ) = [-153.00000000,0.71776731]
  cl(0:1, 10 ) = [-150.00000000,0.71244531]
  cl(0:1, 11 ) = [-147.00000000,0.69863083]
  cl(0:1, 12 ) = [-144.00000000,0.68481610]
  cl(0:1, 13 ) = [-141.00000000,0.67100086]
  cl(0:1, 14 ) = [-138.00000000,0.65284736]
  cl(0:1, 15 ) = [-135.00000000,0.63252474]
  cl(0:1, 16 ) = [-132.00000000,0.61220133]
  cl(0:1, 17 ) = [-129.00000000,0.58806555]
  cl(0:1, 18 ) = [-126.00000000,0.55630500]
  cl(0:1, 19 ) = [-123.00000000,0.52454433]
  cl(0:1, 20 ) = [-120.00000000,0.49278359]
  cl(0:1, 21 ) = [-117.00000000,0.44979164]
  cl(0:1, 22 ) = [-114.00000000,0.40679964]
  cl(0:1, 23 ) = [-111.00000000,0.36380760]
  cl(0:1, 24 ) = [-108.00000000,0.31548032]
  cl(0:1, 25 ) = [-105.00000000,0.26448537]
  cl(0:1, 26 ) = [-102.00000000,0.21349041]
  cl(0:1, 27 ) = [-99.00000000,0.16154443]
  cl(0:1, 28 ) = [-96.00000000,0.10769639]
  cl(0:1, 29 ) = [-93.00000000,0.05384836]
  cl(0:1, 30 ) = [-90.00000000,0.00000034]
  cl(0:1, 31 ) = [-87.00000000,-0.05384831]
  cl(0:1, 32 ) = [-84.00000000,-0.10769654]
  cl(0:1, 33 ) = [-81.00000000,-0.16154397]
  cl(0:1, 34 ) = [-78.00000000,-0.21349016]
  cl(0:1, 35 ) = [-75.00000000,-0.26448571]
  cl(0:1, 36 ) = [-72.00000000,-0.31548007]
  cl(0:1, 37 ) = [-69.00000000,-0.36380723]
  cl(0:1, 38 ) = [-66.00000000,-0.40679976]
  cl(0:1, 39 ) = [-63.00000000,-0.44979192]
  cl(0:1, 40 ) = [-60.00000000,-0.49278380]
  cl(0:1, 41 ) = [-57.00000000,-0.52454454]
  cl(0:1, 42 ) = [-54.00000000,-0.55630521]
  cl(0:1, 43 ) = [-51.00000000,-0.58806576]
  cl(0:1, 44 ) = [-48.00000000,-0.61391035]
  cl(0:1, 45 ) = [-45.00000000,-0.63679706]
  cl(0:1, 46 ) = [-42.00000000,-0.65455637]
  cl(0:1, 47 ) = [-39.00000000,-0.67138018]
  cl(0:1, 48 ) = [-36.00000000,-0.68560420]
  cl(0:1, 49 ) = [-33.00000000,-0.69881571]
  cl(0:1, 50 ) = [-30.00000000,-0.71244541]
  cl(0:1, 51 ) = [-29.39393939,-0.71556926]
  cl(0:1, 52 ) = [-28.78787879,-0.71869311]
  cl(0:1, 53 ) = [-28.18181818,-0.72181696]
  cl(0:1, 54 ) = [-27.57575758,-0.72533754]
  cl(0:1, 55 ) = [-26.96969697,-0.72902815]
  cl(0:1, 56 ) = [-26.36363636,-0.73271877]
  cl(0:1, 57 ) = [-25.75757576,-0.73676260]
  cl(0:1, 58 ) = [-25.15151515,-0.74133619]
  cl(0:1, 59 ) = [-24.54545455,-0.74590978]
  cl(0:1, 60 ) = [-23.93939394,-0.75062714]
  cl(0:1, 61 ) = [-23.33333333,-0.75663846]
  cl(0:1, 62 ) = [-22.72727273,-0.76264978]
  cl(0:1, 63 ) = [-22.12121212,-0.76866109]
  cl(0:1, 64 ) = [-21.51515152,-0.80723302]
  cl(0:1, 65 ) = [-20.90909091,-0.85006394]
  cl(0:1, 66 ) = [-20.30303030,-0.89481893]
  cl(0:1, 67 ) = [-19.69696970,-0.93930159]
  cl(0:1, 68 ) = [-19.09090909,-0.98365956]
  cl(0:1, 69 ) = [-18.48484848,-1.02835646]
  cl(0:1, 70 ) = [-17.87878788,-1.07312023]
  cl(0:1, 71 ) = [-17.27272727,-1.11789561]
  cl(0:1, 72 ) = [-16.66666667,-1.16291637]
  cl(0:1, 73 ) = [-16.06060606,-1.20810673]
  cl(0:1, 74 ) = [-15.45454545,-1.22780192]
  cl(0:1, 75 ) = [-14.84848485,-1.23148075]
  cl(0:1, 76 ) = [-14.24242424,-1.19557688]
  cl(0:1, 77 ) = [-13.63636364,-1.15320276]
  cl(0:1, 78 ) = [-13.03030303,-1.10624219]
  cl(0:1, 79 ) = [-12.42424242,-1.05404262]
  cl(0:1, 80 ) = [-11.81818182,-1.00136073]
  cl(0:1, 81 ) = [-11.21212121,-0.94858584]
  cl(0:1, 82 ) = [-10.60606061,-0.89366789]
  cl(0:1, 83 ) = [-10.00000000,-0.83806520]
  cl(0:1, 84 ) = [-9.39393939,-0.79541079]
  cl(0:1, 85 ) = [-8.78787879,-0.75911458]
  cl(0:1, 86 ) = [-8.18181818,-0.73990830]
  cl(0:1, 87 ) = [-7.57575758,-0.69551522]
  cl(0:1, 88 ) = [-6.96969697,-0.63897035]
  cl(0:1, 89 ) = [-6.36363636,-0.55305098]
  cl(0:1, 90 ) = [-5.75757576,-0.46785548]
  cl(0:1, 91 ) = [-5.15151515,-0.38376188]
  cl(0:1, 92 ) = [-4.54545455,-0.30085343]
  cl(0:1, 93 ) = [-3.93939394,-0.21831683]
  cl(0:1, 94 ) = [-3.33333333,-0.13580223]
  cl(0:1, 95 ) = [-2.72727273,-0.05524019]
  cl(0:1, 96 ) = [-2.12121212,0.02363969]
  cl(0:1, 97 ) = [-1.51515152,0.10148461]
  cl(0:1, 98 ) = [-0.90909091,0.17946658]
  cl(0:1, 99 ) = [-0.30303030,0.25908762]
  cl(0:1, 100) = [0.30303030,0.33783759]
  cl(0:1, 101) = [0.90909091,0.41578997]
  cl(0:1, 102) = [1.51515152,0.49314790]
  cl(0:1, 103) = [2.12121212,0.56985954]
  cl(0:1, 104) = [2.72727273,0.64430764]
  cl(0:1, 105) = [3.33333333,0.71860094]
  cl(0:1, 106) = [3.93939394,0.79295017]
  cl(0:1, 107) = [4.54545455,0.86675063]
  cl(0:1, 108) = [5.15151515,0.93993688]
  cl(0:1, 109) = [5.75757576,1.01143405]
  cl(0:1, 110) = [6.36363636,1.08182810]
  cl(0:1, 111) = [6.96969697,1.15154798]
  cl(0:1, 112) = [7.57575758,1.21829985]
  cl(0:1, 113) = [8.18181818,1.28284334]
  cl(0:1, 114) = [8.78787879,1.34152951]
  cl(0:1, 115) = [9.39393939,1.39175205]
  cl(0:1, 116) = [10.00000000,1.44072055]
  cl(0:1, 117) = [10.60606061,1.47445007]
  cl(0:1, 118) = [11.21212121,1.50211111]
  cl(0:1, 119) = [11.81818182,1.51849743]
  cl(0:1, 120) = [12.42424242,1.38110105]
  cl(0:1, 121) = [13.03030303,1.17394609]
  cl(0:1, 122) = [13.63636364,1.12719503]
  cl(0:1, 123) = [14.24242424,1.08888195]
  cl(0:1, 124) = [14.84848485,1.06009153]
  cl(0:1, 125) = [15.45454545,1.04088100]
  cl(0:1, 126) = [16.06060606,1.02566184]
  cl(0:1, 127) = [16.66666667,1.01747137]
  cl(0:1, 128) = [17.27272727,1.01393797]
  cl(0:1, 129) = [17.87878788,1.01620978]
  cl(0:1, 130) = [18.48484848,1.03062727]
  cl(0:1, 131) = [19.09090909,1.04771494]
  cl(0:1, 132) = [19.69696970,1.06291878]
  cl(0:1, 133) = [20.30303030,1.07502785]
  cl(0:1, 134) = [20.90909091,1.08382116]
  cl(0:1, 135) = [21.51515152,1.09214909]
  cl(0:1, 136) = [22.12121212,1.09715356]
  cl(0:1, 137) = [22.72727273,1.08886763]
  cl(0:1, 138) = [23.33333333,1.08058169]
  cl(0:1, 139) = [23.93939394,1.07229575]
  cl(0:1, 140) = [24.54545455,1.06558611]
  cl(0:1, 141) = [25.15151515,1.05905162]
  cl(0:1, 142) = [25.75757576,1.05251712]
  cl(0:1, 143) = [26.36363636,1.04673941]
  cl(0:1, 144) = [26.96969697,1.04146631]
  cl(0:1, 145) = [27.57575758,1.03619321]
  cl(0:1, 146) = [28.18181818,1.03116399]
  cl(0:1, 147) = [28.78787879,1.02670377]
  cl(0:1, 148) = [29.39393939,1.02224355]
  cl(0:1, 149) = [30.00000000,1.01778333]
  cl(0:1, 150) = [33.00000000,0.99830667]
  cl(0:1, 151) = [36.00000000,0.97943525]
  cl(0:1, 152) = [39.00000000,0.95911502]
  cl(0:1, 153) = [42.00000000,0.93508110]
  cl(0:1, 154) = [45.00000000,0.90971055]
  cl(0:1, 155) = [48.00000000,0.87701468]
  cl(0:1, 156) = [51.00000000,0.84009334]
  cl(0:1, 157) = [54.00000000,0.79472083]
  cl(0:1, 158) = [57.00000000,0.74934863]
  cl(0:1, 159) = [60.00000000,0.70397659]
  cl(0:1, 160) = [63.00000000,0.64255938]
  cl(0:1, 161) = [66.00000000,0.58114203]
  cl(0:1, 162) = [69.00000000,0.51972468]
  cl(0:1, 163) = [72.00000000,0.45068601]
  cl(0:1, 164) = [75.00000000,0.37783682]
  cl(0:1, 165) = [78.00000000,0.30498621]
  cl(0:1, 166) = [81.00000000,0.23077738]
  cl(0:1, 167) = [84.00000000,0.15385222]
  cl(0:1, 168) = [87.00000000,0.07692611]
  cl(0:1, 169) = [90.00000000,-0.00000034]
  cl(0:1, 170) = [93.00000000,-0.05384836]
  cl(0:1, 171) = [96.00000000,-0.10769639]
  cl(0:1, 172) = [99.00000000,-0.16154443]
  cl(0:1, 173) = [102.00000000,-0.21349041]
  cl(0:1, 174) = [105.00000000,-0.26448537]
  cl(0:1, 175) = [108.00000000,-0.31548032]
  cl(0:1, 176) = [111.00000000,-0.36380760]
  cl(0:1, 177) = [114.00000000,-0.40679964]
  cl(0:1, 178) = [117.00000000,-0.44979164]
  cl(0:1, 179) = [120.00000000,-0.49278359]
  cl(0:1, 180) = [123.00000000,-0.52454433]
  cl(0:1, 181) = [126.00000000,-0.55630500]
  cl(0:1, 182) = [129.00000000,-0.58806555]
  cl(0:1, 183) = [132.00000000,-0.61220133]
  cl(0:1, 184) = [135.00000000,-0.63252474]
  cl(0:1, 185) = [138.00000000,-0.65284736]
  cl(0:1, 186) = [141.00000000,-0.67100086]
  cl(0:1, 187) = [144.00000000,-0.68481610]
  cl(0:1, 188) = [147.00000000,-0.69863083]
  cl(0:1, 189) = [150.00000000,-0.71244531]
  cl(0:1, 190) = [153.00000000,-0.70700953]
  cl(0:1, 191) = [156.00000000,-0.70231920]
  cl(0:1, 192) = [159.00000000,-0.69706761]
  cl(0:1, 193) = [162.00000000,-0.62525248]
  cl(0:1, 194) = [165.00000000,-0.52104347]
  cl(0:1, 195) = [168.00000000,-0.41683506]
  cl(0:1, 196) = [171.00000000,-0.31262644]
  cl(0:1, 197) = [174.00000000,-0.20841743]
  cl(0:1, 198) = [177.00000000,-0.10373429]
  cl(0:1, 199) = [180.00000000,0.00000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl17_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl17 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl17_ge28.pro'
  ENDIF

!print,y,cl17

!_________________________________________________________________________
  
END FUNCTION calc_cl17_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl18_ge28( y ) RESULT( cl18 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl18
  INTEGER :: i

  cl18 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.1087180]
  cl(0:1, 2  ) = [-174.0000000,0.2184308]
  cl(0:1, 3  ) = [-171.0000000,0.3276465]
  cl(0:1, 4  ) = [-168.0000000,0.4368693]
  cl(0:1, 5  ) = [-165.0000000,0.5460994]
  cl(0:1, 6  ) = [-162.0000000,0.6553345]
  cl(0:1, 7  ) = [-159.0000000,0.7280104]
  cl(0:1, 8  ) = [-156.0000000,0.7246443]
  cl(0:1, 9  ) = [-153.0000000,0.7213086]
  cl(0:1, 10 ) = [-150.0000000,0.7200526]
  cl(0:1, 11 ) = [-147.0000000,0.7053450]
  cl(0:1, 12 ) = [-144.0000000,0.6906371]
  cl(0:1, 13 ) = [-141.0000000,0.6759287]
  cl(0:1, 14 ) = [-138.0000000,0.6570985]
  cl(0:1, 15 ) = [-135.0000000,0.6362073]
  cl(0:1, 16 ) = [-132.0000000,0.6153156]
  cl(0:1, 17 ) = [-129.0000000,0.5906737]
  cl(0:1, 18 ) = [-126.0000000,0.5585317]
  cl(0:1, 19 ) = [-123.0000000,0.5263896]
  cl(0:1, 20 ) = [-120.0000000,0.4942475]
  cl(0:1, 21 ) = [-117.0000000,0.4510058]
  cl(0:1, 22 ) = [-114.0000000,0.4077640]
  cl(0:1, 23 ) = [-111.0000000,0.3645223]
  cl(0:1, 24 ) = [-108.0000000,0.3160166]
  cl(0:1, 25 ) = [-105.0000000,0.2648788]
  cl(0:1, 26 ) = [-102.0000000,0.2137410]
  cl(0:1, 27 ) = [-99.0000000,0.1616843]
  cl(0:1, 28 ) = [-96.0000000,0.1077896]
  cl(0:1, 29 ) = [-93.0000000,0.0538950]
  cl(0:1, 30 ) = [-90.0000000,0.0000003]
  cl(0:1, 31 ) = [-87.0000000,-0.0538949]
  cl(0:1, 32 ) = [-84.0000000,-0.1077898]
  cl(0:1, 33 ) = [-81.0000000,-0.1616838]
  cl(0:1, 34 ) = [-78.0000000,-0.2137408]
  cl(0:1, 35 ) = [-75.0000000,-0.2648792]
  cl(0:1, 36 ) = [-72.0000000,-0.3160164]
  cl(0:1, 37 ) = [-69.0000000,-0.3645219]
  cl(0:1, 38 ) = [-66.0000000,-0.4077641]
  cl(0:1, 39 ) = [-63.0000000,-0.4510060]
  cl(0:1, 40 ) = [-60.0000000,-0.4942477]
  cl(0:1, 41 ) = [-57.0000000,-0.5263898]
  cl(0:1, 42 ) = [-54.0000000,-0.5585319]
  cl(0:1, 43 ) = [-51.0000000,-0.5906739]
  cl(0:1, 44 ) = [-48.0000000,-0.6169860]
  cl(0:1, 45 ) = [-45.0000000,-0.6403831]
  cl(0:1, 46 ) = [-42.0000000,-0.6587689]
  cl(0:1, 47 ) = [-39.0000000,-0.6762529]
  cl(0:1, 48 ) = [-36.0000000,-0.6912515]
  cl(0:1, 49 ) = [-33.0000000,-0.7053737]
  cl(0:1, 50 ) = [-30.0000000,-0.7200527]
  cl(0:1, 51 ) = [-29.3939394,-0.7234234]
  cl(0:1, 52 ) = [-28.7878788,-0.7267940]
  cl(0:1, 53 ) = [-28.1818182,-0.7301647]
  cl(0:1, 54 ) = [-27.5757576,-0.7339553]
  cl(0:1, 55 ) = [-26.9696970,-0.7379259]
  cl(0:1, 56 ) = [-26.3636364,-0.7418966]
  cl(0:1, 57 ) = [-25.7575758,-0.7462369]
  cl(0:1, 58 ) = [-25.1515152,-0.7511317]
  cl(0:1, 59 ) = [-24.5454545,-0.7560265]
  cl(0:1, 60 ) = [-23.9393939,-0.7611180]
  cl(0:1, 61 ) = [-23.3333333,-0.7679795]
  cl(0:1, 62 ) = [-22.7272727,-0.7748409]
  cl(0:1, 63 ) = [-22.1212121,-0.7817024]
  cl(0:1, 64 ) = [-21.5151515,-0.8124483]
  cl(0:1, 65 ) = [-20.9090909,-0.8464185]
  cl(0:1, 66 ) = [-20.3030303,-0.8824104]
  cl(0:1, 67 ) = [-19.6969697,-0.9173025]
  cl(0:1, 68 ) = [-19.0909091,-0.9516908]
  cl(0:1, 69 ) = [-18.4848485,-0.9874511]
  cl(0:1, 70 ) = [-17.8787879,-1.0234811]
  cl(0:1, 71 ) = [-17.2727273,-1.0595543]
  cl(0:1, 72 ) = [-16.6666667,-1.0966181]
  cl(0:1, 73 ) = [-16.0606061,-1.1343665]
  cl(0:1, 74 ) = [-15.4545455,-1.1539344]
  cl(0:1, 75 ) = [-14.8484848,-1.1617965]
  cl(0:1, 76 ) = [-14.2424242,-1.1405068]
  cl(0:1, 77 ) = [-13.6363636,-1.1131898]
  cl(0:1, 78 ) = [-13.0303030,-1.0807674]
  cl(0:1, 79 ) = [-12.4242424,-1.0372730]
  cl(0:1, 80 ) = [-11.8181818,-0.9914402]
  cl(0:1, 81 ) = [-11.2121212,-0.9430455]
  cl(0:1, 82 ) = [-10.6060606,-0.8913571]
  cl(0:1, 83 ) = [-10.0000000,-0.8386409]
  cl(0:1, 84 ) = [-9.3939394,-0.7971548]
  cl(0:1, 85 ) = [-8.7878788,-0.7577002]
  cl(0:1, 86 ) = [-8.1818182,-0.7296158]
  cl(0:1, 87 ) = [-7.5757576,-0.6777690]
  cl(0:1, 88 ) = [-6.9696970,-0.6152536]
  cl(0:1, 89 ) = [-6.3636364,-0.5292229]
  cl(0:1, 90 ) = [-5.7575758,-0.4437142]
  cl(0:1, 91 ) = [-5.1515152,-0.3590212]
  cl(0:1, 92 ) = [-4.5454545,-0.2756260]
  cl(0:1, 93 ) = [-3.9393939,-0.1925874]
  cl(0:1, 94 ) = [-3.3333333,-0.1092925]
  cl(0:1, 95 ) = [-2.7272727,-0.0277773]
  cl(0:1, 96 ) = [-2.1212121,0.0520912]
  cl(0:1, 97 ) = [-1.5151515,0.1305864]
  cl(0:1, 98 ) = [-0.9090909,0.2088405]
  cl(0:1, 99 ) = [-0.3030303,0.2875785]
  cl(0:1, 100) = [0.3030303,0.3656646]
  cl(0:1, 101) = [0.9090909,0.4432050]
  cl(0:1, 102) = [1.5151515,0.5202810]
  cl(0:1, 103) = [2.1212121,0.5968115]
  cl(0:1, 104) = [2.7272727,0.6714349]
  cl(0:1, 105) = [3.3333333,0.7456568]
  cl(0:1, 106) = [3.9393939,0.8198578]
  cl(0:1, 107) = [4.5454545,0.8936279]
  cl(0:1, 108) = [5.1515152,0.9667888]
  cl(0:1, 109) = [5.7575758,1.0382164]
  cl(0:1, 110) = [6.3636364,1.1079583]
  cl(0:1, 111) = [6.9696970,1.1769442]
  cl(0:1, 112) = [7.5757576,1.2392153]
  cl(0:1, 113) = [8.1818182,1.2970601]
  cl(0:1, 114) = [8.7878788,1.3455405]
  cl(0:1, 115) = [9.3939394,1.3761090]
  cl(0:1, 116) = [10.0000000,1.3983627]
  cl(0:1, 117) = [10.6060606,1.3928731]
  cl(0:1, 118) = [11.2121212,1.3900460]
  cl(0:1, 119) = [11.8181818,1.3921641]
  cl(0:1, 120) = [12.4242424,1.2937030]
  cl(0:1, 121) = [13.0303030,1.1438458]
  cl(0:1, 122) = [13.6363636,1.1178292]
  cl(0:1, 123) = [14.2424242,1.0928995]
  cl(0:1, 124) = [14.8484848,1.0710235]
  cl(0:1, 125) = [15.4545455,1.0569616]
  cl(0:1, 126) = [16.0606061,1.0460430]
  cl(0:1, 127) = [16.6666667,1.0393231]
  cl(0:1, 128) = [17.2727273,1.0374793]
  cl(0:1, 129) = [17.8787879,1.0420522]
  cl(0:1, 130) = [18.4848485,1.0549034]
  cl(0:1, 131) = [19.0909091,1.0694655]
  cl(0:1, 132) = [19.6969697,1.0827609]
  cl(0:1, 133) = [20.3030303,1.0940337]
  cl(0:1, 134) = [20.9090909,1.1023924]
  cl(0:1, 135) = [21.5151515,1.1088725]
  cl(0:1, 136) = [22.1212121,1.1123759]
  cl(0:1, 137) = [22.7272727,1.1039748]
  cl(0:1, 138) = [23.3333333,1.0955738]
  cl(0:1, 139) = [23.9393939,1.0871727]
  cl(0:1, 140) = [24.5454545,1.0800388]
  cl(0:1, 141) = [25.1515152,1.0730457]
  cl(0:1, 142) = [25.7575758,1.0660526]
  cl(0:1, 143) = [26.3636364,1.0598515]
  cl(0:1, 144) = [26.9696970,1.0541787]
  cl(0:1, 145) = [27.5757576,1.0485058]
  cl(0:1, 146) = [28.1818182,1.0430907]
  cl(0:1, 147) = [28.7878788,1.0382773]
  cl(0:1, 148) = [29.3939394,1.0334639]
  cl(0:1, 149) = [30.0000000,1.0286505]
  cl(0:1, 150) = [33.0000000,1.0076752]
  cl(0:1, 151) = [36.0000000,0.9875021]
  cl(0:1, 152) = [39.0000000,0.9660759]
  cl(0:1, 153) = [42.0000000,0.9410989]
  cl(0:1, 154) = [45.0000000,0.9148336]
  cl(0:1, 155) = [48.0000000,0.8814086]
  cl(0:1, 156) = [51.0000000,0.8438196]
  cl(0:1, 157) = [54.0000000,0.7979022]
  cl(0:1, 158) = [57.0000000,0.7519850]
  cl(0:1, 159) = [60.0000000,0.7060680]
  cl(0:1, 160) = [63.0000000,0.6442939]
  cl(0:1, 161) = [66.0000000,0.5825198]
  cl(0:1, 162) = [69.0000000,0.5207456]
  cl(0:1, 163) = [72.0000000,0.4514520]
  cl(0:1, 164) = [75.0000000,0.3783987]
  cl(0:1, 165) = [78.0000000,0.3053441]
  cl(0:1, 166) = [81.0000000,0.2309770]
  cl(0:1, 167) = [84.0000000,0.1539852]
  cl(0:1, 168) = [87.0000000,0.0769926]
  cl(0:1, 169) = [90.0000000,-0.0000003]
  cl(0:1, 170) = [93.0000000,-0.0538950]
  cl(0:1, 171) = [96.0000000,-0.1077896]
  cl(0:1, 172) = [99.0000000,-0.1616843]
  cl(0:1, 173) = [102.0000000,-0.2137410]
  cl(0:1, 174) = [105.0000000,-0.2648788]
  cl(0:1, 175) = [108.0000000,-0.3160166]
  cl(0:1, 176) = [111.0000000,-0.3645223]
  cl(0:1, 177) = [114.0000000,-0.4077640]
  cl(0:1, 178) = [117.0000000,-0.4510058]
  cl(0:1, 179) = [120.0000000,-0.4942475]
  cl(0:1, 180) = [123.0000000,-0.5263896]
  cl(0:1, 181) = [126.0000000,-0.5585317]
  cl(0:1, 182) = [129.0000000,-0.5906737]
  cl(0:1, 183) = [132.0000000,-0.6153156]
  cl(0:1, 184) = [135.0000000,-0.6362073]
  cl(0:1, 185) = [138.0000000,-0.6570985]
  cl(0:1, 186) = [141.0000000,-0.6759287]
  cl(0:1, 187) = [144.0000000,-0.6906371]
  cl(0:1, 188) = [147.0000000,-0.7053450]
  cl(0:1, 189) = [150.0000000,-0.7200526]
  cl(0:1, 190) = [153.0000000,-0.7101816]
  cl(0:1, 191) = [156.0000000,-0.7033199]
  cl(0:1, 192) = [159.0000000,-0.6960870]
  cl(0:1, 193) = [162.0000000,-0.6233944]
  cl(0:1, 194) = [165.0000000,-0.5194950]
  cl(0:1, 195) = [168.0000000,-0.4155962]
  cl(0:1, 196) = [171.0000000,-0.3116972]
  cl(0:1, 197) = [174.0000000,-0.2077978]
  cl(0:1, 198) = [177.0000000,-0.1034260]
  cl(0:1, 199) = [180.0000000,0.0000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl18_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl18 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl18_ge28.pro'
  ENDIF

!print,y,cl18

!_________________________________________________________________________
  
END FUNCTION calc_cl18_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl19_ge28( y ) RESULT( cl19 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl19
  INTEGER :: i

  cl19 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.1085042]
  cl(0:1, 2  ) = [-174.0000000,0.2180016]
  cl(0:1, 3  ) = [-171.0000000,0.3270026]
  cl(0:1, 4  ) = [-168.0000000,0.4360073]
  cl(0:1, 5  ) = [-165.0000000,0.5450159]
  cl(0:1, 6  ) = [-162.0000000,0.6540279]
  cl(0:1, 7  ) = [-159.0000000,0.7273352]
  cl(0:1, 8  ) = [-156.0000000,0.7264739]
  cl(0:1, 9  ) = [-153.0000000,0.7264018]
  cl(0:1, 10 ) = [-150.0000000,0.7294149]
  cl(0:1, 11 ) = [-147.0000000,0.7136081]
  cl(0:1, 12 ) = [-144.0000000,0.6978010]
  cl(0:1, 13 ) = [-141.0000000,0.6819934]
  cl(0:1, 14 ) = [-138.0000000,0.6623303]
  cl(0:1, 15 ) = [-135.0000000,0.6407395]
  cl(0:1, 16 ) = [-132.0000000,0.6191483]
  cl(0:1, 17 ) = [-129.0000000,0.5938836]
  cl(0:1, 18 ) = [-126.0000000,0.5612720]
  cl(0:1, 19 ) = [-123.0000000,0.5286606]
  cl(0:1, 20 ) = [-120.0000000,0.4960492]
  cl(0:1, 21 ) = [-117.0000000,0.4525000]
  cl(0:1, 22 ) = [-114.0000000,0.4089508]
  cl(0:1, 23 ) = [-111.0000000,0.3654019]
  cl(0:1, 24 ) = [-108.0000000,0.3166766]
  cl(0:1, 25 ) = [-105.0000000,0.2653631]
  cl(0:1, 26 ) = [-102.0000000,0.2140495]
  cl(0:1, 27 ) = [-99.0000000,0.1618564]
  cl(0:1, 28 ) = [-96.0000000,0.1079044]
  cl(0:1, 29 ) = [-93.0000000,0.0539524]
  cl(0:1, 30 ) = [-90.0000000,0.0000003]
  cl(0:1, 31 ) = [-87.0000000,-0.0539523]
  cl(0:1, 32 ) = [-84.0000000,-0.1079045]
  cl(0:1, 33 ) = [-81.0000000,-0.1618560]
  cl(0:1, 34 ) = [-78.0000000,-0.2140492]
  cl(0:1, 35 ) = [-75.0000000,-0.2653634]
  cl(0:1, 36 ) = [-72.0000000,-0.3166764]
  cl(0:1, 37 ) = [-69.0000000,-0.3654015]
  cl(0:1, 38 ) = [-66.0000000,-0.4089510]
  cl(0:1, 39 ) = [-63.0000000,-0.4525003]
  cl(0:1, 40 ) = [-60.0000000,-0.4960494]
  cl(0:1, 41 ) = [-57.0000000,-0.5286608]
  cl(0:1, 42 ) = [-54.0000000,-0.5612722]
  cl(0:1, 43 ) = [-51.0000000,-0.5938838]
  cl(0:1, 44 ) = [-48.0000000,-0.6207712]
  cl(0:1, 45 ) = [-45.0000000,-0.6447965]
  cl(0:1, 46 ) = [-42.0000000,-0.6639532]
  cl(0:1, 47 ) = [-39.0000000,-0.6822497]
  cl(0:1, 48 ) = [-36.0000000,-0.6982016]
  cl(0:1, 49 ) = [-33.0000000,-0.7134446]
  cl(0:1, 50 ) = [-30.0000000,-0.7294150]
  cl(0:1, 51 ) = [-29.3939394,-0.7330894]
  cl(0:1, 52 ) = [-28.7878788,-0.7367638]
  cl(0:1, 53 ) = [-28.1818182,-0.7404382]
  cl(0:1, 54 ) = [-27.5757576,-0.7445612]
  cl(0:1, 55 ) = [-26.9696970,-0.7488764]
  cl(0:1, 56 ) = [-26.3636364,-0.7531917]
  cl(0:1, 57 ) = [-25.7575758,-0.7578969]
  cl(0:1, 58 ) = [-25.1515152,-0.7631871]
  cl(0:1, 59 ) = [-24.5454545,-0.7684772]
  cl(0:1, 60 ) = [-23.9393939,-0.7740291]
  cl(0:1, 61 ) = [-23.3333333,-0.7819368]
  cl(0:1, 62 ) = [-22.7272727,-0.7898446]
  cl(0:1, 63 ) = [-22.1212121,-0.7977523]
  cl(0:1, 64 ) = [-21.5151515,-0.8172456]
  cl(0:1, 65 ) = [-20.9090909,-0.8407158]
  cl(0:1, 66 ) = [-20.3030303,-0.8648931]
  cl(0:1, 67 ) = [-19.6969697,-0.8874389]
  cl(0:1, 68 ) = [-19.0909091,-0.9092374]
  cl(0:1, 69 ) = [-18.4848485,-0.9330718]
  cl(0:1, 70 ) = [-17.8787879,-0.9573062]
  cl(0:1, 71 ) = [-17.2727273,-0.9816040]
  cl(0:1, 72 ) = [-16.6666667,-1.0073713]
  cl(0:1, 73 ) = [-16.0606061,-1.0341542]
  cl(0:1, 74 ) = [-15.4545455,-1.0521393]
  cl(0:1, 75 ) = [-14.8484848,-1.0641143]
  cl(0:1, 76 ) = [-14.2424242,-1.0608592]
  cl(0:1, 77 ) = [-13.6363636,-1.0529718]
  cl(0:1, 78 ) = [-13.0303030,-1.0403862]
  cl(0:1, 79 ) = [-12.4242424,-1.0096331]
  cl(0:1, 80 ) = [-11.8181818,-0.9743930]
  cl(0:1, 81 ) = [-11.2121212,-0.9331840]
  cl(0:1, 82 ) = [-10.6060606,-0.8870952]
  cl(0:1, 83 ) = [-10.0000000,-0.8390303]
  cl(0:1, 84 ) = [-9.3939394,-0.7983553]
  cl(0:1, 85 ) = [-8.7878788,-0.7550757]
  cl(0:1, 86 ) = [-8.1818182,-0.7127604]
  cl(0:1, 87 ) = [-7.5757576,-0.6502127]
  cl(0:1, 88 ) = [-6.9696970,-0.5793162]
  cl(0:1, 89 ) = [-6.3636364,-0.4933325]
  cl(0:1, 90 ) = [-5.7575758,-0.4076252]
  cl(0:1, 91 ) = [-5.1515152,-0.3223757]
  cl(0:1, 92 ) = [-4.5454545,-0.2385698]
  cl(0:1, 93 ) = [-3.9393939,-0.1552788]
  cl(0:1, 94 ) = [-3.3333333,-0.0727589]
  cl(0:1, 95 ) = [-2.7272727,0.0087555]
  cl(0:1, 96 ) = [-2.1212121,0.0890952]
  cl(0:1, 97 ) = [-1.5151515,0.1680209]
  cl(0:1, 98 ) = [-0.9090909,0.2462846]
  cl(0:1, 99 ) = [-0.3030303,0.3235269]
  cl(0:1, 100) = [0.3030303,0.4005687]
  cl(0:1, 101) = [0.9090909,0.4775324]
  cl(0:1, 102) = [1.5151515,0.5543110]
  cl(0:1, 103) = [2.1212121,0.6307486]
  cl(0:1, 104) = [2.7272727,0.7059672]
  cl(0:1, 105) = [3.3333333,0.7804116]
  cl(0:1, 106) = [3.9393939,0.8546349]
  cl(0:1, 107) = [4.5454545,0.9283681]
  cl(0:1, 108) = [5.1515152,1.0014569]
  cl(0:1, 109) = [5.7575758,1.0727073]
  cl(0:1, 110) = [6.3636364,1.1412772]
  cl(0:1, 111) = [6.9696970,1.2086494]
  cl(0:1, 112) = [7.5757576,1.2644344]
  cl(0:1, 113) = [8.1818182,1.3132771]
  cl(0:1, 114) = [8.7878788,1.3493168]
  cl(0:1, 115) = [9.3939394,1.3568570]
  cl(0:1, 116) = [10.0000000,1.3462327]
  cl(0:1, 117) = [10.6060606,1.2924763]
  cl(0:1, 118) = [11.2121212,1.2521272]
  cl(0:1, 119) = [11.8181818,1.2366853]
  cl(0:1, 120) = [12.4242424,1.1861419]
  cl(0:1, 121) = [13.0303030,1.1159019]
  cl(0:1, 122) = [13.6363636,1.1107064]
  cl(0:1, 123) = [14.2424242,1.1001643]
  cl(0:1, 124) = [14.8484848,1.0893239]
  cl(0:1, 125) = [15.4545455,1.0826584]
  cl(0:1, 126) = [16.0606061,1.0775035]
  cl(0:1, 127) = [16.6666667,1.0724520]
  cl(0:1, 128) = [17.2727273,1.0722411]
  cl(0:1, 129) = [17.8787879,1.0786245]
  cl(0:1, 130) = [18.4848485,1.0886126]
  cl(0:1, 131) = [19.0909091,1.0991891]
  cl(0:1, 132) = [19.6969697,1.1091281]
  cl(0:1, 133) = [20.3030303,1.1182552]
  cl(0:1, 134) = [20.9090909,1.1252480]
  cl(0:1, 135) = [21.5151515,1.1294541]
  cl(0:1, 136) = [22.1212121,1.1311100]
  cl(0:1, 137) = [22.7272727,1.1225673]
  cl(0:1, 138) = [23.3333333,1.1140246]
  cl(0:1, 139) = [23.9393939,1.1054819]
  cl(0:1, 140) = [24.5454545,1.0978258]
  cl(0:1, 141) = [25.1515152,1.0902682]
  cl(0:1, 142) = [25.7575758,1.0827107]
  cl(0:1, 143) = [26.3636364,1.0759887]
  cl(0:1, 144) = [26.9696970,1.0698238]
  cl(0:1, 145) = [27.5757576,1.0636589]
  cl(0:1, 146) = [28.1818182,1.0577690]
  cl(0:1, 147) = [28.7878788,1.0525209]
  cl(0:1, 148) = [29.3939394,1.0472728]
  cl(0:1, 149) = [30.0000000,1.0420247]
  cl(0:1, 150) = [33.0000000,1.0192051]
  cl(0:1, 151) = [36.0000000,0.9974300]
  cl(0:1, 152) = [39.0000000,0.9746426]
  cl(0:1, 153) = [42.0000000,0.9485049]
  cl(0:1, 154) = [45.0000000,0.9211385]
  cl(0:1, 155) = [48.0000000,0.8868163]
  cl(0:1, 156) = [51.0000000,0.8484055]
  cl(0:1, 157) = [54.0000000,0.8018176]
  cl(0:1, 158) = [57.0000000,0.7552297]
  cl(0:1, 159) = [60.0000000,0.7086418]
  cl(0:1, 160) = [63.0000000,0.6464287]
  cl(0:1, 161) = [66.0000000,0.5842154]
  cl(0:1, 162) = [69.0000000,0.5220021]
  cl(0:1, 163) = [72.0000000,0.4523947]
  cl(0:1, 164) = [75.0000000,0.3790903]
  cl(0:1, 165) = [78.0000000,0.3057845]
  cl(0:1, 166) = [81.0000000,0.2312226]
  cl(0:1, 167) = [84.0000000,0.1541488]
  cl(0:1, 168) = [87.0000000,0.0770744]
  cl(0:1, 169) = [90.0000000,-0.0000003]
  cl(0:1, 170) = [93.0000000,-0.0539524]
  cl(0:1, 171) = [96.0000000,-0.1079044]
  cl(0:1, 172) = [99.0000000,-0.1618564]
  cl(0:1, 173) = [102.0000000,-0.2140495]
  cl(0:1, 174) = [105.0000000,-0.2653631]
  cl(0:1, 175) = [108.0000000,-0.3166766]
  cl(0:1, 176) = [111.0000000,-0.3654019]
  cl(0:1, 177) = [114.0000000,-0.4089508]
  cl(0:1, 178) = [117.0000000,-0.4525000]
  cl(0:1, 179) = [120.0000000,-0.4960492]
  cl(0:1, 180) = [123.0000000,-0.5286606]
  cl(0:1, 181) = [126.0000000,-0.5612720]
  cl(0:1, 182) = [129.0000000,-0.5938836]
  cl(0:1, 183) = [132.0000000,-0.6191483]
  cl(0:1, 184) = [135.0000000,-0.6407395]
  cl(0:1, 185) = [138.0000000,-0.6623303]
  cl(0:1, 186) = [141.0000000,-0.6819934]
  cl(0:1, 187) = [144.0000000,-0.6978010]
  cl(0:1, 188) = [147.0000000,-0.7136081]
  cl(0:1, 189) = [150.0000000,-0.7294149]
  cl(0:1, 190) = [153.0000000,-0.7150371]
  cl(0:1, 191) = [156.0000000,-0.7051233]
  cl(0:1, 192) = [159.0000000,-0.6954147]
  cl(0:1, 193) = [162.0000000,-0.6220983]
  cl(0:1, 194) = [165.0000000,-0.5184149]
  cl(0:1, 195) = [168.0000000,-0.4147321]
  cl(0:1, 196) = [171.0000000,-0.3110491]
  cl(0:1, 197) = [174.0000000,-0.2073657]
  cl(0:1, 198) = [177.0000000,-0.1032105]
  cl(0:1, 199) = [180.0000000,0.0000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl19_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl19 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl19_ge28.pro'
  ENDIF

!print,y,cl19

!_________________________________________________________________________
  
END FUNCTION calc_cl19_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl20_ge28( y ) RESULT( cl20 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl20
  INTEGER :: i

  cl20 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.1084038]
  cl(0:1, 2  ) = [-174.0000000,0.2178005]
  cl(0:1, 3  ) = [-171.0000000,0.3267009]
  cl(0:1, 4  ) = [-168.0000000,0.4356016]
  cl(0:1, 5  ) = [-165.0000000,0.5445027]
  cl(0:1, 6  ) = [-162.0000000,0.6534055]
  cl(0:1, 7  ) = [-159.0000000,0.7270193]
  cl(0:1, 8  ) = [-156.0000000,0.7287286]
  cl(0:1, 9  ) = [-153.0000000,0.7311423]
  cl(0:1, 10 ) = [-150.0000000,0.7355197]
  cl(0:1, 11 ) = [-147.0000000,0.7189962]
  cl(0:1, 12 ) = [-144.0000000,0.7024723]
  cl(0:1, 13 ) = [-141.0000000,0.6859480]
  cl(0:1, 14 ) = [-138.0000000,0.6657418]
  cl(0:1, 15 ) = [-135.0000000,0.6436948]
  cl(0:1, 16 ) = [-132.0000000,0.6216475]
  cl(0:1, 17 ) = [-129.0000000,0.5959766]
  cl(0:1, 18 ) = [-126.0000000,0.5630589]
  cl(0:1, 19 ) = [-123.0000000,0.5301414]
  cl(0:1, 20 ) = [-120.0000000,0.4972240]
  cl(0:1, 21 ) = [-117.0000000,0.4534743]
  cl(0:1, 22 ) = [-114.0000000,0.4097247]
  cl(0:1, 23 ) = [-111.0000000,0.3659755]
  cl(0:1, 24 ) = [-108.0000000,0.3171070]
  cl(0:1, 25 ) = [-105.0000000,0.2656788]
  cl(0:1, 26 ) = [-102.0000000,0.2142506]
  cl(0:1, 27 ) = [-99.0000000,0.1619687]
  cl(0:1, 28 ) = [-96.0000000,0.1079792]
  cl(0:1, 29 ) = [-93.0000000,0.0539898]
  cl(0:1, 30 ) = [-90.0000000,0.0000003]
  cl(0:1, 31 ) = [-87.0000000,-0.0539897]
  cl(0:1, 32 ) = [-84.0000000,-0.1079794]
  cl(0:1, 33 ) = [-81.0000000,-0.1619682]
  cl(0:1, 34 ) = [-78.0000000,-0.2142504]
  cl(0:1, 35 ) = [-75.0000000,-0.2656791]
  cl(0:1, 36 ) = [-72.0000000,-0.3171067]
  cl(0:1, 37 ) = [-69.0000000,-0.3659751]
  cl(0:1, 38 ) = [-66.0000000,-0.4097248]
  cl(0:1, 39 ) = [-63.0000000,-0.4534746]
  cl(0:1, 40 ) = [-60.0000000,-0.4972242]
  cl(0:1, 41 ) = [-57.0000000,-0.5301416]
  cl(0:1, 42 ) = [-54.0000000,-0.5630591]
  cl(0:1, 43 ) = [-51.0000000,-0.5959769]
  cl(0:1, 44 ) = [-48.0000000,-0.6232393]
  cl(0:1, 45 ) = [-45.0000000,-0.6476742]
  cl(0:1, 46 ) = [-42.0000000,-0.6673337]
  cl(0:1, 47 ) = [-39.0000000,-0.6861600]
  cl(0:1, 48 ) = [-36.0000000,-0.7027335]
  cl(0:1, 49 ) = [-33.0000000,-0.7187073]
  cl(0:1, 50 ) = [-30.0000000,-0.7355198]
  cl(0:1, 51 ) = [-29.3939394,-0.7393923]
  cl(0:1, 52 ) = [-28.7878788,-0.7432647]
  cl(0:1, 53 ) = [-28.1818182,-0.7471372]
  cl(0:1, 54 ) = [-27.5757576,-0.7514769]
  cl(0:1, 55 ) = [-26.9696970,-0.7560169]
  cl(0:1, 56 ) = [-26.3636364,-0.7605568]
  cl(0:1, 57 ) = [-25.7575758,-0.7655000]
  cl(0:1, 58 ) = [-25.1515152,-0.7710479]
  cl(0:1, 59 ) = [-24.5454545,-0.7765958]
  cl(0:1, 60 ) = [-23.9393939,-0.7824480]
  cl(0:1, 61 ) = [-23.3333333,-0.7910379]
  cl(0:1, 62 ) = [-22.7272727,-0.7996279]
  cl(0:1, 63 ) = [-22.1212121,-0.8082179]
  cl(0:1, 64 ) = [-21.5151515,-0.8199905]
  cl(0:1, 65 ) = [-20.9090909,-0.8346480]
  cl(0:1, 66 ) = [-20.3030303,-0.8491325]
  cl(0:1, 67 ) = [-19.6969697,-0.8625787]
  cl(0:1, 68 ) = [-19.0909091,-0.8755493]
  cl(0:1, 69 ) = [-18.4848485,-0.8898155]
  cl(0:1, 70 ) = [-17.8787879,-0.9043364]
  cl(0:1, 71 ) = [-17.2727273,-0.9188974]
  cl(0:1, 72 ) = [-16.6666667,-0.9343937]
  cl(0:1, 73 ) = [-16.0606061,-0.9505366]
  cl(0:1, 74 ) = [-15.4545455,-0.9647348]
  cl(0:1, 75 ) = [-14.8484848,-0.9773919]
  cl(0:1, 76 ) = [-14.2424242,-0.9859912]
  cl(0:1, 77 ) = [-13.6363636,-0.9925088]
  cl(0:1, 78 ) = [-13.0303030,-0.9966145]
  cl(0:1, 79 ) = [-12.4242424,-0.9780861]
  cl(0:1, 80 ) = [-11.8181818,-0.9539321]
  cl(0:1, 81 ) = [-11.2121212,-0.9208761]
  cl(0:1, 82 ) = [-10.6060606,-0.8815771]
  cl(0:1, 83 ) = [-10.0000000,-0.8392088]
  cl(0:1, 84 ) = [-9.3939394,-0.7989144]
  cl(0:1, 85 ) = [-8.7878788,-0.7516572]
  cl(0:1, 86 ) = [-8.1818182,-0.6936796]
  cl(0:1, 87 ) = [-7.5757576,-0.6212032]
  cl(0:1, 88 ) = [-6.9696970,-0.5428461]
  cl(0:1, 89 ) = [-6.3636364,-0.4572487]
  cl(0:1, 90 ) = [-5.7575758,-0.3717728]
  cl(0:1, 91 ) = [-5.1515152,-0.2865103]
  cl(0:1, 92 ) = [-4.5454545,-0.2028005]
  cl(0:1, 93 ) = [-3.9393939,-0.1197792]
  cl(0:1, 94 ) = [-3.3333333,-0.0384842]
  cl(0:1, 95 ) = [-2.7272727,0.0424696]
  cl(0:1, 96 ) = [-2.1212121,0.1226560]
  cl(0:1, 97 ) = [-1.5151515,0.2015137]
  cl(0:1, 98 ) = [-0.9090909,0.2794237]
  cl(0:1, 99 ) = [-0.3030303,0.3553132]
  cl(0:1, 100) = [0.3030303,0.4313486]
  cl(0:1, 101) = [0.9090909,0.5076415]
  cl(0:1, 102) = [1.5151515,0.5839481]
  cl(0:1, 103) = [2.1212121,0.6600759]
  cl(0:1, 104) = [2.7272727,0.7355276]
  cl(0:1, 105) = [3.3333333,0.8099802]
  cl(0:1, 106) = [3.9393939,0.8840453]
  cl(0:1, 107) = [4.5454545,0.9575335]
  cl(0:1, 108) = [5.1515152,1.0303584]
  cl(0:1, 109) = [5.7575758,1.1013138]
  cl(0:1, 110) = [6.3636364,1.1689107]
  cl(0:1, 111) = [6.9696970,1.2346526]
  cl(0:1, 112) = [7.5757576,1.2857574]
  cl(0:1, 113) = [8.1818182,1.3282285]
  cl(0:1, 114) = [8.7878788,1.3540826]
  cl(0:1, 115) = [9.3939394,1.3443035]
  cl(0:1, 116) = [10.0000000,1.3122407]
  cl(0:1, 117) = [10.6060606,1.2270113]
  cl(0:1, 118) = [11.2121212,1.1621956]
  cl(0:1, 119) = [11.8181818,1.1353036]
  cl(0:1, 120) = [12.4242424,1.1160055]
  cl(0:1, 121) = [13.0303030,1.0998329]
  cl(0:1, 122) = [13.6363636,1.1071034]
  cl(0:1, 123) = [14.2424242,1.1093836]
  cl(0:1, 124) = [14.8484848,1.1106181]
  cl(0:1, 125) = [15.4545455,1.1108227]
  cl(0:1, 126) = [16.0606061,1.1103361]
  cl(0:1, 127) = [16.6666667,1.1060993]
  cl(0:1, 128) = [17.2727273,1.1060907]
  cl(0:1, 129) = [17.8787879,1.1116815]
  cl(0:1, 130) = [18.4848485,1.1179956]
  cl(0:1, 131) = [19.0909091,1.1242787]
  cl(0:1, 132) = [19.6969697,1.1300831]
  cl(0:1, 133) = [20.3030303,1.1356545]
  cl(0:1, 134) = [20.9090909,1.1401513]
  cl(0:1, 135) = [21.5151515,1.1428746]
  cl(0:1, 136) = [22.1212121,1.1433258]
  cl(0:1, 137) = [22.7272727,1.1346907]
  cl(0:1, 138) = [23.3333333,1.1260557]
  cl(0:1, 139) = [23.9393939,1.1174206]
  cl(0:1, 140) = [24.5454545,1.1094240]
  cl(0:1, 141) = [25.1515152,1.1014984]
  cl(0:1, 142) = [25.7575758,1.0935728]
  cl(0:1, 143) = [26.3636364,1.0865110]
  cl(0:1, 144) = [26.9696970,1.0800253]
  cl(0:1, 145) = [27.5757576,1.0735396]
  cl(0:1, 146) = [28.1818182,1.0673402]
  cl(0:1, 147) = [28.7878788,1.0618086]
  cl(0:1, 148) = [29.3939394,1.0562771]
  cl(0:1, 149) = [30.0000000,1.0507455]
  cl(0:1, 150) = [33.0000000,1.0267233]
  cl(0:1, 151) = [36.0000000,1.0039036]
  cl(0:1, 152) = [39.0000000,0.9802287]
  cl(0:1, 153) = [42.0000000,0.9533341]
  cl(0:1, 154) = [45.0000000,0.9252497]
  cl(0:1, 155) = [48.0000000,0.8903424]
  cl(0:1, 156) = [51.0000000,0.8513958]
  cl(0:1, 157) = [54.0000000,0.8043706]
  cl(0:1, 158) = [57.0000000,0.7573454]
  cl(0:1, 159) = [60.0000000,0.7103201]
  cl(0:1, 160) = [63.0000000,0.6478207]
  cl(0:1, 161) = [66.0000000,0.5853210]
  cl(0:1, 162) = [69.0000000,0.5228214]
  cl(0:1, 163) = [72.0000000,0.4530094]
  cl(0:1, 164) = [75.0000000,0.3795412]
  cl(0:1, 165) = [78.0000000,0.3060716]
  cl(0:1, 166) = [81.0000000,0.2313828]
  cl(0:1, 167) = [84.0000000,0.1542555]
  cl(0:1, 168) = [87.0000000,0.0771277]
  cl(0:1, 169) = [90.0000000,-0.0000003]
  cl(0:1, 170) = [93.0000000,-0.0539898]
  cl(0:1, 171) = [96.0000000,-0.1079792]
  cl(0:1, 172) = [99.0000000,-0.1619687]
  cl(0:1, 173) = [102.0000000,-0.2142506]
  cl(0:1, 174) = [105.0000000,-0.2656788]
  cl(0:1, 175) = [108.0000000,-0.3171070]
  cl(0:1, 176) = [111.0000000,-0.3659755]
  cl(0:1, 177) = [114.0000000,-0.4097247]
  cl(0:1, 178) = [117.0000000,-0.4534743]
  cl(0:1, 179) = [120.0000000,-0.4972240]
  cl(0:1, 180) = [123.0000000,-0.5301414]
  cl(0:1, 181) = [126.0000000,-0.5630589]
  cl(0:1, 182) = [129.0000000,-0.5959766]
  cl(0:1, 183) = [132.0000000,-0.6216475]
  cl(0:1, 184) = [135.0000000,-0.6436948]
  cl(0:1, 185) = [138.0000000,-0.6657418]
  cl(0:1, 186) = [141.0000000,-0.6859480]
  cl(0:1, 187) = [144.0000000,-0.7024723]
  cl(0:1, 188) = [147.0000000,-0.7189962]
  cl(0:1, 189) = [150.0000000,-0.7355197]
  cl(0:1, 190) = [153.0000000,-0.7200413]
  cl(0:1, 191) = [156.0000000,-0.7074040]
  cl(0:1, 192) = [159.0000000,-0.6951027]
  cl(0:1, 193) = [162.0000000,-0.6214875]
  cl(0:1, 194) = [165.0000000,-0.5179059]
  cl(0:1, 195) = [168.0000000,-0.4143248]
  cl(0:1, 196) = [171.0000000,-0.3107436]
  cl(0:1, 197) = [174.0000000,-0.2071620]
  cl(0:1, 198) = [177.0000000,-0.1031088]
  cl(0:1, 199) = [180.0000000,0.0000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl20_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl20 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl20_ge28.pro'
  ENDIF

!print,y,cl20

!_________________________________________________________________________
  
END FUNCTION calc_cl20_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl21_ge28( y ) RESULT( cl21 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl21
  INTEGER :: i

  cl21 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.1086898]
  cl(0:1, 2  ) = [-174.0000000,0.2183752]
  cl(0:1, 3  ) = [-171.0000000,0.3275630]
  cl(0:1, 4  ) = [-168.0000000,0.4364672]
  cl(0:1, 5  ) = [-165.0000000,0.5452295]
  cl(0:1, 6  ) = [-162.0000000,0.6539931]
  cl(0:1, 7  ) = [-159.0000000,0.7275039]
  cl(0:1, 8  ) = [-156.0000000,0.7311108]
  cl(0:1, 9  ) = [-153.0000000,0.7343196]
  cl(0:1, 10 ) = [-150.0000000,0.7365232]
  cl(0:1, 11 ) = [-147.0000000,0.7198818]
  cl(0:1, 12 ) = [-144.0000000,0.7032402]
  cl(0:1, 13 ) = [-141.0000000,0.6865980]
  cl(0:1, 14 ) = [-138.0000000,0.6663026]
  cl(0:1, 15 ) = [-135.0000000,0.6441806]
  cl(0:1, 16 ) = [-132.0000000,0.6220582]
  cl(0:1, 17 ) = [-129.0000000,0.5963207]
  cl(0:1, 18 ) = [-126.0000000,0.5633526]
  cl(0:1, 19 ) = [-123.0000000,0.5303848]
  cl(0:1, 20 ) = [-120.0000000,0.4974171]
  cl(0:1, 21 ) = [-117.0000000,0.4536344]
  cl(0:1, 22 ) = [-114.0000000,0.4098519]
  cl(0:1, 23 ) = [-111.0000000,0.3660697]
  cl(0:1, 24 ) = [-108.0000000,0.3171777]
  cl(0:1, 25 ) = [-105.0000000,0.2657307]
  cl(0:1, 26 ) = [-102.0000000,0.2142837]
  cl(0:1, 27 ) = [-99.0000000,0.1619871]
  cl(0:1, 28 ) = [-96.0000000,0.1079915]
  cl(0:1, 29 ) = [-93.0000000,0.0539959]
  cl(0:1, 30 ) = [-90.0000000,0.0000003]
  cl(0:1, 31 ) = [-87.0000000,-0.0539959]
  cl(0:1, 32 ) = [-84.0000000,-0.1079917]
  cl(0:1, 33 ) = [-81.0000000,-0.1619867]
  cl(0:1, 34 ) = [-78.0000000,-0.2142834]
  cl(0:1, 35 ) = [-75.0000000,-0.2657310]
  cl(0:1, 36 ) = [-72.0000000,-0.3171775]
  cl(0:1, 37 ) = [-69.0000000,-0.3660694]
  cl(0:1, 38 ) = [-66.0000000,-0.4098520]
  cl(0:1, 39 ) = [-63.0000000,-0.4536347]
  cl(0:1, 40 ) = [-60.0000000,-0.4974173]
  cl(0:1, 41 ) = [-57.0000000,-0.5303850]
  cl(0:1, 42 ) = [-54.0000000,-0.5633528]
  cl(0:1, 43 ) = [-51.0000000,-0.5963209]
  cl(0:1, 44 ) = [-48.0000000,-0.6236450]
  cl(0:1, 45 ) = [-45.0000000,-0.6481473]
  cl(0:1, 46 ) = [-42.0000000,-0.6678894]
  cl(0:1, 47 ) = [-39.0000000,-0.6868027]
  cl(0:1, 48 ) = [-36.0000000,-0.7034784]
  cl(0:1, 49 ) = [-33.0000000,-0.7195724]
  cl(0:1, 50 ) = [-30.0000000,-0.7365233]
  cl(0:1, 51 ) = [-29.3939394,-0.7404283]
  cl(0:1, 52 ) = [-28.7878788,-0.7443333]
  cl(0:1, 53 ) = [-28.1818182,-0.7482383]
  cl(0:1, 54 ) = [-27.5757576,-0.7526136]
  cl(0:1, 55 ) = [-26.9696970,-0.7571906]
  cl(0:1, 56 ) = [-26.3636364,-0.7617675]
  cl(0:1, 57 ) = [-25.7575758,-0.7667498]
  cl(0:1, 58 ) = [-25.1515152,-0.7723400]
  cl(0:1, 59 ) = [-24.5454545,-0.7779303]
  cl(0:1, 60 ) = [-23.9393939,-0.7838313]
  cl(0:1, 61 ) = [-23.3333333,-0.7925289]
  cl(0:1, 62 ) = [-22.7272727,-0.8012264]
  cl(0:1, 63 ) = [-22.1212121,-0.8099239]
  cl(0:1, 64 ) = [-21.5151515,-0.8202358]
  cl(0:1, 65 ) = [-20.9090909,-0.8296226]
  cl(0:1, 66 ) = [-20.3030303,-0.8392133]
  cl(0:1, 67 ) = [-19.6969697,-0.8493429]
  cl(0:1, 68 ) = [-19.0909091,-0.8597236]
  cl(0:1, 69 ) = [-18.4848485,-0.8693556]
  cl(0:1, 70 ) = [-17.8787879,-0.8788415]
  cl(0:1, 71 ) = [-17.2727273,-0.8883041]
  cl(0:1, 72 ) = [-16.6666667,-0.8972268]
  cl(0:1, 73 ) = [-16.0606061,-0.9057687]
  cl(0:1, 74 ) = [-15.4545455,-0.9147693]
  cl(0:1, 75 ) = [-14.8484848,-0.9242245]
  cl(0:1, 76 ) = [-14.2424242,-0.9349332]
  cl(0:1, 77 ) = [-13.6363636,-0.9465800]
  cl(0:1, 78 ) = [-13.0303030,-0.9593765]
  cl(0:1, 79 ) = [-12.4242424,-0.9491372]
  cl(0:1, 80 ) = [-11.8181818,-0.9335113]
  cl(0:1, 81 ) = [-11.2121212,-0.9074126]
  cl(0:1, 82 ) = [-10.6060606,-0.8743587]
  cl(0:1, 83 ) = [-10.0000000,-0.8374362]
  cl(0:1, 84 ) = [-9.3939394,-0.7973600]
  cl(0:1, 85 ) = [-8.7878788,-0.7469181]
  cl(0:1, 86 ) = [-8.1818182,-0.6763852]
  cl(0:1, 87 ) = [-7.5757576,-0.5975488]
  cl(0:1, 88 ) = [-6.9696970,-0.5146872]
  cl(0:1, 89 ) = [-6.3636364,-0.4297833]
  cl(0:1, 90 ) = [-5.7575758,-0.3449857]
  cl(0:1, 91 ) = [-5.1515152,-0.2603421]
  cl(0:1, 92 ) = [-4.5454545,-0.1772840]
  cl(0:1, 93 ) = [-3.9393939,-0.0949968]
  cl(0:1, 94 ) = [-3.3333333,-0.0145488]
  cl(0:1, 95 ) = [-2.7272727,0.0657363]
  cl(0:1, 96 ) = [-2.1212121,0.1453328]
  cl(0:1, 97 ) = [-1.5151515,0.2236727]
  cl(0:1, 98 ) = [-0.9090909,0.3009788]
  cl(0:1, 99 ) = [-0.3030303,0.3760989]
  cl(0:1, 100) = [0.3030303,0.4514448]
  cl(0:1, 101) = [0.9090909,0.5270968]
  cl(0:1, 102) = [1.5151515,0.6027778]
  cl(0:1, 103) = [2.1212121,0.6783281]
  cl(0:1, 104) = [2.7272727,0.7533696]
  cl(0:1, 105) = [3.3333333,0.8274360]
  cl(0:1, 106) = [3.9393939,0.9010643]
  cl(0:1, 107) = [4.5454545,0.9740976]
  cl(0:1, 108) = [5.1515152,1.0464894]
  cl(0:1, 109) = [5.7575758,1.1170974]
  cl(0:1, 110) = [6.3636364,1.1842899]
  cl(0:1, 111) = [6.9696970,1.2489689]
  cl(0:1, 112) = [7.5757576,1.2988194]
  cl(0:1, 113) = [8.1818182,1.3395873]
  cl(0:1, 114) = [8.7878788,1.3600535]
  cl(0:1, 115) = [9.3939394,1.3430137]
  cl(0:1, 116) = [10.0000000,1.3075309]
  cl(0:1, 117) = [10.6060606,1.2174014]
  cl(0:1, 118) = [11.2121212,1.1487663]
  cl(0:1, 119) = [11.8181818,1.1200868]
  cl(0:1, 120) = [12.4242424,1.1064168]
  cl(0:1, 121) = [13.0303030,1.1000115]
  cl(0:1, 122) = [13.6363636,1.1089989]
  cl(0:1, 123) = [14.2424242,1.1200888]
  cl(0:1, 124) = [14.8484848,1.1307560]
  cl(0:1, 125) = [15.4545455,1.1351693]
  cl(0:1, 126) = [16.0606061,1.1367684]
  cl(0:1, 127) = [16.6666667,1.1321107]
  cl(0:1, 128) = [17.2727273,1.1305866]
  cl(0:1, 129) = [17.8787879,1.1326714]
  cl(0:1, 130) = [18.4848485,1.1353449]
  cl(0:1, 131) = [19.0909091,1.1380811]
  cl(0:1, 132) = [19.6969697,1.1399694]
  cl(0:1, 133) = [20.3030303,1.1413626]
  cl(0:1, 134) = [20.9090909,1.1427892]
  cl(0:1, 135) = [21.5151515,1.1452009]
  cl(0:1, 136) = [22.1212121,1.1453462]
  cl(0:1, 137) = [22.7272727,1.1366920]
  cl(0:1, 138) = [23.3333333,1.1280377]
  cl(0:1, 139) = [23.9393939,1.1193834]
  cl(0:1, 140) = [24.5454545,1.1113305]
  cl(0:1, 141) = [25.1515152,1.1033443]
  cl(0:1, 142) = [25.7575758,1.0953582]
  cl(0:1, 143) = [26.3636364,1.0882406]
  cl(0:1, 144) = [26.9696970,1.0817022]
  cl(0:1, 145) = [27.5757576,1.0751638]
  cl(0:1, 146) = [28.1818182,1.0689134]
  cl(0:1, 147) = [28.7878788,1.0633353]
  cl(0:1, 148) = [29.3939394,1.0577571]
  cl(0:1, 149) = [30.0000000,1.0521790]
  cl(0:1, 150) = [33.0000000,1.0279592]
  cl(0:1, 151) = [36.0000000,1.0049677]
  cl(0:1, 152) = [39.0000000,0.9811469]
  cl(0:1, 153) = [42.0000000,0.9541279]
  cl(0:1, 154) = [45.0000000,0.9259255]
  cl(0:1, 155) = [48.0000000,0.8909220]
  cl(0:1, 156) = [51.0000000,0.8518874]
  cl(0:1, 157) = [54.0000000,0.8047903]
  cl(0:1, 158) = [57.0000000,0.7576932]
  cl(0:1, 159) = [60.0000000,0.7105960]
  cl(0:1, 160) = [63.0000000,0.6480495]
  cl(0:1, 161) = [66.0000000,0.5855028]
  cl(0:1, 162) = [69.0000000,0.5229561]
  cl(0:1, 163) = [72.0000000,0.4531104]
  cl(0:1, 164) = [75.0000000,0.3796153]
  cl(0:1, 165) = [78.0000000,0.3061188]
  cl(0:1, 166) = [81.0000000,0.2314091]
  cl(0:1, 167) = [84.0000000,0.1542731]
  cl(0:1, 168) = [87.0000000,0.0771364]
  cl(0:1, 169) = [90.0000000,-0.0000003]
  cl(0:1, 170) = [93.0000000,-0.0539959]
  cl(0:1, 171) = [96.0000000,-0.1079915]
  cl(0:1, 172) = [99.0000000,-0.1619871]
  cl(0:1, 173) = [102.0000000,-0.2142837]
  cl(0:1, 174) = [105.0000000,-0.2657307]
  cl(0:1, 175) = [108.0000000,-0.3171777]
  cl(0:1, 176) = [111.0000000,-0.3660697]
  cl(0:1, 177) = [114.0000000,-0.4098519]
  cl(0:1, 178) = [117.0000000,-0.4536344]
  cl(0:1, 179) = [120.0000000,-0.4974171]
  cl(0:1, 180) = [123.0000000,-0.5303848]
  cl(0:1, 181) = [126.0000000,-0.5633526]
  cl(0:1, 182) = [129.0000000,-0.5963207]
  cl(0:1, 183) = [132.0000000,-0.6220582]
  cl(0:1, 184) = [135.0000000,-0.6441806]
  cl(0:1, 185) = [138.0000000,-0.6663026]
  cl(0:1, 186) = [141.0000000,-0.6865980]
  cl(0:1, 187) = [144.0000000,-0.7032402]
  cl(0:1, 188) = [147.0000000,-0.7198818]
  cl(0:1, 189) = [150.0000000,-0.7365232]
  cl(0:1, 190) = [153.0000000,-0.7239960]
  cl(0:1, 191) = [156.0000000,-0.7100144]
  cl(0:1, 192) = [159.0000000,-0.6958230]
  cl(0:1, 193) = [162.0000000,-0.6222667]
  cl(0:1, 194) = [165.0000000,-0.5187150]
  cl(0:1, 195) = [168.0000000,-0.4151640]
  cl(0:1, 196) = [171.0000000,-0.3115168]
  cl(0:1, 197) = [174.0000000,-0.2076775]
  cl(0:1, 198) = [177.0000000,-0.1033653]
  cl(0:1, 199) = [180.0000000,0.0000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl21_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl21 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl21_ge28.pro'
  ENDIF

!print,y,cl21

!_________________________________________________________________________
  
END FUNCTION calc_cl21_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl22_ge28( y ) RESULT( cl22 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl22
  INTEGER :: i

  cl22 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.1130251]
  cl(0:1, 2  ) = [-174.0000000,0.2270855]
  cl(0:1, 3  ) = [-171.0000000,0.3406285]
  cl(0:1, 4  ) = [-168.0000000,0.4498103]
  cl(0:1, 5  ) = [-165.0000000,0.5568116]
  cl(0:1, 6  ) = [-162.0000000,0.6638141]
  cl(0:1, 7  ) = [-159.0000000,0.7351634]
  cl(0:1, 8  ) = [-156.0000000,0.7366585]
  cl(0:1, 9  ) = [-153.0000000,0.7371917]
  cl(0:1, 10 ) = [-150.0000000,0.7352959]
  cl(0:1, 11 ) = [-147.0000000,0.7187986]
  cl(0:1, 12 ) = [-144.0000000,0.7023011]
  cl(0:1, 13 ) = [-141.0000000,0.6858031]
  cl(0:1, 14 ) = [-138.0000000,0.6656168]
  cl(0:1, 15 ) = [-135.0000000,0.6435865]
  cl(0:1, 16 ) = [-132.0000000,0.6215559]
  cl(0:1, 17 ) = [-129.0000000,0.5959000]
  cl(0:1, 18 ) = [-126.0000000,0.5629934]
  cl(0:1, 19 ) = [-123.0000000,0.5300872]
  cl(0:1, 20 ) = [-120.0000000,0.4971810]
  cl(0:1, 21 ) = [-117.0000000,0.4534386]
  cl(0:1, 22 ) = [-114.0000000,0.4096963]
  cl(0:1, 23 ) = [-111.0000000,0.3659544]
  cl(0:1, 24 ) = [-108.0000000,0.3170911]
  cl(0:1, 25 ) = [-105.0000000,0.2656672]
  cl(0:1, 26 ) = [-102.0000000,0.2142432]
  cl(0:1, 27 ) = [-99.0000000,0.1619645]
  cl(0:1, 28 ) = [-96.0000000,0.1079764]
  cl(0:1, 29 ) = [-93.0000000,0.0539884]
  cl(0:1, 30 ) = [-90.0000000,0.0000003]
  cl(0:1, 31 ) = [-87.0000000,-0.0539883]
  cl(0:1, 32 ) = [-84.0000000,-0.1079766]
  cl(0:1, 33 ) = [-81.0000000,-0.1619641]
  cl(0:1, 34 ) = [-78.0000000,-0.2142430]
  cl(0:1, 35 ) = [-75.0000000,-0.2656675]
  cl(0:1, 36 ) = [-72.0000000,-0.3170909]
  cl(0:1, 37 ) = [-69.0000000,-0.3659540]
  cl(0:1, 38 ) = [-66.0000000,-0.4096964]
  cl(0:1, 39 ) = [-63.0000000,-0.4534388]
  cl(0:1, 40 ) = [-60.0000000,-0.4971812]
  cl(0:1, 41 ) = [-57.0000000,-0.5300874]
  cl(0:1, 42 ) = [-54.0000000,-0.5629937]
  cl(0:1, 43 ) = [-51.0000000,-0.5959002]
  cl(0:1, 44 ) = [-48.0000000,-0.6231489]
  cl(0:1, 45 ) = [-45.0000000,-0.6475688]
  cl(0:1, 46 ) = [-42.0000000,-0.6672098]
  cl(0:1, 47 ) = [-39.0000000,-0.6860167]
  cl(0:1, 48 ) = [-36.0000000,-0.7025674]
  cl(0:1, 49 ) = [-33.0000000,-0.7185144]
  cl(0:1, 50 ) = [-30.0000000,-0.7352960]
  cl(0:1, 51 ) = [-29.3939394,-0.7391612]
  cl(0:1, 52 ) = [-28.7878788,-0.7430264]
  cl(0:1, 53 ) = [-28.1818182,-0.7468916]
  cl(0:1, 54 ) = [-27.5757576,-0.7512234]
  cl(0:1, 55 ) = [-26.9696970,-0.7557551]
  cl(0:1, 56 ) = [-26.3636364,-0.7602868]
  cl(0:1, 57 ) = [-25.7575758,-0.7652213]
  cl(0:1, 58 ) = [-25.1515152,-0.7707598]
  cl(0:1, 59 ) = [-24.5454545,-0.7762983]
  cl(0:1, 60 ) = [-23.9393939,-0.7821323]
  cl(0:1, 61 ) = [-23.3333333,-0.7906267]
  cl(0:1, 62 ) = [-22.7272727,-0.7991210]
  cl(0:1, 63 ) = [-22.1212121,-0.8076154]
  cl(0:1, 64 ) = [-21.5151515,-0.8171177]
  cl(0:1, 65 ) = [-20.9090909,-0.8236613]
  cl(0:1, 66 ) = [-20.3030303,-0.8306974]
  cl(0:1, 67 ) = [-19.6969697,-0.8391302]
  cl(0:1, 68 ) = [-19.0909091,-0.8482638]
  cl(0:1, 69 ) = [-18.4848485,-0.8544917]
  cl(0:1, 70 ) = [-17.8787879,-0.8601639]
  cl(0:1, 71 ) = [-17.2727273,-0.8657424]
  cl(0:1, 72 ) = [-16.6666667,-0.8692178]
  cl(0:1, 73 ) = [-16.0606061,-0.8711194]
  cl(0:1, 74 ) = [-15.4545455,-0.8745908]
  cl(0:1, 75 ) = [-14.8484848,-0.8794480]
  cl(0:1, 76 ) = [-14.2424242,-0.8880413]
  cl(0:1, 77 ) = [-13.6363636,-0.8990114]
  cl(0:1, 78 ) = [-13.0303030,-0.9128323]
  cl(0:1, 79 ) = [-12.4242424,-0.9058377]
  cl(0:1, 80 ) = [-11.8181818,-0.8941497]
  cl(0:1, 81 ) = [-11.2121212,-0.8722891]
  cl(0:1, 82 ) = [-10.6060606,-0.8438369]
  cl(0:1, 83 ) = [-10.0000000,-0.8115337]
  cl(0:1, 84 ) = [-9.3939394,-0.7737753]
  cl(0:1, 85 ) = [-8.7878788,-0.7243862]
  cl(0:1, 86 ) = [-8.1818182,-0.6513506]
  cl(0:1, 87 ) = [-7.5757576,-0.5702593]
  cl(0:1, 88 ) = [-6.9696970,-0.4848635]
  cl(0:1, 89 ) = [-6.3636364,-0.4012603]
  cl(0:1, 90 ) = [-5.7575758,-0.3178290]
  cl(0:1, 91 ) = [-5.1515152,-0.2345606]
  cl(0:1, 92 ) = [-4.5454545,-0.1527682]
  cl(0:1, 93 ) = [-3.9393939,-0.0717447]
  cl(0:1, 94 ) = [-3.3333333,0.0076527]
  cl(0:1, 95 ) = [-2.7272727,0.0869540]
  cl(0:1, 96 ) = [-2.1212121,0.1656027]
  cl(0:1, 97 ) = [-1.5151515,0.2431530]
  cl(0:1, 98 ) = [-0.9090909,0.3196901]
  cl(0:1, 99 ) = [-0.3030303,0.3941856]
  cl(0:1, 100) = [0.3030303,0.4689024]
  cl(0:1, 101) = [0.9090909,0.5438816]
  cl(0:1, 102) = [1.5151515,0.6188530]
  cl(0:1, 103) = [2.1212121,0.6937184]
  cl(0:1, 104) = [2.7272727,0.7681496]
  cl(0:1, 105) = [3.3333333,0.8417152]
  cl(0:1, 106) = [3.9393939,0.9148296]
  cl(0:1, 107) = [4.5454545,0.9873549]
  cl(0:1, 108) = [5.1515152,1.0592775]
  cl(0:1, 109) = [5.7575758,1.1295334]
  cl(0:1, 110) = [6.3636364,1.1964661]
  cl(0:1, 111) = [6.9696970,1.2602473]
  cl(0:1, 112) = [7.5757576,1.3097100]
  cl(0:1, 113) = [8.1818182,1.3504022]
  cl(0:1, 114) = [8.7878788,1.3709132]
  cl(0:1, 115) = [9.3939394,1.3566771]
  cl(0:1, 116) = [10.0000000,1.3270001]
  cl(0:1, 117) = [10.6060606,1.2471320]
  cl(0:1, 118) = [11.2121212,1.1863267]
  cl(0:1, 119) = [11.8181818,1.1613130]
  cl(0:1, 120) = [12.4242424,1.1484475]
  cl(0:1, 121) = [13.0303030,1.1416104]
  cl(0:1, 122) = [13.6363636,1.1444620]
  cl(0:1, 123) = [14.2424242,1.1536093]
  cl(0:1, 124) = [14.8484848,1.1635545]
  cl(0:1, 125) = [15.4545455,1.1666477]
  cl(0:1, 126) = [16.0606061,1.1666101]
  cl(0:1, 127) = [16.6666667,1.1597082]
  cl(0:1, 128) = [17.2727273,1.1544956]
  cl(0:1, 129) = [17.8787879,1.1508142]
  cl(0:1, 130) = [18.4848485,1.1495857]
  cl(0:1, 131) = [19.0909091,1.1489288]
  cl(0:1, 132) = [19.6969697,1.1471430]
  cl(0:1, 133) = [20.3030303,1.1442905]
  cl(0:1, 134) = [20.9090909,1.1426696]
  cl(0:1, 135) = [21.5151515,1.1442351]
  cl(0:1, 136) = [22.1212121,1.1430707]
  cl(0:1, 137) = [22.7272727,1.1343768]
  cl(0:1, 138) = [23.3333333,1.1256828]
  cl(0:1, 139) = [23.9393939,1.1169889]
  cl(0:1, 140) = [24.5454545,1.1089985]
  cl(0:1, 141) = [25.1515152,1.1010864]
  cl(0:1, 142) = [25.7575758,1.0931742]
  cl(0:1, 143) = [26.3636364,1.0861250]
  cl(0:1, 144) = [26.9696970,1.0796512]
  cl(0:1, 145) = [27.5757576,1.0731774]
  cl(0:1, 146) = [28.1818182,1.0669893]
  cl(0:1, 147) = [28.7878788,1.0614679]
  cl(0:1, 148) = [29.3939394,1.0559466]
  cl(0:1, 149) = [30.0000000,1.0504252]
  cl(0:1, 150) = [33.0000000,1.0264483]
  cl(0:1, 151) = [36.0000000,1.0036664]
  cl(0:1, 152) = [39.0000000,0.9800239]
  cl(0:1, 153) = [42.0000000,0.9531571]
  cl(0:1, 154) = [45.0000000,0.9250990]
  cl(0:1, 155) = [48.0000000,0.8902132]
  cl(0:1, 156) = [51.0000000,0.8512863]
  cl(0:1, 157) = [54.0000000,0.8042771]
  cl(0:1, 158) = [57.0000000,0.7572679]
  cl(0:1, 159) = [60.0000000,0.7102586]
  cl(0:1, 160) = [63.0000000,0.6477697]
  cl(0:1, 161) = [66.0000000,0.5852806]
  cl(0:1, 162) = [69.0000000,0.5227914]
  cl(0:1, 163) = [72.0000000,0.4529868]
  cl(0:1, 164) = [75.0000000,0.3795246]
  cl(0:1, 165) = [78.0000000,0.3060611]
  cl(0:1, 166) = [81.0000000,0.2313769]
  cl(0:1, 167) = [84.0000000,0.1542517]
  cl(0:1, 168) = [87.0000000,0.0771257]
  cl(0:1, 169) = [90.0000000,-0.0000003]
  cl(0:1, 170) = [93.0000000,-0.0539884]
  cl(0:1, 171) = [96.0000000,-0.1079764]
  cl(0:1, 172) = [99.0000000,-0.1619645]
  cl(0:1, 173) = [102.0000000,-0.2142432]
  cl(0:1, 174) = [105.0000000,-0.2656672]
  cl(0:1, 175) = [108.0000000,-0.3170911]
  cl(0:1, 176) = [111.0000000,-0.3659544]
  cl(0:1, 177) = [114.0000000,-0.4096963]
  cl(0:1, 178) = [117.0000000,-0.4534386]
  cl(0:1, 179) = [120.0000000,-0.4971810]
  cl(0:1, 180) = [123.0000000,-0.5300872]
  cl(0:1, 181) = [126.0000000,-0.5629934]
  cl(0:1, 182) = [129.0000000,-0.5959000]
  cl(0:1, 183) = [132.0000000,-0.6215559]
  cl(0:1, 184) = [135.0000000,-0.6435865]
  cl(0:1, 185) = [138.0000000,-0.6656168]
  cl(0:1, 186) = [141.0000000,-0.6858031]
  cl(0:1, 187) = [144.0000000,-0.7023011]
  cl(0:1, 188) = [147.0000000,-0.7187986]
  cl(0:1, 189) = [150.0000000,-0.7352959]
  cl(0:1, 190) = [153.0000000,-0.7283305]
  cl(0:1, 191) = [156.0000000,-0.7178508]
  cl(0:1, 192) = [159.0000000,-0.7068637]
  cl(0:1, 193) = [162.0000000,-0.6348112]
  cl(0:1, 194) = [165.0000000,-0.5314704]
  cl(0:1, 195) = [168.0000000,-0.4281303]
  cl(0:1, 196) = [171.0000000,-0.3233131]
  cl(0:1, 197) = [174.0000000,-0.2155416]
  cl(0:1, 198) = [177.0000000,-0.1072794]
  cl(0:1, 199) = [180.0000000,0.0000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl22_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl22 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl22_ge28.pro'
  ENDIF

!print,y,cl22

!_________________________________________________________________________
  
END FUNCTION calc_cl22_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl23_ge28( y ) RESULT( cl23 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl23
  INTEGER :: i

  cl23 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.1213732]
  cl(0:1, 2  ) = [-174.0000000,0.2438581]
  cl(0:1, 3  ) = [-171.0000000,0.3657876]
  cl(0:1, 4  ) = [-168.0000000,0.4755152]
  cl(0:1, 5  ) = [-165.0000000,0.5791420]
  cl(0:1, 6  ) = [-162.0000000,0.6827700]
  cl(0:1, 7  ) = [-159.0000000,0.7499291]
  cl(0:1, 8  ) = [-156.0000000,0.7454019]
  cl(0:1, 9  ) = [-153.0000000,0.7400458]
  cl(0:1, 10 ) = [-150.0000000,0.7325966]
  cl(0:1, 11 ) = [-147.0000000,0.7164162]
  cl(0:1, 12 ) = [-144.0000000,0.7002356]
  cl(0:1, 13 ) = [-141.0000000,0.6840546]
  cl(0:1, 14 ) = [-138.0000000,0.6641085]
  cl(0:1, 15 ) = [-135.0000000,0.6422798]
  cl(0:1, 16 ) = [-132.0000000,0.6204509]
  cl(0:1, 17 ) = [-129.0000000,0.5949746]
  cl(0:1, 18 ) = [-126.0000000,0.5622035]
  cl(0:1, 19 ) = [-123.0000000,0.5294325]
  cl(0:1, 20 ) = [-120.0000000,0.4966616]
  cl(0:1, 21 ) = [-117.0000000,0.4530077]
  cl(0:1, 22 ) = [-114.0000000,0.4093540]
  cl(0:1, 23 ) = [-111.0000000,0.3657007]
  cl(0:1, 24 ) = [-108.0000000,0.3169008]
  cl(0:1, 25 ) = [-105.0000000,0.2655275]
  cl(0:1, 26 ) = [-102.0000000,0.2141542]
  cl(0:1, 27 ) = [-99.0000000,0.1619149]
  cl(0:1, 28 ) = [-96.0000000,0.1079432]
  cl(0:1, 29 ) = [-93.0000000,0.0539718]
  cl(0:1, 30 ) = [-90.0000000,0.0000003]
  cl(0:1, 31 ) = [-87.0000000,-0.0539717]
  cl(0:1, 32 ) = [-84.0000000,-0.1079434]
  cl(0:1, 33 ) = [-81.0000000,-0.1619144]
  cl(0:1, 34 ) = [-78.0000000,-0.2141540]
  cl(0:1, 35 ) = [-75.0000000,-0.2655279]
  cl(0:1, 36 ) = [-72.0000000,-0.3169005]
  cl(0:1, 37 ) = [-69.0000000,-0.3657003]
  cl(0:1, 38 ) = [-66.0000000,-0.4093542]
  cl(0:1, 39 ) = [-63.0000000,-0.4530080]
  cl(0:1, 40 ) = [-60.0000000,-0.4966618]
  cl(0:1, 41 ) = [-57.0000000,-0.5294327]
  cl(0:1, 42 ) = [-54.0000000,-0.5622037]
  cl(0:1, 43 ) = [-51.0000000,-0.5949748]
  cl(0:1, 44 ) = [-48.0000000,-0.6220577]
  cl(0:1, 45 ) = [-45.0000000,-0.6462964]
  cl(0:1, 46 ) = [-42.0000000,-0.6657152]
  cl(0:1, 47 ) = [-39.0000000,-0.6842878]
  cl(0:1, 48 ) = [-36.0000000,-0.7005639]
  cl(0:1, 49 ) = [-33.0000000,-0.7161875]
  cl(0:1, 50 ) = [-30.0000000,-0.7325967]
  cl(0:1, 51 ) = [-29.3939394,-0.7363744]
  cl(0:1, 52 ) = [-28.7878788,-0.7401520]
  cl(0:1, 53 ) = [-28.1818182,-0.7439297]
  cl(0:1, 54 ) = [-27.5757576,-0.7481656]
  cl(0:1, 55 ) = [-26.9696970,-0.7525980]
  cl(0:1, 56 ) = [-26.3636364,-0.7570304]
  cl(0:1, 57 ) = [-25.7575758,-0.7618597]
  cl(0:1, 58 ) = [-25.1515152,-0.7672842]
  cl(0:1, 59 ) = [-24.5454545,-0.7727087]
  cl(0:1, 60 ) = [-23.9393939,-0.7783974]
  cl(0:1, 61 ) = [-23.3333333,-0.7864631]
  cl(0:1, 62 ) = [-22.7272727,-0.7945289]
  cl(0:1, 63 ) = [-22.1212121,-0.8025946]
  cl(0:1, 64 ) = [-21.5151515,-0.8109078]
  cl(0:1, 65 ) = [-20.9090909,-0.8165161]
  cl(0:1, 66 ) = [-20.3030303,-0.8225488]
  cl(0:1, 67 ) = [-19.6969697,-0.8300021]
  cl(0:1, 68 ) = [-19.0909091,-0.8382761]
  cl(0:1, 69 ) = [-18.4848485,-0.8415258]
  cl(0:1, 70 ) = [-17.8787879,-0.8438307]
  cl(0:1, 71 ) = [-17.2727273,-0.8459694]
  cl(0:1, 72 ) = [-16.6666667,-0.8444627]
  cl(0:1, 73 ) = [-16.0606061,-0.8401000]
  cl(0:1, 74 ) = [-15.4545455,-0.8378176]
  cl(0:1, 75 ) = [-14.8484848,-0.8371837]
  cl(0:1, 76 ) = [-14.2424242,-0.8408901]
  cl(0:1, 77 ) = [-13.6363636,-0.8468174]
  cl(0:1, 78 ) = [-13.0303030,-0.8553160]
  cl(0:1, 79 ) = [-12.4242424,-0.8472452]
  cl(0:1, 80 ) = [-11.8181818,-0.8354327]
  cl(0:1, 81 ) = [-11.2121212,-0.8153935]
  cl(0:1, 82 ) = [-10.6060606,-0.7900948]
  cl(0:1, 83 ) = [-10.0000000,-0.7617041]
  cl(0:1, 84 ) = [-9.3939394,-0.7283577]
  cl(0:1, 85 ) = [-8.7878788,-0.6841425]
  cl(0:1, 86 ) = [-8.1818182,-0.6179794]
  cl(0:1, 87 ) = [-7.5757576,-0.5381040]
  cl(0:1, 88 ) = [-6.9696970,-0.4516312]
  cl(0:1, 89 ) = [-6.3636364,-0.3699048]
  cl(0:1, 90 ) = [-5.7575758,-0.2884767]
  cl(0:1, 91 ) = [-5.1515152,-0.2072605]
  cl(0:1, 92 ) = [-4.5454545,-0.1272799]
  cl(0:1, 93 ) = [-3.9393939,-0.0480058]
  cl(0:1, 94 ) = [-3.3333333,0.0299773]
  cl(0:1, 95 ) = [-2.7272727,0.1079250]
  cl(0:1, 96 ) = [-2.1212121,0.1852867]
  cl(0:1, 97 ) = [-1.5151515,0.2618187]
  cl(0:1, 98 ) = [-0.9090909,0.3374373]
  cl(0:1, 99 ) = [-0.3030303,0.4113400]
  cl(0:1, 100) = [0.3030303,0.4854252]
  cl(0:1, 101) = [0.9090909,0.5596887]
  cl(0:1, 102) = [1.5151515,0.6338894]
  cl(0:1, 103) = [2.1212121,0.7080073]
  cl(0:1, 104) = [2.7272727,0.7817439]
  cl(0:1, 105) = [3.3333333,0.8547687]
  cl(0:1, 106) = [3.9393939,0.9273436]
  cl(0:1, 107) = [4.5454545,0.9993371]
  cl(0:1, 108) = [5.1515152,1.0707753]
  cl(0:1, 109) = [5.7575758,1.1406757]
  cl(0:1, 110) = [6.3636364,1.2073859]
  cl(0:1, 111) = [6.9696970,1.2703053]
  cl(0:1, 112) = [7.5757576,1.3196772]
  cl(0:1, 113) = [8.1818182,1.3611790]
  cl(0:1, 114) = [8.7878788,1.3864516]
  cl(0:1, 115) = [9.3939394,1.3836479]
  cl(0:1, 116) = [10.0000000,1.3663262]
  cl(0:1, 117) = [10.6060606,1.3079390]
  cl(0:1, 118) = [11.2121212,1.2635492]
  cl(0:1, 119) = [11.8181818,1.2462208]
  cl(0:1, 120) = [12.4242424,1.2331657]
  cl(0:1, 121) = [13.0303030,1.2228388]
  cl(0:1, 122) = [13.6363636,1.2130210]
  cl(0:1, 123) = [14.2424242,1.2099585]
  cl(0:1, 124) = [14.8484848,1.2095778]
  cl(0:1, 125) = [15.4545455,1.2062823]
  cl(0:1, 126) = [16.0606061,1.2012965]
  cl(0:1, 127) = [16.6666667,1.1904965]
  cl(0:1, 128) = [17.2727273,1.1796414]
  cl(0:1, 129) = [17.8787879,1.1682593]
  cl(0:1, 130) = [18.4848485,1.1627937]
  cl(0:1, 131) = [19.0909091,1.1587220]
  cl(0:1, 132) = [19.6969697,1.1533984]
  cl(0:1, 133) = [20.3030303,1.1466735]
  cl(0:1, 134) = [20.9090909,1.1421426]
  cl(0:1, 135) = [21.5151515,1.1416320]
  cl(0:1, 136) = [22.1212121,1.1380162]
  cl(0:1, 137) = [22.7272727,1.1292511]
  cl(0:1, 138) = [23.3333333,1.1204861]
  cl(0:1, 139) = [23.9393939,1.1117210]
  cl(0:1, 140) = [24.5454545,1.1038699]
  cl(0:1, 141) = [25.1515152,1.0961204]
  cl(0:1, 142) = [25.7575758,1.0883708]
  cl(0:1, 143) = [26.3636364,1.0814720]
  cl(0:1, 144) = [26.9696970,1.0751403]
  cl(0:1, 145) = [27.5757576,1.0688086]
  cl(0:1, 146) = [28.1818182,1.0627575]
  cl(0:1, 147) = [28.7878788,1.0573611]
  cl(0:1, 148) = [29.3939394,1.0519646]
  cl(0:1, 149) = [30.0000000,1.0465682]
  cl(0:1, 150) = [33.0000000,1.0231251]
  cl(0:1, 151) = [36.0000000,1.0008044]
  cl(0:1, 152) = [39.0000000,0.9775541]
  cl(0:1, 153) = [42.0000000,0.9510219]
  cl(0:1, 154) = [45.0000000,0.9232813]
  cl(0:1, 155) = [48.0000000,0.8886542]
  cl(0:1, 156) = [51.0000000,0.8499643]
  cl(0:1, 157) = [54.0000000,0.8031484]
  cl(0:1, 158) = [57.0000000,0.7563325]
  cl(0:1, 159) = [60.0000000,0.7095166]
  cl(0:1, 160) = [63.0000000,0.6471543]
  cl(0:1, 161) = [66.0000000,0.5847918]
  cl(0:1, 162) = [69.0000000,0.5224291]
  cl(0:1, 163) = [72.0000000,0.4527150]
  cl(0:1, 164) = [75.0000000,0.3793251]
  cl(0:1, 165) = [78.0000000,0.3059341]
  cl(0:1, 166) = [81.0000000,0.2313061]
  cl(0:1, 167) = [84.0000000,0.1542045]
  cl(0:1, 168) = [87.0000000,0.0771022]
  cl(0:1, 169) = [90.0000000,-0.0000003]
  cl(0:1, 170) = [93.0000000,-0.0539718]
  cl(0:1, 171) = [96.0000000,-0.1079432]
  cl(0:1, 172) = [99.0000000,-0.1619149]
  cl(0:1, 173) = [102.0000000,-0.2141542]
  cl(0:1, 174) = [105.0000000,-0.2655275]
  cl(0:1, 175) = [108.0000000,-0.3169008]
  cl(0:1, 176) = [111.0000000,-0.3657007]
  cl(0:1, 177) = [114.0000000,-0.4093540]
  cl(0:1, 178) = [117.0000000,-0.4530077]
  cl(0:1, 179) = [120.0000000,-0.4966616]
  cl(0:1, 180) = [123.0000000,-0.5294325]
  cl(0:1, 181) = [126.0000000,-0.5622035]
  cl(0:1, 182) = [129.0000000,-0.5949746]
  cl(0:1, 183) = [132.0000000,-0.6204509]
  cl(0:1, 184) = [135.0000000,-0.6422798]
  cl(0:1, 185) = [138.0000000,-0.6641085]
  cl(0:1, 186) = [141.0000000,-0.6840546]
  cl(0:1, 187) = [144.0000000,-0.7002356]
  cl(0:1, 188) = [147.0000000,-0.7164162]
  cl(0:1, 189) = [150.0000000,-0.7325966]
  cl(0:1, 190) = [153.0000000,-0.7332728]
  cl(0:1, 191) = [156.0000000,-0.7309207]
  cl(0:1, 192) = [159.0000000,-0.7281314]
  cl(0:1, 193) = [162.0000000,-0.6590043]
  cl(0:1, 194) = [165.0000000,-0.5560572]
  cl(0:1, 195) = [168.0000000,-0.4531107]
  cl(0:1, 196) = [171.0000000,-0.3460317]
  cl(0:1, 197) = [174.0000000,-0.2306874]
  cl(0:1, 198) = [177.0000000,-0.1148178]
  cl(0:1, 199) = [180.0000000,0.0000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl23_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl23 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl23_ge28.pro'
  ENDIF

!print,y,cl23

!_________________________________________________________________________
  
END FUNCTION calc_cl23_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl24_ge28( y ) RESULT( cl24 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl24
  INTEGER :: i

  cl24 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.1314079]
  cl(0:1, 2  ) = [-174.0000000,0.2640195]
  cl(0:1, 3  ) = [-171.0000000,0.3960298]
  cl(0:1, 4  ) = [-168.0000000,0.5064309]
  cl(0:1, 5  ) = [-165.0000000,0.6060275]
  cl(0:1, 6  ) = [-162.0000000,0.7056253]
  cl(0:1, 7  ) = [-159.0000000,0.7677039]
  cl(0:1, 8  ) = [-156.0000000,0.7553977]
  cl(0:1, 9  ) = [-153.0000000,0.7426591]
  cl(0:1, 10 ) = [-150.0000000,0.7288285]
  cl(0:1, 11 ) = [-147.0000000,0.7130905]
  cl(0:1, 12 ) = [-144.0000000,0.6973523]
  cl(0:1, 13 ) = [-141.0000000,0.6816138]
  cl(0:1, 14 ) = [-138.0000000,0.6620029]
  cl(0:1, 15 ) = [-135.0000000,0.6404558]
  cl(0:1, 16 ) = [-132.0000000,0.6189084]
  cl(0:1, 17 ) = [-129.0000000,0.5936828]
  cl(0:1, 18 ) = [-126.0000000,0.5611007]
  cl(0:1, 19 ) = [-123.0000000,0.5285186]
  cl(0:1, 20 ) = [-120.0000000,0.4959366]
  cl(0:1, 21 ) = [-117.0000000,0.4524063]
  cl(0:1, 22 ) = [-114.0000000,0.4088763]
  cl(0:1, 23 ) = [-111.0000000,0.3653466]
  cl(0:1, 24 ) = [-108.0000000,0.3166350]
  cl(0:1, 25 ) = [-105.0000000,0.2653325]
  cl(0:1, 26 ) = [-102.0000000,0.2140300]
  cl(0:1, 27 ) = [-99.0000000,0.1618455]
  cl(0:1, 28 ) = [-96.0000000,0.1078969]
  cl(0:1, 29 ) = [-93.0000000,0.0539486]
  cl(0:1, 30 ) = [-90.0000000,0.0000003]
  cl(0:1, 31 ) = [-87.0000000,-0.0539485]
  cl(0:1, 32 ) = [-84.0000000,-0.1078971]
  cl(0:1, 33 ) = [-81.0000000,-0.1618451]
  cl(0:1, 34 ) = [-78.0000000,-0.2140298]
  cl(0:1, 35 ) = [-75.0000000,-0.2653329]
  cl(0:1, 36 ) = [-72.0000000,-0.3166348]
  cl(0:1, 37 ) = [-69.0000000,-0.3653462]
  cl(0:1, 38 ) = [-66.0000000,-0.4088764]
  cl(0:1, 39 ) = [-63.0000000,-0.4524066]
  cl(0:1, 40 ) = [-60.0000000,-0.4959368]
  cl(0:1, 41 ) = [-57.0000000,-0.5285188]
  cl(0:1, 42 ) = [-54.0000000,-0.5611009]
  cl(0:1, 43 ) = [-51.0000000,-0.5936830]
  cl(0:1, 44 ) = [-48.0000000,-0.6205343]
  cl(0:1, 45 ) = [-45.0000000,-0.6445203]
  cl(0:1, 46 ) = [-42.0000000,-0.6636288]
  cl(0:1, 47 ) = [-39.0000000,-0.6818744]
  cl(0:1, 48 ) = [-36.0000000,-0.6977669]
  cl(0:1, 49 ) = [-33.0000000,-0.7129392]
  cl(0:1, 50 ) = [-30.0000000,-0.7288286]
  cl(0:1, 51 ) = [-29.3939394,-0.7324841]
  cl(0:1, 52 ) = [-28.7878788,-0.7361395]
  cl(0:1, 53 ) = [-28.1818182,-0.7397949]
  cl(0:1, 54 ) = [-27.5757576,-0.7438971]
  cl(0:1, 55 ) = [-26.9696970,-0.7481908]
  cl(0:1, 56 ) = [-26.3636364,-0.7524845]
  cl(0:1, 57 ) = [-25.7575758,-0.7571669]
  cl(0:1, 58 ) = [-25.1515152,-0.7624324]
  cl(0:1, 59 ) = [-24.5454545,-0.7676978]
  cl(0:1, 60 ) = [-23.9393939,-0.7731859]
  cl(0:1, 61 ) = [-23.3333333,-0.7806783]
  cl(0:1, 62 ) = [-22.7272727,-0.7881706]
  cl(0:1, 63 ) = [-22.1212121,-0.7956630]
  cl(0:1, 64 ) = [-21.5151515,-0.8031229]
  cl(0:1, 65 ) = [-20.9090909,-0.8091315]
  cl(0:1, 66 ) = [-20.3030303,-0.8153615]
  cl(0:1, 67 ) = [-19.6969697,-0.8227243]
  cl(0:1, 68 ) = [-19.0909091,-0.8309069]
  cl(0:1, 69 ) = [-18.4848485,-0.8319181]
  cl(0:1, 70 ) = [-17.8787879,-0.8315951]
  cl(0:1, 71 ) = [-17.2727273,-0.8310314]
  cl(0:1, 72 ) = [-16.6666667,-0.8252564]
  cl(0:1, 73 ) = [-16.0606061,-0.8152839]
  cl(0:1, 74 ) = [-15.4545455,-0.8072027]
  cl(0:1, 75 ) = [-14.8484848,-0.8004848]
  cl(0:1, 76 ) = [-14.2424242,-0.7972730]
  cl(0:1, 77 ) = [-13.6363636,-0.7954113]
  cl(0:1, 78 ) = [-13.0303030,-0.7950177]
  cl(0:1, 79 ) = [-12.4242424,-0.7834917]
  cl(0:1, 80 ) = [-11.8181818,-0.7693906]
  cl(0:1, 81 ) = [-11.2121212,-0.7498525]
  cl(0:1, 82 ) = [-10.6060606,-0.7268642]
  cl(0:1, 83 ) = [-10.0000000,-0.7018830]
  cl(0:1, 84 ) = [-9.3939394,-0.6737601]
  cl(0:1, 85 ) = [-8.7878788,-0.6366347]
  cl(0:1, 86 ) = [-8.1818182,-0.5818746]
  cl(0:1, 87 ) = [-7.5757576,-0.5051507]
  cl(0:1, 88 ) = [-6.9696970,-0.4185952]
  cl(0:1, 89 ) = [-6.3636364,-0.3389870]
  cl(0:1, 90 ) = [-5.7575758,-0.2598405]
  cl(0:1, 91 ) = [-5.1515152,-0.1809890]
  cl(0:1, 92 ) = [-4.5454545,-0.1030715]
  cl(0:1, 93 ) = [-3.9393939,-0.0257708]
  cl(0:1, 94 ) = [-3.3333333,0.0506291]
  cl(0:1, 95 ) = [-2.7272727,0.1270322]
  cl(0:1, 96 ) = [-2.1212121,0.2029201]
  cl(0:1, 97 ) = [-1.5151515,0.2783072]
  cl(0:1, 98 ) = [-0.9090909,0.3529362]
  cl(0:1, 99 ) = [-0.3030303,0.4263206]
  cl(0:1, 100) = [0.3030303,0.4998186]
  cl(0:1, 101) = [0.9090909,0.5733765]
  cl(0:1, 102) = [1.5151515,0.6467996]
  cl(0:1, 103) = [2.1212121,0.7201549]
  cl(0:1, 104) = [2.7272727,0.7931475]
  cl(0:1, 105) = [3.3333333,0.8656160]
  cl(0:1, 106) = [3.9393939,0.9376481]
  cl(0:1, 107) = [4.5454545,1.0091036]
  cl(0:1, 108) = [5.1515152,1.0800549]
  cl(0:1, 109) = [5.7575758,1.1496051]
  cl(0:1, 110) = [6.3636364,1.2161542]
  cl(0:1, 111) = [6.9696970,1.2782848]
  cl(0:1, 112) = [7.5757576,1.3279747]
  cl(0:1, 113) = [8.1818182,1.3710705]
  cl(0:1, 114) = [8.7878788,1.4036951]
  cl(0:1, 115) = [9.3939394,1.4170968]
  cl(0:1, 116) = [10.0000000,1.4164587]
  cl(0:1, 117) = [10.6060606,1.3865746]
  cl(0:1, 118) = [11.2121212,1.3640026]
  cl(0:1, 119) = [11.8181818,1.3568882]
  cl(0:1, 120) = [12.4242424,1.3408957]
  cl(0:1, 121) = [13.0303030,1.3222316]
  cl(0:1, 122) = [13.6363636,1.2958530]
  cl(0:1, 123) = [14.2424242,1.2755050]
  cl(0:1, 124) = [14.8484848,1.2603128]
  cl(0:1, 125) = [15.4545455,1.2481792]
  cl(0:1, 126) = [16.0606061,1.2365951]
  cl(0:1, 127) = [16.6666667,1.2211244]
  cl(0:1, 128) = [17.2727273,1.2037249]
  cl(0:1, 129) = [17.8787879,1.1837280]
  cl(0:1, 130) = [18.4848485,1.1740065]
  cl(0:1, 131) = [19.0909091,1.1666875]
  cl(0:1, 132) = [19.6969697,1.1580263]
  cl(0:1, 133) = [20.3030303,1.1482577]
  cl(0:1, 134) = [20.9090909,1.1410465]
  cl(0:1, 135) = [21.5151515,1.1373448]
  cl(0:1, 136) = [22.1212121,1.1308921]
  cl(0:1, 137) = [22.7272727,1.1220498]
  cl(0:1, 138) = [23.3333333,1.1132074]
  cl(0:1, 139) = [23.9393939,1.1043650]
  cl(0:1, 140) = [24.5454545,1.0967106]
  cl(0:1, 141) = [25.1515152,1.0891881]
  cl(0:1, 142) = [25.7575758,1.0816656]
  cl(0:1, 143) = [26.3636364,1.0749765]
  cl(0:1, 144) = [26.9696970,1.0688432]
  cl(0:1, 145) = [27.5757576,1.0627099]
  cl(0:1, 146) = [28.1818182,1.0568500]
  cl(0:1, 147) = [28.7878788,1.0516281]
  cl(0:1, 148) = [29.3939394,1.0464061]
  cl(0:1, 149) = [30.0000000,1.0411841]
  cl(0:1, 150) = [33.0000000,1.0184859]
  cl(0:1, 151) = [36.0000000,0.9968089]
  cl(0:1, 152) = [39.0000000,0.9741063]
  cl(0:1, 153) = [42.0000000,0.9480412]
  cl(0:1, 154) = [45.0000000,0.9207438]
  cl(0:1, 155) = [48.0000000,0.8864779]
  cl(0:1, 156) = [51.0000000,0.8481187]
  cl(0:1, 157) = [54.0000000,0.8015728]
  cl(0:1, 158) = [57.0000000,0.7550268]
  cl(0:1, 159) = [60.0000000,0.7084807]
  cl(0:1, 160) = [63.0000000,0.6462952]
  cl(0:1, 161) = [66.0000000,0.5841094]
  cl(0:1, 162) = [69.0000000,0.5219234]
  cl(0:1, 163) = [72.0000000,0.4523355]
  cl(0:1, 164) = [75.0000000,0.3790467]
  cl(0:1, 165) = [78.0000000,0.3057568]
  cl(0:1, 166) = [81.0000000,0.2312073]
  cl(0:1, 167) = [84.0000000,0.1541387]
  cl(0:1, 168) = [87.0000000,0.0770693]
  cl(0:1, 169) = [90.0000000,-0.0000003]
  cl(0:1, 170) = [93.0000000,-0.0539486]
  cl(0:1, 171) = [96.0000000,-0.1078969]
  cl(0:1, 172) = [99.0000000,-0.1618455]
  cl(0:1, 173) = [102.0000000,-0.2140300]
  cl(0:1, 174) = [105.0000000,-0.2653325]
  cl(0:1, 175) = [108.0000000,-0.3166350]
  cl(0:1, 176) = [111.0000000,-0.3653466]
  cl(0:1, 177) = [114.0000000,-0.4088763]
  cl(0:1, 178) = [117.0000000,-0.4524063]
  cl(0:1, 179) = [120.0000000,-0.4959366]
  cl(0:1, 180) = [123.0000000,-0.5285186]
  cl(0:1, 181) = [126.0000000,-0.5611007]
  cl(0:1, 182) = [129.0000000,-0.5936828]
  cl(0:1, 183) = [132.0000000,-0.6189084]
  cl(0:1, 184) = [135.0000000,-0.6404558]
  cl(0:1, 185) = [138.0000000,-0.6620029]
  cl(0:1, 186) = [141.0000000,-0.6816138]
  cl(0:1, 187) = [144.0000000,-0.6973523]
  cl(0:1, 188) = [147.0000000,-0.7130905]
  cl(0:1, 189) = [150.0000000,-0.7288285]
  cl(0:1, 190) = [153.0000000,-0.7382508]
  cl(0:1, 191) = [156.0000000,-0.7460933]
  cl(0:1, 192) = [159.0000000,-0.7537076]
  cl(0:1, 193) = [162.0000000,-0.6881434]
  cl(0:1, 194) = [165.0000000,-0.5856501]
  cl(0:1, 195) = [168.0000000,-0.4831572]
  cl(0:1, 196) = [171.0000000,-0.3733458]
  cl(0:1, 197) = [174.0000000,-0.2488969]
  cl(0:1, 198) = [177.0000000,-0.1238810]
  cl(0:1, 199) = [180.0000000,0.0000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl24_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl24 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl24_ge28.pro'
  ENDIF

!print,y,cl24

!_________________________________________________________________________
  
END FUNCTION calc_cl24_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl25_ge28( y ) RESULT( cl25 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl25
  INTEGER :: i

  cl25 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.1406324]
  cl(0:1, 2  ) = [-174.0000000,0.2825531]
  cl(0:1, 3  ) = [-171.0000000,0.4238303]
  cl(0:1, 4  ) = [-168.0000000,0.5348691]
  cl(0:1, 5  ) = [-165.0000000,0.6307890]
  cl(0:1, 6  ) = [-162.0000000,0.7267100]
  cl(0:1, 7  ) = [-159.0000000,0.7840714]
  cl(0:1, 8  ) = [-156.0000000,0.7645006]
  cl(0:1, 9  ) = [-153.0000000,0.7448066]
  cl(0:1, 10 ) = [-150.0000000,0.7248016]
  cl(0:1, 11 ) = [-147.0000000,0.7095363]
  cl(0:1, 12 ) = [-144.0000000,0.6942710]
  cl(0:1, 13 ) = [-141.0000000,0.6790054]
  cl(0:1, 14 ) = [-138.0000000,0.6597527]
  cl(0:1, 15 ) = [-135.0000000,0.6385065]
  cl(0:1, 16 ) = [-132.0000000,0.6172600]
  cl(0:1, 17 ) = [-129.0000000,0.5923023]
  cl(0:1, 18 ) = [-126.0000000,0.5599222]
  cl(0:1, 19 ) = [-123.0000000,0.5275420]
  cl(0:1, 20 ) = [-120.0000000,0.4951617]
  cl(0:1, 21 ) = [-117.0000000,0.4517636]
  cl(0:1, 22 ) = [-114.0000000,0.4083657]
  cl(0:1, 23 ) = [-111.0000000,0.3649681]
  cl(0:1, 24 ) = [-108.0000000,0.3163511]
  cl(0:1, 25 ) = [-105.0000000,0.2651242]
  cl(0:1, 26 ) = [-102.0000000,0.2138973]
  cl(0:1, 27 ) = [-99.0000000,0.1617714]
  cl(0:1, 28 ) = [-96.0000000,0.1078475]
  cl(0:1, 29 ) = [-93.0000000,0.0539238]
  cl(0:1, 30 ) = [-90.0000000,0.0000003]
  cl(0:1, 31 ) = [-87.0000000,-0.0539238]
  cl(0:1, 32 ) = [-84.0000000,-0.1078476]
  cl(0:1, 33 ) = [-81.0000000,-0.1617710]
  cl(0:1, 34 ) = [-78.0000000,-0.2138971]
  cl(0:1, 35 ) = [-75.0000000,-0.2651245]
  cl(0:1, 36 ) = [-72.0000000,-0.3163508]
  cl(0:1, 37 ) = [-69.0000000,-0.3649678]
  cl(0:1, 38 ) = [-66.0000000,-0.4083658]
  cl(0:1, 39 ) = [-63.0000000,-0.4517639]
  cl(0:1, 40 ) = [-60.0000000,-0.4951619]
  cl(0:1, 41 ) = [-57.0000000,-0.5275422]
  cl(0:1, 42 ) = [-54.0000000,-0.5599224]
  cl(0:1, 43 ) = [-51.0000000,-0.5923025]
  cl(0:1, 44 ) = [-48.0000000,-0.6189064]
  cl(0:1, 45 ) = [-45.0000000,-0.6426221]
  cl(0:1, 46 ) = [-42.0000000,-0.6613991]
  cl(0:1, 47 ) = [-39.0000000,-0.6792952]
  cl(0:1, 48 ) = [-36.0000000,-0.6947777]
  cl(0:1, 49 ) = [-33.0000000,-0.7094677]
  cl(0:1, 50 ) = [-30.0000000,-0.7248017]
  cl(0:1, 51 ) = [-29.3939394,-0.7283265]
  cl(0:1, 52 ) = [-28.7878788,-0.7318513]
  cl(0:1, 53 ) = [-28.1818182,-0.7353762]
  cl(0:1, 54 ) = [-27.5757576,-0.7393354]
  cl(0:1, 55 ) = [-26.9696970,-0.7434809]
  cl(0:1, 56 ) = [-26.3636364,-0.7476264]
  cl(0:1, 57 ) = [-25.7575758,-0.7521518]
  cl(0:1, 58 ) = [-25.1515152,-0.7572473]
  cl(0:1, 59 ) = [-24.5454545,-0.7623427]
  cl(0:1, 60 ) = [-23.9393939,-0.7676188]
  cl(0:1, 61 ) = [-23.3333333,-0.7745214]
  cl(0:1, 62 ) = [-22.7272727,-0.7814241]
  cl(0:1, 63 ) = [-22.1212121,-0.7883267]
  cl(0:1, 64 ) = [-21.5151515,-0.7956220]
  cl(0:1, 65 ) = [-20.9090909,-0.8026047]
  cl(0:1, 66 ) = [-20.3030303,-0.8096503]
  cl(0:1, 67 ) = [-19.6969697,-0.8176375]
  cl(0:1, 68 ) = [-19.0909091,-0.8264770]
  cl(0:1, 69 ) = [-18.4848485,-0.8260848]
  cl(0:1, 70 ) = [-17.8787879,-0.8239835]
  cl(0:1, 71 ) = [-17.2727273,-0.8215701]
  cl(0:1, 72 ) = [-16.6666667,-0.8124440]
  cl(0:1, 73 ) = [-16.0606061,-0.7978443]
  cl(0:1, 74 ) = [-15.4545455,-0.7844224]
  cl(0:1, 75 ) = [-14.8484848,-0.7717876]
  cl(0:1, 76 ) = [-14.2424242,-0.7611350]
  cl(0:1, 77 ) = [-13.6363636,-0.7510190]
  cl(0:1, 78 ) = [-13.0303030,-0.7414211]
  cl(0:1, 79 ) = [-12.4242424,-0.7261171]
  cl(0:1, 80 ) = [-11.8181818,-0.7094504]
  cl(0:1, 81 ) = [-11.2121212,-0.6900826]
  cl(0:1, 82 ) = [-10.6060606,-0.6690068]
  cl(0:1, 83 ) = [-10.0000000,-0.6469737]
  cl(0:1, 84 ) = [-9.3939394,-0.6235665]
  cl(0:1, 85 ) = [-8.7878788,-0.5931439]
  cl(0:1, 86 ) = [-8.1818182,-0.5494371]
  cl(0:1, 87 ) = [-7.5757576,-0.4761296]
  cl(0:1, 88 ) = [-6.9696970,-0.3899235]
  cl(0:1, 89 ) = [-6.3636364,-0.3122705]
  cl(0:1, 90 ) = [-5.7575758,-0.2352445]
  cl(0:1, 91 ) = [-5.1515152,-0.1586110]
  cl(0:1, 92 ) = [-4.5454545,-0.0826265]
  cl(0:1, 93 ) = [-3.9393939,-0.0071751]
  cl(0:1, 94 ) = [-3.3333333,0.0677399]
  cl(0:1, 95 ) = [-2.7272727,0.1426704]
  cl(0:1, 96 ) = [-2.1212121,0.2171398]
  cl(0:1, 97 ) = [-1.5151515,0.2914287]
  cl(0:1, 98 ) = [-0.9090909,0.3651289]
  cl(0:1, 99 ) = [-0.3030303,0.4381051]
  cl(0:1, 100) = [0.3030303,0.5111121]
  cl(0:1, 101) = [0.9090909,0.5840483]
  cl(0:1, 102) = [1.5151515,0.6567712]
  cl(0:1, 103) = [2.1212121,0.7294314]
  cl(0:1, 104) = [2.7272727,0.8017151]
  cl(0:1, 105) = [3.3333333,0.8736677]
  cl(0:1, 106) = [3.9393939,0.9452038]
  cl(0:1, 107) = [4.5454545,1.0161626]
  cl(0:1, 108) = [5.1515152,1.0866650]
  cl(0:1, 109) = [5.7575758,1.1558971]
  cl(0:1, 110) = [6.3636364,1.2223514]
  cl(0:1, 111) = [6.9696970,1.2838173]
  cl(0:1, 112) = [7.5757576,1.3341482]
  cl(0:1, 113) = [8.1818182,1.3792153]
  cl(0:1, 114) = [8.7878788,1.4192960]
  cl(0:1, 115) = [9.3939394,1.4489518]
  cl(0:1, 116) = [10.0000000,1.4656206]
  cl(0:1, 117) = [10.6060606,1.4648228]
  cl(0:1, 118) = [11.2121212,1.4645501]
  cl(0:1, 119) = [11.8181818,1.4678748]
  cl(0:1, 120) = [12.4242424,1.4462680]
  cl(0:1, 121) = [13.0303030,1.4154846]
  cl(0:1, 122) = [13.6363636,1.3724505]
  cl(0:1, 123) = [14.2424242,1.3353784]
  cl(0:1, 124) = [14.8484848,1.3061366]
  cl(0:1, 125) = [15.4545455,1.2855330]
  cl(0:1, 126) = [16.0606061,1.2675842]
  cl(0:1, 127) = [16.6666667,1.2477159]
  cl(0:1, 128) = [17.2727273,1.2241769]
  cl(0:1, 129) = [17.8787879,1.1960831]
  cl(0:1, 130) = [18.4848485,1.1825548]
  cl(0:1, 131) = [19.0909091,1.1724274]
  cl(0:1, 132) = [19.6969697,1.1608258]
  cl(0:1, 133) = [20.3030303,1.1490432]
  cl(0:1, 134) = [20.9090909,1.1395414]
  cl(0:1, 135) = [21.5151515,1.1321581]
  cl(0:1, 136) = [22.1212121,1.1232159]
  cl(0:1, 137) = [22.7272727,1.1143112]
  cl(0:1, 138) = [23.3333333,1.1054065]
  cl(0:1, 139) = [23.9393939,1.0965019]
  cl(0:1, 140) = [24.5454545,1.0890596]
  cl(0:1, 141) = [25.1515152,1.0817798]
  cl(0:1, 142) = [25.7575758,1.0744999]
  cl(0:1, 143) = [26.3636364,1.0680351]
  cl(0:1, 144) = [26.9696970,1.0621137]
  cl(0:1, 145) = [27.5757576,1.0561924]
  cl(0:1, 146) = [28.1818182,1.0505368]
  cl(0:1, 147) = [28.7878788,1.0455013]
  cl(0:1, 148) = [29.3939394,1.0404659]
  cl(0:1, 149) = [30.0000000,1.0354305]
  cl(0:1, 150) = [33.0000000,1.0135278]
  cl(0:1, 151) = [36.0000000,0.9925390]
  cl(0:1, 152) = [39.0000000,0.9704217]
  cl(0:1, 153) = [42.0000000,0.9448557]
  cl(0:1, 154) = [45.0000000,0.9180320]
  cl(0:1, 155) = [48.0000000,0.8841521]
  cl(0:1, 156) = [51.0000000,0.8461464]
  cl(0:1, 157) = [54.0000000,0.7998889]
  cl(0:1, 158) = [57.0000000,0.7536313]
  cl(0:1, 159) = [60.0000000,0.7073737]
  cl(0:1, 160) = [63.0000000,0.6453771]
  cl(0:1, 161) = [66.0000000,0.5833802]
  cl(0:1, 162) = [69.0000000,0.5213830]
  cl(0:1, 163) = [72.0000000,0.4519300]
  cl(0:1, 164) = [75.0000000,0.3787491]
  cl(0:1, 165) = [78.0000000,0.3055673]
  cl(0:1, 166) = [81.0000000,0.2311017]
  cl(0:1, 167) = [84.0000000,0.1540683]
  cl(0:1, 168) = [87.0000000,0.0770342]
  cl(0:1, 169) = [90.0000000,-0.0000003]
  cl(0:1, 170) = [93.0000000,-0.0539238]
  cl(0:1, 171) = [96.0000000,-0.1078475]
  cl(0:1, 172) = [99.0000000,-0.1617714]
  cl(0:1, 173) = [102.0000000,-0.2138973]
  cl(0:1, 174) = [105.0000000,-0.2651242]
  cl(0:1, 175) = [108.0000000,-0.3163511]
  cl(0:1, 176) = [111.0000000,-0.3649681]
  cl(0:1, 177) = [114.0000000,-0.4083657]
  cl(0:1, 178) = [117.0000000,-0.4517636]
  cl(0:1, 179) = [120.0000000,-0.4951617]
  cl(0:1, 180) = [123.0000000,-0.5275420]
  cl(0:1, 181) = [126.0000000,-0.5599222]
  cl(0:1, 182) = [129.0000000,-0.5923023]
  cl(0:1, 183) = [132.0000000,-0.6172600]
  cl(0:1, 184) = [135.0000000,-0.6385065]
  cl(0:1, 185) = [138.0000000,-0.6597527]
  cl(0:1, 186) = [141.0000000,-0.6790054]
  cl(0:1, 187) = [144.0000000,-0.6942710]
  cl(0:1, 188) = [147.0000000,-0.7095363]
  cl(0:1, 189) = [150.0000000,-0.7248016]
  cl(0:1, 190) = [153.0000000,-0.7425998]
  cl(0:1, 191) = [156.0000000,-0.7599481]
  cl(0:1, 192) = [159.0000000,-0.7772313]
  cl(0:1, 193) = [162.0000000,-0.7149923]
  cl(0:1, 194) = [165.0000000,-0.6128950]
  cl(0:1, 195) = [168.0000000,-0.5107983]
  cl(0:1, 196) = [171.0000000,-0.3984605]
  cl(0:1, 197) = [174.0000000,-0.2656400]
  cl(0:1, 198) = [177.0000000,-0.1322145]
  cl(0:1, 199) = [180.0000000,0.0000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl25_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl25 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl25_ge28.pro'
  ENDIF

!print,y,cl25

!_________________________________________________________________________
  
END FUNCTION calc_cl25_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl26_ge28( y ) RESULT( cl26 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl26
  INTEGER :: i

  cl26 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.1470851]
  cl(0:1, 2  ) = [-174.0000000,0.2955176]
  cl(0:1, 3  ) = [-171.0000000,0.4432773]
  cl(0:1, 4  ) = [-168.0000000,0.5547752]
  cl(0:1, 5  ) = [-165.0000000,0.6481427]
  cl(0:1, 6  ) = [-162.0000000,0.7415112]
  cl(0:1, 7  ) = [-159.0000000,0.7955401]
  cl(0:1, 8  ) = [-156.0000000,0.7709065]
  cl(0:1, 9  ) = [-153.0000000,0.7462625]
  cl(0:1, 10 ) = [-150.0000000,0.7215930]
  cl(0:1, 11 ) = [-147.0000000,0.7067044]
  cl(0:1, 12 ) = [-144.0000000,0.6918158]
  cl(0:1, 13 ) = [-141.0000000,0.6769270]
  cl(0:1, 14 ) = [-138.0000000,0.6579597]
  cl(0:1, 15 ) = [-135.0000000,0.6369533]
  cl(0:1, 16 ) = [-132.0000000,0.6159465]
  cl(0:1, 17 ) = [-129.0000000,0.5912023]
  cl(0:1, 18 ) = [-126.0000000,0.5589831]
  cl(0:1, 19 ) = [-123.0000000,0.5267637]
  cl(0:1, 20 ) = [-120.0000000,0.4945443]
  cl(0:1, 21 ) = [-117.0000000,0.4512516]
  cl(0:1, 22 ) = [-114.0000000,0.4079589]
  cl(0:1, 23 ) = [-111.0000000,0.3646666]
  cl(0:1, 24 ) = [-108.0000000,0.3161248]
  cl(0:1, 25 ) = [-105.0000000,0.2649582]
  cl(0:1, 26 ) = [-102.0000000,0.2137916]
  cl(0:1, 27 ) = [-99.0000000,0.1617124]
  cl(0:1, 28 ) = [-96.0000000,0.1078081]
  cl(0:1, 29 ) = [-93.0000000,0.0539041]
  cl(0:1, 30 ) = [-90.0000000,0.0000003]
  cl(0:1, 31 ) = [-87.0000000,-0.0539040]
  cl(0:1, 32 ) = [-84.0000000,-0.1078082]
  cl(0:1, 33 ) = [-81.0000000,-0.1617119]
  cl(0:1, 34 ) = [-78.0000000,-0.2137913]
  cl(0:1, 35 ) = [-75.0000000,-0.2649585]
  cl(0:1, 36 ) = [-72.0000000,-0.3161246]
  cl(0:1, 37 ) = [-69.0000000,-0.3646662]
  cl(0:1, 38 ) = [-66.0000000,-0.4079590]
  cl(0:1, 39 ) = [-63.0000000,-0.4512518]
  cl(0:1, 40 ) = [-60.0000000,-0.4945445]
  cl(0:1, 41 ) = [-57.0000000,-0.5267640]
  cl(0:1, 42 ) = [-54.0000000,-0.5589833]
  cl(0:1, 43 ) = [-51.0000000,-0.5912025]
  cl(0:1, 44 ) = [-48.0000000,-0.6176092]
  cl(0:1, 45 ) = [-45.0000000,-0.6411097]
  cl(0:1, 46 ) = [-42.0000000,-0.6596224]
  cl(0:1, 47 ) = [-39.0000000,-0.6772401]
  cl(0:1, 48 ) = [-36.0000000,-0.6923960]
  cl(0:1, 49 ) = [-33.0000000,-0.7067017]
  cl(0:1, 50 ) = [-30.0000000,-0.7215931]
  cl(0:1, 51 ) = [-29.3939394,-0.7250139]
  cl(0:1, 52 ) = [-28.7878788,-0.7284346]
  cl(0:1, 53 ) = [-28.1818182,-0.7318553]
  cl(0:1, 54 ) = [-27.5757576,-0.7357007]
  cl(0:1, 55 ) = [-26.9696970,-0.7397281]
  cl(0:1, 56 ) = [-26.3636364,-0.7437554]
  cl(0:1, 57 ) = [-25.7575758,-0.7481559]
  cl(0:1, 58 ) = [-25.1515152,-0.7531158]
  cl(0:1, 59 ) = [-24.5454545,-0.7580758]
  cl(0:1, 60 ) = [-23.9393939,-0.7631844]
  cl(0:1, 61 ) = [-23.3333333,-0.7696309]
  cl(0:1, 62 ) = [-22.7272727,-0.7760774]
  cl(0:1, 63 ) = [-22.1212121,-0.7825239]
  cl(0:1, 64 ) = [-21.5151515,-0.7901353]
  cl(0:1, 65 ) = [-20.9090909,-0.7980043]
  cl(0:1, 66 ) = [-20.3030303,-0.8058785]
  cl(0:1, 67 ) = [-19.6969697,-0.8146979]
  cl(0:1, 68 ) = [-19.0909091,-0.8244553]
  cl(0:1, 69 ) = [-18.4848485,-0.8233715]
  cl(0:1, 70 ) = [-17.8787879,-0.8202831]
  cl(0:1, 71 ) = [-17.2727273,-0.8168277]
  cl(0:1, 72 ) = [-16.6666667,-0.8054876]
  cl(0:1, 73 ) = [-16.0606061,-0.7876981]
  cl(0:1, 74 ) = [-15.4545455,-0.7702792]
  cl(0:1, 75 ) = [-14.8484848,-0.7530953]
  cl(0:1, 76 ) = [-14.2424242,-0.7364943]
  cl(0:1, 77 ) = [-13.6363636,-0.7200032]
  cl(0:1, 78 ) = [-13.0303030,-0.7035964]
  cl(0:1, 79 ) = [-12.4242424,-0.6855921]
  cl(0:1, 80 ) = [-11.8181818,-0.6671948]
  cl(0:1, 81 ) = [-11.2121212,-0.6480638]
  cl(0:1, 82 ) = [-10.6060606,-0.6284683]
  cl(0:1, 83 ) = [-10.0000000,-0.6086205]
  cl(0:1, 84 ) = [-9.3939394,-0.5884523]
  cl(0:1, 85 ) = [-8.7878788,-0.5626899]
  cl(0:1, 86 ) = [-8.1818182,-0.5265238]
  cl(0:1, 87 ) = [-7.5757576,-0.4556953]
  cl(0:1, 88 ) = [-6.9696970,-0.3698455]
  cl(0:1, 89 ) = [-6.3636364,-0.2935987]
  cl(0:1, 90 ) = [-5.7575758,-0.2181053]
  cl(0:1, 91 ) = [-5.1515152,-0.1430864]
  cl(0:1, 92 ) = [-4.5454545,-0.0685130]
  cl(0:1, 93 ) = [-3.9393939,0.0055839]
  cl(0:1, 94 ) = [-3.3333333,0.0794070]
  cl(0:1, 95 ) = [-2.7272727,0.1532410]
  cl(0:1, 96 ) = [-2.1212121,0.2266439]
  cl(0:1, 97 ) = [-1.5151515,0.3001054]
  cl(0:1, 98 ) = [-0.9090909,0.3731128]
  cl(0:1, 99 ) = [-0.3030303,0.4458216]
  cl(0:1, 100) = [0.3030303,0.5184903]
  cl(0:1, 101) = [0.9090909,0.5909812]
  cl(0:1, 102) = [1.5151515,0.6631941]
  cl(0:1, 103) = [2.1212121,0.7353426]
  cl(0:1, 104) = [2.7272727,0.8070875]
  cl(0:1, 105) = [3.3333333,0.8786539]
  cl(0:1, 106) = [3.9393939,0.9498218]
  cl(0:1, 107) = [4.5454545,1.0204086]
  cl(0:1, 108) = [5.1515152,1.0905743]
  cl(0:1, 109) = [5.7575758,1.1595699]
  cl(0:1, 110) = [6.3636364,1.2259823]
  cl(0:1, 111) = [6.9696970,1.2869821]
  cl(0:1, 112) = [7.5757576,1.3379808]
  cl(0:1, 113) = [8.1818182,1.3847449]
  cl(0:1, 114) = [8.7878788,1.4303057]
  cl(0:1, 115) = [9.3939394,1.4720049]
  cl(0:1, 116) = [10.0000000,1.5021510]
  cl(0:1, 117) = [10.6060606,1.5237065]
  cl(0:1, 118) = [11.2121212,1.5405932]
  cl(0:1, 119) = [11.8181818,1.5519506]
  cl(0:1, 120) = [12.4242424,1.5243890]
  cl(0:1, 121) = [13.0303030,1.4820286]
  cl(0:1, 122) = [13.6363636,1.4263473]
  cl(0:1, 123) = [14.2424242,1.3774093]
  cl(0:1, 124) = [14.8484848,1.3384876]
  cl(0:1, 125) = [15.4545455,1.3119076]
  cl(0:1, 126) = [16.0606061,1.2893824]
  cl(0:1, 127) = [16.6666667,1.2663402]
  cl(0:1, 128) = [17.2727273,1.2383410]
  cl(0:1, 129) = [17.8787879,1.2042777]
  cl(0:1, 130) = [18.4848485,1.1879932]
  cl(0:1, 131) = [19.0909091,1.1758680]
  cl(0:1, 132) = [19.6969697,1.1621189]
  cl(0:1, 133) = [20.3030303,1.1492741]
  cl(0:1, 134) = [20.9090909,1.1381424]
  cl(0:1, 135) = [21.5151515,1.1276632]
  cl(0:1, 136) = [22.1212121,1.1170618]
  cl(0:1, 137) = [22.7272727,1.1081196]
  cl(0:1, 138) = [23.3333333,1.0991775]
  cl(0:1, 139) = [23.9393939,1.0902354]
  cl(0:1, 140) = [24.5454545,1.0829634]
  cl(0:1, 141) = [25.1515152,1.0758769]
  cl(0:1, 142) = [25.7575758,1.0687905]
  cl(0:1, 143) = [26.3636364,1.0625043]
  cl(0:1, 144) = [26.9696970,1.0567518]
  cl(0:1, 145) = [27.5757576,1.0509992]
  cl(0:1, 146) = [28.1818182,1.0455064]
  cl(0:1, 147) = [28.7878788,1.0406196]
  cl(0:1, 148) = [29.3939394,1.0357329]
  cl(0:1, 149) = [30.0000000,1.0308461]
  cl(0:1, 150) = [33.0000000,1.0095772]
  cl(0:1, 151) = [36.0000000,0.9891368]
  cl(0:1, 152) = [39.0000000,0.9674859]
  cl(0:1, 153) = [42.0000000,0.9423176]
  cl(0:1, 154) = [45.0000000,0.9158713]
  cl(0:1, 155) = [48.0000000,0.8822989]
  cl(0:1, 156) = [51.0000000,0.8445749]
  cl(0:1, 157) = [54.0000000,0.7985472]
  cl(0:1, 158) = [57.0000000,0.7525194]
  cl(0:1, 159) = [60.0000000,0.7064917]
  cl(0:1, 160) = [63.0000000,0.6446456]
  cl(0:1, 161) = [66.0000000,0.5827992]
  cl(0:1, 162) = [69.0000000,0.5209524]
  cl(0:1, 163) = [72.0000000,0.4516068]
  cl(0:1, 164) = [75.0000000,0.3785120]
  cl(0:1, 165) = [78.0000000,0.3054164]
  cl(0:1, 166) = [81.0000000,0.2310175]
  cl(0:1, 167) = [84.0000000,0.1540123]
  cl(0:1, 168) = [87.0000000,0.0770061]
  cl(0:1, 169) = [90.0000000,-0.0000003]
  cl(0:1, 170) = [93.0000000,-0.0539041]
  cl(0:1, 171) = [96.0000000,-0.1078081]
  cl(0:1, 172) = [99.0000000,-0.1617124]
  cl(0:1, 173) = [102.0000000,-0.2137916]
  cl(0:1, 174) = [105.0000000,-0.2649582]
  cl(0:1, 175) = [108.0000000,-0.3161248]
  cl(0:1, 176) = [111.0000000,-0.3646666]
  cl(0:1, 177) = [114.0000000,-0.4079589]
  cl(0:1, 178) = [117.0000000,-0.4512516]
  cl(0:1, 179) = [120.0000000,-0.4945443]
  cl(0:1, 180) = [123.0000000,-0.5267637]
  cl(0:1, 181) = [126.0000000,-0.5589831]
  cl(0:1, 182) = [129.0000000,-0.5912023]
  cl(0:1, 183) = [132.0000000,-0.6159465]
  cl(0:1, 184) = [135.0000000,-0.6369533]
  cl(0:1, 185) = [138.0000000,-0.6579597]
  cl(0:1, 186) = [141.0000000,-0.6769270]
  cl(0:1, 187) = [144.0000000,-0.6918158]
  cl(0:1, 188) = [147.0000000,-0.7067044]
  cl(0:1, 189) = [150.0000000,-0.7215930]
  cl(0:1, 190) = [153.0000000,-0.7456536]
  cl(0:1, 191) = [156.0000000,-0.7696771]
  cl(0:1, 192) = [159.0000000,-0.7936952]
  cl(0:1, 193) = [162.0000000,-0.7338171]
  cl(0:1, 194) = [165.0000000,-0.6319823]
  cl(0:1, 195) = [168.0000000,-0.5301481]
  cl(0:1, 196) = [171.0000000,-0.4160329]
  cl(0:1, 197) = [174.0000000,-0.2773550]
  cl(0:1, 198) = [177.0000000,-0.1380452]
  cl(0:1, 199) = [180.0000000,0.0000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl26_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl26 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl26_ge28.pro'
  ENDIF

!print,y,cl26

!_________________________________________________________________________
  
END FUNCTION calc_cl26_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl27_ge28( y ) RESULT( cl27 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl27
  INTEGER :: i

  cl27 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.1494717]
  cl(0:1, 2  ) = [-174.0000000,0.3003128]
  cl(0:1, 3  ) = [-171.0000000,0.4504702]
  cl(0:1, 4  ) = [-168.0000000,0.5621412]
  cl(0:1, 5  ) = [-165.0000000,0.6545694]
  cl(0:1, 6  ) = [-162.0000000,0.7469987]
  cl(0:1, 7  ) = [-159.0000000,0.7997869]
  cl(0:1, 8  ) = [-156.0000000,0.7732941]
  cl(0:1, 9  ) = [-153.0000000,0.7468011]
  cl(0:1, 10 ) = [-150.0000000,0.7203080]
  cl(0:1, 11 ) = [-147.0000000,0.7055703]
  cl(0:1, 12 ) = [-144.0000000,0.6908325]
  cl(0:1, 13 ) = [-141.0000000,0.6760946]
  cl(0:1, 14 ) = [-138.0000000,0.6572417]
  cl(0:1, 15 ) = [-135.0000000,0.6363312]
  cl(0:1, 16 ) = [-132.0000000,0.6154205]
  cl(0:1, 17 ) = [-129.0000000,0.5907618]
  cl(0:1, 18 ) = [-126.0000000,0.5586070]
  cl(0:1, 19 ) = [-123.0000000,0.5264521]
  cl(0:1, 20 ) = [-120.0000000,0.4942971]
  cl(0:1, 21 ) = [-117.0000000,0.4510465]
  cl(0:1, 22 ) = [-114.0000000,0.4077960]
  cl(0:1, 23 ) = [-111.0000000,0.3645459]
  cl(0:1, 24 ) = [-108.0000000,0.3160342]
  cl(0:1, 25 ) = [-105.0000000,0.2648917]
  cl(0:1, 26 ) = [-102.0000000,0.2137492]
  cl(0:1, 27 ) = [-99.0000000,0.1616888]
  cl(0:1, 28 ) = [-96.0000000,0.1077923]
  cl(0:1, 29 ) = [-93.0000000,0.0538962]
  cl(0:1, 30 ) = [-90.0000000,0.0000003]
  cl(0:1, 31 ) = [-87.0000000,-0.0538961]
  cl(0:1, 32 ) = [-84.0000000,-0.1077924]
  cl(0:1, 33 ) = [-81.0000000,-0.1616883]
  cl(0:1, 34 ) = [-78.0000000,-0.2137490]
  cl(0:1, 35 ) = [-75.0000000,-0.2648921]
  cl(0:1, 36 ) = [-72.0000000,-0.3160339]
  cl(0:1, 37 ) = [-69.0000000,-0.3645455]
  cl(0:1, 38 ) = [-66.0000000,-0.4077961]
  cl(0:1, 39 ) = [-63.0000000,-0.4510467]
  cl(0:1, 40 ) = [-60.0000000,-0.4942973]
  cl(0:1, 41 ) = [-57.0000000,-0.5264523]
  cl(0:1, 42 ) = [-54.0000000,-0.5586072]
  cl(0:1, 43 ) = [-51.0000000,-0.5907620]
  cl(0:1, 44 ) = [-48.0000000,-0.6170897]
  cl(0:1, 45 ) = [-45.0000000,-0.6405039]
  cl(0:1, 46 ) = [-42.0000000,-0.6589109]
  cl(0:1, 47 ) = [-39.0000000,-0.6764170]
  cl(0:1, 48 ) = [-36.0000000,-0.6914421]
  cl(0:1, 49 ) = [-33.0000000,-0.7055940]
  cl(0:1, 50 ) = [-30.0000000,-0.7203081]
  cl(0:1, 51 ) = [-29.3939394,-0.7236871]
  cl(0:1, 52 ) = [-28.7878788,-0.7270662]
  cl(0:1, 53 ) = [-28.1818182,-0.7304453]
  cl(0:1, 54 ) = [-27.5757576,-0.7342450]
  cl(0:1, 55 ) = [-26.9696970,-0.7382251]
  cl(0:1, 56 ) = [-26.3636364,-0.7422051]
  cl(0:1, 57 ) = [-25.7575758,-0.7465555]
  cl(0:1, 58 ) = [-25.1515152,-0.7514612]
  cl(0:1, 59 ) = [-24.5454545,-0.7563669]
  cl(0:1, 60 ) = [-23.9393939,-0.7614087]
  cl(0:1, 61 ) = [-23.3333333,-0.7676756]
  cl(0:1, 62 ) = [-22.7272727,-0.7739425]
  cl(0:1, 63 ) = [-22.1212121,-0.7802094]
  cl(0:1, 64 ) = [-21.5151515,-0.7880458]
  cl(0:1, 65 ) = [-20.9090909,-0.7962748]
  cl(0:1, 66 ) = [-20.3030303,-0.8045037]
  cl(0:1, 67 ) = [-19.6969697,-0.8137216]
  cl(0:1, 68 ) = [-19.0909091,-0.8239284]
  cl(0:1, 69 ) = [-18.4848485,-0.8226468]
  cl(0:1, 70 ) = [-17.8787879,-0.8192412]
  cl(0:1, 71 ) = [-17.2727273,-0.8154466]
  cl(0:1, 72 ) = [-16.6666667,-0.8032965]
  cl(0:1, 73 ) = [-16.0606061,-0.7843101]
  cl(0:1, 74 ) = [-15.4545455,-0.7653248]
  cl(0:1, 75 ) = [-14.8484848,-0.7463391]
  cl(0:1, 76 ) = [-14.2424242,-0.7273521]
  cl(0:1, 77 ) = [-13.6363636,-0.7083666]
  cl(0:1, 78 ) = [-13.0303030,-0.6893793]
  cl(0:1, 79 ) = [-12.4242424,-0.6703941]
  cl(0:1, 80 ) = [-11.8181818,-0.6514070]
  cl(0:1, 81 ) = [-11.2121212,-0.6324218]
  cl(0:1, 82 ) = [-10.6060606,-0.6134352]
  cl(0:1, 83 ) = [-10.0000000,-0.5944491]
  cl(0:1, 84 ) = [-9.3939394,-0.5754638]
  cl(0:1, 85 ) = [-8.7878788,-0.5514040]
  cl(0:1, 86 ) = [-8.1818182,-0.5179265]
  cl(0:1, 87 ) = [-7.5757576,-0.4480131]
  cl(0:1, 88 ) = [-6.9696970,-0.3623067]
  cl(0:1, 89 ) = [-6.3636364,-0.2865926]
  cl(0:1, 90 ) = [-5.7575758,-0.2116814]
  cl(0:1, 91 ) = [-5.1515152,-0.1372784]
  cl(0:1, 92 ) = [-4.5454545,-0.0632447]
  cl(0:1, 93 ) = [-3.9393939,0.0103326]
  cl(0:1, 94 ) = [-3.3333333,0.0837357]
  cl(0:1, 95 ) = [-2.7272727,0.1571449]
  cl(0:1, 96 ) = [-2.1212121,0.2301320]
  cl(0:1, 97 ) = [-1.5151515,0.3032701]
  cl(0:1, 98 ) = [-0.9090909,0.3760078]
  cl(0:1, 99 ) = [-0.3030303,0.4486196]
  cl(0:1, 100) = [0.3030303,0.5211621]
  cl(0:1, 101) = [0.9090909,0.5934830]
  cl(0:1, 102) = [1.5151515,0.6654995]
  cl(0:1, 103) = [2.1212121,0.7374497]
  cl(0:1, 104) = [2.7272727,0.8089823]
  cl(0:1, 105) = [3.3333333,0.8803975]
  cl(0:1, 106) = [3.9393939,0.9514218]
  cl(0:1, 107) = [4.5454545,1.0218628]
  cl(0:1, 108) = [5.1515152,1.0918963]
  cl(0:1, 109) = [5.7575758,1.1607995]
  cl(0:1, 110) = [6.3636364,1.2272014]
  cl(0:1, 111) = [6.9696970,1.2880246]
  cl(0:1, 112) = [7.5757576,1.3393233]
  cl(0:1, 113) = [8.1818182,1.3867918]
  cl(0:1, 114) = [8.7878788,1.4344262]
  cl(0:1, 115) = [9.3939394,1.4807247]
  cl(0:1, 116) = [10.0000000,1.5161995]
  cl(0:1, 117) = [10.6060606,1.5465261]
  cl(0:1, 118) = [11.2121212,1.5701511]
  cl(0:1, 119) = [11.8181818,1.5846627]
  cl(0:1, 120) = [12.4242424,1.5543901]
  cl(0:1, 121) = [13.0303030,1.5069701]
  cl(0:1, 122) = [13.6363636,1.4463612]
  cl(0:1, 123) = [14.2424242,1.3930293]
  cl(0:1, 124) = [14.8484848,1.3506044]
  cl(0:1, 125) = [15.4545455,1.3218178]
  cl(0:1, 126) = [16.0606061,1.2975763]
  cl(0:1, 127) = [16.6666667,1.2733335]
  cl(0:1, 128) = [17.2727273,1.2436361]
  cl(0:1, 129) = [17.8787879,1.2072719]
  cl(0:1, 130) = [18.4848485,1.1899298]
  cl(0:1, 131) = [19.0909091,1.1770433]
  cl(0:1, 132) = [19.6969697,1.1624612]
  cl(0:1, 133) = [20.3030303,1.1492928]
  cl(0:1, 134) = [20.9090909,1.1375381]
  cl(0:1, 135) = [21.5151515,1.1257833]
  cl(0:1, 136) = [22.1212121,1.1145888]
  cl(0:1, 137) = [22.7272727,1.1056344]
  cl(0:1, 138) = [23.3333333,1.0966799]
  cl(0:1, 139) = [23.9393939,1.0877255]
  cl(0:1, 140) = [24.5454545,1.0805219]
  cl(0:1, 141) = [25.1515152,1.0735129]
  cl(0:1, 142) = [25.7575758,1.0665039]
  cl(0:1, 143) = [26.3636364,1.0602893]
  cl(0:1, 144) = [26.9696970,1.0546043]
  cl(0:1, 145) = [27.5757576,1.0489194]
  cl(0:1, 146) = [28.1818182,1.0434918]
  cl(0:1, 147) = [28.7878788,1.0386646]
  cl(0:1, 148) = [29.3939394,1.0338374]
  cl(0:1, 149) = [30.0000000,1.0290101]
  cl(0:1, 150) = [33.0000000,1.0079950]
  cl(0:1, 151) = [36.0000000,0.9877742]
  cl(0:1, 152) = [39.0000000,0.9663101]
  cl(0:1, 153) = [42.0000000,0.9413011]
  cl(0:1, 154) = [45.0000000,0.9150059]
  cl(0:1, 155) = [48.0000000,0.8815567]
  cl(0:1, 156) = [51.0000000,0.8439455]
  cl(0:1, 157) = [54.0000000,0.7980098]
  cl(0:1, 158) = [57.0000000,0.7520741]
  cl(0:1, 159) = [60.0000000,0.7061384]
  cl(0:1, 160) = [63.0000000,0.6443526]
  cl(0:1, 161) = [66.0000000,0.5825665]
  cl(0:1, 162) = [69.0000000,0.5207800]
  cl(0:1, 163) = [72.0000000,0.4514774]
  cl(0:1, 164) = [75.0000000,0.3784171]
  cl(0:1, 165) = [78.0000000,0.3053559]
  cl(0:1, 166) = [81.0000000,0.2309838]
  cl(0:1, 167) = [84.0000000,0.1539898]
  cl(0:1, 168) = [87.0000000,0.0769949]
  cl(0:1, 169) = [90.0000000,-0.0000003]
  cl(0:1, 170) = [93.0000000,-0.0538962]
  cl(0:1, 171) = [96.0000000,-0.1077923]
  cl(0:1, 172) = [99.0000000,-0.1616888]
  cl(0:1, 173) = [102.0000000,-0.2137492]
  cl(0:1, 174) = [105.0000000,-0.2648917]
  cl(0:1, 175) = [108.0000000,-0.3160342]
  cl(0:1, 176) = [111.0000000,-0.3645459]
  cl(0:1, 177) = [114.0000000,-0.4077960]
  cl(0:1, 178) = [117.0000000,-0.4510465]
  cl(0:1, 179) = [120.0000000,-0.4942971]
  cl(0:1, 180) = [123.0000000,-0.5264521]
  cl(0:1, 181) = [126.0000000,-0.5586070]
  cl(0:1, 182) = [129.0000000,-0.5907618]
  cl(0:1, 183) = [132.0000000,-0.6154205]
  cl(0:1, 184) = [135.0000000,-0.6363312]
  cl(0:1, 185) = [138.0000000,-0.6572417]
  cl(0:1, 186) = [141.0000000,-0.6760946]
  cl(0:1, 187) = [144.0000000,-0.6908325]
  cl(0:1, 188) = [147.0000000,-0.7055703]
  cl(0:1, 189) = [150.0000000,-0.7203080]
  cl(0:1, 190) = [153.0000000,-0.7468011]
  cl(0:1, 191) = [156.0000000,-0.7732940]
  cl(0:1, 192) = [159.0000000,-0.7997868]
  cl(0:1, 193) = [162.0000000,-0.7407907]
  cl(0:1, 194) = [165.0000000,-0.6390493]
  cl(0:1, 195) = [168.0000000,-0.5373086]
  cl(0:1, 196) = [171.0000000,-0.4225334]
  cl(0:1, 197) = [174.0000000,-0.2816887]
  cl(0:1, 198) = [177.0000000,-0.1402022]
  cl(0:1, 199) = [180.0000000,0.0000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl27_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl27 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl27_ge28.pro'
  ENDIF

!print,y,cl27

!_________________________________________________________________________
  
END FUNCTION calc_cl27_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl28_ge28( y ) RESULT( cl28 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl28
  INTEGER :: i

  cl28 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.1494718]
  cl(0:1, 2  ) = [-174.0000000,0.3003129]
  cl(0:1, 3  ) = [-171.0000000,0.4504703]
  cl(0:1, 4  ) = [-168.0000000,0.5621413]
  cl(0:1, 5  ) = [-165.0000000,0.6545695]
  cl(0:1, 6  ) = [-162.0000000,0.7469988]
  cl(0:1, 7  ) = [-159.0000000,0.7997870]
  cl(0:1, 8  ) = [-156.0000000,0.7732941]
  cl(0:1, 9  ) = [-153.0000000,0.7468011]
  cl(0:1, 10 ) = [-150.0000000,0.7203080]
  cl(0:1, 11 ) = [-147.0000000,0.7055703]
  cl(0:1, 12 ) = [-144.0000000,0.6908325]
  cl(0:1, 13 ) = [-141.0000000,0.6760946]
  cl(0:1, 14 ) = [-138.0000000,0.6572417]
  cl(0:1, 15 ) = [-135.0000000,0.6363312]
  cl(0:1, 16 ) = [-132.0000000,0.6154205]
  cl(0:1, 17 ) = [-129.0000000,0.5907618]
  cl(0:1, 18 ) = [-126.0000000,0.5586070]
  cl(0:1, 19 ) = [-123.0000000,0.5264521]
  cl(0:1, 20 ) = [-120.0000000,0.4942971]
  cl(0:1, 21 ) = [-117.0000000,0.4510465]
  cl(0:1, 22 ) = [-114.0000000,0.4077960]
  cl(0:1, 23 ) = [-111.0000000,0.3645459]
  cl(0:1, 24 ) = [-108.0000000,0.3160342]
  cl(0:1, 25 ) = [-105.0000000,0.2648917]
  cl(0:1, 26 ) = [-102.0000000,0.2137492]
  cl(0:1, 27 ) = [-99.0000000,0.1616888]
  cl(0:1, 28 ) = [-96.0000000,0.1077923]
  cl(0:1, 29 ) = [-93.0000000,0.0538962]
  cl(0:1, 30 ) = [-90.0000000,0.0000003]
  cl(0:1, 31 ) = [-87.0000000,-0.0538961]
  cl(0:1, 32 ) = [-84.0000000,-0.1077924]
  cl(0:1, 33 ) = [-81.0000000,-0.1616883]
  cl(0:1, 34 ) = [-78.0000000,-0.2137490]
  cl(0:1, 35 ) = [-75.0000000,-0.2648921]
  cl(0:1, 36 ) = [-72.0000000,-0.3160339]
  cl(0:1, 37 ) = [-69.0000000,-0.3645455]
  cl(0:1, 38 ) = [-66.0000000,-0.4077961]
  cl(0:1, 39 ) = [-63.0000000,-0.4510467]
  cl(0:1, 40 ) = [-60.0000000,-0.4942973]
  cl(0:1, 41 ) = [-57.0000000,-0.5264523]
  cl(0:1, 42 ) = [-54.0000000,-0.5586072]
  cl(0:1, 43 ) = [-51.0000000,-0.5907620]
  cl(0:1, 44 ) = [-48.0000000,-0.6170897]
  cl(0:1, 45 ) = [-45.0000000,-0.6405039]
  cl(0:1, 46 ) = [-42.0000000,-0.6589108]
  cl(0:1, 47 ) = [-39.0000000,-0.6764170]
  cl(0:1, 48 ) = [-36.0000000,-0.6914421]
  cl(0:1, 49 ) = [-33.0000000,-0.7055940]
  cl(0:1, 50 ) = [-30.0000000,-0.7203081]
  cl(0:1, 51 ) = [-29.3939394,-0.7236871]
  cl(0:1, 52 ) = [-28.7878788,-0.7270662]
  cl(0:1, 53 ) = [-28.1818182,-0.7304452]
  cl(0:1, 54 ) = [-27.5757576,-0.7342450]
  cl(0:1, 55 ) = [-26.9696970,-0.7382250]
  cl(0:1, 56 ) = [-26.3636364,-0.7422051]
  cl(0:1, 57 ) = [-25.7575758,-0.7465555]
  cl(0:1, 58 ) = [-25.1515152,-0.7514612]
  cl(0:1, 59 ) = [-24.5454545,-0.7563669]
  cl(0:1, 60 ) = [-23.9393939,-0.7614087]
  cl(0:1, 61 ) = [-23.3333333,-0.7676756]
  cl(0:1, 62 ) = [-22.7272727,-0.7739425]
  cl(0:1, 63 ) = [-22.1212121,-0.7802094]
  cl(0:1, 64 ) = [-21.5151515,-0.7880458]
  cl(0:1, 65 ) = [-20.9090909,-0.7962747]
  cl(0:1, 66 ) = [-20.3030303,-0.8045037]
  cl(0:1, 67 ) = [-19.6969697,-0.8137216]
  cl(0:1, 68 ) = [-19.0909091,-0.8239284]
  cl(0:1, 69 ) = [-18.4848485,-0.8226468]
  cl(0:1, 70 ) = [-17.8787879,-0.8192412]
  cl(0:1, 71 ) = [-17.2727273,-0.8154466]
  cl(0:1, 72 ) = [-16.6666667,-0.8032964]
  cl(0:1, 73 ) = [-16.0606061,-0.7843101]
  cl(0:1, 74 ) = [-15.4545455,-0.7653247]
  cl(0:1, 75 ) = [-14.8484848,-0.7463389]
  cl(0:1, 76 ) = [-14.2424242,-0.7273520]
  cl(0:1, 77 ) = [-13.6363636,-0.7083664]
  cl(0:1, 78 ) = [-13.0303030,-0.6893790]
  cl(0:1, 79 ) = [-12.4242424,-0.6703938]
  cl(0:1, 80 ) = [-11.8181818,-0.6514067]
  cl(0:1, 81 ) = [-11.2121212,-0.6324215]
  cl(0:1, 82 ) = [-10.6060606,-0.6134349]
  cl(0:1, 83 ) = [-10.0000000,-0.5944489]
  cl(0:1, 84 ) = [-9.3939394,-0.5754636]
  cl(0:1, 85 ) = [-8.7878788,-0.5514037]
  cl(0:1, 86 ) = [-8.1818182,-0.5179263]
  cl(0:1, 87 ) = [-7.5757576,-0.4480129]
  cl(0:1, 88 ) = [-6.9696970,-0.3623066]
  cl(0:1, 89 ) = [-6.3636364,-0.2865925]
  cl(0:1, 90 ) = [-5.7575758,-0.2116813]
  cl(0:1, 91 ) = [-5.1515152,-0.1372783]
  cl(0:1, 92 ) = [-4.5454545,-0.0632446]
  cl(0:1, 93 ) = [-3.9393939,0.0103327]
  cl(0:1, 94 ) = [-3.3333333,0.0837358]
  cl(0:1, 95 ) = [-2.7272727,0.1571450]
  cl(0:1, 96 ) = [-2.1212121,0.2301321]
  cl(0:1, 97 ) = [-1.5151515,0.3032702]
  cl(0:1, 98 ) = [-0.9090909,0.3760079]
  cl(0:1, 99 ) = [-0.3030303,0.4486196]
  cl(0:1, 100) = [0.3030303,0.5211622]
  cl(0:1, 101) = [0.9090909,0.5934830]
  cl(0:1, 102) = [1.5151515,0.6654995]
  cl(0:1, 103) = [2.1212121,0.7374498]
  cl(0:1, 104) = [2.7272727,0.8089823]
  cl(0:1, 105) = [3.3333333,0.8803976]
  cl(0:1, 106) = [3.9393939,0.9514218]
  cl(0:1, 107) = [4.5454545,1.0218628]
  cl(0:1, 108) = [5.1515152,1.0918963]
  cl(0:1, 109) = [5.7575758,1.1607995]
  cl(0:1, 110) = [6.3636364,1.2272014]
  cl(0:1, 111) = [6.9696970,1.2880246]
  cl(0:1, 112) = [7.5757576,1.3393233]
  cl(0:1, 113) = [8.1818182,1.3867919]
  cl(0:1, 114) = [8.7878788,1.4344263]
  cl(0:1, 115) = [9.3939394,1.4807249]
  cl(0:1, 116) = [10.0000000,1.5161998]
  cl(0:1, 117) = [10.6060606,1.5465266]
  cl(0:1, 118) = [11.2121212,1.5701517]
  cl(0:1, 119) = [11.8181818,1.5846634]
  cl(0:1, 120) = [12.4242424,1.5543907]
  cl(0:1, 121) = [13.0303030,1.5069706]
  cl(0:1, 122) = [13.6363636,1.4463616]
  cl(0:1, 123) = [14.2424242,1.3930296]
  cl(0:1, 124) = [14.8484848,1.3506047]
  cl(0:1, 125) = [15.4545455,1.3218180]
  cl(0:1, 126) = [16.0606061,1.2975764]
  cl(0:1, 127) = [16.6666667,1.2733336]
  cl(0:1, 128) = [17.2727273,1.2436362]
  cl(0:1, 129) = [17.8787879,1.2072719]
  cl(0:1, 130) = [18.4848485,1.1899298]
  cl(0:1, 131) = [19.0909091,1.1770433]
  cl(0:1, 132) = [19.6969697,1.1624612]
  cl(0:1, 133) = [20.3030303,1.1492928]
  cl(0:1, 134) = [20.9090909,1.1375381]
  cl(0:1, 135) = [21.5151515,1.1257833]
  cl(0:1, 136) = [22.1212121,1.1145887]
  cl(0:1, 137) = [22.7272727,1.1056343]
  cl(0:1, 138) = [23.3333333,1.0966799]
  cl(0:1, 139) = [23.9393939,1.0877254]
  cl(0:1, 140) = [24.5454545,1.0805219]
  cl(0:1, 141) = [25.1515152,1.0735129]
  cl(0:1, 142) = [25.7575758,1.0665039]
  cl(0:1, 143) = [26.3636364,1.0602892]
  cl(0:1, 144) = [26.9696970,1.0546043]
  cl(0:1, 145) = [27.5757576,1.0489194]
  cl(0:1, 146) = [28.1818182,1.0434917]
  cl(0:1, 147) = [28.7878788,1.0386645]
  cl(0:1, 148) = [29.3939394,1.0338373]
  cl(0:1, 149) = [30.0000000,1.0290101]
  cl(0:1, 150) = [33.0000000,1.0079949]
  cl(0:1, 151) = [36.0000000,0.9877742]
  cl(0:1, 152) = [39.0000000,0.9663100]
  cl(0:1, 153) = [42.0000000,0.9413011]
  cl(0:1, 154) = [45.0000000,0.9150059]
  cl(0:1, 155) = [48.0000000,0.8815567]
  cl(0:1, 156) = [51.0000000,0.8439454]
  cl(0:1, 157) = [54.0000000,0.7980098]
  cl(0:1, 158) = [57.0000000,0.7520741]
  cl(0:1, 159) = [60.0000000,0.7061384]
  cl(0:1, 160) = [63.0000000,0.6443526]
  cl(0:1, 161) = [66.0000000,0.5825665]
  cl(0:1, 162) = [69.0000000,0.5207799]
  cl(0:1, 163) = [72.0000000,0.4514774]
  cl(0:1, 164) = [75.0000000,0.3784171]
  cl(0:1, 165) = [78.0000000,0.3053559]
  cl(0:1, 166) = [81.0000000,0.2309838]
  cl(0:1, 167) = [84.0000000,0.1539898]
  cl(0:1, 168) = [87.0000000,0.0769949]
  cl(0:1, 169) = [90.0000000,-0.0000003]
  cl(0:1, 170) = [93.0000000,-0.0538962]
  cl(0:1, 171) = [96.0000000,-0.1077923]
  cl(0:1, 172) = [99.0000000,-0.1616888]
  cl(0:1, 173) = [102.0000000,-0.2137492]
  cl(0:1, 174) = [105.0000000,-0.2648917]
  cl(0:1, 175) = [108.0000000,-0.3160342]
  cl(0:1, 176) = [111.0000000,-0.3645459]
  cl(0:1, 177) = [114.0000000,-0.4077960]
  cl(0:1, 178) = [117.0000000,-0.4510465]
  cl(0:1, 179) = [120.0000000,-0.4942971]
  cl(0:1, 180) = [123.0000000,-0.5264521]
  cl(0:1, 181) = [126.0000000,-0.5586070]
  cl(0:1, 182) = [129.0000000,-0.5907618]
  cl(0:1, 183) = [132.0000000,-0.6154205]
  cl(0:1, 184) = [135.0000000,-0.6363312]
  cl(0:1, 185) = [138.0000000,-0.6572417]
  cl(0:1, 186) = [141.0000000,-0.6760946]
  cl(0:1, 187) = [144.0000000,-0.6908325]
  cl(0:1, 188) = [147.0000000,-0.7055703]
  cl(0:1, 189) = [150.0000000,-0.7203080]
  cl(0:1, 190) = [153.0000000,-0.7468011]
  cl(0:1, 191) = [156.0000000,-0.7732941]
  cl(0:1, 192) = [159.0000000,-0.7997870]
  cl(0:1, 193) = [162.0000000,-0.7407908]
  cl(0:1, 194) = [165.0000000,-0.6390495]
  cl(0:1, 195) = [168.0000000,-0.5373087]
  cl(0:1, 196) = [171.0000000,-0.4225335]
  cl(0:1, 197) = [174.0000000,-0.2816887]
  cl(0:1, 198) = [177.0000000,-0.1402023]
  cl(0:1, 199) = [180.0000000,0.0000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl28_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl28 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl28_ge28.pro'
  ENDIF

!print,y,cl28

!_________________________________________________________________________
  
END FUNCTION calc_cl28_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl29_ge28( y ) RESULT( cl29 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl29
  INTEGER :: i

  cl29 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.1494718]
  cl(0:1, 2  ) = [-174.0000000,0.3003129]
  cl(0:1, 3  ) = [-171.0000000,0.4504703]
  cl(0:1, 4  ) = [-168.0000000,0.5621413]
  cl(0:1, 5  ) = [-165.0000000,0.6545695]
  cl(0:1, 6  ) = [-162.0000000,0.7469988]
  cl(0:1, 7  ) = [-159.0000000,0.7997870]
  cl(0:1, 8  ) = [-156.0000000,0.7732941]
  cl(0:1, 9  ) = [-153.0000000,0.7468011]
  cl(0:1, 10 ) = [-150.0000000,0.7203080]
  cl(0:1, 11 ) = [-147.0000000,0.7055703]
  cl(0:1, 12 ) = [-144.0000000,0.6908325]
  cl(0:1, 13 ) = [-141.0000000,0.6760946]
  cl(0:1, 14 ) = [-138.0000000,0.6572417]
  cl(0:1, 15 ) = [-135.0000000,0.6363312]
  cl(0:1, 16 ) = [-132.0000000,0.6154205]
  cl(0:1, 17 ) = [-129.0000000,0.5907618]
  cl(0:1, 18 ) = [-126.0000000,0.5586070]
  cl(0:1, 19 ) = [-123.0000000,0.5264521]
  cl(0:1, 20 ) = [-120.0000000,0.4942971]
  cl(0:1, 21 ) = [-117.0000000,0.4510465]
  cl(0:1, 22 ) = [-114.0000000,0.4077960]
  cl(0:1, 23 ) = [-111.0000000,0.3645459]
  cl(0:1, 24 ) = [-108.0000000,0.3160342]
  cl(0:1, 25 ) = [-105.0000000,0.2648917]
  cl(0:1, 26 ) = [-102.0000000,0.2137492]
  cl(0:1, 27 ) = [-99.0000000,0.1616888]
  cl(0:1, 28 ) = [-96.0000000,0.1077923]
  cl(0:1, 29 ) = [-93.0000000,0.0538962]
  cl(0:1, 30 ) = [-90.0000000,0.0000003]
  cl(0:1, 31 ) = [-87.0000000,-0.0538961]
  cl(0:1, 32 ) = [-84.0000000,-0.1077924]
  cl(0:1, 33 ) = [-81.0000000,-0.1616883]
  cl(0:1, 34 ) = [-78.0000000,-0.2137490]
  cl(0:1, 35 ) = [-75.0000000,-0.2648921]
  cl(0:1, 36 ) = [-72.0000000,-0.3160339]
  cl(0:1, 37 ) = [-69.0000000,-0.3645455]
  cl(0:1, 38 ) = [-66.0000000,-0.4077961]
  cl(0:1, 39 ) = [-63.0000000,-0.4510467]
  cl(0:1, 40 ) = [-60.0000000,-0.4942973]
  cl(0:1, 41 ) = [-57.0000000,-0.5264523]
  cl(0:1, 42 ) = [-54.0000000,-0.5586072]
  cl(0:1, 43 ) = [-51.0000000,-0.5907620]
  cl(0:1, 44 ) = [-48.0000000,-0.6170897]
  cl(0:1, 45 ) = [-45.0000000,-0.6405039]
  cl(0:1, 46 ) = [-42.0000000,-0.6589108]
  cl(0:1, 47 ) = [-39.0000000,-0.6764170]
  cl(0:1, 48 ) = [-36.0000000,-0.6914421]
  cl(0:1, 49 ) = [-33.0000000,-0.7055940]
  cl(0:1, 50 ) = [-30.0000000,-0.7203081]
  cl(0:1, 51 ) = [-29.3939394,-0.7236871]
  cl(0:1, 52 ) = [-28.7878788,-0.7270662]
  cl(0:1, 53 ) = [-28.1818182,-0.7304452]
  cl(0:1, 54 ) = [-27.5757576,-0.7342450]
  cl(0:1, 55 ) = [-26.9696970,-0.7382250]
  cl(0:1, 56 ) = [-26.3636364,-0.7422051]
  cl(0:1, 57 ) = [-25.7575758,-0.7465555]
  cl(0:1, 58 ) = [-25.1515152,-0.7514612]
  cl(0:1, 59 ) = [-24.5454545,-0.7563669]
  cl(0:1, 60 ) = [-23.9393939,-0.7614087]
  cl(0:1, 61 ) = [-23.3333333,-0.7676756]
  cl(0:1, 62 ) = [-22.7272727,-0.7739425]
  cl(0:1, 63 ) = [-22.1212121,-0.7802094]
  cl(0:1, 64 ) = [-21.5151515,-0.7880458]
  cl(0:1, 65 ) = [-20.9090909,-0.7962747]
  cl(0:1, 66 ) = [-20.3030303,-0.8045037]
  cl(0:1, 67 ) = [-19.6969697,-0.8137216]
  cl(0:1, 68 ) = [-19.0909091,-0.8239284]
  cl(0:1, 69 ) = [-18.4848485,-0.8226468]
  cl(0:1, 70 ) = [-17.8787879,-0.8192412]
  cl(0:1, 71 ) = [-17.2727273,-0.8154466]
  cl(0:1, 72 ) = [-16.6666667,-0.8032964]
  cl(0:1, 73 ) = [-16.0606061,-0.7843101]
  cl(0:1, 74 ) = [-15.4545455,-0.7653247]
  cl(0:1, 75 ) = [-14.8484848,-0.7463389]
  cl(0:1, 76 ) = [-14.2424242,-0.7273520]
  cl(0:1, 77 ) = [-13.6363636,-0.7083664]
  cl(0:1, 78 ) = [-13.0303030,-0.6893790]
  cl(0:1, 79 ) = [-12.4242424,-0.6703938]
  cl(0:1, 80 ) = [-11.8181818,-0.6514067]
  cl(0:1, 81 ) = [-11.2121212,-0.6324215]
  cl(0:1, 82 ) = [-10.6060606,-0.6134349]
  cl(0:1, 83 ) = [-10.0000000,-0.5944489]
  cl(0:1, 84 ) = [-9.3939394,-0.5754636]
  cl(0:1, 85 ) = [-8.7878788,-0.5514037]
  cl(0:1, 86 ) = [-8.1818182,-0.5179263]
  cl(0:1, 87 ) = [-7.5757576,-0.4480129]
  cl(0:1, 88 ) = [-6.9696970,-0.3623066]
  cl(0:1, 89 ) = [-6.3636364,-0.2865925]
  cl(0:1, 90 ) = [-5.7575758,-0.2116813]
  cl(0:1, 91 ) = [-5.1515152,-0.1372783]
  cl(0:1, 92 ) = [-4.5454545,-0.0632446]
  cl(0:1, 93 ) = [-3.9393939,0.0103327]
  cl(0:1, 94 ) = [-3.3333333,0.0837358]
  cl(0:1, 95 ) = [-2.7272727,0.1571450]
  cl(0:1, 96 ) = [-2.1212121,0.2301321]
  cl(0:1, 97 ) = [-1.5151515,0.3032702]
  cl(0:1, 98 ) = [-0.9090909,0.3760079]
  cl(0:1, 99 ) = [-0.3030303,0.4486196]
  cl(0:1, 100) = [0.3030303,0.5211622]
  cl(0:1, 101) = [0.9090909,0.5934830]
  cl(0:1, 102) = [1.5151515,0.6654995]
  cl(0:1, 103) = [2.1212121,0.7374498]
  cl(0:1, 104) = [2.7272727,0.8089823]
  cl(0:1, 105) = [3.3333333,0.8803976]
  cl(0:1, 106) = [3.9393939,0.9514218]
  cl(0:1, 107) = [4.5454545,1.0218628]
  cl(0:1, 108) = [5.1515152,1.0918963]
  cl(0:1, 109) = [5.7575758,1.1607995]
  cl(0:1, 110) = [6.3636364,1.2272014]
  cl(0:1, 111) = [6.9696970,1.2880246]
  cl(0:1, 112) = [7.5757576,1.3393233]
  cl(0:1, 113) = [8.1818182,1.3867919]
  cl(0:1, 114) = [8.7878788,1.4344263]
  cl(0:1, 115) = [9.3939394,1.4807249]
  cl(0:1, 116) = [10.0000000,1.5161998]
  cl(0:1, 117) = [10.6060606,1.5465266]
  cl(0:1, 118) = [11.2121212,1.5701517]
  cl(0:1, 119) = [11.8181818,1.5846634]
  cl(0:1, 120) = [12.4242424,1.5543907]
  cl(0:1, 121) = [13.0303030,1.5069706]
  cl(0:1, 122) = [13.6363636,1.4463616]
  cl(0:1, 123) = [14.2424242,1.3930296]
  cl(0:1, 124) = [14.8484848,1.3506047]
  cl(0:1, 125) = [15.4545455,1.3218180]
  cl(0:1, 126) = [16.0606061,1.2975764]
  cl(0:1, 127) = [16.6666667,1.2733336]
  cl(0:1, 128) = [17.2727273,1.2436362]
  cl(0:1, 129) = [17.8787879,1.2072719]
  cl(0:1, 130) = [18.4848485,1.1899298]
  cl(0:1, 131) = [19.0909091,1.1770433]
  cl(0:1, 132) = [19.6969697,1.1624612]
  cl(0:1, 133) = [20.3030303,1.1492928]
  cl(0:1, 134) = [20.9090909,1.1375381]
  cl(0:1, 135) = [21.5151515,1.1257833]
  cl(0:1, 136) = [22.1212121,1.1145887]
  cl(0:1, 137) = [22.7272727,1.1056343]
  cl(0:1, 138) = [23.3333333,1.0966799]
  cl(0:1, 139) = [23.9393939,1.0877254]
  cl(0:1, 140) = [24.5454545,1.0805219]
  cl(0:1, 141) = [25.1515152,1.0735129]
  cl(0:1, 142) = [25.7575758,1.0665039]
  cl(0:1, 143) = [26.3636364,1.0602892]
  cl(0:1, 144) = [26.9696970,1.0546043]
  cl(0:1, 145) = [27.5757576,1.0489194]
  cl(0:1, 146) = [28.1818182,1.0434917]
  cl(0:1, 147) = [28.7878788,1.0386645]
  cl(0:1, 148) = [29.3939394,1.0338373]
  cl(0:1, 149) = [30.0000000,1.0290101]
  cl(0:1, 150) = [33.0000000,1.0079949]
  cl(0:1, 151) = [36.0000000,0.9877742]
  cl(0:1, 152) = [39.0000000,0.9663100]
  cl(0:1, 153) = [42.0000000,0.9413011]
  cl(0:1, 154) = [45.0000000,0.9150059]
  cl(0:1, 155) = [48.0000000,0.8815567]
  cl(0:1, 156) = [51.0000000,0.8439454]
  cl(0:1, 157) = [54.0000000,0.7980098]
  cl(0:1, 158) = [57.0000000,0.7520741]
  cl(0:1, 159) = [60.0000000,0.7061384]
  cl(0:1, 160) = [63.0000000,0.6443526]
  cl(0:1, 161) = [66.0000000,0.5825665]
  cl(0:1, 162) = [69.0000000,0.5207799]
  cl(0:1, 163) = [72.0000000,0.4514774]
  cl(0:1, 164) = [75.0000000,0.3784171]
  cl(0:1, 165) = [78.0000000,0.3053559]
  cl(0:1, 166) = [81.0000000,0.2309838]
  cl(0:1, 167) = [84.0000000,0.1539898]
  cl(0:1, 168) = [87.0000000,0.0769949]
  cl(0:1, 169) = [90.0000000,-0.0000003]
  cl(0:1, 170) = [93.0000000,-0.0538962]
  cl(0:1, 171) = [96.0000000,-0.1077923]
  cl(0:1, 172) = [99.0000000,-0.1616888]
  cl(0:1, 173) = [102.0000000,-0.2137492]
  cl(0:1, 174) = [105.0000000,-0.2648917]
  cl(0:1, 175) = [108.0000000,-0.3160342]
  cl(0:1, 176) = [111.0000000,-0.3645459]
  cl(0:1, 177) = [114.0000000,-0.4077960]
  cl(0:1, 178) = [117.0000000,-0.4510465]
  cl(0:1, 179) = [120.0000000,-0.4942971]
  cl(0:1, 180) = [123.0000000,-0.5264521]
  cl(0:1, 181) = [126.0000000,-0.5586070]
  cl(0:1, 182) = [129.0000000,-0.5907618]
  cl(0:1, 183) = [132.0000000,-0.6154205]
  cl(0:1, 184) = [135.0000000,-0.6363312]
  cl(0:1, 185) = [138.0000000,-0.6572417]
  cl(0:1, 186) = [141.0000000,-0.6760946]
  cl(0:1, 187) = [144.0000000,-0.6908325]
  cl(0:1, 188) = [147.0000000,-0.7055703]
  cl(0:1, 189) = [150.0000000,-0.7203080]
  cl(0:1, 190) = [153.0000000,-0.7468011]
  cl(0:1, 191) = [156.0000000,-0.7732941]
  cl(0:1, 192) = [159.0000000,-0.7997870]
  cl(0:1, 193) = [162.0000000,-0.7407908]
  cl(0:1, 194) = [165.0000000,-0.6390495]
  cl(0:1, 195) = [168.0000000,-0.5373087]
  cl(0:1, 196) = [171.0000000,-0.4225335]
  cl(0:1, 197) = [174.0000000,-0.2816887]
  cl(0:1, 198) = [177.0000000,-0.1402023]
  cl(0:1, 199) = [180.0000000,0.0000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl29_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl29 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl29_ge28.pro'
  ENDIF

!print,y,cl29

!_________________________________________________________________________
  
END FUNCTION calc_cl29_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cl30_ge28( y ) RESULT( cl30 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cl

  REAL :: cl30
  INTEGER :: i

  cl30 = 0.0
!                alpha    CL   
  cl(0:1, 0  ) = [-180.0000000,0.0000000]
  cl(0:1, 1  ) = [-177.0000000,0.1494718]
  cl(0:1, 2  ) = [-174.0000000,0.3003129]
  cl(0:1, 3  ) = [-171.0000000,0.4504703]
  cl(0:1, 4  ) = [-168.0000000,0.5621413]
  cl(0:1, 5  ) = [-165.0000000,0.6545695]
  cl(0:1, 6  ) = [-162.0000000,0.7469988]
  cl(0:1, 7  ) = [-159.0000000,0.7997870]
  cl(0:1, 8  ) = [-156.0000000,0.7732941]
  cl(0:1, 9  ) = [-153.0000000,0.7468011]
  cl(0:1, 10 ) = [-150.0000000,0.7203080]
  cl(0:1, 11 ) = [-147.0000000,0.7055703]
  cl(0:1, 12 ) = [-144.0000000,0.6908325]
  cl(0:1, 13 ) = [-141.0000000,0.6760946]
  cl(0:1, 14 ) = [-138.0000000,0.6572417]
  cl(0:1, 15 ) = [-135.0000000,0.6363312]
  cl(0:1, 16 ) = [-132.0000000,0.6154205]
  cl(0:1, 17 ) = [-129.0000000,0.5907618]
  cl(0:1, 18 ) = [-126.0000000,0.5586070]
  cl(0:1, 19 ) = [-123.0000000,0.5264521]
  cl(0:1, 20 ) = [-120.0000000,0.4942971]
  cl(0:1, 21 ) = [-117.0000000,0.4510465]
  cl(0:1, 22 ) = [-114.0000000,0.4077960]
  cl(0:1, 23 ) = [-111.0000000,0.3645459]
  cl(0:1, 24 ) = [-108.0000000,0.3160342]
  cl(0:1, 25 ) = [-105.0000000,0.2648917]
  cl(0:1, 26 ) = [-102.0000000,0.2137492]
  cl(0:1, 27 ) = [-99.0000000,0.1616888]
  cl(0:1, 28 ) = [-96.0000000,0.1077923]
  cl(0:1, 29 ) = [-93.0000000,0.0538962]
  cl(0:1, 30 ) = [-90.0000000,0.0000003]
  cl(0:1, 31 ) = [-87.0000000,-0.0538961]
  cl(0:1, 32 ) = [-84.0000000,-0.1077924]
  cl(0:1, 33 ) = [-81.0000000,-0.1616883]
  cl(0:1, 34 ) = [-78.0000000,-0.2137490]
  cl(0:1, 35 ) = [-75.0000000,-0.2648921]
  cl(0:1, 36 ) = [-72.0000000,-0.3160339]
  cl(0:1, 37 ) = [-69.0000000,-0.3645455]
  cl(0:1, 38 ) = [-66.0000000,-0.4077961]
  cl(0:1, 39 ) = [-63.0000000,-0.4510467]
  cl(0:1, 40 ) = [-60.0000000,-0.4942973]
  cl(0:1, 41 ) = [-57.0000000,-0.5264523]
  cl(0:1, 42 ) = [-54.0000000,-0.5586072]
  cl(0:1, 43 ) = [-51.0000000,-0.5907620]
  cl(0:1, 44 ) = [-48.0000000,-0.6170897]
  cl(0:1, 45 ) = [-45.0000000,-0.6405039]
  cl(0:1, 46 ) = [-42.0000000,-0.6589108]
  cl(0:1, 47 ) = [-39.0000000,-0.6764170]
  cl(0:1, 48 ) = [-36.0000000,-0.6914421]
  cl(0:1, 49 ) = [-33.0000000,-0.7055940]
  cl(0:1, 50 ) = [-30.0000000,-0.7203081]
  cl(0:1, 51 ) = [-29.3939394,-0.7236871]
  cl(0:1, 52 ) = [-28.7878788,-0.7270662]
  cl(0:1, 53 ) = [-28.1818182,-0.7304452]
  cl(0:1, 54 ) = [-27.5757576,-0.7342450]
  cl(0:1, 55 ) = [-26.9696970,-0.7382250]
  cl(0:1, 56 ) = [-26.3636364,-0.7422051]
  cl(0:1, 57 ) = [-25.7575758,-0.7465555]
  cl(0:1, 58 ) = [-25.1515152,-0.7514612]
  cl(0:1, 59 ) = [-24.5454545,-0.7563669]
  cl(0:1, 60 ) = [-23.9393939,-0.7614087]
  cl(0:1, 61 ) = [-23.3333333,-0.7676756]
  cl(0:1, 62 ) = [-22.7272727,-0.7739425]
  cl(0:1, 63 ) = [-22.1212121,-0.7802094]
  cl(0:1, 64 ) = [-21.5151515,-0.7880458]
  cl(0:1, 65 ) = [-20.9090909,-0.7962747]
  cl(0:1, 66 ) = [-20.3030303,-0.8045037]
  cl(0:1, 67 ) = [-19.6969697,-0.8137216]
  cl(0:1, 68 ) = [-19.0909091,-0.8239284]
  cl(0:1, 69 ) = [-18.4848485,-0.8226468]
  cl(0:1, 70 ) = [-17.8787879,-0.8192412]
  cl(0:1, 71 ) = [-17.2727273,-0.8154466]
  cl(0:1, 72 ) = [-16.6666667,-0.8032964]
  cl(0:1, 73 ) = [-16.0606061,-0.7843101]
  cl(0:1, 74 ) = [-15.4545455,-0.7653247]
  cl(0:1, 75 ) = [-14.8484848,-0.7463389]
  cl(0:1, 76 ) = [-14.2424242,-0.7273520]
  cl(0:1, 77 ) = [-13.6363636,-0.7083664]
  cl(0:1, 78 ) = [-13.0303030,-0.6893790]
  cl(0:1, 79 ) = [-12.4242424,-0.6703938]
  cl(0:1, 80 ) = [-11.8181818,-0.6514067]
  cl(0:1, 81 ) = [-11.2121212,-0.6324215]
  cl(0:1, 82 ) = [-10.6060606,-0.6134349]
  cl(0:1, 83 ) = [-10.0000000,-0.5944489]
  cl(0:1, 84 ) = [-9.3939394,-0.5754636]
  cl(0:1, 85 ) = [-8.7878788,-0.5514037]
  cl(0:1, 86 ) = [-8.1818182,-0.5179263]
  cl(0:1, 87 ) = [-7.5757576,-0.4480129]
  cl(0:1, 88 ) = [-6.9696970,-0.3623066]
  cl(0:1, 89 ) = [-6.3636364,-0.2865925]
  cl(0:1, 90 ) = [-5.7575758,-0.2116813]
  cl(0:1, 91 ) = [-5.1515152,-0.1372783]
  cl(0:1, 92 ) = [-4.5454545,-0.0632446]
  cl(0:1, 93 ) = [-3.9393939,0.0103327]
  cl(0:1, 94 ) = [-3.3333333,0.0837358]
  cl(0:1, 95 ) = [-2.7272727,0.1571450]
  cl(0:1, 96 ) = [-2.1212121,0.2301321]
  cl(0:1, 97 ) = [-1.5151515,0.3032702]
  cl(0:1, 98 ) = [-0.9090909,0.3760079]
  cl(0:1, 99 ) = [-0.3030303,0.4486196]
  cl(0:1, 100) = [0.3030303,0.5211622]
  cl(0:1, 101) = [0.9090909,0.5934830]
  cl(0:1, 102) = [1.5151515,0.6654995]
  cl(0:1, 103) = [2.1212121,0.7374498]
  cl(0:1, 104) = [2.7272727,0.8089823]
  cl(0:1, 105) = [3.3333333,0.8803976]
  cl(0:1, 106) = [3.9393939,0.9514218]
  cl(0:1, 107) = [4.5454545,1.0218628]
  cl(0:1, 108) = [5.1515152,1.0918963]
  cl(0:1, 109) = [5.7575758,1.1607995]
  cl(0:1, 110) = [6.3636364,1.2272014]
  cl(0:1, 111) = [6.9696970,1.2880246]
  cl(0:1, 112) = [7.5757576,1.3393233]
  cl(0:1, 113) = [8.1818182,1.3867919]
  cl(0:1, 114) = [8.7878788,1.4344263]
  cl(0:1, 115) = [9.3939394,1.4807249]
  cl(0:1, 116) = [10.0000000,1.5161998]
  cl(0:1, 117) = [10.6060606,1.5465266]
  cl(0:1, 118) = [11.2121212,1.5701517]
  cl(0:1, 119) = [11.8181818,1.5846634]
  cl(0:1, 120) = [12.4242424,1.5543907]
  cl(0:1, 121) = [13.0303030,1.5069706]
  cl(0:1, 122) = [13.6363636,1.4463616]
  cl(0:1, 123) = [14.2424242,1.3930296]
  cl(0:1, 124) = [14.8484848,1.3506047]
  cl(0:1, 125) = [15.4545455,1.3218180]
  cl(0:1, 126) = [16.0606061,1.2975764]
  cl(0:1, 127) = [16.6666667,1.2733336]
  cl(0:1, 128) = [17.2727273,1.2436362]
  cl(0:1, 129) = [17.8787879,1.2072719]
  cl(0:1, 130) = [18.4848485,1.1899298]
  cl(0:1, 131) = [19.0909091,1.1770433]
  cl(0:1, 132) = [19.6969697,1.1624612]
  cl(0:1, 133) = [20.3030303,1.1492928]
  cl(0:1, 134) = [20.9090909,1.1375381]
  cl(0:1, 135) = [21.5151515,1.1257833]
  cl(0:1, 136) = [22.1212121,1.1145887]
  cl(0:1, 137) = [22.7272727,1.1056343]
  cl(0:1, 138) = [23.3333333,1.0966799]
  cl(0:1, 139) = [23.9393939,1.0877254]
  cl(0:1, 140) = [24.5454545,1.0805219]
  cl(0:1, 141) = [25.1515152,1.0735129]
  cl(0:1, 142) = [25.7575758,1.0665039]
  cl(0:1, 143) = [26.3636364,1.0602892]
  cl(0:1, 144) = [26.9696970,1.0546043]
  cl(0:1, 145) = [27.5757576,1.0489194]
  cl(0:1, 146) = [28.1818182,1.0434917]
  cl(0:1, 147) = [28.7878788,1.0386645]
  cl(0:1, 148) = [29.3939394,1.0338373]
  cl(0:1, 149) = [30.0000000,1.0290101]
  cl(0:1, 150) = [33.0000000,1.0079949]
  cl(0:1, 151) = [36.0000000,0.9877742]
  cl(0:1, 152) = [39.0000000,0.9663100]
  cl(0:1, 153) = [42.0000000,0.9413011]
  cl(0:1, 154) = [45.0000000,0.9150059]
  cl(0:1, 155) = [48.0000000,0.8815567]
  cl(0:1, 156) = [51.0000000,0.8439454]
  cl(0:1, 157) = [54.0000000,0.7980098]
  cl(0:1, 158) = [57.0000000,0.7520741]
  cl(0:1, 159) = [60.0000000,0.7061384]
  cl(0:1, 160) = [63.0000000,0.6443526]
  cl(0:1, 161) = [66.0000000,0.5825665]
  cl(0:1, 162) = [69.0000000,0.5207799]
  cl(0:1, 163) = [72.0000000,0.4514774]
  cl(0:1, 164) = [75.0000000,0.3784171]
  cl(0:1, 165) = [78.0000000,0.3053559]
  cl(0:1, 166) = [81.0000000,0.2309838]
  cl(0:1, 167) = [84.0000000,0.1539898]
  cl(0:1, 168) = [87.0000000,0.0769949]
  cl(0:1, 169) = [90.0000000,-0.0000003]
  cl(0:1, 170) = [93.0000000,-0.0538962]
  cl(0:1, 171) = [96.0000000,-0.1077923]
  cl(0:1, 172) = [99.0000000,-0.1616888]
  cl(0:1, 173) = [102.0000000,-0.2137492]
  cl(0:1, 174) = [105.0000000,-0.2648917]
  cl(0:1, 175) = [108.0000000,-0.3160342]
  cl(0:1, 176) = [111.0000000,-0.3645459]
  cl(0:1, 177) = [114.0000000,-0.4077960]
  cl(0:1, 178) = [117.0000000,-0.4510465]
  cl(0:1, 179) = [120.0000000,-0.4942971]
  cl(0:1, 180) = [123.0000000,-0.5264521]
  cl(0:1, 181) = [126.0000000,-0.5586070]
  cl(0:1, 182) = [129.0000000,-0.5907618]
  cl(0:1, 183) = [132.0000000,-0.6154205]
  cl(0:1, 184) = [135.0000000,-0.6363312]
  cl(0:1, 185) = [138.0000000,-0.6572417]
  cl(0:1, 186) = [141.0000000,-0.6760946]
  cl(0:1, 187) = [144.0000000,-0.6908325]
  cl(0:1, 188) = [147.0000000,-0.7055703]
  cl(0:1, 189) = [150.0000000,-0.7203080]
  cl(0:1, 190) = [153.0000000,-0.7468011]
  cl(0:1, 191) = [156.0000000,-0.7732941]
  cl(0:1, 192) = [159.0000000,-0.7997870]
  cl(0:1, 193) = [162.0000000,-0.7407908]
  cl(0:1, 194) = [165.0000000,-0.6390495]
  cl(0:1, 195) = [168.0000000,-0.5373087]
  cl(0:1, 196) = [171.0000000,-0.4225335]
  cl(0:1, 197) = [174.0000000,-0.2816887]
  cl(0:1, 198) = [177.0000000,-0.1402023]
  cl(0:1, 199) = [180.0000000,0.0000000]

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
!     PRINT*,'alpha < tabulated range in calc_cl30_ge28.pro'
  ENDIF
  IF ( y .GT. cl(0,na-1) ) THEN
     cl30 = cl(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cl30_ge28.pro'
  ENDIF

!print,y,cl30

!_________________________________________________________________________
  
END FUNCTION calc_cl30_ge28
!_________________________________________________________________________

!=========================================================================



!=========================================================================

FUNCTION calc_cd_ge28( x,y ) RESULT( cd )
!_________________________________________________________________________
!
! Calls appropriate airfoil section table depending upon radial distance
! from blade root. Data from GE 2.8 MW
!
! calc_cd1_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd2_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd3_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd4_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd5_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd6_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd7_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd8_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd9_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd10_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd11_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd12_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd13_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd14_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd15_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd16_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd17_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd18_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd19_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd20_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd21_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd22_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd23_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd24_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd25_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd26_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd27_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd28_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd29_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
! calc_cd30_ge28 uses data from https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
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
  
  r(0)  = 2.00000000
  r(1)  = 4.11919952
  r(2)  = 6.23839904
  r(3)  = 8.35759855
  r(4)  = 10.47679807
  r(5)  = 12.59599759
  r(6)  = 14.71519711
  r(7)  = 16.83439663
  r(8)  = 18.95359614
  r(9)  = 21.07279566
  r(10) = 23.19199518
  r(11) = 25.31119470
  r(12) = 27.43039421
  r(13) = 29.54959373
  r(14) = 31.66879325
  r(15) = 33.78799277
  r(16) = 35.90719229
  r(17) = 38.02639180
  r(18) = 40.14559132
  r(19) = 42.26479084
  r(20) = 44.38399036
  r(21) = 46.50318988
  r(22) = 48.62238939
  r(23) = 50.74158891
  r(24) = 52.86078843
  r(25) = 54.97998795
  r(26) = 57.09918747
  r(27) = 59.21838698
  r(28) = 61.33758650
  r(29) = 63.45678602
  
  IF ( ( x .GT. r( 0) ) .AND. ( x .LE. r( 1) ) ) THEN
       cd = calc_cd1_ge28(y)
     ELSEIF ( ( x .GT. r( 1) ) .AND. ( x .LE. r( 2) ) ) THEN
       cd = calc_cd2_ge28(y)
     ELSEIF ( ( x .GT. r( 2) ) .AND. ( x .LE. r( 3) ) ) THEN
       cd = calc_cd3_ge28(y)
     ELSEIF ( ( x .GT. r( 3) ) .AND. ( x .LE. r( 4) ) ) THEN
       cd = calc_cd4_ge28(y)
     ELSEIF ( ( x .GT. r( 4) ) .AND. ( x .LE. r( 5) ) ) THEN
       cd = calc_cd5_ge28(y)
     ELSEIF ( ( x .GT. r( 5) ) .AND. ( x .LE. r( 6) ) ) THEN
       cd = calc_cd6_ge28(y)
     ELSEIF ( ( x .GT. r( 6) ) .AND. ( x .LE. r( 7) ) ) THEN
       cd = calc_cd7_ge28(y)
     ELSEIF ( ( x .GT. r( 7) ) .AND. ( x .LE. r( 8) ) ) THEN
       cd = calc_cd8_ge28(y)  
     ELSEIF ( ( x .GT. r( 8) ) .AND. ( x .LE. r( 9) ) ) THEN
       cd = calc_cd9_ge28(y)  
     ELSEIF ( ( x .GT. r( 9) ) .AND. ( x .LE. r(10) ) ) THEN
       cd = calc_cd10_ge28(y)  
     ELSEIF ( ( x .GT. r(10) ) .AND. ( x .LE. r(11) ) ) THEN
       cd = calc_cd11_ge28(y)
     ELSEIF ( ( x .GT. r(11) ) .AND. ( x .LE. r(12) ) ) THEN
       cd = calc_cd12_ge28(y)
     ELSEIF ( ( x .GT. r(12) ) .AND. ( x .LE. r(13) ) ) THEN
       cd = calc_cd13_ge28(y)
     ELSEIF ( ( x .GT. r(13) ) .AND. ( x .LE. r(14) ) ) THEN
       cd = calc_cd14_ge28(y)
     ELSEIF ( ( x .GT. r(14) ) .AND. ( x .LE. r(15) ) ) THEN
       cd = calc_cd15_ge28(y)
     ELSEIF ( ( x .GT. r(15) ) .AND. ( x .LE. r(16) ) ) THEN
       cd = calc_cd16_ge28(y)
     ELSEIF ( ( x .GT. r(16) ) .AND. ( x .LE. r(17) ) ) THEN
       cd = calc_cd17_ge28(y)
     ELSEIF ( ( x .GT. r(17) ) .AND. ( x .LE. r(18) ) ) THEN
       cd = calc_cd18_ge28(y)  
     ELSEIF ( ( x .GT. r(18) ) .AND. ( x .LE. r(19) ) ) THEN
       cd = calc_cd19_ge28(y)  
     ELSEIF ( ( x .GT. r(19) ) .AND. ( x .LE. r(20) ) ) THEN
       cd = calc_cd20_ge28(y)  
     ELSEIF ( ( x .GT. r(20) ) .AND. ( x .LE. r(21) ) ) THEN
       cd = calc_cd21_ge28(y)
     ELSEIF ( ( x .GT. r(21) ) .AND. ( x .LE. r(22) ) ) THEN
       cd = calc_cd22_ge28(y)
     ELSEIF ( ( x .GT. r(22) ) .AND. ( x .LE. r(23) ) ) THEN
       cd = calc_cd23_ge28(y)
     ELSEIF ( ( x .GT. r(23) ) .AND. ( x .LE. r(24) ) ) THEN
       cd = calc_cd24_ge28(y)
     ELSEIF ( ( x .GT. r(24) ) .AND. ( x .LE. r(25) ) ) THEN
       cd = calc_cd25_ge28(y)
     ELSEIF ( ( x .GT. r(25) ) .AND. ( x .LE. r(26) ) ) THEN
       cd = calc_cd26_ge28(y)
     ELSEIF ( ( x .GT. r(26) ) .AND. ( x .LE. r(27) ) ) THEN
       cd = calc_cd27_ge28(y)
     ELSEIF ( ( x .GT. r(27) ) .AND. ( x .LE. r(28) ) ) THEN
       cd = calc_cd28_ge28(y)  
     ELSEIF ( ( x .GT. r(28) ) .AND. ( x .LE. r(29) ) ) THEN
       cd = calc_cd29_ge28(y)  
     ELSEIF ( ( x .GT. r(29) ) .AND. ( x .LE. r(30) ) ) THEN
       cd = calc_cd30_ge28(y)  
  ENDIF
  
  IF ( x .LT. r(0) ) THEN 
     PRINT*,'—————————————'
     PRINT*,'radius less than blade root in calc_cd_ge28.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
  IF ( x .GT. r(ns-1) ) THEN 
     PRINT*,'—————————————'
     PRINT*,'radius greater than blade length in calc_cd_ge28.pro'
     PRINT*,'radius: ', x
     PRINT*,'—————————————'
     STOP
  ENDIF
  
!_________________________________________________________________________
  
END FUNCTION calc_cd_ge28
!_________________________________________________________________________


!=========================================================================

FUNCTION calc_cd1_ge28( y ) RESULT( cd1 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd1
  INTEGER :: i

  cd1 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.5000000]
  cd(0:1, 1  ) = [-177.0000000,0.5000000]
  cd(0:1, 2  ) = [-174.0000000,0.5000000]
  cd(0:1, 3  ) = [-171.0000000,0.5000000]
  cd(0:1, 4  ) = [-168.0000000,0.5000000]
  cd(0:1, 5  ) = [-165.0000000,0.5000000]
  cd(0:1, 6  ) = [-162.0000000,0.5000000]
  cd(0:1, 7  ) = [-159.0000000,0.5000000]
  cd(0:1, 8  ) = [-156.0000000,0.5000000]
  cd(0:1, 9  ) = [-153.0000000,0.5000000]
  cd(0:1, 10 ) = [-150.0000000,0.5000000]
  cd(0:1, 11 ) = [-147.0000000,0.5000000]
  cd(0:1, 12 ) = [-144.0000000,0.5000000]
  cd(0:1, 13 ) = [-141.0000000,0.5000000]
  cd(0:1, 14 ) = [-138.0000000,0.5000000]
  cd(0:1, 15 ) = [-135.0000000,0.5000000]
  cd(0:1, 16 ) = [-132.0000000,0.5000000]
  cd(0:1, 17 ) = [-129.0000000,0.5000000]
  cd(0:1, 18 ) = [-126.0000000,0.5000000]
  cd(0:1, 19 ) = [-123.0000000,0.5000000]
  cd(0:1, 20 ) = [-120.0000000,0.5000000]
  cd(0:1, 21 ) = [-117.0000000,0.5000000]
  cd(0:1, 22 ) = [-114.0000000,0.5000000]
  cd(0:1, 23 ) = [-111.0000000,0.5000000]
  cd(0:1, 24 ) = [-108.0000000,0.5000000]
  cd(0:1, 25 ) = [-105.0000000,0.5000000]
  cd(0:1, 26 ) = [-102.0000000,0.5000000]
  cd(0:1, 27 ) = [-99.0000000,0.5000000]
  cd(0:1, 28 ) = [-96.0000000,0.5000000]
  cd(0:1, 29 ) = [-93.0000000,0.5000000]
  cd(0:1, 30 ) = [-90.0000000,0.5000000]
  cd(0:1, 31 ) = [-87.0000000,0.5000000]
  cd(0:1, 32 ) = [-84.0000000,0.5000000]
  cd(0:1, 33 ) = [-81.0000000,0.5000000]
  cd(0:1, 34 ) = [-78.0000000,0.5000000]
  cd(0:1, 35 ) = [-75.0000000,0.5000000]
  cd(0:1, 36 ) = [-72.0000000,0.5000000]
  cd(0:1, 37 ) = [-69.0000000,0.5000000]
  cd(0:1, 38 ) = [-66.0000000,0.5000000]
  cd(0:1, 39 ) = [-63.0000000,0.5000000]
  cd(0:1, 40 ) = [-60.0000000,0.5000000]
  cd(0:1, 41 ) = [-57.0000000,0.5000000]
  cd(0:1, 42 ) = [-54.0000000,0.5000000]
  cd(0:1, 43 ) = [-51.0000000,0.5000000]
  cd(0:1, 44 ) = [-48.0000000,0.5000000]
  cd(0:1, 45 ) = [-45.0000000,0.5000000]
  cd(0:1, 46 ) = [-42.0000000,0.5000000]
  cd(0:1, 47 ) = [-39.0000000,0.5000000]
  cd(0:1, 48 ) = [-36.0000000,0.5000000]
  cd(0:1, 49 ) = [-33.0000000,0.5000000]
  cd(0:1, 50 ) = [-30.0000000,0.5000000]
  cd(0:1, 51 ) = [-29.3939394,0.5000000]
  cd(0:1, 52 ) = [-28.7878788,0.5000000]
  cd(0:1, 53 ) = [-28.1818182,0.5000000]
  cd(0:1, 54 ) = [-27.5757576,0.5000000]
  cd(0:1, 55 ) = [-26.9696970,0.5000000]
  cd(0:1, 56 ) = [-26.3636364,0.5000000]
  cd(0:1, 57 ) = [-25.7575758,0.5000000]
  cd(0:1, 58 ) = [-25.1515152,0.5000000]
  cd(0:1, 59 ) = [-24.5454545,0.5000000]
  cd(0:1, 60 ) = [-23.9393939,0.5000000]
  cd(0:1, 61 ) = [-23.3333333,0.5000000]
  cd(0:1, 62 ) = [-22.7272727,0.5000000]
  cd(0:1, 63 ) = [-22.1212121,0.5000000]
  cd(0:1, 64 ) = [-21.5151515,0.5000000]
  cd(0:1, 65 ) = [-20.9090909,0.5000000]
  cd(0:1, 66 ) = [-20.3030303,0.5000000]
  cd(0:1, 67 ) = [-19.6969697,0.5000000]
  cd(0:1, 68 ) = [-19.0909091,0.5000000]
  cd(0:1, 69 ) = [-18.4848485,0.5000000]
  cd(0:1, 70 ) = [-17.8787879,0.5000000]
  cd(0:1, 71 ) = [-17.2727273,0.5000000]
  cd(0:1, 72 ) = [-16.6666667,0.5000000]
  cd(0:1, 73 ) = [-16.0606061,0.5000000]
  cd(0:1, 74 ) = [-15.4545455,0.5000000]
  cd(0:1, 75 ) = [-14.8484848,0.5000000]
  cd(0:1, 76 ) = [-14.2424242,0.5000000]
  cd(0:1, 77 ) = [-13.6363636,0.5000000]
  cd(0:1, 78 ) = [-13.0303030,0.5000000]
  cd(0:1, 79 ) = [-12.4242424,0.5000000]
  cd(0:1, 80 ) = [-11.8181818,0.5000000]
  cd(0:1, 81 ) = [-11.2121212,0.5000000]
  cd(0:1, 82 ) = [-10.6060606,0.5000000]
  cd(0:1, 83 ) = [-10.0000000,0.5000000]
  cd(0:1, 84 ) = [-9.3939394,0.5000000]
  cd(0:1, 85 ) = [-8.7878788,0.5000000]
  cd(0:1, 86 ) = [-8.1818182,0.5000000]
  cd(0:1, 87 ) = [-7.5757576,0.5000000]
  cd(0:1, 88 ) = [-6.9696970,0.5000000]
  cd(0:1, 89 ) = [-6.3636364,0.5000000]
  cd(0:1, 90 ) = [-5.7575758,0.5000000]
  cd(0:1, 91 ) = [-5.1515152,0.5000000]
  cd(0:1, 92 ) = [-4.5454545,0.5000000]
  cd(0:1, 93 ) = [-3.9393939,0.5000000]
  cd(0:1, 94 ) = [-3.3333333,0.5000000]
  cd(0:1, 95 ) = [-2.7272727,0.5000000]
  cd(0:1, 96 ) = [-2.1212121,0.5000000]
  cd(0:1, 97 ) = [-1.5151515,0.5000000]
  cd(0:1, 98 ) = [-0.9090909,0.5000000]
  cd(0:1, 99 ) = [-0.3030303,0.5000000]
  cd(0:1, 100) = [0.3030303,0.5000000]
  cd(0:1, 101) = [0.9090909,0.5000000]
  cd(0:1, 102) = [1.5151515,0.5000000]
  cd(0:1, 103) = [2.1212121,0.5000000]
  cd(0:1, 104) = [2.7272727,0.5000000]
  cd(0:1, 105) = [3.3333333,0.5000000]
  cd(0:1, 106) = [3.9393939,0.5000000]
  cd(0:1, 107) = [4.5454545,0.5000000]
  cd(0:1, 108) = [5.1515152,0.5000000]
  cd(0:1, 109) = [5.7575758,0.5000000]
  cd(0:1, 110) = [6.3636364,0.5000000]
  cd(0:1, 111) = [6.9696970,0.5000000]
  cd(0:1, 112) = [7.5757576,0.5000000]
  cd(0:1, 113) = [8.1818182,0.5000000]
  cd(0:1, 114) = [8.7878788,0.5000000]
  cd(0:1, 115) = [9.3939394,0.5000000]
  cd(0:1, 116) = [10.0000000,0.5000000]
  cd(0:1, 117) = [10.6060606,0.5000000]
  cd(0:1, 118) = [11.2121212,0.5000000]
  cd(0:1, 119) = [11.8181818,0.5000000]
  cd(0:1, 120) = [12.4242424,0.5000000]
  cd(0:1, 121) = [13.0303030,0.5000000]
  cd(0:1, 122) = [13.6363636,0.5000000]
  cd(0:1, 123) = [14.2424242,0.5000000]
  cd(0:1, 124) = [14.8484848,0.5000000]
  cd(0:1, 125) = [15.4545455,0.5000000]
  cd(0:1, 126) = [16.0606061,0.5000000]
  cd(0:1, 127) = [16.6666667,0.5000000]
  cd(0:1, 128) = [17.2727273,0.5000000]
  cd(0:1, 129) = [17.8787879,0.5000000]
  cd(0:1, 130) = [18.4848485,0.5000000]
  cd(0:1, 131) = [19.0909091,0.5000000]
  cd(0:1, 132) = [19.6969697,0.5000000]
  cd(0:1, 133) = [20.3030303,0.5000000]
  cd(0:1, 134) = [20.9090909,0.5000000]
  cd(0:1, 135) = [21.5151515,0.5000000]
  cd(0:1, 136) = [22.1212121,0.5000000]
  cd(0:1, 137) = [22.7272727,0.5000000]
  cd(0:1, 138) = [23.3333333,0.5000000]
  cd(0:1, 139) = [23.9393939,0.5000000]
  cd(0:1, 140) = [24.5454545,0.5000000]
  cd(0:1, 141) = [25.1515152,0.5000000]
  cd(0:1, 142) = [25.7575758,0.5000000]
  cd(0:1, 143) = [26.3636364,0.5000000]
  cd(0:1, 144) = [26.9696970,0.5000000]
  cd(0:1, 145) = [27.5757576,0.5000000]
  cd(0:1, 146) = [28.1818182,0.5000000]
  cd(0:1, 147) = [28.7878788,0.5000000]
  cd(0:1, 148) = [29.3939394,0.5000000]
  cd(0:1, 149) = [30.0000000,0.5000000]
  cd(0:1, 150) = [33.0000000,0.5000000]
  cd(0:1, 151) = [36.0000000,0.5000000]
  cd(0:1, 152) = [39.0000000,0.5000000]
  cd(0:1, 153) = [42.0000000,0.5000000]
  cd(0:1, 154) = [45.0000000,0.5000000]
  cd(0:1, 155) = [48.0000000,0.5000000]
  cd(0:1, 156) = [51.0000000,0.5000000]
  cd(0:1, 157) = [54.0000000,0.5000000]
  cd(0:1, 158) = [57.0000000,0.5000000]
  cd(0:1, 159) = [60.0000000,0.5000000]
  cd(0:1, 160) = [63.0000000,0.5000000]
  cd(0:1, 161) = [66.0000000,0.5000000]
  cd(0:1, 162) = [69.0000000,0.5000000]
  cd(0:1, 163) = [72.0000000,0.5000000]
  cd(0:1, 164) = [75.0000000,0.5000000]
  cd(0:1, 165) = [78.0000000,0.5000000]
  cd(0:1, 166) = [81.0000000,0.5000000]
  cd(0:1, 167) = [84.0000000,0.5000000]
  cd(0:1, 168) = [87.0000000,0.5000000]
  cd(0:1, 169) = [90.0000000,0.5000000]
  cd(0:1, 170) = [93.0000000,0.5000000]
  cd(0:1, 171) = [96.0000000,0.5000000]
  cd(0:1, 172) = [99.0000000,0.5000000]
  cd(0:1, 173) = [102.0000000,0.5000000]
  cd(0:1, 174) = [105.0000000,0.5000000]
  cd(0:1, 175) = [108.0000000,0.5000000]
  cd(0:1, 176) = [111.0000000,0.5000000]
  cd(0:1, 177) = [114.0000000,0.5000000]
  cd(0:1, 178) = [117.0000000,0.5000000]
  cd(0:1, 179) = [120.0000000,0.5000000]
  cd(0:1, 180) = [123.0000000,0.5000000]
  cd(0:1, 181) = [126.0000000,0.5000000]
  cd(0:1, 182) = [129.0000000,0.5000000]
  cd(0:1, 183) = [132.0000000,0.5000000]
  cd(0:1, 184) = [135.0000000,0.5000000]
  cd(0:1, 185) = [138.0000000,0.5000000]
  cd(0:1, 186) = [141.0000000,0.5000000]
  cd(0:1, 187) = [144.0000000,0.5000000]
  cd(0:1, 188) = [147.0000000,0.5000000]
  cd(0:1, 189) = [150.0000000,0.5000000]
  cd(0:1, 190) = [153.0000000,0.5000000]
  cd(0:1, 191) = [156.0000000,0.5000000]
  cd(0:1, 192) = [159.0000000,0.5000000]
  cd(0:1, 193) = [162.0000000,0.5000000]
  cd(0:1, 194) = [165.0000000,0.5000000]
  cd(0:1, 195) = [168.0000000,0.5000000]
  cd(0:1, 196) = [171.0000000,0.5000000]
  cd(0:1, 197) = [174.0000000,0.5000000]
  cd(0:1, 198) = [177.0000000,0.5000000]
  cd(0:1, 199) = [180.0000000,0.5000000]

  IF ( y .EQ. cd(0,0) ) cd1 = cd(1,0)
  DO i = 1,na-1
     IF ( ( y .GT. cd(0,i-1) ) .AND. (y .LE. cd(0,i) ) ) THEN
        cd1 = cd(1,i-1) + (y - cd(0,i-1))*( ( cd(1,i) - cd(1,i-1) )/( cd(0,i) - cd(0,i-1) ) )
        GOTO 1
     ENDIF
  ENDDO
1 CONTINUE

  IF ( y .LT. cd(0,0) ) THEN
     cd1 = cd(1,0)
!     PRINT*,'alpha < tabulated range in calc_cd1_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd1 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd1_ge28.pro'
  ENDIF

!print,y,cd1

!_________________________________________________________________________
  
END FUNCTION calc_cd1_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd2_ge28( y ) RESULT( cd2 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd2
  INTEGER :: i

  cd2 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.4982985]
  cd(0:1, 1  ) = [-177.0000000,0.4993738]
  cd(0:1, 2  ) = [-174.0000000,0.4994195]
  cd(0:1, 3  ) = [-171.0000000,0.4994620]
  cd(0:1, 4  ) = [-168.0000000,0.4995304]
  cd(0:1, 5  ) = [-165.0000000,0.4996082]
  cd(0:1, 6  ) = [-162.0000000,0.4996801]
  cd(0:1, 7  ) = [-159.0000000,0.4997542]
  cd(0:1, 8  ) = [-156.0000000,0.4998369]
  cd(0:1, 9  ) = [-153.0000000,0.4999083]
  cd(0:1, 10 ) = [-150.0000000,0.4999651]
  cd(0:1, 11 ) = [-147.0000000,0.5015496]
  cd(0:1, 12 ) = [-144.0000000,0.5076337]
  cd(0:1, 13 ) = [-141.0000000,0.5137178]
  cd(0:1, 14 ) = [-138.0000000,0.5200100]
  cd(0:1, 15 ) = [-135.0000000,0.5264060]
  cd(0:1, 16 ) = [-132.0000000,0.5328022]
  cd(0:1, 17 ) = [-129.0000000,0.5390257]
  cd(0:1, 18 ) = [-126.0000000,0.5449039]
  cd(0:1, 19 ) = [-123.0000000,0.5479970]
  cd(0:1, 20 ) = [-120.0000000,0.5503904]
  cd(0:1, 21 ) = [-117.0000000,0.5518423]
  cd(0:1, 22 ) = [-114.0000000,0.5532942]
  cd(0:1, 23 ) = [-111.0000000,0.5547460]
  cd(0:1, 24 ) = [-108.0000000,0.5553394]
  cd(0:1, 25 ) = [-105.0000000,0.5555035]
  cd(0:1, 26 ) = [-102.0000000,0.5556677]
  cd(0:1, 27 ) = [-99.0000000,0.5553366]
  cd(0:1, 28 ) = [-96.0000000,0.5540151]
  cd(0:1, 29 ) = [-93.0000000,0.5526936]
  cd(0:1, 30 ) = [-90.0000000,0.5513720]
  cd(0:1, 31 ) = [-87.0000000,0.5526936]
  cd(0:1, 32 ) = [-84.0000000,0.5540151]
  cd(0:1, 33 ) = [-81.0000000,0.5553366]
  cd(0:1, 34 ) = [-78.0000000,0.5556677]
  cd(0:1, 35 ) = [-75.0000000,0.5555035]
  cd(0:1, 36 ) = [-72.0000000,0.5553394]
  cd(0:1, 37 ) = [-69.0000000,0.5547460]
  cd(0:1, 38 ) = [-66.0000000,0.5532942]
  cd(0:1, 39 ) = [-63.0000000,0.5518423]
  cd(0:1, 40 ) = [-60.0000000,0.5503904]
  cd(0:1, 41 ) = [-57.0000000,0.5479970]
  cd(0:1, 42 ) = [-54.0000000,0.5449039]
  cd(0:1, 43 ) = [-51.0000000,0.5390257]
  cd(0:1, 44 ) = [-48.0000000,0.5328022]
  cd(0:1, 45 ) = [-45.0000000,0.5264061]
  cd(0:1, 46 ) = [-42.0000000,0.5200100]
  cd(0:1, 47 ) = [-39.0000000,0.5137178]
  cd(0:1, 48 ) = [-36.0000000,0.5076337]
  cd(0:1, 49 ) = [-33.0000000,0.5015496]
  cd(0:1, 50 ) = [-30.0000000,0.4999651]
  cd(0:1, 51 ) = [-29.3939394,0.4999550]
  cd(0:1, 52 ) = [-28.7878788,0.4999442]
  cd(0:1, 53 ) = [-28.1818182,0.4999327]
  cd(0:1, 54 ) = [-27.5757576,0.4999205]
  cd(0:1, 55 ) = [-26.9696970,0.4999077]
  cd(0:1, 56 ) = [-26.3636364,0.4998943]
  cd(0:1, 57 ) = [-25.7575758,0.4998803]
  cd(0:1, 58 ) = [-25.1515152,0.4998658]
  cd(0:1, 59 ) = [-24.5454545,0.4998508]
  cd(0:1, 60 ) = [-23.9393939,0.4998353]
  cd(0:1, 61 ) = [-23.3333333,0.4998194]
  cd(0:1, 62 ) = [-22.7272727,0.4998031]
  cd(0:1, 63 ) = [-22.1212121,0.4997863]
  cd(0:1, 64 ) = [-21.5151515,0.4997691]
  cd(0:1, 65 ) = [-20.9090909,0.4997516]
  cd(0:1, 66 ) = [-20.3030303,0.4997337]
  cd(0:1, 67 ) = [-19.6969697,0.4997159]
  cd(0:1, 68 ) = [-19.0909091,0.4996981]
  cd(0:1, 69 ) = [-18.4848485,0.4996797]
  cd(0:1, 70 ) = [-17.8787879,0.4996606]
  cd(0:1, 71 ) = [-17.2727273,0.4996409]
  cd(0:1, 72 ) = [-16.6666667,0.4996206]
  cd(0:1, 73 ) = [-16.0606061,0.4995996]
  cd(0:1, 74 ) = [-15.4545455,0.4995779]
  cd(0:1, 75 ) = [-14.8484848,0.4995555]
  cd(0:1, 76 ) = [-14.2424242,0.4995325]
  cd(0:1, 77 ) = [-13.6363636,0.4995087]
  cd(0:1, 78 ) = [-13.0303030,0.4994841]
  cd(0:1, 79 ) = [-12.4242424,0.4994589]
  cd(0:1, 80 ) = [-11.8181818,0.4994328]
  cd(0:1, 81 ) = [-11.2121212,0.4994060]
  cd(0:1, 82 ) = [-10.6060606,0.4990527]
  cd(0:1, 83 ) = [-10.0000000,0.4979937]
  cd(0:1, 84 ) = [-9.3939394,0.4964871]
  cd(0:1, 85 ) = [-8.7878788,0.4949777]
  cd(0:1, 86 ) = [-8.1818182,0.4934670]
  cd(0:1, 87 ) = [-7.5757576,0.4919550]
  cd(0:1, 88 ) = [-6.9696970,0.4904402]
  cd(0:1, 89 ) = [-6.3636364,0.4889213]
  cd(0:1, 90 ) = [-5.7575758,0.4874018]
  cd(0:1, 91 ) = [-5.1515152,0.4858796]
  cd(0:1, 92 ) = [-4.5454545,0.4843539]
  cd(0:1, 93 ) = [-3.9393939,0.4828257]
  cd(0:1, 94 ) = [-3.3333333,0.4814856]
  cd(0:1, 95 ) = [-2.7272727,0.4810812]
  cd(0:1, 96 ) = [-2.1212121,0.4812053]
  cd(0:1, 97 ) = [-1.5151515,0.4813293]
  cd(0:1, 98 ) = [-0.9090909,0.4814504]
  cd(0:1, 99 ) = [-0.3030303,0.4815609]
  cd(0:1, 100) = [0.3030303,0.4816849]
  cd(0:1, 101) = [0.9090909,0.4818089]
  cd(0:1, 102) = [1.5151515,0.4819298]
  cd(0:1, 103) = [2.1212121,0.4819250]
  cd(0:1, 104) = [2.7272727,0.4818795]
  cd(0:1, 105) = [3.3333333,0.4818339]
  cd(0:1, 106) = [3.9393939,0.4817757]
  cd(0:1, 107) = [4.5454545,0.4817286]
  cd(0:1, 108) = [5.1515152,0.4816806]
  cd(0:1, 109) = [5.7575758,0.4816261]
  cd(0:1, 110) = [6.3636364,0.4815846]
  cd(0:1, 111) = [6.9696970,0.4815306]
  cd(0:1, 112) = [7.5757576,0.4814892]
  cd(0:1, 113) = [8.1818182,0.4814427]
  cd(0:1, 114) = [8.7878788,0.4814856]
  cd(0:1, 115) = [9.3939394,0.4817349]
  cd(0:1, 116) = [10.0000000,0.4820336]
  cd(0:1, 117) = [10.6060606,0.4863678]
  cd(0:1, 118) = [11.2121212,0.4909773]
  cd(0:1, 119) = [11.8181818,0.4945966]
  cd(0:1, 120) = [12.4242424,0.4976370]
  cd(0:1, 121) = [13.0303030,0.4994084]
  cd(0:1, 122) = [13.6363636,0.4994667]
  cd(0:1, 123) = [14.2424242,0.4995154]
  cd(0:1, 124) = [14.8484848,0.4995586]
  cd(0:1, 125) = [15.4545455,0.4995952]
  cd(0:1, 126) = [16.0606061,0.4996267]
  cd(0:1, 127) = [16.6666667,0.4996430]
  cd(0:1, 128) = [17.2727273,0.4996587]
  cd(0:1, 129) = [17.8787879,0.4996737]
  cd(0:1, 130) = [18.4848485,0.4996885]
  cd(0:1, 131) = [19.0909091,0.4997031]
  cd(0:1, 132) = [19.6969697,0.4997175]
  cd(0:1, 133) = [20.3030303,0.4997337]
  cd(0:1, 134) = [20.9090909,0.4997516]
  cd(0:1, 135) = [21.5151515,0.4997691]
  cd(0:1, 136) = [22.1212121,0.4997863]
  cd(0:1, 137) = [22.7272727,0.4998031]
  cd(0:1, 138) = [23.3333333,0.4998194]
  cd(0:1, 139) = [23.9393939,0.4998353]
  cd(0:1, 140) = [24.5454545,0.4998508]
  cd(0:1, 141) = [25.1515152,0.4998658]
  cd(0:1, 142) = [25.7575758,0.4998803]
  cd(0:1, 143) = [26.3636364,0.4998943]
  cd(0:1, 144) = [26.9696970,0.4999077]
  cd(0:1, 145) = [27.5757576,0.4999205]
  cd(0:1, 146) = [28.1818182,0.4999327]
  cd(0:1, 147) = [28.7878788,0.4999442]
  cd(0:1, 148) = [29.3939394,0.4999550]
  cd(0:1, 149) = [30.0000000,0.4999651]
  cd(0:1, 150) = [33.0000000,0.5015496]
  cd(0:1, 151) = [36.0000000,0.5076337]
  cd(0:1, 152) = [39.0000000,0.5137178]
  cd(0:1, 153) = [42.0000000,0.5200100]
  cd(0:1, 154) = [45.0000000,0.5264061]
  cd(0:1, 155) = [48.0000000,0.5328022]
  cd(0:1, 156) = [51.0000000,0.5390257]
  cd(0:1, 157) = [54.0000000,0.5449039]
  cd(0:1, 158) = [57.0000000,0.5479970]
  cd(0:1, 159) = [60.0000000,0.5503904]
  cd(0:1, 160) = [63.0000000,0.5518423]
  cd(0:1, 161) = [66.0000000,0.5532942]
  cd(0:1, 162) = [69.0000000,0.5547460]
  cd(0:1, 163) = [72.0000000,0.5553394]
  cd(0:1, 164) = [75.0000000,0.5555035]
  cd(0:1, 165) = [78.0000000,0.5556677]
  cd(0:1, 166) = [81.0000000,0.5553366]
  cd(0:1, 167) = [84.0000000,0.5540151]
  cd(0:1, 168) = [87.0000000,0.5526936]
  cd(0:1, 169) = [90.0000000,0.5513720]
  cd(0:1, 170) = [93.0000000,0.5526936]
  cd(0:1, 171) = [96.0000000,0.5540151]
  cd(0:1, 172) = [99.0000000,0.5553366]
  cd(0:1, 173) = [102.0000000,0.5556677]
  cd(0:1, 174) = [105.0000000,0.5555035]
  cd(0:1, 175) = [108.0000000,0.5553394]
  cd(0:1, 176) = [111.0000000,0.5547460]
  cd(0:1, 177) = [114.0000000,0.5532942]
  cd(0:1, 178) = [117.0000000,0.5518423]
  cd(0:1, 179) = [120.0000000,0.5503904]
  cd(0:1, 180) = [123.0000000,0.5479970]
  cd(0:1, 181) = [126.0000000,0.5449039]
  cd(0:1, 182) = [129.0000000,0.5390257]
  cd(0:1, 183) = [132.0000000,0.5328022]
  cd(0:1, 184) = [135.0000000,0.5264060]
  cd(0:1, 185) = [138.0000000,0.5200100]
  cd(0:1, 186) = [141.0000000,0.5137178]
  cd(0:1, 187) = [144.0000000,0.5076337]
  cd(0:1, 188) = [147.0000000,0.5015496]
  cd(0:1, 189) = [150.0000000,0.4999651]
  cd(0:1, 190) = [153.0000000,0.4999083]
  cd(0:1, 191) = [156.0000000,0.4998369]
  cd(0:1, 192) = [159.0000000,0.4997542]
  cd(0:1, 193) = [162.0000000,0.4996801]
  cd(0:1, 194) = [165.0000000,0.4996082]
  cd(0:1, 195) = [168.0000000,0.4995304]
  cd(0:1, 196) = [171.0000000,0.4994620]
  cd(0:1, 197) = [174.0000000,0.4994195]
  cd(0:1, 198) = [177.0000000,0.4993738]
  cd(0:1, 199) = [180.0000000,0.4982985]

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
!     PRINT*,'alpha < tabulated range in calc_cd2_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd2 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd2_ge28.pro'
  ENDIF

!print,y,cd2

!_________________________________________________________________________
  
END FUNCTION calc_cd2_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd3_ge28( y ) RESULT( cd3 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd3
  INTEGER :: i

  cd3 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.00000000,0.44399418]
  cd(0:1, 1  ) = [-177.00000000,0.45177895]
  cd(0:1, 2  ) = [-174.00000000,0.45484620]
  cd(0:1, 3  ) = [-171.00000000,0.45772169]
  cd(0:1, 4  ) = [-168.00000000,0.46259361]
  cd(0:1, 5  ) = [-165.00000000,0.46824188]
  cd(0:1, 6  ) = [-162.00000000,0.47354548]
  cd(0:1, 7  ) = [-159.00000000,0.47917066]
  cd(0:1, 8  ) = [-156.00000000,0.48566859]
  cd(0:1, 9  ) = [-153.00000000,0.49150204]
  cd(0:1, 10 ) = [-150.00000000,0.49647307]
  cd(0:1, 11 ) = [-147.00000000,0.51044967]
  cd(0:1, 12 ) = [-144.00000000,0.55147740]
  cd(0:1, 13 ) = [-141.00000000,0.59250545]
  cd(0:1, 14 ) = [-138.00000000,0.63493584]
  cd(0:1, 15 ) = [-135.00000000,0.67806740]
  cd(0:1, 16 ) = [-132.00000000,0.72119946]
  cd(0:1, 17 ) = [-129.00000000,0.76316729]
  cd(0:1, 18 ) = [-126.00000000,0.80280670]
  cd(0:1, 19 ) = [-123.00000000,0.83009514]
  cd(0:1, 20 ) = [-120.00000000,0.85428045]
  cd(0:1, 21 ) = [-117.00000000,0.87132271]
  cd(0:1, 22 ) = [-114.00000000,0.88836490]
  cd(0:1, 23 ) = [-111.00000000,0.90540696]
  cd(0:1, 24 ) = [-108.00000000,0.91569820]
  cd(0:1, 25 ) = [-105.00000000,0.92261398]
  cd(0:1, 26 ) = [-102.00000000,0.92952968]
  cd(0:1, 27 ) = [-99.00000000,0.93247726]
  cd(0:1, 28 ) = [-96.00000000,0.92748841]
  cd(0:1, 29 ) = [-93.00000000,0.92249960]
  cd(0:1, 30 ) = [-90.00000000,0.91751080]
  cd(0:1, 31 ) = [-87.00000000,0.92249959]
  cd(0:1, 32 ) = [-84.00000000,0.92748842]
  cd(0:1, 33 ) = [-81.00000000,0.93247722]
  cd(0:1, 34 ) = [-78.00000000,0.92952972]
  cd(0:1, 35 ) = [-75.00000000,0.92261393]
  cd(0:1, 36 ) = [-72.00000000,0.91569823]
  cd(0:1, 37 ) = [-69.00000000,0.90540710]
  cd(0:1, 38 ) = [-66.00000000,0.88836485]
  cd(0:1, 39 ) = [-63.00000000,0.87132260]
  cd(0:1, 40 ) = [-60.00000000,0.85428029]
  cd(0:1, 41 ) = [-57.00000000,0.83009498]
  cd(0:1, 42 ) = [-54.00000000,0.80280644]
  cd(0:1, 43 ) = [-51.00000000,0.76316703]
  cd(0:1, 44 ) = [-48.00000000,0.72119950]
  cd(0:1, 45 ) = [-45.00000000,0.67806794]
  cd(0:1, 46 ) = [-42.00000000,0.63493589]
  cd(0:1, 47 ) = [-39.00000000,0.59250542]
  cd(0:1, 48 ) = [-36.00000000,0.55147729]
  cd(0:1, 49 ) = [-33.00000000,0.51044994]
  cd(0:1, 50 ) = [-30.00000000,0.49647303]
  cd(0:1, 51 ) = [-29.39393939,0.49555048]
  cd(0:1, 52 ) = [-28.78787879,0.49458422]
  cd(0:1, 53 ) = [-28.18181818,0.49357674]
  cd(0:1, 54 ) = [-27.57575758,0.49253029]
  cd(0:1, 55 ) = [-26.96969697,0.49144704]
  cd(0:1, 56 ) = [-26.36363636,0.49032896]
  cd(0:1, 57 ) = [-25.75757576,0.48917790]
  cd(0:1, 58 ) = [-25.15151515,0.48799558]
  cd(0:1, 59 ) = [-24.54545455,0.48678357]
  cd(0:1, 60 ) = [-23.93939394,0.48554337]
  cd(0:1, 61 ) = [-23.33333333,0.48427636]
  cd(0:1, 62 ) = [-22.72727273,0.48298385]
  cd(0:1, 63 ) = [-22.12121212,0.48166704]
  cd(0:1, 64 ) = [-21.51515152,0.48032706]
  cd(0:1, 65 ) = [-20.90909091,0.47896499]
  cd(0:1, 66 ) = [-20.30303030,0.47758184]
  cd(0:1, 67 ) = [-19.69696970,0.47624168]
  cd(0:1, 68 ) = [-19.09090909,0.47493286]
  cd(0:1, 69 ) = [-18.48484848,0.47358777]
  cd(0:1, 70 ) = [-17.87878788,0.47220564]
  cd(0:1, 71 ) = [-17.27272727,0.47078574]
  cd(0:1, 72 ) = [-16.66666667,0.46932739]
  cd(0:1, 73 ) = [-16.06060606,0.46782987]
  cd(0:1, 74 ) = [-15.45454545,0.46629251]
  cd(0:1, 75 ) = [-14.84848485,0.46471445]
  cd(0:1, 76 ) = [-14.24242424,0.46309483]
  cd(0:1, 77 ) = [-13.63636364,0.46143391]
  cd(0:1, 78 ) = [-13.03030303,0.45972795]
  cd(0:1, 79 ) = [-12.42424242,0.45798006]
  cd(0:1, 80 ) = [-11.81818182,0.45618547]
  cd(0:1, 81 ) = [-11.21212121,0.45434602]
  cd(0:1, 82 ) = [-10.60606061,0.45101564]
  cd(0:1, 83 ) = [-10.00000000,0.44451035]
  cd(0:1, 84 ) = [-9.39393939,0.43560892]
  cd(0:1, 85 ) = [-8.78787879,0.42659451]
  cd(0:1, 86 ) = [-8.18181818,0.41746956]
  cd(0:1, 87 ) = [-7.57575758,0.40822757]
  cd(0:1, 88 ) = [-6.96969697,0.39885322]
  cd(0:1, 89 ) = [-6.36363636,0.38932922]
  cd(0:1, 90 ) = [-5.75757576,0.37966917]
  cd(0:1, 91 ) = [-5.15151515,0.36985172]
  cd(0:1, 92 ) = [-4.54545455,0.35985937]
  cd(0:1, 93 ) = [-3.93939394,0.34968975]
  cd(0:1, 94 ) = [-3.33333333,0.34062483]
  cd(0:1, 95 ) = [-2.72727273,0.33785158]
  cd(0:1, 96 ) = [-2.12121212,0.33869697]
  cd(0:1, 97 ) = [-1.51515152,0.33954089]
  cd(0:1, 98 ) = [-0.90909091,0.34036432]
  cd(0:1, 99 ) = [-0.30303030,0.34111990]
  cd(0:1, 100) = [0.30303030,0.34195980]
  cd(0:1, 101) = [0.90909091,0.34279838]
  cd(0:1, 102) = [1.51515152,0.34361573]
  cd(0:1, 103) = [2.12121212,0.34359004]
  cd(0:1, 104) = [2.72727273,0.34328261]
  cd(0:1, 105) = [3.33333333,0.34297502]
  cd(0:1, 106) = [3.93939394,0.34258802]
  cd(0:1, 107) = [4.54545455,0.34227034]
  cd(0:1, 108) = [5.15151515,0.34194757]
  cd(0:1, 109) = [5.75757576,0.34158343]
  cd(0:1, 110) = [6.36363636,0.34130219]
  cd(0:1, 111) = [6.96969697,0.34094262]
  cd(0:1, 112) = [7.57575758,0.34066239]
  cd(0:1, 113) = [8.18181818,0.34034935]
  cd(0:1, 114) = [8.78787879,0.34064923]
  cd(0:1, 115) = [9.39393939,0.34234727]
  cd(0:1, 116) = [10.00000000,0.34437418]
  cd(0:1, 117) = [10.60606061,0.37291811]
  cd(0:1, 118) = [11.21212121,0.40195106]
  cd(0:1, 119) = [11.81818182,0.42399209]
  cd(0:1, 120) = [12.42424242,0.44228312]
  cd(0:1, 121) = [13.03030303,0.45441948]
  cd(0:1, 122) = [13.63636364,0.45836092]
  cd(0:1, 123) = [14.24242424,0.46168480]
  cd(0:1, 124) = [14.84848485,0.46466357]
  cd(0:1, 125) = [15.45454545,0.46721159]
  cd(0:1, 126) = [16.06060606,0.46943376]
  cd(0:1, 127) = [16.66666667,0.47066576]
  cd(0:1, 128) = [17.27272727,0.47185330]
  cd(0:1, 129) = [17.87878788,0.47299075]
  cd(0:1, 130) = [18.48484848,0.47411645]
  cd(0:1, 131) = [19.09090909,0.47523100]
  cd(0:1, 132) = [19.69696970,0.47633507]
  cd(0:1, 133) = [20.30303030,0.47758184]
  cd(0:1, 134) = [20.90909091,0.47896499]
  cd(0:1, 135) = [21.51515152,0.48032706]
  cd(0:1, 136) = [22.12121212,0.48166704]
  cd(0:1, 137) = [22.72727273,0.48298385]
  cd(0:1, 138) = [23.33333333,0.48427636]
  cd(0:1, 139) = [23.93939394,0.48554337]
  cd(0:1, 140) = [24.54545455,0.48678357]
  cd(0:1, 141) = [25.15151515,0.48799558]
  cd(0:1, 142) = [25.75757576,0.48917790]
  cd(0:1, 143) = [26.36363636,0.49032896]
  cd(0:1, 144) = [26.96969697,0.49144704]
  cd(0:1, 145) = [27.57575758,0.49253029]
  cd(0:1, 146) = [28.18181818,0.49357674]
  cd(0:1, 147) = [28.78787879,0.49458422]
  cd(0:1, 148) = [29.39393939,0.49555048]
  cd(0:1, 149) = [30.00000000,0.49647303]
  cd(0:1, 150) = [33.00000000,0.51044994]
  cd(0:1, 151) = [36.00000000,0.55147729]
  cd(0:1, 152) = [39.00000000,0.59250542]
  cd(0:1, 153) = [42.00000000,0.63493589]
  cd(0:1, 154) = [45.00000000,0.67806794]
  cd(0:1, 155) = [48.00000000,0.72119950]
  cd(0:1, 156) = [51.00000000,0.76316703]
  cd(0:1, 157) = [54.00000000,0.80280644]
  cd(0:1, 158) = [57.00000000,0.83009498]
  cd(0:1, 159) = [60.00000000,0.85428029]
  cd(0:1, 160) = [63.00000000,0.87132260]
  cd(0:1, 161) = [66.00000000,0.88836485]
  cd(0:1, 162) = [69.00000000,0.90540710]
  cd(0:1, 163) = [72.00000000,0.91569823]
  cd(0:1, 164) = [75.00000000,0.92261393]
  cd(0:1, 165) = [78.00000000,0.92952972]
  cd(0:1, 166) = [81.00000000,0.93247722]
  cd(0:1, 167) = [84.00000000,0.92748842]
  cd(0:1, 168) = [87.00000000,0.92249959]
  cd(0:1, 169) = [90.00000000,0.91751080]
  cd(0:1, 170) = [93.00000000,0.92249960]
  cd(0:1, 171) = [96.00000000,0.92748841]
  cd(0:1, 172) = [99.00000000,0.93247726]
  cd(0:1, 173) = [102.00000000,0.92952968]
  cd(0:1, 174) = [105.00000000,0.92261398]
  cd(0:1, 175) = [108.00000000,0.91569820]
  cd(0:1, 176) = [111.00000000,0.90540696]
  cd(0:1, 177) = [114.00000000,0.88836490]
  cd(0:1, 178) = [117.00000000,0.87132271]
  cd(0:1, 179) = [120.00000000,0.85428045]
  cd(0:1, 180) = [123.00000000,0.83009514]
  cd(0:1, 181) = [126.00000000,0.80280670]
  cd(0:1, 182) = [129.00000000,0.76316729]
  cd(0:1, 183) = [132.00000000,0.72119946]
  cd(0:1, 184) = [135.00000000,0.67806740]
  cd(0:1, 185) = [138.00000000,0.63493584]
  cd(0:1, 186) = [141.00000000,0.59250545]
  cd(0:1, 187) = [144.00000000,0.55147740]
  cd(0:1, 188) = [147.00000000,0.51044967]
  cd(0:1, 189) = [150.00000000,0.49647307]
  cd(0:1, 190) = [153.00000000,0.49150204]
  cd(0:1, 191) = [156.00000000,0.48566859]
  cd(0:1, 192) = [159.00000000,0.47917066]
  cd(0:1, 193) = [162.00000000,0.47354548]
  cd(0:1, 194) = [165.00000000,0.46824188]
  cd(0:1, 195) = [168.00000000,0.46259361]
  cd(0:1, 196) = [171.00000000,0.45772169]
  cd(0:1, 197) = [174.00000000,0.45484620]
  cd(0:1, 198) = [177.00000000,0.45177895]
  cd(0:1, 199) = [180.00000000,0.44399418]

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
!     PRINT*,'alpha < tabulated range in calc_cd3_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd3 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd3_ge28.pro'
  ENDIF

!print,y,cd3

!_________________________________________________________________________
  
END FUNCTION calc_cd3_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd4_ge28( y ) RESULT( cd4 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd4
  INTEGER :: i

  cd4 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.00000000,0.28666794]
  cd(0:1, 1  ) = [-177.00000000,0.29809955]
  cd(0:1, 2  ) = [-174.00000000,0.30763237]
  cd(0:1, 3  ) = [-171.00000000,0.31677462]
  cd(0:1, 4  ) = [-168.00000000,0.33423437]
  cd(0:1, 5  ) = [-165.00000000,0.35541953]
  cd(0:1, 6  ) = [-162.00000000,0.37593165]
  cd(0:1, 7  ) = [-159.00000000,0.39884627]
  cd(0:1, 8  ) = [-156.00000000,0.42701422]
  cd(0:1, 9  ) = [-153.00000000,0.45388456]
  cd(0:1, 10 ) = [-150.00000000,0.47907067]
  cd(0:1, 11 ) = [-147.00000000,0.51461498]
  cd(0:1, 12 ) = [-144.00000000,0.57199667]
  cd(0:1, 13 ) = [-141.00000000,0.62937881]
  cd(0:1, 14 ) = [-138.00000000,0.68872226]
  cd(0:1, 15 ) = [-135.00000000,0.74904638]
  cd(0:1, 16 ) = [-132.00000000,0.80937118]
  cd(0:1, 17 ) = [-129.00000000,0.86806771]
  cd(0:1, 18 ) = [-126.00000000,0.92350768]
  cd(0:1, 19 ) = [-123.00000000,0.97458084]
  cd(0:1, 20 ) = [-120.00000000,1.02455678]
  cd(0:1, 21 ) = [-117.00000000,1.06294870]
  cd(0:1, 22 ) = [-114.00000000,1.10134045]
  cd(0:1, 23 ) = [-111.00000000,1.13973190]
  cd(0:1, 24 ) = [-108.00000000,1.16675110]
  cd(0:1, 25 ) = [-105.00000000,1.18808407]
  cd(0:1, 26 ) = [-102.00000000,1.20941680]
  cd(0:1, 27 ) = [-99.00000000,1.22393767]
  cd(0:1, 28 ) = [-96.00000000,1.22483454]
  cd(0:1, 29 ) = [-93.00000000,1.22573139]
  cd(0:1, 30 ) = [-90.00000000,1.22662825]
  cd(0:1, 31 ) = [-87.00000000,1.22573139]
  cd(0:1, 32 ) = [-84.00000000,1.22483453]
  cd(0:1, 33 ) = [-81.00000000,1.22393768]
  cd(0:1, 34 ) = [-78.00000000,1.20941690]
  cd(0:1, 35 ) = [-75.00000000,1.18808393]
  cd(0:1, 36 ) = [-72.00000000,1.16675120]
  cd(0:1, 37 ) = [-69.00000000,1.13973224]
  cd(0:1, 38 ) = [-66.00000000,1.10134034]
  cd(0:1, 39 ) = [-63.00000000,1.06294845]
  cd(0:1, 40 ) = [-60.00000000,1.02455645]
  cd(0:1, 41 ) = [-57.00000000,0.97458051]
  cd(0:1, 42 ) = [-54.00000000,0.92350731]
  cd(0:1, 43 ) = [-51.00000000,0.86806734]
  cd(0:1, 44 ) = [-48.00000000,0.80937124]
  cd(0:1, 45 ) = [-45.00000000,0.74904713]
  cd(0:1, 46 ) = [-42.00000000,0.68872232]
  cd(0:1, 47 ) = [-39.00000000,0.62937876]
  cd(0:1, 48 ) = [-36.00000000,0.57199651]
  cd(0:1, 49 ) = [-33.00000000,0.51461537]
  cd(0:1, 50 ) = [-30.00000000,0.47907049]
  cd(0:1, 51 ) = [-29.39393939,0.47414200]
  cd(0:1, 52 ) = [-28.78787879,0.46912814]
  cd(0:1, 53 ) = [-28.18181818,0.46403377]
  cd(0:1, 54 ) = [-27.57575758,0.45886326]
  cd(0:1, 55 ) = [-26.96969697,0.45362082]
  cd(0:1, 56 ) = [-26.36363636,0.44831037]
  cd(0:1, 57 ) = [-25.75757576,0.44293554]
  cd(0:1, 58 ) = [-25.15151515,0.43749972]
  cd(0:1, 59 ) = [-24.54545455,0.43200593]
  cd(0:1, 60 ) = [-23.93939394,0.42645707]
  cd(0:1, 61 ) = [-23.33333333,0.42085585]
  cd(0:1, 62 ) = [-22.72727273,0.41520482]
  cd(0:1, 63 ) = [-22.12121212,0.40950635]
  cd(0:1, 64 ) = [-21.51515152,0.40376254]
  cd(0:1, 65 ) = [-20.90909091,0.39797558]
  cd(0:1, 66 ) = [-20.30303030,0.39214746]
  cd(0:1, 67 ) = [-19.69696970,0.38673413]
  cd(0:1, 68 ) = [-19.09090909,0.38171285]
  cd(0:1, 69 ) = [-18.48484848,0.37662083]
  cd(0:1, 70 ) = [-17.87878788,0.37145650]
  cd(0:1, 71 ) = [-17.27272727,0.36621826]
  cd(0:1, 72 ) = [-16.66666667,0.36090495]
  cd(0:1, 73 ) = [-16.06060606,0.35551513]
  cd(0:1, 74 ) = [-15.45454545,0.35004763]
  cd(0:1, 75 ) = [-14.84848485,0.34450062]
  cd(0:1, 76 ) = [-14.24242424,0.33887234]
  cd(0:1, 77 ) = [-13.63636364,0.33316465]
  cd(0:1, 78 ) = [-13.03030303,0.32736661]
  cd(0:1, 79 ) = [-12.42424242,0.32148918]
  cd(0:1, 80 ) = [-11.81818182,0.31551835]
  cd(0:1, 81 ) = [-11.21212121,0.30946098]
  cd(0:1, 82 ) = [-10.60606061,0.30280264]
  cd(0:1, 83 ) = [-10.00000000,0.29494132]
  cd(0:1, 84 ) = [-9.39393939,0.28549834]
  cd(0:1, 85 ) = [-8.78787879,0.27583980]
  cd(0:1, 86 ) = [-8.18181818,0.26596015]
  cd(0:1, 87 ) = [-7.57575758,0.25584893]
  cd(0:1, 88 ) = [-6.96969697,0.24548296]
  cd(0:1, 89 ) = [-6.36363636,0.23483446]
  cd(0:1, 90 ) = [-5.75757576,0.22391027]
  cd(0:1, 91 ) = [-5.15151515,0.21267816]
  cd(0:1, 92 ) = [-4.54545455,0.20110615]
  cd(0:1, 93 ) = [-3.93939394,0.18918649]
  cd(0:1, 94 ) = [-3.33333333,0.17841655]
  cd(0:1, 95 ) = [-2.72727273,0.17505820]
  cd(0:1, 96 ) = [-2.12121212,0.17605608]
  cd(0:1, 97 ) = [-1.51515152,0.17705113]
  cd(0:1, 98 ) = [-0.90909091,0.17802612]
  cd(0:1, 99 ) = [-0.30303030,0.17893838]
  cd(0:1, 100) = [0.30303030,0.17992559]
  cd(0:1, 101) = [0.90909091,0.18091016]
  cd(0:1, 102) = [1.51515152,0.18186964]
  cd(0:1, 103) = [2.12121212,0.18186722]
  cd(0:1, 104) = [2.72727273,0.18150675]
  cd(0:1, 105) = [3.33333333,0.18114595]
  cd(0:1, 106) = [3.93939394,0.18071684]
  cd(0:1, 107) = [4.54545455,0.18034729]
  cd(0:1, 108) = [5.15151515,0.17997679]
  cd(0:1, 109) = [5.75757576,0.17956869]
  cd(0:1, 110) = [6.36363636,0.17923804]
  cd(0:1, 111) = [6.96969697,0.17884180]
  cd(0:1, 112) = [7.57575758,0.17851378]
  cd(0:1, 113) = [8.18181818,0.17815558]
  cd(0:1, 114) = [8.78787879,0.17854303]
  cd(0:1, 115) = [9.39393939,0.18057401]
  cd(0:1, 116) = [10.00000000,0.18298610]
  cd(0:1, 117) = [10.60606061,0.21585226]
  cd(0:1, 118) = [11.21212121,0.24810611]
  cd(0:1, 119) = [11.81818182,0.27199071]
  cd(0:1, 120) = [12.42424242,0.29215754]
  cd(0:1, 121) = [13.03030303,0.30897518]
  cd(0:1, 122) = [13.63636364,0.32145593]
  cd(0:1, 123) = [14.24242424,0.33226319]
  cd(0:1, 124) = [14.84848485,0.34219517]
  cd(0:1, 125) = [15.45454545,0.35089050]
  cd(0:1, 126) = [16.06060606,0.35871328]
  cd(0:1, 127) = [16.66666667,0.36366261]
  cd(0:1, 128) = [17.27272727,0.36847329]
  cd(0:1, 129) = [17.87878788,0.37312282]
  cd(0:1, 130) = [18.48484848,0.37774772]
  cd(0:1, 131) = [19.09090909,0.38235073]
  cd(0:1, 132) = [19.69696970,0.38693507]
  cd(0:1, 133) = [20.30303030,0.39214746]
  cd(0:1, 134) = [20.90909091,0.39797558]
  cd(0:1, 135) = [21.51515152,0.40376254]
  cd(0:1, 136) = [22.12121212,0.40950635]
  cd(0:1, 137) = [22.72727273,0.41520482]
  cd(0:1, 138) = [23.33333333,0.42085585]
  cd(0:1, 139) = [23.93939394,0.42645707]
  cd(0:1, 140) = [24.54545455,0.43200593]
  cd(0:1, 141) = [25.15151515,0.43749972]
  cd(0:1, 142) = [25.75757576,0.44293554]
  cd(0:1, 143) = [26.36363636,0.44831037]
  cd(0:1, 144) = [26.96969697,0.45362082]
  cd(0:1, 145) = [27.57575758,0.45886326]
  cd(0:1, 146) = [28.18181818,0.46403377]
  cd(0:1, 147) = [28.78787879,0.46912814]
  cd(0:1, 148) = [29.39393939,0.47414200]
  cd(0:1, 149) = [30.00000000,0.47907049]
  cd(0:1, 150) = [33.00000000,0.51461537]
  cd(0:1, 151) = [36.00000000,0.57199651]
  cd(0:1, 152) = [39.00000000,0.62937876]
  cd(0:1, 153) = [42.00000000,0.68872232]
  cd(0:1, 154) = [45.00000000,0.74904713]
  cd(0:1, 155) = [48.00000000,0.80937124]
  cd(0:1, 156) = [51.00000000,0.86806734]
  cd(0:1, 157) = [54.00000000,0.92350731]
  cd(0:1, 158) = [57.00000000,0.97458051]
  cd(0:1, 159) = [60.00000000,1.02455645]
  cd(0:1, 160) = [63.00000000,1.06294845]
  cd(0:1, 161) = [66.00000000,1.10134034]
  cd(0:1, 162) = [69.00000000,1.13973224]
  cd(0:1, 163) = [72.00000000,1.16675120]
  cd(0:1, 164) = [75.00000000,1.18808393]
  cd(0:1, 165) = [78.00000000,1.20941690]
  cd(0:1, 166) = [81.00000000,1.22393768]
  cd(0:1, 167) = [84.00000000,1.22483453]
  cd(0:1, 168) = [87.00000000,1.22573139]
  cd(0:1, 169) = [90.00000000,1.22662825]
  cd(0:1, 170) = [93.00000000,1.22573139]
  cd(0:1, 171) = [96.00000000,1.22483454]
  cd(0:1, 172) = [99.00000000,1.22393767]
  cd(0:1, 173) = [102.00000000,1.20941680]
  cd(0:1, 174) = [105.00000000,1.18808407]
  cd(0:1, 175) = [108.00000000,1.16675110]
  cd(0:1, 176) = [111.00000000,1.13973190]
  cd(0:1, 177) = [114.00000000,1.10134045]
  cd(0:1, 178) = [117.00000000,1.06294870]
  cd(0:1, 179) = [120.00000000,1.02455678]
  cd(0:1, 180) = [123.00000000,0.97458084]
  cd(0:1, 181) = [126.00000000,0.92350768]
  cd(0:1, 182) = [129.00000000,0.86806771]
  cd(0:1, 183) = [132.00000000,0.80937118]
  cd(0:1, 184) = [135.00000000,0.74904638]
  cd(0:1, 185) = [138.00000000,0.68872226]
  cd(0:1, 186) = [141.00000000,0.62937881]
  cd(0:1, 187) = [144.00000000,0.57199667]
  cd(0:1, 188) = [147.00000000,0.51461498]
  cd(0:1, 189) = [150.00000000,0.47907067]
  cd(0:1, 190) = [153.00000000,0.45388456]
  cd(0:1, 191) = [156.00000000,0.42701422]
  cd(0:1, 192) = [159.00000000,0.39884627]
  cd(0:1, 193) = [162.00000000,0.37593165]
  cd(0:1, 194) = [165.00000000,0.35541953]
  cd(0:1, 195) = [168.00000000,0.33423437]
  cd(0:1, 196) = [171.00000000,0.31677462]
  cd(0:1, 197) = [174.00000000,0.30763237]
  cd(0:1, 198) = [177.00000000,0.29809955]
  cd(0:1, 199) = [180.00000000,0.28666794]

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
!     PRINT*,'alpha < tabulated range in calc_cd4_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd4 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd4_ge28.pro'
  ENDIF

!print,y,cd4

!_________________________________________________________________________
  
END FUNCTION calc_cd4_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd5_ge28( y ) RESULT( cd5 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd5
  INTEGER :: i

  cd5 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.000000000,0.152890685]
  cd(0:1, 1  ) = [-177.000000000,0.163763724]
  cd(0:1, 2  ) = [-174.000000000,0.174744684]
  cd(0:1, 3  ) = [-171.000000000,0.185683483]
  cd(0:1, 4  ) = [-168.000000000,0.210410090]
  cd(0:1, 5  ) = [-165.000000000,0.242039936]
  cd(0:1, 6  ) = [-162.000000000,0.273684904]
  cd(0:1, 7  ) = [-159.000000000,0.310889487]
  cd(0:1, 8  ) = [-156.000000000,0.359203285]
  cd(0:1, 9  ) = [-153.000000000,0.407545900]
  cd(0:1, 10 ) = [-150.000000000,0.455925648]
  cd(0:1, 11 ) = [-147.000000000,0.514758887]
  cd(0:1, 12 ) = [-144.000000000,0.573263830]
  cd(0:1, 13 ) = [-141.000000000,0.631769220]
  cd(0:1, 14 ) = [-138.000000000,0.692274906]
  cd(0:1, 15 ) = [-135.000000000,0.753780736]
  cd(0:1, 16 ) = [-132.000000000,0.815287271]
  cd(0:1, 17 ) = [-129.000000000,0.875134137]
  cd(0:1, 18 ) = [-126.000000000,0.931661657]
  cd(0:1, 19 ) = [-123.000000000,0.988188745]
  cd(0:1, 20 ) = [-120.000000000,1.044715616]
  cd(0:1, 21 ) = [-117.000000000,1.088883871]
  cd(0:1, 22 ) = [-114.000000000,1.133051932]
  cd(0:1, 23 ) = [-111.000000000,1.177219657]
  cd(0:1, 24 ) = [-108.000000000,1.209128060]
  cd(0:1, 25 ) = [-105.000000000,1.234906711]
  cd(0:1, 26 ) = [-102.000000000,1.260685065]
  cd(0:1, 27 ) = [-99.000000000,1.279083672]
  cd(0:1, 28 ) = [-96.000000000,1.282722464]
  cd(0:1, 29 ) = [-93.000000000,1.286361227]
  cd(0:1, 30 ) = [-90.000000000,1.289999977]
  cd(0:1, 31 ) = [-87.000000000,1.286361231]
  cd(0:1, 32 ) = [-84.000000000,1.282722454]
  cd(0:1, 33 ) = [-81.000000000,1.279083704]
  cd(0:1, 34 ) = [-78.000000000,1.260685190]
  cd(0:1, 35 ) = [-75.000000000,1.234906540]
  cd(0:1, 36 ) = [-72.000000000,1.209128185]
  cd(0:1, 37 ) = [-69.000000000,1.177220039]
  cd(0:1, 38 ) = [-66.000000000,1.133051809]
  cd(0:1, 39 ) = [-63.000000000,1.088883579]
  cd(0:1, 40 ) = [-60.000000000,1.044715242]
  cd(0:1, 41 ) = [-57.000000000,0.988188371]
  cd(0:1, 42 ) = [-54.000000000,0.931661283]
  cd(0:1, 43 ) = [-51.000000000,0.875133763]
  cd(0:1, 44 ) = [-48.000000000,0.815287334]
  cd(0:1, 45 ) = [-45.000000000,0.753781504]
  cd(0:1, 46 ) = [-42.000000000,0.692274969]
  cd(0:1, 47 ) = [-39.000000000,0.631769169]
  cd(0:1, 48 ) = [-36.000000000,0.573263667]
  cd(0:1, 49 ) = [-33.000000000,0.514759282]
  cd(0:1, 50 ) = [-30.000000000,0.455925303]
  cd(0:1, 51 ) = [-29.393939394,0.446148293]
  cd(0:1, 52 ) = [-28.787878788,0.436373154]
  cd(0:1, 53 ) = [-28.181818182,0.426599780]
  cd(0:1, 54 ) = [-27.575757576,0.416827877]
  cd(0:1, 55 ) = [-26.969696970,0.407057465]
  cd(0:1, 56 ) = [-26.363636364,0.397288545]
  cd(0:1, 57 ) = [-25.757575758,0.387521149]
  cd(0:1, 58 ) = [-25.151515152,0.377755260]
  cd(0:1, 59 ) = [-24.545454545,0.367990641]
  cd(0:1, 60 ) = [-23.939393939,0.358227230]
  cd(0:1, 61 ) = [-23.333333333,0.348464967]
  cd(0:1, 62 ) = [-22.727272727,0.338703795]
  cd(0:1, 63 ) = [-22.121212121,0.328943663]
  cd(0:1, 64 ) = [-21.515151515,0.319184299]
  cd(0:1, 65 ) = [-20.909090909,0.309425824]
  cd(0:1, 66 ) = [-20.303030303,0.299668251]
  cd(0:1, 67 ) = [-19.696969697,0.290949411]
  cd(0:1, 68 ) = [-19.090909091,0.283269743]
  cd(0:1, 69 ) = [-18.484848485,0.275592004]
  cd(0:1, 70 ) = [-17.878787879,0.267915829]
  cd(0:1, 71 ) = [-17.272727273,0.260240916]
  cd(0:1, 72 ) = [-16.666666667,0.252567649]
  cd(0:1, 73 ) = [-16.060606061,0.244896060]
  cd(0:1, 74 ) = [-15.454545455,0.237226574]
  cd(0:1, 75 ) = [-14.848484848,0.229558765]
  cd(0:1, 76 ) = [-14.242424242,0.221892403]
  cd(0:1, 77 ) = [-13.636363636,0.214231748]
  cd(0:1, 78 ) = [-13.030303030,0.206565470]
  cd(0:1, 79 ) = [-12.424242424,0.198908867]
  cd(0:1, 80 ) = [-11.818181818,0.191247369]
  cd(0:1, 81 ) = [-11.212121212,0.183591167]
  cd(0:1, 82 ) = [-10.606060606,0.175937905]
  cd(0:1, 83 ) = [-10.000000000,0.168281357]
  cd(0:1, 84 ) = [-9.393939394,0.160570609]
  cd(0:1, 85 ) = [-8.787878788,0.152859809]
  cd(0:1, 86 ) = [-8.181818182,0.145152920]
  cd(0:1, 87 ) = [-7.575757576,0.137458302]
  cd(0:1, 88 ) = [-6.969696970,0.129770338]
  cd(0:1, 89 ) = [-6.363636364,0.122078681]
  cd(0:1, 90 ) = [-5.757575758,0.114401643]
  cd(0:1, 91 ) = [-5.151515152,0.106731126]
  cd(0:1, 92 ) = [-4.545454545,0.099057764]
  cd(0:1, 93 ) = [-3.939393939,0.091400638]
  cd(0:1, 94 ) = [-3.333333333,0.084657083]
  cd(0:1, 95 ) = [-2.727272727,0.082527395]
  cd(0:1, 96 ) = [-2.121212121,0.083114175]
  cd(0:1, 97 ) = [-1.515151515,0.083700818]
  cd(0:1, 98 ) = [-0.909090909,0.084285919]
  cd(0:1, 99 ) = [-0.303030303,0.084865644]
  cd(0:1, 100) = [0.303030303,0.085452333]
  cd(0:1, 101) = [0.909090909,0.086039104]
  cd(0:1, 102) = [1.515151515,0.086613885]
  cd(0:1, 103) = [2.121212121,0.086658195]
  cd(0:1, 104) = [2.727272727,0.086442533]
  cd(0:1, 105) = [3.333333333,0.086226865]
  cd(0:1, 106) = [3.939393939,0.086012334]
  cd(0:1, 107) = [4.545454545,0.085797667]
  cd(0:1, 108) = [5.151515152,0.085591312]
  cd(0:1, 109) = [5.757575758,0.085381616]
  cd(0:1, 110) = [6.363636364,0.085184991]
  cd(0:1, 111) = [6.969696970,0.084994404]
  cd(0:1, 112) = [7.575757576,0.084802530]
  cd(0:1, 113) = [8.181818182,0.084607409]
  cd(0:1, 114) = [8.787878788,0.084893958]
  cd(0:1, 115) = [9.393939394,0.086156213]
  cd(0:1, 116) = [10.000000000,0.087652781]
  cd(0:1, 117) = [10.606060606,0.108368637]
  cd(0:1, 118) = [11.212121212,0.130650271]
  cd(0:1, 119) = [11.818181818,0.148602204]
  cd(0:1, 120) = [12.424242424,0.165510670]
  cd(0:1, 121) = [13.030303030,0.181609086]
  cd(0:1, 122) = [13.636363636,0.196445539]
  cd(0:1, 123) = [14.242424242,0.209842058]
  cd(0:1, 124) = [14.848484848,0.222621761]
  cd(0:1, 125) = [15.454545455,0.234179209]
  cd(0:1, 126) = [16.060606061,0.245010339]
  cd(0:1, 127) = [16.666666667,0.252941797]
  cd(0:1, 128) = [17.272727273,0.260712604]
  cd(0:1, 129) = [17.878787879,0.268286876]
  cd(0:1, 130) = [18.484848485,0.275856403]
  cd(0:1, 131) = [19.090909091,0.283426080]
  cd(0:1, 132) = [19.696969697,0.291001781]
  cd(0:1, 133) = [20.303030303,0.299668251]
  cd(0:1, 134) = [20.909090909,0.309425824]
  cd(0:1, 135) = [21.515151515,0.319184299]
  cd(0:1, 136) = [22.121212121,0.328943663]
  cd(0:1, 137) = [22.727272727,0.338703795]
  cd(0:1, 138) = [23.333333333,0.348464967]
  cd(0:1, 139) = [23.939393939,0.358227230]
  cd(0:1, 140) = [24.545454545,0.367990641]
  cd(0:1, 141) = [25.151515152,0.377755260]
  cd(0:1, 142) = [25.757575758,0.387521149]
  cd(0:1, 143) = [26.363636364,0.397288545]
  cd(0:1, 144) = [26.969696970,0.407057465]
  cd(0:1, 145) = [27.575757576,0.416827877]
  cd(0:1, 146) = [28.181818182,0.426599780]
  cd(0:1, 147) = [28.787878788,0.436373154]
  cd(0:1, 148) = [29.393939394,0.446148293]
  cd(0:1, 149) = [30.000000000,0.455925303]
  cd(0:1, 150) = [33.000000000,0.514759282]
  cd(0:1, 151) = [36.000000000,0.573263667]
  cd(0:1, 152) = [39.000000000,0.631769169]
  cd(0:1, 153) = [42.000000000,0.692274969]
  cd(0:1, 154) = [45.000000000,0.753781504]
  cd(0:1, 155) = [48.000000000,0.815287334]
  cd(0:1, 156) = [51.000000000,0.875133763]
  cd(0:1, 157) = [54.000000000,0.931661283]
  cd(0:1, 158) = [57.000000000,0.988188371]
  cd(0:1, 159) = [60.000000000,1.044715242]
  cd(0:1, 160) = [63.000000000,1.088883579]
  cd(0:1, 161) = [66.000000000,1.133051809]
  cd(0:1, 162) = [69.000000000,1.177220039]
  cd(0:1, 163) = [72.000000000,1.209128185]
  cd(0:1, 164) = [75.000000000,1.234906540]
  cd(0:1, 165) = [78.000000000,1.260685190]
  cd(0:1, 166) = [81.000000000,1.279083704]
  cd(0:1, 167) = [84.000000000,1.282722454]
  cd(0:1, 168) = [87.000000000,1.286361231]
  cd(0:1, 169) = [90.000000000,1.289999977]
  cd(0:1, 170) = [93.000000000,1.286361227]
  cd(0:1, 171) = [96.000000000,1.282722464]
  cd(0:1, 172) = [99.000000000,1.279083672]
  cd(0:1, 173) = [102.000000000,1.260685065]
  cd(0:1, 174) = [105.000000000,1.234906711]
  cd(0:1, 175) = [108.000000000,1.209128060]
  cd(0:1, 176) = [111.000000000,1.177219657]
  cd(0:1, 177) = [114.000000000,1.133051932]
  cd(0:1, 178) = [117.000000000,1.088883871]
  cd(0:1, 179) = [120.000000000,1.044715616]
  cd(0:1, 180) = [123.000000000,0.988188745]
  cd(0:1, 181) = [126.000000000,0.931661657]
  cd(0:1, 182) = [129.000000000,0.875134137]
  cd(0:1, 183) = [132.000000000,0.815287271]
  cd(0:1, 184) = [135.000000000,0.753780736]
  cd(0:1, 185) = [138.000000000,0.692274906]
  cd(0:1, 186) = [141.000000000,0.631769220]
  cd(0:1, 187) = [144.000000000,0.573263830]
  cd(0:1, 188) = [147.000000000,0.514758887]
  cd(0:1, 189) = [150.000000000,0.455925648]
  cd(0:1, 190) = [153.000000000,0.407545900]
  cd(0:1, 191) = [156.000000000,0.359203285]
  cd(0:1, 192) = [159.000000000,0.310889487]
  cd(0:1, 193) = [162.000000000,0.273684904]
  cd(0:1, 194) = [165.000000000,0.242039936]
  cd(0:1, 195) = [168.000000000,0.210410090]
  cd(0:1, 196) = [171.000000000,0.185683483]
  cd(0:1, 197) = [174.000000000,0.174744684]
  cd(0:1, 198) = [177.000000000,0.163763724]
  cd(0:1, 199) = [180.000000000,0.152890685]

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
!     PRINT*,'alpha < tabulated range in calc_cd5_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd5 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd5_ge28.pro'
  ENDIF

!print,y,cd5

!_________________________________________________________________________
  
END FUNCTION calc_cd5_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd6_ge28( y ) RESULT( cd6 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd6
  INTEGER :: i

  cd6 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.00000000,0.05309279]
  cd(0:1, 1  ) = [-177.00000000,0.05698512]
  cd(0:1, 2  ) = [-174.00000000,0.06096567]
  cd(0:1, 3  ) = [-171.00000000,0.06497633]
  cd(0:1, 4  ) = [-168.00000000,0.08509314]
  cd(0:1, 5  ) = [-165.00000000,0.11332290]
  cd(0:1, 6  ) = [-162.00000000,0.14164632]
  cd(0:1, 7  ) = [-159.00000000,0.17809596]
  cd(0:1, 8  ) = [-156.00000000,0.23073561]
  cd(0:1, 9  ) = [-153.00000000,0.28355558]
  cd(0:1, 10 ) = [-150.00000000,0.33660928]
  cd(0:1, 11 ) = [-147.00000000,0.40128305]
  cd(0:1, 12 ) = [-144.00000000,0.46387948]
  cd(0:1, 13 ) = [-141.00000000,0.52647638]
  cd(0:1, 14 ) = [-138.00000000,0.59167970]
  cd(0:1, 15 ) = [-135.00000000,0.65818621]
  cd(0:1, 16 ) = [-132.00000000,0.72469349]
  cd(0:1, 17 ) = [-129.00000000,0.78981260]
  cd(0:1, 18 ) = [-126.00000000,0.85215538]
  cd(0:1, 19 ) = [-123.00000000,0.91449768]
  cd(0:1, 20 ) = [-120.00000000,0.97683974]
  cd(0:1, 21 ) = [-117.00000000,1.02744834]
  cd(0:1, 22 ) = [-114.00000000,1.07805673]
  cd(0:1, 23 ) = [-111.00000000,1.12866472]
  cd(0:1, 24 ) = [-108.00000000,1.16729127]
  cd(0:1, 25 ) = [-105.00000000,1.19992700]
  cd(0:1, 26 ) = [-102.00000000,1.23256235]
  cd(0:1, 27 ) = [-99.00000000,1.25788744]
  cd(0:1, 28 ) = [-96.00000000,1.26859167]
  cd(0:1, 29 ) = [-93.00000000,1.27929582]
  cd(0:1, 30 ) = [-90.00000000,1.28999993]
  cd(0:1, 31 ) = [-87.00000000,1.27929583]
  cd(0:1, 32 ) = [-84.00000000,1.26859164]
  cd(0:1, 33 ) = [-81.00000000,1.25788753]
  cd(0:1, 34 ) = [-78.00000000,1.23256251]
  cd(0:1, 35 ) = [-75.00000000,1.19992678]
  cd(0:1, 36 ) = [-72.00000000,1.16729143]
  cd(0:1, 37 ) = [-69.00000000,1.12866516]
  cd(0:1, 38 ) = [-66.00000000,1.07805658]
  cd(0:1, 39 ) = [-63.00000000,1.02744800]
  cd(0:1, 40 ) = [-60.00000000,0.97683932]
  cd(0:1, 41 ) = [-57.00000000,0.91449726]
  cd(0:1, 42 ) = [-54.00000000,0.85215496]
  cd(0:1, 43 ) = [-51.00000000,0.78981219]
  cd(0:1, 44 ) = [-48.00000000,0.72469356]
  cd(0:1, 45 ) = [-45.00000000,0.65818704]
  cd(0:1, 46 ) = [-42.00000000,0.59167977]
  cd(0:1, 47 ) = [-39.00000000,0.52647633]
  cd(0:1, 48 ) = [-36.00000000,0.46387930]
  cd(0:1, 49 ) = [-33.00000000,0.40128347]
  cd(0:1, 50 ) = [-30.00000000,0.33660890]
  cd(0:1, 51 ) = [-29.39393939,0.32586906]
  cd(0:1, 52 ) = [-28.78787879,0.31514105]
  cd(0:1, 53 ) = [-28.18181818,0.30442420]
  cd(0:1, 54 ) = [-27.57575758,0.29371768]
  cd(0:1, 55 ) = [-26.96969697,0.28302103]
  cd(0:1, 56 ) = [-26.36363636,0.27233381]
  cd(0:1, 57 ) = [-25.75757576,0.26165565]
  cd(0:1, 58 ) = [-25.15151515,0.25098614]
  cd(0:1, 59 ) = [-24.54545455,0.24032466]
  cd(0:1, 60 ) = [-23.93939394,0.22967082]
  cd(0:1, 61 ) = [-23.33333333,0.21902423]
  cd(0:1, 62 ) = [-22.72727273,0.20838456]
  cd(0:1, 63 ) = [-22.12121212,0.19775145]
  cd(0:1, 64 ) = [-21.51515152,0.18712437]
  cd(0:1, 65 ) = [-20.90909091,0.17650321]
  cd(0:1, 66 ) = [-20.30303030,0.16588776]
  cd(0:1, 67 ) = [-19.69696970,0.15793614]
  cd(0:1, 68 ) = [-19.09090909,0.15265137]
  cd(0:1, 69 ) = [-18.48484848,0.14737669]
  cd(0:1, 70 ) = [-17.87878788,0.14211203]
  cd(0:1, 71 ) = [-17.27272727,0.13685733]
  cd(0:1, 72 ) = [-16.66666667,0.13161304]
  cd(0:1, 73 ) = [-16.06060606,0.12637936]
  cd(0:1, 74 ) = [-15.45454545,0.12115675]
  cd(0:1, 75 ) = [-14.84848485,0.11594512]
  cd(0:1, 76 ) = [-14.24242424,0.11074449]
  cd(0:1, 77 ) = [-13.63636364,0.10555683]
  cd(0:1, 78 ) = [-13.03030303,0.10037794]
  cd(0:1, 79 ) = [-12.42424242,0.09521398]
  cd(0:1, 80 ) = [-11.81818182,0.09005954]
  cd(0:1, 81 ) = [-11.21212121,0.08491888]
  cd(0:1, 82 ) = [-10.60606061,0.07979103]
  cd(0:1, 83 ) = [-10.00000000,0.07467432]
  cd(0:1, 84 ) = [-9.39393939,0.07157612]
  cd(0:1, 85 ) = [-8.78787879,0.06850534]
  cd(0:1, 86 ) = [-8.18181818,0.06546091]
  cd(0:1, 87 ) = [-7.57575758,0.06351950]
  cd(0:1, 88 ) = [-6.96969697,0.06226467]
  cd(0:1, 89 ) = [-6.36363636,0.06060800]
  cd(0:1, 90 ) = [-5.75757576,0.05860728]
  cd(0:1, 91 ) = [-5.15151515,0.05622558]
  cd(0:1, 92 ) = [-4.54545455,0.05369294]
  cd(0:1, 93 ) = [-3.93939394,0.05107914]
  cd(0:1, 94 ) = [-3.33333333,0.04846720]
  cd(0:1, 95 ) = [-2.72727273,0.04763182]
  cd(0:1, 96 ) = [-2.12121212,0.04799518]
  cd(0:1, 97 ) = [-1.51515152,0.04826547]
  cd(0:1, 98 ) = [-0.90909091,0.04852549]
  cd(0:1, 99 ) = [-0.30303030,0.04881986]
  cd(0:1, 100) = [0.30303030,0.04903034]
  cd(0:1, 101) = [0.90909091,0.04922839]
  cd(0:1, 102) = [1.51515152,0.04938132]
  cd(0:1, 103) = [2.12121212,0.04943466]
  cd(0:1, 104) = [2.72727273,0.04931293]
  cd(0:1, 105) = [3.33333333,0.04917551]
  cd(0:1, 106) = [3.93939394,0.04910566]
  cd(0:1, 107) = [4.54545455,0.04893987]
  cd(0:1, 108) = [5.15151515,0.04878861]
  cd(0:1, 109) = [5.75757576,0.04866818]
  cd(0:1, 110) = [6.36363636,0.04845803]
  cd(0:1, 111) = [6.96969697,0.04831671]
  cd(0:1, 112) = [7.57575758,0.04803320]
  cd(0:1, 113) = [8.18181818,0.04774341]
  cd(0:1, 114) = [8.78787879,0.04762086]
  cd(0:1, 115) = [9.39393939,0.04741257]
  cd(0:1, 116) = [10.00000000,0.04695924]
  cd(0:1, 117) = [10.60606061,0.05177956]
  cd(0:1, 118) = [11.21212121,0.05811859]
  cd(0:1, 119) = [11.81818182,0.06470409]
  cd(0:1, 120) = [12.42424242,0.07270111]
  cd(0:1, 121) = [13.03030303,0.07995816]
  cd(0:1, 122) = [13.63636364,0.08642525]
  cd(0:1, 123) = [14.24242424,0.09218336]
  cd(0:1, 124) = [14.84848485,0.09757769]
  cd(0:1, 125) = [15.45454545,0.10229297]
  cd(0:1, 126) = [16.06060606,0.10718177]
  cd(0:1, 127) = [16.66666667,0.11549382]
  cd(0:1, 128) = [17.27272727,0.12374753]
  cd(0:1, 129) = [17.87878788,0.13192878]
  cd(0:1, 130) = [18.48484848,0.14011070]
  cd(0:1, 131) = [19.09090909,0.14829549]
  cd(0:1, 132) = [19.69696970,0.15648578]
  cd(0:1, 133) = [20.30303030,0.16588776]
  cd(0:1, 134) = [20.90909091,0.17650321]
  cd(0:1, 135) = [21.51515152,0.18712437]
  cd(0:1, 136) = [22.12121212,0.19775145]
  cd(0:1, 137) = [22.72727273,0.20838456]
  cd(0:1, 138) = [23.33333333,0.21902423]
  cd(0:1, 139) = [23.93939394,0.22967082]
  cd(0:1, 140) = [24.54545455,0.24032466]
  cd(0:1, 141) = [25.15151515,0.25098614]
  cd(0:1, 142) = [25.75757576,0.26165565]
  cd(0:1, 143) = [26.36363636,0.27233381]
  cd(0:1, 144) = [26.96969697,0.28302103]
  cd(0:1, 145) = [27.57575758,0.29371768]
  cd(0:1, 146) = [28.18181818,0.30442420]
  cd(0:1, 147) = [28.78787879,0.31514105]
  cd(0:1, 148) = [29.39393939,0.32586906]
  cd(0:1, 149) = [30.00000000,0.33660890]
  cd(0:1, 150) = [33.00000000,0.40128347]
  cd(0:1, 151) = [36.00000000,0.46387930]
  cd(0:1, 152) = [39.00000000,0.52647633]
  cd(0:1, 153) = [42.00000000,0.59167977]
  cd(0:1, 154) = [45.00000000,0.65818704]
  cd(0:1, 155) = [48.00000000,0.72469356]
  cd(0:1, 156) = [51.00000000,0.78981219]
  cd(0:1, 157) = [54.00000000,0.85215496]
  cd(0:1, 158) = [57.00000000,0.91449726]
  cd(0:1, 159) = [60.00000000,0.97683932]
  cd(0:1, 160) = [63.00000000,1.02744800]
  cd(0:1, 161) = [66.00000000,1.07805658]
  cd(0:1, 162) = [69.00000000,1.12866516]
  cd(0:1, 163) = [72.00000000,1.16729143]
  cd(0:1, 164) = [75.00000000,1.19992678]
  cd(0:1, 165) = [78.00000000,1.23256251]
  cd(0:1, 166) = [81.00000000,1.25788753]
  cd(0:1, 167) = [84.00000000,1.26859164]
  cd(0:1, 168) = [87.00000000,1.27929583]
  cd(0:1, 169) = [90.00000000,1.28999993]
  cd(0:1, 170) = [93.00000000,1.27929582]
  cd(0:1, 171) = [96.00000000,1.26859167]
  cd(0:1, 172) = [99.00000000,1.25788744]
  cd(0:1, 173) = [102.00000000,1.23256235]
  cd(0:1, 174) = [105.00000000,1.19992700]
  cd(0:1, 175) = [108.00000000,1.16729127]
  cd(0:1, 176) = [111.00000000,1.12866472]
  cd(0:1, 177) = [114.00000000,1.07805673]
  cd(0:1, 178) = [117.00000000,1.02744834]
  cd(0:1, 179) = [120.00000000,0.97683974]
  cd(0:1, 180) = [123.00000000,0.91449768]
  cd(0:1, 181) = [126.00000000,0.85215538]
  cd(0:1, 182) = [129.00000000,0.78981260]
  cd(0:1, 183) = [132.00000000,0.72469349]
  cd(0:1, 184) = [135.00000000,0.65818621]
  cd(0:1, 185) = [138.00000000,0.59167970]
  cd(0:1, 186) = [141.00000000,0.52647638]
  cd(0:1, 187) = [144.00000000,0.46387948]
  cd(0:1, 188) = [147.00000000,0.40128305]
  cd(0:1, 189) = [150.00000000,0.33660928]
  cd(0:1, 190) = [153.00000000,0.28355558]
  cd(0:1, 191) = [156.00000000,0.23073561]
  cd(0:1, 192) = [159.00000000,0.17809596]
  cd(0:1, 193) = [162.00000000,0.14164632]
  cd(0:1, 194) = [165.00000000,0.11332290]
  cd(0:1, 195) = [168.00000000,0.08509314]
  cd(0:1, 196) = [171.00000000,0.06497633]
  cd(0:1, 197) = [174.00000000,0.06096567]
  cd(0:1, 198) = [177.00000000,0.05698512]
  cd(0:1, 199) = [180.00000000,0.05309279]

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
!     PRINT*,'alpha < tabulated range in calc_cd6_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd6 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd6_ge28.pro'
  ENDIF

!print,y,cd6

!_________________________________________________________________________
  
END FUNCTION calc_cd6_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd7_ge28( y ) RESULT( cd7 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd7
  INTEGER :: i

  cd7 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.0104451]
  cd(0:1, 1  ) = [-177.0000000,0.0104872]
  cd(0:1, 2  ) = [-174.0000000,0.0105304]
  cd(0:1, 3  ) = [-171.0000000,0.0105740]
  cd(0:1, 4  ) = [-168.0000000,0.0274506]
  cd(0:1, 5  ) = [-165.0000000,0.0527444]
  cd(0:1, 6  ) = [-162.0000000,0.0780396]
  cd(0:1, 7  ) = [-159.0000000,0.1125041]
  cd(0:1, 8  ) = [-156.0000000,0.1653057]
  cd(0:1, 9  ) = [-153.0000000,0.2181102]
  cd(0:1, 10 ) = [-150.0000000,0.2709180]
  cd(0:1, 11 ) = [-147.0000000,0.3358992]
  cd(0:1, 12 ) = [-144.0000000,0.4008531]
  cd(0:1, 13 ) = [-141.0000000,0.4658075]
  cd(0:1, 14 ) = [-138.0000000,0.5337175]
  cd(0:1, 15 ) = [-135.0000000,0.6031054]
  cd(0:1, 16 ) = [-132.0000000,0.6724941]
  cd(0:1, 17 ) = [-129.0000000,0.7406510]
  cd(0:1, 18 ) = [-126.0000000,0.8063445]
  cd(0:1, 19 ) = [-123.0000000,0.8720375]
  cd(0:1, 20 ) = [-120.0000000,0.9377302]
  cd(0:1, 21 ) = [-117.0000000,0.9920497]
  cd(0:1, 22 ) = [-114.0000000,1.0463689]
  cd(0:1, 23 ) = [-111.0000000,1.1006877]
  cd(0:1, 24 ) = [-108.0000000,1.1431852]
  cd(0:1, 25 ) = [-105.0000000,1.1797720]
  cd(0:1, 26 ) = [-102.0000000,1.2163583]
  cd(0:1, 27 ) = [-99.0000000,1.2456743]
  cd(0:1, 28 ) = [-96.0000000,1.2604496]
  cd(0:1, 29 ) = [-93.0000000,1.2752248]
  cd(0:1, 30 ) = [-90.0000000,1.2899999]
  cd(0:1, 31 ) = [-87.0000000,1.2752248]
  cd(0:1, 32 ) = [-84.0000000,1.2604496]
  cd(0:1, 33 ) = [-81.0000000,1.2456745]
  cd(0:1, 34 ) = [-78.0000000,1.2163584]
  cd(0:1, 35 ) = [-75.0000000,1.1797717]
  cd(0:1, 36 ) = [-72.0000000,1.1431854]
  cd(0:1, 37 ) = [-69.0000000,1.1006882]
  cd(0:1, 38 ) = [-66.0000000,1.0463688]
  cd(0:1, 39 ) = [-63.0000000,0.9920493]
  cd(0:1, 40 ) = [-60.0000000,0.9377297]
  cd(0:1, 41 ) = [-57.0000000,0.8720370]
  cd(0:1, 42 ) = [-54.0000000,0.8063440]
  cd(0:1, 43 ) = [-51.0000000,0.7406506]
  cd(0:1, 44 ) = [-48.0000000,0.6724941]
  cd(0:1, 45 ) = [-45.0000000,0.6031063]
  cd(0:1, 46 ) = [-42.0000000,0.5337176]
  cd(0:1, 47 ) = [-39.0000000,0.4658074]
  cd(0:1, 48 ) = [-36.0000000,0.4008529]
  cd(0:1, 49 ) = [-33.0000000,0.3358996]
  cd(0:1, 50 ) = [-30.0000000,0.2709176]
  cd(0:1, 51 ) = [-29.3939394,0.2602493]
  cd(0:1, 52 ) = [-28.7878788,0.2495811]
  cd(0:1, 53 ) = [-28.1818182,0.2389130]
  cd(0:1, 54 ) = [-27.5757576,0.2282449]
  cd(0:1, 55 ) = [-26.9696970,0.2175768]
  cd(0:1, 56 ) = [-26.3636364,0.2069089]
  cd(0:1, 57 ) = [-25.7575758,0.1962412]
  cd(0:1, 58 ) = [-25.1515152,0.1855738]
  cd(0:1, 59 ) = [-24.5454545,0.1749065]
  cd(0:1, 60 ) = [-23.9393939,0.1642393]
  cd(0:1, 61 ) = [-23.3333333,0.1535722]
  cd(0:1, 62 ) = [-22.7272727,0.1429052]
  cd(0:1, 63 ) = [-22.1212121,0.1322382]
  cd(0:1, 64 ) = [-21.5151515,0.1215711]
  cd(0:1, 65 ) = [-20.9090909,0.1109041]
  cd(0:1, 66 ) = [-20.3030303,0.1002371]
  cd(0:1, 67 ) = [-19.6969697,0.0930822]
  cd(0:1, 68 ) = [-19.0909091,0.0894395]
  cd(0:1, 69 ) = [-18.4848485,0.0857970]
  cd(0:1, 70 ) = [-17.8787879,0.0821546]
  cd(0:1, 71 ) = [-17.2727273,0.0785122]
  cd(0:1, 72 ) = [-16.6666667,0.0748699]
  cd(0:1, 73 ) = [-16.0606061,0.0712278]
  cd(0:1, 74 ) = [-15.4545455,0.0675860]
  cd(0:1, 75 ) = [-14.8484848,0.0639444]
  cd(0:1, 76 ) = [-14.2424242,0.0603027]
  cd(0:1, 77 ) = [-13.6363636,0.0566613]
  cd(0:1, 78 ) = [-13.0303030,0.0530198]
  cd(0:1, 79 ) = [-12.4242424,0.0493787]
  cd(0:1, 80 ) = [-11.8181818,0.0457376]
  cd(0:1, 81 ) = [-11.2121212,0.0420969]
  cd(0:1, 82 ) = [-10.6060606,0.0384561]
  cd(0:1, 83 ) = [-10.0000000,0.0348157]
  cd(0:1, 84 ) = [-9.3939394,0.0342188]
  cd(0:1, 85 ) = [-8.7878788,0.0336222]
  cd(0:1, 86 ) = [-8.1818182,0.0330196]
  cd(0:1, 87 ) = [-7.5757576,0.0326768]
  cd(0:1, 88 ) = [-6.9696970,0.0324943]
  cd(0:1, 89 ) = [-6.3636364,0.0322073]
  cd(0:1, 90 ) = [-5.7575758,0.0318276]
  cd(0:1, 91 ) = [-5.1515152,0.0313467]
  cd(0:1, 92 ) = [-4.5454545,0.0308121]
  cd(0:1, 93 ) = [-3.9393939,0.0302529]
  cd(0:1, 94 ) = [-3.3333333,0.0296208]
  cd(0:1, 95 ) = [-2.7272727,0.0293065]
  cd(0:1, 96 ) = [-2.1212121,0.0293539]
  cd(0:1, 97 ) = [-1.5151515,0.0293788]
  cd(0:1, 98 ) = [-0.9090909,0.0294163]
  cd(0:1, 99 ) = [-0.3030303,0.0295150]
  cd(0:1, 100) = [0.3030303,0.0295252]
  cd(0:1, 101) = [0.9090909,0.0295324]
  cd(0:1, 102) = [1.5151515,0.0295320]
  cd(0:1, 103) = [2.1212121,0.0296220]
  cd(0:1, 104) = [2.7272727,0.0296076]
  cd(0:1, 105) = [3.3333333,0.0295895]
  cd(0:1, 106) = [3.9393939,0.0296602]
  cd(0:1, 107) = [4.5454545,0.0296451]
  cd(0:1, 108) = [5.1515152,0.0296480]
  cd(0:1, 109) = [5.7575758,0.0296911]
  cd(0:1, 110) = [6.3636364,0.0296540]
  cd(0:1, 111) = [6.9696970,0.0297108]
  cd(0:1, 112) = [7.5757576,0.0296608]
  cd(0:1, 113) = [8.1818182,0.0296345]
  cd(0:1, 114) = [8.7878788,0.0296856]
  cd(0:1, 115) = [9.3939394,0.0296488]
  cd(0:1, 116) = [10.0000000,0.0295366]
  cd(0:1, 117) = [10.6060606,0.0293338]
  cd(0:1, 118) = [11.2121212,0.0293799]
  cd(0:1, 119) = [11.8181818,0.0301308]
  cd(0:1, 120) = [12.4242424,0.0333106]
  cd(0:1, 121) = [13.0303030,0.0356492]
  cd(0:1, 122) = [13.6363636,0.0373441]
  cd(0:1, 123) = [14.2424242,0.0386537]
  cd(0:1, 124) = [14.8484848,0.0396718]
  cd(0:1, 125) = [15.4545455,0.0402779]
  cd(0:1, 126) = [16.0606061,0.0415211]
  cd(0:1, 127) = [16.6666667,0.0497350]
  cd(0:1, 128) = [17.2727273,0.0579482]
  cd(0:1, 129) = [17.8787879,0.0661606]
  cd(0:1, 130) = [18.4848485,0.0743727]
  cd(0:1, 131) = [19.0909091,0.0825848]
  cd(0:1, 132) = [19.6969697,0.0907973]
  cd(0:1, 133) = [20.3030303,0.1002371]
  cd(0:1, 134) = [20.9090909,0.1109041]
  cd(0:1, 135) = [21.5151515,0.1215711]
  cd(0:1, 136) = [22.1212121,0.1322382]
  cd(0:1, 137) = [22.7272727,0.1429052]
  cd(0:1, 138) = [23.3333333,0.1535722]
  cd(0:1, 139) = [23.9393939,0.1642393]
  cd(0:1, 140) = [24.5454545,0.1749065]
  cd(0:1, 141) = [25.1515152,0.1855738]
  cd(0:1, 142) = [25.7575758,0.1962412]
  cd(0:1, 143) = [26.3636364,0.2069089]
  cd(0:1, 144) = [26.9696970,0.2175768]
  cd(0:1, 145) = [27.5757576,0.2282449]
  cd(0:1, 146) = [28.1818182,0.2389130]
  cd(0:1, 147) = [28.7878788,0.2495811]
  cd(0:1, 148) = [29.3939394,0.2602493]
  cd(0:1, 149) = [30.0000000,0.2709176]
  cd(0:1, 150) = [33.0000000,0.3358996]
  cd(0:1, 151) = [36.0000000,0.4008529]
  cd(0:1, 152) = [39.0000000,0.4658074]
  cd(0:1, 153) = [42.0000000,0.5337176]
  cd(0:1, 154) = [45.0000000,0.6031063]
  cd(0:1, 155) = [48.0000000,0.6724941]
  cd(0:1, 156) = [51.0000000,0.7406506]
  cd(0:1, 157) = [54.0000000,0.8063440]
  cd(0:1, 158) = [57.0000000,0.8720370]
  cd(0:1, 159) = [60.0000000,0.9377297]
  cd(0:1, 160) = [63.0000000,0.9920493]
  cd(0:1, 161) = [66.0000000,1.0463688]
  cd(0:1, 162) = [69.0000000,1.1006882]
  cd(0:1, 163) = [72.0000000,1.1431854]
  cd(0:1, 164) = [75.0000000,1.1797717]
  cd(0:1, 165) = [78.0000000,1.2163584]
  cd(0:1, 166) = [81.0000000,1.2456745]
  cd(0:1, 167) = [84.0000000,1.2604496]
  cd(0:1, 168) = [87.0000000,1.2752248]
  cd(0:1, 169) = [90.0000000,1.2899999]
  cd(0:1, 170) = [93.0000000,1.2752248]
  cd(0:1, 171) = [96.0000000,1.2604496]
  cd(0:1, 172) = [99.0000000,1.2456743]
  cd(0:1, 173) = [102.0000000,1.2163583]
  cd(0:1, 174) = [105.0000000,1.1797720]
  cd(0:1, 175) = [108.0000000,1.1431852]
  cd(0:1, 176) = [111.0000000,1.1006877]
  cd(0:1, 177) = [114.0000000,1.0463689]
  cd(0:1, 178) = [117.0000000,0.9920497]
  cd(0:1, 179) = [120.0000000,0.9377302]
  cd(0:1, 180) = [123.0000000,0.8720375]
  cd(0:1, 181) = [126.0000000,0.8063445]
  cd(0:1, 182) = [129.0000000,0.7406510]
  cd(0:1, 183) = [132.0000000,0.6724941]
  cd(0:1, 184) = [135.0000000,0.6031054]
  cd(0:1, 185) = [138.0000000,0.5337175]
  cd(0:1, 186) = [141.0000000,0.4658075]
  cd(0:1, 187) = [144.0000000,0.4008531]
  cd(0:1, 188) = [147.0000000,0.3358992]
  cd(0:1, 189) = [150.0000000,0.2709180]
  cd(0:1, 190) = [153.0000000,0.2181102]
  cd(0:1, 191) = [156.0000000,0.1653057]
  cd(0:1, 192) = [159.0000000,0.1125041]
  cd(0:1, 193) = [162.0000000,0.0780396]
  cd(0:1, 194) = [165.0000000,0.0527444]
  cd(0:1, 195) = [168.0000000,0.0274506]
  cd(0:1, 196) = [171.0000000,0.0105740]
  cd(0:1, 197) = [174.0000000,0.0105304]
  cd(0:1, 198) = [177.0000000,0.0104872]
  cd(0:1, 199) = [180.0000000,0.0104451]

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
!     PRINT*,'alpha < tabulated range in calc_cd7_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd7 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd7_ge28.pro'
  ENDIF

!print,y,cd7

!_________________________________________________________________________
  
END FUNCTION calc_cd7_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd8_ge28( y ) RESULT( cd8 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd8
  INTEGER :: i

  cd8 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.0233914]
  cd(0:1, 1  ) = [-177.0000000,0.0233914]
  cd(0:1, 2  ) = [-174.0000000,0.0266846]
  cd(0:1, 3  ) = [-171.0000000,0.0365643]
  cd(0:1, 4  ) = [-168.0000000,0.0584099]
  cd(0:1, 5  ) = [-165.0000000,0.0862381]
  cd(0:1, 6  ) = [-162.0000000,0.1140667]
  cd(0:1, 7  ) = [-159.0000000,0.1506323]
  cd(0:1, 8  ) = [-156.0000000,0.2046714]
  cd(0:1, 9  ) = [-153.0000000,0.2587109]
  cd(0:1, 10 ) = [-150.0000000,0.3127505]
  cd(0:1, 11 ) = [-147.0000000,0.3791391]
  cd(0:1, 12 ) = [-144.0000000,0.4455279]
  cd(0:1, 13 ) = [-141.0000000,0.5119171]
  cd(0:1, 14 ) = [-138.0000000,0.5812167]
  cd(0:1, 15 ) = [-135.0000000,0.6519715]
  cd(0:1, 16 ) = [-132.0000000,0.7227269]
  cd(0:1, 17 ) = [-129.0000000,0.7921312]
  cd(0:1, 18 ) = [-126.0000000,0.8588329]
  cd(0:1, 19 ) = [-123.0000000,0.9255341]
  cd(0:1, 20 ) = [-120.0000000,0.9922351]
  cd(0:1, 21 ) = [-117.0000000,1.0469488]
  cd(0:1, 22 ) = [-114.0000000,1.1016622]
  cd(0:1, 23 ) = [-111.0000000,1.1563752]
  cd(0:1, 24 ) = [-108.0000000,1.1987409]
  cd(0:1, 25 ) = [-105.0000000,1.2349329]
  cd(0:1, 26 ) = [-102.0000000,1.2711245]
  cd(0:1, 27 ) = [-99.0000000,1.2997412]
  cd(0:1, 28 ) = [-96.0000000,1.3132080]
  cd(0:1, 29 ) = [-93.0000000,1.3266746]
  cd(0:1, 30 ) = [-90.0000000,1.3401412]
  cd(0:1, 31 ) = [-87.0000000,1.3266746]
  cd(0:1, 32 ) = [-84.0000000,1.3132079]
  cd(0:1, 33 ) = [-81.0000000,1.2997413]
  cd(0:1, 34 ) = [-78.0000000,1.2711246]
  cd(0:1, 35 ) = [-75.0000000,1.2349326]
  cd(0:1, 36 ) = [-72.0000000,1.1987411]
  cd(0:1, 37 ) = [-69.0000000,1.1563756]
  cd(0:1, 38 ) = [-66.0000000,1.1016620]
  cd(0:1, 39 ) = [-63.0000000,1.0469484]
  cd(0:1, 40 ) = [-60.0000000,0.9922347]
  cd(0:1, 41 ) = [-57.0000000,0.9255337]
  cd(0:1, 42 ) = [-54.0000000,0.8588325]
  cd(0:1, 43 ) = [-51.0000000,0.7921307]
  cd(0:1, 44 ) = [-48.0000000,0.7227442]
  cd(0:1, 45 ) = [-45.0000000,0.6520155]
  cd(0:1, 46 ) = [-42.0000000,0.5812340]
  cd(0:1, 47 ) = [-39.0000000,0.5115975]
  cd(0:1, 48 ) = [-36.0000000,0.4444969]
  cd(0:1, 49 ) = [-33.0000000,0.3781982]
  cd(0:1, 50 ) = [-30.0000000,0.3127501]
  cd(0:1, 51 ) = [-29.3939394,0.3014387]
  cd(0:1, 52 ) = [-28.7878788,0.2901274]
  cd(0:1, 53 ) = [-28.1818182,0.2788160]
  cd(0:1, 54 ) = [-27.5757576,0.2676299]
  cd(0:1, 55 ) = [-26.9696970,0.2564974]
  cd(0:1, 56 ) = [-26.3636364,0.2453649]
  cd(0:1, 57 ) = [-25.7575758,0.2343116]
  cd(0:1, 58 ) = [-25.1515152,0.2233772]
  cd(0:1, 59 ) = [-24.5454545,0.2124427]
  cd(0:1, 60 ) = [-23.9393939,0.2015298]
  cd(0:1, 61 ) = [-23.3333333,0.1908107]
  cd(0:1, 62 ) = [-22.7272727,0.1800915]
  cd(0:1, 63 ) = [-22.1212121,0.1693723]
  cd(0:1, 64 ) = [-21.5151515,0.1588382]
  cd(0:1, 65 ) = [-20.9090909,0.1483505]
  cd(0:1, 66 ) = [-20.3030303,0.1378627]
  cd(0:1, 67 ) = [-19.6969697,0.1299189]
  cd(0:1, 68 ) = [-19.0909091,0.1245189]
  cd(0:1, 69 ) = [-18.4848485,0.1191192]
  cd(0:1, 70 ) = [-17.8787879,0.1137194]
  cd(0:1, 71 ) = [-17.2727273,0.1083194]
  cd(0:1, 72 ) = [-16.6666667,0.1022557]
  cd(0:1, 73 ) = [-16.0606061,0.0961448]
  cd(0:1, 74 ) = [-15.4545455,0.0903872]
  cd(0:1, 75 ) = [-14.8484848,0.0848231]
  cd(0:1, 76 ) = [-14.2424242,0.0796631]
  cd(0:1, 77 ) = [-13.6363636,0.0745632]
  cd(0:1, 78 ) = [-13.0303030,0.0695101]
  cd(0:1, 79 ) = [-12.4242424,0.0644104]
  cd(0:1, 80 ) = [-11.8181818,0.0592220]
  cd(0:1, 81 ) = [-11.2121212,0.0538126]
  cd(0:1, 82 ) = [-10.6060606,0.0482803]
  cd(0:1, 83 ) = [-10.0000000,0.0427092]
  cd(0:1, 84 ) = [-9.3939394,0.0397763]
  cd(0:1, 85 ) = [-8.7878788,0.0369499]
  cd(0:1, 86 ) = [-8.1818182,0.0336610]
  cd(0:1, 87 ) = [-7.5757576,0.0311984]
  cd(0:1, 88 ) = [-6.9696970,0.0288846]
  cd(0:1, 89 ) = [-6.3636364,0.0266987]
  cd(0:1, 90 ) = [-5.7575758,0.0247794]
  cd(0:1, 91 ) = [-5.1515152,0.0233543]
  cd(0:1, 92 ) = [-4.5454545,0.0220951]
  cd(0:1, 93 ) = [-3.9393939,0.0209757]
  cd(0:1, 94 ) = [-3.3333333,0.0204804]
  cd(0:1, 95 ) = [-2.7272727,0.0200079]
  cd(0:1, 96 ) = [-2.1212121,0.0196028]
  cd(0:1, 97 ) = [-1.5151515,0.0194248]
  cd(0:1, 98 ) = [-0.9090909,0.0193210]
  cd(0:1, 99 ) = [-0.3030303,0.0193085]
  cd(0:1, 100) = [0.3030303,0.0192708]
  cd(0:1, 101) = [0.9090909,0.0192564]
  cd(0:1, 102) = [1.5151515,0.0193241]
  cd(0:1, 103) = [2.1212121,0.0194700]
  cd(0:1, 104) = [2.7272727,0.0195637]
  cd(0:1, 105) = [3.3333333,0.0196859]
  cd(0:1, 106) = [3.9393939,0.0199153]
  cd(0:1, 107) = [4.5454545,0.0201575]
  cd(0:1, 108) = [5.1515152,0.0204147]
  cd(0:1, 109) = [5.7575758,0.0206936]
  cd(0:1, 110) = [6.3636364,0.0209895]
  cd(0:1, 111) = [6.9696970,0.0213767]
  cd(0:1, 112) = [7.5757576,0.0218254]
  cd(0:1, 113) = [8.1818182,0.0223379]
  cd(0:1, 114) = [8.7878788,0.0229917]
  cd(0:1, 115) = [9.3939394,0.0242669]
  cd(0:1, 116) = [10.0000000,0.0258159]
  cd(0:1, 117) = [10.6060606,0.0278716]
  cd(0:1, 118) = [11.2121212,0.0299228]
  cd(0:1, 119) = [11.8181818,0.0325599]
  cd(0:1, 120) = [12.4242424,0.0376947]
  cd(0:1, 121) = [13.0303030,0.0425731]
  cd(0:1, 122) = [13.6363636,0.0476558]
  cd(0:1, 123) = [14.2424242,0.0526403]
  cd(0:1, 124) = [14.8484848,0.0577455]
  cd(0:1, 125) = [15.4545455,0.0628922]
  cd(0:1, 126) = [16.0606061,0.0685681]
  cd(0:1, 127) = [16.6666667,0.0788845]
  cd(0:1, 128) = [17.2727273,0.0891883]
  cd(0:1, 129) = [17.8787879,0.0994767]
  cd(0:1, 130) = [18.4848485,0.1097221]
  cd(0:1, 131) = [19.0909091,0.1197123]
  cd(0:1, 132) = [19.6969697,0.1283167]
  cd(0:1, 133) = [20.3030303,0.1378627]
  cd(0:1, 134) = [20.9090909,0.1483505]
  cd(0:1, 135) = [21.5151515,0.1588382]
  cd(0:1, 136) = [22.1212121,0.1693723]
  cd(0:1, 137) = [22.7272727,0.1800915]
  cd(0:1, 138) = [23.3333333,0.1908107]
  cd(0:1, 139) = [23.9393939,0.2015298]
  cd(0:1, 140) = [24.5454545,0.2124427]
  cd(0:1, 141) = [25.1515152,0.2233772]
  cd(0:1, 142) = [25.7575758,0.2343116]
  cd(0:1, 143) = [26.3636364,0.2453649]
  cd(0:1, 144) = [26.9696970,0.2564974]
  cd(0:1, 145) = [27.5757576,0.2676299]
  cd(0:1, 146) = [28.1818182,0.2788160]
  cd(0:1, 147) = [28.7878788,0.2901274]
  cd(0:1, 148) = [29.3939394,0.3014387]
  cd(0:1, 149) = [30.0000000,0.3127501]
  cd(0:1, 150) = [33.0000000,0.3781982]
  cd(0:1, 151) = [36.0000000,0.4444969]
  cd(0:1, 152) = [39.0000000,0.5115975]
  cd(0:1, 153) = [42.0000000,0.5812340]
  cd(0:1, 154) = [45.0000000,0.6520155]
  cd(0:1, 155) = [48.0000000,0.7227442]
  cd(0:1, 156) = [51.0000000,0.7921307]
  cd(0:1, 157) = [54.0000000,0.8588325]
  cd(0:1, 158) = [57.0000000,0.9255337]
  cd(0:1, 159) = [60.0000000,0.9922347]
  cd(0:1, 160) = [63.0000000,1.0469484]
  cd(0:1, 161) = [66.0000000,1.1016620]
  cd(0:1, 162) = [69.0000000,1.1563756]
  cd(0:1, 163) = [72.0000000,1.1987411]
  cd(0:1, 164) = [75.0000000,1.2349326]
  cd(0:1, 165) = [78.0000000,1.2711246]
  cd(0:1, 166) = [81.0000000,1.2997413]
  cd(0:1, 167) = [84.0000000,1.3132079]
  cd(0:1, 168) = [87.0000000,1.3266746]
  cd(0:1, 169) = [90.0000000,1.3401412]
  cd(0:1, 170) = [93.0000000,1.3266746]
  cd(0:1, 171) = [96.0000000,1.3132080]
  cd(0:1, 172) = [99.0000000,1.2997412]
  cd(0:1, 173) = [102.0000000,1.2711245]
  cd(0:1, 174) = [105.0000000,1.2349329]
  cd(0:1, 175) = [108.0000000,1.1987409]
  cd(0:1, 176) = [111.0000000,1.1563752]
  cd(0:1, 177) = [114.0000000,1.1016622]
  cd(0:1, 178) = [117.0000000,1.0469488]
  cd(0:1, 179) = [120.0000000,0.9922351]
  cd(0:1, 180) = [123.0000000,0.9255341]
  cd(0:1, 181) = [126.0000000,0.8588329]
  cd(0:1, 182) = [129.0000000,0.7921312]
  cd(0:1, 183) = [132.0000000,0.7227269]
  cd(0:1, 184) = [135.0000000,0.6519715]
  cd(0:1, 185) = [138.0000000,0.5812167]
  cd(0:1, 186) = [141.0000000,0.5119171]
  cd(0:1, 187) = [144.0000000,0.4455279]
  cd(0:1, 188) = [147.0000000,0.3791391]
  cd(0:1, 189) = [150.0000000,0.3127505]
  cd(0:1, 190) = [153.0000000,0.2587109]
  cd(0:1, 191) = [156.0000000,0.2046714]
  cd(0:1, 192) = [159.0000000,0.1506323]
  cd(0:1, 193) = [162.0000000,0.1142189]
  cd(0:1, 194) = [165.0000000,0.0866185]
  cd(0:1, 195) = [168.0000000,0.0590183]
  cd(0:1, 196) = [171.0000000,0.0372487]
  cd(0:1, 197) = [174.0000000,0.0271408]
  cd(0:1, 198) = [177.0000000,0.0236166]
  cd(0:1, 199) = [180.0000000,0.0233914]

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
!     PRINT*,'alpha < tabulated range in calc_cd8_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd8 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd8_ge28.pro'
  ENDIF

!print,y,cd8

!_________________________________________________________________________
  
END FUNCTION calc_cd8_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd9_ge28( y ) RESULT( cd9 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd9
  INTEGER :: i

  cd9 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.0436560]
  cd(0:1, 1  ) = [-177.0000000,0.0436560]
  cd(0:1, 2  ) = [-174.0000000,0.0519327]
  cd(0:1, 3  ) = [-171.0000000,0.0767630]
  cd(0:1, 4  ) = [-168.0000000,0.1061831]
  cd(0:1, 5  ) = [-165.0000000,0.1378980]
  cd(0:1, 6  ) = [-162.0000000,0.1696133]
  cd(0:1, 7  ) = [-159.0000000,0.2093952]
  cd(0:1, 8  ) = [-156.0000000,0.2653100]
  cd(0:1, 9  ) = [-153.0000000,0.3212249]
  cd(0:1, 10 ) = [-150.0000000,0.3771399]
  cd(0:1, 11 ) = [-147.0000000,0.4456608]
  cd(0:1, 12 ) = [-144.0000000,0.5141817]
  cd(0:1, 13 ) = [-141.0000000,0.5827029]
  cd(0:1, 14 ) = [-138.0000000,0.6540602]
  cd(0:1, 15 ) = [-135.0000000,0.7268355]
  cd(0:1, 16 ) = [-132.0000000,0.7996112]
  cd(0:1, 17 ) = [-129.0000000,0.8708523]
  cd(0:1, 18 ) = [-126.0000000,0.9390240]
  cd(0:1, 19 ) = [-123.0000000,1.0071952]
  cd(0:1, 20 ) = [-120.0000000,1.0753662]
  cd(0:1, 21 ) = [-117.0000000,1.1306145]
  cd(0:1, 22 ) = [-114.0000000,1.1858625]
  cd(0:1, 23 ) = [-111.0000000,1.2411102]
  cd(0:1, 24 ) = [-108.0000000,1.2832121]
  cd(0:1, 25 ) = [-105.0000000,1.3187410]
  cd(0:1, 26 ) = [-102.0000000,1.3542695]
  cd(0:1, 27 ) = [-99.0000000,1.3817615]
  cd(0:1, 28 ) = [-96.0000000,1.3931803]
  cd(0:1, 29 ) = [-93.0000000,1.4045991]
  cd(0:1, 30 ) = [-90.0000000,1.4160178]
  cd(0:1, 31 ) = [-87.0000000,1.4045991]
  cd(0:1, 32 ) = [-84.0000000,1.3931803]
  cd(0:1, 33 ) = [-81.0000000,1.3817616]
  cd(0:1, 34 ) = [-78.0000000,1.3542696]
  cd(0:1, 35 ) = [-75.0000000,1.3187407]
  cd(0:1, 36 ) = [-72.0000000,1.2832122]
  cd(0:1, 37 ) = [-69.0000000,1.2411107]
  cd(0:1, 38 ) = [-66.0000000,1.1858624]
  cd(0:1, 39 ) = [-63.0000000,1.1306141]
  cd(0:1, 40 ) = [-60.0000000,1.0753657]
  cd(0:1, 41 ) = [-57.0000000,1.0071948]
  cd(0:1, 42 ) = [-54.0000000,0.9390235]
  cd(0:1, 43 ) = [-51.0000000,0.8708518]
  cd(0:1, 44 ) = [-48.0000000,0.7996546]
  cd(0:1, 45 ) = [-45.0000000,0.7269446]
  cd(0:1, 46 ) = [-42.0000000,0.6541035]
  cd(0:1, 47 ) = [-39.0000000,0.5818996]
  cd(0:1, 48 ) = [-36.0000000,0.5115908]
  cd(0:1, 49 ) = [-33.0000000,0.4432954]
  cd(0:1, 50 ) = [-30.0000000,0.3771395]
  cd(0:1, 51 ) = [-29.3939394,0.3648524]
  cd(0:1, 52 ) = [-28.7878788,0.3525652]
  cd(0:1, 53 ) = [-28.1818182,0.3402781]
  cd(0:1, 54 ) = [-27.5757576,0.3283060]
  cd(0:1, 55 ) = [-26.9696970,0.3164689]
  cd(0:1, 56 ) = [-26.3636364,0.3046317]
  cd(0:1, 57 ) = [-25.7575758,0.2929936]
  cd(0:1, 58 ) = [-25.1515152,0.2816537]
  cd(0:1, 59 ) = [-24.5454545,0.2703139]
  cd(0:1, 60 ) = [-23.9393939,0.2590282]
  cd(0:1, 61 ) = [-23.3333333,0.2482293]
  cd(0:1, 62 ) = [-22.7272727,0.2374305]
  cd(0:1, 63 ) = [-22.1212121,0.2266316]
  cd(0:1, 64 ) = [-21.5151515,0.2162984]
  cd(0:1, 65 ) = [-20.9090909,0.2060817]
  cd(0:1, 66 ) = [-20.3030303,0.1958650]
  cd(0:1, 67 ) = [-19.6969697,0.1867131]
  cd(0:1, 68 ) = [-19.0909091,0.1786260]
  cd(0:1, 69 ) = [-18.4848485,0.1705392]
  cd(0:1, 70 ) = [-17.8787879,0.1624525]
  cd(0:1, 71 ) = [-17.2727273,0.1543653]
  cd(0:1, 72 ) = [-16.6666667,0.1451371]
  cd(0:1, 73 ) = [-16.0606061,0.1358274]
  cd(0:1, 74 ) = [-15.4545455,0.1271254]
  cd(0:1, 75 ) = [-14.8484848,0.1187560]
  cd(0:1, 76 ) = [-14.2424242,0.1110816]
  cd(0:1, 77 ) = [-13.6363636,0.1035105]
  cd(0:1, 78 ) = [-13.0303030,0.0960198]
  cd(0:1, 79 ) = [-12.4242424,0.0884489]
  cd(0:1, 80 ) = [-11.8181818,0.0807256]
  cd(0:1, 81 ) = [-11.2121212,0.0726223]
  cd(0:1, 82 ) = [-10.6060606,0.0643078]
  cd(0:1, 83 ) = [-10.0000000,0.0559264]
  cd(0:1, 84 ) = [-9.3939394,0.0491379]
  cd(0:1, 85 ) = [-8.7878788,0.0425327]
  cd(0:1, 86 ) = [-8.1818182,0.0351385]
  cd(0:1, 87 ) = [-7.5757576,0.0307167]
  cd(0:1, 88 ) = [-6.9696970,0.0273461]
  cd(0:1, 89 ) = [-6.3636364,0.0240467]
  cd(0:1, 90 ) = [-5.7575758,0.0211022]
  cd(0:1, 91 ) = [-5.1515152,0.0189551]
  cd(0:1, 92 ) = [-4.5454545,0.0170622]
  cd(0:1, 93 ) = [-3.9393939,0.0153692]
  cd(0:1, 94 ) = [-3.3333333,0.0148140]
  cd(0:1, 95 ) = [-2.7272727,0.0141625]
  cd(0:1, 96 ) = [-2.1212121,0.0134146]
  cd(0:1, 97 ) = [-1.5151515,0.0130922]
  cd(0:1, 98 ) = [-0.9090909,0.0128924]
  cd(0:1, 99 ) = [-0.3030303,0.0128051]
  cd(0:1, 100) = [0.3030303,0.0127478]
  cd(0:1, 101) = [0.9090909,0.0127354]
  cd(0:1, 102) = [1.5151515,0.0128777]
  cd(0:1, 103) = [2.1212121,0.0130676]
  cd(0:1, 104) = [2.7272727,0.0132425]
  cd(0:1, 105) = [3.3333333,0.0134718]
  cd(0:1, 106) = [3.9393939,0.0138172]
  cd(0:1, 107) = [4.5454545,0.0142557]
  cd(0:1, 108) = [5.1515152,0.0147060]
  cd(0:1, 109) = [5.7575758,0.0151614]
  cd(0:1, 110) = [6.3636364,0.0157108]
  cd(0:1, 111) = [6.9696970,0.0163448]
  cd(0:1, 112) = [7.5757576,0.0171676]
  cd(0:1, 113) = [8.1818182,0.0180775]
  cd(0:1, 114) = [8.7878788,0.0191750]
  cd(0:1, 115) = [9.3939394,0.0213859]
  cd(0:1, 116) = [10.0000000,0.0240167]
  cd(0:1, 117) = [10.6060606,0.0273519]
  cd(0:1, 118) = [11.2121212,0.0311908]
  cd(0:1, 119) = [11.8181818,0.0367777]
  cd(0:1, 120) = [12.4242424,0.0449502]
  cd(0:1, 121) = [13.0303030,0.0537918]
  cd(0:1, 122) = [13.6363636,0.0640422]
  cd(0:1, 123) = [14.2424242,0.0746618]
  cd(0:1, 124) = [14.8484848,0.0860251]
  cd(0:1, 125) = [15.4545455,0.0981117]
  cd(0:1, 126) = [16.0606061,0.1105567]
  cd(0:1, 127) = [16.6666667,0.1240571]
  cd(0:1, 128) = [17.2727273,0.1375258]
  cd(0:1, 129) = [17.8787879,0.1509560]
  cd(0:1, 130) = [18.4848485,0.1642787]
  cd(0:1, 131) = [19.0909091,0.1769600]
  cd(0:1, 132) = [19.6969697,0.1861578]
  cd(0:1, 133) = [20.3030303,0.1958650]
  cd(0:1, 134) = [20.9090909,0.2060817]
  cd(0:1, 135) = [21.5151515,0.2162984]
  cd(0:1, 136) = [22.1212121,0.2266316]
  cd(0:1, 137) = [22.7272727,0.2374305]
  cd(0:1, 138) = [23.3333333,0.2482293]
  cd(0:1, 139) = [23.9393939,0.2590282]
  cd(0:1, 140) = [24.5454545,0.2703139]
  cd(0:1, 141) = [25.1515152,0.2816537]
  cd(0:1, 142) = [25.7575758,0.2929936]
  cd(0:1, 143) = [26.3636364,0.3046317]
  cd(0:1, 144) = [26.9696970,0.3164689]
  cd(0:1, 145) = [27.5757576,0.3283060]
  cd(0:1, 146) = [28.1818182,0.3402781]
  cd(0:1, 147) = [28.7878788,0.3525652]
  cd(0:1, 148) = [29.3939394,0.3648524]
  cd(0:1, 149) = [30.0000000,0.3771395]
  cd(0:1, 150) = [33.0000000,0.4432954]
  cd(0:1, 151) = [36.0000000,0.5115908]
  cd(0:1, 152) = [39.0000000,0.5818996]
  cd(0:1, 153) = [42.0000000,0.6541035]
  cd(0:1, 154) = [45.0000000,0.7269446]
  cd(0:1, 155) = [48.0000000,0.7996546]
  cd(0:1, 156) = [51.0000000,0.8708518]
  cd(0:1, 157) = [54.0000000,0.9390235]
  cd(0:1, 158) = [57.0000000,1.0071948]
  cd(0:1, 159) = [60.0000000,1.0753657]
  cd(0:1, 160) = [63.0000000,1.1306141]
  cd(0:1, 161) = [66.0000000,1.1858624]
  cd(0:1, 162) = [69.0000000,1.2411107]
  cd(0:1, 163) = [72.0000000,1.2832122]
  cd(0:1, 164) = [75.0000000,1.3187407]
  cd(0:1, 165) = [78.0000000,1.3542696]
  cd(0:1, 166) = [81.0000000,1.3817616]
  cd(0:1, 167) = [84.0000000,1.3931803]
  cd(0:1, 168) = [87.0000000,1.4045991]
  cd(0:1, 169) = [90.0000000,1.4160178]
  cd(0:1, 170) = [93.0000000,1.4045991]
  cd(0:1, 171) = [96.0000000,1.3931803]
  cd(0:1, 172) = [99.0000000,1.3817615]
  cd(0:1, 173) = [102.0000000,1.3542695]
  cd(0:1, 174) = [105.0000000,1.3187410]
  cd(0:1, 175) = [108.0000000,1.2832121]
  cd(0:1, 176) = [111.0000000,1.2411102]
  cd(0:1, 177) = [114.0000000,1.1858625]
  cd(0:1, 178) = [117.0000000,1.1306145]
  cd(0:1, 179) = [120.0000000,1.0753662]
  cd(0:1, 180) = [123.0000000,1.0071952]
  cd(0:1, 181) = [126.0000000,0.9390240]
  cd(0:1, 182) = [129.0000000,0.8708523]
  cd(0:1, 183) = [132.0000000,0.7996112]
  cd(0:1, 184) = [135.0000000,0.7268355]
  cd(0:1, 185) = [138.0000000,0.6540602]
  cd(0:1, 186) = [141.0000000,0.5827029]
  cd(0:1, 187) = [144.0000000,0.5141817]
  cd(0:1, 188) = [147.0000000,0.4456608]
  cd(0:1, 189) = [150.0000000,0.3771399]
  cd(0:1, 190) = [153.0000000,0.3212249]
  cd(0:1, 191) = [156.0000000,0.2653100]
  cd(0:1, 192) = [159.0000000,0.2093952]
  cd(0:1, 193) = [162.0000000,0.1699957]
  cd(0:1, 194) = [165.0000000,0.1388540]
  cd(0:1, 195) = [168.0000000,0.1077122]
  cd(0:1, 196) = [171.0000000,0.0784830]
  cd(0:1, 197) = [174.0000000,0.0530791]
  cd(0:1, 198) = [177.0000000,0.0442221]
  cd(0:1, 199) = [180.0000000,0.0436560]

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
!     PRINT*,'alpha < tabulated range in calc_cd9_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd9 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd9_ge28.pro'
  ENDIF

!print,y,cd9

!_________________________________________________________________________
  
END FUNCTION calc_cd9_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd10_ge28( y ) RESULT( cd10 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd10
  INTEGER :: i

  cd10 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.0531472]
  cd(0:1, 1  ) = [-177.0000000,0.0531472]
  cd(0:1, 2  ) = [-174.0000000,0.0637580]
  cd(0:1, 3  ) = [-171.0000000,0.0955905]
  cd(0:1, 4  ) = [-168.0000000,0.1285583]
  cd(0:1, 5  ) = [-165.0000000,0.1620936]
  cd(0:1, 6  ) = [-162.0000000,0.1956293]
  cd(0:1, 7  ) = [-159.0000000,0.2369176]
  cd(0:1, 8  ) = [-156.0000000,0.2937109]
  cd(0:1, 9  ) = [-153.0000000,0.3505042]
  cd(0:1, 10 ) = [-150.0000000,0.4072975]
  cd(0:1, 11 ) = [-147.0000000,0.4768170]
  cd(0:1, 12 ) = [-144.0000000,0.5463366]
  cd(0:1, 13 ) = [-141.0000000,0.6158563]
  cd(0:1, 14 ) = [-138.0000000,0.6881773]
  cd(0:1, 15 ) = [-135.0000000,0.7618990]
  cd(0:1, 16 ) = [-132.0000000,0.8356209]
  cd(0:1, 17 ) = [-129.0000000,0.9077223]
  cd(0:1, 18 ) = [-126.0000000,0.9765825]
  cd(0:1, 19 ) = [-123.0000000,1.0454422]
  cd(0:1, 20 ) = [-120.0000000,1.1143016]
  cd(0:1, 21 ) = [-117.0000000,1.1698004]
  cd(0:1, 22 ) = [-114.0000000,1.2252989]
  cd(0:1, 23 ) = [-111.0000000,1.2807969]
  cd(0:1, 24 ) = [-108.0000000,1.3227752]
  cd(0:1, 25 ) = [-105.0000000,1.3579936]
  cd(0:1, 26 ) = [-102.0000000,1.3932115]
  cd(0:1, 27 ) = [-99.0000000,1.4201768]
  cd(0:1, 28 ) = [-96.0000000,1.4306364]
  cd(0:1, 29 ) = [-93.0000000,1.4410960]
  cd(0:1, 30 ) = [-90.0000000,1.4515556]
  cd(0:1, 31 ) = [-87.0000000,1.4410960]
  cd(0:1, 32 ) = [-84.0000000,1.4306364]
  cd(0:1, 33 ) = [-81.0000000,1.4201769]
  cd(0:1, 34 ) = [-78.0000000,1.3932117]
  cd(0:1, 35 ) = [-75.0000000,1.3579933]
  cd(0:1, 36 ) = [-72.0000000,1.3227754]
  cd(0:1, 37 ) = [-69.0000000,1.2807974]
  cd(0:1, 38 ) = [-66.0000000,1.2252987]
  cd(0:1, 39 ) = [-63.0000000,1.1698000]
  cd(0:1, 40 ) = [-60.0000000,1.1143012]
  cd(0:1, 41 ) = [-57.0000000,1.0454418]
  cd(0:1, 42 ) = [-54.0000000,0.9765821]
  cd(0:1, 43 ) = [-51.0000000,0.9077218]
  cd(0:1, 44 ) = [-48.0000000,0.8356765]
  cd(0:1, 45 ) = [-45.0000000,0.7620386]
  cd(0:1, 46 ) = [-42.0000000,0.6882329]
  cd(0:1, 47 ) = [-39.0000000,0.6148265]
  cd(0:1, 48 ) = [-36.0000000,0.5430151]
  cd(0:1, 49 ) = [-33.0000000,0.4737845]
  cd(0:1, 50 ) = [-30.0000000,0.4072971]
  cd(0:1, 51 ) = [-29.3939394,0.3945530]
  cd(0:1, 52 ) = [-28.7878788,0.3818088]
  cd(0:1, 53 ) = [-28.1818182,0.3690647]
  cd(0:1, 54 ) = [-27.5757576,0.3567244]
  cd(0:1, 55 ) = [-26.9696970,0.3445573]
  cd(0:1, 56 ) = [-26.3636364,0.3323901]
  cd(0:1, 57 ) = [-25.7575758,0.3204780]
  cd(0:1, 58 ) = [-25.1515152,0.3089483]
  cd(0:1, 59 ) = [-24.5454545,0.2974186]
  cd(0:1, 60 ) = [-23.9393939,0.2859582]
  cd(0:1, 61 ) = [-23.3333333,0.2751221]
  cd(0:1, 62 ) = [-22.7272727,0.2642859]
  cd(0:1, 63 ) = [-22.1212121,0.2534497]
  cd(0:1, 64 ) = [-21.5151515,0.2432106]
  cd(0:1, 65 ) = [-20.9090909,0.2331209]
  cd(0:1, 66 ) = [-20.3030303,0.2230311]
  cd(0:1, 67 ) = [-19.6969697,0.2133134]
  cd(0:1, 68 ) = [-19.0909091,0.2039677]
  cd(0:1, 69 ) = [-18.4848485,0.1946225]
  cd(0:1, 70 ) = [-17.8787879,0.1852772]
  cd(0:1, 71 ) = [-17.2727273,0.1759316]
  cd(0:1, 72 ) = [-16.6666667,0.1661964]
  cd(0:1, 73 ) = [-16.0606061,0.1564333]
  cd(0:1, 74 ) = [-15.4545455,0.1468782]
  cd(0:1, 75 ) = [-14.8484848,0.1374366]
  cd(0:1, 76 ) = [-14.2424242,0.1282317]
  cd(0:1, 77 ) = [-13.6363636,0.1190623]
  cd(0:1, 78 ) = [-13.0303030,0.1099191]
  cd(0:1, 79 ) = [-12.4242424,0.1007497]
  cd(0:1, 80 ) = [-11.8181818,0.0915280]
  cd(0:1, 81 ) = [-11.2121212,0.0821772]
  cd(0:1, 82 ) = [-10.6060606,0.0727539]
  cd(0:1, 83 ) = [-10.0000000,0.0633080]
  cd(0:1, 84 ) = [-9.3939394,0.0541964]
  cd(0:1, 85 ) = [-8.7878788,0.0451475]
  cd(0:1, 86 ) = [-8.1818182,0.0358305]
  cd(0:1, 87 ) = [-7.5757576,0.0301489]
  cd(0:1, 88 ) = [-6.9696970,0.0259836]
  cd(0:1, 89 ) = [-6.3636364,0.0222212]
  cd(0:1, 90 ) = [-5.7575758,0.0190004]
  cd(0:1, 91 ) = [-5.1515152,0.0166928]
  cd(0:1, 92 ) = [-4.5454545,0.0147079]
  cd(0:1, 93 ) = [-3.9393939,0.0129545]
  cd(0:1, 94 ) = [-3.3333333,0.0123887]
  cd(0:1, 95 ) = [-2.7272727,0.0117323]
  cd(0:1, 96 ) = [-2.1212121,0.0109678]
  cd(0:1, 97 ) = [-1.5151515,0.0106280]
  cd(0:1, 98 ) = [-0.9090909,0.0104100]
  cd(0:1, 99 ) = [-0.3030303,0.0102932]
  cd(0:1, 100) = [0.3030303,0.0102298]
  cd(0:1, 101) = [0.9090909,0.0102203]
  cd(0:1, 102) = [1.5151515,0.0103957]
  cd(0:1, 103) = [2.1212121,0.0106037]
  cd(0:1, 104) = [2.7272727,0.0108116]
  cd(0:1, 105) = [3.3333333,0.0110839]
  cd(0:1, 106) = [3.9393939,0.0114292]
  cd(0:1, 107) = [4.5454545,0.0118860]
  cd(0:1, 108) = [5.1515152,0.0123570]
  cd(0:1, 109) = [5.7575758,0.0128375]
  cd(0:1, 110) = [6.3636364,0.0134311]
  cd(0:1, 111) = [6.9696970,0.0140997]
  cd(0:1, 112) = [7.5757576,0.0149846]
  cd(0:1, 113) = [8.1818182,0.0159667]
  cd(0:1, 114) = [8.7878788,0.0171545]
  cd(0:1, 115) = [9.3939394,0.0195660]
  cd(0:1, 116) = [10.0000000,0.0225666]
  cd(0:1, 117) = [10.6060606,0.0267311]
  cd(0:1, 118) = [11.2121212,0.0317847]
  cd(0:1, 119) = [11.8181818,0.0387531]
  cd(0:1, 120) = [12.4242424,0.0483485]
  cd(0:1, 121) = [13.0303030,0.0591171]
  cd(0:1, 122) = [13.6363636,0.0717169]
  cd(0:1, 123) = [14.2424242,0.0849759]
  cd(0:1, 124) = [14.8484848,0.0992702]
  cd(0:1, 125) = [15.4545455,0.1146073]
  cd(0:1, 126) = [16.0606061,0.1302225]
  cd(0:1, 127) = [16.6666667,0.1452142]
  cd(0:1, 128) = [17.2727273,0.1601653]
  cd(0:1, 129) = [17.8787879,0.1750669]
  cd(0:1, 130) = [18.4848485,0.1898309]
  cd(0:1, 131) = [19.0909091,0.2037727]
  cd(0:1, 132) = [19.6969697,0.2132484]
  cd(0:1, 133) = [20.3030303,0.2230311]
  cd(0:1, 134) = [20.9090909,0.2331209]
  cd(0:1, 135) = [21.5151515,0.2432106]
  cd(0:1, 136) = [22.1212121,0.2534497]
  cd(0:1, 137) = [22.7272727,0.2642859]
  cd(0:1, 138) = [23.3333333,0.2751221]
  cd(0:1, 139) = [23.9393939,0.2859582]
  cd(0:1, 140) = [24.5454545,0.2974186]
  cd(0:1, 141) = [25.1515152,0.3089483]
  cd(0:1, 142) = [25.7575758,0.3204780]
  cd(0:1, 143) = [26.3636364,0.3323901]
  cd(0:1, 144) = [26.9696970,0.3445573]
  cd(0:1, 145) = [27.5757576,0.3567244]
  cd(0:1, 146) = [28.1818182,0.3690647]
  cd(0:1, 147) = [28.7878788,0.3818088]
  cd(0:1, 148) = [29.3939394,0.3945530]
  cd(0:1, 149) = [30.0000000,0.4072971]
  cd(0:1, 150) = [33.0000000,0.4737845]
  cd(0:1, 151) = [36.0000000,0.5430151]
  cd(0:1, 152) = [39.0000000,0.6148265]
  cd(0:1, 153) = [42.0000000,0.6882329]
  cd(0:1, 154) = [45.0000000,0.7620386]
  cd(0:1, 155) = [48.0000000,0.8356765]
  cd(0:1, 156) = [51.0000000,0.9077218]
  cd(0:1, 157) = [54.0000000,0.9765821]
  cd(0:1, 158) = [57.0000000,1.0454418]
  cd(0:1, 159) = [60.0000000,1.1143012]
  cd(0:1, 160) = [63.0000000,1.1698000]
  cd(0:1, 161) = [66.0000000,1.2252987]
  cd(0:1, 162) = [69.0000000,1.2807974]
  cd(0:1, 163) = [72.0000000,1.3227754]
  cd(0:1, 164) = [75.0000000,1.3579933]
  cd(0:1, 165) = [78.0000000,1.3932117]
  cd(0:1, 166) = [81.0000000,1.4201769]
  cd(0:1, 167) = [84.0000000,1.4306364]
  cd(0:1, 168) = [87.0000000,1.4410960]
  cd(0:1, 169) = [90.0000000,1.4515556]
  cd(0:1, 170) = [93.0000000,1.4410960]
  cd(0:1, 171) = [96.0000000,1.4306364]
  cd(0:1, 172) = [99.0000000,1.4201768]
  cd(0:1, 173) = [102.0000000,1.3932115]
  cd(0:1, 174) = [105.0000000,1.3579936]
  cd(0:1, 175) = [108.0000000,1.3227752]
  cd(0:1, 176) = [111.0000000,1.2807969]
  cd(0:1, 177) = [114.0000000,1.2252989]
  cd(0:1, 178) = [117.0000000,1.1698004]
  cd(0:1, 179) = [120.0000000,1.1143016]
  cd(0:1, 180) = [123.0000000,1.0454422]
  cd(0:1, 181) = [126.0000000,0.9765825]
  cd(0:1, 182) = [129.0000000,0.9077223]
  cd(0:1, 183) = [132.0000000,0.8356209]
  cd(0:1, 184) = [135.0000000,0.7618990]
  cd(0:1, 185) = [138.0000000,0.6881773]
  cd(0:1, 186) = [141.0000000,0.6158563]
  cd(0:1, 187) = [144.0000000,0.5463366]
  cd(0:1, 188) = [147.0000000,0.4768170]
  cd(0:1, 189) = [150.0000000,0.4072975]
  cd(0:1, 190) = [153.0000000,0.3505042]
  cd(0:1, 191) = [156.0000000,0.2937109]
  cd(0:1, 192) = [159.0000000,0.2369176]
  cd(0:1, 193) = [162.0000000,0.1961195]
  cd(0:1, 194) = [165.0000000,0.1633192]
  cd(0:1, 195) = [168.0000000,0.1305186]
  cd(0:1, 196) = [171.0000000,0.0977956]
  cd(0:1, 197) = [174.0000000,0.0652277]
  cd(0:1, 198) = [177.0000000,0.0538729]
  cd(0:1, 199) = [180.0000000,0.0531472]

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
!     PRINT*,'alpha < tabulated range in calc_cd10_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd10 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd10_ge28.pro'
  ENDIF

!print,y,cd10

!_________________________________________________________________________
  
END FUNCTION calc_cd10_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd11_ge28( y ) RESULT( cd11 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd11
  INTEGER :: i

  cd11 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.0530838]
  cd(0:1, 1  ) = [-177.0000000,0.0530838]
  cd(0:1, 2  ) = [-174.0000000,0.0637003]
  cd(0:1, 3  ) = [-171.0000000,0.0955502]
  cd(0:1, 4  ) = [-168.0000000,0.1283214]
  cd(0:1, 5  ) = [-165.0000000,0.1615532]
  cd(0:1, 6  ) = [-162.0000000,0.1947855]
  cd(0:1, 7  ) = [-159.0000000,0.2358464]
  cd(0:1, 8  ) = [-156.0000000,0.2925644]
  cd(0:1, 9  ) = [-153.0000000,0.3492825]
  cd(0:1, 10 ) = [-150.0000000,0.4060006]
  cd(0:1, 11 ) = [-147.0000000,0.4756515]
  cd(0:1, 12 ) = [-144.0000000,0.5453024]
  cd(0:1, 13 ) = [-141.0000000,0.6149536]
  cd(0:1, 14 ) = [-138.0000000,0.6874184]
  cd(0:1, 15 ) = [-135.0000000,0.7612900]
  cd(0:1, 16 ) = [-132.0000000,0.8351618]
  cd(0:1, 17 ) = [-129.0000000,0.9074153]
  cd(0:1, 18 ) = [-126.0000000,0.9764321]
  cd(0:1, 19 ) = [-123.0000000,1.0454484]
  cd(0:1, 20 ) = [-120.0000000,1.1144643]
  cd(0:1, 21 ) = [-117.0000000,1.1701143]
  cd(0:1, 22 ) = [-114.0000000,1.2257641]
  cd(0:1, 23 ) = [-111.0000000,1.2814135]
  cd(0:1, 24 ) = [-108.0000000,1.3235335]
  cd(0:1, 25 ) = [-105.0000000,1.3588888]
  cd(0:1, 26 ) = [-102.0000000,1.3942437]
  cd(0:1, 27 ) = [-99.0000000,1.4213384]
  cd(0:1, 28 ) = [-96.0000000,1.4319123]
  cd(0:1, 29 ) = [-93.0000000,1.4424862]
  cd(0:1, 30 ) = [-90.0000000,1.4530599]
  cd(0:1, 31 ) = [-87.0000000,1.4424862]
  cd(0:1, 32 ) = [-84.0000000,1.4319123]
  cd(0:1, 33 ) = [-81.0000000,1.4213385]
  cd(0:1, 34 ) = [-78.0000000,1.3942439]
  cd(0:1, 35 ) = [-75.0000000,1.3588886]
  cd(0:1, 36 ) = [-72.0000000,1.3235337]
  cd(0:1, 37 ) = [-69.0000000,1.2814140]
  cd(0:1, 38 ) = [-66.0000000,1.2257640]
  cd(0:1, 39 ) = [-63.0000000,1.1701140]
  cd(0:1, 40 ) = [-60.0000000,1.1144639]
  cd(0:1, 41 ) = [-57.0000000,1.0454479]
  cd(0:1, 42 ) = [-54.0000000,0.9764317]
  cd(0:1, 43 ) = [-51.0000000,0.9074149]
  cd(0:1, 44 ) = [-48.0000000,0.8352147]
  cd(0:1, 45 ) = [-45.0000000,0.7614230]
  cd(0:1, 46 ) = [-42.0000000,0.6874713]
  cd(0:1, 47 ) = [-39.0000000,0.6139113]
  cd(0:1, 48 ) = [-36.0000000,0.5419412]
  cd(0:1, 49 ) = [-33.0000000,0.4725833]
  cd(0:1, 50 ) = [-30.0000000,0.4060001]
  cd(0:1, 51 ) = [-29.3939394,0.3932214]
  cd(0:1, 52 ) = [-28.7878788,0.3804427]
  cd(0:1, 53 ) = [-28.1818182,0.3676640]
  cd(0:1, 54 ) = [-27.5757576,0.3552936]
  cd(0:1, 55 ) = [-26.9696970,0.3430982]
  cd(0:1, 56 ) = [-26.3636364,0.3309028]
  cd(0:1, 57 ) = [-25.7575758,0.3189652]
  cd(0:1, 58 ) = [-25.1515152,0.3074142]
  cd(0:1, 59 ) = [-24.5454545,0.2958632]
  cd(0:1, 60 ) = [-23.9393939,0.2843822]
  cd(0:1, 61 ) = [-23.3333333,0.2735322]
  cd(0:1, 62 ) = [-22.7272727,0.2626822]
  cd(0:1, 63 ) = [-22.1212121,0.2518321]
  cd(0:1, 64 ) = [-21.5151515,0.2418935]
  cd(0:1, 65 ) = [-20.9090909,0.2321829]
  cd(0:1, 66 ) = [-20.3030303,0.2224722]
  cd(0:1, 67 ) = [-19.6969697,0.2130808]
  cd(0:1, 68 ) = [-19.0909091,0.2040087]
  cd(0:1, 69 ) = [-18.4848485,0.1950107]
  cd(0:1, 70 ) = [-17.8787879,0.1860075]
  cd(0:1, 71 ) = [-17.2727273,0.1768239]
  cd(0:1, 72 ) = [-16.6666667,0.1685930]
  cd(0:1, 73 ) = [-16.0606061,0.1604544]
  cd(0:1, 74 ) = [-15.4545455,0.1517109]
  cd(0:1, 75 ) = [-14.8484848,0.1426253]
  cd(0:1, 76 ) = [-14.2424242,0.1327919]
  cd(0:1, 77 ) = [-13.6363636,0.1228498]
  cd(0:1, 78 ) = [-13.0303030,0.1128238]
  cd(0:1, 79 ) = [-12.4242424,0.1028903]
  cd(0:1, 80 ) = [-11.8181818,0.0931176]
  cd(0:1, 81 ) = [-11.2121212,0.0837383]
  cd(0:1, 82 ) = [-10.6060606,0.0745798]
  cd(0:1, 83 ) = [-10.0000000,0.0655060]
  cd(0:1, 84 ) = [-9.3939394,0.0554692]
  cd(0:1, 85 ) = [-8.7878788,0.0452369]
  cd(0:1, 86 ) = [-8.1818182,0.0355800]
  cd(0:1, 87 ) = [-7.5757576,0.0291667]
  cd(0:1, 88 ) = [-6.9696970,0.0243865]
  cd(0:1, 89 ) = [-6.3636364,0.0206688]
  cd(0:1, 90 ) = [-5.7575758,0.0176933]
  cd(0:1, 91 ) = [-5.1515152,0.0156052]
  cd(0:1, 92 ) = [-4.5454545,0.0138965]
  cd(0:1, 93 ) = [-3.9393939,0.0124333]
  cd(0:1, 94 ) = [-3.3333333,0.0119150]
  cd(0:1, 95 ) = [-2.7272727,0.0113706]
  cd(0:1, 96 ) = [-2.1212121,0.0107859]
  cd(0:1, 97 ) = [-1.5151515,0.0105033]
  cd(0:1, 98 ) = [-0.9090909,0.0103139]
  cd(0:1, 99 ) = [-0.3030303,0.0102094]
  cd(0:1, 100) = [0.3030303,0.0101564]
  cd(0:1, 101) = [0.9090909,0.0101545]
  cd(0:1, 102) = [1.5151515,0.0103195]
  cd(0:1, 103) = [2.1212121,0.0105158]
  cd(0:1, 104) = [2.7272727,0.0107235]
  cd(0:1, 105) = [3.3333333,0.0109892]
  cd(0:1, 106) = [3.9393939,0.0112675]
  cd(0:1, 107) = [4.5454545,0.0116354]
  cd(0:1, 108) = [5.1515152,0.0120214]
  cd(0:1, 109) = [5.7575758,0.0124326]
  cd(0:1, 110) = [6.3636364,0.0129374]
  cd(0:1, 111) = [6.9696970,0.0135052]
  cd(0:1, 112) = [7.5757576,0.0142446]
  cd(0:1, 113) = [8.1818182,0.0150703]
  cd(0:1, 114) = [8.7878788,0.0160681]
  cd(0:1, 115) = [9.3939394,0.0181467]
  cd(0:1, 116) = [10.0000000,0.0209439]
  cd(0:1, 117) = [10.6060606,0.0253974]
  cd(0:1, 118) = [11.2121212,0.0308345]
  cd(0:1, 119) = [11.8181818,0.0375119]
  cd(0:1, 120) = [12.4242424,0.0476822]
  cd(0:1, 121) = [13.0303030,0.0594393]
  cd(0:1, 122) = [13.6363636,0.0718387]
  cd(0:1, 123) = [14.2424242,0.0849147]
  cd(0:1, 124) = [14.8484848,0.0989302]
  cd(0:1, 125) = [15.4545455,0.1139248]
  cd(0:1, 126) = [16.0606061,0.1291763]
  cd(0:1, 127) = [16.6666667,0.1438019]
  cd(0:1, 128) = [17.2727273,0.1584129]
  cd(0:1, 129) = [17.8787879,0.1730059]
  cd(0:1, 130) = [18.4848485,0.1876060]
  cd(0:1, 131) = [19.0909091,0.2014718]
  cd(0:1, 132) = [19.6969697,0.2111647]
  cd(0:1, 133) = [20.3030303,0.2211097]
  cd(0:1, 134) = [20.9090909,0.2313070]
  cd(0:1, 135) = [21.5151515,0.2415043]
  cd(0:1, 136) = [22.1212121,0.2518321]
  cd(0:1, 137) = [22.7272727,0.2626822]
  cd(0:1, 138) = [23.3333333,0.2735322]
  cd(0:1, 139) = [23.9393939,0.2843822]
  cd(0:1, 140) = [24.5454545,0.2958632]
  cd(0:1, 141) = [25.1515152,0.3074142]
  cd(0:1, 142) = [25.7575758,0.3189652]
  cd(0:1, 143) = [26.3636364,0.3309028]
  cd(0:1, 144) = [26.9696970,0.3430982]
  cd(0:1, 145) = [27.5757576,0.3552936]
  cd(0:1, 146) = [28.1818182,0.3676640]
  cd(0:1, 147) = [28.7878788,0.3804427]
  cd(0:1, 148) = [29.3939394,0.3932214]
  cd(0:1, 149) = [30.0000000,0.4060001]
  cd(0:1, 150) = [33.0000000,0.4725833]
  cd(0:1, 151) = [36.0000000,0.5419412]
  cd(0:1, 152) = [39.0000000,0.6139113]
  cd(0:1, 153) = [42.0000000,0.6874713]
  cd(0:1, 154) = [45.0000000,0.7614230]
  cd(0:1, 155) = [48.0000000,0.8352147]
  cd(0:1, 156) = [51.0000000,0.9074149]
  cd(0:1, 157) = [54.0000000,0.9764317]
  cd(0:1, 158) = [57.0000000,1.0454479]
  cd(0:1, 159) = [60.0000000,1.1144639]
  cd(0:1, 160) = [63.0000000,1.1701140]
  cd(0:1, 161) = [66.0000000,1.2257640]
  cd(0:1, 162) = [69.0000000,1.2814140]
  cd(0:1, 163) = [72.0000000,1.3235337]
  cd(0:1, 164) = [75.0000000,1.3588886]
  cd(0:1, 165) = [78.0000000,1.3942439]
  cd(0:1, 166) = [81.0000000,1.4213385]
  cd(0:1, 167) = [84.0000000,1.4319123]
  cd(0:1, 168) = [87.0000000,1.4424862]
  cd(0:1, 169) = [90.0000000,1.4530599]
  cd(0:1, 170) = [93.0000000,1.4424862]
  cd(0:1, 171) = [96.0000000,1.4319123]
  cd(0:1, 172) = [99.0000000,1.4213384]
  cd(0:1, 173) = [102.0000000,1.3942437]
  cd(0:1, 174) = [105.0000000,1.3588888]
  cd(0:1, 175) = [108.0000000,1.3235335]
  cd(0:1, 176) = [111.0000000,1.2814135]
  cd(0:1, 177) = [114.0000000,1.2257641]
  cd(0:1, 178) = [117.0000000,1.1701143]
  cd(0:1, 179) = [120.0000000,1.1144643]
  cd(0:1, 180) = [123.0000000,1.0454484]
  cd(0:1, 181) = [126.0000000,0.9764321]
  cd(0:1, 182) = [129.0000000,0.9074153]
  cd(0:1, 183) = [132.0000000,0.8351618]
  cd(0:1, 184) = [135.0000000,0.7612900]
  cd(0:1, 185) = [138.0000000,0.6874184]
  cd(0:1, 186) = [141.0000000,0.6149536]
  cd(0:1, 187) = [144.0000000,0.5453024]
  cd(0:1, 188) = [147.0000000,0.4756515]
  cd(0:1, 189) = [150.0000000,0.4060006]
  cd(0:1, 190) = [153.0000000,0.3494025]
  cd(0:1, 191) = [156.0000000,0.2928045]
  cd(0:1, 192) = [159.0000000,0.2362066]
  cd(0:1, 193) = [162.0000000,0.1956067]
  cd(0:1, 194) = [165.0000000,0.1630059]
  cd(0:1, 195) = [168.0000000,0.1304049]
  cd(0:1, 196) = [171.0000000,0.0978037]
  cd(0:1, 197) = [174.0000000,0.0652024]
  cd(0:1, 198) = [177.0000000,0.0538259]
  cd(0:1, 199) = [180.0000000,0.0530838]

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
!     PRINT*,'alpha < tabulated range in calc_cd11_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd11 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd11_ge28.pro'
  ENDIF

!print,y,cd11

!_________________________________________________________________________
  
END FUNCTION calc_cd11_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd12_ge28( y ) RESULT( cd12 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd12
  INTEGER :: i

  cd12 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.0517338]
  cd(0:1, 1  ) = [-177.0000000,0.0517338]
  cd(0:1, 2  ) = [-174.0000000,0.0620804]
  cd(0:1, 3  ) = [-171.0000000,0.0931204]
  cd(0:1, 4  ) = [-168.0000000,0.1248854]
  cd(0:1, 5  ) = [-165.0000000,0.1570129]
  cd(0:1, 6  ) = [-162.0000000,0.1891409]
  cd(0:1, 7  ) = [-159.0000000,0.2293569]
  cd(0:1, 8  ) = [-156.0000000,0.2857487]
  cd(0:1, 9  ) = [-153.0000000,0.3421405]
  cd(0:1, 10 ) = [-150.0000000,0.3985323]
  cd(0:1, 11 ) = [-147.0000000,0.4684419]
  cd(0:1, 12 ) = [-144.0000000,0.5383516]
  cd(0:1, 13 ) = [-141.0000000,0.6082615]
  cd(0:1, 14 ) = [-138.0000000,0.6810250]
  cd(0:1, 15 ) = [-135.0000000,0.7552153]
  cd(0:1, 16 ) = [-132.0000000,0.8294061]
  cd(0:1, 17 ) = [-129.0000000,0.9019954]
  cd(0:1, 18 ) = [-126.0000000,0.9713819]
  cd(0:1, 19 ) = [-123.0000000,1.0407678]
  cd(0:1, 20 ) = [-120.0000000,1.1101535]
  cd(0:1, 21 ) = [-117.0000000,1.1662118]
  cd(0:1, 22 ) = [-114.0000000,1.2222698]
  cd(0:1, 23 ) = [-111.0000000,1.2783275]
  cd(0:1, 24 ) = [-108.0000000,1.3208740]
  cd(0:1, 25 ) = [-105.0000000,1.3566650]
  cd(0:1, 26 ) = [-102.0000000,1.3924555]
  cd(0:1, 27 ) = [-99.0000000,1.4199904]
  cd(0:1, 28 ) = [-96.0000000,1.4310137]
  cd(0:1, 29 ) = [-93.0000000,1.4420368]
  cd(0:1, 30 ) = [-90.0000000,1.4530599]
  cd(0:1, 31 ) = [-87.0000000,1.4420368]
  cd(0:1, 32 ) = [-84.0000000,1.4310136]
  cd(0:1, 33 ) = [-81.0000000,1.4199905]
  cd(0:1, 34 ) = [-78.0000000,1.3924557]
  cd(0:1, 35 ) = [-75.0000000,1.3566648]
  cd(0:1, 36 ) = [-72.0000000,1.3208742]
  cd(0:1, 37 ) = [-69.0000000,1.2783279]
  cd(0:1, 38 ) = [-66.0000000,1.2222697]
  cd(0:1, 39 ) = [-63.0000000,1.1662114]
  cd(0:1, 40 ) = [-60.0000000,1.1101531]
  cd(0:1, 41 ) = [-57.0000000,1.0407674]
  cd(0:1, 42 ) = [-54.0000000,0.9713814]
  cd(0:1, 43 ) = [-51.0000000,0.9019950]
  cd(0:1, 44 ) = [-48.0000000,0.8294498]
  cd(0:1, 45 ) = [-45.0000000,0.7553253]
  cd(0:1, 46 ) = [-42.0000000,0.6810687]
  cd(0:1, 47 ) = [-39.0000000,0.6072106]
  cd(0:1, 48 ) = [-36.0000000,0.5349648]
  cd(0:1, 49 ) = [-33.0000000,0.4653521]
  cd(0:1, 50 ) = [-30.0000000,0.3985319]
  cd(0:1, 51 ) = [-29.3939394,0.3857090]
  cd(0:1, 52 ) = [-28.7878788,0.3728861]
  cd(0:1, 53 ) = [-28.1818182,0.3600632]
  cd(0:1, 54 ) = [-27.5757576,0.3476506]
  cd(0:1, 55 ) = [-26.9696970,0.3354138]
  cd(0:1, 56 ) = [-26.3636364,0.3231770]
  cd(0:1, 57 ) = [-25.7575758,0.3111991]
  cd(0:1, 58 ) = [-25.1515152,0.2996095]
  cd(0:1, 59 ) = [-24.5454545,0.2880200]
  cd(0:1, 60 ) = [-23.9393939,0.2765008]
  cd(0:1, 61 ) = [-23.3333333,0.2656151]
  cd(0:1, 62 ) = [-22.7272727,0.2547294]
  cd(0:1, 63 ) = [-22.1212121,0.2438437]
  cd(0:1, 64 ) = [-21.5151515,0.2347656]
  cd(0:1, 65 ) = [-20.9090909,0.2261394]
  cd(0:1, 66 ) = [-20.3030303,0.2175132]
  cd(0:1, 67 ) = [-19.6969697,0.2091383]
  cd(0:1, 68 ) = [-19.0909091,0.2010148]
  cd(0:1, 69 ) = [-18.4848485,0.1931524]
  cd(0:1, 70 ) = [-17.8787879,0.1852719]
  cd(0:1, 71 ) = [-17.2727273,0.1767531]
  cd(0:1, 72 ) = [-16.6666667,0.1696667]
  cd(0:1, 73 ) = [-16.0606061,0.1627696]
  cd(0:1, 74 ) = [-15.4545455,0.1547095]
  cd(0:1, 75 ) = [-14.8484848,0.1459729]
  cd(0:1, 76 ) = [-14.2424242,0.1356952]
  cd(0:1, 77 ) = [-13.6363636,0.1251966]
  cd(0:1, 78 ) = [-13.0303030,0.1145364]
  cd(0:1, 79 ) = [-12.4242424,0.1040715]
  cd(0:1, 80 ) = [-11.8181818,0.0939351]
  cd(0:1, 81 ) = [-11.2121212,0.0845784]
  cd(0:1, 82 ) = [-10.6060606,0.0756687]
  cd(0:1, 83 ) = [-10.0000000,0.0669542]
  cd(0:1, 84 ) = [-9.3939394,0.0562659]
  cd(0:1, 85 ) = [-8.7878788,0.0451632]
  cd(0:1, 86 ) = [-8.1818182,0.0346950]
  cd(0:1, 87 ) = [-7.5757576,0.0277831]
  cd(0:1, 88 ) = [-6.9696970,0.0228417]
  cd(0:1, 89 ) = [-6.3636364,0.0194966]
  cd(0:1, 90 ) = [-5.7575758,0.0168485]
  cd(0:1, 91 ) = [-5.1515152,0.0149611]
  cd(0:1, 92 ) = [-4.5454545,0.0134636]
  cd(0:1, 93 ) = [-3.9393939,0.0121954]
  cd(0:1, 94 ) = [-3.3333333,0.0117062]
  cd(0:1, 95 ) = [-2.7272727,0.0112294]
  cd(0:1, 96 ) = [-2.1212121,0.0107440]
  cd(0:1, 97 ) = [-1.5151515,0.0104902]
  cd(0:1, 98 ) = [-0.9090909,0.0103184]
  cd(0:1, 99 ) = [-0.3030303,0.0102531]
  cd(0:1, 100) = [0.3030303,0.0102310]
  cd(0:1, 101) = [0.9090909,0.0102503]
  cd(0:1, 102) = [1.5151515,0.0103813]
  cd(0:1, 103) = [2.1212121,0.0105416]
  cd(0:1, 104) = [2.7272727,0.0107446]
  cd(0:1, 105) = [3.3333333,0.0109888]
  cd(0:1, 106) = [3.9393939,0.0112345]
  cd(0:1, 107) = [4.5454545,0.0115551]
  cd(0:1, 108) = [5.1515152,0.0118924]
  cd(0:1, 109) = [5.7575758,0.0122611]
  cd(0:1, 110) = [6.3636364,0.0127080]
  cd(0:1, 111) = [6.9696970,0.0132063]
  cd(0:1, 112) = [7.5757576,0.0138302]
  cd(0:1, 113) = [8.1818182,0.0145051]
  cd(0:1, 114) = [8.7878788,0.0152650]
  cd(0:1, 115) = [9.3939394,0.0169597]
  cd(0:1, 116) = [10.0000000,0.0192951]
  cd(0:1, 117) = [10.6060606,0.0232087]
  cd(0:1, 118) = [11.2121212,0.0280043]
  cd(0:1, 119) = [11.8181818,0.0336675]
  cd(0:1, 120) = [12.4242424,0.0453311]
  cd(0:1, 121) = [13.0303030,0.0594850]
  cd(0:1, 122) = [13.6363636,0.0712492]
  cd(0:1, 123) = [14.2424242,0.0834702]
  cd(0:1, 124) = [14.8484848,0.0963164]
  cd(0:1, 125) = [15.4545455,0.1099182]
  cd(0:1, 126) = [16.0606061,0.1237245]
  cd(0:1, 127) = [16.6666667,0.1371048]
  cd(0:1, 128) = [17.2727273,0.1505464]
  cd(0:1, 129) = [17.8787879,0.1640631]
  cd(0:1, 130) = [18.4848485,0.1780106]
  cd(0:1, 131) = [19.0909091,0.1915011]
  cd(0:1, 132) = [19.6969697,0.2017902]
  cd(0:1, 133) = [20.3030303,0.2121972]
  cd(0:1, 134) = [20.9090909,0.2227220]
  cd(0:1, 135) = [21.5151515,0.2332468]
  cd(0:1, 136) = [22.1212121,0.2438437]
  cd(0:1, 137) = [22.7272727,0.2547294]
  cd(0:1, 138) = [23.3333333,0.2656151]
  cd(0:1, 139) = [23.9393939,0.2765008]
  cd(0:1, 140) = [24.5454545,0.2880200]
  cd(0:1, 141) = [25.1515152,0.2996095]
  cd(0:1, 142) = [25.7575758,0.3111991]
  cd(0:1, 143) = [26.3636364,0.3231770]
  cd(0:1, 144) = [26.9696970,0.3354138]
  cd(0:1, 145) = [27.5757576,0.3476506]
  cd(0:1, 146) = [28.1818182,0.3600632]
  cd(0:1, 147) = [28.7878788,0.3728861]
  cd(0:1, 148) = [29.3939394,0.3857090]
  cd(0:1, 149) = [30.0000000,0.3985319]
  cd(0:1, 150) = [33.0000000,0.4653521]
  cd(0:1, 151) = [36.0000000,0.5349648]
  cd(0:1, 152) = [39.0000000,0.6072106]
  cd(0:1, 153) = [42.0000000,0.6810687]
  cd(0:1, 154) = [45.0000000,0.7553253]
  cd(0:1, 155) = [48.0000000,0.8294498]
  cd(0:1, 156) = [51.0000000,0.9019950]
  cd(0:1, 157) = [54.0000000,0.9713814]
  cd(0:1, 158) = [57.0000000,1.0407674]
  cd(0:1, 159) = [60.0000000,1.1101531]
  cd(0:1, 160) = [63.0000000,1.1662114]
  cd(0:1, 161) = [66.0000000,1.2222697]
  cd(0:1, 162) = [69.0000000,1.2783279]
  cd(0:1, 163) = [72.0000000,1.3208742]
  cd(0:1, 164) = [75.0000000,1.3566648]
  cd(0:1, 165) = [78.0000000,1.3924557]
  cd(0:1, 166) = [81.0000000,1.4199905]
  cd(0:1, 167) = [84.0000000,1.4310136]
  cd(0:1, 168) = [87.0000000,1.4420368]
  cd(0:1, 169) = [90.0000000,1.4530599]
  cd(0:1, 170) = [93.0000000,1.4420368]
  cd(0:1, 171) = [96.0000000,1.4310137]
  cd(0:1, 172) = [99.0000000,1.4199904]
  cd(0:1, 173) = [102.0000000,1.3924555]
  cd(0:1, 174) = [105.0000000,1.3566650]
  cd(0:1, 175) = [108.0000000,1.3208740]
  cd(0:1, 176) = [111.0000000,1.2783275]
  cd(0:1, 177) = [114.0000000,1.2222698]
  cd(0:1, 178) = [117.0000000,1.1662118]
  cd(0:1, 179) = [120.0000000,1.1101535]
  cd(0:1, 180) = [123.0000000,1.0407678]
  cd(0:1, 181) = [126.0000000,0.9713819]
  cd(0:1, 182) = [129.0000000,0.9019954]
  cd(0:1, 183) = [132.0000000,0.8294061]
  cd(0:1, 184) = [135.0000000,0.7552153]
  cd(0:1, 185) = [138.0000000,0.6810250]
  cd(0:1, 186) = [141.0000000,0.6082615]
  cd(0:1, 187) = [144.0000000,0.5383516]
  cd(0:1, 188) = [147.0000000,0.4684419]
  cd(0:1, 189) = [150.0000000,0.3985323]
  cd(0:1, 190) = [153.0000000,0.3426089]
  cd(0:1, 191) = [156.0000000,0.2866857]
  cd(0:1, 192) = [159.0000000,0.2307625]
  cd(0:1, 193) = [162.0000000,0.1909094]
  cd(0:1, 194) = [165.0000000,0.1590915]
  cd(0:1, 195) = [168.0000000,0.1272733]
  cd(0:1, 196) = [171.0000000,0.0954550]
  cd(0:1, 197) = [174.0000000,0.0636366]
  cd(0:1, 198) = [177.0000000,0.0525041]

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
!     PRINT*,'alpha < tabulated range in calc_cd12_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd12 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd12_ge28.pro'
  ENDIF

!print,y,cd12

!_________________________________________________________________________
  
END FUNCTION calc_cd12_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd13_ge28( y ) RESULT( cd13 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd13
  INTEGER :: i

  cd13 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.0502018]
  cd(0:1, 1  ) = [-177.0000000,0.0502018]
  cd(0:1, 2  ) = [-174.0000000,0.0602420]
  cd(0:1, 3  ) = [-171.0000000,0.0903629]
  cd(0:1, 4  ) = [-168.0000000,0.1209861]
  cd(0:1, 5  ) = [-165.0000000,0.1518603]
  cd(0:1, 6  ) = [-162.0000000,0.1827352]
  cd(0:1, 7  ) = [-159.0000000,0.2219924]
  cd(0:1, 8  ) = [-156.0000000,0.2780139]
  cd(0:1, 9  ) = [-153.0000000,0.3340354]
  cd(0:1, 10 ) = [-150.0000000,0.3900570]
  cd(0:1, 11 ) = [-147.0000000,0.4602601]
  cd(0:1, 12 ) = [-144.0000000,0.5304633]
  cd(0:1, 13 ) = [-141.0000000,0.6006669]
  cd(0:1, 14 ) = [-138.0000000,0.6737695]
  cd(0:1, 15 ) = [-135.0000000,0.7483216]
  cd(0:1, 16 ) = [-132.0000000,0.8228742]
  cd(0:1, 17 ) = [-129.0000000,0.8958447]
  cd(0:1, 18 ) = [-126.0000000,0.9656507]
  cd(0:1, 19 ) = [-123.0000000,1.0354562]
  cd(0:1, 20 ) = [-120.0000000,1.1052614]
  cd(0:1, 21 ) = [-117.0000000,1.1617830]
  cd(0:1, 22 ) = [-114.0000000,1.2183044]
  cd(0:1, 23 ) = [-111.0000000,1.2748253]
  cd(0:1, 24 ) = [-108.0000000,1.3178559]
  cd(0:1, 25 ) = [-105.0000000,1.3541413]
  cd(0:1, 26 ) = [-102.0000000,1.3904262]
  cd(0:1, 27 ) = [-99.0000000,1.4184607]
  cd(0:1, 28 ) = [-96.0000000,1.4299938]
  cd(0:1, 29 ) = [-93.0000000,1.4415269]
  cd(0:1, 30 ) = [-90.0000000,1.4530599]
  cd(0:1, 31 ) = [-87.0000000,1.4415269]
  cd(0:1, 32 ) = [-84.0000000,1.4299938]
  cd(0:1, 33 ) = [-81.0000000,1.4184608]
  cd(0:1, 34 ) = [-78.0000000,1.3904264]
  cd(0:1, 35 ) = [-75.0000000,1.3541410]
  cd(0:1, 36 ) = [-72.0000000,1.3178561]
  cd(0:1, 37 ) = [-69.0000000,1.2748258]
  cd(0:1, 38 ) = [-66.0000000,1.2183042]
  cd(0:1, 39 ) = [-63.0000000,1.1617826]
  cd(0:1, 40 ) = [-60.0000000,1.1052610]
  cd(0:1, 41 ) = [-57.0000000,1.0354557]
  cd(0:1, 42 ) = [-54.0000000,0.9656502]
  cd(0:1, 43 ) = [-51.0000000,0.8958442]
  cd(0:1, 44 ) = [-48.0000000,0.8229075]
  cd(0:1, 45 ) = [-45.0000000,0.7484055]
  cd(0:1, 46 ) = [-42.0000000,0.6738028]
  cd(0:1, 47 ) = [-39.0000000,0.5996063]
  cd(0:1, 48 ) = [-36.0000000,0.5270476]
  cd(0:1, 49 ) = [-33.0000000,0.4571459]
  cd(0:1, 50 ) = [-30.0000000,0.3900566]
  cd(0:1, 51 ) = [-29.3939394,0.3771836]
  cd(0:1, 52 ) = [-28.7878788,0.3643105]
  cd(0:1, 53 ) = [-28.1818182,0.3514375]
  cd(0:1, 54 ) = [-27.5757576,0.3389769]
  cd(0:1, 55 ) = [-26.9696970,0.3266931]
  cd(0:1, 56 ) = [-26.3636364,0.3144093]
  cd(0:1, 57 ) = [-25.7575758,0.3023857]
  cd(0:1, 58 ) = [-25.1515152,0.2907524]
  cd(0:1, 59 ) = [-24.5454545,0.2791191]
  cd(0:1, 60 ) = [-23.9393939,0.2675565]
  cd(0:1, 61 ) = [-23.3333333,0.2566304]
  cd(0:1, 62 ) = [-22.7272727,0.2457043]
  cd(0:1, 63 ) = [-22.1212121,0.2347781]
  cd(0:1, 64 ) = [-21.5151515,0.2266764]
  cd(0:1, 65 ) = [-20.9090909,0.2192810]
  cd(0:1, 66 ) = [-20.3030303,0.2118855]
  cd(0:1, 67 ) = [-19.6969697,0.2046642]
  cd(0:1, 68 ) = [-19.0909091,0.1976171]
  cd(0:1, 69 ) = [-18.4848485,0.1909949]
  cd(0:1, 70 ) = [-17.8787879,0.1843433]
  cd(0:1, 71 ) = [-17.2727273,0.1766526]
  cd(0:1, 72 ) = [-16.6666667,0.1704065]
  cd(0:1, 73 ) = [-16.0606061,0.1644055]
  cd(0:1, 74 ) = [-15.4545455,0.1568629]
  cd(0:1, 75 ) = [-14.8484848,0.1484038]
  cd(0:1, 76 ) = [-14.2424242,0.1377950]
  cd(0:1, 77 ) = [-13.6363636,0.1268812]
  cd(0:1, 78 ) = [-13.0303030,0.1157519]
  cd(0:1, 79 ) = [-12.4242424,0.1049007]
  cd(0:1, 80 ) = [-11.8181818,0.0945041]
  cd(0:1, 81 ) = [-11.2121212,0.0851661]
  cd(0:1, 82 ) = [-10.6060606,0.0764433]
  cd(0:1, 83 ) = [-10.0000000,0.0680131]
  cd(0:1, 84 ) = [-9.3939394,0.0568394]
  cd(0:1, 85 ) = [-8.7878788,0.0450577]
  cd(0:1, 86 ) = [-8.1818182,0.0335557]
  cd(0:1, 87 ) = [-7.5757576,0.0262982]
  cd(0:1, 88 ) = [-6.9696970,0.0214176]
  cd(0:1, 89 ) = [-6.3636364,0.0185403]
  cd(0:1, 90 ) = [-5.7575758,0.0162205]
  cd(0:1, 91 ) = [-5.1515152,0.0145075]
  cd(0:1, 92 ) = [-4.5454545,0.0131752]
  cd(0:1, 93 ) = [-3.9393939,0.0120463]
  cd(0:1, 94 ) = [-3.3333333,0.0115766]
  cd(0:1, 95 ) = [-2.7272727,0.0111444]
  cd(0:1, 96 ) = [-2.1212121,0.0107205]
  cd(0:1, 97 ) = [-1.5151515,0.0104830]
  cd(0:1, 98 ) = [-0.9090909,0.0103234]
  cd(0:1, 99 ) = [-0.3030303,0.0103026]
  cd(0:1, 100) = [0.3030303,0.0103156]
  cd(0:1, 101) = [0.9090909,0.0103591]
  cd(0:1, 102) = [1.5151515,0.0104514]
  cd(0:1, 103) = [2.1212121,0.0105708]
  cd(0:1, 104) = [2.7272727,0.0107685]
  cd(0:1, 105) = [3.3333333,0.0109885]
  cd(0:1, 106) = [3.9393939,0.0112161]
  cd(0:1, 107) = [4.5454545,0.0115084]
  cd(0:1, 108) = [5.1515152,0.0118149]
  cd(0:1, 109) = [5.7575758,0.0121557]
  cd(0:1, 110) = [6.3636364,0.0125630]
  cd(0:1, 111) = [6.9696970,0.0130120]
  cd(0:1, 112) = [7.5757576,0.0135489]
  cd(0:1, 113) = [8.1818182,0.0141028]
  cd(0:1, 114) = [8.7878788,0.0146613]
  cd(0:1, 115) = [9.3939394,0.0160082]
  cd(0:1, 116) = [10.0000000,0.0178443]
  cd(0:1, 117) = [10.6060606,0.0209589]
  cd(0:1, 118) = [11.2121212,0.0247925]
  cd(0:1, 119) = [11.8181818,0.0293047]
  cd(0:1, 120) = [12.4242424,0.0426631]
  cd(0:1, 121) = [13.0303030,0.0595101]
  cd(0:1, 122) = [13.6363636,0.0705802]
  cd(0:1, 123) = [14.2424242,0.0818308]
  cd(0:1, 124) = [14.8484848,0.0933501]
  cd(0:1, 125) = [15.4545455,0.1053713]
  cd(0:1, 126) = [16.0606061,0.1175375]
  cd(0:1, 127) = [16.6666667,0.1295045]
  cd(0:1, 128) = [17.2727273,0.1416191]
  cd(0:1, 129) = [17.8787879,0.1539143]
  cd(0:1, 130) = [18.4848485,0.1671213]
  cd(0:1, 131) = [19.0909091,0.1801859]
  cd(0:1, 132) = [19.6969697,0.1911517]
  cd(0:1, 133) = [20.3030303,0.2020828]
  cd(0:1, 134) = [20.9090909,0.2129793]
  cd(0:1, 135) = [21.5151515,0.2238757]
  cd(0:1, 136) = [22.1212121,0.2347781]
  cd(0:1, 137) = [22.7272727,0.2457043]
  cd(0:1, 138) = [23.3333333,0.2566304]
  cd(0:1, 139) = [23.9393939,0.2675565]
  cd(0:1, 140) = [24.5454545,0.2791191]
  cd(0:1, 141) = [25.1515152,0.2907524]
  cd(0:1, 142) = [25.7575758,0.3023857]
  cd(0:1, 143) = [26.3636364,0.3144093]
  cd(0:1, 144) = [26.9696970,0.3266931]
  cd(0:1, 145) = [27.5757576,0.3389769]
  cd(0:1, 146) = [28.1818182,0.3514375]
  cd(0:1, 147) = [28.7878788,0.3643105]
  cd(0:1, 148) = [29.3939394,0.3771836]
  cd(0:1, 149) = [30.0000000,0.3900566]
  cd(0:1, 150) = [33.0000000,0.4571459]
  cd(0:1, 151) = [36.0000000,0.5270476]
  cd(0:1, 152) = [39.0000000,0.5996063]
  cd(0:1, 153) = [42.0000000,0.6738028]
  cd(0:1, 154) = [45.0000000,0.7484055]
  cd(0:1, 155) = [48.0000000,0.8229075]
  cd(0:1, 156) = [51.0000000,0.8958442]
  cd(0:1, 157) = [54.0000000,0.9656502]
  cd(0:1, 158) = [57.0000000,1.0354557]
  cd(0:1, 159) = [60.0000000,1.1052610]
  cd(0:1, 160) = [63.0000000,1.1617826]
  cd(0:1, 161) = [66.0000000,1.2183042]
  cd(0:1, 162) = [69.0000000,1.2748258]
  cd(0:1, 163) = [72.0000000,1.3178561]
  cd(0:1, 164) = [75.0000000,1.3541410]
  cd(0:1, 165) = [78.0000000,1.3904264]
  cd(0:1, 166) = [81.0000000,1.4184608]
  cd(0:1, 167) = [84.0000000,1.4299938]
  cd(0:1, 168) = [87.0000000,1.4415269]
  cd(0:1, 169) = [90.0000000,1.4530599]
  cd(0:1, 170) = [93.0000000,1.4415269]
  cd(0:1, 171) = [96.0000000,1.4299938]
  cd(0:1, 172) = [99.0000000,1.4184607]
  cd(0:1, 173) = [102.0000000,1.3904262]
  cd(0:1, 174) = [105.0000000,1.3541413]
  cd(0:1, 175) = [108.0000000,1.3178559]
  cd(0:1, 176) = [111.0000000,1.2748253]
  cd(0:1, 177) = [114.0000000,1.2183044]
  cd(0:1, 178) = [117.0000000,1.1617830]
  cd(0:1, 179) = [120.0000000,1.1052614]
  cd(0:1, 180) = [123.0000000,1.0354562]
  cd(0:1, 181) = [126.0000000,0.9656507]
  cd(0:1, 182) = [129.0000000,0.8958447]
  cd(0:1, 183) = [132.0000000,0.8228742]
  cd(0:1, 184) = [135.0000000,0.7483216]
  cd(0:1, 185) = [138.0000000,0.6737695]
  cd(0:1, 186) = [141.0000000,0.6006669]
  cd(0:1, 187) = [144.0000000,0.5304633]
  cd(0:1, 188) = [147.0000000,0.4602601]
  cd(0:1, 189) = [150.0000000,0.3900570]
  cd(0:1, 190) = [153.0000000,0.3348993]
  cd(0:1, 191) = [156.0000000,0.2797417]
  cd(0:1, 192) = [159.0000000,0.2245843]
  cd(0:1, 193) = [162.0000000,0.1855787]
  cd(0:1, 194) = [165.0000000,0.1546492]
  cd(0:1, 195) = [168.0000000,0.1237194]
  cd(0:1, 196) = [171.0000000,0.0927896]
  cd(0:1, 197) = [174.0000000,0.0618598]
  cd(0:1, 198) = [177.0000000,0.0510040]
  cd(0:1, 199) = [180.0000000,0.0502018]

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
!     PRINT*,'alpha < tabulated range in calc_cd13_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd13 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd13_ge28.pro'
  ENDIF

!print,y,cd13

!_________________________________________________________________________
  
END FUNCTION calc_cd13_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd14_ge28( y ) RESULT( cd14 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd14
  INTEGER :: i

  cd14 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.0487439]
  cd(0:1, 1  ) = [-177.0000000,0.0487439]
  cd(0:1, 2  ) = [-174.0000000,0.0584926]
  cd(0:1, 3  ) = [-171.0000000,0.0877389]
  cd(0:1, 4  ) = [-168.0000000,0.1172754]
  cd(0:1, 5  ) = [-165.0000000,0.1469571]
  cd(0:1, 6  ) = [-162.0000000,0.1766394]
  cd(0:1, 7  ) = [-159.0000000,0.2149842]
  cd(0:1, 8  ) = [-156.0000000,0.2706534]
  cd(0:1, 9  ) = [-153.0000000,0.3263226]
  cd(0:1, 10 ) = [-150.0000000,0.3819919]
  cd(0:1, 11 ) = [-147.0000000,0.4524742]
  cd(0:1, 12 ) = [-144.0000000,0.5229569]
  cd(0:1, 13 ) = [-141.0000000,0.5934399]
  cd(0:1, 14 ) = [-138.0000000,0.6668651]
  cd(0:1, 15 ) = [-135.0000000,0.7417614]
  cd(0:1, 16 ) = [-132.0000000,0.8166584]
  cd(0:1, 17 ) = [-129.0000000,0.8899915]
  cd(0:1, 18 ) = [-126.0000000,0.9601968]
  cd(0:1, 19 ) = [-123.0000000,1.0304016]
  cd(0:1, 20 ) = [-120.0000000,1.1006060]
  cd(0:1, 21 ) = [-117.0000000,1.1575685]
  cd(0:1, 22 ) = [-114.0000000,1.2145308]
  cd(0:1, 23 ) = [-111.0000000,1.2714926]
  cd(0:1, 24 ) = [-108.0000000,1.3149839]
  cd(0:1, 25 ) = [-105.0000000,1.3517397]
  cd(0:1, 26 ) = [-102.0000000,1.3884951]
  cd(0:1, 27 ) = [-99.0000000,1.4170049]
  cd(0:1, 28 ) = [-96.0000000,1.4290233]
  cd(0:1, 29 ) = [-93.0000000,1.4410417]
  cd(0:1, 30 ) = [-90.0000000,1.4530599]
  cd(0:1, 31 ) = [-87.0000000,1.4410417]
  cd(0:1, 32 ) = [-84.0000000,1.4290233]
  cd(0:1, 33 ) = [-81.0000000,1.4170050]
  cd(0:1, 34 ) = [-78.0000000,1.3884953]
  cd(0:1, 35 ) = [-75.0000000,1.3517394]
  cd(0:1, 36 ) = [-72.0000000,1.3149840]
  cd(0:1, 37 ) = [-69.0000000,1.2714931]
  cd(0:1, 38 ) = [-66.0000000,1.2145306]
  cd(0:1, 39 ) = [-63.0000000,1.1575682]
  cd(0:1, 40 ) = [-60.0000000,1.1006056]
  cd(0:1, 41 ) = [-57.0000000,1.0304011]
  cd(0:1, 42 ) = [-54.0000000,0.9601963]
  cd(0:1, 43 ) = [-51.0000000,0.8899911]
  cd(0:1, 44 ) = [-48.0000000,0.8166817]
  cd(0:1, 45 ) = [-45.0000000,0.7418205]
  cd(0:1, 46 ) = [-42.0000000,0.6668884]
  cd(0:1, 47 ) = [-39.0000000,0.5923700]
  cd(0:1, 48 ) = [-36.0000000,0.5195135]
  cd(0:1, 49 ) = [-33.0000000,0.4493367]
  cd(0:1, 50 ) = [-30.0000000,0.3819914]
  cd(0:1, 51 ) = [-29.3939394,0.3690707]
  cd(0:1, 52 ) = [-28.7878788,0.3561499]
  cd(0:1, 53 ) = [-28.1818182,0.3432292]
  cd(0:1, 54 ) = [-27.5757576,0.3307229]
  cd(0:1, 55 ) = [-26.9696970,0.3183944]
  cd(0:1, 56 ) = [-26.3636364,0.3060658]
  cd(0:1, 57 ) = [-25.7575758,0.2939988]
  cd(0:1, 58 ) = [-25.1515152,0.2823239]
  cd(0:1, 59 ) = [-24.5454545,0.2706490]
  cd(0:1, 60 ) = [-23.9393939,0.2590451]
  cd(0:1, 61 ) = [-23.3333333,0.2480805]
  cd(0:1, 62 ) = [-22.7272727,0.2371159]
  cd(0:1, 63 ) = [-22.1212121,0.2261512]
  cd(0:1, 64 ) = [-21.5151515,0.2189787]
  cd(0:1, 65 ) = [-20.9090909,0.2127544]
  cd(0:1, 66 ) = [-20.3030303,0.2065302]
  cd(0:1, 67 ) = [-19.6969697,0.2004066]
  cd(0:1, 68 ) = [-19.0909091,0.1943838]
  cd(0:1, 69 ) = [-18.4848485,0.1888681]
  cd(0:1, 70 ) = [-17.8787879,0.1833171]
  cd(0:1, 71 ) = [-17.2727273,0.1765264]
  cd(0:1, 72 ) = [-16.6666667,0.1709322]
  cd(0:1, 73 ) = [-16.0606061,0.1655930]
  cd(0:1, 74 ) = [-15.4545455,0.1584467]
  cd(0:1, 75 ) = [-14.8484848,0.1502075]
  cd(0:1, 76 ) = [-14.2424242,0.1393481]
  cd(0:1, 77 ) = [-13.6363636,0.1281199]
  cd(0:1, 78 ) = [-13.0303030,0.1166374]
  cd(0:1, 79 ) = [-12.4242424,0.1054992]
  cd(0:1, 80 ) = [-11.8181818,0.0949120]
  cd(0:1, 81 ) = [-11.2121212,0.0855892]
  cd(0:1, 82 ) = [-10.6060606,0.0770088]
  cd(0:1, 83 ) = [-10.0000000,0.0688029]
  cd(0:1, 84 ) = [-9.3939394,0.0572620]
  cd(0:1, 85 ) = [-8.7878788,0.0449241]
  cd(0:1, 86 ) = [-8.1818182,0.0322667]
  cd(0:1, 87 ) = [-7.5757576,0.0247696]
  cd(0:1, 88 ) = [-6.9696970,0.0200640]
  cd(0:1, 89 ) = [-6.3636364,0.0176704]
  cd(0:1, 90 ) = [-5.7575758,0.0156676]
  cd(0:1, 91 ) = [-5.1515152,0.0141171]
  cd(0:1, 92 ) = [-4.5454545,0.0129300]
  cd(0:1, 93 ) = [-3.9393939,0.0119194]
  cd(0:1, 94 ) = [-3.3333333,0.0114662]
  cd(0:1, 95 ) = [-2.7272727,0.0110713]
  cd(0:1, 96 ) = [-2.1212121,0.0106995]
  cd(0:1, 97 ) = [-1.5151515,0.0104765]
  cd(0:1, 98 ) = [-0.9090909,0.0103282]
  cd(0:1, 99 ) = [-0.3030303,0.0103497]
  cd(0:1, 100) = [0.3030303,0.0103961]
  cd(0:1, 101) = [0.9090909,0.0104626]
  cd(0:1, 102) = [1.5151515,0.0105182]
  cd(0:1, 103) = [2.1212121,0.0105987]
  cd(0:1, 104) = [2.7272727,0.0107914]
  cd(0:1, 105) = [3.3333333,0.0109883]
  cd(0:1, 106) = [3.9393939,0.0111996]
  cd(0:1, 107) = [4.5454545,0.0114677]
  cd(0:1, 108) = [5.1515152,0.0117484]
  cd(0:1, 109) = [5.7575758,0.0120661]
  cd(0:1, 110) = [6.3636364,0.0124411]
  cd(0:1, 111) = [6.9696970,0.0128500]
  cd(0:1, 112) = [7.5757576,0.0133186]
  cd(0:1, 113) = [8.1818182,0.0137822]
  cd(0:1, 114) = [8.7878788,0.0142015]
  cd(0:1, 115) = [9.3939394,0.0152499]
  cd(0:1, 116) = [10.0000000,0.0166203]
  cd(0:1, 117) = [10.6060606,0.0189050]
  cd(0:1, 118) = [11.2121212,0.0217361]
  cd(0:1, 119) = [11.8181818,0.0251530]
  cd(0:1, 120) = [12.4242424,0.0401241]
  cd(0:1, 121) = [13.0303030,0.0595330]
  cd(0:1, 122) = [13.6363636,0.0699437]
  cd(0:1, 123) = [14.2424242,0.0802708]
  cd(0:1, 124) = [14.8484848,0.0905274]
  cd(0:1, 125) = [15.4545455,0.1010444]
  cd(0:1, 126) = [16.0606061,0.1116499]
  cd(0:1, 127) = [16.6666667,0.1222720]
  cd(0:1, 128) = [17.2727273,0.1331239]
  cd(0:1, 129) = [17.8787879,0.1442567]
  cd(0:1, 130) = [18.4848485,0.1567590]
  cd(0:1, 131) = [19.0909091,0.1694183]
  cd(0:1, 132) = [19.6969697,0.1810279]
  cd(0:1, 133) = [20.3030303,0.1924579]
  cd(0:1, 134) = [20.9090909,0.2037080]
  cd(0:1, 135) = [21.5151515,0.2149582]
  cd(0:1, 136) = [22.1212121,0.2261512]
  cd(0:1, 137) = [22.7272727,0.2371159]
  cd(0:1, 138) = [23.3333333,0.2480805]
  cd(0:1, 139) = [23.9393939,0.2590451]
  cd(0:1, 140) = [24.5454545,0.2706490]
  cd(0:1, 141) = [25.1515152,0.2823239]
  cd(0:1, 142) = [25.7575758,0.2939988]
  cd(0:1, 143) = [26.3636364,0.3060658]
  cd(0:1, 144) = [26.9696970,0.3183944]
  cd(0:1, 145) = [27.5757576,0.3307229]
  cd(0:1, 146) = [28.1818182,0.3432292]
  cd(0:1, 147) = [28.7878788,0.3561499]
  cd(0:1, 148) = [29.3939394,0.3690707]
  cd(0:1, 149) = [30.0000000,0.3819914]
  cd(0:1, 150) = [33.0000000,0.4493367]
  cd(0:1, 151) = [36.0000000,0.5195135]
  cd(0:1, 152) = [39.0000000,0.5923700]
  cd(0:1, 153) = [42.0000000,0.6668884]
  cd(0:1, 154) = [45.0000000,0.7418205]
  cd(0:1, 155) = [48.0000000,0.8166817]
  cd(0:1, 156) = [51.0000000,0.8899911]
  cd(0:1, 157) = [54.0000000,0.9601963]
  cd(0:1, 158) = [57.0000000,1.0304011]
  cd(0:1, 159) = [60.0000000,1.1006056]
  cd(0:1, 160) = [63.0000000,1.1575682]
  cd(0:1, 161) = [66.0000000,1.2145306]
  cd(0:1, 162) = [69.0000000,1.2714931]
  cd(0:1, 163) = [72.0000000,1.3149840]
  cd(0:1, 164) = [75.0000000,1.3517394]
  cd(0:1, 165) = [78.0000000,1.3884953]
  cd(0:1, 166) = [81.0000000,1.4170050]
  cd(0:1, 167) = [84.0000000,1.4290233]
  cd(0:1, 168) = [87.0000000,1.4410417]
  cd(0:1, 169) = [90.0000000,1.4530599]
  cd(0:1, 170) = [93.0000000,1.4410417]
  cd(0:1, 171) = [96.0000000,1.4290233]
  cd(0:1, 172) = [99.0000000,1.4170049]
  cd(0:1, 173) = [102.0000000,1.3884951]
  cd(0:1, 174) = [105.0000000,1.3517397]
  cd(0:1, 175) = [108.0000000,1.3149839]
  cd(0:1, 176) = [111.0000000,1.2714926]
  cd(0:1, 177) = [114.0000000,1.2145308]
  cd(0:1, 178) = [117.0000000,1.1575685]
  cd(0:1, 179) = [120.0000000,1.1006060]
  cd(0:1, 180) = [123.0000000,1.0304016]
  cd(0:1, 181) = [126.0000000,0.9601968]
  cd(0:1, 182) = [129.0000000,0.8899915]
  cd(0:1, 183) = [132.0000000,0.8166584]
  cd(0:1, 184) = [135.0000000,0.7417614]
  cd(0:1, 185) = [138.0000000,0.6668651]
  cd(0:1, 186) = [141.0000000,0.5934399]
  cd(0:1, 187) = [144.0000000,0.5229569]
  cd(0:1, 188) = [147.0000000,0.4524742]
  cd(0:1, 189) = [150.0000000,0.3819919]
  cd(0:1, 190) = [153.0000000,0.3275627]
  cd(0:1, 191) = [156.0000000,0.2731337]
  cd(0:1, 192) = [159.0000000,0.2187050]
  cd(0:1, 193) = [162.0000000,0.1805059]
  cd(0:1, 194) = [165.0000000,0.1504218]
  cd(0:1, 195) = [168.0000000,0.1203375]
  cd(0:1, 196) = [171.0000000,0.0902532]
  cd(0:1, 197) = [174.0000000,0.0601689]
  cd(0:1, 198) = [177.0000000,0.0495766]
  cd(0:1, 199) = [180.0000000,0.0487439]

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
!     PRINT*,'alpha < tabulated range in calc_cd14_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd14 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd14_ge28.pro'
  ENDIF

!print,y,cd14

!_________________________________________________________________________
  
END FUNCTION calc_cd14_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd15_ge28( y ) RESULT( cd15 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd15
  INTEGER :: i

  cd15 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.0474972]
  cd(0:1, 1  ) = [-177.0000000,0.0474972]
  cd(0:1, 2  ) = [-174.0000000,0.0569965]
  cd(0:1, 3  ) = [-171.0000000,0.0854949]
  cd(0:1, 4  ) = [-168.0000000,0.1141022]
  cd(0:1, 5  ) = [-165.0000000,0.1427639]
  cd(0:1, 6  ) = [-162.0000000,0.1714265]
  cd(0:1, 7  ) = [-159.0000000,0.2089910]
  cd(0:1, 8  ) = [-156.0000000,0.2643589]
  cd(0:1, 9  ) = [-153.0000000,0.3197268]
  cd(0:1, 10 ) = [-150.0000000,0.3750947]
  cd(0:1, 11 ) = [-147.0000000,0.4458160]
  cd(0:1, 12 ) = [-144.0000000,0.5165375]
  cd(0:1, 13 ) = [-141.0000000,0.5872595]
  cd(0:1, 14 ) = [-138.0000000,0.6609606]
  cd(0:1, 15 ) = [-135.0000000,0.7361513]
  cd(0:1, 16 ) = [-132.0000000,0.8113428]
  cd(0:1, 17 ) = [-129.0000000,0.8849861]
  cd(0:1, 18 ) = [-126.0000000,0.9555328]
  cd(0:1, 19 ) = [-123.0000000,1.0260790]
  cd(0:1, 20 ) = [-120.0000000,1.0966249]
  cd(0:1, 21 ) = [-117.0000000,1.1539644]
  cd(0:1, 22 ) = [-114.0000000,1.2113037]
  cd(0:1, 23 ) = [-111.0000000,1.2686426]
  cd(0:1, 24 ) = [-108.0000000,1.3125277]
  cd(0:1, 25 ) = [-105.0000000,1.3496859]
  cd(0:1, 26 ) = [-102.0000000,1.3868436]
  cd(0:1, 27 ) = [-99.0000000,1.4157600]
  cd(0:1, 28 ) = [-96.0000000,1.4281934]
  cd(0:1, 29 ) = [-93.0000000,1.4406267]
  cd(0:1, 30 ) = [-90.0000000,1.4530599]
  cd(0:1, 31 ) = [-87.0000000,1.4406267]
  cd(0:1, 32 ) = [-84.0000000,1.4281934]
  cd(0:1, 33 ) = [-81.0000000,1.4157601]
  cd(0:1, 34 ) = [-78.0000000,1.3868438]
  cd(0:1, 35 ) = [-75.0000000,1.3496856]
  cd(0:1, 36 ) = [-72.0000000,1.3125279]
  cd(0:1, 37 ) = [-69.0000000,1.2686431]
  cd(0:1, 38 ) = [-66.0000000,1.2113036]
  cd(0:1, 39 ) = [-63.0000000,1.1539641]
  cd(0:1, 40 ) = [-60.0000000,1.0966244]
  cd(0:1, 41 ) = [-57.0000000,1.0260785]
  cd(0:1, 42 ) = [-54.0000000,0.9555323]
  cd(0:1, 43 ) = [-51.0000000,0.8849856]
  cd(0:1, 44 ) = [-48.0000000,0.8113577]
  cd(0:1, 45 ) = [-45.0000000,0.7361891]
  cd(0:1, 46 ) = [-42.0000000,0.6609755]
  cd(0:1, 47 ) = [-39.0000000,0.5861817]
  cd(0:1, 48 ) = [-36.0000000,0.5130705]
  cd(0:1, 49 ) = [-33.0000000,0.4426585]
  cd(0:1, 50 ) = [-30.0000000,0.3750943]
  cd(0:1, 51 ) = [-29.3939394,0.3621327]
  cd(0:1, 52 ) = [-28.7878788,0.3491712]
  cd(0:1, 53 ) = [-28.1818182,0.3362096]
  cd(0:1, 54 ) = [-27.5757576,0.3236644]
  cd(0:1, 55 ) = [-26.9696970,0.3112976]
  cd(0:1, 56 ) = [-26.3636364,0.2989308]
  cd(0:1, 57 ) = [-25.7575758,0.2868265]
  cd(0:1, 58 ) = [-25.1515152,0.2751160]
  cd(0:1, 59 ) = [-24.5454545,0.2634056]
  cd(0:1, 60 ) = [-23.9393939,0.2517664]
  cd(0:1, 61 ) = [-23.3333333,0.2407688]
  cd(0:1, 62 ) = [-22.7272727,0.2297713]
  cd(0:1, 63 ) = [-22.1212121,0.2187737]
  cd(0:1, 64 ) = [-21.5151515,0.2123958]
  cd(0:1, 65 ) = [-20.9090909,0.2071731]
  cd(0:1, 66 ) = [-20.3030303,0.2019504]
  cd(0:1, 67 ) = [-19.6969697,0.1967656]
  cd(0:1, 68 ) = [-19.0909091,0.1916188]
  cd(0:1, 69 ) = [-18.4848485,0.1869194]
  cd(0:1, 70 ) = [-17.8787879,0.1821890]
  cd(0:1, 71 ) = [-17.2727273,0.1763647]
  cd(0:1, 72 ) = [-16.6666667,0.1712953]
  cd(0:1, 73 ) = [-16.0606061,0.1664295]
  cd(0:1, 74 ) = [-15.4545455,0.1595754]
  cd(0:1, 75 ) = [-14.8484848,0.1515029]
  cd(0:1, 76 ) = [-14.2424242,0.1404604]
  cd(0:1, 77 ) = [-13.6363636,0.1290024]
  cd(0:1, 78 ) = [-13.0303030,0.1172633]
  cd(0:1, 79 ) = [-12.4242424,0.1059186]
  cd(0:1, 80 ) = [-11.8181818,0.0951960]
  cd(0:1, 81 ) = [-11.2121212,0.0858849]
  cd(0:1, 82 ) = [-10.6060606,0.0774090]
  cd(0:1, 83 ) = [-10.0000000,0.0693728]
  cd(0:1, 84 ) = [-9.3939394,0.0575636]
  cd(0:1, 85 ) = [-8.7878788,0.0447516]
  cd(0:1, 86 ) = [-8.1818182,0.0308042]
  cd(0:1, 87 ) = [-7.5757576,0.0231580]
  cd(0:1, 88 ) = [-6.9696970,0.0187033]
  cd(0:1, 89 ) = [-6.3636364,0.0167784]
  cd(0:1, 90 ) = [-5.7575758,0.0150845]
  cd(0:1, 91 ) = [-5.1515152,0.0136983]
  cd(0:1, 92 ) = [-4.5454545,0.0126562]
  cd(0:1, 93 ) = [-3.9393939,0.0117673]
  cd(0:1, 94 ) = [-3.3333333,0.0113325]
  cd(0:1, 95 ) = [-2.7272727,0.0109783]
  cd(0:1, 96 ) = [-2.1212121,0.0106696]
  cd(0:1, 97 ) = [-1.5151515,0.0104667]
  cd(0:1, 98 ) = [-0.9090909,0.0103323]
  cd(0:1, 99 ) = [-0.3030303,0.0103900]
  cd(0:1, 100) = [0.3030303,0.0104649]
  cd(0:1, 101) = [0.9090909,0.0105512]
  cd(0:1, 102) = [1.5151515,0.0105752]
  cd(0:1, 103) = [2.1212121,0.0106225]
  cd(0:1, 104) = [2.7272727,0.0108109]
  cd(0:1, 105) = [3.3333333,0.0109879]
  cd(0:1, 106) = [3.9393939,0.0111759]
  cd(0:1, 107) = [4.5454545,0.0114129]
  cd(0:1, 108) = [5.1515152,0.0116638]
  cd(0:1, 109) = [5.7575758,0.0119568]
  cd(0:1, 110) = [6.3636364,0.0122991]
  cd(0:1, 111) = [6.9696970,0.0126699]
  cd(0:1, 112) = [7.5757576,0.0130839]
  cd(0:1, 113) = [8.1818182,0.0134927]
  cd(0:1, 114) = [8.7878788,0.0138638]
  cd(0:1, 115) = [9.3939394,0.0146730]
  cd(0:1, 116) = [10.0000000,0.0156495]
  cd(0:1, 117) = [10.6060606,0.0171908]
  cd(0:1, 118) = [11.2121212,0.0191223]
  cd(0:1, 119) = [11.8181818,0.0216026]
  cd(0:1, 120) = [12.4242424,0.0379529]
  cd(0:1, 121) = [13.0303030,0.0595675]
  cd(0:1, 122) = [13.6363636,0.0693993]
  cd(0:1, 123) = [14.2424242,0.0789367]
  cd(0:1, 124) = [14.8484848,0.0881135]
  cd(0:1, 125) = [15.4545455,0.0973441]
  cd(0:1, 126) = [16.0606061,0.1066150]
  cd(0:1, 127) = [16.6666667,0.1160869]
  cd(0:1, 128) = [17.2727273,0.1258590]
  cd(0:1, 129) = [17.8787879,0.1359978]
  cd(0:1, 130) = [18.4848485,0.1478973]
  cd(0:1, 131) = [19.0909091,0.1602101]
  cd(0:1, 132) = [19.6969697,0.1723704]
  cd(0:1, 133) = [20.3030303,0.1842269]
  cd(0:1, 134) = [20.9090909,0.1957795]
  cd(0:1, 135) = [21.5151515,0.2073321]
  cd(0:1, 136) = [22.1212121,0.2187737]
  cd(0:1, 137) = [22.7272727,0.2297713]
  cd(0:1, 138) = [23.3333333,0.2407688]
  cd(0:1, 139) = [23.9393939,0.2517664]
  cd(0:1, 140) = [24.5454545,0.2634056]
  cd(0:1, 141) = [25.1515152,0.2751160]
  cd(0:1, 142) = [25.7575758,0.2868265]
  cd(0:1, 143) = [26.3636364,0.2989308]
  cd(0:1, 144) = [26.9696970,0.3112976]
  cd(0:1, 145) = [27.5757576,0.3236644]
  cd(0:1, 146) = [28.1818182,0.3362096]
  cd(0:1, 147) = [28.7878788,0.3491712]
  cd(0:1, 148) = [29.3939394,0.3621327]
  cd(0:1, 149) = [30.0000000,0.3750943]
  cd(0:1, 150) = [33.0000000,0.4426585]
  cd(0:1, 151) = [36.0000000,0.5130705]
  cd(0:1, 152) = [39.0000000,0.5861817]
  cd(0:1, 153) = [42.0000000,0.6609755]
  cd(0:1, 154) = [45.0000000,0.7361891]
  cd(0:1, 155) = [48.0000000,0.8113577]
  cd(0:1, 156) = [51.0000000,0.8849856]
  cd(0:1, 157) = [54.0000000,0.9555323]
  cd(0:1, 158) = [57.0000000,1.0260785]
  cd(0:1, 159) = [60.0000000,1.0966244]
  cd(0:1, 160) = [63.0000000,1.1539641]
  cd(0:1, 161) = [66.0000000,1.2113036]
  cd(0:1, 162) = [69.0000000,1.2686431]
  cd(0:1, 163) = [72.0000000,1.3125279]
  cd(0:1, 164) = [75.0000000,1.3496856]
  cd(0:1, 165) = [78.0000000,1.3868438]
  cd(0:1, 166) = [81.0000000,1.4157601]
  cd(0:1, 167) = [84.0000000,1.4281934]
  cd(0:1, 168) = [87.0000000,1.4406267]
  cd(0:1, 169) = [90.0000000,1.4530599]
  cd(0:1, 170) = [93.0000000,1.4406267]
  cd(0:1, 171) = [96.0000000,1.4281934]
  cd(0:1, 172) = [99.0000000,1.4157600]
  cd(0:1, 173) = [102.0000000,1.3868436]
  cd(0:1, 174) = [105.0000000,1.3496859]
  cd(0:1, 175) = [108.0000000,1.3125277]
  cd(0:1, 176) = [111.0000000,1.2686426]
  cd(0:1, 177) = [114.0000000,1.2113037]
  cd(0:1, 178) = [117.0000000,1.1539644]
  cd(0:1, 179) = [120.0000000,1.0966249]
  cd(0:1, 180) = [123.0000000,1.0260790]
  cd(0:1, 181) = [126.0000000,0.9555328]
  cd(0:1, 182) = [129.0000000,0.8849861]
  cd(0:1, 183) = [132.0000000,0.8113428]
  cd(0:1, 184) = [135.0000000,0.7361513]
  cd(0:1, 185) = [138.0000000,0.6609606]
  cd(0:1, 186) = [141.0000000,0.5872595]
  cd(0:1, 187) = [144.0000000,0.5165375]
  cd(0:1, 188) = [147.0000000,0.4458160]
  cd(0:1, 189) = [150.0000000,0.3750947]
  cd(0:1, 190) = [153.0000000,0.3212886]
  cd(0:1, 191) = [156.0000000,0.2674827]
  cd(0:1, 192) = [159.0000000,0.2136772]
  cd(0:1, 193) = [162.0000000,0.1761678]
  cd(0:1, 194) = [165.0000000,0.1468067]
  cd(0:1, 195) = [168.0000000,0.1174454]
  cd(0:1, 196) = [171.0000000,0.0880841]
  cd(0:1, 197) = [174.0000000,0.0587229]
  cd(0:1, 198) = [177.0000000,0.0483558]
  cd(0:1, 199) = [180.0000000,0.0474972]

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
!     PRINT*,'alpha < tabulated range in calc_cd15_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd15 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd15_ge28.pro'
  ENDIF

!print,y,cd15

!_________________________________________________________________________
  
END FUNCTION calc_cd15_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd16_ge28( y ) RESULT( cd16 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd16
  INTEGER :: i

  cd16 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.0467684]
  cd(0:1, 1  ) = [-177.0000000,0.0467684]
  cd(0:1, 2  ) = [-174.0000000,0.0561221]
  cd(0:1, 3  ) = [-171.0000000,0.0841833]
  cd(0:1, 4  ) = [-168.0000000,0.1122474]
  cd(0:1, 5  ) = [-165.0000000,0.1403131]
  cd(0:1, 6  ) = [-162.0000000,0.1683796]
  cd(0:1, 7  ) = [-159.0000000,0.2054880]
  cd(0:1, 8  ) = [-156.0000000,0.2606798]
  cd(0:1, 9  ) = [-153.0000000,0.3158715]
  cd(0:1, 10 ) = [-150.0000000,0.3710634]
  cd(0:1, 11 ) = [-147.0000000,0.4419243]
  cd(0:1, 12 ) = [-144.0000000,0.5127854]
  cd(0:1, 13 ) = [-141.0000000,0.5836471]
  cd(0:1, 14 ) = [-138.0000000,0.6575095]
  cd(0:1, 15 ) = [-135.0000000,0.7328723]
  cd(0:1, 16 ) = [-132.0000000,0.8082359]
  cd(0:1, 17 ) = [-129.0000000,0.8820604]
  cd(0:1, 18 ) = [-126.0000000,0.9528067]
  cd(0:1, 19 ) = [-123.0000000,1.0235524]
  cd(0:1, 20 ) = [-120.0000000,1.0942979]
  cd(0:1, 21 ) = [-117.0000000,1.1518579]
  cd(0:1, 22 ) = [-114.0000000,1.2094175]
  cd(0:1, 23 ) = [-111.0000000,1.2669768]
  cd(0:1, 24 ) = [-108.0000000,1.3110922]
  cd(0:1, 25 ) = [-105.0000000,1.3484855]
  cd(0:1, 26 ) = [-102.0000000,1.3858784]
  cd(0:1, 27 ) = [-99.0000000,1.4150324]
  cd(0:1, 28 ) = [-96.0000000,1.4277083]
  cd(0:1, 29 ) = [-93.0000000,1.4403841]
  cd(0:1, 30 ) = [-90.0000000,1.4530599]
  cd(0:1, 31 ) = [-87.0000000,1.4403841]
  cd(0:1, 32 ) = [-84.0000000,1.4277083]
  cd(0:1, 33 ) = [-81.0000000,1.4150325]
  cd(0:1, 34 ) = [-78.0000000,1.3858785]
  cd(0:1, 35 ) = [-75.0000000,1.3484852]
  cd(0:1, 36 ) = [-72.0000000,1.3110923]
  cd(0:1, 37 ) = [-69.0000000,1.2669773]
  cd(0:1, 38 ) = [-66.0000000,1.2094174]
  cd(0:1, 39 ) = [-63.0000000,1.1518575]
  cd(0:1, 40 ) = [-60.0000000,1.0942974]
  cd(0:1, 41 ) = [-57.0000000,1.0235520]
  cd(0:1, 42 ) = [-54.0000000,0.9528062]
  cd(0:1, 43 ) = [-51.0000000,0.8820600]
  cd(0:1, 44 ) = [-48.0000000,0.8082457]
  cd(0:1, 45 ) = [-45.0000000,0.7328976]
  cd(0:1, 46 ) = [-42.0000000,0.6575194]
  cd(0:1, 47 ) = [-39.0000000,0.5825647]
  cd(0:1, 48 ) = [-36.0000000,0.5093047]
  cd(0:1, 49 ) = [-33.0000000,0.4387551]
  cd(0:1, 50 ) = [-30.0000000,0.3710629]
  cd(0:1, 51 ) = [-29.3939394,0.3580775]
  cd(0:1, 52 ) = [-28.7878788,0.3450921]
  cd(0:1, 53 ) = [-28.1818182,0.3321067]
  cd(0:1, 54 ) = [-27.5757576,0.3195387]
  cd(0:1, 55 ) = [-26.9696970,0.3071495]
  cd(0:1, 56 ) = [-26.3636364,0.2947604]
  cd(0:1, 57 ) = [-25.7575758,0.2826344]
  cd(0:1, 58 ) = [-25.1515152,0.2709031]
  cd(0:1, 59 ) = [-24.5454545,0.2591718]
  cd(0:1, 60 ) = [-23.9393939,0.2475120]
  cd(0:1, 61 ) = [-23.3333333,0.2364952]
  cd(0:1, 62 ) = [-22.7272727,0.2254784]
  cd(0:1, 63 ) = [-22.1212121,0.2144616]
  cd(0:1, 64 ) = [-21.5151515,0.2085481]
  cd(0:1, 65 ) = [-20.9090909,0.2039108]
  cd(0:1, 66 ) = [-20.3030303,0.1992735]
  cd(0:1, 67 ) = [-19.6969697,0.1946375]
  cd(0:1, 68 ) = [-19.0909091,0.1900027]
  cd(0:1, 69 ) = [-18.4848485,0.1854805]
  cd(0:1, 70 ) = [-17.8787879,0.1809505]
  cd(0:1, 71 ) = [-17.2727273,0.1761460]
  cd(0:1, 72 ) = [-16.6666667,0.1714739]
  cd(0:1, 73 ) = [-16.0606061,0.1668489]
  cd(0:1, 74 ) = [-15.4545455,0.1601475]
  cd(0:1, 75 ) = [-14.8484848,0.1521640]
  cd(0:1, 76 ) = [-14.2424242,0.1410268]
  cd(0:1, 77 ) = [-13.6363636,0.1294496]
  cd(0:1, 78 ) = [-13.0303030,0.1175780]
  cd(0:1, 79 ) = [-12.4242424,0.1061280]
  cd(0:1, 80 ) = [-11.8181818,0.0953368]
  cd(0:1, 81 ) = [-11.2121212,0.0860321]
  cd(0:1, 82 ) = [-10.6060606,0.0776107]
  cd(0:1, 83 ) = [-10.0000000,0.0696649]
  cd(0:1, 84 ) = [-9.3939394,0.0577167]
  cd(0:1, 85 ) = [-8.7878788,0.0445160]
  cd(0:1, 86 ) = [-8.1818182,0.0291171]
  cd(0:1, 87 ) = [-7.5757576,0.0214382]
  cd(0:1, 88 ) = [-6.9696970,0.0172874]
  cd(0:1, 89 ) = [-6.3636364,0.0157551]
  cd(0:1, 90 ) = [-5.7575758,0.0143502]
  cd(0:1, 91 ) = [-5.1515152,0.0131420]
  cd(0:1, 92 ) = [-4.5454545,0.0122637]
  cd(0:1, 93 ) = [-3.9393939,0.0115266]
  cd(0:1, 94 ) = [-3.3333333,0.0111182]
  cd(0:1, 95 ) = [-2.7272727,0.0108213]
  cd(0:1, 96 ) = [-2.1212121,0.0106140]
  cd(0:1, 97 ) = [-1.5151515,0.0104480]
  cd(0:1, 98 ) = [-0.9090909,0.0103347]
  cd(0:1, 99 ) = [-0.3030303,0.0104136]
  cd(0:1, 100) = [0.3030303,0.0105052]
  cd(0:1, 101) = [0.9090909,0.0106029]
  cd(0:1, 102) = [1.5151515,0.0106086]
  cd(0:1, 103) = [2.1212121,0.0106364]
  cd(0:1, 104) = [2.7272727,0.0108223]
  cd(0:1, 105) = [3.3333333,0.0109872]
  cd(0:1, 106) = [3.9393939,0.0111314]
  cd(0:1, 107) = [4.5454545,0.0113155]
  cd(0:1, 108) = [5.1515152,0.0115208]
  cd(0:1, 109) = [5.7575758,0.0117796]
  cd(0:1, 110) = [6.3636364,0.0120808]
  cd(0:1, 111) = [6.9696970,0.0124089]
  cd(0:1, 112) = [7.5757576,0.0127841]
  cd(0:1, 113) = [8.1818182,0.0131971]
  cd(0:1, 114) = [8.7878788,0.0136880]
  cd(0:1, 115) = [9.3939394,0.0143635]
  cd(0:1, 116) = [10.0000000,0.0151115]
  cd(0:1, 117) = [10.6060606,0.0162053]
  cd(0:1, 118) = [11.2121212,0.0175946]
  cd(0:1, 119) = [11.8181818,0.0195274]
  cd(0:1, 120) = [12.4242424,0.0366838]
  cd(0:1, 121) = [13.0303030,0.0596341]
  cd(0:1, 122) = [13.6363636,0.0690811]
  cd(0:1, 123) = [14.2424242,0.0781570]
  cd(0:1, 124) = [14.8484848,0.0867025]
  cd(0:1, 125) = [15.4545455,0.0951813]
  cd(0:1, 126) = [16.0606061,0.1036722]
  cd(0:1, 127) = [16.6666667,0.1124718]
  cd(0:1, 128) = [17.2727273,0.1216127]
  cd(0:1, 129) = [17.8787879,0.1311705]
  cd(0:1, 130) = [18.4848485,0.1427178]
  cd(0:1, 131) = [19.0909091,0.1548279]
  cd(0:1, 132) = [19.6969697,0.1673101]
  cd(0:1, 133) = [20.3030303,0.1794159]
  cd(0:1, 134) = [20.9090909,0.1911453]
  cd(0:1, 135) = [21.5151515,0.2028747]
  cd(0:1, 136) = [22.1212121,0.2144616]
  cd(0:1, 137) = [22.7272727,0.2254784]
  cd(0:1, 138) = [23.3333333,0.2364952]
  cd(0:1, 139) = [23.9393939,0.2475120]
  cd(0:1, 140) = [24.5454545,0.2591718]
  cd(0:1, 141) = [25.1515152,0.2709031]
  cd(0:1, 142) = [25.7575758,0.2826344]
  cd(0:1, 143) = [26.3636364,0.2947604]
  cd(0:1, 144) = [26.9696970,0.3071495]
  cd(0:1, 145) = [27.5757576,0.3195387]
  cd(0:1, 146) = [28.1818182,0.3321067]
  cd(0:1, 147) = [28.7878788,0.3450921]
  cd(0:1, 148) = [29.3939394,0.3580775]
  cd(0:1, 149) = [30.0000000,0.3710629]
  cd(0:1, 150) = [33.0000000,0.4387551]
  cd(0:1, 151) = [36.0000000,0.5093047]
  cd(0:1, 152) = [39.0000000,0.5825647]
  cd(0:1, 153) = [42.0000000,0.6575194]
  cd(0:1, 154) = [45.0000000,0.7328976]
  cd(0:1, 155) = [48.0000000,0.8082457]
  cd(0:1, 156) = [51.0000000,0.8820600]
  cd(0:1, 157) = [54.0000000,0.9528062]
  cd(0:1, 158) = [57.0000000,1.0235520]
  cd(0:1, 159) = [60.0000000,1.0942974]
  cd(0:1, 160) = [63.0000000,1.1518575]
  cd(0:1, 161) = [66.0000000,1.2094174]
  cd(0:1, 162) = [69.0000000,1.2669773]
  cd(0:1, 163) = [72.0000000,1.3110923]
  cd(0:1, 164) = [75.0000000,1.3484852]
  cd(0:1, 165) = [78.0000000,1.3858785]
  cd(0:1, 166) = [81.0000000,1.4150325]
  cd(0:1, 167) = [84.0000000,1.4277083]
  cd(0:1, 168) = [87.0000000,1.4403841]
  cd(0:1, 169) = [90.0000000,1.4530599]
  cd(0:1, 170) = [93.0000000,1.4403841]
  cd(0:1, 171) = [96.0000000,1.4277083]
  cd(0:1, 172) = [99.0000000,1.4150324]
  cd(0:1, 173) = [102.0000000,1.3858784]
  cd(0:1, 174) = [105.0000000,1.3484855]
  cd(0:1, 175) = [108.0000000,1.3110922]
  cd(0:1, 176) = [111.0000000,1.2669768]
  cd(0:1, 177) = [114.0000000,1.2094175]
  cd(0:1, 178) = [117.0000000,1.1518579]
  cd(0:1, 179) = [120.0000000,1.0942979]
  cd(0:1, 180) = [123.0000000,1.0235524]
  cd(0:1, 181) = [126.0000000,0.9528067]
  cd(0:1, 182) = [129.0000000,0.8820604]
  cd(0:1, 183) = [132.0000000,0.8082359]
  cd(0:1, 184) = [135.0000000,0.7328723]
  cd(0:1, 185) = [138.0000000,0.6575095]
  cd(0:1, 186) = [141.0000000,0.5836471]
  cd(0:1, 187) = [144.0000000,0.5127854]
  cd(0:1, 188) = [147.0000000,0.4419243]
  cd(0:1, 189) = [150.0000000,0.3710634]
  cd(0:1, 190) = [153.0000000,0.3176214]
  cd(0:1, 191) = [156.0000000,0.2641797]
  cd(0:1, 192) = [159.0000000,0.2107385]
  cd(0:1, 193) = [162.0000000,0.1736323]
  cd(0:1, 194) = [165.0000000,0.1446937]
  cd(0:1, 195) = [168.0000000,0.1157549]
  cd(0:1, 196) = [171.0000000,0.0868162]
  cd(0:1, 197) = [174.0000000,0.0578777]
  cd(0:1, 198) = [177.0000000,0.0476423]
  cd(0:1, 199) = [180.0000000,0.0467684]

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
!     PRINT*,'alpha < tabulated range in calc_cd16_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd16 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd16_ge28.pro'
  ENDIF

!print,y,cd16

!_________________________________________________________________________
  
END FUNCTION calc_cd16_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd17_ge28( y ) RESULT( cd17 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd17
  INTEGER :: i

  cd17 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.00000000,0.04749453]
  cd(0:1, 1  ) = [-177.00000000,0.04749453]
  cd(0:1, 2  ) = [-174.00000000,0.05699338]
  cd(0:1, 3  ) = [-171.00000000,0.08549025]
  cd(0:1, 4  ) = [-168.00000000,0.11398653]
  cd(0:1, 5  ) = [-165.00000000,0.14248252]
  cd(0:1, 6  ) = [-162.00000000,0.17097934]
  cd(0:1, 7  ) = [-159.00000000,0.20832586]
  cd(0:1, 8  ) = [-156.00000000,0.26337150]
  cd(0:1, 9  ) = [-153.00000000,0.31841716]
  cd(0:1, 10 ) = [-150.00000000,0.37346287]
  cd(0:1, 11 ) = [-147.00000000,0.44424066]
  cd(0:1, 12 ) = [-144.00000000,0.51501871]
  cd(0:1, 13 ) = [-141.00000000,0.58579727]
  cd(0:1, 14 ) = [-138.00000000,0.65956370]
  cd(0:1, 15 ) = [-135.00000000,0.73482404]
  cd(0:1, 16 ) = [-132.00000000,0.81008521]
  cd(0:1, 17 ) = [-129.00000000,0.88380187]
  cd(0:1, 18 ) = [-126.00000000,0.95442952]
  cd(0:1, 19 ) = [-123.00000000,1.02505663]
  cd(0:1, 20 ) = [-120.00000000,1.09568347]
  cd(0:1, 21 ) = [-117.00000000,1.15311206]
  cd(0:1, 22 ) = [-114.00000000,1.21054040]
  cd(0:1, 23 ) = [-111.00000000,1.26796830]
  cd(0:1, 24 ) = [-108.00000000,1.31194661]
  cd(0:1, 25 ) = [-105.00000000,1.34920001]
  cd(0:1, 26 ) = [-102.00000000,1.38645300]
  cd(0:1, 27 ) = [-99.00000000,1.41546561]
  cd(0:1, 28 ) = [-96.00000000,1.42799713]
  cd(0:1, 29 ) = [-93.00000000,1.44052855]
  cd(0:1, 30 ) = [-90.00000000,1.45305992]
  cd(0:1, 31 ) = [-87.00000000,1.44052856]
  cd(0:1, 32 ) = [-84.00000000,1.42799709]
  cd(0:1, 33 ) = [-81.00000000,1.41546572]
  cd(0:1, 34 ) = [-78.00000000,1.38645318]
  cd(0:1, 35 ) = [-75.00000000,1.34919977]
  cd(0:1, 36 ) = [-72.00000000,1.31194679]
  cd(0:1, 37 ) = [-69.00000000,1.26796880]
  cd(0:1, 38 ) = [-66.00000000,1.21054024]
  cd(0:1, 39 ) = [-63.00000000,1.15311168]
  cd(0:1, 40 ) = [-60.00000000,1.09568301]
  cd(0:1, 41 ) = [-57.00000000,1.02505616]
  cd(0:1, 42 ) = [-54.00000000,0.95442905]
  cd(0:1, 43 ) = [-51.00000000,0.88380140]
  cd(0:1, 44 ) = [-48.00000000,0.81009799]
  cd(0:1, 45 ) = [-45.00000000,0.73485676]
  cd(0:1, 46 ) = [-42.00000000,0.65957648]
  cd(0:1, 47 ) = [-39.00000000,0.58471760]
  cd(0:1, 48 ) = [-36.00000000,0.51154616]
  cd(0:1, 49 ) = [-33.00000000,0.44107843]
  cd(0:1, 50 ) = [-30.00000000,0.37346243]
  cd(0:1, 51 ) = [-29.39393939,0.36049122]
  cd(0:1, 52 ) = [-28.78787879,0.34752001]
  cd(0:1, 53 ) = [-28.18181818,0.33454880]
  cd(0:1, 54 ) = [-27.57575758,0.32199434]
  cd(0:1, 55 ) = [-26.96969697,0.30961850]
  cd(0:1, 56 ) = [-26.36363636,0.29724266]
  cd(0:1, 57 ) = [-25.75757576,0.28512961]
  cd(0:1, 58 ) = [-25.15151515,0.27341071]
  cd(0:1, 59 ) = [-24.54545455,0.26169180]
  cd(0:1, 60 ) = [-23.93939394,0.25004367]
  cd(0:1, 61 ) = [-23.33333333,0.23903253]
  cd(0:1, 62 ) = [-22.72727273,0.22802138]
  cd(0:1, 63 ) = [-22.12121212,0.21701023]
  cd(0:1, 64 ) = [-21.51515152,0.21071943]
  cd(0:1, 65 ) = [-20.90909091,0.20560902]
  cd(0:1, 66 ) = [-20.30303030,0.20049861]
  cd(0:1, 67 ) = [-19.69696970,0.19538822]
  cd(0:1, 68 ) = [-19.09090909,0.19027786]
  cd(0:1, 69 ) = [-18.48484848,0.18486274]
  cd(0:1, 70 ) = [-17.87878788,0.17962363]
  cd(0:1, 71 ) = [-17.27272727,0.17530532]
  cd(0:1, 72 ) = [-16.66666667,0.17047965]
  cd(0:1, 73 ) = [-16.06060606,0.16543551]
  cd(0:1, 74 ) = [-15.45454545,0.15841402]
  cd(0:1, 75 ) = [-14.84848485,0.15017601]
  cd(0:1, 76 ) = [-14.24242424,0.13899191]
  cd(0:1, 77 ) = [-13.63636364,0.12739180]
  cd(0:1, 78 ) = [-13.03030303,0.11551031]
  cd(0:1, 79 ) = [-12.42424242,0.10376371]
  cd(0:1, 80 ) = [-11.81818182,0.09261674]
  cd(0:1, 81 ) = [-11.21212121,0.08311405]
  cd(0:1, 82 ) = [-10.60606061,0.07462271]
  cd(0:1, 83 ) = [-10.00000000,0.06665118]
  cd(0:1, 84 ) = [-9.39393939,0.05512362]
  cd(0:1, 85 ) = [-8.78787879,0.04226127]
  cd(0:1, 86 ) = [-8.18181818,0.02653475]
  cd(0:1, 87 ) = [-7.57575758,0.01937440]
  cd(0:1, 88 ) = [-6.96969697,0.01569037]
  cd(0:1, 89 ) = [-6.36363636,0.01443305]
  cd(0:1, 90 ) = [-5.75757576,0.01327285]
  cd(0:1, 91 ) = [-5.15151515,0.01227476]
  cd(0:1, 92 ) = [-4.54545455,0.01159213]
  cd(0:1, 93 ) = [-3.93939394,0.01104442]
  cd(0:1, 94 ) = [-3.33333333,0.01067564]
  cd(0:1, 95 ) = [-2.72727273,0.01044785]
  cd(0:1, 96 ) = [-2.12121212,0.01036043]
  cd(0:1, 97 ) = [-1.51515152,0.01024520]
  cd(0:1, 98 ) = [-0.90909091,0.01016192]
  cd(0:1, 99 ) = [-0.30303030,0.01024266]
  cd(0:1, 100) = [0.30303030,0.01033031]
  cd(0:1, 101) = [0.90909091,0.01042121]
  cd(0:1, 102) = [1.51515152,0.01042361]
  cd(0:1, 103) = [2.12121212,0.01046451]
  cd(0:1, 104) = [2.72727273,0.01064953]
  cd(0:1, 105) = [3.33333333,0.01081268]
  cd(0:1, 106) = [3.93939394,0.01090431]
  cd(0:1, 107) = [4.54545455,0.01102873]
  cd(0:1, 108) = [5.15151515,0.01118597]
  cd(0:1, 109) = [5.75757576,0.01141115]
  cd(0:1, 110) = [6.36363636,0.01167204]
  cd(0:1, 111) = [6.96969697,0.01195071]
  cd(0:1, 112) = [7.57575758,0.01231110]
  cd(0:1, 113) = [8.18181818,0.01284536]
  cd(0:1, 114) = [8.78787879,0.01370246]
  cd(0:1, 115) = [9.39393939,0.01505019]
  cd(0:1, 116) = [10.00000000,0.01676195]
  cd(0:1, 117) = [10.60606061,0.01865887]
  cd(0:1, 118) = [11.21212121,0.02064426]
  cd(0:1, 119) = [11.81818182,0.02279490]
  cd(0:1, 120) = [12.42424242,0.03919328]
  cd(0:1, 121) = [13.03030303,0.06111166]
  cd(0:1, 122) = [13.63636364,0.07051261]
  cd(0:1, 123) = [14.24242424,0.07967314]
  cd(0:1, 124) = [14.84848485,0.08837364]
  cd(0:1, 125) = [15.45454545,0.09688453]
  cd(0:1, 126) = [16.06060606,0.10535329]
  cd(0:1, 127) = [16.66666667,0.11401165]
  cd(0:1, 128) = [17.27272727,0.12311170]
  cd(0:1, 129) = [17.87878788,0.13275158]
  cd(0:1, 130) = [18.48484848,0.14435773]
  cd(0:1, 131) = [19.09090909,0.15650462]
  cd(0:1, 132) = [19.69696970,0.16893068]
  cd(0:1, 133) = [20.30303030,0.18113498]
  cd(0:1, 134) = [20.90909091,0.19311750]
  cd(0:1, 135) = [21.51515152,0.20510002]
  cd(0:1, 136) = [22.12121212,0.21689562]
  cd(0:1, 137) = [22.72727273,0.22794374]
  cd(0:1, 138) = [23.33333333,0.23899186]
  cd(0:1, 139) = [23.93939394,0.25003998]
  cd(0:1, 140) = [24.54545455,0.26169180]
  cd(0:1, 141) = [25.15151515,0.27341071]
  cd(0:1, 142) = [25.75757576,0.28512961]
  cd(0:1, 143) = [26.36363636,0.29724266]
  cd(0:1, 144) = [26.96969697,0.30961850]
  cd(0:1, 145) = [27.57575758,0.32199434]
  cd(0:1, 146) = [28.18181818,0.33454880]
  cd(0:1, 147) = [28.78787879,0.34752001]
  cd(0:1, 148) = [29.39393939,0.36049122]
  cd(0:1, 149) = [30.00000000,0.37346243]
  cd(0:1, 150) = [33.00000000,0.44107843]
  cd(0:1, 151) = [36.00000000,0.51154616]
  cd(0:1, 152) = [39.00000000,0.58471760]
  cd(0:1, 153) = [42.00000000,0.65957648]
  cd(0:1, 154) = [45.00000000,0.73485676]
  cd(0:1, 155) = [48.00000000,0.81009799]
  cd(0:1, 156) = [51.00000000,0.88380140]
  cd(0:1, 157) = [54.00000000,0.95442905]
  cd(0:1, 158) = [57.00000000,1.02505616]
  cd(0:1, 159) = [60.00000000,1.09568301]
  cd(0:1, 160) = [63.00000000,1.15311168]
  cd(0:1, 161) = [66.00000000,1.21054024]
  cd(0:1, 162) = [69.00000000,1.26796880]
  cd(0:1, 163) = [72.00000000,1.31194679]
  cd(0:1, 164) = [75.00000000,1.34919977]
  cd(0:1, 165) = [78.00000000,1.38645318]
  cd(0:1, 166) = [81.00000000,1.41546572]
  cd(0:1, 167) = [84.00000000,1.42799709]
  cd(0:1, 168) = [87.00000000,1.44052856]
  cd(0:1, 169) = [90.00000000,1.45305992]
  cd(0:1, 170) = [93.00000000,1.44052855]
  cd(0:1, 171) = [96.00000000,1.42799713]
  cd(0:1, 172) = [99.00000000,1.41546561]
  cd(0:1, 173) = [102.00000000,1.38645300]
  cd(0:1, 174) = [105.00000000,1.34920001]
  cd(0:1, 175) = [108.00000000,1.31194661]
  cd(0:1, 176) = [111.00000000,1.26796830]
  cd(0:1, 177) = [114.00000000,1.21054040]
  cd(0:1, 178) = [117.00000000,1.15311206]
  cd(0:1, 179) = [120.00000000,1.09568347]
  cd(0:1, 180) = [123.00000000,1.02505663]
  cd(0:1, 181) = [126.00000000,0.95442952]
  cd(0:1, 182) = [129.00000000,0.88380187]
  cd(0:1, 183) = [132.00000000,0.81008521]
  cd(0:1, 184) = [135.00000000,0.73482404]
  cd(0:1, 185) = [138.00000000,0.65956370]
  cd(0:1, 186) = [141.00000000,0.58579727]
  cd(0:1, 187) = [144.00000000,0.51501871]
  cd(0:1, 188) = [147.00000000,0.44424066]
  cd(0:1, 189) = [150.00000000,0.37346287]
  cd(0:1, 190) = [153.00000000,0.32014590]
  cd(0:1, 191) = [156.00000000,0.26682917]
  cd(0:1, 192) = [159.00000000,0.21351281]
  cd(0:1, 193) = [162.00000000,0.17616665]
  cd(0:1, 194) = [165.00000000,0.14680573]
  cd(0:1, 195) = [168.00000000,0.11744455]
  cd(0:1, 196) = [171.00000000,0.08808345]
  cd(0:1, 197) = [174.00000000,0.05872248]
  cd(0:1, 198) = [177.00000000,0.04835523]
  cd(0:1, 199) = [180.00000000,0.04749453]

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
!     PRINT*,'alpha < tabulated range in calc_cd17_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd17 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd17_ge28.pro'
  ENDIF

!print,y,cd17

!_________________________________________________________________________
  
END FUNCTION calc_cd17_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd18_ge28( y ) RESULT( cd18 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd18
  INTEGER :: i

  cd18 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.0502377]
  cd(0:1, 1  ) = [-177.0000000,0.0502377]
  cd(0:1, 2  ) = [-174.0000000,0.0602852]
  cd(0:1, 3  ) = [-171.0000000,0.0904280]
  cd(0:1, 4  ) = [-168.0000000,0.1205703]
  cd(0:1, 5  ) = [-165.0000000,0.1507123]
  cd(0:1, 6  ) = [-162.0000000,0.1808549]
  cd(0:1, 7  ) = [-159.0000000,0.2191277]
  cd(0:1, 8  ) = [-156.0000000,0.2736608]
  cd(0:1, 9  ) = [-153.0000000,0.3281940]
  cd(0:1, 10 ) = [-150.0000000,0.3827272]
  cd(0:1, 11 ) = [-147.0000000,0.4531842]
  cd(0:1, 12 ) = [-144.0000000,0.5236414]
  cd(0:1, 13 ) = [-141.0000000,0.5940990]
  cd(0:1, 14 ) = [-138.0000000,0.6674948]
  cd(0:1, 15 ) = [-135.0000000,0.7423597]
  cd(0:1, 16 ) = [-132.0000000,0.8172253]
  cd(0:1, 17 ) = [-129.0000000,0.8905255]
  cd(0:1, 18 ) = [-126.0000000,0.9606951]
  cd(0:1, 19 ) = [-123.0000000,1.0308642]
  cd(0:1, 20 ) = [-120.0000000,1.1010330]
  cd(0:1, 21 ) = [-117.0000000,1.1579544]
  cd(0:1, 22 ) = [-114.0000000,1.2148757]
  cd(0:1, 23 ) = [-111.0000000,1.2717964]
  cd(0:1, 24 ) = [-108.0000000,1.3152456]
  cd(0:1, 25 ) = [-105.0000000,1.3519588]
  cd(0:1, 26 ) = [-102.0000000,1.3886717]
  cd(0:1, 27 ) = [-99.0000000,1.4171383]
  cd(0:1, 28 ) = [-96.0000000,1.4291123]
  cd(0:1, 29 ) = [-93.0000000,1.4410861]
  cd(0:1, 30 ) = [-90.0000000,1.4530599]
  cd(0:1, 31 ) = [-87.0000000,1.4410861]
  cd(0:1, 32 ) = [-84.0000000,1.4291122]
  cd(0:1, 33 ) = [-81.0000000,1.4171384]
  cd(0:1, 34 ) = [-78.0000000,1.3886718]
  cd(0:1, 35 ) = [-75.0000000,1.3519586]
  cd(0:1, 36 ) = [-72.0000000,1.3152458]
  cd(0:1, 37 ) = [-69.0000000,1.2717969]
  cd(0:1, 38 ) = [-66.0000000,1.2148755]
  cd(0:1, 39 ) = [-63.0000000,1.1579541]
  cd(0:1, 40 ) = [-60.0000000,1.1010325]
  cd(0:1, 41 ) = [-57.0000000,1.0308637]
  cd(0:1, 42 ) = [-54.0000000,0.9606947]
  cd(0:1, 43 ) = [-51.0000000,0.8905251]
  cd(0:1, 44 ) = [-48.0000000,0.8172494]
  cd(0:1, 45 ) = [-45.0000000,0.7424208]
  cd(0:1, 46 ) = [-42.0000000,0.6675190]
  cd(0:1, 47 ) = [-39.0000000,0.5930299]
  cd(0:1, 48 ) = [-36.0000000,0.5202004]
  cd(0:1, 49 ) = [-33.0000000,0.4500486]
  cd(0:1, 50 ) = [-30.0000000,0.3827268]
  cd(0:1, 51 ) = [-29.3939394,0.3698104]
  cd(0:1, 52 ) = [-28.7878788,0.3568940]
  cd(0:1, 53 ) = [-28.1818182,0.3439776]
  cd(0:1, 54 ) = [-27.5757576,0.3314756]
  cd(0:1, 55 ) = [-26.9696970,0.3191511]
  cd(0:1, 56 ) = [-26.3636364,0.3068267]
  cd(0:1, 57 ) = [-25.7575758,0.2947636]
  cd(0:1, 58 ) = [-25.1515152,0.2830925]
  cd(0:1, 59 ) = [-24.5454545,0.2714214]
  cd(0:1, 60 ) = [-23.9393939,0.2598186]
  cd(0:1, 61 ) = [-23.3333333,0.2488305]
  cd(0:1, 62 ) = [-22.7272727,0.2378423]
  cd(0:1, 63 ) = [-22.1212121,0.2268542]
  cd(0:1, 64 ) = [-21.5151515,0.2191273]
  cd(0:1, 65 ) = [-20.9090909,0.2122158]
  cd(0:1, 66 ) = [-20.3030303,0.2053044]
  cd(0:1, 67 ) = [-19.6969697,0.1983930]
  cd(0:1, 68 ) = [-19.0909091,0.1914818]
  cd(0:1, 69 ) = [-18.4848485,0.1844881]
  cd(0:1, 70 ) = [-17.8787879,0.1781251]
  cd(0:1, 71 ) = [-17.2727273,0.1726673]
  cd(0:1, 72 ) = [-16.6666667,0.1664291]
  cd(0:1, 73 ) = [-16.0606061,0.1597852]
  cd(0:1, 74 ) = [-15.4545455,0.1515747]
  cd(0:1, 75 ) = [-14.8484848,0.1424153]
  cd(0:1, 76 ) = [-14.2424242,0.1311131]
  cd(0:1, 77 ) = [-13.6363636,0.1194915]
  cd(0:1, 78 ) = [-13.0303030,0.1076437]
  cd(0:1, 79 ) = [-12.4242424,0.0949347]
  cd(0:1, 80 ) = [-11.8181818,0.0826305]
  cd(0:1, 81 ) = [-11.2121212,0.0724324]
  cd(0:1, 82 ) = [-10.6060606,0.0636793]
  cd(0:1, 83 ) = [-10.0000000,0.0556031]
  cd(0:1, 84 ) = [-9.3939394,0.0456286]
  cd(0:1, 85 ) = [-8.7878788,0.0348441]
  cd(0:1, 86 ) = [-8.1818182,0.0221903]
  cd(0:1, 87 ) = [-7.5757576,0.0166726]
  cd(0:1, 88 ) = [-6.9696970,0.0138194]
  cd(0:1, 89 ) = [-6.3636364,0.0128208]
  cd(0:1, 90 ) = [-5.7575758,0.0118605]
  cd(0:1, 91 ) = [-5.1515152,0.0111014]
  cd(0:1, 92 ) = [-4.5454545,0.0106136]
  cd(0:1, 93 ) = [-3.9393939,0.0102273]
  cd(0:1, 94 ) = [-3.3333333,0.0099052]
  cd(0:1, 95 ) = [-2.7272727,0.0097109]
  cd(0:1, 96 ) = [-2.1212121,0.0096459]
  cd(0:1, 97 ) = [-1.5151515,0.0095566]
  cd(0:1, 98 ) = [-0.9090909,0.0094998]
  cd(0:1, 99 ) = [-0.3030303,0.0095841]
  cd(0:1, 100) = [0.3030303,0.0096556]
  cd(0:1, 101) = [0.9090909,0.0097198]
  cd(0:1, 102) = [1.5151515,0.0097139]
  cd(0:1, 103) = [2.1212121,0.0098033]
  cd(0:1, 104) = [2.7272727,0.0099849]
  cd(0:1, 105) = [3.3333333,0.0101456]
  cd(0:1, 106) = [3.9393939,0.0102378]
  cd(0:1, 107) = [4.5454545,0.0103669]
  cd(0:1, 108) = [5.1515152,0.0105314]
  cd(0:1, 109) = [5.7575758,0.0107650]
  cd(0:1, 110) = [6.3636364,0.0110273]
  cd(0:1, 111) = [6.9696970,0.0112742]
  cd(0:1, 112) = [7.5757576,0.0116671]
  cd(0:1, 113) = [8.1818182,0.0124490]
  cd(0:1, 114) = [8.7878788,0.0137772]
  cd(0:1, 115) = [9.3939394,0.0175962]
  cd(0:1, 116) = [10.0000000,0.0228583]
  cd(0:1, 117) = [10.6060606,0.0277464]
  cd(0:1, 118) = [11.2121212,0.0319782]
  cd(0:1, 119) = [11.8181818,0.0349922]
  cd(0:1, 120) = [12.4242424,0.0485210]
  cd(0:1, 121) = [13.0303030,0.0662158]
  cd(0:1, 122) = [13.6363636,0.0757839]
  cd(0:1, 123) = [14.2424242,0.0853103]
  cd(0:1, 124) = [14.8484848,0.0946544]
  cd(0:1, 125) = [15.4545455,0.1033749]
  cd(0:1, 126) = [16.0606061,0.1118586]
  cd(0:1, 127) = [16.6666667,0.1200829]
  cd(0:1, 128) = [17.2727273,0.1291112]
  cd(0:1, 129) = [17.8787879,0.1391223]
  cd(0:1, 130) = [18.4848485,0.1509868]
  cd(0:1, 131) = [19.0909091,0.1632927]
  cd(0:1, 132) = [19.6969697,0.1754743]
  cd(0:1, 133) = [20.3030303,0.1880081]
  cd(0:1, 134) = [20.9090909,0.2008940]
  cd(0:1, 135) = [21.5151515,0.2137800]
  cd(0:1, 136) = [22.1212121,0.2263207]
  cd(0:1, 137) = [22.7272727,0.2374809]
  cd(0:1, 138) = [23.3333333,0.2486411]
  cd(0:1, 139) = [23.9393939,0.2598014]
  cd(0:1, 140) = [24.5454545,0.2714214]
  cd(0:1, 141) = [25.1515152,0.2830925]
  cd(0:1, 142) = [25.7575758,0.2947636]
  cd(0:1, 143) = [26.3636364,0.3068267]
  cd(0:1, 144) = [26.9696970,0.3191511]
  cd(0:1, 145) = [27.5757576,0.3314756]
  cd(0:1, 146) = [28.1818182,0.3439776]
  cd(0:1, 147) = [28.7878788,0.3568940]
  cd(0:1, 148) = [29.3939394,0.3698104]
  cd(0:1, 149) = [30.0000000,0.3827268]
  cd(0:1, 150) = [33.0000000,0.4500486]
  cd(0:1, 151) = [36.0000000,0.5202004]
  cd(0:1, 152) = [39.0000000,0.5930299]
  cd(0:1, 153) = [42.0000000,0.6675190]
  cd(0:1, 154) = [45.0000000,0.7424208]
  cd(0:1, 155) = [48.0000000,0.8172494]
  cd(0:1, 156) = [51.0000000,0.8905251]
  cd(0:1, 157) = [54.0000000,0.9606947]
  cd(0:1, 158) = [57.0000000,1.0308637]
  cd(0:1, 159) = [60.0000000,1.1010325]
  cd(0:1, 160) = [63.0000000,1.1579541]
  cd(0:1, 161) = [66.0000000,1.2148755]
  cd(0:1, 162) = [69.0000000,1.2717969]
  cd(0:1, 163) = [72.0000000,1.3152458]
  cd(0:1, 164) = [75.0000000,1.3519586]
  cd(0:1, 165) = [78.0000000,1.3886718]
  cd(0:1, 166) = [81.0000000,1.4171384]
  cd(0:1, 167) = [84.0000000,1.4291122]
  cd(0:1, 168) = [87.0000000,1.4410861]
  cd(0:1, 169) = [90.0000000,1.4530599]
  cd(0:1, 170) = [93.0000000,1.4410861]
  cd(0:1, 171) = [96.0000000,1.4291123]
  cd(0:1, 172) = [99.0000000,1.4171383]
  cd(0:1, 173) = [102.0000000,1.3886717]
  cd(0:1, 174) = [105.0000000,1.3519588]
  cd(0:1, 175) = [108.0000000,1.3152456]
  cd(0:1, 176) = [111.0000000,1.2717964]
  cd(0:1, 177) = [114.0000000,1.2148757]
  cd(0:1, 178) = [117.0000000,1.1579544]
  cd(0:1, 179) = [120.0000000,1.1010330]
  cd(0:1, 180) = [123.0000000,1.0308642]
  cd(0:1, 181) = [126.0000000,0.9606951]
  cd(0:1, 182) = [129.0000000,0.8905255]
  cd(0:1, 183) = [132.0000000,0.8172253]
  cd(0:1, 184) = [135.0000000,0.7423597]
  cd(0:1, 185) = [138.0000000,0.6674948]
  cd(0:1, 186) = [141.0000000,0.5940990]
  cd(0:1, 187) = [144.0000000,0.5236414]
  cd(0:1, 188) = [147.0000000,0.4531842]
  cd(0:1, 189) = [150.0000000,0.3827272]
  cd(0:1, 190) = [153.0000000,0.3298224]
  cd(0:1, 191) = [156.0000000,0.2769177]
  cd(0:1, 192) = [159.0000000,0.2240133]
  cd(0:1, 193) = [162.0000000,0.1857407]
  cd(0:1, 194) = [165.0000000,0.1547841]
  cd(0:1, 195) = [168.0000000,0.1238274]
  cd(0:1, 196) = [171.0000000,0.0928706]
  cd(0:1, 197) = [174.0000000,0.0619138]
  cd(0:1, 198) = [177.0000000,0.0510484]
  cd(0:1, 199) = [180.0000000,0.0502377]

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
!     PRINT*,'alpha < tabulated range in calc_cd18_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd18 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd18_ge28.pro'
  ENDIF

!print,y,cd18

!_________________________________________________________________________
  
END FUNCTION calc_cd18_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd19_ge28( y ) RESULT( cd19 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd19
  INTEGER :: i

  cd19 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.0536137]
  cd(0:1, 1  ) = [-177.0000000,0.0536137]
  cd(0:1, 2  ) = [-174.0000000,0.0643364]
  cd(0:1, 3  ) = [-171.0000000,0.0965048]
  cd(0:1, 4  ) = [-168.0000000,0.1286729]
  cd(0:1, 5  ) = [-165.0000000,0.1608407]
  cd(0:1, 6  ) = [-162.0000000,0.1930087]
  cd(0:1, 7  ) = [-159.0000000,0.2324216]
  cd(0:1, 8  ) = [-156.0000000,0.2863240]
  cd(0:1, 9  ) = [-153.0000000,0.3402264]
  cd(0:1, 10 ) = [-150.0000000,0.3941288]
  cd(0:1, 11 ) = [-147.0000000,0.4641910]
  cd(0:1, 12 ) = [-144.0000000,0.5342533]
  cd(0:1, 13 ) = [-141.0000000,0.6043159]
  cd(0:1, 14 ) = [-138.0000000,0.6772556]
  cd(0:1, 15 ) = [-135.0000000,0.7516339]
  cd(0:1, 16 ) = [-132.0000000,0.8260126]
  cd(0:1, 17 ) = [-129.0000000,0.8988003]
  cd(0:1, 18 ) = [-126.0000000,0.9684062]
  cd(0:1, 19 ) = [-123.0000000,1.0380115]
  cd(0:1, 20 ) = [-120.0000000,1.1076166]
  cd(0:1, 21 ) = [-117.0000000,1.1639139]
  cd(0:1, 22 ) = [-114.0000000,1.2202111]
  cd(0:1, 23 ) = [-111.0000000,1.2765078]
  cd(0:1, 24 ) = [-108.0000000,1.3193057]
  cd(0:1, 25 ) = [-105.0000000,1.3553541]
  cd(0:1, 26 ) = [-102.0000000,1.3914022]
  cd(0:1, 27 ) = [-99.0000000,1.4191969]
  cd(0:1, 28 ) = [-96.0000000,1.4304846]
  cd(0:1, 29 ) = [-93.0000000,1.4417723]
  cd(0:1, 30 ) = [-90.0000000,1.4530599]
  cd(0:1, 31 ) = [-87.0000000,1.4417723]
  cd(0:1, 32 ) = [-84.0000000,1.4304846]
  cd(0:1, 33 ) = [-81.0000000,1.4191970]
  cd(0:1, 34 ) = [-78.0000000,1.3914023]
  cd(0:1, 35 ) = [-75.0000000,1.3553539]
  cd(0:1, 36 ) = [-72.0000000,1.3193059]
  cd(0:1, 37 ) = [-69.0000000,1.2765082]
  cd(0:1, 38 ) = [-66.0000000,1.2202109]
  cd(0:1, 39 ) = [-63.0000000,1.1639136]
  cd(0:1, 40 ) = [-60.0000000,1.1076161]
  cd(0:1, 41 ) = [-57.0000000,1.0380111]
  cd(0:1, 42 ) = [-54.0000000,0.9684057]
  cd(0:1, 43 ) = [-51.0000000,0.8987999]
  cd(0:1, 44 ) = [-48.0000000,0.8260507]
  cd(0:1, 45 ) = [-45.0000000,0.7517300]
  cd(0:1, 46 ) = [-42.0000000,0.6772938]
  cd(0:1, 47 ) = [-39.0000000,0.6032599]
  cd(0:1, 48 ) = [-36.0000000,0.5308513]
  cd(0:1, 49 ) = [-33.0000000,0.4610882]
  cd(0:1, 50 ) = [-30.0000000,0.3941284]
  cd(0:1, 51 ) = [-29.3939394,0.3812795]
  cd(0:1, 52 ) = [-28.7878788,0.3684306]
  cd(0:1, 53 ) = [-28.1818182,0.3555817]
  cd(0:1, 54 ) = [-27.5757576,0.3431441]
  cd(0:1, 55 ) = [-26.9696970,0.3308830]
  cd(0:1, 56 ) = [-26.3636364,0.3186218]
  cd(0:1, 57 ) = [-25.7575758,0.3066202]
  cd(0:1, 58 ) = [-25.1515152,0.2950079]
  cd(0:1, 59 ) = [-24.5454545,0.2833956]
  cd(0:1, 60 ) = [-23.9393939,0.2718486]
  cd(0:1, 61 ) = [-23.3333333,0.2608888]
  cd(0:1, 62 ) = [-22.7272727,0.2499290]
  cd(0:1, 63 ) = [-22.1212121,0.2389692]
  cd(0:1, 64 ) = [-21.5151515,0.2294748]
  cd(0:1, 65 ) = [-20.9090909,0.2203468]
  cd(0:1, 66 ) = [-20.3030303,0.2112188]
  cd(0:1, 67 ) = [-19.6969697,0.2020911]
  cd(0:1, 68 ) = [-19.0909091,0.1929635]
  cd(0:1, 69 ) = [-18.4848485,0.1841718]
  cd(0:1, 70 ) = [-17.8787879,0.1762839]
  cd(0:1, 71 ) = [-17.2727273,0.1686168]
  cd(0:1, 72 ) = [-16.6666667,0.1602975]
  cd(0:1, 73 ) = [-16.0606061,0.1515665]
  cd(0:1, 74 ) = [-15.4545455,0.1418874]
  cd(0:1, 75 ) = [-14.8484848,0.1316502]
  cd(0:1, 76 ) = [-14.2424242,0.1203253]
  cd(0:1, 77 ) = [-13.6363636,0.1088181]
  cd(0:1, 78 ) = [-13.0303030,0.0971705]
  cd(0:1, 79 ) = [-12.4242424,0.0835932]
  cd(0:1, 80 ) = [-11.8181818,0.0702569]
  cd(0:1, 81 ) = [-11.2121212,0.0592864]
  cd(0:1, 82 ) = [-10.6060606,0.0502112]
  cd(0:1, 83 ) = [-10.0000000,0.0420063]
  cd(0:1, 84 ) = [-9.3939394,0.0339432]
  cd(0:1, 85 ) = [-8.7878788,0.0258616]
  cd(0:1, 86 ) = [-8.1818182,0.0176807]
  cd(0:1, 87 ) = [-7.5757576,0.0140780]
  cd(0:1, 88 ) = [-6.9696970,0.0120930]
  cd(0:1, 89 ) = [-6.3636364,0.0112571]
  cd(0:1, 90 ) = [-5.7575758,0.0104387]
  cd(0:1, 91 ) = [-5.1515152,0.0098905]
  cd(0:1, 92 ) = [-4.5454545,0.0095557]
  cd(0:1, 93 ) = [-3.9393939,0.0092823]
  cd(0:1, 94 ) = [-3.3333333,0.0090091]
  cd(0:1, 95 ) = [-2.7272727,0.0088226]
  cd(0:1, 96 ) = [-2.1212121,0.0087171]
  cd(0:1, 97 ) = [-1.5151515,0.0086351]
  cd(0:1, 98 ) = [-0.9090909,0.0085979]
  cd(0:1, 99 ) = [-0.3030303,0.0086847]
  cd(0:1, 100) = [0.3030303,0.0087367]
  cd(0:1, 101) = [0.9090909,0.0087669]
  cd(0:1, 102) = [1.5151515,0.0087556]
  cd(0:1, 103) = [2.1212121,0.0088994]
  cd(0:1, 104) = [2.7272727,0.0090747]
  cd(0:1, 105) = [3.3333333,0.0092315]
  cd(0:1, 106) = [3.9393939,0.0093577]
  cd(0:1, 107) = [4.5454545,0.0095295]
  cd(0:1, 108) = [5.1515152,0.0097331]
  cd(0:1, 109) = [5.7575758,0.0100007]
  cd(0:1, 110) = [6.3636364,0.0102974]
  cd(0:1, 111) = [6.9696970,0.0105582]
  cd(0:1, 112) = [7.5757576,0.0110669]
  cd(0:1, 113) = [8.1818182,0.0121179]
  cd(0:1, 114) = [8.7878788,0.0138745]
  cd(0:1, 115) = [9.3939394,0.0207295]
  cd(0:1, 116) = [10.0000000,0.0303611]
  cd(0:1, 117) = [10.6060606,0.0389303]
  cd(0:1, 118) = [11.2121212,0.0459269]
  cd(0:1, 119) = [11.8181818,0.0500033]
  cd(0:1, 120) = [12.4242424,0.0600007]
  cd(0:1, 121) = [13.0303030,0.0724446]
  cd(0:1, 122) = [13.6363636,0.0822712]
  cd(0:1, 123) = [14.2424242,0.0922479]
  cd(0:1, 124) = [14.8484848,0.1023843]
  cd(0:1, 125) = [15.4545455,0.1113626]
  cd(0:1, 126) = [16.0606061,0.1198647]
  cd(0:1, 127) = [16.6666667,0.1275548]
  cd(0:1, 128) = [17.2727273,0.1364949]
  cd(0:1, 129) = [17.8787879,0.1469627]
  cd(0:1, 130) = [18.4848485,0.1591452]
  cd(0:1, 131) = [19.0909091,0.1716468]
  cd(0:1, 132) = [19.6969697,0.1835276]
  cd(0:1, 133) = [20.3030303,0.1964668]
  cd(0:1, 134) = [20.9090909,0.2104646]
  cd(0:1, 135) = [21.5151515,0.2244624]
  cd(0:1, 136) = [22.1212121,0.2379201]
  cd(0:1, 137) = [22.7272727,0.2492183]
  cd(0:1, 138) = [23.3333333,0.2605165]
  cd(0:1, 139) = [23.9393939,0.2718147]
  cd(0:1, 140) = [24.5454545,0.2833956]
  cd(0:1, 141) = [25.1515152,0.2950079]
  cd(0:1, 142) = [25.7575758,0.3066202]
  cd(0:1, 143) = [26.3636364,0.3186218]
  cd(0:1, 144) = [26.9696970,0.3308830]
  cd(0:1, 145) = [27.5757576,0.3431441]
  cd(0:1, 146) = [28.1818182,0.3555817]
  cd(0:1, 147) = [28.7878788,0.3684306]
  cd(0:1, 148) = [29.3939394,0.3812795]
  cd(0:1, 149) = [30.0000000,0.3941284]
  cd(0:1, 150) = [33.0000000,0.4610882]
  cd(0:1, 151) = [36.0000000,0.5308513]
  cd(0:1, 152) = [39.0000000,0.6032599]
  cd(0:1, 153) = [42.0000000,0.6772938]
  cd(0:1, 154) = [45.0000000,0.7517300]
  cd(0:1, 155) = [48.0000000,0.8260507]
  cd(0:1, 156) = [51.0000000,0.8987999]
  cd(0:1, 157) = [54.0000000,0.9684057]
  cd(0:1, 158) = [57.0000000,1.0380111]
  cd(0:1, 159) = [60.0000000,1.1076161]
  cd(0:1, 160) = [63.0000000,1.1639136]
  cd(0:1, 161) = [66.0000000,1.2202109]
  cd(0:1, 162) = [69.0000000,1.2765082]
  cd(0:1, 163) = [72.0000000,1.3193059]
  cd(0:1, 164) = [75.0000000,1.3553539]
  cd(0:1, 165) = [78.0000000,1.3914023]
  cd(0:1, 166) = [81.0000000,1.4191970]
  cd(0:1, 167) = [84.0000000,1.4304846]
  cd(0:1, 168) = [87.0000000,1.4417723]
  cd(0:1, 169) = [90.0000000,1.4530599]
  cd(0:1, 170) = [93.0000000,1.4417723]
  cd(0:1, 171) = [96.0000000,1.4304846]
  cd(0:1, 172) = [99.0000000,1.4191969]
  cd(0:1, 173) = [102.0000000,1.3914022]
  cd(0:1, 174) = [105.0000000,1.3553541]
  cd(0:1, 175) = [108.0000000,1.3193057]
  cd(0:1, 176) = [111.0000000,1.2765078]
  cd(0:1, 177) = [114.0000000,1.2202111]
  cd(0:1, 178) = [117.0000000,1.1639139]
  cd(0:1, 179) = [120.0000000,1.1076166]
  cd(0:1, 180) = [123.0000000,1.0380115]
  cd(0:1, 181) = [126.0000000,0.9684062]
  cd(0:1, 182) = [129.0000000,0.8988003]
  cd(0:1, 183) = [132.0000000,0.8260126]
  cd(0:1, 184) = [135.0000000,0.7516339]
  cd(0:1, 185) = [138.0000000,0.6772556]
  cd(0:1, 186) = [141.0000000,0.6043159]
  cd(0:1, 187) = [144.0000000,0.5342533]
  cd(0:1, 188) = [147.0000000,0.4641910]
  cd(0:1, 189) = [150.0000000,0.3941288]
  cd(0:1, 190) = [153.0000000,0.3417312]
  cd(0:1, 191) = [156.0000000,0.2893337]
  cd(0:1, 192) = [159.0000000,0.2369362]
  cd(0:1, 193) = [162.0000000,0.1975235]
  cd(0:1, 194) = [165.0000000,0.1646032]
  cd(0:1, 195) = [168.0000000,0.1316827]
  cd(0:1, 196) = [171.0000000,0.0987621]
  cd(0:1, 197) = [174.0000000,0.0658413]
  cd(0:1, 198) = [177.0000000,0.0543628]
  cd(0:1, 199) = [180.0000000,0.0536137]

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
!     PRINT*,'alpha < tabulated range in calc_cd19_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd19 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd19_ge28.pro'
  ENDIF

!print,y,cd19

!_________________________________________________________________________
  
END FUNCTION calc_cd19_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd20_ge28( y ) RESULT( cd20 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd20
  INTEGER :: i

  cd20 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.0558151]
  cd(0:1, 1  ) = [-177.0000000,0.0558151]
  cd(0:1, 2  ) = [-174.0000000,0.0669781]
  cd(0:1, 3  ) = [-171.0000000,0.1004673]
  cd(0:1, 4  ) = [-168.0000000,0.1339563]
  cd(0:1, 5  ) = [-165.0000000,0.1674451]
  cd(0:1, 6  ) = [-162.0000000,0.2009338]
  cd(0:1, 7  ) = [-159.0000000,0.2410900]
  cd(0:1, 8  ) = [-156.0000000,0.2945811]
  cd(0:1, 9  ) = [-153.0000000,0.3480722]
  cd(0:1, 10 ) = [-150.0000000,0.4015634]
  cd(0:1, 11 ) = [-147.0000000,0.4713682]
  cd(0:1, 12 ) = [-144.0000000,0.5411730]
  cd(0:1, 13 ) = [-141.0000000,0.6109780]
  cd(0:1, 14 ) = [-138.0000000,0.6836203]
  cd(0:1, 15 ) = [-135.0000000,0.7576812]
  cd(0:1, 16 ) = [-132.0000000,0.8317425]
  cd(0:1, 17 ) = [-129.0000000,0.9041960]
  cd(0:1, 18 ) = [-126.0000000,0.9734343]
  cd(0:1, 19 ) = [-123.0000000,1.0426720]
  cd(0:1, 20 ) = [-120.0000000,1.1119095]
  cd(0:1, 21 ) = [-117.0000000,1.1677999]
  cd(0:1, 22 ) = [-114.0000000,1.2236901]
  cd(0:1, 23 ) = [-111.0000000,1.2795798]
  cd(0:1, 24 ) = [-108.0000000,1.3219531]
  cd(0:1, 25 ) = [-105.0000000,1.3575681]
  cd(0:1, 26 ) = [-102.0000000,1.3931826]
  cd(0:1, 27 ) = [-99.0000000,1.4205392]
  cd(0:1, 28 ) = [-96.0000000,1.4313795]
  cd(0:1, 29 ) = [-93.0000000,1.4422198]
  cd(0:1, 30 ) = [-90.0000000,1.4530599]
  cd(0:1, 31 ) = [-87.0000000,1.4422198]
  cd(0:1, 32 ) = [-84.0000000,1.4313795]
  cd(0:1, 33 ) = [-81.0000000,1.4205393]
  cd(0:1, 34 ) = [-78.0000000,1.3931828]
  cd(0:1, 35 ) = [-75.0000000,1.3575678]
  cd(0:1, 36 ) = [-72.0000000,1.3219533]
  cd(0:1, 37 ) = [-69.0000000,1.2795803]
  cd(0:1, 38 ) = [-66.0000000,1.2236899]
  cd(0:1, 39 ) = [-63.0000000,1.1677996]
  cd(0:1, 40 ) = [-60.0000000,1.1119091]
  cd(0:1, 41 ) = [-57.0000000,1.0426716]
  cd(0:1, 42 ) = [-54.0000000,0.9734338]
  cd(0:1, 43 ) = [-51.0000000,0.9041956]
  cd(0:1, 44 ) = [-48.0000000,0.8317897]
  cd(0:1, 45 ) = [-45.0000000,0.7578001]
  cd(0:1, 46 ) = [-42.0000000,0.6836675]
  cd(0:1, 47 ) = [-39.0000000,0.6099304]
  cd(0:1, 48 ) = [-36.0000000,0.5377963]
  cd(0:1, 49 ) = [-33.0000000,0.4682867]
  cd(0:1, 50 ) = [-30.0000000,0.4015630]
  cd(0:1, 51 ) = [-29.3939394,0.3887580]
  cd(0:1, 52 ) = [-28.7878788,0.3759531]
  cd(0:1, 53 ) = [-28.1818182,0.3631482]
  cd(0:1, 54 ) = [-27.5757576,0.3507528]
  cd(0:1, 55 ) = [-26.9696970,0.3385328]
  cd(0:1, 56 ) = [-26.3636364,0.3263129]
  cd(0:1, 57 ) = [-25.7575758,0.3143514]
  cd(0:1, 58 ) = [-25.1515152,0.3027775]
  cd(0:1, 59 ) = [-24.5454545,0.2912035]
  cd(0:1, 60 ) = [-23.9393939,0.2796929]
  cd(0:1, 61 ) = [-23.3333333,0.2687515]
  cd(0:1, 62 ) = [-22.7272727,0.2578102]
  cd(0:1, 63 ) = [-22.1212121,0.2468689]
  cd(0:1, 64 ) = [-21.5151515,0.2362220]
  cd(0:1, 65 ) = [-20.9090909,0.2256487]
  cd(0:1, 66 ) = [-20.3030303,0.2150754]
  cd(0:1, 67 ) = [-19.6969697,0.2045024]
  cd(0:1, 68 ) = [-19.0909091,0.1939297]
  cd(0:1, 69 ) = [-18.4848485,0.1836954]
  cd(0:1, 70 ) = [-17.8787879,0.1740275]
  cd(0:1, 71 ) = [-17.2727273,0.1641948]
  cd(0:1, 72 ) = [-16.6666667,0.1540848]
  cd(0:1, 73 ) = [-16.0606061,0.1437644]
  cd(0:1, 74 ) = [-15.4545455,0.1331174]
  cd(0:1, 75 ) = [-14.8484848,0.1222861]
  cd(0:1, 76 ) = [-14.2424242,0.1111830]
  cd(0:1, 77 ) = [-13.6363636,0.1000227]
  cd(0:1, 78 ) = [-13.0303030,0.0888117]
  cd(0:1, 79 ) = [-12.4242424,0.0752792]
  cd(0:1, 80 ) = [-11.8181818,0.0620273]
  cd(0:1, 81 ) = [-11.2121212,0.0507144]
  cd(0:1, 82 ) = [-10.6060606,0.0414292]
  cd(0:1, 83 ) = [-10.0000000,0.0331403]
  cd(0:1, 84 ) = [-9.3939394,0.0263235]
  cd(0:1, 85 ) = [-8.7878788,0.0200390]
  cd(0:1, 86 ) = [-8.1818182,0.0148308]
  cd(0:1, 87 ) = [-7.5757576,0.0123437]
  cd(0:1, 88 ) = [-6.9696970,0.0108868]
  cd(0:1, 89 ) = [-6.3636364,0.0100699]
  cd(0:1, 90 ) = [-5.7575758,0.0093497]
  cd(0:1, 91 ) = [-5.1515152,0.0089373]
  cd(0:1, 92 ) = [-4.5454545,0.0086907]
  cd(0:1, 93 ) = [-3.9393939,0.0084857]
  cd(0:1, 94 ) = [-3.3333333,0.0082586]
  cd(0:1, 95 ) = [-2.7272727,0.0080791]
  cd(0:1, 96 ) = [-2.1212121,0.0079421]
  cd(0:1, 97 ) = [-1.5151515,0.0078659]
  cd(0:1, 98 ) = [-0.9090909,0.0078414]
  cd(0:1, 99 ) = [-0.3030303,0.0079263]
  cd(0:1, 100) = [0.3030303,0.0079664]
  cd(0:1, 101) = [0.9090909,0.0079726]
  cd(0:1, 102) = [1.5151515,0.0079667]
  cd(0:1, 103) = [2.1212121,0.0081359]
  cd(0:1, 104) = [2.7272727,0.0083030]
  cd(0:1, 105) = [3.3333333,0.0084548]
  cd(0:1, 106) = [3.9393939,0.0086089]
  cd(0:1, 107) = [4.5454545,0.0088121]
  cd(0:1, 108) = [5.1515152,0.0090438]
  cd(0:1, 109) = [5.7575758,0.0093399]
  cd(0:1, 110) = [6.3636364,0.0096783]
  cd(0:1, 111) = [6.9696970,0.0100088]
  cd(0:1, 112) = [7.5757576,0.0107211]
  cd(0:1, 113) = [8.1818182,0.0119391]
  cd(0:1, 114) = [8.7878788,0.0139481]
  cd(0:1, 115) = [9.3939394,0.0227726]
  cd(0:1, 116) = [10.0000000,0.0352534]
  cd(0:1, 117) = [10.6060606,0.0462230]
  cd(0:1, 118) = [11.2121212,0.0550223]
  cd(0:1, 119) = [11.8181818,0.0597916]
  cd(0:1, 120) = [12.4242424,0.0674862]
  cd(0:1, 121) = [13.0303030,0.0764936]
  cd(0:1, 122) = [13.6363636,0.0865014]
  cd(0:1, 123) = [14.2424242,0.0967716]
  cd(0:1, 124) = [14.8484848,0.1074246]
  cd(0:1, 125) = [15.4545455,0.1165710]
  cd(0:1, 126) = [16.0606061,0.1250852]
  cd(0:1, 127) = [16.6666667,0.1324270]
  cd(0:1, 128) = [17.2727273,0.1413095]
  cd(0:1, 129) = [17.8787879,0.1520751]
  cd(0:1, 130) = [18.4848485,0.1644650]
  cd(0:1, 131) = [19.0909091,0.1770942]
  cd(0:1, 132) = [19.6969697,0.1887788]
  cd(0:1, 133) = [20.3030303,0.2019825]
  cd(0:1, 134) = [20.9090909,0.2167052]
  cd(0:1, 135) = [21.5151515,0.2314280]
  cd(0:1, 136) = [22.1212121,0.2454837]
  cd(0:1, 137) = [22.7272727,0.2568718]
  cd(0:1, 138) = [23.3333333,0.2682600]
  cd(0:1, 139) = [23.9393939,0.2796482]
  cd(0:1, 140) = [24.5454545,0.2912035]
  cd(0:1, 141) = [25.1515152,0.3027775]
  cd(0:1, 142) = [25.7575758,0.3143514]
  cd(0:1, 143) = [26.3636364,0.3263129]
  cd(0:1, 144) = [26.9696970,0.3385328]
  cd(0:1, 145) = [27.5757576,0.3507528]
  cd(0:1, 146) = [28.1818182,0.3631482]
  cd(0:1, 147) = [28.7878788,0.3759531]
  cd(0:1, 148) = [29.3939394,0.3887580]
  cd(0:1, 149) = [30.0000000,0.4015630]
  cd(0:1, 150) = [33.0000000,0.4682867]
  cd(0:1, 151) = [36.0000000,0.5377963]
  cd(0:1, 152) = [39.0000000,0.6099304]
  cd(0:1, 153) = [42.0000000,0.6836675]
  cd(0:1, 154) = [45.0000000,0.7578001]
  cd(0:1, 155) = [48.0000000,0.8317897]
  cd(0:1, 156) = [51.0000000,0.9041956]
  cd(0:1, 157) = [54.0000000,0.9734338]
  cd(0:1, 158) = [57.0000000,1.0426716]
  cd(0:1, 159) = [60.0000000,1.1119091]
  cd(0:1, 160) = [63.0000000,1.1677996]
  cd(0:1, 161) = [66.0000000,1.2236899]
  cd(0:1, 162) = [69.0000000,1.2795803]
  cd(0:1, 163) = [72.0000000,1.3219533]
  cd(0:1, 164) = [75.0000000,1.3575678]
  cd(0:1, 165) = [78.0000000,1.3931828]
  cd(0:1, 166) = [81.0000000,1.4205393]
  cd(0:1, 167) = [84.0000000,1.4313795]
  cd(0:1, 168) = [87.0000000,1.4422198]
  cd(0:1, 169) = [90.0000000,1.4530599]
  cd(0:1, 170) = [93.0000000,1.4422198]
  cd(0:1, 171) = [96.0000000,1.4313795]
  cd(0:1, 172) = [99.0000000,1.4205392]
  cd(0:1, 173) = [102.0000000,1.3931826]
  cd(0:1, 174) = [105.0000000,1.3575681]
  cd(0:1, 175) = [108.0000000,1.3219531]
  cd(0:1, 176) = [111.0000000,1.2795798]
  cd(0:1, 177) = [114.0000000,1.2236901]
  cd(0:1, 178) = [117.0000000,1.1677999]
  cd(0:1, 179) = [120.0000000,1.1119095]
  cd(0:1, 180) = [123.0000000,1.0426720]
  cd(0:1, 181) = [126.0000000,0.9734343]
  cd(0:1, 182) = [129.0000000,0.9041960]
  cd(0:1, 183) = [132.0000000,0.8317425]
  cd(0:1, 184) = [135.0000000,0.7576812]
  cd(0:1, 185) = [138.0000000,0.6836203]
  cd(0:1, 186) = [141.0000000,0.6109780]
  cd(0:1, 187) = [144.0000000,0.5411730]
  cd(0:1, 188) = [147.0000000,0.4713682]
  cd(0:1, 189) = [150.0000000,0.4015634]
  cd(0:1, 190) = [153.0000000,0.3494965]
  cd(0:1, 191) = [156.0000000,0.2974296]
  cd(0:1, 192) = [159.0000000,0.2453628]
  cd(0:1, 193) = [162.0000000,0.2052066]
  cd(0:1, 194) = [165.0000000,0.1710058]
  cd(0:1, 195) = [168.0000000,0.1368048]
  cd(0:1, 196) = [171.0000000,0.1026037]
  cd(0:1, 197) = [174.0000000,0.0684023]
  cd(0:1, 198) = [177.0000000,0.0565240]
  cd(0:1, 199) = [180.0000000,0.0558151]

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
!     PRINT*,'alpha < tabulated range in calc_cd20_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd20 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd20_ge28.pro'
  ENDIF

!print,y,cd20

!_________________________________________________________________________
  
END FUNCTION calc_cd20_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd21_ge28( y ) RESULT( cd21 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd21
  INTEGER :: i

  cd21 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.0560840]
  cd(0:1, 1  ) = [-177.0000000,0.0560840]
  cd(0:1, 2  ) = [-174.0000000,0.0673007]
  cd(0:1, 3  ) = [-171.0000000,0.1009513]
  cd(0:1, 4  ) = [-168.0000000,0.1346333]
  cd(0:1, 5  ) = [-165.0000000,0.1683311]
  cd(0:1, 6  ) = [-162.0000000,0.2020287]
  cd(0:1, 7  ) = [-159.0000000,0.2423100]
  cd(0:1, 8  ) = [-156.0000000,0.2957587]
  cd(0:1, 9  ) = [-153.0000000,0.3492073]
  cd(0:1, 10 ) = [-150.0000000,0.4026560]
  cd(0:1, 11 ) = [-147.0000000,0.4724229]
  cd(0:1, 12 ) = [-144.0000000,0.5421899]
  cd(0:1, 13 ) = [-141.0000000,0.6119571]
  cd(0:1, 14 ) = [-138.0000000,0.6845557]
  cd(0:1, 15 ) = [-135.0000000,0.7585700]
  cd(0:1, 16 ) = [-132.0000000,0.8325845]
  cd(0:1, 17 ) = [-129.0000000,0.9049890]
  cd(0:1, 18 ) = [-126.0000000,0.9741733]
  cd(0:1, 19 ) = [-123.0000000,1.0433570]
  cd(0:1, 20 ) = [-120.0000000,1.1125405]
  cd(0:1, 21 ) = [-117.0000000,1.1683710]
  cd(0:1, 22 ) = [-114.0000000,1.2242014]
  cd(0:1, 23 ) = [-111.0000000,1.2800313]
  cd(0:1, 24 ) = [-108.0000000,1.3223422]
  cd(0:1, 25 ) = [-105.0000000,1.3578935]
  cd(0:1, 26 ) = [-102.0000000,1.3934443]
  cd(0:1, 27 ) = [-99.0000000,1.4207365]
  cd(0:1, 28 ) = [-96.0000000,1.4315110]
  cd(0:1, 29 ) = [-93.0000000,1.4422855]
  cd(0:1, 30 ) = [-90.0000000,1.4530599]
  cd(0:1, 31 ) = [-87.0000000,1.4422855]
  cd(0:1, 32 ) = [-84.0000000,1.4315110]
  cd(0:1, 33 ) = [-81.0000000,1.4207366]
  cd(0:1, 34 ) = [-78.0000000,1.3934445]
  cd(0:1, 35 ) = [-75.0000000,1.3578932]
  cd(0:1, 36 ) = [-72.0000000,1.3223424]
  cd(0:1, 37 ) = [-69.0000000,1.2800318]
  cd(0:1, 38 ) = [-66.0000000,1.2242012]
  cd(0:1, 39 ) = [-63.0000000,1.1683707]
  cd(0:1, 40 ) = [-60.0000000,1.1125400]
  cd(0:1, 41 ) = [-57.0000000,1.0433565]
  cd(0:1, 42 ) = [-54.0000000,0.9741728]
  cd(0:1, 43 ) = [-51.0000000,0.9049885]
  cd(0:1, 44 ) = [-48.0000000,0.8326331]
  cd(0:1, 45 ) = [-45.0000000,0.7586922]
  cd(0:1, 46 ) = [-42.0000000,0.6846043]
  cd(0:1, 47 ) = [-39.0000000,0.6109107]
  cd(0:1, 48 ) = [-36.0000000,0.5388170]
  cd(0:1, 49 ) = [-33.0000000,0.4693447]
  cd(0:1, 50 ) = [-30.0000000,0.4026556]
  cd(0:1, 51 ) = [-29.3939394,0.3898571]
  cd(0:1, 52 ) = [-28.7878788,0.3770587]
  cd(0:1, 53 ) = [-28.1818182,0.3642602]
  cd(0:1, 54 ) = [-27.5757576,0.3518710]
  cd(0:1, 55 ) = [-26.9696970,0.3396571]
  cd(0:1, 56 ) = [-26.3636364,0.3274432]
  cd(0:1, 57 ) = [-25.7575758,0.3154876]
  cd(0:1, 58 ) = [-25.1515152,0.3039193]
  cd(0:1, 59 ) = [-24.5454545,0.2923510]
  cd(0:1, 60 ) = [-23.9393939,0.2808457]
  cd(0:1, 61 ) = [-23.3333333,0.2699070]
  cd(0:1, 62 ) = [-22.7272727,0.2589683]
  cd(0:1, 63 ) = [-22.1212121,0.2480296]
  cd(0:1, 64 ) = [-21.5151515,0.2371737]
  cd(0:1, 65 ) = [-20.9090909,0.2263386]
  cd(0:1, 66 ) = [-20.3030303,0.2155034]
  cd(0:1, 67 ) = [-19.6969697,0.2046708]
  cd(0:1, 68 ) = [-19.0909091,0.1938408]
  cd(0:1, 69 ) = [-18.4848485,0.1828171]
  cd(0:1, 70 ) = [-17.8787879,0.1715072]
  cd(0:1, 71 ) = [-17.2727273,0.1603447]
  cd(0:1, 72 ) = [-16.6666667,0.1492992]
  cd(0:1, 73 ) = [-16.0606061,0.1383522]
  cd(0:1, 74 ) = [-15.4545455,0.1275320]
  cd(0:1, 75 ) = [-14.8484848,0.1167811]
  cd(0:1, 76 ) = [-14.2424242,0.1061076]
  cd(0:1, 77 ) = [-13.6363636,0.0954550]
  cd(0:1, 78 ) = [-13.0303030,0.0848210]
  cd(0:1, 79 ) = [-12.4242424,0.0723014]
  cd(0:1, 80 ) = [-11.8181818,0.0603072]
  cd(0:1, 81 ) = [-11.2121212,0.0491970]
  cd(0:1, 82 ) = [-10.6060606,0.0398751]
  cd(0:1, 83 ) = [-10.0000000,0.0315713]
  cd(0:1, 84 ) = [-9.3939394,0.0249748]
  cd(0:1, 85 ) = [-8.7878788,0.0190107]
  cd(0:1, 86 ) = [-8.1818182,0.0141430]
  cd(0:1, 87 ) = [-7.5757576,0.0116500]
  cd(0:1, 88 ) = [-6.9696970,0.0102795]
  cd(0:1, 89 ) = [-6.3636364,0.0093710]
  cd(0:1, 90 ) = [-5.7575758,0.0087225]
  cd(0:1, 91 ) = [-5.1515152,0.0083663]
  cd(0:1, 92 ) = [-4.5454545,0.0081505]
  cd(0:1, 93 ) = [-3.9393939,0.0079840]
  cd(0:1, 94 ) = [-3.3333333,0.0077952]
  cd(0:1, 95 ) = [-2.7272727,0.0076345]
  cd(0:1, 96 ) = [-2.1212121,0.0075111]
  cd(0:1, 97 ) = [-1.5151515,0.0074483]
  cd(0:1, 98 ) = [-0.9090909,0.0074316]
  cd(0:1, 99 ) = [-0.3030303,0.0075101]
  cd(0:1, 100) = [0.3030303,0.0075494]
  cd(0:1, 101) = [0.9090909,0.0075481]
  cd(0:1, 102) = [1.5151515,0.0075580]
  cd(0:1, 103) = [2.1212121,0.0077151]
  cd(0:1, 104) = [2.7272727,0.0078742]
  cd(0:1, 105) = [3.3333333,0.0080207]
  cd(0:1, 106) = [3.9393939,0.0081763]
  cd(0:1, 107) = [4.5454545,0.0083763]
  cd(0:1, 108) = [5.1515152,0.0086050]
  cd(0:1, 109) = [5.7575758,0.0089080]
  cd(0:1, 110) = [6.3636364,0.0092759]
  cd(0:1, 111) = [6.9696970,0.0097088]
  cd(0:1, 112) = [7.5757576,0.0106633]
  cd(0:1, 113) = [8.1818182,0.0119127]
  cd(0:1, 114) = [8.7878788,0.0139780]
  cd(0:1, 115) = [9.3939394,0.0230914]
  cd(0:1, 116) = [10.0000000,0.0360093]
  cd(0:1, 117) = [10.6060606,0.0473489]
  cd(0:1, 118) = [11.2121212,0.0564290]
  cd(0:1, 119) = [11.8181818,0.0613614]
  cd(0:1, 120) = [12.4242424,0.0686991]
  cd(0:1, 121) = [13.0303030,0.0771321]
  cd(0:1, 122) = [13.6363636,0.0872204]
  cd(0:1, 123) = [14.2424242,0.0975651]
  cd(0:1, 124) = [14.8484848,0.1082942]
  cd(0:1, 125) = [15.4545455,0.1174643]
  cd(0:1, 126) = [16.0606061,0.1259782]
  cd(0:1, 127) = [16.6666667,0.1332625]
  cd(0:1, 128) = [17.2727273,0.1421294]
  cd(0:1, 129) = [17.8787879,0.1529304]
  cd(0:1, 130) = [18.4848485,0.1653268]
  cd(0:1, 131) = [19.0909091,0.1779479]
  cd(0:1, 132) = [19.6969697,0.1895818]
  cd(0:1, 133) = [20.3030303,0.2028099]
  cd(0:1, 134) = [20.9090909,0.2176324]
  cd(0:1, 135) = [21.5151515,0.2324549]
  cd(0:1, 136) = [22.1212121,0.2465931]
  cd(0:1, 137) = [22.7272727,0.2579952]
  cd(0:1, 138) = [23.3333333,0.2693973]
  cd(0:1, 139) = [23.9393939,0.2807994]
  cd(0:1, 140) = [24.5454545,0.2923510]
  cd(0:1, 141) = [25.1515152,0.3039193]
  cd(0:1, 142) = [25.7575758,0.3154876]
  cd(0:1, 143) = [26.3636364,0.3274432]
  cd(0:1, 144) = [26.9696970,0.3396571]
  cd(0:1, 145) = [27.5757576,0.3518710]
  cd(0:1, 146) = [28.1818182,0.3642602]
  cd(0:1, 147) = [28.7878788,0.3770587]
  cd(0:1, 148) = [29.3939394,0.3898571]
  cd(0:1, 149) = [30.0000000,0.4026556]
  cd(0:1, 150) = [33.0000000,0.4693447]
  cd(0:1, 151) = [36.0000000,0.5388170]
  cd(0:1, 152) = [39.0000000,0.6109107]
  cd(0:1, 153) = [42.0000000,0.6846043]
  cd(0:1, 154) = [45.0000000,0.7586922]
  cd(0:1, 155) = [48.0000000,0.8326331]
  cd(0:1, 156) = [51.0000000,0.9049885]
  cd(0:1, 157) = [54.0000000,0.9741728]
  cd(0:1, 158) = [57.0000000,1.0433565]
  cd(0:1, 159) = [60.0000000,1.1125400]
  cd(0:1, 160) = [63.0000000,1.1683707]
  cd(0:1, 161) = [66.0000000,1.2242012]
  cd(0:1, 162) = [69.0000000,1.2800318]
  cd(0:1, 163) = [72.0000000,1.3223424]
  cd(0:1, 164) = [75.0000000,1.3578932]
  cd(0:1, 165) = [78.0000000,1.3934445]
  cd(0:1, 166) = [81.0000000,1.4207366]
  cd(0:1, 167) = [84.0000000,1.4315110]
  cd(0:1, 168) = [87.0000000,1.4422855]
  cd(0:1, 169) = [90.0000000,1.4530599]
  cd(0:1, 170) = [93.0000000,1.4422855]
  cd(0:1, 171) = [96.0000000,1.4315110]
  cd(0:1, 172) = [99.0000000,1.4207365]
  cd(0:1, 173) = [102.0000000,1.3934443]
  cd(0:1, 174) = [105.0000000,1.3578935]
  cd(0:1, 175) = [108.0000000,1.3223422]
  cd(0:1, 176) = [111.0000000,1.2800313]
  cd(0:1, 177) = [114.0000000,1.2242014]
  cd(0:1, 178) = [117.0000000,1.1683710]
  cd(0:1, 179) = [120.0000000,1.1125405]
  cd(0:1, 180) = [123.0000000,1.0433570]
  cd(0:1, 181) = [126.0000000,0.9741733]
  cd(0:1, 182) = [129.0000000,0.9049890]
  cd(0:1, 183) = [132.0000000,0.8325845]
  cd(0:1, 184) = [135.0000000,0.7585700]
  cd(0:1, 185) = [138.0000000,0.6845557]
  cd(0:1, 186) = [141.0000000,0.6119571]
  cd(0:1, 187) = [144.0000000,0.5421899]
  cd(0:1, 188) = [147.0000000,0.4724229]
  cd(0:1, 189) = [150.0000000,0.4026560]
  cd(0:1, 190) = [153.0000000,0.3506064]
  cd(0:1, 191) = [156.0000000,0.2985568]
  cd(0:1, 192) = [159.0000000,0.2465072]
  cd(0:1, 193) = [162.0000000,0.2062304]
  cd(0:1, 194) = [165.0000000,0.1718399]
  cd(0:1, 195) = [168.0000000,0.1374493]
  cd(0:1, 196) = [171.0000000,0.1030699]
  cd(0:1, 197) = [174.0000000,0.0687132]
  cd(0:1, 198) = [177.0000000,0.0567870]
  cd(0:1, 199) = [180.0000000,0.0560840]

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
!     PRINT*,'alpha < tabulated range in calc_cd21_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd21 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd21_ge28.pro'
  ENDIF

!print,y,cd21

!_________________________________________________________________________
  
END FUNCTION calc_cd21_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd22_ge28( y ) RESULT( cd22 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd22
  INTEGER :: i

  cd22 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.0543029]
  cd(0:1, 1  ) = [-177.0000000,0.0543029]
  cd(0:1, 2  ) = [-174.0000000,0.0651634]
  cd(0:1, 3  ) = [-171.0000000,0.0977453]
  cd(0:1, 4  ) = [-168.0000000,0.1308156]
  cd(0:1, 5  ) = [-165.0000000,0.1641301]
  cd(0:1, 6  ) = [-162.0000000,0.1974444]
  cd(0:1, 7  ) = [-159.0000000,0.2376184]
  cd(0:1, 8  ) = [-156.0000000,0.2915115]
  cd(0:1, 9  ) = [-153.0000000,0.3454046]
  cd(0:1, 10 ) = [-150.0000000,0.3992978]
  cd(0:1, 11 ) = [-147.0000000,0.4691810]
  cd(0:1, 12 ) = [-144.0000000,0.5390643]
  cd(0:1, 13 ) = [-141.0000000,0.6089478]
  cd(0:1, 14 ) = [-138.0000000,0.6816807]
  cd(0:1, 15 ) = [-135.0000000,0.7558384]
  cd(0:1, 16 ) = [-132.0000000,0.8299963]
  cd(0:1, 17 ) = [-129.0000000,0.9025518]
  cd(0:1, 18 ) = [-126.0000000,0.9719023]
  cd(0:1, 19 ) = [-123.0000000,1.0412521]
  cd(0:1, 20 ) = [-120.0000000,1.1106017]
  cd(0:1, 21 ) = [-117.0000000,1.1666162]
  cd(0:1, 22 ) = [-114.0000000,1.2226303]
  cd(0:1, 23 ) = [-111.0000000,1.2786441]
  cd(0:1, 24 ) = [-108.0000000,1.3211467]
  cd(0:1, 25 ) = [-105.0000000,1.3568936]
  cd(0:1, 26 ) = [-102.0000000,1.3926401]
  cd(0:1, 27 ) = [-99.0000000,1.4201301]
  cd(0:1, 28 ) = [-96.0000000,1.4311068]
  cd(0:1, 29 ) = [-93.0000000,1.4420834]
  cd(0:1, 30 ) = [-90.0000000,1.4530599]
  cd(0:1, 31 ) = [-87.0000000,1.4420834]
  cd(0:1, 32 ) = [-84.0000000,1.4311067]
  cd(0:1, 33 ) = [-81.0000000,1.4201302]
  cd(0:1, 34 ) = [-78.0000000,1.3926402]
  cd(0:1, 35 ) = [-75.0000000,1.3568934]
  cd(0:1, 36 ) = [-72.0000000,1.3211469]
  cd(0:1, 37 ) = [-69.0000000,1.2786446]
  cd(0:1, 38 ) = [-66.0000000,1.2226302]
  cd(0:1, 39 ) = [-63.0000000,1.1666158]
  cd(0:1, 40 ) = [-60.0000000,1.1106013]
  cd(0:1, 41 ) = [-57.0000000,1.0412517]
  cd(0:1, 42 ) = [-54.0000000,0.9719018]
  cd(0:1, 43 ) = [-51.0000000,0.9025513]
  cd(0:1, 44 ) = [-48.0000000,0.8300408]
  cd(0:1, 45 ) = [-45.0000000,0.7559503]
  cd(0:1, 46 ) = [-42.0000000,0.6817252]
  cd(0:1, 47 ) = [-39.0000000,0.6078977]
  cd(0:1, 48 ) = [-36.0000000,0.5356798]
  cd(0:1, 49 ) = [-33.0000000,0.4660930]
  cd(0:1, 50 ) = [-30.0000000,0.3992974]
  cd(0:1, 51 ) = [-29.3939394,0.3864791]
  cd(0:1, 52 ) = [-28.7878788,0.3736607]
  cd(0:1, 53 ) = [-28.1818182,0.3608424]
  cd(0:1, 54 ) = [-27.5757576,0.3484342]
  cd(0:1, 55 ) = [-26.9696970,0.3362017]
  cd(0:1, 56 ) = [-26.3636364,0.3239692]
  cd(0:1, 57 ) = [-25.7575758,0.3119955]
  cd(0:1, 58 ) = [-25.1515152,0.3004098]
  cd(0:1, 59 ) = [-24.5454545,0.2888242]
  cd(0:1, 60 ) = [-23.9393939,0.2773023]
  cd(0:1, 61 ) = [-23.3333333,0.2663539]
  cd(0:1, 62 ) = [-22.7272727,0.2554055]
  cd(0:1, 63 ) = [-22.1212121,0.2444572]
  cd(0:1, 64 ) = [-21.5151515,0.2335520]
  cd(0:1, 65 ) = [-20.9090909,0.2226577]
  cd(0:1, 66 ) = [-20.3030303,0.2117633]
  cd(0:1, 67 ) = [-19.6969697,0.2009038]
  cd(0:1, 68 ) = [-19.0909091,0.1900791]
  cd(0:1, 69 ) = [-18.4848485,0.1788209]
  cd(0:1, 70 ) = [-17.8787879,0.1669009]
  cd(0:1, 71 ) = [-17.2727273,0.1554330]
  cd(0:1, 72 ) = [-16.6666667,0.1442476]
  cd(0:1, 73 ) = [-16.0606061,0.1332997]
  cd(0:1, 74 ) = [-15.4545455,0.1226930]
  cd(0:1, 75 ) = [-14.8484848,0.1122752]
  cd(0:1, 76 ) = [-14.2424242,0.1020975]
  cd(0:1, 77 ) = [-13.6363636,0.0919774]
  cd(0:1, 78 ) = [-13.0303030,0.0819089]
  cd(0:1, 79 ) = [-12.4242424,0.0705955]
  cd(0:1, 80 ) = [-11.8181818,0.0600305]
  cd(0:1, 81 ) = [-11.2121212,0.0492519]
  cd(0:1, 82 ) = [-10.6060606,0.0399602]
  cd(0:1, 83 ) = [-10.0000000,0.0316635]
  cd(0:1, 84 ) = [-9.3939394,0.0250351]
  cd(0:1, 85 ) = [-8.7878788,0.0190363]
  cd(0:1, 86 ) = [-8.1818182,0.0139308]
  cd(0:1, 87 ) = [-7.5757576,0.0111966]
  cd(0:1, 88 ) = [-6.9696970,0.0098021]
  cd(0:1, 89 ) = [-6.3636364,0.0087936]
  cd(0:1, 90 ) = [-5.7575758,0.0082107]
  cd(0:1, 91 ) = [-5.1515152,0.0078942]
  cd(0:1, 92 ) = [-4.5454545,0.0076963]
  cd(0:1, 93 ) = [-3.9393939,0.0075598]
  cd(0:1, 94 ) = [-3.3333333,0.0074074]
  cd(0:1, 95 ) = [-2.7272727,0.0072685]
  cd(0:1, 96 ) = [-2.1212121,0.0071700]
  cd(0:1, 97 ) = [-1.5151515,0.0071224]
  cd(0:1, 98 ) = [-0.9090909,0.0071122]
  cd(0:1, 99 ) = [-0.3030303,0.0071834]
  cd(0:1, 100) = [0.3030303,0.0072247]
  cd(0:1, 101) = [0.9090909,0.0072200]
  cd(0:1, 102) = [1.5151515,0.0072472]
  cd(0:1, 103) = [2.1212121,0.0073841]
  cd(0:1, 104) = [2.7272727,0.0075352]
  cd(0:1, 105) = [3.3333333,0.0076764]
  cd(0:1, 106) = [3.9393939,0.0078268]
  cd(0:1, 107) = [4.5454545,0.0080139]
  cd(0:1, 108) = [5.1515152,0.0082307]
  cd(0:1, 109) = [5.7575758,0.0085345]
  cd(0:1, 110) = [6.3636364,0.0089301]
  cd(0:1, 111) = [6.9696970,0.0094703]
  cd(0:1, 112) = [7.5757576,0.0106647]
  cd(0:1, 113) = [8.1818182,0.0119527]
  cd(0:1, 114) = [8.7878788,0.0139975]
  cd(0:1, 115) = [9.3939394,0.0224354]
  cd(0:1, 116) = [10.0000000,0.0343304]
  cd(0:1, 117) = [10.6060606,0.0448349]
  cd(0:1, 118) = [11.2121212,0.0533280]
  cd(0:1, 119) = [11.8181818,0.0588309]
  cd(0:1, 120) = [12.4242424,0.0669421]
  cd(0:1, 121) = [13.0303030,0.0759412]
  cd(0:1, 122) = [13.6363636,0.0867113]
  cd(0:1, 123) = [14.2424242,0.0973733]
  cd(0:1, 124) = [14.8484848,0.1078731]
  cd(0:1, 125) = [15.4545455,0.1169515]
  cd(0:1, 126) = [16.0606061,0.1254309]
  cd(0:1, 127) = [16.6666667,0.1327824]
  cd(0:1, 128) = [17.2727273,0.1415714]
  cd(0:1, 129) = [17.8787879,0.1521174]
  cd(0:1, 130) = [18.4848485,0.1640760]
  cd(0:1, 131) = [19.0909091,0.1762521]
  cd(0:1, 132) = [19.6969697,0.1876595]
  cd(0:1, 133) = [20.3030303,0.2005615]
  cd(0:1, 134) = [20.9090909,0.2149581]
  cd(0:1, 135) = [21.5151515,0.2293548]
  cd(0:1, 136) = [22.1212121,0.2431463]
  cd(0:1, 137) = [22.7272727,0.2545175]
  cd(0:1, 138) = [23.3333333,0.2658888]
  cd(0:1, 139) = [23.9393939,0.2772600]
  cd(0:1, 140) = [24.5454545,0.2888242]
  cd(0:1, 141) = [25.1515152,0.3004098]
  cd(0:1, 142) = [25.7575758,0.3119955]
  cd(0:1, 143) = [26.3636364,0.3239692]
  cd(0:1, 144) = [26.9696970,0.3362017]
  cd(0:1, 145) = [27.5757576,0.3484342]
  cd(0:1, 146) = [28.1818182,0.3608424]
  cd(0:1, 147) = [28.7878788,0.3736607]
  cd(0:1, 148) = [29.3939394,0.3864791]
  cd(0:1, 149) = [30.0000000,0.3992974]
  cd(0:1, 150) = [33.0000000,0.4660930]
  cd(0:1, 151) = [36.0000000,0.5356798]
  cd(0:1, 152) = [39.0000000,0.6078977]
  cd(0:1, 153) = [42.0000000,0.6817252]
  cd(0:1, 154) = [45.0000000,0.7559503]
  cd(0:1, 155) = [48.0000000,0.8300408]
  cd(0:1, 156) = [51.0000000,0.9025513]
  cd(0:1, 157) = [54.0000000,0.9719018]
  cd(0:1, 158) = [57.0000000,1.0412517]
  cd(0:1, 159) = [60.0000000,1.1106013]
  cd(0:1, 160) = [63.0000000,1.1666158]
  cd(0:1, 161) = [66.0000000,1.2226302]
  cd(0:1, 162) = [69.0000000,1.2786446]
  cd(0:1, 163) = [72.0000000,1.3211469]
  cd(0:1, 164) = [75.0000000,1.3568934]
  cd(0:1, 165) = [78.0000000,1.3926402]
  cd(0:1, 166) = [81.0000000,1.4201302]
  cd(0:1, 167) = [84.0000000,1.4311067]
  cd(0:1, 168) = [87.0000000,1.4420834]
  cd(0:1, 169) = [90.0000000,1.4530599]
  cd(0:1, 170) = [93.0000000,1.4420834]
  cd(0:1, 171) = [96.0000000,1.4311068]
  cd(0:1, 172) = [99.0000000,1.4201301]
  cd(0:1, 173) = [102.0000000,1.3926401]
  cd(0:1, 174) = [105.0000000,1.3568936]
  cd(0:1, 175) = [108.0000000,1.3211467]
  cd(0:1, 176) = [111.0000000,1.2786441]
  cd(0:1, 177) = [114.0000000,1.2226303]
  cd(0:1, 178) = [117.0000000,1.1666162]
  cd(0:1, 179) = [120.0000000,1.1106017]
  cd(0:1, 180) = [123.0000000,1.0412521]
  cd(0:1, 181) = [126.0000000,0.9719023]
  cd(0:1, 182) = [129.0000000,0.9025518]
  cd(0:1, 183) = [132.0000000,0.8299963]
  cd(0:1, 184) = [135.0000000,0.7558384]
  cd(0:1, 185) = [138.0000000,0.6816807]
  cd(0:1, 186) = [141.0000000,0.6089478]
  cd(0:1, 187) = [144.0000000,0.5390643]
  cd(0:1, 188) = [147.0000000,0.4691810]
  cd(0:1, 189) = [150.0000000,0.3992978]
  cd(0:1, 190) = [153.0000000,0.3466477]
  cd(0:1, 191) = [156.0000000,0.2939977]
  cd(0:1, 192) = [159.0000000,0.2413476]
  cd(0:1, 193) = [162.0000000,0.2012423]
  cd(0:1, 194) = [165.0000000,0.1674093]
  cd(0:1, 195) = [168.0000000,0.1335762]
  cd(0:1, 196) = [171.0000000,0.0999186]
  cd(0:1, 197) = [174.0000000,0.0666123]
  cd(0:1, 198) = [177.0000000,0.0550240]
  cd(0:1, 199) = [180.0000000,0.0543029]

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
!     PRINT*,'alpha < tabulated range in calc_cd22_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd22 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd22_ge28.pro'
  ENDIF

!print,y,cd22

!_________________________________________________________________________
  
END FUNCTION calc_cd22_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd23_ge28( y ) RESULT( cd23 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd23
  INTEGER :: i

  cd23 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.0507557]
  cd(0:1, 1  ) = [-177.0000000,0.0507557]
  cd(0:1, 2  ) = [-174.0000000,0.0609068]
  cd(0:1, 3  ) = [-171.0000000,0.0913603]
  cd(0:1, 4  ) = [-168.0000000,0.1231809]
  cd(0:1, 5  ) = [-165.0000000,0.1556849]
  cd(0:1, 6  ) = [-162.0000000,0.1881888]
  cd(0:1, 7  ) = [-159.0000000,0.2281151]
  cd(0:1, 8  ) = [-156.0000000,0.2828858]
  cd(0:1, 9  ) = [-153.0000000,0.3376565]
  cd(0:1, 10 ) = [-150.0000000,0.3924272]
  cd(0:1, 11 ) = [-147.0000000,0.4625483]
  cd(0:1, 12 ) = [-144.0000000,0.5326695]
  cd(0:1, 13 ) = [-141.0000000,0.6027910]
  cd(0:1, 14 ) = [-138.0000000,0.6757987]
  cd(0:1, 15 ) = [-135.0000000,0.7502497]
  cd(0:1, 16 ) = [-132.0000000,0.8247011]
  cd(0:1, 17 ) = [-129.0000000,0.8975655]
  cd(0:1, 18 ) = [-126.0000000,0.9672559]
  cd(0:1, 19 ) = [-123.0000000,1.0369457]
  cd(0:1, 20 ) = [-120.0000000,1.1066352]
  cd(0:1, 21 ) = [-117.0000000,1.1630258]
  cd(0:1, 22 ) = [-114.0000000,1.2194160]
  cd(0:1, 23 ) = [-111.0000000,1.2758059]
  cd(0:1, 24 ) = [-108.0000000,1.3187007]
  cd(0:1, 25 ) = [-105.0000000,1.3548479]
  cd(0:1, 26 ) = [-102.0000000,1.3909947]
  cd(0:1, 27 ) = [-99.0000000,1.4188894]
  cd(0:1, 28 ) = [-96.0000000,1.4302797]
  cd(0:1, 29 ) = [-93.0000000,1.4416698]
  cd(0:1, 30 ) = [-90.0000000,1.4530599]
  cd(0:1, 31 ) = [-87.0000000,1.4416698]
  cd(0:1, 32 ) = [-84.0000000,1.4302796]
  cd(0:1, 33 ) = [-81.0000000,1.4188895]
  cd(0:1, 34 ) = [-78.0000000,1.3909949]
  cd(0:1, 35 ) = [-75.0000000,1.3548477]
  cd(0:1, 36 ) = [-72.0000000,1.3187009]
  cd(0:1, 37 ) = [-69.0000000,1.2758064]
  cd(0:1, 38 ) = [-66.0000000,1.2194159]
  cd(0:1, 39 ) = [-63.0000000,1.1630254]
  cd(0:1, 40 ) = [-60.0000000,1.1066348]
  cd(0:1, 41 ) = [-57.0000000,1.0369453]
  cd(0:1, 42 ) = [-54.0000000,0.9672555]
  cd(0:1, 43 ) = [-51.0000000,0.8975650]
  cd(0:1, 44 ) = [-48.0000000,0.8247371]
  cd(0:1, 45 ) = [-45.0000000,0.7503405]
  cd(0:1, 46 ) = [-42.0000000,0.6758348]
  cd(0:1, 47 ) = [-39.0000000,0.6017331]
  cd(0:1, 48 ) = [-36.0000000,0.5292615]
  cd(0:1, 49 ) = [-33.0000000,0.4594405]
  cd(0:1, 50 ) = [-30.0000000,0.3924268]
  cd(0:1, 51 ) = [-29.3939394,0.3795678]
  cd(0:1, 52 ) = [-28.7878788,0.3667088]
  cd(0:1, 53 ) = [-28.1818182,0.3538498]
  cd(0:1, 54 ) = [-27.5757576,0.3414027]
  cd(0:1, 55 ) = [-26.9696970,0.3291321]
  cd(0:1, 56 ) = [-26.3636364,0.3168615]
  cd(0:1, 57 ) = [-25.7575758,0.3048508]
  cd(0:1, 58 ) = [-25.1515152,0.2932296]
  cd(0:1, 59 ) = [-24.5454545,0.2816085]
  cd(0:1, 60 ) = [-23.9393939,0.2700527]
  cd(0:1, 61 ) = [-23.3333333,0.2590847]
  cd(0:1, 62 ) = [-22.7272727,0.2481167]
  cd(0:1, 63 ) = [-22.1212121,0.2371488]
  cd(0:1, 64 ) = [-21.5151515,0.2262129]
  cd(0:1, 65 ) = [-20.9090909,0.2152850]
  cd(0:1, 66 ) = [-20.3030303,0.2043571]
  cd(0:1, 67 ) = [-19.6969697,0.1935262]
  cd(0:1, 68 ) = [-19.0909091,0.1827921]
  cd(0:1, 69 ) = [-18.4848485,0.1717632]
  cd(0:1, 70 ) = [-17.8787879,0.1602327]
  cd(0:1, 71 ) = [-17.2727273,0.1493104]
  cd(0:1, 72 ) = [-16.6666667,0.1386315]
  cd(0:1, 73 ) = [-16.0606061,0.1281572]
  cd(0:1, 74 ) = [-15.4545455,0.1180322]
  cd(0:1, 75 ) = [-14.8484848,0.1081040]
  cd(0:1, 76 ) = [-14.2424242,0.0984661]
  cd(0:1, 77 ) = [-13.6363636,0.0888897]
  cd(0:1, 78 ) = [-13.0303030,0.0793654]
  cd(0:1, 79 ) = [-12.4242424,0.0691821]
  cd(0:1, 80 ) = [-11.8181818,0.0598196]
  cd(0:1, 81 ) = [-11.2121212,0.0494940]
  cd(0:1, 82 ) = [-10.6060606,0.0403354]
  cd(0:1, 83 ) = [-10.0000000,0.0320698]
  cd(0:1, 84 ) = [-9.3939394,0.0253011]
  cd(0:1, 85 ) = [-8.7878788,0.0191495]
  cd(0:1, 86 ) = [-8.1818182,0.0137668]
  cd(0:1, 87 ) = [-7.5757576,0.0108228]
  cd(0:1, 88 ) = [-6.9696970,0.0093916]
  cd(0:1, 89 ) = [-6.3636364,0.0082868]
  cd(0:1, 90 ) = [-5.7575758,0.0077614]
  cd(0:1, 91 ) = [-5.1515152,0.0074754]
  cd(0:1, 92 ) = [-4.5454545,0.0072862]
  cd(0:1, 93 ) = [-3.9393939,0.0071709]
  cd(0:1, 94 ) = [-3.3333333,0.0070539]
  cd(0:1, 95 ) = [-2.7272727,0.0069363]
  cd(0:1, 96 ) = [-2.1212121,0.0068633]
  cd(0:1, 97 ) = [-1.5151515,0.0068302]
  cd(0:1, 98 ) = [-0.9090909,0.0068253]
  cd(0:1, 99 ) = [-0.3030303,0.0068888]
  cd(0:1, 100) = [0.3030303,0.0069331]
  cd(0:1, 101) = [0.9090909,0.0069264]
  cd(0:1, 102) = [1.5151515,0.0069715]
  cd(0:1, 103) = [2.1212121,0.0070850]
  cd(0:1, 104) = [2.7272727,0.0072281]
  cd(0:1, 105) = [3.3333333,0.0073639]
  cd(0:1, 106) = [3.9393939,0.0075080]
  cd(0:1, 107) = [4.5454545,0.0076803]
  cd(0:1, 108) = [5.1515152,0.0078825]
  cd(0:1, 109) = [5.7575758,0.0081856]
  cd(0:1, 110) = [6.3636364,0.0086108]
  cd(0:1, 111) = [6.9696970,0.0092615]
  cd(0:1, 112) = [7.5757576,0.0106712]
  cd(0:1, 113) = [8.1818182,0.0120457]
  cd(0:1, 114) = [8.7878788,0.0140141]
  cd(0:1, 115) = [9.3939394,0.0210604]
  cd(0:1, 116) = [10.0000000,0.0308302]
  cd(0:1, 117) = [10.6060606,0.0395954]
  cd(0:1, 118) = [11.2121212,0.0468596]
  cd(0:1, 119) = [11.8181818,0.0534209]
  cd(0:1, 120) = [12.4242424,0.0631476]
  cd(0:1, 121) = [13.0303030,0.0734241]
  cd(0:1, 122) = [13.6363636,0.0854973]
  cd(0:1, 123) = [14.2424242,0.0967530]
  cd(0:1, 124) = [14.8484848,0.1067833]
  cd(0:1, 125) = [15.4545455,0.1156761]
  cd(0:1, 126) = [16.0606061,0.1240886]
  cd(0:1, 127) = [16.6666667,0.1315884]
  cd(0:1, 128) = [17.2727273,0.1402309]
  cd(0:1, 129) = [17.8787879,0.1502700]
  cd(0:1, 130) = [18.4848485,0.1613752]
  cd(0:1, 131) = [19.0909091,0.1726884]
  cd(0:1, 132) = [19.6969697,0.1836712]
  cd(0:1, 133) = [20.3030303,0.1959313]
  cd(0:1, 134) = [20.9090909,0.2094690]
  cd(0:1, 135) = [21.5151515,0.2230067]
  cd(0:1, 136) = [22.1212121,0.2360981]
  cd(0:1, 137) = [22.7272727,0.2474050]
  cd(0:1, 138) = [23.3333333,0.2587119]
  cd(0:1, 139) = [23.9393939,0.2700188]
  cd(0:1, 140) = [24.5454545,0.2816085]
  cd(0:1, 141) = [25.1515152,0.2932296]
  cd(0:1, 142) = [25.7575758,0.3048508]
  cd(0:1, 143) = [26.3636364,0.3168615]
  cd(0:1, 144) = [26.9696970,0.3291321]
  cd(0:1, 145) = [27.5757576,0.3414027]
  cd(0:1, 146) = [28.1818182,0.3538498]
  cd(0:1, 147) = [28.7878788,0.3667088]
  cd(0:1, 148) = [29.3939394,0.3795678]
  cd(0:1, 149) = [30.0000000,0.3924268]
  cd(0:1, 150) = [33.0000000,0.4594405]
  cd(0:1, 151) = [36.0000000,0.5292615]
  cd(0:1, 152) = [39.0000000,0.6017331]
  cd(0:1, 153) = [42.0000000,0.6758348]
  cd(0:1, 154) = [45.0000000,0.7503405]
  cd(0:1, 155) = [48.0000000,0.8247371]
  cd(0:1, 156) = [51.0000000,0.8975650]
  cd(0:1, 157) = [54.0000000,0.9672555]
  cd(0:1, 158) = [57.0000000,1.0369453]
  cd(0:1, 159) = [60.0000000,1.1066348]
  cd(0:1, 160) = [63.0000000,1.1630254]
  cd(0:1, 161) = [66.0000000,1.2194159]
  cd(0:1, 162) = [69.0000000,1.2758064]
  cd(0:1, 163) = [72.0000000,1.3187009]
  cd(0:1, 164) = [75.0000000,1.3548477]
  cd(0:1, 165) = [78.0000000,1.3909949]
  cd(0:1, 166) = [81.0000000,1.4188895]
  cd(0:1, 167) = [84.0000000,1.4302796]
  cd(0:1, 168) = [87.0000000,1.4416698]
  cd(0:1, 169) = [90.0000000,1.4530599]
  cd(0:1, 170) = [93.0000000,1.4416698]
  cd(0:1, 171) = [96.0000000,1.4302797]
  cd(0:1, 172) = [99.0000000,1.4188894]
  cd(0:1, 173) = [102.0000000,1.3909947]
  cd(0:1, 174) = [105.0000000,1.3548479]
  cd(0:1, 175) = [108.0000000,1.3187007]
  cd(0:1, 176) = [111.0000000,1.2758059]
  cd(0:1, 177) = [114.0000000,1.2194160]
  cd(0:1, 178) = [117.0000000,1.1630258]
  cd(0:1, 179) = [120.0000000,1.1066352]
  cd(0:1, 180) = [123.0000000,1.0369457]
  cd(0:1, 181) = [126.0000000,0.9672559]
  cd(0:1, 182) = [129.0000000,0.8975655]
  cd(0:1, 183) = [132.0000000,0.8247011]
  cd(0:1, 184) = [135.0000000,0.7502497]
  cd(0:1, 185) = [138.0000000,0.6757987]
  cd(0:1, 186) = [141.0000000,0.6027910]
  cd(0:1, 187) = [144.0000000,0.5326695]
  cd(0:1, 188) = [147.0000000,0.4625483]
  cd(0:1, 189) = [150.0000000,0.3924272]
  cd(0:1, 190) = [153.0000000,0.3386041]
  cd(0:1, 191) = [156.0000000,0.2847811]
  cd(0:1, 192) = [159.0000000,0.2309580]
  cd(0:1, 193) = [162.0000000,0.1912237]
  cd(0:1, 194) = [165.0000000,0.1585339]
  cd(0:1, 195) = [168.0000000,0.1258439]
  cd(0:1, 196) = [171.0000000,0.0936455]
  cd(0:1, 197) = [174.0000000,0.0624303]
  cd(0:1, 198) = [177.0000000,0.0515140]
  cd(0:1, 199) = [180.0000000,0.0507557]

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
!     PRINT*,'alpha < tabulated range in calc_cd23_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd23 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd23_ge28.pro'
  ENDIF

!print,y,cd23

!_________________________________________________________________________
  
END FUNCTION calc_cd23_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd24_ge28( y ) RESULT( cd24 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd24
  INTEGER :: i

  cd24 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.0463087]
  cd(0:1, 1  ) = [-177.0000000,0.0463087]
  cd(0:1, 2  ) = [-174.0000000,0.0555704]
  cd(0:1, 3  ) = [-171.0000000,0.0833558]
  cd(0:1, 4  ) = [-168.0000000,0.1135623]
  cd(0:1, 5  ) = [-165.0000000,0.1449793]
  cd(0:1, 6  ) = [-162.0000000,0.1763964]
  cd(0:1, 7  ) = [-159.0000000,0.2159609]
  cd(0:1, 8  ) = [-156.0000000,0.2718202]
  cd(0:1, 9  ) = [-153.0000000,0.3276795]
  cd(0:1, 10 ) = [-150.0000000,0.3835389]
  cd(0:1, 11 ) = [-147.0000000,0.4539677]
  cd(0:1, 12 ) = [-144.0000000,0.5243967]
  cd(0:1, 13 ) = [-141.0000000,0.5948261]
  cd(0:1, 14 ) = [-138.0000000,0.6681894]
  cd(0:1, 15 ) = [-135.0000000,0.7430198]
  cd(0:1, 16 ) = [-132.0000000,0.8178507]
  cd(0:1, 17 ) = [-129.0000000,0.8911147]
  cd(0:1, 18 ) = [-126.0000000,0.9612451]
  cd(0:1, 19 ) = [-123.0000000,1.0313746]
  cd(0:1, 20 ) = [-120.0000000,1.1015038]
  cd(0:1, 21 ) = [-117.0000000,1.1583808]
  cd(0:1, 22 ) = [-114.0000000,1.2152577]
  cd(0:1, 23 ) = [-111.0000000,1.2721341]
  cd(0:1, 24 ) = [-108.0000000,1.3155364]
  cd(0:1, 25 ) = [-105.0000000,1.3522015]
  cd(0:1, 26 ) = [-102.0000000,1.3888661]
  cd(0:1, 27 ) = [-99.0000000,1.4172844]
  cd(0:1, 28 ) = [-96.0000000,1.4292097]
  cd(0:1, 29 ) = [-93.0000000,1.4411348]
  cd(0:1, 30 ) = [-90.0000000,1.4530599]
  cd(0:1, 31 ) = [-87.0000000,1.4411348]
  cd(0:1, 32 ) = [-84.0000000,1.4292096]
  cd(0:1, 33 ) = [-81.0000000,1.4172845]
  cd(0:1, 34 ) = [-78.0000000,1.3888663]
  cd(0:1, 35 ) = [-75.0000000,1.3522012]
  cd(0:1, 36 ) = [-72.0000000,1.3155365]
  cd(0:1, 37 ) = [-69.0000000,1.2721346]
  cd(0:1, 38 ) = [-66.0000000,1.2152575]
  cd(0:1, 39 ) = [-63.0000000,1.1583805]
  cd(0:1, 40 ) = [-60.0000000,1.1015033]
  cd(0:1, 41 ) = [-57.0000000,1.0313741]
  cd(0:1, 42 ) = [-54.0000000,0.9612446]
  cd(0:1, 43 ) = [-51.0000000,0.8911143]
  cd(0:1, 44 ) = [-48.0000000,0.8178758]
  cd(0:1, 45 ) = [-45.0000000,0.7430833]
  cd(0:1, 46 ) = [-42.0000000,0.6682145]
  cd(0:1, 47 ) = [-39.0000000,0.5937582]
  cd(0:1, 48 ) = [-36.0000000,0.5209583]
  cd(0:1, 49 ) = [-33.0000000,0.4508343]
  cd(0:1, 50 ) = [-30.0000000,0.3835385]
  cd(0:1, 51 ) = [-29.3939394,0.3706269]
  cd(0:1, 52 ) = [-28.7878788,0.3577153]
  cd(0:1, 53 ) = [-28.1818182,0.3448037]
  cd(0:1, 54 ) = [-27.5757576,0.3323063]
  cd(0:1, 55 ) = [-26.9696970,0.3199864]
  cd(0:1, 56 ) = [-26.3636364,0.3076666]
  cd(0:1, 57 ) = [-25.7575758,0.2956078]
  cd(0:1, 58 ) = [-25.1515152,0.2839408]
  cd(0:1, 59 ) = [-24.5454545,0.2722737]
  cd(0:1, 60 ) = [-23.9393939,0.2606741]
  cd(0:1, 61 ) = [-23.3333333,0.2496810]
  cd(0:1, 62 ) = [-22.7272727,0.2386879]
  cd(0:1, 63 ) = [-22.1212121,0.2276948]
  cd(0:1, 64 ) = [-21.5151515,0.2168220]
  cd(0:1, 65 ) = [-20.9090909,0.2059792]
  cd(0:1, 66 ) = [-20.3030303,0.1951365]
  cd(0:1, 67 ) = [-19.6969697,0.1844652]
  cd(0:1, 68 ) = [-19.0909091,0.1739652]
  cd(0:1, 69 ) = [-18.4848485,0.1634537]
  cd(0:1, 70 ) = [-17.8787879,0.1528042]
  cd(0:1, 71 ) = [-17.2727273,0.1428666]
  cd(0:1, 72 ) = [-16.6666667,0.1330560]
  cd(0:1, 73 ) = [-16.0606061,0.1233522]
  cd(0:1, 74 ) = [-15.4545455,0.1138921]
  cd(0:1, 75 ) = [-14.8484848,0.1045721]
  cd(0:1, 76 ) = [-14.2424242,0.0954961]
  cd(0:1, 77 ) = [-13.6363636,0.0864651]
  cd(0:1, 78 ) = [-13.0303030,0.0774697]
  cd(0:1, 79 ) = [-12.4242424,0.0682294]
  cd(0:1, 80 ) = [-11.8181818,0.0596868]
  cd(0:1, 81 ) = [-11.2121212,0.0499976]
  cd(0:1, 82 ) = [-10.6060606,0.0411158]
  cd(0:1, 83 ) = [-10.0000000,0.0329149]
  cd(0:1, 84 ) = [-9.3939394,0.0258543]
  cd(0:1, 85 ) = [-8.7878788,0.0193849]
  cd(0:1, 86 ) = [-8.1818182,0.0136624]
  cd(0:1, 87 ) = [-7.5757576,0.0105717]
  cd(0:1, 88 ) = [-6.9696970,0.0091073]
  cd(0:1, 89 ) = [-6.3636364,0.0079016]
  cd(0:1, 90 ) = [-5.7575758,0.0074196]
  cd(0:1, 91 ) = [-5.1515152,0.0071472]
  cd(0:1, 92 ) = [-4.5454545,0.0069524]
  cd(0:1, 93 ) = [-3.9393939,0.0068463]
  cd(0:1, 94 ) = [-3.3333333,0.0067615]
  cd(0:1, 95 ) = [-2.7272727,0.0066633]
  cd(0:1, 96 ) = [-2.1212121,0.0066158]
  cd(0:1, 97 ) = [-1.5151515,0.0065955]
  cd(0:1, 98 ) = [-0.9090909,0.0065940]
  cd(0:1, 99 ) = [-0.3030303,0.0066494]
  cd(0:1, 100) = [0.3030303,0.0066982]
  cd(0:1, 101) = [0.9090909,0.0066918]
  cd(0:1, 102) = [1.5151515,0.0067552]
  cd(0:1, 103) = [2.1212121,0.0068416]
  cd(0:1, 104) = [2.7272727,0.0069768]
  cd(0:1, 105) = [3.3333333,0.0071074]
  cd(0:1, 106) = [3.9393939,0.0072442]
  cd(0:1, 107) = [4.5454545,0.0073998]
  cd(0:1, 108) = [5.1515152,0.0075856]
  cd(0:1, 109) = [5.7575758,0.0078865]
  cd(0:1, 110) = [6.3636364,0.0083415]
  cd(0:1, 111) = [6.9696970,0.0091049]
  cd(0:1, 112) = [7.5757576,0.0106845]
  cd(0:1, 113) = [8.1818182,0.0121821]
  cd(0:1, 114) = [8.7878788,0.0140256]
  cd(0:1, 115) = [9.3939394,0.0192335]
  cd(0:1, 116) = [10.0000000,0.0262062]
  cd(0:1, 117) = [10.6060606,0.0326764]
  cd(0:1, 118) = [11.2121212,0.0383098]
  cd(0:1, 119) = [11.8181818,0.0460810]
  cd(0:1, 120) = [12.4242424,0.0579457]
  cd(0:1, 121) = [13.0303030,0.0700497]
  cd(0:1, 122) = [13.6363636,0.0836737]
  cd(0:1, 123) = [14.2424242,0.0956164]
  cd(0:1, 124) = [14.8484848,0.1050385]
  cd(0:1, 125) = [15.4545455,0.1136941]
  cd(0:1, 126) = [16.0606061,0.1220252]
  cd(0:1, 127) = [16.6666667,0.1297332]
  cd(0:1, 128) = [17.2727273,0.1382049]
  cd(0:1, 129) = [17.8787879,0.1476101]
  cd(0:1, 130) = [18.4848485,0.1576733]
  cd(0:1, 131) = [19.0909091,0.1679403]
  cd(0:1, 132) = [19.6969697,0.1784305]
  cd(0:1, 133) = [20.3030303,0.1898976]
  cd(0:1, 134) = [20.9090909,0.2023417]
  cd(0:1, 135) = [21.5151515,0.2147859]
  cd(0:1, 136) = [22.1212121,0.2269855]
  cd(0:1, 137) = [22.7272727,0.2382074]
  cd(0:1, 138) = [23.3333333,0.2494293]
  cd(0:1, 139) = [23.9393939,0.2606512]
  cd(0:1, 140) = [24.5454545,0.2722737]
  cd(0:1, 141) = [25.1515152,0.2839408]
  cd(0:1, 142) = [25.7575758,0.2956078]
  cd(0:1, 143) = [26.3636364,0.3076666]
  cd(0:1, 144) = [26.9696970,0.3199864]
  cd(0:1, 145) = [27.5757576,0.3323063]
  cd(0:1, 146) = [28.1818182,0.3448037]
  cd(0:1, 147) = [28.7878788,0.3577153]
  cd(0:1, 148) = [29.3939394,0.3706269]
  cd(0:1, 149) = [30.0000000,0.3835385]
  cd(0:1, 150) = [33.0000000,0.4508343]
  cd(0:1, 151) = [36.0000000,0.5209583]
  cd(0:1, 152) = [39.0000000,0.5937582]
  cd(0:1, 153) = [42.0000000,0.6682145]
  cd(0:1, 154) = [45.0000000,0.7430833]
  cd(0:1, 155) = [48.0000000,0.8178758]
  cd(0:1, 156) = [51.0000000,0.8911143]
  cd(0:1, 157) = [54.0000000,0.9612446]
  cd(0:1, 158) = [57.0000000,1.0313741]
  cd(0:1, 159) = [60.0000000,1.1015033]
  cd(0:1, 160) = [63.0000000,1.1583805]
  cd(0:1, 161) = [66.0000000,1.2152575]
  cd(0:1, 162) = [69.0000000,1.2721346]
  cd(0:1, 163) = [72.0000000,1.3155365]
  cd(0:1, 164) = [75.0000000,1.3522012]
  cd(0:1, 165) = [78.0000000,1.3888663]
  cd(0:1, 166) = [81.0000000,1.4172845]
  cd(0:1, 167) = [84.0000000,1.4292096]
  cd(0:1, 168) = [87.0000000,1.4411348]
  cd(0:1, 169) = [90.0000000,1.4530599]
  cd(0:1, 170) = [93.0000000,1.4411348]
  cd(0:1, 171) = [96.0000000,1.4292097]
  cd(0:1, 172) = [99.0000000,1.4172844]
  cd(0:1, 173) = [102.0000000,1.3888661]
  cd(0:1, 174) = [105.0000000,1.3522015]
  cd(0:1, 175) = [108.0000000,1.3155364]
  cd(0:1, 176) = [111.0000000,1.2721341]
  cd(0:1, 177) = [114.0000000,1.2152577]
  cd(0:1, 178) = [117.0000000,1.1583808]
  cd(0:1, 179) = [120.0000000,1.1015038]
  cd(0:1, 180) = [123.0000000,1.0313746]
  cd(0:1, 181) = [126.0000000,0.9612451]
  cd(0:1, 182) = [129.0000000,0.8911147]
  cd(0:1, 183) = [132.0000000,0.8178507]
  cd(0:1, 184) = [135.0000000,0.7430198]
  cd(0:1, 185) = [138.0000000,0.6681894]
  cd(0:1, 186) = [141.0000000,0.5948261]
  cd(0:1, 187) = [144.0000000,0.5243967]
  cd(0:1, 188) = [147.0000000,0.4539677]
  cd(0:1, 189) = [150.0000000,0.3835389]
  cd(0:1, 190) = [153.0000000,0.3282797]
  cd(0:1, 191) = [156.0000000,0.2730206]
  cd(0:1, 192) = [159.0000000,0.2177614]
  cd(0:1, 193) = [162.0000000,0.1785368]
  cd(0:1, 194) = [165.0000000,0.1473297]
  cd(0:1, 195) = [168.0000000,0.1161224]
  cd(0:1, 196) = [171.0000000,0.0857857]
  cd(0:1, 197) = [174.0000000,0.0571905]
  cd(0:1, 198) = [177.0000000,0.0471151]
  cd(0:1, 199) = [180.0000000,0.0463087]

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
!     PRINT*,'alpha < tabulated range in calc_cd24_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd24 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd24_ge28.pro'
  ENDIF

!print,y,cd24

!_________________________________________________________________________
  
END FUNCTION calc_cd24_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd25_ge28( y ) RESULT( cd25 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd25
  INTEGER :: i

  cd25 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.0420239]
  cd(0:1, 1  ) = [-177.0000000,0.0420239]
  cd(0:1, 2  ) = [-174.0000000,0.0504287]
  cd(0:1, 3  ) = [-171.0000000,0.0756432]
  cd(0:1, 4  ) = [-168.0000000,0.1042457]
  cd(0:1, 5  ) = [-165.0000000,0.1345422]
  cd(0:1, 6  ) = [-162.0000000,0.1648389]
  cd(0:1, 7  ) = [-159.0000000,0.2040022]
  cd(0:1, 8  ) = [-156.0000000,0.2608985]
  cd(0:1, 9  ) = [-153.0000000,0.3177948]
  cd(0:1, 10 ) = [-150.0000000,0.3746911]
  cd(0:1, 11 ) = [-147.0000000,0.4454263]
  cd(0:1, 12 ) = [-144.0000000,0.5161617]
  cd(0:1, 13 ) = [-141.0000000,0.5868975]
  cd(0:1, 14 ) = [-138.0000000,0.6606148]
  cd(0:1, 15 ) = [-135.0000000,0.7358228]
  cd(0:1, 16 ) = [-132.0000000,0.8110316]
  cd(0:1, 17 ) = [-129.0000000,0.8846934]
  cd(0:1, 18 ) = [-126.0000000,0.9552616]
  cd(0:1, 19 ) = [-123.0000000,1.0258288]
  cd(0:1, 20 ) = [-120.0000000,1.0963956]
  cd(0:1, 21 ) = [-117.0000000,1.1537570]
  cd(0:1, 22 ) = [-114.0000000,1.2111182]
  cd(0:1, 23 ) = [-111.0000000,1.2684790]
  cd(0:1, 24 ) = [-108.0000000,1.3123864]
  cd(0:1, 25 ) = [-105.0000000,1.3495670]
  cd(0:1, 26 ) = [-102.0000000,1.3867472]
  cd(0:1, 27 ) = [-99.0000000,1.4156868]
  cd(0:1, 28 ) = [-96.0000000,1.4281446]
  cd(0:1, 29 ) = [-93.0000000,1.4406023]
  cd(0:1, 30 ) = [-90.0000000,1.4530599]
  cd(0:1, 31 ) = [-87.0000000,1.4406023]
  cd(0:1, 32 ) = [-84.0000000,1.4281445]
  cd(0:1, 33 ) = [-81.0000000,1.4156869]
  cd(0:1, 34 ) = [-78.0000000,1.3867474]
  cd(0:1, 35 ) = [-75.0000000,1.3495668]
  cd(0:1, 36 ) = [-72.0000000,1.3123866]
  cd(0:1, 37 ) = [-69.0000000,1.2684795]
  cd(0:1, 38 ) = [-66.0000000,1.2111181]
  cd(0:1, 39 ) = [-63.0000000,1.1537567]
  cd(0:1, 40 ) = [-60.0000000,1.0963952]
  cd(0:1, 41 ) = [-57.0000000,1.0258283]
  cd(0:1, 42 ) = [-54.0000000,0.9552611]
  cd(0:1, 43 ) = [-51.0000000,0.8846930]
  cd(0:1, 44 ) = [-48.0000000,0.8110458]
  cd(0:1, 45 ) = [-45.0000000,0.7358592]
  cd(0:1, 46 ) = [-42.0000000,0.6606290]
  cd(0:1, 47 ) = [-39.0000000,0.5858196]
  cd(0:1, 48 ) = [-36.0000000,0.5126929]
  cd(0:1, 49 ) = [-33.0000000,0.4422673]
  cd(0:1, 50 ) = [-30.0000000,0.3746907]
  cd(0:1, 51 ) = [-29.3939394,0.3617267]
  cd(0:1, 52 ) = [-28.7878788,0.3487628]
  cd(0:1, 53 ) = [-28.1818182,0.3357988]
  cd(0:1, 54 ) = [-27.5757576,0.3232514]
  cd(0:1, 55 ) = [-26.9696970,0.3108824]
  cd(0:1, 56 ) = [-26.3636364,0.2985135]
  cd(0:1, 57 ) = [-25.7575758,0.2864070]
  cd(0:1, 58 ) = [-25.1515152,0.2746943]
  cd(0:1, 59 ) = [-24.5454545,0.2629816]
  cd(0:1, 60 ) = [-23.9393939,0.2513383]
  cd(0:1, 61 ) = [-23.3333333,0.2403204]
  cd(0:1, 62 ) = [-22.7272727,0.2293026]
  cd(0:1, 63 ) = [-22.1212121,0.2182847]
  cd(0:1, 64 ) = [-21.5151515,0.2075774]
  cd(0:1, 65 ) = [-20.9090909,0.1969479]
  cd(0:1, 66 ) = [-20.3030303,0.1863183]
  cd(0:1, 67 ) = [-19.6969697,0.1759284]
  cd(0:1, 68 ) = [-19.0909091,0.1657783]
  cd(0:1, 69 ) = [-18.4848485,0.1558495]
  cd(0:1, 70 ) = [-17.8787879,0.1460769]
  cd(0:1, 71 ) = [-17.2727273,0.1371307]
  cd(0:1, 72 ) = [-16.6666667,0.1282206]
  cd(0:1, 73 ) = [-16.0606061,0.1193409]
  cd(0:1, 74 ) = [-15.4545455,0.1105796]
  cd(0:1, 75 ) = [-14.8484848,0.1018880]
  cd(0:1, 76 ) = [-14.2424242,0.0933388]
  cd(0:1, 77 ) = [-13.6363636,0.0848128]
  cd(0:1, 78 ) = [-13.0303030,0.0763034]
  cd(0:1, 79 ) = [-12.4242424,0.0677523]
  cd(0:1, 80 ) = [-11.8181818,0.0596236]
  cd(0:1, 81 ) = [-11.2121212,0.0506890]
  cd(0:1, 82 ) = [-10.6060606,0.0421875]
  cd(0:1, 83 ) = [-10.0000000,0.0340754]
  cd(0:1, 84 ) = [-9.3939394,0.0266139]
  cd(0:1, 85 ) = [-8.7878788,0.0197081]
  cd(0:1, 86 ) = [-8.1818182,0.0136122]
  cd(0:1, 87 ) = [-7.5757576,0.0104463]
  cd(0:1, 88 ) = [-6.9696970,0.0089622]
  cd(0:1, 89 ) = [-6.3636364,0.0076547]
  cd(0:1, 90 ) = [-5.7575758,0.0072003]
  cd(0:1, 91 ) = [-5.1515152,0.0069244]
  cd(0:1, 92 ) = [-4.5454545,0.0067118]
  cd(0:1, 93 ) = [-3.9393939,0.0066040]
  cd(0:1, 94 ) = [-3.3333333,0.0065457]
  cd(0:1, 95 ) = [-2.7272727,0.0064636]
  cd(0:1, 96 ) = [-2.1212121,0.0064395]
  cd(0:1, 97 ) = [-1.5151515,0.0064297]
  cd(0:1, 98 ) = [-0.9090909,0.0064297]
  cd(0:1, 99 ) = [-0.3030303,0.0064774]
  cd(0:1, 100) = [0.3030303,0.0065314]
  cd(0:1, 101) = [0.9090909,0.0065273]
  cd(0:1, 102) = [1.5151515,0.0066083]
  cd(0:1, 103) = [2.1212121,0.0066658]
  cd(0:1, 104) = [2.7272727,0.0067941]
  cd(0:1, 105) = [3.3333333,0.0069200]
  cd(0:1, 106) = [3.9393939,0.0070495]
  cd(0:1, 107) = [4.5454545,0.0071887]
  cd(0:1, 108) = [5.1515152,0.0073581]
  cd(0:1, 109) = [5.7575758,0.0076560]
  cd(0:1, 110) = [6.3636364,0.0081378]
  cd(0:1, 111) = [6.9696970,0.0090070]
  cd(0:1, 112) = [7.5757576,0.0107028]
  cd(0:1, 113) = [8.1818182,0.0123339]
  cd(0:1, 114) = [8.7878788,0.0140314]
  cd(0:1, 115) = [9.3939394,0.0173667]
  cd(0:1, 116) = [10.0000000,0.0215076]
  cd(0:1, 117) = [10.6060606,0.0256482]
  cd(0:1, 118) = [11.2121212,0.0296174]
  cd(0:1, 119) = [11.8181818,0.0384335]
  cd(0:1, 120) = [12.4242424,0.0524745]
  cd(0:1, 121) = [13.0303030,0.0665727]
  cd(0:1, 122) = [13.6363636,0.0816057]
  cd(0:1, 123) = [14.2424242,0.0941511]
  cd(0:1, 124) = [14.8484848,0.1029670]
  cd(0:1, 125) = [15.4545455,0.1113894]
  cd(0:1, 126) = [16.0606061,0.1196446]
  cd(0:1, 127) = [16.6666667,0.1275761]
  cd(0:1, 128) = [17.2727273,0.1358966]
  cd(0:1, 129) = [17.8787879,0.1446924]
  cd(0:1, 130) = [18.4848485,0.1537805]
  cd(0:1, 131) = [19.0909091,0.1630758]
  cd(0:1, 132) = [19.6969697,0.1731326]
  cd(0:1, 133) = [20.3030303,0.1838476]
  cd(0:1, 134) = [20.9090909,0.1952209]
  cd(0:1, 135) = [21.5151515,0.2065942]
  cd(0:1, 136) = [22.1212121,0.2179199]
  cd(0:1, 137) = [22.7272727,0.2290554]
  cd(0:1, 138) = [23.3333333,0.2401910]
  cd(0:1, 139) = [23.9393939,0.2513265]
  cd(0:1, 140) = [24.5454545,0.2629816]
  cd(0:1, 141) = [25.1515152,0.2746943]
  cd(0:1, 142) = [25.7575758,0.2864070]
  cd(0:1, 143) = [26.3636364,0.2985135]
  cd(0:1, 144) = [26.9696970,0.3108824]
  cd(0:1, 145) = [27.5757576,0.3232514]
  cd(0:1, 146) = [28.1818182,0.3357988]
  cd(0:1, 147) = [28.7878788,0.3487628]
  cd(0:1, 148) = [29.3939394,0.3617267]
  cd(0:1, 149) = [30.0000000,0.3746907]
  cd(0:1, 150) = [33.0000000,0.4422673]
  cd(0:1, 151) = [36.0000000,0.5126929]
  cd(0:1, 152) = [39.0000000,0.5858196]
  cd(0:1, 153) = [42.0000000,0.6606290]
  cd(0:1, 154) = [45.0000000,0.7358592]
  cd(0:1, 155) = [48.0000000,0.8110458]
  cd(0:1, 156) = [51.0000000,0.8846930]
  cd(0:1, 157) = [54.0000000,0.9552611]
  cd(0:1, 158) = [57.0000000,1.0258283]
  cd(0:1, 159) = [60.0000000,1.0963952]
  cd(0:1, 160) = [63.0000000,1.1537567]
  cd(0:1, 161) = [66.0000000,1.2111181]
  cd(0:1, 162) = [69.0000000,1.2684795]
  cd(0:1, 163) = [72.0000000,1.3123866]
  cd(0:1, 164) = [75.0000000,1.3495668]
  cd(0:1, 165) = [78.0000000,1.3867474]
  cd(0:1, 166) = [81.0000000,1.4156869]
  cd(0:1, 167) = [84.0000000,1.4281445]
  cd(0:1, 168) = [87.0000000,1.4406023]
  cd(0:1, 169) = [90.0000000,1.4530599]
  cd(0:1, 170) = [93.0000000,1.4406023]
  cd(0:1, 171) = [96.0000000,1.4281446]
  cd(0:1, 172) = [99.0000000,1.4156868]
  cd(0:1, 173) = [102.0000000,1.3867472]
  cd(0:1, 174) = [105.0000000,1.3495670]
  cd(0:1, 175) = [108.0000000,1.3123864]
  cd(0:1, 176) = [111.0000000,1.2684790]
  cd(0:1, 177) = [114.0000000,1.2111182]
  cd(0:1, 178) = [117.0000000,1.1537570]
  cd(0:1, 179) = [120.0000000,1.0963956]
  cd(0:1, 180) = [123.0000000,1.0258288]
  cd(0:1, 181) = [126.0000000,0.9552616]
  cd(0:1, 182) = [129.0000000,0.8846934]
  cd(0:1, 183) = [132.0000000,0.8110316]
  cd(0:1, 184) = [135.0000000,0.7358228]
  cd(0:1, 185) = [138.0000000,0.6606148]
  cd(0:1, 186) = [141.0000000,0.5868975]
  cd(0:1, 187) = [144.0000000,0.5161617]
  cd(0:1, 188) = [147.0000000,0.4454263]
  cd(0:1, 189) = [150.0000000,0.3746911]
  cd(0:1, 190) = [153.0000000,0.3180837]
  cd(0:1, 191) = [156.0000000,0.2614764]
  cd(0:1, 192) = [159.0000000,0.2048690]
  cd(0:1, 193) = [162.0000000,0.1661815]
  cd(0:1, 194) = [165.0000000,0.1364541]
  cd(0:1, 195) = [168.0000000,0.1067264]
  cd(0:1, 196) = [171.0000000,0.0782171]
  cd(0:1, 197) = [174.0000000,0.0521449]
  cd(0:1, 198) = [177.0000000,0.0428782]
  cd(0:1, 199) = [180.0000000,0.0420239]

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
!     PRINT*,'alpha < tabulated range in calc_cd25_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd25 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd25_ge28.pro'
  ENDIF

!print,y,cd25

!_________________________________________________________________________
  
END FUNCTION calc_cd25_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd26_ge28( y ) RESULT( cd26 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd26
  INTEGER :: i

  cd26 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.0388897]
  cd(0:1, 1  ) = [-177.0000000,0.0388897]
  cd(0:1, 2  ) = [-174.0000000,0.0466675]
  cd(0:1, 3  ) = [-171.0000000,0.0700015]
  cd(0:1, 4  ) = [-168.0000000,0.0973983]
  cd(0:1, 5  ) = [-165.0000000,0.1268267]
  cd(0:1, 6  ) = [-162.0000000,0.1562553]
  cd(0:1, 7  ) = [-159.0000000,0.1950901]
  cd(0:1, 8  ) = [-156.0000000,0.2527369]
  cd(0:1, 9  ) = [-153.0000000,0.3103839]
  cd(0:1, 10 ) = [-150.0000000,0.3680308]
  cd(0:1, 11 ) = [-147.0000000,0.4389966]
  cd(0:1, 12 ) = [-144.0000000,0.5099626]
  cd(0:1, 13 ) = [-141.0000000,0.5809292]
  cd(0:1, 14 ) = [-138.0000000,0.6549129]
  cd(0:1, 15 ) = [-135.0000000,0.7304053]
  cd(0:1, 16 ) = [-132.0000000,0.8058984]
  cd(0:1, 17 ) = [-129.0000000,0.8798597]
  cd(0:1, 18 ) = [-126.0000000,0.9507574]
  cd(0:1, 19 ) = [-123.0000000,1.0216541]
  cd(0:1, 20 ) = [-120.0000000,1.0925504]
  cd(0:1, 21 ) = [-117.0000000,1.1502764]
  cd(0:1, 22 ) = [-114.0000000,1.2080021]
  cd(0:1, 23 ) = [-111.0000000,1.2657275]
  cd(0:1, 24 ) = [-108.0000000,1.3100151]
  cd(0:1, 25 ) = [-105.0000000,1.3475839]
  cd(0:1, 26 ) = [-102.0000000,1.3851522]
  cd(0:1, 27 ) = [-99.0000000,1.4144841]
  cd(0:1, 28 ) = [-96.0000000,1.4273428]
  cd(0:1, 29 ) = [-93.0000000,1.4402014]
  cd(0:1, 30 ) = [-90.0000000,1.4530599]
  cd(0:1, 31 ) = [-87.0000000,1.4402014]
  cd(0:1, 32 ) = [-84.0000000,1.4273428]
  cd(0:1, 33 ) = [-81.0000000,1.4144842]
  cd(0:1, 34 ) = [-78.0000000,1.3851524]
  cd(0:1, 35 ) = [-75.0000000,1.3475837]
  cd(0:1, 36 ) = [-72.0000000,1.3100153]
  cd(0:1, 37 ) = [-69.0000000,1.2657280]
  cd(0:1, 38 ) = [-66.0000000,1.2080020]
  cd(0:1, 39 ) = [-63.0000000,1.1502760]
  cd(0:1, 40 ) = [-60.0000000,1.0925499]
  cd(0:1, 41 ) = [-57.0000000,1.0216536]
  cd(0:1, 42 ) = [-54.0000000,0.9507569]
  cd(0:1, 43 ) = [-51.0000000,0.8798593]
  cd(0:1, 44 ) = [-48.0000000,0.8059045]
  cd(0:1, 45 ) = [-45.0000000,0.7304211]
  cd(0:1, 46 ) = [-42.0000000,0.6549190]
  cd(0:1, 47 ) = [-39.0000000,0.5798437]
  cd(0:1, 48 ) = [-36.0000000,0.5064711]
  cd(0:1, 49 ) = [-33.0000000,0.4358184]
  cd(0:1, 50 ) = [-30.0000000,0.3680304]
  cd(0:1, 51 ) = [-29.3939394,0.3550270]
  cd(0:1, 52 ) = [-28.7878788,0.3420237]
  cd(0:1, 53 ) = [-28.1818182,0.3290203]
  cd(0:1, 54 ) = [-27.5757576,0.3164352]
  cd(0:1, 55 ) = [-26.9696970,0.3040293]
  cd(0:1, 56 ) = [-26.3636364,0.2916234]
  cd(0:1, 57 ) = [-25.7575758,0.2794810]
  cd(0:1, 58 ) = [-25.1515152,0.2677339]
  cd(0:1, 59 ) = [-24.5454545,0.2559868]
  cd(0:1, 60 ) = [-23.9393939,0.2443107]
  cd(0:1, 61 ) = [-23.3333333,0.2332743]
  cd(0:1, 62 ) = [-22.7272727,0.2222380]
  cd(0:1, 63 ) = [-22.1212121,0.2112016]
  cd(0:1, 64 ) = [-21.5151515,0.2006850]
  cd(0:1, 65 ) = [-20.9090909,0.1902984]
  cd(0:1, 66 ) = [-20.3030303,0.1799118]
  cd(0:1, 67 ) = [-19.6969697,0.1698126]
  cd(0:1, 68 ) = [-19.0909091,0.1600007]
  cd(0:1, 69 ) = [-18.4848485,0.1505222]
  cd(0:1, 70 ) = [-17.8787879,0.1413307]
  cd(0:1, 71 ) = [-17.2727273,0.1330816]
  cd(0:1, 72 ) = [-16.6666667,0.1248355]
  cd(0:1, 73 ) = [-16.0606061,0.1165919]
  cd(0:1, 74 ) = [-15.4545455,0.1083795]
  cd(0:1, 75 ) = [-14.8484848,0.1001855]
  cd(0:1, 76 ) = [-14.2424242,0.0920339]
  cd(0:1, 77 ) = [-13.6363636,0.0838889]
  cd(0:1, 78 ) = [-13.0303030,0.0757476]
  cd(0:1, 79 ) = [-12.4242424,0.0676111]
  cd(0:1, 80 ) = [-11.8181818,0.0596056]
  cd(0:1, 81 ) = [-11.2121212,0.0513317]
  cd(0:1, 82 ) = [-10.6060606,0.0431837]
  cd(0:1, 83 ) = [-10.0000000,0.0351540]
  cd(0:1, 84 ) = [-9.3939394,0.0273200]
  cd(0:1, 85 ) = [-8.7878788,0.0200085]
  cd(0:1, 86 ) = [-8.1818182,0.0135978]
  cd(0:1, 87 ) = [-7.5757576,0.0104092]
  cd(0:1, 88 ) = [-6.9696970,0.0089187]
  cd(0:1, 89 ) = [-6.3636364,0.0075285]
  cd(0:1, 90 ) = [-5.7575758,0.0070880]
  cd(0:1, 91 ) = [-5.1515152,0.0068005]
  cd(0:1, 92 ) = [-4.5454545,0.0065675]
  cd(0:1, 93 ) = [-3.9393939,0.0064530]
  cd(0:1, 94 ) = [-3.3333333,0.0064129]
  cd(0:1, 95 ) = [-2.7272727,0.0063420]
  cd(0:1, 96 ) = [-2.1212121,0.0063353]
  cd(0:1, 97 ) = [-1.5151515,0.0063326]
  cd(0:1, 98 ) = [-0.9090909,0.0063327]
  cd(0:1, 99 ) = [-0.3030303,0.0063745]
  cd(0:1, 100) = [0.3030303,0.0064331]
  cd(0:1, 101) = [0.9090909,0.0064318]
  cd(0:1, 102) = [1.5151515,0.0065264]
  cd(0:1, 103) = [2.1212121,0.0065603]
  cd(0:1, 104) = [2.7272727,0.0066835]
  cd(0:1, 105) = [3.3333333,0.0068060]
  cd(0:1, 106) = [3.9393939,0.0069296]
  cd(0:1, 107) = [4.5454545,0.0070560]
  cd(0:1, 108) = [5.1515152,0.0072126]
  cd(0:1, 109) = [5.7575758,0.0075077]
  cd(0:1, 110) = [6.3636364,0.0080092]
  cd(0:1, 111) = [6.9696970,0.0089591]
  cd(0:1, 112) = [7.5757576,0.0107199]
  cd(0:1, 113) = [8.1818182,0.0124584]
  cd(0:1, 114) = [8.7878788,0.0140332]
  cd(0:1, 115) = [9.3939394,0.0159306]
  cd(0:1, 116) = [10.0000000,0.0179092]
  cd(0:1, 117) = [10.6060606,0.0202674]
  cd(0:1, 118) = [11.2121212,0.0229573]
  cd(0:1, 119) = [11.8181818,0.0324577]
  cd(0:1, 120) = [12.4242424,0.0481678]
  cd(0:1, 121) = [13.0303030,0.0638795]
  cd(0:1, 122) = [13.6363636,0.0798866]
  cd(0:1, 123) = [14.2424242,0.0928336]
  cd(0:1, 124) = [14.8484848,0.1011927]
  cd(0:1, 125) = [15.4545455,0.1094412]
  cd(0:1, 126) = [16.0606061,0.1176428]
  cd(0:1, 127) = [16.6666667,0.1257530]
  cd(0:1, 128) = [17.2727273,0.1339717]
  cd(0:1, 129) = [17.8787879,0.1423228]
  cd(0:1, 130) = [18.4848485,0.1507166]
  cd(0:1, 131) = [19.0909091,0.1593254]
  cd(0:1, 132) = [19.6969697,0.1690924]
  cd(0:1, 133) = [20.3030303,0.1792652]
  cd(0:1, 134) = [20.9090909,0.1898438]
  cd(0:1, 135) = [21.5151515,0.2004224]
  cd(0:1, 136) = [22.1212121,0.2110992]
  cd(0:1, 137) = [22.7272727,0.2221686]
  cd(0:1, 138) = [23.3333333,0.2332380]
  cd(0:1, 139) = [23.9393939,0.2443074]
  cd(0:1, 140) = [24.5454545,0.2559868]
  cd(0:1, 141) = [25.1515152,0.2677339]
  cd(0:1, 142) = [25.7575758,0.2794810]
  cd(0:1, 143) = [26.3636364,0.2916234]
  cd(0:1, 144) = [26.9696970,0.3040293]
  cd(0:1, 145) = [27.5757576,0.3164352]
  cd(0:1, 146) = [28.1818182,0.3290203]
  cd(0:1, 147) = [28.7878788,0.3420237]
  cd(0:1, 148) = [29.3939394,0.3550270]
  cd(0:1, 149) = [30.0000000,0.3680304]
  cd(0:1, 150) = [33.0000000,0.4358184]
  cd(0:1, 151) = [36.0000000,0.5064711]
  cd(0:1, 152) = [39.0000000,0.5798437]
  cd(0:1, 153) = [42.0000000,0.6549190]
  cd(0:1, 154) = [45.0000000,0.7304211]
  cd(0:1, 155) = [48.0000000,0.8059045]
  cd(0:1, 156) = [51.0000000,0.8798593]
  cd(0:1, 157) = [54.0000000,0.9507569]
  cd(0:1, 158) = [57.0000000,1.0216536]
  cd(0:1, 159) = [60.0000000,1.0925499]
  cd(0:1, 160) = [63.0000000,1.1502760]
  cd(0:1, 161) = [66.0000000,1.2080020]
  cd(0:1, 162) = [69.0000000,1.2657280]
  cd(0:1, 163) = [72.0000000,1.3100153]
  cd(0:1, 164) = [75.0000000,1.3475837]
  cd(0:1, 165) = [78.0000000,1.3851524]
  cd(0:1, 166) = [81.0000000,1.4144842]
  cd(0:1, 167) = [84.0000000,1.4273428]
  cd(0:1, 168) = [87.0000000,1.4402014]
  cd(0:1, 169) = [90.0000000,1.4530599]
  cd(0:1, 170) = [93.0000000,1.4402014]
  cd(0:1, 171) = [96.0000000,1.4273428]
  cd(0:1, 172) = [99.0000000,1.4144841]
  cd(0:1, 173) = [102.0000000,1.3851522]
  cd(0:1, 174) = [105.0000000,1.3475839]
  cd(0:1, 175) = [108.0000000,1.3100151]
  cd(0:1, 176) = [111.0000000,1.2657275]
  cd(0:1, 177) = [114.0000000,1.2080021]
  cd(0:1, 178) = [117.0000000,1.1502764]
  cd(0:1, 179) = [120.0000000,1.0925504]
  cd(0:1, 180) = [123.0000000,1.0216541]
  cd(0:1, 181) = [126.0000000,0.9507574]
  cd(0:1, 182) = [129.0000000,0.8798597]
  cd(0:1, 183) = [132.0000000,0.8058984]
  cd(0:1, 184) = [135.0000000,0.7304053]
  cd(0:1, 185) = [138.0000000,0.6549129]
  cd(0:1, 186) = [141.0000000,0.5809292]
  cd(0:1, 187) = [144.0000000,0.5099626]
  cd(0:1, 188) = [147.0000000,0.4389966]
  cd(0:1, 189) = [150.0000000,0.3680308]
  cd(0:1, 190) = [153.0000000,0.3104608]
  cd(0:1, 191) = [156.0000000,0.2528909]
  cd(0:1, 192) = [159.0000000,0.1953210]
  cd(0:1, 193) = [162.0000000,0.1570568]
  cd(0:1, 194) = [165.0000000,0.1284456]
  cd(0:1, 195) = [168.0000000,0.0998341]
  cd(0:1, 196) = [171.0000000,0.0726838]
  cd(0:1, 197) = [174.0000000,0.0484561]
  cd(0:1, 198) = [177.0000000,0.0397800]
  cd(0:1, 199) = [180.0000000,0.0388897]

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
!     PRINT*,'alpha < tabulated range in calc_cd26_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd26 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd26_ge28.pro'
  ENDIF

!print,y,cd26

!_________________________________________________________________________
  
END FUNCTION calc_cd26_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd27_ge28( y ) RESULT( cd27 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd27
  INTEGER :: i

  cd27 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.0376960]
  cd(0:1, 1  ) = [-177.0000000,0.0376960]
  cd(0:1, 2  ) = [-174.0000000,0.0452352]
  cd(0:1, 3  ) = [-171.0000000,0.0678529]
  cd(0:1, 4  ) = [-168.0000000,0.0947829]
  cd(0:1, 5  ) = [-165.0000000,0.1238689]
  cd(0:1, 6  ) = [-162.0000000,0.1529553]
  cd(0:1, 7  ) = [-159.0000000,0.1916566]
  cd(0:1, 8  ) = [-156.0000000,0.2495874]
  cd(0:1, 9  ) = [-153.0000000,0.3075183]
  cd(0:1, 10 ) = [-150.0000000,0.3654492]
  cd(0:1, 11 ) = [-147.0000000,0.4365044]
  cd(0:1, 12 ) = [-144.0000000,0.5075598]
  cd(0:1, 13 ) = [-141.0000000,0.5786158]
  cd(0:1, 14 ) = [-138.0000000,0.6527028]
  cd(0:1, 15 ) = [-135.0000000,0.7283053]
  cd(0:1, 16 ) = [-132.0000000,0.8039087]
  cd(0:1, 17 ) = [-129.0000000,0.8779861]
  cd(0:1, 18 ) = [-126.0000000,0.9490115]
  cd(0:1, 19 ) = [-123.0000000,1.0200359]
  cd(0:1, 20 ) = [-120.0000000,1.0910599]
  cd(0:1, 21 ) = [-117.0000000,1.1489272]
  cd(0:1, 22 ) = [-114.0000000,1.2067943]
  cd(0:1, 23 ) = [-111.0000000,1.2646609]
  cd(0:1, 24 ) = [-108.0000000,1.3090960]
  cd(0:1, 25 ) = [-105.0000000,1.3468152]
  cd(0:1, 26 ) = [-102.0000000,1.3845340]
  cd(0:1, 27 ) = [-99.0000000,1.4140180]
  cd(0:1, 28 ) = [-96.0000000,1.4270320]
  cd(0:1, 29 ) = [-93.0000000,1.4400460]
  cd(0:1, 30 ) = [-90.0000000,1.4530599]
  cd(0:1, 31 ) = [-87.0000000,1.4400460]
  cd(0:1, 32 ) = [-84.0000000,1.4270320]
  cd(0:1, 33 ) = [-81.0000000,1.4140181]
  cd(0:1, 34 ) = [-78.0000000,1.3845342]
  cd(0:1, 35 ) = [-75.0000000,1.3468150]
  cd(0:1, 36 ) = [-72.0000000,1.3090962]
  cd(0:1, 37 ) = [-69.0000000,1.2646614]
  cd(0:1, 38 ) = [-66.0000000,1.2067941]
  cd(0:1, 39 ) = [-63.0000000,1.1489268]
  cd(0:1, 40 ) = [-60.0000000,1.0910594]
  cd(0:1, 41 ) = [-57.0000000,1.0200355]
  cd(0:1, 42 ) = [-54.0000000,0.9490110]
  cd(0:1, 43 ) = [-51.0000000,0.8779857]
  cd(0:1, 44 ) = [-48.0000000,0.8039116]
  cd(0:1, 45 ) = [-45.0000000,0.7283133]
  cd(0:1, 46 ) = [-42.0000000,0.6527057]
  cd(0:1, 47 ) = [-39.0000000,0.5775274]
  cd(0:1, 48 ) = [-36.0000000,0.5040594]
  cd(0:1, 49 ) = [-33.0000000,0.4333188]
  cd(0:1, 50 ) = [-30.0000000,0.3654488]
  cd(0:1, 51 ) = [-29.3939394,0.3524301]
  cd(0:1, 52 ) = [-28.7878788,0.3394115]
  cd(0:1, 53 ) = [-28.1818182,0.3263929]
  cd(0:1, 54 ) = [-27.5757576,0.3137931]
  cd(0:1, 55 ) = [-26.9696970,0.3013729]
  cd(0:1, 56 ) = [-26.3636364,0.2889527]
  cd(0:1, 57 ) = [-25.7575758,0.2767964]
  cd(0:1, 58 ) = [-25.1515152,0.2650360]
  cd(0:1, 59 ) = [-24.5454545,0.2532755]
  cd(0:1, 60 ) = [-23.9393939,0.2415867]
  cd(0:1, 61 ) = [-23.3333333,0.2305432]
  cd(0:1, 62 ) = [-22.7272727,0.2194997]
  cd(0:1, 63 ) = [-22.1212121,0.2084562]
  cd(0:1, 64 ) = [-21.5151515,0.1980289]
  cd(0:1, 65 ) = [-20.9090909,0.1877557]
  cd(0:1, 66 ) = [-20.3030303,0.1774825]
  cd(0:1, 67 ) = [-19.6969697,0.1675143]
  cd(0:1, 68 ) = [-19.0909091,0.1578511]
  cd(0:1, 69 ) = [-18.4848485,0.1485469]
  cd(0:1, 70 ) = [-17.8787879,0.1395552]
  cd(0:1, 71 ) = [-17.2727273,0.1315599]
  cd(0:1, 72 ) = [-16.6666667,0.1235646]
  cd(0:1, 73 ) = [-16.0606061,0.1155693]
  cd(0:1, 74 ) = [-15.4545455,0.1075750]
  cd(0:1, 75 ) = [-14.8484848,0.0995805]
  cd(0:1, 76 ) = [-14.2424242,0.0915852]
  cd(0:1, 77 ) = [-13.6363636,0.0835902]
  cd(0:1, 78 ) = [-13.0303030,0.0755946]
  cd(0:1, 79 ) = [-12.4242424,0.0675997]
  cd(0:1, 80 ) = [-11.8181818,0.0596042]
  cd(0:1, 81 ) = [-11.2121212,0.0516093]
  cd(0:1, 82 ) = [-10.6060606,0.0436139]
  cd(0:1, 83 ) = [-10.0000000,0.0356199]
  cd(0:1, 84 ) = [-9.3939394,0.0276249]
  cd(0:1, 85 ) = [-8.7878788,0.0201382]
  cd(0:1, 86 ) = [-8.1818182,0.0135967]
  cd(0:1, 87 ) = [-7.5757576,0.0104062]
  cd(0:1, 88 ) = [-6.9696970,0.0089152]
  cd(0:1, 89 ) = [-6.3636364,0.0074909]
  cd(0:1, 90 ) = [-5.7575758,0.0070545]
  cd(0:1, 91 ) = [-5.1515152,0.0067606]
  cd(0:1, 92 ) = [-4.5454545,0.0065182]
  cd(0:1, 93 ) = [-3.9393939,0.0064000]
  cd(0:1, 94 ) = [-3.3333333,0.0063667]
  cd(0:1, 95 ) = [-2.7272727,0.0063000]
  cd(0:1, 96 ) = [-2.1212121,0.0063000]
  cd(0:1, 97 ) = [-1.5151515,0.0063000]
  cd(0:1, 98 ) = [-0.9090909,0.0063000]
  cd(0:1, 99 ) = [-0.3030303,0.0063394]
  cd(0:1, 100) = [0.3030303,0.0064000]
  cd(0:1, 101) = [0.9090909,0.0064000]
  cd(0:1, 102) = [1.5151515,0.0065000]
  cd(0:1, 103) = [2.1212121,0.0065242]
  cd(0:1, 104) = [2.7272727,0.0066455]
  cd(0:1, 105) = [3.3333333,0.0067667]
  cd(0:1, 106) = [3.9393939,0.0068879]
  cd(0:1, 107) = [4.5454545,0.0070091]
  cd(0:1, 108) = [5.1515152,0.0071606]
  cd(0:1, 109) = [5.7575758,0.0074545]
  cd(0:1, 110) = [6.3636364,0.0079637]
  cd(0:1, 111) = [6.9696970,0.0089455]
  cd(0:1, 112) = [7.5757576,0.0107272]
  cd(0:1, 113) = [8.1818182,0.0125091]
  cd(0:1, 114) = [8.7878788,0.0140333]
  cd(0:1, 115) = [9.3939394,0.0153667]
  cd(0:1, 116) = [10.0000000,0.0165000]
  cd(0:1, 117) = [10.6060606,0.0181606]
  cd(0:1, 118) = [11.2121212,0.0203486]
  cd(0:1, 119) = [11.8181818,0.0300904]
  cd(0:1, 120) = [12.4242424,0.0464547]
  cd(0:1, 121) = [13.0303030,0.0628180]
  cd(0:1, 122) = [13.6363636,0.0791824]
  cd(0:1, 123) = [14.2424242,0.0922729]
  cd(0:1, 124) = [14.8484848,0.1004548]
  cd(0:1, 125) = [15.4545455,0.1086364]
  cd(0:1, 126) = [16.0606061,0.1168180]
  cd(0:1, 127) = [16.6666667,0.1249999]
  cd(0:1, 128) = [17.2727273,0.1331819]
  cd(0:1, 129) = [17.8787879,0.1413638]
  cd(0:1, 130) = [18.4848485,0.1494979]
  cd(0:1, 131) = [19.0909091,0.1578511]
  cd(0:1, 132) = [19.6969697,0.1675143]
  cd(0:1, 133) = [20.3030303,0.1774825]
  cd(0:1, 134) = [20.9090909,0.1877557]
  cd(0:1, 135) = [21.5151515,0.1980289]
  cd(0:1, 136) = [22.1212121,0.2084562]
  cd(0:1, 137) = [22.7272727,0.2194997]
  cd(0:1, 138) = [23.3333333,0.2305432]
  cd(0:1, 139) = [23.9393939,0.2415867]
  cd(0:1, 140) = [24.5454545,0.2532755]
  cd(0:1, 141) = [25.1515152,0.2650360]
  cd(0:1, 142) = [25.7575758,0.2767964]
  cd(0:1, 143) = [26.3636364,0.2889527]
  cd(0:1, 144) = [26.9696970,0.3013729]
  cd(0:1, 145) = [27.5757576,0.3137931]
  cd(0:1, 146) = [28.1818182,0.3263929]
  cd(0:1, 147) = [28.7878788,0.3394115]
  cd(0:1, 148) = [29.3939394,0.3524301]
  cd(0:1, 149) = [30.0000000,0.3654488]
  cd(0:1, 150) = [33.0000000,0.4333188]
  cd(0:1, 151) = [36.0000000,0.5040594]
  cd(0:1, 152) = [39.0000000,0.5775274]
  cd(0:1, 153) = [42.0000000,0.6527057]
  cd(0:1, 154) = [45.0000000,0.7283133]
  cd(0:1, 155) = [48.0000000,0.8039116]
  cd(0:1, 156) = [51.0000000,0.8779857]
  cd(0:1, 157) = [54.0000000,0.9490110]
  cd(0:1, 158) = [57.0000000,1.0200355]
  cd(0:1, 159) = [60.0000000,1.0910594]
  cd(0:1, 160) = [63.0000000,1.1489268]
  cd(0:1, 161) = [66.0000000,1.2067941]
  cd(0:1, 162) = [69.0000000,1.2646614]
  cd(0:1, 163) = [72.0000000,1.3090962]
  cd(0:1, 164) = [75.0000000,1.3468150]
  cd(0:1, 165) = [78.0000000,1.3845342]
  cd(0:1, 166) = [81.0000000,1.4140181]
  cd(0:1, 167) = [84.0000000,1.4270320]
  cd(0:1, 168) = [87.0000000,1.4400460]
  cd(0:1, 169) = [90.0000000,1.4530599]
  cd(0:1, 170) = [93.0000000,1.4400460]
  cd(0:1, 171) = [96.0000000,1.4270320]
  cd(0:1, 172) = [99.0000000,1.4140180]
  cd(0:1, 173) = [102.0000000,1.3845340]
  cd(0:1, 174) = [105.0000000,1.3468152]
  cd(0:1, 175) = [108.0000000,1.3090960]
  cd(0:1, 176) = [111.0000000,1.2646609]
  cd(0:1, 177) = [114.0000000,1.2067943]
  cd(0:1, 178) = [117.0000000,1.1489272]
  cd(0:1, 179) = [120.0000000,1.0910599]
  cd(0:1, 180) = [123.0000000,1.0200359]
  cd(0:1, 181) = [126.0000000,0.9490115]
  cd(0:1, 182) = [129.0000000,0.8779861]
  cd(0:1, 183) = [132.0000000,0.8039087]
  cd(0:1, 184) = [135.0000000,0.7283053]
  cd(0:1, 185) = [138.0000000,0.6527028]
  cd(0:1, 186) = [141.0000000,0.5786158]
  cd(0:1, 187) = [144.0000000,0.5075598]
  cd(0:1, 188) = [147.0000000,0.4365044]
  cd(0:1, 189) = [150.0000000,0.3654492]
  cd(0:1, 190) = [153.0000000,0.3075183]
  cd(0:1, 191) = [156.0000000,0.2495874]
  cd(0:1, 192) = [159.0000000,0.1916566]
  cd(0:1, 193) = [162.0000000,0.1535609]
  cd(0:1, 194) = [165.0000000,0.1253829]
  cd(0:1, 195) = [168.0000000,0.0972047]
  cd(0:1, 196) = [171.0000000,0.0705773]
  cd(0:1, 197) = [174.0000000,0.0470518]
  cd(0:1, 198) = [177.0000000,0.0386003]
  cd(0:1, 199) = [180.0000000,0.0376960]

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
!     PRINT*,'alpha < tabulated range in calc_cd27_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd27 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd27_ge28.pro'
  ENDIF

!print,y,cd27

!_________________________________________________________________________
  
END FUNCTION calc_cd27_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd28_ge28( y ) RESULT( cd28 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd28
  INTEGER :: i

  cd28 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.0376960]
  cd(0:1, 1  ) = [-177.0000000,0.0376960]
  cd(0:1, 2  ) = [-174.0000000,0.0452352]
  cd(0:1, 3  ) = [-171.0000000,0.0678529]
  cd(0:1, 4  ) = [-168.0000000,0.0947828]
  cd(0:1, 5  ) = [-165.0000000,0.1238689]
  cd(0:1, 6  ) = [-162.0000000,0.1529552]
  cd(0:1, 7  ) = [-159.0000000,0.1916565]
  cd(0:1, 8  ) = [-156.0000000,0.2495873]
  cd(0:1, 9  ) = [-153.0000000,0.3075182]
  cd(0:1, 10 ) = [-150.0000000,0.3654492]
  cd(0:1, 11 ) = [-147.0000000,0.4365043]
  cd(0:1, 12 ) = [-144.0000000,0.5075598]
  cd(0:1, 13 ) = [-141.0000000,0.5786157]
  cd(0:1, 14 ) = [-138.0000000,0.6527028]
  cd(0:1, 15 ) = [-135.0000000,0.7283053]
  cd(0:1, 16 ) = [-132.0000000,0.8039087]
  cd(0:1, 17 ) = [-129.0000000,0.8779861]
  cd(0:1, 18 ) = [-126.0000000,0.9490115]
  cd(0:1, 19 ) = [-123.0000000,1.0200359]
  cd(0:1, 20 ) = [-120.0000000,1.0910599]
  cd(0:1, 21 ) = [-117.0000000,1.1489272]
  cd(0:1, 22 ) = [-114.0000000,1.2067943]
  cd(0:1, 23 ) = [-111.0000000,1.2646609]
  cd(0:1, 24 ) = [-108.0000000,1.3090960]
  cd(0:1, 25 ) = [-105.0000000,1.3468152]
  cd(0:1, 26 ) = [-102.0000000,1.3845340]
  cd(0:1, 27 ) = [-99.0000000,1.4140180]
  cd(0:1, 28 ) = [-96.0000000,1.4270320]
  cd(0:1, 29 ) = [-93.0000000,1.4400460]
  cd(0:1, 30 ) = [-90.0000000,1.4530599]
  cd(0:1, 31 ) = [-87.0000000,1.4400460]
  cd(0:1, 32 ) = [-84.0000000,1.4270320]
  cd(0:1, 33 ) = [-81.0000000,1.4140181]
  cd(0:1, 34 ) = [-78.0000000,1.3845342]
  cd(0:1, 35 ) = [-75.0000000,1.3468150]
  cd(0:1, 36 ) = [-72.0000000,1.3090962]
  cd(0:1, 37 ) = [-69.0000000,1.2646614]
  cd(0:1, 38 ) = [-66.0000000,1.2067941]
  cd(0:1, 39 ) = [-63.0000000,1.1489268]
  cd(0:1, 40 ) = [-60.0000000,1.0910594]
  cd(0:1, 41 ) = [-57.0000000,1.0200354]
  cd(0:1, 42 ) = [-54.0000000,0.9490110]
  cd(0:1, 43 ) = [-51.0000000,0.8779856]
  cd(0:1, 44 ) = [-48.0000000,0.8039116]
  cd(0:1, 45 ) = [-45.0000000,0.7283132]
  cd(0:1, 46 ) = [-42.0000000,0.6527056]
  cd(0:1, 47 ) = [-39.0000000,0.5775274]
  cd(0:1, 48 ) = [-36.0000000,0.5040594]
  cd(0:1, 49 ) = [-33.0000000,0.4333187]
  cd(0:1, 50 ) = [-30.0000000,0.3654487]
  cd(0:1, 51 ) = [-29.3939394,0.3524301]
  cd(0:1, 52 ) = [-28.7878788,0.3394115]
  cd(0:1, 53 ) = [-28.1818182,0.3263928]
  cd(0:1, 54 ) = [-27.5757576,0.3137931]
  cd(0:1, 55 ) = [-26.9696970,0.3013729]
  cd(0:1, 56 ) = [-26.3636364,0.2889527]
  cd(0:1, 57 ) = [-25.7575758,0.2767964]
  cd(0:1, 58 ) = [-25.1515152,0.2650359]
  cd(0:1, 59 ) = [-24.5454545,0.2532754]
  cd(0:1, 60 ) = [-23.9393939,0.2415867]
  cd(0:1, 61 ) = [-23.3333333,0.2305432]
  cd(0:1, 62 ) = [-22.7272727,0.2194997]
  cd(0:1, 63 ) = [-22.1212121,0.2084562]
  cd(0:1, 64 ) = [-21.5151515,0.1980289]
  cd(0:1, 65 ) = [-20.9090909,0.1877557]
  cd(0:1, 66 ) = [-20.3030303,0.1774825]
  cd(0:1, 67 ) = [-19.6969697,0.1675143]
  cd(0:1, 68 ) = [-19.0909091,0.1578511]
  cd(0:1, 69 ) = [-18.4848485,0.1485469]
  cd(0:1, 70 ) = [-17.8787879,0.1395551]
  cd(0:1, 71 ) = [-17.2727273,0.1315599]
  cd(0:1, 72 ) = [-16.6666667,0.1235646]
  cd(0:1, 73 ) = [-16.0606061,0.1155693]
  cd(0:1, 74 ) = [-15.4545455,0.1075750]
  cd(0:1, 75 ) = [-14.8484848,0.0995805]
  cd(0:1, 76 ) = [-14.2424242,0.0915852]
  cd(0:1, 77 ) = [-13.6363636,0.0835902]
  cd(0:1, 78 ) = [-13.0303030,0.0755946]
  cd(0:1, 79 ) = [-12.4242424,0.0675997]
  cd(0:1, 80 ) = [-11.8181818,0.0596042]
  cd(0:1, 81 ) = [-11.2121212,0.0516094]
  cd(0:1, 82 ) = [-10.6060606,0.0436139]
  cd(0:1, 83 ) = [-10.0000000,0.0356199]
  cd(0:1, 84 ) = [-9.3939394,0.0276250]
  cd(0:1, 85 ) = [-8.7878788,0.0201382]
  cd(0:1, 86 ) = [-8.1818182,0.0135967]
  cd(0:1, 87 ) = [-7.5757576,0.0104062]
  cd(0:1, 88 ) = [-6.9696970,0.0089152]
  cd(0:1, 89 ) = [-6.3636364,0.0074909]
  cd(0:1, 90 ) = [-5.7575758,0.0070545]
  cd(0:1, 91 ) = [-5.1515152,0.0067606]
  cd(0:1, 92 ) = [-4.5454545,0.0065182]
  cd(0:1, 93 ) = [-3.9393939,0.0064000]
  cd(0:1, 94 ) = [-3.3333333,0.0063667]
  cd(0:1, 95 ) = [-2.7272727,0.0063000]
  cd(0:1, 96 ) = [-2.1212121,0.0063000]
  cd(0:1, 97 ) = [-1.5151515,0.0063000]
  cd(0:1, 98 ) = [-0.9090909,0.0063000]
  cd(0:1, 99 ) = [-0.3030303,0.0063394]
  cd(0:1, 100) = [0.3030303,0.0064000]
  cd(0:1, 101) = [0.9090909,0.0064000]
  cd(0:1, 102) = [1.5151515,0.0065000]
  cd(0:1, 103) = [2.1212121,0.0065242]
  cd(0:1, 104) = [2.7272727,0.0066455]
  cd(0:1, 105) = [3.3333333,0.0067667]
  cd(0:1, 106) = [3.9393939,0.0068879]
  cd(0:1, 107) = [4.5454545,0.0070091]
  cd(0:1, 108) = [5.1515152,0.0071606]
  cd(0:1, 109) = [5.7575758,0.0074545]
  cd(0:1, 110) = [6.3636364,0.0079636]
  cd(0:1, 111) = [6.9696970,0.0089455]
  cd(0:1, 112) = [7.5757576,0.0107272]
  cd(0:1, 113) = [8.1818182,0.0125091]
  cd(0:1, 114) = [8.7878788,0.0140333]
  cd(0:1, 115) = [9.3939394,0.0153667]
  cd(0:1, 116) = [10.0000000,0.0165000]
  cd(0:1, 117) = [10.6060606,0.0181605]
  cd(0:1, 118) = [11.2121212,0.0203485]
  cd(0:1, 119) = [11.8181818,0.0300904]
  cd(0:1, 120) = [12.4242424,0.0464546]
  cd(0:1, 121) = [13.0303030,0.0628179]
  cd(0:1, 122) = [13.6363636,0.0791824]
  cd(0:1, 123) = [14.2424242,0.0922729]
  cd(0:1, 124) = [14.8484848,0.1004548]
  cd(0:1, 125) = [15.4545455,0.1086364]
  cd(0:1, 126) = [16.0606061,0.1168179]
  cd(0:1, 127) = [16.6666667,0.1249999]
  cd(0:1, 128) = [17.2727273,0.1331819]
  cd(0:1, 129) = [17.8787879,0.1413638]
  cd(0:1, 130) = [18.4848485,0.1494979]
  cd(0:1, 131) = [19.0909091,0.1578511]
  cd(0:1, 132) = [19.6969697,0.1675143]
  cd(0:1, 133) = [20.3030303,0.1774825]
  cd(0:1, 134) = [20.9090909,0.1877557]
  cd(0:1, 135) = [21.5151515,0.1980289]
  cd(0:1, 136) = [22.1212121,0.2084562]
  cd(0:1, 137) = [22.7272727,0.2194997]
  cd(0:1, 138) = [23.3333333,0.2305432]
  cd(0:1, 139) = [23.9393939,0.2415867]
  cd(0:1, 140) = [24.5454545,0.2532754]
  cd(0:1, 141) = [25.1515152,0.2650359]
  cd(0:1, 142) = [25.7575758,0.2767964]
  cd(0:1, 143) = [26.3636364,0.2889527]
  cd(0:1, 144) = [26.9696970,0.3013729]
  cd(0:1, 145) = [27.5757576,0.3137931]
  cd(0:1, 146) = [28.1818182,0.3263928]
  cd(0:1, 147) = [28.7878788,0.3394115]
  cd(0:1, 148) = [29.3939394,0.3524301]
  cd(0:1, 149) = [30.0000000,0.3654487]
  cd(0:1, 150) = [33.0000000,0.4333187]
  cd(0:1, 151) = [36.0000000,0.5040594]
  cd(0:1, 152) = [39.0000000,0.5775274]
  cd(0:1, 153) = [42.0000000,0.6527056]
  cd(0:1, 154) = [45.0000000,0.7283132]
  cd(0:1, 155) = [48.0000000,0.8039116]
  cd(0:1, 156) = [51.0000000,0.8779856]
  cd(0:1, 157) = [54.0000000,0.9490110]
  cd(0:1, 158) = [57.0000000,1.0200354]
  cd(0:1, 159) = [60.0000000,1.0910594]
  cd(0:1, 160) = [63.0000000,1.1489268]
  cd(0:1, 161) = [66.0000000,1.2067941]
  cd(0:1, 162) = [69.0000000,1.2646614]
  cd(0:1, 163) = [72.0000000,1.3090962]
  cd(0:1, 164) = [75.0000000,1.3468150]
  cd(0:1, 165) = [78.0000000,1.3845342]
  cd(0:1, 166) = [81.0000000,1.4140181]
  cd(0:1, 167) = [84.0000000,1.4270320]
  cd(0:1, 168) = [87.0000000,1.4400460]
  cd(0:1, 169) = [90.0000000,1.4530599]
  cd(0:1, 170) = [93.0000000,1.4400460]
  cd(0:1, 171) = [96.0000000,1.4270320]
  cd(0:1, 172) = [99.0000000,1.4140180]
  cd(0:1, 173) = [102.0000000,1.3845340]
  cd(0:1, 174) = [105.0000000,1.3468152]
  cd(0:1, 175) = [108.0000000,1.3090960]
  cd(0:1, 176) = [111.0000000,1.2646609]
  cd(0:1, 177) = [114.0000000,1.2067943]
  cd(0:1, 178) = [117.0000000,1.1489272]
  cd(0:1, 179) = [120.0000000,1.0910599]
  cd(0:1, 180) = [123.0000000,1.0200359]
  cd(0:1, 181) = [126.0000000,0.9490115]
  cd(0:1, 182) = [129.0000000,0.8779861]
  cd(0:1, 183) = [132.0000000,0.8039087]
  cd(0:1, 184) = [135.0000000,0.7283053]
  cd(0:1, 185) = [138.0000000,0.6527028]
  cd(0:1, 186) = [141.0000000,0.5786157]
  cd(0:1, 187) = [144.0000000,0.5075598]
  cd(0:1, 188) = [147.0000000,0.4365043]
  cd(0:1, 189) = [150.0000000,0.3654492]
  cd(0:1, 190) = [153.0000000,0.3075182]
  cd(0:1, 191) = [156.0000000,0.2495873]
  cd(0:1, 192) = [159.0000000,0.1916565]
  cd(0:1, 193) = [162.0000000,0.1535608]
  cd(0:1, 194) = [165.0000000,0.1253829]
  cd(0:1, 195) = [168.0000000,0.0972046]
  cd(0:1, 196) = [171.0000000,0.0705773]
  cd(0:1, 197) = [174.0000000,0.0470518]
  cd(0:1, 198) = [177.0000000,0.0386003]
  cd(0:1, 199) = [180.0000000,0.0376960]

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
!     PRINT*,'alpha < tabulated range in calc_cd28_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd28 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd28_ge28.pro'
  ENDIF

!print,y,cd28

!_________________________________________________________________________
  
END FUNCTION calc_cd28_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd29_ge28( y ) RESULT( cd29 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd29
  INTEGER :: i

  cd29 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.0376960]
  cd(0:1, 1  ) = [-177.0000000,0.0376960]
  cd(0:1, 2  ) = [-174.0000000,0.0452352]
  cd(0:1, 3  ) = [-171.0000000,0.0678529]
  cd(0:1, 4  ) = [-168.0000000,0.0947828]
  cd(0:1, 5  ) = [-165.0000000,0.1238689]
  cd(0:1, 6  ) = [-162.0000000,0.1529552]
  cd(0:1, 7  ) = [-159.0000000,0.1916565]
  cd(0:1, 8  ) = [-156.0000000,0.2495873]
  cd(0:1, 9  ) = [-153.0000000,0.3075182]
  cd(0:1, 10 ) = [-150.0000000,0.3654492]
  cd(0:1, 11 ) = [-147.0000000,0.4365043]
  cd(0:1, 12 ) = [-144.0000000,0.5075598]
  cd(0:1, 13 ) = [-141.0000000,0.5786157]
  cd(0:1, 14 ) = [-138.0000000,0.6527028]
  cd(0:1, 15 ) = [-135.0000000,0.7283053]
  cd(0:1, 16 ) = [-132.0000000,0.8039087]
  cd(0:1, 17 ) = [-129.0000000,0.8779861]
  cd(0:1, 18 ) = [-126.0000000,0.9490115]
  cd(0:1, 19 ) = [-123.0000000,1.0200359]
  cd(0:1, 20 ) = [-120.0000000,1.0910599]
  cd(0:1, 21 ) = [-117.0000000,1.1489272]
  cd(0:1, 22 ) = [-114.0000000,1.2067943]
  cd(0:1, 23 ) = [-111.0000000,1.2646609]
  cd(0:1, 24 ) = [-108.0000000,1.3090960]
  cd(0:1, 25 ) = [-105.0000000,1.3468152]
  cd(0:1, 26 ) = [-102.0000000,1.3845340]
  cd(0:1, 27 ) = [-99.0000000,1.4140180]
  cd(0:1, 28 ) = [-96.0000000,1.4270320]
  cd(0:1, 29 ) = [-93.0000000,1.4400460]
  cd(0:1, 30 ) = [-90.0000000,1.4530599]
  cd(0:1, 31 ) = [-87.0000000,1.4400460]
  cd(0:1, 32 ) = [-84.0000000,1.4270320]
  cd(0:1, 33 ) = [-81.0000000,1.4140181]
  cd(0:1, 34 ) = [-78.0000000,1.3845342]
  cd(0:1, 35 ) = [-75.0000000,1.3468150]
  cd(0:1, 36 ) = [-72.0000000,1.3090962]
  cd(0:1, 37 ) = [-69.0000000,1.2646614]
  cd(0:1, 38 ) = [-66.0000000,1.2067941]
  cd(0:1, 39 ) = [-63.0000000,1.1489268]
  cd(0:1, 40 ) = [-60.0000000,1.0910594]
  cd(0:1, 41 ) = [-57.0000000,1.0200354]
  cd(0:1, 42 ) = [-54.0000000,0.9490110]
  cd(0:1, 43 ) = [-51.0000000,0.8779856]
  cd(0:1, 44 ) = [-48.0000000,0.8039116]
  cd(0:1, 45 ) = [-45.0000000,0.7283132]
  cd(0:1, 46 ) = [-42.0000000,0.6527056]
  cd(0:1, 47 ) = [-39.0000000,0.5775274]
  cd(0:1, 48 ) = [-36.0000000,0.5040594]
  cd(0:1, 49 ) = [-33.0000000,0.4333187]
  cd(0:1, 50 ) = [-30.0000000,0.3654487]
  cd(0:1, 51 ) = [-29.3939394,0.3524301]
  cd(0:1, 52 ) = [-28.7878788,0.3394115]
  cd(0:1, 53 ) = [-28.1818182,0.3263928]
  cd(0:1, 54 ) = [-27.5757576,0.3137931]
  cd(0:1, 55 ) = [-26.9696970,0.3013729]
  cd(0:1, 56 ) = [-26.3636364,0.2889527]
  cd(0:1, 57 ) = [-25.7575758,0.2767964]
  cd(0:1, 58 ) = [-25.1515152,0.2650359]
  cd(0:1, 59 ) = [-24.5454545,0.2532754]
  cd(0:1, 60 ) = [-23.9393939,0.2415867]
  cd(0:1, 61 ) = [-23.3333333,0.2305432]
  cd(0:1, 62 ) = [-22.7272727,0.2194997]
  cd(0:1, 63 ) = [-22.1212121,0.2084562]
  cd(0:1, 64 ) = [-21.5151515,0.1980289]
  cd(0:1, 65 ) = [-20.9090909,0.1877557]
  cd(0:1, 66 ) = [-20.3030303,0.1774825]
  cd(0:1, 67 ) = [-19.6969697,0.1675143]
  cd(0:1, 68 ) = [-19.0909091,0.1578511]
  cd(0:1, 69 ) = [-18.4848485,0.1485469]
  cd(0:1, 70 ) = [-17.8787879,0.1395551]
  cd(0:1, 71 ) = [-17.2727273,0.1315599]
  cd(0:1, 72 ) = [-16.6666667,0.1235646]
  cd(0:1, 73 ) = [-16.0606061,0.1155693]
  cd(0:1, 74 ) = [-15.4545455,0.1075750]
  cd(0:1, 75 ) = [-14.8484848,0.0995805]
  cd(0:1, 76 ) = [-14.2424242,0.0915852]
  cd(0:1, 77 ) = [-13.6363636,0.0835902]
  cd(0:1, 78 ) = [-13.0303030,0.0755946]
  cd(0:1, 79 ) = [-12.4242424,0.0675997]
  cd(0:1, 80 ) = [-11.8181818,0.0596042]
  cd(0:1, 81 ) = [-11.2121212,0.0516094]
  cd(0:1, 82 ) = [-10.6060606,0.0436139]
  cd(0:1, 83 ) = [-10.0000000,0.0356199]
  cd(0:1, 84 ) = [-9.3939394,0.0276250]
  cd(0:1, 85 ) = [-8.7878788,0.0201382]
  cd(0:1, 86 ) = [-8.1818182,0.0135967]
  cd(0:1, 87 ) = [-7.5757576,0.0104062]
  cd(0:1, 88 ) = [-6.9696970,0.0089152]
  cd(0:1, 89 ) = [-6.3636364,0.0074909]
  cd(0:1, 90 ) = [-5.7575758,0.0070545]
  cd(0:1, 91 ) = [-5.1515152,0.0067606]
  cd(0:1, 92 ) = [-4.5454545,0.0065182]
  cd(0:1, 93 ) = [-3.9393939,0.0064000]
  cd(0:1, 94 ) = [-3.3333333,0.0063667]
  cd(0:1, 95 ) = [-2.7272727,0.0063000]
  cd(0:1, 96 ) = [-2.1212121,0.0063000]
  cd(0:1, 97 ) = [-1.5151515,0.0063000]
  cd(0:1, 98 ) = [-0.9090909,0.0063000]
  cd(0:1, 99 ) = [-0.3030303,0.0063394]
  cd(0:1, 100) = [0.3030303,0.0064000]
  cd(0:1, 101) = [0.9090909,0.0064000]
  cd(0:1, 102) = [1.5151515,0.0065000]
  cd(0:1, 103) = [2.1212121,0.0065242]
  cd(0:1, 104) = [2.7272727,0.0066455]
  cd(0:1, 105) = [3.3333333,0.0067667]
  cd(0:1, 106) = [3.9393939,0.0068879]
  cd(0:1, 107) = [4.5454545,0.0070091]
  cd(0:1, 108) = [5.1515152,0.0071606]
  cd(0:1, 109) = [5.7575758,0.0074545]
  cd(0:1, 110) = [6.3636364,0.0079636]
  cd(0:1, 111) = [6.9696970,0.0089455]
  cd(0:1, 112) = [7.5757576,0.0107272]
  cd(0:1, 113) = [8.1818182,0.0125091]
  cd(0:1, 114) = [8.7878788,0.0140333]
  cd(0:1, 115) = [9.3939394,0.0153667]
  cd(0:1, 116) = [10.0000000,0.0165000]
  cd(0:1, 117) = [10.6060606,0.0181605]
  cd(0:1, 118) = [11.2121212,0.0203485]
  cd(0:1, 119) = [11.8181818,0.0300904]
  cd(0:1, 120) = [12.4242424,0.0464546]
  cd(0:1, 121) = [13.0303030,0.0628179]
  cd(0:1, 122) = [13.6363636,0.0791824]
  cd(0:1, 123) = [14.2424242,0.0922729]
  cd(0:1, 124) = [14.8484848,0.1004548]
  cd(0:1, 125) = [15.4545455,0.1086364]
  cd(0:1, 126) = [16.0606061,0.1168179]
  cd(0:1, 127) = [16.6666667,0.1249999]
  cd(0:1, 128) = [17.2727273,0.1331819]
  cd(0:1, 129) = [17.8787879,0.1413638]
  cd(0:1, 130) = [18.4848485,0.1494979]
  cd(0:1, 131) = [19.0909091,0.1578511]
  cd(0:1, 132) = [19.6969697,0.1675143]
  cd(0:1, 133) = [20.3030303,0.1774825]
  cd(0:1, 134) = [20.9090909,0.1877557]
  cd(0:1, 135) = [21.5151515,0.1980289]
  cd(0:1, 136) = [22.1212121,0.2084562]
  cd(0:1, 137) = [22.7272727,0.2194997]
  cd(0:1, 138) = [23.3333333,0.2305432]
  cd(0:1, 139) = [23.9393939,0.2415867]
  cd(0:1, 140) = [24.5454545,0.2532754]
  cd(0:1, 141) = [25.1515152,0.2650359]
  cd(0:1, 142) = [25.7575758,0.2767964]
  cd(0:1, 143) = [26.3636364,0.2889527]
  cd(0:1, 144) = [26.9696970,0.3013729]
  cd(0:1, 145) = [27.5757576,0.3137931]
  cd(0:1, 146) = [28.1818182,0.3263928]
  cd(0:1, 147) = [28.7878788,0.3394115]
  cd(0:1, 148) = [29.3939394,0.3524301]
  cd(0:1, 149) = [30.0000000,0.3654487]
  cd(0:1, 150) = [33.0000000,0.4333187]
  cd(0:1, 151) = [36.0000000,0.5040594]
  cd(0:1, 152) = [39.0000000,0.5775274]
  cd(0:1, 153) = [42.0000000,0.6527056]
  cd(0:1, 154) = [45.0000000,0.7283132]
  cd(0:1, 155) = [48.0000000,0.8039116]
  cd(0:1, 156) = [51.0000000,0.8779856]
  cd(0:1, 157) = [54.0000000,0.9490110]
  cd(0:1, 158) = [57.0000000,1.0200354]
  cd(0:1, 159) = [60.0000000,1.0910594]
  cd(0:1, 160) = [63.0000000,1.1489268]
  cd(0:1, 161) = [66.0000000,1.2067941]
  cd(0:1, 162) = [69.0000000,1.2646614]
  cd(0:1, 163) = [72.0000000,1.3090962]
  cd(0:1, 164) = [75.0000000,1.3468150]
  cd(0:1, 165) = [78.0000000,1.3845342]
  cd(0:1, 166) = [81.0000000,1.4140181]
  cd(0:1, 167) = [84.0000000,1.4270320]
  cd(0:1, 168) = [87.0000000,1.4400460]
  cd(0:1, 169) = [90.0000000,1.4530599]
  cd(0:1, 170) = [93.0000000,1.4400460]
  cd(0:1, 171) = [96.0000000,1.4270320]
  cd(0:1, 172) = [99.0000000,1.4140180]
  cd(0:1, 173) = [102.0000000,1.3845340]
  cd(0:1, 174) = [105.0000000,1.3468152]
  cd(0:1, 175) = [108.0000000,1.3090960]
  cd(0:1, 176) = [111.0000000,1.2646609]
  cd(0:1, 177) = [114.0000000,1.2067943]
  cd(0:1, 178) = [117.0000000,1.1489272]
  cd(0:1, 179) = [120.0000000,1.0910599]
  cd(0:1, 180) = [123.0000000,1.0200359]
  cd(0:1, 181) = [126.0000000,0.9490115]
  cd(0:1, 182) = [129.0000000,0.8779861]
  cd(0:1, 183) = [132.0000000,0.8039087]
  cd(0:1, 184) = [135.0000000,0.7283053]
  cd(0:1, 185) = [138.0000000,0.6527028]
  cd(0:1, 186) = [141.0000000,0.5786157]
  cd(0:1, 187) = [144.0000000,0.5075598]
  cd(0:1, 188) = [147.0000000,0.4365043]
  cd(0:1, 189) = [150.0000000,0.3654492]
  cd(0:1, 190) = [153.0000000,0.3075182]
  cd(0:1, 191) = [156.0000000,0.2495873]
  cd(0:1, 192) = [159.0000000,0.1916565]
  cd(0:1, 193) = [162.0000000,0.1535608]
  cd(0:1, 194) = [165.0000000,0.1253829]
  cd(0:1, 195) = [168.0000000,0.0972046]
  cd(0:1, 196) = [171.0000000,0.0705773]
  cd(0:1, 197) = [174.0000000,0.0470518]
  cd(0:1, 198) = [177.0000000,0.0386003]
  cd(0:1, 199) = [180.0000000,0.0376960]

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
!     PRINT*,'alpha < tabulated range in calc_cd29_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd29 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd29_ge28.pro'
  ENDIF

!print,y,cd29

!_________________________________________________________________________
  
END FUNCTION calc_cd29_ge28
!_________________________________________________________________________

!=========================================================================

FUNCTION calc_cd30_ge28( y ) RESULT( cd30 )
!_________________________________________________________________________
!
! Data from GE 2.8 MW: https://github.com/NREL/openfast-turbine-models/tree/f/IEA-scaled-2.5-116/IEA-scaled/NREL-2.8-127/OpenFAST/Airfoils
!_________________________________________________________________________
  
  REAL, INTENT( IN ) :: y

  INTEGER, PARAMETER :: na = 200

  REAL, DIMENSION( 0:1,0:na-1 ) :: cd

  REAL :: cd30
  INTEGER :: i

  cd30 = 0.0
!                alpha    CD
  cd(0:1, 0  ) = [-180.0000000,0.0376960]
  cd(0:1, 1  ) = [-177.0000000,0.0376960]
  cd(0:1, 2  ) = [-174.0000000,0.0452352]
  cd(0:1, 3  ) = [-171.0000000,0.0678529]
  cd(0:1, 4  ) = [-168.0000000,0.0947828]
  cd(0:1, 5  ) = [-165.0000000,0.1238689]
  cd(0:1, 6  ) = [-162.0000000,0.1529552]
  cd(0:1, 7  ) = [-159.0000000,0.1916565]
  cd(0:1, 8  ) = [-156.0000000,0.2495873]
  cd(0:1, 9  ) = [-153.0000000,0.3075182]
  cd(0:1, 10 ) = [-150.0000000,0.3654492]
  cd(0:1, 11 ) = [-147.0000000,0.4365043]
  cd(0:1, 12 ) = [-144.0000000,0.5075598]
  cd(0:1, 13 ) = [-141.0000000,0.5786157]
  cd(0:1, 14 ) = [-138.0000000,0.6527028]
  cd(0:1, 15 ) = [-135.0000000,0.7283053]
  cd(0:1, 16 ) = [-132.0000000,0.8039087]
  cd(0:1, 17 ) = [-129.0000000,0.8779861]
  cd(0:1, 18 ) = [-126.0000000,0.9490115]
  cd(0:1, 19 ) = [-123.0000000,1.0200359]
  cd(0:1, 20 ) = [-120.0000000,1.0910599]
  cd(0:1, 21 ) = [-117.0000000,1.1489272]
  cd(0:1, 22 ) = [-114.0000000,1.2067943]
  cd(0:1, 23 ) = [-111.0000000,1.2646609]
  cd(0:1, 24 ) = [-108.0000000,1.3090960]
  cd(0:1, 25 ) = [-105.0000000,1.3468152]
  cd(0:1, 26 ) = [-102.0000000,1.3845340]
  cd(0:1, 27 ) = [-99.0000000,1.4140180]
  cd(0:1, 28 ) = [-96.0000000,1.4270320]
  cd(0:1, 29 ) = [-93.0000000,1.4400460]
  cd(0:1, 30 ) = [-90.0000000,1.4530599]
  cd(0:1, 31 ) = [-87.0000000,1.4400460]
  cd(0:1, 32 ) = [-84.0000000,1.4270320]
  cd(0:1, 33 ) = [-81.0000000,1.4140181]
  cd(0:1, 34 ) = [-78.0000000,1.3845342]
  cd(0:1, 35 ) = [-75.0000000,1.3468150]
  cd(0:1, 36 ) = [-72.0000000,1.3090962]
  cd(0:1, 37 ) = [-69.0000000,1.2646614]
  cd(0:1, 38 ) = [-66.0000000,1.2067941]
  cd(0:1, 39 ) = [-63.0000000,1.1489268]
  cd(0:1, 40 ) = [-60.0000000,1.0910594]
  cd(0:1, 41 ) = [-57.0000000,1.0200354]
  cd(0:1, 42 ) = [-54.0000000,0.9490110]
  cd(0:1, 43 ) = [-51.0000000,0.8779856]
  cd(0:1, 44 ) = [-48.0000000,0.8039116]
  cd(0:1, 45 ) = [-45.0000000,0.7283132]
  cd(0:1, 46 ) = [-42.0000000,0.6527056]
  cd(0:1, 47 ) = [-39.0000000,0.5775274]
  cd(0:1, 48 ) = [-36.0000000,0.5040594]
  cd(0:1, 49 ) = [-33.0000000,0.4333187]
  cd(0:1, 50 ) = [-30.0000000,0.3654487]
  cd(0:1, 51 ) = [-29.3939394,0.3524301]
  cd(0:1, 52 ) = [-28.7878788,0.3394115]
  cd(0:1, 53 ) = [-28.1818182,0.3263928]
  cd(0:1, 54 ) = [-27.5757576,0.3137931]
  cd(0:1, 55 ) = [-26.9696970,0.3013729]
  cd(0:1, 56 ) = [-26.3636364,0.2889527]
  cd(0:1, 57 ) = [-25.7575758,0.2767964]
  cd(0:1, 58 ) = [-25.1515152,0.2650359]
  cd(0:1, 59 ) = [-24.5454545,0.2532754]
  cd(0:1, 60 ) = [-23.9393939,0.2415867]
  cd(0:1, 61 ) = [-23.3333333,0.2305432]
  cd(0:1, 62 ) = [-22.7272727,0.2194997]
  cd(0:1, 63 ) = [-22.1212121,0.2084562]
  cd(0:1, 64 ) = [-21.5151515,0.1980289]
  cd(0:1, 65 ) = [-20.9090909,0.1877557]
  cd(0:1, 66 ) = [-20.3030303,0.1774825]
  cd(0:1, 67 ) = [-19.6969697,0.1675143]
  cd(0:1, 68 ) = [-19.0909091,0.1578511]
  cd(0:1, 69 ) = [-18.4848485,0.1485469]
  cd(0:1, 70 ) = [-17.8787879,0.1395551]
  cd(0:1, 71 ) = [-17.2727273,0.1315599]
  cd(0:1, 72 ) = [-16.6666667,0.1235646]
  cd(0:1, 73 ) = [-16.0606061,0.1155693]
  cd(0:1, 74 ) = [-15.4545455,0.1075750]
  cd(0:1, 75 ) = [-14.8484848,0.0995805]
  cd(0:1, 76 ) = [-14.2424242,0.0915852]
  cd(0:1, 77 ) = [-13.6363636,0.0835902]
  cd(0:1, 78 ) = [-13.0303030,0.0755946]
  cd(0:1, 79 ) = [-12.4242424,0.0675997]
  cd(0:1, 80 ) = [-11.8181818,0.0596042]
  cd(0:1, 81 ) = [-11.2121212,0.0516094]
  cd(0:1, 82 ) = [-10.6060606,0.0436139]
  cd(0:1, 83 ) = [-10.0000000,0.0356199]
  cd(0:1, 84 ) = [-9.3939394,0.0276250]
  cd(0:1, 85 ) = [-8.7878788,0.0201382]
  cd(0:1, 86 ) = [-8.1818182,0.0135967]
  cd(0:1, 87 ) = [-7.5757576,0.0104062]
  cd(0:1, 88 ) = [-6.9696970,0.0089152]
  cd(0:1, 89 ) = [-6.3636364,0.0074909]
  cd(0:1, 90 ) = [-5.7575758,0.0070545]
  cd(0:1, 91 ) = [-5.1515152,0.0067606]
  cd(0:1, 92 ) = [-4.5454545,0.0065182]
  cd(0:1, 93 ) = [-3.9393939,0.0064000]
  cd(0:1, 94 ) = [-3.3333333,0.0063667]
  cd(0:1, 95 ) = [-2.7272727,0.0063000]
  cd(0:1, 96 ) = [-2.1212121,0.0063000]
  cd(0:1, 97 ) = [-1.5151515,0.0063000]
  cd(0:1, 98 ) = [-0.9090909,0.0063000]
  cd(0:1, 99 ) = [-0.3030303,0.0063394]
  cd(0:1, 100) = [0.3030303,0.0064000]
  cd(0:1, 101) = [0.9090909,0.0064000]
  cd(0:1, 102) = [1.5151515,0.0065000]
  cd(0:1, 103) = [2.1212121,0.0065242]
  cd(0:1, 104) = [2.7272727,0.0066455]
  cd(0:1, 105) = [3.3333333,0.0067667]
  cd(0:1, 106) = [3.9393939,0.0068879]
  cd(0:1, 107) = [4.5454545,0.0070091]
  cd(0:1, 108) = [5.1515152,0.0071606]
  cd(0:1, 109) = [5.7575758,0.0074545]
  cd(0:1, 110) = [6.3636364,0.0079636]
  cd(0:1, 111) = [6.9696970,0.0089455]
  cd(0:1, 112) = [7.5757576,0.0107272]
  cd(0:1, 113) = [8.1818182,0.0125091]
  cd(0:1, 114) = [8.7878788,0.0140333]
  cd(0:1, 115) = [9.3939394,0.0153667]
  cd(0:1, 116) = [10.0000000,0.0165000]
  cd(0:1, 117) = [10.6060606,0.0181605]
  cd(0:1, 118) = [11.2121212,0.0203485]
  cd(0:1, 119) = [11.8181818,0.0300904]
  cd(0:1, 120) = [12.4242424,0.0464546]
  cd(0:1, 121) = [13.0303030,0.0628179]
  cd(0:1, 122) = [13.6363636,0.0791824]
  cd(0:1, 123) = [14.2424242,0.0922729]
  cd(0:1, 124) = [14.8484848,0.1004548]
  cd(0:1, 125) = [15.4545455,0.1086364]
  cd(0:1, 126) = [16.0606061,0.1168179]
  cd(0:1, 127) = [16.6666667,0.1249999]
  cd(0:1, 128) = [17.2727273,0.1331819]
  cd(0:1, 129) = [17.8787879,0.1413638]
  cd(0:1, 130) = [18.4848485,0.1494979]
  cd(0:1, 131) = [19.0909091,0.1578511]
  cd(0:1, 132) = [19.6969697,0.1675143]
  cd(0:1, 133) = [20.3030303,0.1774825]
  cd(0:1, 134) = [20.9090909,0.1877557]
  cd(0:1, 135) = [21.5151515,0.1980289]
  cd(0:1, 136) = [22.1212121,0.2084562]
  cd(0:1, 137) = [22.7272727,0.2194997]
  cd(0:1, 138) = [23.3333333,0.2305432]
  cd(0:1, 139) = [23.9393939,0.2415867]
  cd(0:1, 140) = [24.5454545,0.2532754]
  cd(0:1, 141) = [25.1515152,0.2650359]
  cd(0:1, 142) = [25.7575758,0.2767964]
  cd(0:1, 143) = [26.3636364,0.2889527]
  cd(0:1, 144) = [26.9696970,0.3013729]
  cd(0:1, 145) = [27.5757576,0.3137931]
  cd(0:1, 146) = [28.1818182,0.3263928]
  cd(0:1, 147) = [28.7878788,0.3394115]
  cd(0:1, 148) = [29.3939394,0.3524301]
  cd(0:1, 149) = [30.0000000,0.3654487]
  cd(0:1, 150) = [33.0000000,0.4333187]
  cd(0:1, 151) = [36.0000000,0.5040594]
  cd(0:1, 152) = [39.0000000,0.5775274]
  cd(0:1, 153) = [42.0000000,0.6527056]
  cd(0:1, 154) = [45.0000000,0.7283132]
  cd(0:1, 155) = [48.0000000,0.8039116]
  cd(0:1, 156) = [51.0000000,0.8779856]
  cd(0:1, 157) = [54.0000000,0.9490110]
  cd(0:1, 158) = [57.0000000,1.0200354]
  cd(0:1, 159) = [60.0000000,1.0910594]
  cd(0:1, 160) = [63.0000000,1.1489268]
  cd(0:1, 161) = [66.0000000,1.2067941]
  cd(0:1, 162) = [69.0000000,1.2646614]
  cd(0:1, 163) = [72.0000000,1.3090962]
  cd(0:1, 164) = [75.0000000,1.3468150]
  cd(0:1, 165) = [78.0000000,1.3845342]
  cd(0:1, 166) = [81.0000000,1.4140181]
  cd(0:1, 167) = [84.0000000,1.4270320]
  cd(0:1, 168) = [87.0000000,1.4400460]
  cd(0:1, 169) = [90.0000000,1.4530599]
  cd(0:1, 170) = [93.0000000,1.4400460]
  cd(0:1, 171) = [96.0000000,1.4270320]
  cd(0:1, 172) = [99.0000000,1.4140180]
  cd(0:1, 173) = [102.0000000,1.3845340]
  cd(0:1, 174) = [105.0000000,1.3468152]
  cd(0:1, 175) = [108.0000000,1.3090960]
  cd(0:1, 176) = [111.0000000,1.2646609]
  cd(0:1, 177) = [114.0000000,1.2067943]
  cd(0:1, 178) = [117.0000000,1.1489272]
  cd(0:1, 179) = [120.0000000,1.0910599]
  cd(0:1, 180) = [123.0000000,1.0200359]
  cd(0:1, 181) = [126.0000000,0.9490115]
  cd(0:1, 182) = [129.0000000,0.8779861]
  cd(0:1, 183) = [132.0000000,0.8039087]
  cd(0:1, 184) = [135.0000000,0.7283053]
  cd(0:1, 185) = [138.0000000,0.6527028]
  cd(0:1, 186) = [141.0000000,0.5786157]
  cd(0:1, 187) = [144.0000000,0.5075598]
  cd(0:1, 188) = [147.0000000,0.4365043]
  cd(0:1, 189) = [150.0000000,0.3654492]
  cd(0:1, 190) = [153.0000000,0.3075182]
  cd(0:1, 191) = [156.0000000,0.2495873]
  cd(0:1, 192) = [159.0000000,0.1916565]
  cd(0:1, 193) = [162.0000000,0.1535608]
  cd(0:1, 194) = [165.0000000,0.1253829]
  cd(0:1, 195) = [168.0000000,0.0972046]
  cd(0:1, 196) = [171.0000000,0.0705773]
  cd(0:1, 197) = [174.0000000,0.0470518]
  cd(0:1, 198) = [177.0000000,0.0386003]
  cd(0:1, 199) = [180.0000000,0.0376960]

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
!     PRINT*,'alpha < tabulated range in calc_cd30_ge28.pro'
  ENDIF
  IF ( y .GT. cd(0,na-1) ) THEN
     cd30 = cd(1,na-1)
!     PRINT*,'alpha greater than tabulated range in calc_cd30_ge28.pro'
  ENDIF

!print,y,cd30

!_________________________________________________________________________
  
END FUNCTION calc_cd30_ge28
!_________________________________________________________________________

!=========================================================================



END MODULE module_gad_ge28

