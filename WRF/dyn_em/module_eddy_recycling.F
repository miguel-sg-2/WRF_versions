! WRF:MODEL_LAYER:DYNAMICS

 MODULE module_eddy_recycling

 USE module_model_constants
    
#ifdef DM_PARALLEL
    USE MODULE_DM
#endif

 CONTAINS


!!=======================================================================
REAL FUNCTION ewma(var_curr, avg_prev, alpha_ewma)  ! ELEMENTAL is used to apply elementwise to arrays
  !-----------------------------------------------------------------------
  ! Function to estimate Exponentially Weighted Moving Average (EWMA)
  !-----------------------------------------------------------------------
  IMPLICIT NONE
  REAL, INTENT(IN) :: var_curr, avg_prev, alpha_ewma
  ewma = alpha_ewma*var_curr + (1.0 - alpha_ewma)*avg_prev
END FUNCTION ewma
!!=======================================================================



!!=======================================================================
SUBROUTINE fluctuations_southBndry( t, u, v, w,               &
                           t_avg_south,u_avg_south,v_avg_south,w_avg_south, &
                           t_flct_south,u_flct_south,v_flct_south,w_flct_south, &
                           alpha_ewma, j_sample_south, j_insert_south, k_max_er, &
                           ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           its, ite, jts, jte, kts, kte         )
 !-----------------------------------------------------------------------
 ! Function to estiamte moving average, estiamte fluctuation quantities,
 ! and directly add the fluctuation quantities to inflow conditions
 !-----------------------------------------------------------------------
 IMPLICIT NONE
#ifdef DM_PARALLEL
    INCLUDE 'mpif.h'
#endif
 ! Variables coming and/or out
 INTEGER, INTENT( IN ) :: ids, ide, jds, jde, kds, kde,  &
                          ims, ime, jms, jme, kms, kme,  &
                          its, ite, jts, jte, kts, kte
 INTEGER, INTENT( IN ) :: j_sample_south,j_insert_south,k_max_er
 REAL ,  INTENT( IN ) :: alpha_ewma
 REAL , DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT ) :: t,u,v,w

 REAL , DIMENSION( ims:ime, kms:kme ), INTENT( INOUT ) :: t_avg_south,u_avg_south,v_avg_south,w_avg_south, &
                                                          t_flct_south,u_flct_south,v_flct_south,w_flct_south
 ! Local variables
 INTEGER :: i,k, ierr, local_rank, root, global_rank_flag
 LOGICAL :: has_south_plane,has_south_boundary
#ifdef DM_PARALLEL
    INTEGER :: status(MPI_STATUS_SIZE)
#endif
!! Done with declarations !!

 ! Determine which rank owns j_sample_south
 CALL MPI_Comm_rank(MPI_COMM_WORLD, local_rank, ierr)
 has_south_plane = ((j_sample_south .GE. jms) .AND. (j_sample_south .LE. jme))
#ifdef DM_PARALLEL
 ! Local flag: store rank if owns j_sample_south, else -1
 IF (has_south_plane) THEN
    global_rank_flag = local_rank
    print*,'Rank ',global_rank_flag,' has the south plane (jms,jme)=(',jms,jme,')'
 ELSE
    global_rank_flag = -1
 END IF
 ! Allreduce with MAX to find root
 CALL MPI_Allreduce(global_rank_flag, root, 1, MPI_INTEGER, MPI_MAX, MPI_COMM_WORLD, ierr)
#else
 root = 0
#endif

! Get fluctuation quantities only if patch is in sampling plane
if (has_south_plane) then
 ! Get moving average (vectorized)
! t_avg_south(:,:) = ewma(t(:,:,j_sample_south), t_avg_south(:,:), alpha_ewma)
! u_avg_south(:,:) = ewma(u(:,:,j_sample_south), u_avg_south(:,:), alpha_ewma)
! v_avg_south(:,:) = ewma(v(:,:,j_sample_south), v_avg_south(:,:), alpha_ewma)
! w_avg_south(:,:) = ewma(w(:,:,j_sample_south), w_avg_south(:,:), alpha_ewma)
 ! Get fluctuation quantities (vectorized)
! t_flct_south(:,:) = t(:,:,j_sample_south) - t_avg_south(:,:)
! u_flct_south(:,:) = u(:,:,j_sample_south) - u_avg_south(:,:)
! v_flct_south(:,:) = v(:,:,j_sample_south) - v_avg_south(:,:)
! w_flct_south(:,:) = w(:,:,j_sample_south) - w_avg_south(:,:)
 ! Zero-out fluctuations above k_max_er
 IF (k_max_er .lt. kte) then
   t_flct_south(:, k_max_er:kte) = 0.0
   u_flct_south(:, k_max_er:kte) = 0.0
   v_flct_south(:, k_max_er:kte) = 0.0
   w_flct_south(:, k_max_er:kte) = 0.0
 end if
end if

print*,'Done with mean and fluctuation quantities'
! Element-wise loop
! DO i = its, ite
!   DO k = kts, k_max_er
!     ! Get moving average
!     t_avg_south(i,k) = ewma(t(k,j_sample_south,i), t_avg_south(i,k), alpha) 
!     u_avg_south(i,k) = ewma(u(k,j_sample_south,i), u_avg_south(i,k), alpha)
!     v_avg_south(i,k) = ewma(v(k,j_sample_south,i), v_avg_south(i,k), alpha)
!     w_avg_south(i,k) = ewma(w(k,j_sample_south,i), w_avg_south(i,k), alpha)
!     ! Get fluctuation quantities
!     t_flct_south(i,k) = t(k,j_sample_south,i) - t_avg_south(i,k)
!     t_flct_south(i,k) = t(k,j_sample_south,i) - u_avg_south(i,k)
!     t_flct_south(i,k) = t(k,j_sample_south,i) - t_avg_south(i,k)
!     t_flct_south(i,k) = t(k,j_sample_south,i) - t_avg_south(i,k)
!   END DO
! END DO

! Broadcast fluctuation quantities to all MPI ranks
#ifdef DM_PARALLEL
 CALL MPI_BCAST(t_flct_south, SIZE(t_flct_south), MPI_REAL, root, MPI_COMM_WORLD, ierr)
 CALL MPI_BCAST(u_flct_south, SIZE(u_flct_south), MPI_REAL, root, MPI_COMM_WORLD, ierr)
 CALL MPI_BCAST(v_flct_south, SIZE(v_flct_south), MPI_REAL, root, MPI_COMM_WORLD, ierr)
 CALL MPI_BCAST(w_flct_south, SIZE(w_flct_south), MPI_REAL, root, MPI_COMM_WORLD, ierr)
#endif

! Add fluctuations to south boundary
 has_south_boundary = ((j_insert_south .GE. jms) .AND. (j_insert_south .LE. jme))
! if (has_south_boundary) then
!   t(:,:,j_insert_south) = t(:,:,j_insert_south) + t_flct_south(:,:)
!   u(:,:,j_insert_south) = u(:,:,j_insert_south) + u_flct_south(:,:)
!   v(:,:,j_insert_south) = v(:,:,j_insert_south) + v_flct_south(:,:)
!   w(:,:,j_insert_south) = w(:,:,j_insert_south) + w_flct_south(:,:)
! end if


END SUBROUTINE fluctuations_southBndry
!!======================================================================



!!=======================================================================
SUBROUTINE fluctuations_southBndry_tends( t, u, v, w,               &
                           t_tendf, ru_tendf, rv_tendf, rw_tendf,   &
                           mut,muu,muv,c1h,c1f,c2h,c2f,   &
                           t_avg_south,u_avg_south,v_avg_south,w_avg_south, &
                           t_flct_south,u_flct_south,v_flct_south,w_flct_south, &
                           alpha_ewma, j_sample_south, j_insert_south, k_max_er, &
                           ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           its, ite, jts, jte, kts, kte         )
 !-----------------------------------------------------------------------
 ! Function to estiamte moving average, estiamte fluctuation quantities,
 ! and nudge flow at the inflow toward turbulent conditions
 !-----------------------------------------------------------------------
 IMPLICIT NONE
#ifdef DM_PARALLEL
    INCLUDE 'mpif.h'
#endif
 ! Variables coming and/or out
 INTEGER, INTENT( IN ) :: ids, ide, jds, jde, kds, kde,  &
                          ims, ime, jms, jme, kms, kme,  &
                          its, ite, jts, jte, kts, kte
 INTEGER, INTENT( IN ) :: j_sample_south,j_insert_south,k_max_er
 REAL,  INTENT( IN ) :: alpha_ewma
 REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN ) :: t,u,v,w
 REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT ) :: t_tendf, ru_tendf, rv_tendf, rw_tendf
 REAL, DIMENSION( ims:ime, jms:jme ),  INTENT( IN )  :: mut, muu, muv
 REAL, DIMENSION( kms:kme ) ,  INTENT( IN ) :: c1h, c2h, c1f, c2f
 REAL, DIMENSION( ims:ime, kms:kme ), INTENT( INOUT ) :: t_avg_south,u_avg_south,v_avg_south,w_avg_south, &
                                                          t_flct_south,u_flct_south,v_flct_south,w_flct_south
 ! Local variables
 INTEGER :: i,k, ierr, local_rank, root, nprocs, rank_send, rank_receive, nn, tag_t, tag_u, tag_v, tag_w
 LOGICAL :: has_south_plane,has_south_boundary
 REAL :: dcoef, dampcoef
 INTEGER, DIMENSION(:), ALLOCATABLE :: rank_has_south_plane_local,rank_has_south_boundary_local, &
                                       rank_has_south_plane_global,rank_has_south_boundary_global
#ifdef DM_PARALLEL
    INTEGER :: status(MPI_STATUS_SIZE)
#endif
!! Done with declarations !!

! Get number of processors and current rank
#ifdef DM_PARALLEL
  CALL MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, local_rank, ierr)
#endif

! Allocate arrays
ALLOCATE(rank_has_south_plane_local(0:nprocs-1))
ALLOCATE(rank_has_south_boundary_local(0:nprocs-1))
ALLOCATE(rank_has_south_plane_global(0:nprocs-1))
ALLOCATE(rank_has_south_boundary_global(0:nprocs-1))

! Initialize to zero
DO nn = 0,nprocs-1
  rank_has_south_plane_local(nn) = -1 ! 0
  rank_has_south_boundary_local(nn) = -1 ! 0
  rank_has_south_plane_global(nn) = -1 ! 0
  rank_has_south_boundary_global(nn) = -1 ! 0
END DO

CALL MPI_Barrier(MPI_COMM_WORLD, ierr)

! Determine which ranks own each segment of the sampling and boundary planes
! Store rank that owns each segment of sampling plane at j=j_sample_south
has_south_plane = ((j_sample_south .GE. jms) .AND. (j_sample_south .LE. jme))
if (has_south_plane) then
  rank_has_south_plane_local(local_rank) = max(0,ims)
end if 
! Store rank that owns each segment of boundary plane at j=j_insert_south
has_south_boundary = ((j_insert_south .GE. jms) .AND. (j_insert_south .LE. jme))
if (has_south_boundary) then
  rank_has_south_boundary_local(local_rank) = max(0,ims)
end if

!print*, 'Rank', local_rank, ' allocated? ', ALLOCATED(rank_has_south_plane_local), &
!         ' nprocs=', nprocs, ' size=', SIZE(rank_has_south_plane_local)

! Sync so that all processors know about planes containing sample and boundary planes
#ifdef DM_PARALLEL
  CALL MPI_Barrier(MPI_COMM_WORLD, ierr)
  CALL MPI_Allreduce(rank_has_south_plane_local, rank_has_south_plane_global, nprocs, MPI_INTEGER, MPI_MAX, MPI_COMM_WORLD, ierr)
  CALL MPI_Allreduce(rank_has_south_boundary_local, rank_has_south_boundary_global, nprocs, MPI_INTEGER, MPI_MAX, MPI_COMM_WORLD, ierr)
#endif

 !print*,'Done with all reduce'
 !print*,'Rank ',rank_has_south_plane_global(local_rank),' has the south sampling plane for ims=',ims
 !print*,'Rank ',rank_has_south_boundary_global(local_rank),' has the south boundary plane for ims=',ims

! Get fluctuation quantities only if patch is in sampling plane
if (has_south_plane) then
  DO i = its, ite
    DO k = kts, k_max_er
      ! Get moving average
      t_avg_south(i,k) = ewma(t(i,k,j_sample_south), t_avg_south(i,k), alpha_ewma)
      u_avg_south(i,k) = ewma(u(i,k,j_sample_south), u_avg_south(i,k), alpha_ewma)
      v_avg_south(i,k) = ewma(v(i,k,j_sample_south), v_avg_south(i,k), alpha_ewma)
      w_avg_south(i,k) = ewma(w(i,k,j_sample_south), w_avg_south(i,k), alpha_ewma)
      ! Get fluctuation quantities (vectorized)
      t_flct_south(i,k) = t(i,k,j_sample_south) - t_avg_south(i,k)
      u_flct_south(i,k) = u(i,k,j_sample_south) - u_avg_south(i,k)
      v_flct_south(i,k) = v(i,k,j_sample_south) - v_avg_south(i,k)
      w_flct_south(i,k) = w(i,k,j_sample_south) - w_avg_south(i,k)
   END DO
 END DO
! ! Zero-out fluctuations above k_max_er
! IF (k_max_er .lt. kte) then
!   t_flct_south(:, k_max_er:kte) = 0.0
!   u_flct_south(:, k_max_er:kte) = 0.0
!   v_flct_south(:, k_max_er:kte) = 0.0
!   w_flct_south(:, k_max_er:kte) = 0.0
! end if
end if
 !print*,'Done calculating mean and fluctuation quantities'

! Broadcast fluctuation quantities to proper rank
! Find the matching rank that sends/receives the fluctuations fields
#ifdef DM_PARALLEL
 rank_send = 0
 rank_receive = 0
 DO nn = 0,nprocs-1
   if (rank_has_south_plane_global(nn) .eq.  max(0,ims)) rank_send=nn
   if (rank_has_south_boundary_global(nn) .eq.  max(0,ims)) rank_receive=nn
 END DO
 !print*,'Rank sending data is ',rank_send,' and rank receiving data is ',rank_receive,' for ims=', max(0,ims) 
 
 ! Transfer data
 tag_t=max(0,ims) ! Must be positive integer
 tag_u=tag_t+1
 tag_v=tag_t+2
 tag_w=tag_t+3
 if (local_rank .eq. rank_send) then
   CALL MPI_Send(t_flct_south, SIZE(t_flct_south), MPI_REAL, rank_receive, tag_t, MPI_COMM_WORLD, ierr)
   CALL MPI_Send(u_flct_south, SIZE(u_flct_south), MPI_REAL, rank_receive, tag_u, MPI_COMM_WORLD, ierr)
   CALL MPI_Send(v_flct_south, SIZE(v_flct_south), MPI_REAL, rank_receive, tag_v, MPI_COMM_WORLD, ierr)
   CALL MPI_Send(w_flct_south, SIZE(w_flct_south), MPI_REAL, rank_receive, tag_w, MPI_COMM_WORLD, ierr)
 end if
 if (local_rank .eq. rank_receive) then
   CALL MPI_Recv(t_flct_south, SIZE(t_flct_south), MPI_REAL, rank_send, tag_t, MPI_COMM_WORLD, status, ierr)
   CALL MPI_Recv(u_flct_south, SIZE(u_flct_south), MPI_REAL, rank_send, tag_u, MPI_COMM_WORLD, status, ierr)
   CALL MPI_Recv(v_flct_south, SIZE(v_flct_south), MPI_REAL, rank_send, tag_v, MPI_COMM_WORLD, status, ierr)
   CALL MPI_Recv(w_flct_south, SIZE(w_flct_south), MPI_REAL, rank_send, tag_w, MPI_COMM_WORLD, status, ierr)
 end if
#endif

! Nudge flow conditions at south boundary toward turbulent state
 if (has_south_boundary) then
   dcoef = (SIN( 0.5 * 3.14 * 1.0 ) )**2
   dampcoef = 0.1
   DO i = its, ite
     DO k = kts, MIN(k_max_er,kte-1)
       t_tendf(i,k,j_insert_south) = t_tendf(i,k,j_insert_south) +                                           &
                                      (c1h(k)*mut(i,j_insert_south)+c2h(k)) * ( dcoef * dampcoef ) *           &
                                      ( t_flct_south(i,k) )
       ru_tendf(i,k,j_insert_south) = ru_tendf(i,k,j_insert_south) +                                           &
                                      (c1h(k)*muu(i,j_insert_south)+c2h(k)) * ( dcoef * dampcoef ) *           &
                                      ( u_flct_south(i,k) )
       rv_tendf(i,k,j_insert_south) = rv_tendf(i,k,j_insert_south) +                                           &
                                      (c1h(k)*muv(i,j_insert_south)+c2h(k)) * ( dcoef * dampcoef ) *           &
                                      ( v_flct_south(i,k) )
       rw_tendf(i,k,j_insert_south) = rw_tendf(i,k,j_insert_south) +                                           &
                                      (c1f(k)*mut(i,j_insert_south)+c2f(k)) * ( dcoef * dampcoef ) *           &
                                      ( w_flct_south(i,k) )
   
     END DO
   END DO

 end if

 ! Clean up
 DEALLOCATE(rank_has_south_plane_local)
 DEALLOCATE(rank_has_south_boundary_local)
 DEALLOCATE(rank_has_south_plane_global)
 DEALLOCATE(rank_has_south_boundary_global)

END SUBROUTINE fluctuations_southBndry_tends
!!======================================================================




 END MODULE module_eddy_recycling

