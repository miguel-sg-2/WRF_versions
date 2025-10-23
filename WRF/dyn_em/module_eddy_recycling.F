! WRF:MODEL_LAYER:DYNAMICS

 MODULE module_eddy_recycling

 USE module_model_constants
    
#ifdef DM_PARALLEL
    USE MODULE_DM
#endif

 CONTAINS


!!=======================================================================
ELEMENTAL REAL FUNCTION ewma(var_curr, avg_prev, alpha_ewma)  ! ELEMENTAL is used to apply elementwise to arrays
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
 ! DESCRIBE FUNCTION HERE
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
 t_avg_south(:,:) = ewma(t(:,j_sample_south,:), t_avg_south(:,:), alpha_ewma)
 u_avg_south(:,:) = ewma(u(:,j_sample_south,:), u_avg_south(:,:), alpha_ewma)
 v_avg_south(:,:) = ewma(v(:,j_sample_south,:), v_avg_south(:,:), alpha_ewma)
 w_avg_south(:,:) = ewma(w(:,j_sample_south,:), w_avg_south(:,:), alpha_ewma)
 ! Get fluctuation quantities (vectorized)
 t_flct_south(:,:) = t(:,j_sample_south,:) - t_avg_south(:,:)
 u_flct_south(:,:) = u(:,j_sample_south,:) - u_avg_south(:,:)
 v_flct_south(:,:) = v(:,j_sample_south,:) - v_avg_south(:,:)
 w_flct_south(:,:) = w(:,j_sample_south,:) - w_avg_south(:,:)
 ! Zero-out fluctuations above k_max_er
 IF (k_max_er .lt. kte) then
   t_flct_south(:, k_max_er:kte) = 0.0
   u_flct_south(:, k_max_er:kte) = 0.0
   v_flct_south(:, k_max_er:kte) = 0.0
   w_flct_south(:, k_max_er:kte) = 0.0
 end if
end if

! Element-wise loop
! DO i = its, ite
!   DO k = kts, k_max
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
 if (has_south_boundary) then
   t(:,j_insert_south,:) = t(:,j_insert_south,:) + t_flct_south(:,:)
   u(:,j_insert_south,:) = u(:,j_insert_south,:) + u_flct_south(:,:)
   v(:,j_insert_south,:) = v(:,j_insert_south,:) + v_flct_south(:,:)
   w(:,j_insert_south,:) = w(:,j_insert_south,:) + w_flct_south(:,:)
 end if


END SUBROUTINE fluctuations_southBndry
!!======================================================================



 END MODULE module_eddy_recycling

