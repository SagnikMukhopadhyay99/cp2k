MODULE potentiostat_utils
 
   USE kinds,                           ONLY: dp
   USE potentiostat_types,              ONLY: potentiostat_type
   USE physcon,                         ONLY: Boltzmann
   
   
!#include "../../base/base_uses.f90"


  IMPLICIT NONE
   
   PRIVATE
   PUBLIC :: potentiostat_electron_count, gaussian_random_number_generator
   
   
CONTAINS

SUBROUTINE potentiostat_electron_count(dipole_pott, temp_part, z_eff, C_0, PHI_0, dt)
    
   !TYPE(force_env_type), POINTER                      :: force_env
   !TYPE(qs_environment_type)                          :: qs_env_temp
   !TYPE(dft_control_type), POINTER                    :: dft_control
   !TYPE(qs_energy_type), POINTER                      :: qs_energy
   !TYPE(section_vals_type), POINTER                   :: md_section
   !TYPE(potentiostat_type), POINTER                   :: potentiostat
   real(kind=dp)                                      ::  dipole_pott , n_electron, N, R, k_B, term1, term2
   REAL(kind=dp)                                       :: temp_part, dt
   REAL(kind=dp)                                      ::  C_0, PHI_0
   REAL(KIND=dp), INTENT(INOUT)                       :: z_eff
   !NULLIFY (qs_energy, force_env)
   
   !NULLIFY(potentiostat)
   !PHI_0 = potentiostat%PHI_0
   !C_0 = potentiostat%C_0
   !CPASSERT(ASSOCIATED(force_env%qs_env))
   R=3.166580774_dp* 1.0E-4
   k_B=3.1668114_dp* 1.0E-6
   !CALL create_potentiostat_type(potentiostat, md_section)
   n_electron=z_eff


     !CALL get_qs_env(force_env%qs_env, energy=qs_energy)
     !temp=2.2
      
      
      CALL gaussian_random_number_generator(N)
      
      term1 = C_0*(dipole_pott-PHI_0)*(1-exp(-dt/(R*C_0)))
      term2 = N*sqrt(k_B*temp_part*C_0*(1-exp(-2*dt/(R*C_0))))
      n_electron = n_electron - C_0*(dipole_pott-PHI_0)*(1-exp(-dt/(R*C_0)))
      n_electron = n_electron + N*sqrt(k_B*temp_part*C_0*(1-exp(-2*dt/(R*C_0))))
      
      z_eff=n_electron
      
      !z_eff= z_eff *100
                      
      !PRINT*, "Sagnik Mukhopadhyay change_11.4 dipole potential" ,dipole_pott
      !PRINT*, "Sagnik Mukhopadhyay change_11.5 temperature", temp_part
      PRINT*, "Sagnik Mukhopadhyay change_11.6 z_eff", z_eff
      !PRINT*, "Sagnik Mukhopadhyay change_11.7 C_0 PHI_0", C_0, PHI_0
      !PRINT*, "Sagnik Mukhopadhyay change_11.8 dt", dt
      PRINT*, "Sagnik Mukhopadhyay change_11.9 n_electron", n_electron
      PRINT*, "Sagnik Mukhopadhyay change_11.11 term1 term 2 N", term1, term2, N
      
END SUBROUTINE potentiostat_electron_count


SUBROUTINE gaussian_random_number_generator(N)

    implicit none
    real(kind=dp) :: u1, u2, N
    real(kind=dp) :: pi
    pi = 4*atan(1.00_dp)

    ! Generate two uniform random numbers u1 and u2 in [0,1]
    call random_number(u1)
    call random_number(u2)

    ! Box-Muller transform to generate a single normal random number z1
    N = sqrt(-2.0d0 * log(u1)) * cos(2.0d0 * pi * u2)

    ! Print the generated Gaussian random number
    !print *, "Generated Gaussian random number: ", z1

end subroutine gaussian_random_number_generator


END MODULE potentiostat_utils
