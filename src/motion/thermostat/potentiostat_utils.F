MODULE potentiostat_utils
 
   USE kinds,                           ONLY: dp
   USE potentiostat_types,              ONLY: potentiostat_type
   
   
!#include "../../base/base_uses.f90"


  IMPLICIT NONE
   
   PRIVATE
   PUBLIC :: potentiostat_electron_count
   
   
CONTAINS

SUBROUTINE potentiostat_electron_count(temp, dipole_pott, temp_part, z_eff, C_0, PHI_0, dt)
    
   !TYPE(force_env_type), POINTER                      :: force_env
   !TYPE(qs_environment_type)                          :: qs_env_temp
   !TYPE(dft_control_type), POINTER                    :: dft_control
   !TYPE(qs_energy_type), POINTER                      :: qs_energy
   !TYPE(section_vals_type), POINTER                   :: md_section
   !TYPE(potentiostat_type), POINTER                   :: potentiostat
   real(kind=dp)                                      :: temp, dipole_pott !, n_electron
   REAL(kind=dp)                                       :: temp_part, z_eff, C_0, PHI_0, dt
   !NULLIFY (qs_energy, force_env)
   
   !CPASSERT(ASSOCIATED(force_env%qs_env))
   
   !CALL create_potentiostat_type(potentiostat, md_section)


     !CALL get_qs_env(force_env%qs_env, energy=qs_energy)
     temp=2.2
      PRINT*, "Sagnik Mukhopadhyay change_11.4" ,temp ,dipole_pott
      PRINT*, "Sagnik Mukhopadhyay change_11.5", temp_part
      PRINT*, "Sagnik Mukhopadhyay change_11.6", z_eff
      PRINT*, "Sagnik Mukhopadhyay change_11.7", C_0, PHI_0
      PRINT*, "Sagnik Mukhopadhyay change_11.8", dt
      
      
     ! n_electron = n_electron - C_0*(dipole_pott-PHI_0)*(1-exp(-time_step/(R*C_0) &
                      
      
END SUBROUTINE potentiostat_electron_count

END MODULE potentiostat_utils
