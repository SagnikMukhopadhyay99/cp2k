MODULE potentiostat_utils
 
   USE kinds,                           ONLY: dp
   USE force_env_types,                 ONLY: force_env_type
   USE qs_energy_types,                ONLY: qs_energy_type
   USE qs_environment_types,           ONLY: get_qs_env
#include "../../base/base_uses.f90"


  IMPLICIT NONE
   
   PRIVATE
   PUBLIC :: potentiostat_electron_count
   
   
CONTAINS

SUBROUTINE potentiostat_electron_count(temp)
    
   TYPE(force_env_type), POINTER                      :: force_env
   TYPE(qs_energy_type), POINTER                      :: energy
   real(kind=dp)                                      :: temp
   
   NULLIFY (energy, force_env)


     !CALL get_qs_env(force_env%qs_env, energy=energy)
     temp=2.2
      PRINT*, "Sagnik Mukhopadhyay change_11.4" ,temp !energy%surf_dipole
      
END SUBROUTINE potentiostat_electron_count

END MODULE potentiostat_utils
