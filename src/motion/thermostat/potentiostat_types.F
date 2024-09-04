MODULE potentiostat_types

   USE input_section_types,             ONLY: section_vals_get,&
                                              section_vals_get_subs_vals,&
                                              section_vals_type,&
                                              section_vals_val_get
   USE kinds,                           ONLY: dp
   
#include "../../base/base_uses.f90"

   IMPLICIT NONE
   
   PUBLIC :: potentiostat_type, &
             create_potentiostat_type, &
             release_potentiostat_type
             
  ! **************************************************************************************************
   TYPE potentiostat_type
      INTEGER                          :: ref_count = 0
      REAL(KIND=dp)                    :: C_0 = 0.0_dp
      REAL(KIND=dp)                    :: PHI_0 = 0.0_dp
      TYPE(section_vals_type), POINTER :: section => NULL()
      
   END TYPE potentiostat_type
   
   
   CONTAINS
   
   !***********************************************************************************************
   
   SUBROUTINE create_potentiostat_type(potentiostat, md_section)
   
   TYPE(potentiostat_type), POINTER                   :: potentiostat
   TYPE(section_vals_type), POINTER                   :: md_section
   TYPE(section_vals_type), POINTER                   :: potentiostat_section
   LOGICAL                                            :: check
   
   check = .NOT. ASSOCIATED(potentiostat)
   CPASSERT(check)
   potentiostat_section => section_vals_get_subs_vals(md_section, "POTENTIOSTAT")
   CALL section_vals_get(potentiostat_section)
   ALLOCATE (potentiostat)
   potentiostat%ref_count = 1
   potentiostat%section => potentiostat_section
   CALL section_vals_val_get(potentiostat_section, "C_0", r_val=potentiostat%C_0)
   CALL section_vals_val_get(potentiostat_section, "PHI_0", r_val=potentiostat%PHI_0)
   
   END SUBROUTINE create_potentiostat_type
   
   !***********************************************************************************************
   
   SUBROUTINE release_potentiostat_type(potentiostat)
      TYPE(potentiostat_type), POINTER                       :: potentiostat

      LOGICAL                                            :: check

      IF (ASSOCIATED(potentiostat)) THEN
         check = potentiostat%ref_count > 0
         CPASSERT(check)
         potentiostat%ref_count = potentiostat%ref_count - 1
         IF (potentiostat%ref_count < 1) THEN
            
            NULLIFY (potentiostat%section)
            DEALLOCATE (potentiostat)
         END IF
      END IF

   END SUBROUTINE release_potentiostat_type


END MODULE potentiostat_types


