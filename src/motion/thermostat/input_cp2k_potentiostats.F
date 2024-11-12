MODULE input_cp2k_potentiostats

   USE input_keyword_types,             ONLY: keyword_create,&
                                              keyword_release,&
                                              keyword_type
   USE input_section_types,             ONLY: section_add_keyword,&
                                              section_add_subsection,&
                                              section_create,&
                                              section_release,&
                                              section_type
   USE input_val_types,                 ONLY: real_t
   USE kinds,                           ONLY: dp
   USE cp_output_handling,              ONLY: cp_print_key_section_create,&
                                              high_print_level
                                              
#include "../../base/base_uses.f90"


IMPLICIT NONE
PUBLIC :: create_potentiostat_section

CONTAINS


SUBROUTINE create_potentiostat_section(section)
      TYPE(section_type), POINTER                        :: section

      TYPE(keyword_type), POINTER                        :: keyword
      TYPE(section_type), POINTER                        :: subsection

      CPASSERT(.NOT. ASSOCIATED(section))
      CALL section_create(section, __LOCATION__, name="potentiostat", &
                          description="Has control over potential.") 
                          
      NULLIFY (keyword,subsection) 
      CALL keyword_create(keyword, __LOCATION__, name="PHI_0", &
                          description="Specify the initial potential", &
                          default_r_val=0.0_dp)
      CALL section_add_keyword(section, keyword)
      CALL keyword_release(keyword)
      
      CALL keyword_create(keyword, __LOCATION__, name="C_0", &
                          description="Specify the initial capacitance", &
                          default_r_val=0.0_dp)
      CALL section_add_keyword(section, keyword)
      CALL keyword_release(keyword)
      
      CALL keyword_create(keyword, __LOCATION__, name="POTENTIOSTAT_PRESENT", &
                          description="Specify the initial potential", &
                          default_l_val=.FALSE.)
      CALL section_add_keyword(section, keyword)
      CALL keyword_release(keyword)

   END SUBROUTINE create_potentiostat_section

END MODULE input_cp2k_potentiostats
