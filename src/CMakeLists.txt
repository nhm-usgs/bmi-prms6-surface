add_library(${bmiprms6surface_lib} SHARED
  bmi.f90 bmi_prms_surface.f90 m_prms_surface.f90)
target_link_libraries(${bmiprms6surface_lib} ${EXTRA_LIBS})

install(
  TARGETS ${bmiprms6surface_lib}
  EXPORT ${bmiprms6surface_lib}-config
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
  INCLUDES DESTINATION include)

install(
  DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/
  DESTINATION include
  FILES_MATCHING PATTERN "*.mod"
  PATTERN "CMakeFiles" EXCLUDE)

install(
  EXPORT ${bmiprms6surface_lib}-config
  DESTINATION lib/cmake)
export(
  TARGETS ${bmiprms6surface_lib}
  FILE ${bmiprms6_lib}-config.cmake)
