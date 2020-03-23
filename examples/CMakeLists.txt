include(CTest)

include_directories(${CMAKE_BINARY_DIR}/src)
message(STATUS "example include: ${CMAKE_BINARY_DIR}/src")

function(make_example example_name)
  add_test(NAME ${example_name} COMMAND ${example_name})
  add_executable(${example_name} ${example_name}.f90 testing_helpers.f90)
  target_link_libraries(${example_name} ${bmiprms6surface_lib})
endfunction(make_example)

make_example(irf_ex)

file(
  COPY ${CMAKE_SOURCE_DIR}/projects/pipestem/
  DESTINATION ${CMAKE_BINARY_DIR}/examples)
