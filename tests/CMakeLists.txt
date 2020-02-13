include(CTest)

include_directories(${CMAKE_BINARY_DIR}/src)
message(STATUS "test include: ${CMAKE_BINARY_DIR}/src")

function(make_test test_name)
  add_test(NAME ${test_name} COMMAND ${test_name})
  add_executable(${test_name} ${test_name}.f90 fixtures.f90)
  target_link_libraries(${test_name} ${bmiprms6surface_lib})
endfunction(make_test)

make_test(test_get_component_name)
make_test(test_get_input_item_count)
make_test(test_get_output_item_count)
make_test(test_get_input_var_names)
make_test(test_get_output_var_names)
make_test(test_initialize)
make_test(test_finalize)
make_test(test_update)
make_test(test_update_until)

file(
  COPY ${CMAKE_SOURCE_DIR}/projects/pipestem/
  DESTINATION ${CMAKE_BINARY_DIR}/tests)
