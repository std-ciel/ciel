add_subdirectory(googletest)
include_directories(${gtest_SOURCE_DIR}/include ${gtest_SOURCE_DIR})

# Add unit tests for symbol_table and type_factory
add_executable(unit_tests
    test_type_factory.cpp
    test_symbol_table.cpp
    test_mangling.cpp
)

# Link with GoogleTest and the project libraries
target_link_libraries(unit_tests gtest gtest_main ciel_lexer)

# Include project directories
target_include_directories(unit_tests PRIVATE ${CMAKE_SOURCE_DIR}/include)

# Add the test to CTest
add_test(NAME unit_tests COMMAND unit_tests)


file(GLOB VALID_SAMPLE_FILES
    RELATIVE ${CMAKE_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/tests/testcases/valid/*.ciel
)

foreach(sample_rel ${VALID_SAMPLE_FILES})
    get_filename_component(sample_abs "${CMAKE_SOURCE_DIR}/${sample_rel}" ABSOLUTE)
    get_filename_component(sample_name "${sample_abs}" NAME_WE)

    add_test(
        NAME lexer_valid_${sample_name}
        COMMAND $<TARGET_FILE:cielc> -l ${sample_abs}
    )

    add_test(
        NAME parser_valid_${sample_name}
        COMMAND $<TARGET_FILE:cielc> -p ${sample_abs}
    )
endforeach()


file(GLOB INVALID_SAMPLE_FILES
    RELATIVE ${CMAKE_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/tests/testcases/invalid/*.ciel
)

foreach(sample_rel ${INVALID_SAMPLE_FILES})
    get_filename_component(sample_abs "${CMAKE_SOURCE_DIR}/${sample_rel}" ABSOLUTE)
    get_filename_component(sample_name "${sample_abs}" NAME_WE)

    add_test(
        NAME lexer_invalid_${sample_name}
        COMMAND $<TARGET_FILE:cielc> -l ${sample_abs}
    )

    set_tests_properties(lexer_invalid_${sample_name} PROPERTIES WILL_FAIL TRUE)

    add_test(
        NAME parser_invalid_${sample_name}
        COMMAND $<TARGET_FILE:cielc> -p ${sample_abs}
    )
    set_tests_properties(parser_invalid_${sample_name} PROPERTIES WILL_FAIL TRUE)
endforeach()
