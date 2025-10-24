add_subdirectory(googletest)
include_directories(${gtest_SOURCE_DIR}/include ${gtest_SOURCE_DIR})

# Add unit tests for symbol_table and type_factory
add_executable(unit_tests
    test_type_factory.cpp
    test_symbol_table.cpp
    test_mangling.cpp
)

# Link with GoogleTest and the project libraries
target_link_libraries(unit_tests gtest gtest_main ciel_parser)

# Include project directories
target_include_directories(unit_tests PRIVATE ${CMAKE_SOURCE_DIR}/include)

# Add the test to CTest
add_test(NAME unit_tests COMMAND unit_tests)


# Collect valid test files from root and subdirectories
file(GLOB_RECURSE VALID_SAMPLE_FILES
    RELATIVE ${CMAKE_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/tests/testcases/valid/*.ciel
)

foreach(sample_rel ${VALID_SAMPLE_FILES})
    get_filename_component(sample_abs "${CMAKE_SOURCE_DIR}/${sample_rel}" ABSOLUTE)
    get_filename_component(sample_name "${sample_abs}" NAME_WE)

    # Get the directory path relative to valid/ for better test naming
    string(REGEX REPLACE "^tests/testcases/valid/" "" rel_path "${sample_rel}")
    string(REGEX REPLACE "/[^/]+\\.ciel$" "" test_category "${rel_path}")
    string(REGEX REPLACE "/" "_" test_category_clean "${test_category}")

    # Create test name with category prefix if in subdirectory
    if(test_category_clean STREQUAL "")
        set(test_prefix "")
    else()
        set(test_prefix "${test_category_clean}_")
    endif()

    add_test(
        NAME lexer_valid_${test_prefix}${sample_name}
        COMMAND $<TARGET_FILE:cielc> -l ${sample_abs}
    )

    add_test(
        NAME parser_valid_${test_prefix}${sample_name}
        COMMAND $<TARGET_FILE:cielc> -p ${sample_abs}
    )

    add_test(
        NAME irgen_valid_${test_prefix}${sample_name}
        COMMAND $<TARGET_FILE:cielc> -i ${sample_abs}
    )

endforeach()


file(GLOB LEXICAL_ERROR_FILES
    RELATIVE ${CMAKE_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/tests/testcases/invalid/lexical/*.ciel
)

foreach(sample_rel ${LEXICAL_ERROR_FILES})
    get_filename_component(sample_abs "${CMAKE_SOURCE_DIR}/${sample_rel}" ABSOLUTE)
    get_filename_component(sample_name "${sample_abs}" NAME_WE)

    add_test(
        NAME lexer_invalid_lexical_${sample_name}
        COMMAND $<TARGET_FILE:cielc> -l ${sample_abs}
    )
    set_tests_properties(lexer_invalid_lexical_${sample_name} PROPERTIES WILL_FAIL TRUE)

    add_test(
        NAME parser_invalid_lexical_${sample_name}
        COMMAND $<TARGET_FILE:cielc> -p ${sample_abs}
    )
    set_tests_properties(parser_invalid_lexical_${sample_name} PROPERTIES WILL_FAIL TRUE)
endforeach()


# Semantic error tests - lexer passes but parser semantic checks fail
file(GLOB SEMANTIC_ERROR_FILES
    RELATIVE ${CMAKE_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/tests/testcases/invalid/semantic/*.ciel
)

foreach(sample_rel ${SEMANTIC_ERROR_FILES})
    get_filename_component(sample_abs "${CMAKE_SOURCE_DIR}/${sample_rel}" ABSOLUTE)
    get_filename_component(sample_name "${sample_abs}" NAME_WE)

    add_test(
        NAME parser_invalid_semantic_${sample_name}
        COMMAND $<TARGET_FILE:cielc> -p ${sample_abs}
    )
    # Parser should fail for semantic errors
    set_tests_properties(parser_invalid_semantic_${sample_name} PROPERTIES WILL_FAIL TRUE)
endforeach()
