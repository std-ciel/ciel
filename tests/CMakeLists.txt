add_subdirectory(googletest)
include_directories(${gtest_SOURCE_DIR}/include ${gtest_SOURCE_DIR})


file(GLOB VALID_SAMPLE_FILES
    RELATIVE ${CMAKE_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/tests/testcases/valid/*.ciel
)

foreach(sample_rel ${VALID_SAMPLE_FILES})
    get_filename_component(sample_abs "${CMAKE_SOURCE_DIR}/${sample_rel}" ABSOLUTE)
    get_filename_component(sample_name "${sample_abs}" NAME_WE)

    add_test(
        NAME lexer_valid_${sample_name}
        COMMAND $<TARGET_FILE:cielc> ${sample_abs}
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
        COMMAND $<TARGET_FILE:cielc> ${sample_abs}
    )

    set_tests_properties(lexer_invalid_${sample_name} PROPERTIES WILL_FAIL TRUE)
endforeach()
