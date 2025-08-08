add_subdirectory(googletest)
include_directories(${gtest_SOURCE_DIR}/include ${gtest_SOURCE_DIR})

find_package(FLEX REQUIRED)

FLEX_TARGET(
    CIEL_SCANNER
    ${CMAKE_SOURCE_DIR}/src/lexer/lex.l
    ${CMAKE_CURRENT_BINARY_DIR}/lexer.cpp
)

add_library(ciel_lexer_obj OBJECT
    ${FLEX_CIEL_SCANNER_OUTPUTS}
    ${CMAKE_SOURCE_DIR}/src/utils/token_type.cpp
)

target_include_directories(ciel_lexer_obj PRIVATE
    ${CMAKE_SOURCE_DIR}/src
    ${CMAKE_SOURCE_DIR}/src/utils
    ${CMAKE_SOURCE_DIR}/src/lexer
)

add_executable(lexer_tests
    ${CMAKE_SOURCE_DIR}/tests/lexer_tests.cpp
    $<TARGET_OBJECTS:ciel_lexer_obj>
)

set_target_properties(lexer_tests PROPERTIES
    CXX_STANDARD 20
    CXX_STANDARD_REQUIRED YES
)

target_link_libraries(lexer_tests PRIVATE gtest_main)

enable_testing()
add_test(NAME lexer_tests COMMAND lexer_tests)
