cmake_minimum_required(VERSION 3.15)

project(ciel)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Define release type flags
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Werror -Wpedantic -Wfatal-errors -Wno-unused-but-set-variable")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-missing-braces -Wno-unused-parameter")
endif()

if (CMAKE_BUILD_TYPE STREQUAL "Release")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3")
endif()

find_package(FLEX REQUIRED)
find_package(BISON REQUIRED)

add_subdirectory(vendor)

include_directories(${CMAKE_SOURCE_DIR}/include)
include_directories(${CMAKE_CURRENT_BINARY_DIR})

BISON_TARGET(CielParser src/parser/parser.y ${CMAKE_CURRENT_BINARY_DIR}/parser.cpp
    DEFINES_FILE ${CMAKE_CURRENT_BINARY_DIR}/parser.hpp
    COMPILE_FLAGS "--locations"
)
FLEX_TARGET(CielLexer src/lexer/lex.l ${CMAKE_CURRENT_BINARY_DIR}/lex.yy.cc)

ADD_FLEX_BISON_DEPENDENCY(CielLexer CielParser)

add_library(ciel_parser STATIC
    ${FLEX_CielLexer_OUTPUTS}
    ${BISON_CielParser_OUTPUTS}
    src/token/token_type.cpp
    src/lexer/print_tokens.cpp
    src/parser/token_converter.cpp
    src/parser/parser_errors.cpp
    src/parser/parser_helper.cpp
    src/symbol_table/symbol_table.cpp
    src/symbol_table/symbol.cpp
    src/symbol_table/type.cpp
    src/symbol_table/type_factory.cpp
    src/symbol_table/target_layout.cpp
    src/symbol_table/mangling.cpp
    src/passes/local_static_pass.cpp
    src/passes/layout_pass.cpp
    src/tac/tac.cpp
    src/tac/tac_generator.cpp
)

target_include_directories(ciel_parser PUBLIC
    ${CMAKE_SOURCE_DIR}/include/token
    ${CMAKE_SOURCE_DIR}/include/lexer
    ${CMAKE_SOURCE_DIR}/include/parser
    ${CMAKE_SOURCE_DIR}/include/symbol_table
    ${CMAKE_SOURCE_DIR}/include/local_static_pass
    ${CMAKE_SOURCE_DIR}/include/layout_pass
    ${CMAKE_SOURCE_DIR}/include/tac
    ${CMAKE_SOURCE_DIR}/include/ast
    ${CMAKE_CURRENT_BINARY_DIR})

target_compile_definitions(ciel_parser PUBLIC )

add_executable(cielc src/main.cpp)
target_link_libraries(cielc PRIVATE ciel_parser argparse)

target_include_directories(cielc PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${CMAKE_CURRENT_SOURCE_DIR}/include/parser
    ${CMAKE_CURRENT_SOURCE_DIR}/include/tac
)

if(UNIT_TESTING)
  enable_testing()
  message(STATUS "Build unit tests for the project. Tests should be found in the tests folder\n")
  add_subdirectory(tests)
endif()
