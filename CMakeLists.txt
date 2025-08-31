cmake_minimum_required(VERSION 3.15)

project(ciel)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Define release type flags
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Werror -Wpedantic -Wfatal-errors")
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
)
FLEX_TARGET(CielLexer src/lexer/lex.l ${CMAKE_CURRENT_BINARY_DIR}/lex.yy.cc)
ADD_FLEX_BISON_DEPENDENCY(CielLexer CielParser)

add_library(ciel_lexer STATIC
    ${FLEX_CielLexer_OUTPUTS}
    ${BISON_CielParser_OUTPUTS}
    src/token/token_type.cpp
    src/lexer/print_tokens.cpp
    src/parser/token_converter.cpp
    src/symbol_table/symbol_table.cpp
    src/symbol_table/type.cpp
)
target_include_directories(ciel_lexer PUBLIC
    ${CMAKE_SOURCE_DIR}/include/token
    ${CMAKE_SOURCE_DIR}/include/lexer
    ${CMAKE_SOURCE_DIR}/include/parser
    ${CMAKE_CURRENT_BINARY_DIR})
target_compile_definitions(ciel_lexer PUBLIC )

add_executable(cielc src/main.cpp)
target_link_libraries(cielc PRIVATE ciel_lexer argparse)

if(UNIT_TESTING)
  enable_testing()
  message(STATUS "Build unit tests for the project. Tests should be found in the tests folder\n")
  add_subdirectory(tests)
endif()
