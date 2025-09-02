cmake_minimum_required(VERSION 3.10)

project(tkom_lekser)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

add_definitions(-D_SILENCE_CXX17_CODECVT_HEADER_DEPRECATION_WARNING)

# find_package(Boost REQUIRED COMPONENTS unit_test_framework)

file(GLOB_RECURSE LEXER_SOURCES ${CMAKE_SOURCE_DIR}/src/lexer/*.cc ${CMAKE_SOURCE_DIR}/src/lexer/*.h ${CMAKE_SOURCE_DIR}/src/lexer/*.hpp)
file(GLOB_RECURSE READER_SOURCES ${CMAKE_SOURCE_DIR}/src/reader/*.cc ${CMAKE_SOURCE_DIR}/src/reader/*.h ${CMAKE_SOURCE_DIR}/src/reader/*.hpp)
file(GLOB_RECURSE PARSER_SOURCES ${CMAKE_SOURCE_DIR}/src/parser/*.cc ${CMAKE_SOURCE_DIR}/src/parser/*.h ${CMAKE_SOURCE_DIR}/src/parser/*.hpp)
file(GLOB_RECURSE INTERPRETER_SOURCES ${CMAKE_SOURCE_DIR}/src/interpreter/*.cc ${CMAKE_SOURCE_DIR}/src/interpreter/*.h ${CMAKE_SOURCE_DIR}/src/interpreter/*.hpp)

add_library(lexer_lib ${LEXER_SOURCES})
add_library(reader_lib ${READER_SOURCES})
add_library(parser_lib ${PARSER_SOURCES})
add_library(interpreter_lib ${INTERPRETER_SOURCES})

set(ALL_INCLUDE_DIRS
    ${CMAKE_SOURCE_DIR}/src/lexer
    ${CMAKE_SOURCE_DIR}/src/reader
    ${CMAKE_SOURCE_DIR}/src/parser
    ${CMAKE_SOURCE_DIR}/src/interpreter
)

target_include_directories(lexer_lib PUBLIC ${ALL_INCLUDE_DIRS})
target_include_directories(reader_lib PUBLIC ${ALL_INCLUDE_DIRS})
target_include_directories(parser_lib PUBLIC ${ALL_INCLUDE_DIRS})
target_include_directories(interpreter_lib PUBLIC ${ALL_INCLUDE_DIRS})


add_executable(main ${CMAKE_SOURCE_DIR}/src/main.cc)
target_link_libraries(main PRIVATE lexer_lib reader_lib parser_lib interpreter_lib)

add_executable(lexer_tests ${CMAKE_SOURCE_DIR}/tests/lexer_tests.cc)
target_link_libraries(lexer_tests PRIVATE lexer_lib reader_lib)

add_executable(parser_tests ${CMAKE_SOURCE_DIR}/tests/parser_tests.cc)
target_link_libraries(parser_tests PRIVATE lexer_lib reader_lib parser_lib interpreter_lib)

add_executable(interpreter_tests ${CMAKE_SOURCE_DIR}/tests/interpreter_tests.cc)
target_link_libraries(interpreter_tests PRIVATE lexer_lib reader_lib parser_lib interpreter_lib)
