cmake_minimum_required(VERSION 3.13)

project(codecrafters-interpreter)

file(GLOB_RECURSE SOURCE_FILES src/*.c)

set(CMAKE_C_STANDARD 23) # Enable the C23 standard

add_executable(interpreter ${SOURCE_FILES})
