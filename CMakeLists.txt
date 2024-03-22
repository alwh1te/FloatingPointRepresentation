cmake_minimum_required(VERSION 3.27)
project(FloatingPoint C)

set(CMAKE_C_STANDARD 23)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Weverything")

add_executable(FloatingPoint main.c
        return_codes.h)
