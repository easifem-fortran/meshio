# This program is a part of EASIFEM version 2023.1.0 Copyright (C) 2020-2023
# Vikas Sharma Website: www.easifem.com
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <https: //www.gnu.org/licenses/>
#
cmake_minimum_required(VERSION 3.20.0 FATAL_ERROR)
set(PROJECT_NAME "meshio")
project(${PROJECT_NAME})
enable_language(Fortran)
set(TARGET_NAME "meshio")

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE
      Debug
      CACHE STRING "Build type" FORCE)
endif()

if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" OR Fortran_COMPILER_NAME MATCHES
                                                  "gfortran*")
  list(APPEND FORTRAN_FLAGS "-ffree-form" "-ffree-line-length-none"
       "-std=f2008" "-fimplicit-none")
  list(APPEND FORTRAN_FLAGS_RELEASE "-O3")
  list(
    APPEND
    FORTRAN_FLAGS_DEBUG
    "-fbounds-check"
    "-g"
    "-fbacktrace"
    "-Wextra"
    "-Wall"
    "-fprofile-arcs"
    "-ftest-coverage"
    "-Wimplicit-interface")

elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" OR Fortran_COMPILER_NAME
                                                        MATCHES "ifort*")
  list(APPEND FORTRAN_FLAGS "-r8" "-W1")
  list(APPEND FORTRAN_FLAGS_RELEASE "-O3")
  list(
    APPEND
    FORTRAN_FLAGS_DEBUG
    "-O0"
    "-traceback"
    "-g"
    "-debug all"
    "-check all"
    "-ftrapuv"
    "-warn"
    "nointerfaces")

elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "XL" OR Fortran_COMPILER_NAME
                                                     MATCHES "xlf*")
  list(APPEND FORTRAN_FLAGS "-q64" "-qrealsize=8" "-qsuffix=f=f90:cpp=f90")
  list(APPEND FORTRAN_FLAGS_RELEASE "-O3" "-qstrict")
  list(APPEND FORTRAN_FLAGS_DEBUG "-O0" "-g" "-qfullpath" "-qkeepparm")

else()
  message(ERROR "No optimized Fortran compiler flags are known")
endif()

list(APPEND CMAKE_PREFIX_PATH "$ENV{EASIFEM_EXTPKGS}" "$ENV{EASIFEM_BASE}"
     "$ENV{EASIFEM_CLASSES}")

find_package(easifemClasses)

add_executable(${TARGET_NAME} main.F90)

target_link_libraries(${TARGET_NAME} easifemClasses::easifemClasses)
