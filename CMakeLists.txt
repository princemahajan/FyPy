#    ______      ____       
#   / ____/_  __/ __ \__  __
#  / /_  / / / / /_/ / / / /
# / __/ / /_/ / ____/ /_/ / 
#/_/    \__, /_/    \__, /  
#      /____/      /____/   
                                                  
# Author: Bharat Mahajan

cmake_minimum_required(VERSION 3.12)

###################
# FyPy Build Setup 
###################

project(FyPy VERSION 0.1 LANGUAGES Fortran)
enable_language(Fortran)

# set major and minor version
set(FYPY_MAJOR_VERSION 0)
set(FYPY_MINOR_VERSION 1)
set(FYPY_VERSION  ${FYPY_MAJOR_VERSION}.${FYPY_MINOR_VERSION})

# CMake module directory
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

# Default installation path
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX ${CMAKE_CURRENT_LIST_DIR} CACHE PATH "..." FORCE)
endif()

# Prompt for user options

# Ask for the directory to install FyPy lib
set(FYPY_INSTALL_LIB_DIR lib/FyPy CACHE PATH "Installation directory for FyPy library")

# Ask for the directory to install TestFyPy python module
set(FYPY_INSTALL_BIN_DIR bin CACHE PATH "Installation directory for TestFyPy test python module")

# Make relative paths absolute
foreach(p LIB BIN)
  set(var FYPY_INSTALL_${p}_DIR)
  if(NOT IS_ABSOLUTE "${${var}}")
    set(${var} "${CMAKE_INSTALL_PREFIX}/${${var}}")
  endif()
endforeach()

# Lib dir should contain all the .mod files 
set(CMAKE_Fortran_MODULE_DIRECTORY ${FYPY_INSTALL_LIB_DIR})

# Python library and NumPy static libraries to link agains
set(CPYTHON_LIB_DIR  "C:/Users/bmahajan/Anaconda3/libs")
set(NUMPY_LIB_DIR  "C:/Users/bmahajan/Anaconda3/pkgs/numpy-base-1.18.1-py37hc3f5095_1/Lib/site-packages/numpy/core/lib")

link_directories(${CPYTHON_LIB_DIR} ${NUMPY_LIB_DIR})

###############
# FyPy Targets
###############

set(FYPYBIN FyPy)


# Define the static library target
file(GLOB FYPY_SRC src/*.F90)
add_library(${FYPYBIN} STATIC ${FYPY_SRC})
target_include_directories(${FYPYBIN} INTERFACE ${FYPY_INSTALL_LIB_DIR})

# Find and link Intel MKL Library (Needed for FMUTIL)
message(STATUS "Linking Intel MKL (Needed for FMUTIL)...")
find_package(IntelMKL REQUIRED)
if(IntelMKL_FOUND)
    target_link_libraries(${FYPYBIN} IntelMKL::IntelMKL)    
endif()

# Link FMUTIL
message(STATUS "Linking FMUTIL...")
find_package(FMUTIL REQUIRED 0.1 CONFIG 
    PATHS "${CMAKE_LIBS_DIR}/FMUTIL" NO_CMAKE_PACKAGE_REGISTRY)
target_link_libraries(${FYPYBIN} FMUTIL::FMUTIL)

# The test python module 
file(GLOB TestFYPY_SRC src/tests/*.F90)
set(TESTFYPYBIN TestFyPy)
add_library(${TESTFYPYBIN} MODULE ${TestFYPY_SRC})

# link
target_link_libraries(${TESTFYPYBIN} 
            ${FYPYBIN} ${CPYTHON_LIB_DIR}/python37.lib ${NUMPY_LIB_DIR}/npymath.lib)

# tell cmake to stop prepending anything to lib names
set_target_properties(${FYPYBIN} ${TESTFYPYBIN} PROPERTIES PREFIX "")
set_target_properties(${TESTFYPYBIN} PROPERTIES SUFFIX ".pyd")

###########################
# Compile Options for FLINT
###########################

include(CompilerOptions)

# Thee options are for gfortran
set(COption "-std=f2008ts")
GetPlatformCompilerFlag("${COption}" FALSE Fortran)
add_compile_options("${_Flag}")
set(COption "-ffree-line-length-0")
GetPlatformCompilerFlag("${COption}" FALSE Fortran)
add_compile_options("${_Flag}")

# preprocessor flags
set(COption "/fpp" "-cpp")
GetPlatformCompilerFlag("${COption}" FALSE Fortran)
target_compile_options(${FYPYBIN} PUBLIC "${_Flag}")
target_compile_options(${TESTFYPYBIN} PUBLIC "${_Flag}")


# Add ifort specific options

# Data alignment always
set(COption "/align:all" "-align all")
GetPlatformCompilerFlag("${COption}" FALSE Fortran)
target_compile_options(${FYPYBIN} PUBLIC "${_Flag}")

# Traceback information
set(COption "/traceback" "-traceback")
GetPlatformCompilerFlag("${COption}" FALSE Fortran)
target_compile_options(${FYPYBIN} PRIVATE ${_Flag})


#######################
# Debug Compile Options
#######################

# No optimization
set(COption "/Od" "-O0")
GetPlatformCompilerFlag("${COption}" TRUE Fortran)
target_compile_options(${FYPYBIN} PRIVATE $<$<CONFIG:Debug>:${_Flag}>)

# Generate full debug information, passed to the linker
set(COption "/debug:full" "-debug full")
GetPlatformCompilerFlag("${COption}" FALSE Fortran)
target_compile_options(${FYPYBIN} PRIVATE $<$<CONFIG:Debug>:${_Flag}>)

# Generate full debug information in object or PDB file, passed to compiler
set(COption "/Zi" "-g2")
GetPlatformCompilerFlag("${COption}" FALSE Fortran)
target_compile_options(${FYPYBIN} PRIVATE $<$<CONFIG:Debug>:${_Flag}>)

# Debug information for Parameters
set(COption "/debug-parameters" "-debug-parameters")
GetPlatformCompilerFlag("${COption}" FALSE Fortran)
target_compile_options(${FYPYBIN} PRIVATE $<$<CONFIG:Debug>:${_Flag}>)

# Array bounds checking
set(COption "/check:all" "-check all" "-fcheck=all")
GetPlatformCompilerFlag("${COption}" TRUE Fortran)
target_compile_options(${FYPYBIN} PRIVATE $<$<CONFIG:Debug>:${_Flag}>)


#########################
# Release Compile Options
#########################

# Aggressive optimization for maximum speed: includes bunch of options
# Make sure we also set the appropriat Intel instruction set to use after this
set(COption "/fast" "-fast")
GetPlatformCompilerFlag("${COption}" FALSE Fortran)
target_compile_options(${FYPYBIN} PRIVATE $<$<CONFIG:Release>:${_Flag}>)

# Most optimization enabled
set(COption "/O3" "-O3")
GetPlatformCompilerFlag("${COption}" TRUE Fortran)
target_compile_options(${FYPYBIN} PRIVATE $<$<CONFIG:Release>:${_Flag}>)

# Enable auto-parallelizer
set(COption "/Qparallel" "-parallel")
GetPlatformCompilerFlag("${COption}" FALSE Fortran)
target_compile_options(${FYPYBIN} PRIVATE $<$<CONFIG:Release>:${_Flag}>)



############################
# Install and Export Targets
############################


install(TARGETS ${FYPYBIN}
  # IMPORTANT: Add the FLINT library to the "export-set"
  EXPORT FyPyTargets
  LIBRARY DESTINATION "${FYPY_INSTALL_LIB_DIR}" COMPONENT lib
  ARCHIVE DESTINATION "${FYPY_INSTALL_LIB_DIR}" COMPONENT lib
  COMPONENT dev)

install(TARGETS ${TESTFYPYBIN}
  # IMPORTANT: Add the TEstFyPy python module to the "export-set"
  EXPORT FyPyTargets
  RUNTIME DESTINATION "${FYPY_INSTALL_BIN_DIR}" COMPONENT bin
  ARCHIVE DESTINATION "${FYPY_INSTALL_BIN_DIR}" COMPONENT lib
  LIBRARY DESTINATION "${FYPY_INSTALL_BIN_DIR}" COMPONENT lib)

# Build-tree export set
export(EXPORT FyPyTargets 
    NAMESPACE FyPy::
    FILE "${FYPY_INSTALL_LIB_DIR}/cmake/FyPyTargets.cmake")

##########################
# Config and Version Files
##########################

include(CMakePackageConfigHelpers)

# version file
write_basic_package_version_file(
    ${CMAKE_CURRENT_BINARY_DIR}/FyPyConfigVersion.cmake
    VERSION ${FYPY_VERSION}
    COMPATIBILITY AnyNewerVersion
)

configure_package_config_file(${CMAKE_CURRENT_LIST_DIR}/cmake/FyPyConfig.cmake.in 
        ${CMAKE_CURRENT_BINARY_DIR}/FyPyConfig.cmake
        INSTALL_DESTINATION cmake
        INSTALL_PREFIX ${FYPY_INSTALL_LIB_DIR})

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/FyPyConfig.cmake
        ${CMAKE_CURRENT_BINARY_DIR}/FyPyConfigVersion.cmake
        DESTINATION ${FYPY_INSTALL_LIB_DIR}/cmake)


