cmake_minimum_required(VERSION 2.8.12)
if(POLICY CMP0042)
	cmake_policy(SET CMP0042 NEW)
endif()
if(POLICY CMP0028)
	cmake_policy(SET CMP0028 NEW)
endif()
project(cparser C)
set(cparser_VERSION "1.22.0")

# Enable C99
if(CMAKE_VERSION VERSION_LESS "3.1")
	if(CMAKE_C_COMPILER_ID STREQUAL "GNU")
		set(CMAKE_C_FLAGS "-std=c99 ${CMAKE_C_FLAGS}")
	endif()
else()
	set(CMAKE_C_STANDARD 99)
endif()

set(SOURCES
	src/adt/pset_new.c
	src/adt/strutil.c
	src/ast/ast.c
	src/ast/attribute.c
	src/ast/constfold.c
	src/ast/entity.c
	src/ast/printer.c
	src/ast/string_hash.c
	src/ast/string_rep.c
	src/ast/symbol_table.c
	src/ast/type.c
	src/ast/type_hash.c
	src/ast/types.c
	src/ast/walk.c
	src/driver/actions.c
	src/driver/c_driver.c
	src/driver/diagnostic.c
	src/driver/driver.c
	src/driver/help.c
	src/driver/machine_triple.c
	src/driver/options.c
	src/driver/predefs.c
	src/driver/target.c
	src/driver/tempfile.c
	src/driver/timing.c
	src/driver/warning.c
	src/firm/ast2firm.c
	src/firm/entitymap.c
	src/firm/firm_opt.c
	src/firm/jump_target.c
	src/firm/mangle.c
	src/main.c
	src/parser/builtins.c
	src/parser/format_check.c
	src/parser/input.c
	src/parser/parser.c
	src/parser/preprocessor.c
	src/parser/token.c
	src/wrappergen/write_compoundsizes.c
	src/wrappergen/write_fluffy.c
	src/wrappergen/write_jna.c
)
include_directories(src)

# Need libfirm of the same version as cparser
find_package(libfirm ${cparser_VERSION} REQUIRED)
include_directories(${libfirm_INCLUDE_DIRS})

add_executable(cparser ${SOURCES})
target_link_libraries(cparser libfirm::firm)
if(UNIX)
	target_link_libraries(cparser m)
endif()

set(DEFAULT_SYSTEM_INCLUDE_DIR /usr/include)
if(APPLE)
	execute_process(COMMAND xcrun -show-sdk-path
		OUTPUT_VARIABLE SDK_PATH
		OUTPUT_STRIP_TRAILING_WHITESPACE
	)
	set(DEFAULT_SYSTEM_INCLUDE_DIR ${SDK_PATH}/usr/include)
endif()
set(SYSTEM_INCLUDE_DIR ${DEFAULT_SYSTEM_INCLUDE_DIR} CACHE PATH "system include directory")
set(LOCAL_INCLUDE_DIR /usr/local CACHE PATH "system local include directory")
set(COMPILER_INCLUDE_DIR lib/cparser/${cparser_VERSION} CACHE STRING "compiler include dir (under CMAKE_INSTALL_PREFIX)")

add_definitions(-DSYSTEM_INCLUDE_DIR=\"${SYSTEM_INCLUDE_DIR}\")
add_definitions(-DLOCAL_INCLUDE_DIR=\"${LOCAL_INCLUDE_DIR}\")
add_definitions(-DCOMPILER_INCLUDE_DIR=\"${CMAKE_INSTALL_PREFIX}/${COMPILER_INCLUDE_DIR}\")

# Produce revision.h
include_directories(${CMAKE_CURRENT_BINARY_DIR})
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/.git/HEAD)
	set(REVISIONH "${CMAKE_CURRENT_BINARY_DIR}/revision.h")
	set(REVGEN echo '\#define cparser_REVISION \"'`git describe --abbrev=40 --always --dirty --match ''`'\"')
	add_custom_command(
		OUTPUT ${REVISIONH}
		DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/.git/HEAD
		COMMAND ${REVGEN} | cmp -s - "${REVISIONH}" 2> /dev/null || ${REVGEN} > "${REVISIONH}"
		WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	)
	SET_SOURCE_FILES_PROPERTIES(src/driver/actions.c PROPERTIES OBJECT_DEPENDS ${REVISIONH})
else()
	file(GENERATE OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/revision.h
		 CONTENT "#define cparser_REVISION \"\"")
endif()

# Installation
set(INSTALL_HEADERS
	include/float.h
	include/iso646.h
	include/limits.h
	include/stdarg.h
	include/stdbool.h
	include/stddef.h
)
install(FILES ${INSTALL_HEADERS} DESTINATION ${COMPILER_INCLUDE_DIR})
install(TARGETS cparser DESTINATION bin)
