cmake_minimum_required(VERSION 3.1.0)
project(assign2)
set(CMAKE_CXX_STANDARD 17)
find_package(LLVM REQUIRED CONFIG HINTS ${LLVM_DIR} ${LLVM_DIR}/lib/cmake/llvm
	                NO_DEFAULT_PATH)

include_directories(${LLVM_INCLUDE_DIRS} ${CLANG_INCLUDE_DIRS} SYSTEM)
link_directories(${LLVM_LIBRARY_DIRS})
message(STATUS "LLVM_LIB DIR : ${LLVM_LIBRARY_DIRS}")
set(LLVM_LINK_COMPONENTS
  LLVMCore
  LLVMIRReader
  LLVMPasses
  )

message(STATUS "LLVM LIBS : ${LLVM_LINK_COMPONENTS}")
# Support plugins.

set(ASSIGNMENT_VIEW on)
if(${ASSIGNMENT_VIEW})
add_definitions(-DASSIGNMENT_VIEW)
endif()

add_executable(llvmassignment
  LLVMAssignment.cpp
  )

target_link_libraries(llvmassignment
	${LLVM_LINK_COMPONENTS}
	)
