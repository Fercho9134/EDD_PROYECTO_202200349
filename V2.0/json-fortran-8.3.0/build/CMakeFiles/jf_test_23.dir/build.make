# CMAKE generated file: DO NOT EDIT!
# Generated by "MinGW Makefiles" Generator, CMake Version 3.29

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Disable VCS-based implicit rules.
% : %,v

# Disable VCS-based implicit rules.
% : RCS/%

# Disable VCS-based implicit rules.
% : RCS/%,v

# Disable VCS-based implicit rules.
% : SCCS/s.%

# Disable VCS-based implicit rules.
% : s.%

.SUFFIXES: .hpux_make_needs_suffix_list

# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

#Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

SHELL = cmd.exe

# The CMake executable.
CMAKE_COMMAND = "C:\Program Files\CMake\bin\cmake.exe"

# The command to remove a file.
RM = "C:\Program Files\CMake\bin\cmake.exe" -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = "C:\Users\ferch\Desktop\Proyecto 1 EDD\V2.0\json-fortran-8.3.0"

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = "C:\Users\ferch\Desktop\Proyecto 1 EDD\V2.0\json-fortran-8.3.0\build"

# Include any dependencies generated for this target.
include CMakeFiles/jf_test_23.dir/depend.make
# Include any dependencies generated by the compiler for this target.
include CMakeFiles/jf_test_23.dir/compiler_depend.make

# Include the progress variables for this target.
include CMakeFiles/jf_test_23.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/jf_test_23.dir/flags.make

CMakeFiles/jf_test_23.dir/src/tests/jf_test_23.F90.obj: CMakeFiles/jf_test_23.dir/flags.make
CMakeFiles/jf_test_23.dir/src/tests/jf_test_23.F90.obj: CMakeFiles/jf_test_23.dir/includes_Fortran.rsp
CMakeFiles/jf_test_23.dir/src/tests/jf_test_23.F90.obj: C:/Users/ferch/Desktop/Proyecto\ 1\ EDD/V2.0/json-fortran-8.3.0/src/tests/jf_test_23.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green --progress-dir="C:\Users\ferch\Desktop\Proyecto 1 EDD\V2.0\json-fortran-8.3.0\build\CMakeFiles" --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object CMakeFiles/jf_test_23.dir/src/tests/jf_test_23.F90.obj"
	C:\TDM-GCC-64\bin\gfortran.exe $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c "C:\Users\ferch\Desktop\Proyecto 1 EDD\V2.0\json-fortran-8.3.0\src\tests\jf_test_23.F90" -o CMakeFiles\jf_test_23.dir\src\tests\jf_test_23.F90.obj

CMakeFiles/jf_test_23.dir/src/tests/jf_test_23.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green "Preprocessing Fortran source to CMakeFiles/jf_test_23.dir/src/tests/jf_test_23.F90.i"
	C:\TDM-GCC-64\bin\gfortran.exe $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E "C:\Users\ferch\Desktop\Proyecto 1 EDD\V2.0\json-fortran-8.3.0\src\tests\jf_test_23.F90" > CMakeFiles\jf_test_23.dir\src\tests\jf_test_23.F90.i

CMakeFiles/jf_test_23.dir/src/tests/jf_test_23.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green "Compiling Fortran source to assembly CMakeFiles/jf_test_23.dir/src/tests/jf_test_23.F90.s"
	C:\TDM-GCC-64\bin\gfortran.exe $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S "C:\Users\ferch\Desktop\Proyecto 1 EDD\V2.0\json-fortran-8.3.0\src\tests\jf_test_23.F90" -o CMakeFiles\jf_test_23.dir\src\tests\jf_test_23.F90.s

# Object files for target jf_test_23
jf_test_23_OBJECTS = \
"CMakeFiles/jf_test_23.dir/src/tests/jf_test_23.F90.obj"

# External object files for target jf_test_23
jf_test_23_EXTERNAL_OBJECTS =

bin/jf_test_23.exe: CMakeFiles/jf_test_23.dir/src/tests/jf_test_23.F90.obj
bin/jf_test_23.exe: CMakeFiles/jf_test_23.dir/build.make
bin/jf_test_23.exe: libjsonfortran.dll.a
bin/jf_test_23.exe: CMakeFiles/jf_test_23.dir/linkLibs.rsp
bin/jf_test_23.exe: CMakeFiles/jf_test_23.dir/objects1.rsp
bin/jf_test_23.exe: CMakeFiles/jf_test_23.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color "--switch=$(COLOR)" --green --bold --progress-dir="C:\Users\ferch\Desktop\Proyecto 1 EDD\V2.0\json-fortran-8.3.0\build\CMakeFiles" --progress-num=$(CMAKE_PROGRESS_2) "Linking Fortran executable bin\jf_test_23.exe"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles\jf_test_23.dir\link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/jf_test_23.dir/build: bin/jf_test_23.exe
.PHONY : CMakeFiles/jf_test_23.dir/build

CMakeFiles/jf_test_23.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles\jf_test_23.dir\cmake_clean.cmake
.PHONY : CMakeFiles/jf_test_23.dir/clean

CMakeFiles/jf_test_23.dir/depend:
	$(CMAKE_COMMAND) -E cmake_depends "MinGW Makefiles" "C:\Users\ferch\Desktop\Proyecto 1 EDD\V2.0\json-fortran-8.3.0" "C:\Users\ferch\Desktop\Proyecto 1 EDD\V2.0\json-fortran-8.3.0" "C:\Users\ferch\Desktop\Proyecto 1 EDD\V2.0\json-fortran-8.3.0\build" "C:\Users\ferch\Desktop\Proyecto 1 EDD\V2.0\json-fortran-8.3.0\build" "C:\Users\ferch\Desktop\Proyecto 1 EDD\V2.0\json-fortran-8.3.0\build\CMakeFiles\jf_test_23.dir\DependInfo.cmake" "--color=$(COLOR)"
.PHONY : CMakeFiles/jf_test_23.dir/depend

