# CMake generated Testfile for 
# Source directory: C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0
# Build directory: C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(jf-cleanup-fixture "C:/Program Files/CMake/bin/cmake.exe" "-E" "remove_directory" "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/files")
set_tests_properties(jf-cleanup-fixture PROPERTIES  FIXTURES_SETUP "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;333;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf-setup-fixture "C:/Program Files/CMake/bin/cmake.exe" "-E" "copy_directory" "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files" "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/files")
set_tests_properties(jf-setup-fixture PROPERTIES  DEPENDS "jf-cleanup-fixture" FIXTURES_SETUP "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;338;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_01 "./jf_test_01")
set_tests_properties(jf_test_01 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_02 "./jf_test_02")
set_tests_properties(jf_test_02 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_03 "./jf_test_03")
set_tests_properties(jf_test_03 PROPERTIES  DEPENDS "jf_test_02" FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_04 "./jf_test_04")
set_tests_properties(jf_test_04 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_05 "./jf_test_05")
set_tests_properties(jf_test_05 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_06 "./jf_test_06")
set_tests_properties(jf_test_06 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_07 "./jf_test_07")
set_tests_properties(jf_test_07 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_08 "./jf_test_08")
set_tests_properties(jf_test_08 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_09 "./jf_test_09")
set_tests_properties(jf_test_09 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_10 "./jf_test_10")
set_tests_properties(jf_test_10 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_11 "./jf_test_11")
set_tests_properties(jf_test_11 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_12 "./jf_test_12")
set_tests_properties(jf_test_12 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_13 "./jf_test_13")
set_tests_properties(jf_test_13 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_14 "./jf_test_14")
set_tests_properties(jf_test_14 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_15 "./jf_test_15")
set_tests_properties(jf_test_15 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_16 "./jf_test_16")
set_tests_properties(jf_test_16 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_17 "./jf_test_17")
set_tests_properties(jf_test_17 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_18 "./jf_test_18")
set_tests_properties(jf_test_18 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_19 "./jf_test_19")
set_tests_properties(jf_test_19 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_20 "./jf_test_20")
set_tests_properties(jf_test_20 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_21 "./jf_test_21")
set_tests_properties(jf_test_21 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_22 "./jf_test_22")
set_tests_properties(jf_test_22 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_23 "./jf_test_23")
set_tests_properties(jf_test_23 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_24 "./jf_test_24")
set_tests_properties(jf_test_24 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_25 "./jf_test_25")
set_tests_properties(jf_test_25 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_26 "./jf_test_26")
set_tests_properties(jf_test_26 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_27 "./jf_test_27")
set_tests_properties(jf_test_27 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_28 "./jf_test_28")
set_tests_properties(jf_test_28 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_29 "./jf_test_29")
set_tests_properties(jf_test_29 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_30 "./jf_test_30")
set_tests_properties(jf_test_30 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_31 "./jf_test_31")
set_tests_properties(jf_test_31 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_32 "./jf_test_32")
set_tests_properties(jf_test_32 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_33 "./jf_test_33")
set_tests_properties(jf_test_33 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_34 "./jf_test_34")
set_tests_properties(jf_test_34 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_35 "./jf_test_35")
set_tests_properties(jf_test_35 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_36 "./jf_test_36")
set_tests_properties(jf_test_36 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_37 "./jf_test_37")
set_tests_properties(jf_test_37 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_38 "./jf_test_38")
set_tests_properties(jf_test_38 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_39 "./jf_test_39")
set_tests_properties(jf_test_39 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_40 "./jf_test_40")
set_tests_properties(jf_test_40 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_41 "./jf_test_41")
set_tests_properties(jf_test_41 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_42 "./jf_test_42")
set_tests_properties(jf_test_42 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_43 "./jf_test_43")
set_tests_properties(jf_test_43 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_44 "./jf_test_44")
set_tests_properties(jf_test_44 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_45 "./jf_test_45")
set_tests_properties(jf_test_45 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_46 "./jf_test_46")
set_tests_properties(jf_test_46 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_47 "./jf_test_47")
set_tests_properties(jf_test_47 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_48 "./jf_test_48")
set_tests_properties(jf_test_48 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(jf_test_49 "./jf_test_49")
set_tests_properties(jf_test_49 PROPERTIES  FIXTURES_REQUIRED "JF" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/bin" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;358;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(regression-hello-world-ascii.json "C:/Program Files/CMake/bin/cmake.exe" "-E" "compare_files" "--ignore-eol" "hello-world-ascii.json" "expected-outputs/hello-world-ascii.json")
set_tests_properties(regression-hello-world-ascii.json PROPERTIES  DEPENDS "'';jf_test_01;jf_test_02;jf_test_03;jf_test_04;jf_test_05;jf_test_06;jf_test_07;jf_test_08;jf_test_09;jf_test_10;jf_test_11;jf_test_12;jf_test_13;jf_test_14;jf_test_15;jf_test_16;jf_test_17;jf_test_18;jf_test_19;jf_test_20;jf_test_21;jf_test_22;jf_test_23;jf_test_24;jf_test_25;jf_test_26;jf_test_27;jf_test_28;jf_test_29;jf_test_30;jf_test_31;jf_test_32;jf_test_33;jf_test_34;jf_test_35;jf_test_36;jf_test_37;jf_test_38;jf_test_39;jf_test_40;jf_test_41;jf_test_42;jf_test_43;jf_test_44;jf_test_45;jf_test_46;jf_test_47;jf_test_48;jf_test_49;REQUIRED_FILES;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/hello-world-ascii.json;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/test12.json;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/test2.json;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/test21.json;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/test4.json" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/files" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;402;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(regression-test12.json "C:/Program Files/CMake/bin/cmake.exe" "-E" "compare_files" "--ignore-eol" "test12.json" "expected-outputs/test12.json")
set_tests_properties(regression-test12.json PROPERTIES  DEPENDS "'';jf_test_01;jf_test_02;jf_test_03;jf_test_04;jf_test_05;jf_test_06;jf_test_07;jf_test_08;jf_test_09;jf_test_10;jf_test_11;jf_test_12;jf_test_13;jf_test_14;jf_test_15;jf_test_16;jf_test_17;jf_test_18;jf_test_19;jf_test_20;jf_test_21;jf_test_22;jf_test_23;jf_test_24;jf_test_25;jf_test_26;jf_test_27;jf_test_28;jf_test_29;jf_test_30;jf_test_31;jf_test_32;jf_test_33;jf_test_34;jf_test_35;jf_test_36;jf_test_37;jf_test_38;jf_test_39;jf_test_40;jf_test_41;jf_test_42;jf_test_43;jf_test_44;jf_test_45;jf_test_46;jf_test_47;jf_test_48;jf_test_49;REQUIRED_FILES;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/hello-world-ascii.json;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/test12.json;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/test2.json;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/test21.json;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/test4.json" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/files" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;402;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(regression-test2.json "C:/Program Files/CMake/bin/cmake.exe" "-E" "compare_files" "--ignore-eol" "test2.json" "expected-outputs/test2.json")
set_tests_properties(regression-test2.json PROPERTIES  DEPENDS "'';jf_test_01;jf_test_02;jf_test_03;jf_test_04;jf_test_05;jf_test_06;jf_test_07;jf_test_08;jf_test_09;jf_test_10;jf_test_11;jf_test_12;jf_test_13;jf_test_14;jf_test_15;jf_test_16;jf_test_17;jf_test_18;jf_test_19;jf_test_20;jf_test_21;jf_test_22;jf_test_23;jf_test_24;jf_test_25;jf_test_26;jf_test_27;jf_test_28;jf_test_29;jf_test_30;jf_test_31;jf_test_32;jf_test_33;jf_test_34;jf_test_35;jf_test_36;jf_test_37;jf_test_38;jf_test_39;jf_test_40;jf_test_41;jf_test_42;jf_test_43;jf_test_44;jf_test_45;jf_test_46;jf_test_47;jf_test_48;jf_test_49;REQUIRED_FILES;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/hello-world-ascii.json;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/test12.json;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/test2.json;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/test21.json;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/test4.json" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/files" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;402;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(regression-test21.json "C:/Program Files/CMake/bin/cmake.exe" "-E" "compare_files" "--ignore-eol" "test21.json" "expected-outputs/test21.json")
set_tests_properties(regression-test21.json PROPERTIES  DEPENDS "'';jf_test_01;jf_test_02;jf_test_03;jf_test_04;jf_test_05;jf_test_06;jf_test_07;jf_test_08;jf_test_09;jf_test_10;jf_test_11;jf_test_12;jf_test_13;jf_test_14;jf_test_15;jf_test_16;jf_test_17;jf_test_18;jf_test_19;jf_test_20;jf_test_21;jf_test_22;jf_test_23;jf_test_24;jf_test_25;jf_test_26;jf_test_27;jf_test_28;jf_test_29;jf_test_30;jf_test_31;jf_test_32;jf_test_33;jf_test_34;jf_test_35;jf_test_36;jf_test_37;jf_test_38;jf_test_39;jf_test_40;jf_test_41;jf_test_42;jf_test_43;jf_test_44;jf_test_45;jf_test_46;jf_test_47;jf_test_48;jf_test_49;REQUIRED_FILES;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/hello-world-ascii.json;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/test12.json;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/test2.json;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/test21.json;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/test4.json" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/files" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;402;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
add_test(regression-test4.json "C:/Program Files/CMake/bin/cmake.exe" "-E" "compare_files" "--ignore-eol" "test4.json" "expected-outputs/test4.json")
set_tests_properties(regression-test4.json PROPERTIES  DEPENDS "'';jf_test_01;jf_test_02;jf_test_03;jf_test_04;jf_test_05;jf_test_06;jf_test_07;jf_test_08;jf_test_09;jf_test_10;jf_test_11;jf_test_12;jf_test_13;jf_test_14;jf_test_15;jf_test_16;jf_test_17;jf_test_18;jf_test_19;jf_test_20;jf_test_21;jf_test_22;jf_test_23;jf_test_24;jf_test_25;jf_test_26;jf_test_27;jf_test_28;jf_test_29;jf_test_30;jf_test_31;jf_test_32;jf_test_33;jf_test_34;jf_test_35;jf_test_36;jf_test_37;jf_test_38;jf_test_39;jf_test_40;jf_test_41;jf_test_42;jf_test_43;jf_test_44;jf_test_45;jf_test_46;jf_test_47;jf_test_48;jf_test_49;REQUIRED_FILES;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/hello-world-ascii.json;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/test12.json;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/test2.json;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/test21.json;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/files/expected-outputs/test4.json" WORKING_DIRECTORY "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/build/files" _BACKTRACE_TRIPLES "C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;402;add_test;C:/Users/ferch/Desktop/Proyecto 1 EDD/V2.0/json-fortran-8.3.0/CMakeLists.txt;0;")
