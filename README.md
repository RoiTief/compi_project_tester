
# Compilation course BGU 2022/2023 semester 1
A tester for the compiler.

*IMPORTANT*

This tester assumes you are running under linux and hasn't been tested on windows.

# How to run the tester:
1. Download code_gen_tests.ml and the new makefile then place then inside the compiler directory 
2. Run the tester with `./code_gen_tests.ml`
3. Optional: use make cleanall to remove all the output files

*This tester assumes that using `#use "compiler.ml"` in ocaml/utop will define the module Code_Generation that has the function compile_scheme_string*


The new makefile places the object files in a temp directory at /tmp/testcode_XXX as not to clog the working directory of the compiler.

Using `make clean`, removes all the compiler output files and the tmp directories.

Using `make cleane`, removes the executable files with the name `foo_*` that are found in the compiler directory 

# How to add new tests:
1. Fork the project
2. Make a new commit by adding tests to `cg_tests` array along with the expected result (hint: chez scheme).
3. Create a pull request.

