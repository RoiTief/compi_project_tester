
# Compilation course BGU 2022/2023 semester 1
A tester for the compiler.

# How to run the tester:
1. download code_gen_tests.ml and the new makefile then place then inside the compiler directory 
2. run the tester with './code_gen_tests.ml'
3. optional: use make cleanall to remove all the output files

*This tester assumes that using '#use "compiler.ml"' in ocaml/utop will define the module Code_Generation that has the function compile_scheme_string*

# How to add new tests:
1. fork the project
2. make a new commit by adding tests to 'cg_tests' array along with the expected result (hint: chez scheme).
3. create a pull request.

