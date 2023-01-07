
# Compilation course BGU 2022/2023 semester 1
A tester for the compiler.

*IMPORTANT*

Assumptions:
 - You are running under GNU\Linux (this hasn't been tested on windows).
 - `Code_Generator` module lies in `code-gen.ml` - if not you'll have to edit tester.ml `#use ...` at line 1

# How to run the tester:
1. Download all the files and place them in the same directory
2. Add your project files to said dir (`code-gen.ml` and whatever else it needs)
3. Run the tester with `utop tester.ml`
4. Optional: use `make clean` to remove all the output files

*Please Notice*
 - The tester creates the code as file `foo.asm` in same working dir, which compiles to `foo` and is constantly overwritten. 
 - Tests run in segments based on context so you can edit the file in order to run only the contexts you want to test.
   Simply comment out the `run_cg_tests` lines at the end of `tester.ml` you don't want to run. 


# How to add new tests:
1. Fork the project
2. Add tests to an existing context in the tests_hub directory or create a new context by adding your own file. If you do add a new file remember to add a `#use` and a `run_cg_tests` lines in the `tester.ml` file. 
3. Create a pull request.
