# PATProject

## How to use the specializer

To run BTA for a function `<fName>` in the file `<pathIn>` and write the result to `<pathOut>`, use the following command:

```bash
stack run <pathIn> <pathOut> <fName> <1|0>...
```

For each argument to the function `<fName>`, a `1` should be supplied if the argument is static and a `0` otherwise.

The files in the `specialized_programs` directory were generated using the following commands:

```bash
stack run "test_programs/Pow.hs" "specialized_programs/Pow.hs" pow 1 0
stack run "test_programs/Foo.hs" "specialized_programs/Foo.hs" foo 1 0 0
stack run "test_programs/Find.hs" "specialized_programs/Find.hs" find 1 0
stack run "test_programs/Map.hs" "specialized_programs/Map.hs" mapL 0 1
```

The tests using these files can be run using `stack test`. 

Optionally, `stack test --ghc-options -ddump-splices` can be used to show the code generated from splices.