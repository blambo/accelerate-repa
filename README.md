A back-end for the Accelerate library using the Repa library
============================================================

This library aims at allowing parallel computations on the CPU to be performed through Accelerate by using Repa as a back-end.

The current implementation generates a string containing the generated code.

Installing the library
-----------------

First grab a copy of the library by forking/downloading.

Install the library by the commands

   runghc Setup.hs configure
   runghc Setup.hs build
   runghc Setup.hs install

You may need to alter these commands depending on your individual set-up.

Running the library
-------------------

If you are familiar with the use of the Accelerate library you would know of run function exposed by Data.Array.Accelerate.Interpreter or Data.Array.Accelerate.CUDA, depending on which back-end implementation you wish to use. The run command for this back-end implementation is exposed by the module Data.Array.Accelerate.Repa.

Currently the run command for the Repa back-end generates a String representing the generated code for running the given Accelerate program using Repa. You can obviously do what you like with this String, the following is just one method of using it to get the result of the function.

Pass your Accelerate program to the Repa back-end, and pipe the result into a temporary file, compile and run this file to get the result.

An example program is:

   dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
   dotp xs ys = fold (+) 0 (zipWith (*) xs ys)

   main = do $ putStrLn $ Repa.run $ dotp arr1 arr2

Where arr1 and arr2 are your arrays of data. On the command-line (in Unix) you could run it as such:

   $ runghc Example.hs > Temp.hs
   $ runghc Temp.hs

To get the result.
