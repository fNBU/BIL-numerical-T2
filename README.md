# BIL-numerical-T2

The file galileo.ml is OCaml source code that evolves initial data for a T^2-symmetric 3+1 dimensional spacetime with spatial topology T^3. This code was used to inform the results in two papers by Beverly K Berger, Jim Isenberg, and myself (Adam Layne).

Refer to the files in initial_data to see the form input data should take. The data is held in an array, each element of which has the structure

    { "time" : 0.11497302520551136, "data" : [ [0.9229413205017987, ...] , ... ] }

The functions in the data record are in the order P , \pi_P , Q , \pi_Q , \lambda , \pi_\lambda.

The executable will compile using version 4.11.2 of the OCaml compiler. The required packages are listed in the dune file. Compile simply with the command

    dune build galileo.exe