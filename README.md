STAG: Systolic Tensor Array Generator
=======================

The systolic tensor arrays is a generale and parallelized version of the systolic arrays. 
STAG automatically generate systolic tensor array RTL in various forms.

Currently, STAG propose 2 types of systolic arrays with 3 dataflows input stationary, output stationary and weight stationary.
1. Systolic tensor array
2. Systolic tensor array pod = skew buffer + systolic tensor array + de-skew buffer

We are going to provide control logic and automatic RTL test soon.

## 5 parameters that make up the systolic tensor arrays
R: The row of block PE
C: The column of block PE
A: The row of tensor PE
B: The column of tensor PE
P: The total number of multipliers in PE
Based on these 5 parameters, we can easily generate the systolic tensor array RTL.

## How to run
Download [Chisel](https://github.com/chipsalliance/chisel) and run the makefile.
Configuration files is located in src/main/resources/

## Proposed systolic tensor array architecture image
### Input stationary systolic tensor array 
### Output stationary systolic tensor array
![output stationary systolic tensor arrays](images/output_stationary_systolic_tensor_array.png)
### Weight stationary systolic tensor array