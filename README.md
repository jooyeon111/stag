STAG: Systolic Tensor Array Generator
=======================

The systolic tensor arrays is a generale and parallelized version of the systolic arrays. 
STAG automatically generate systolic tensor array RTL in various forms.

Currently, STAG propose 2 types of systolic arrays with 3 dataflows input stationary, output stationary and weight stationary.
1. Systolic tensor array
2. Systolic tensor array pod = skew buffer + systolic tensor array + de-skew buffer

We are going to provide control logic and automatic RTL test soon.

## How to run
Download [Chisel](https://github.com/chipsalliance/chisel) and run makefile.
Configuration files is located in main/resources/

## Proposed systolic tensor array architecture
### Input stationary systolic tensor array 
### Output stationary systolic tensor array
### Weight stationary systolic tensor array