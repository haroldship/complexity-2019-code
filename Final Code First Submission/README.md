# Separable Least-squares Parameter Estimation for Complex Dynamic Systems

## Simulation code

## Directory Layout

The code for each experiment is contained in a separate directory. The directory names reflect the setup of the experiment. The names are structured as an abbreviation of the model, such as FN for FitzHugh-Nagumo, then underscore "_", then number of samples, such as "n20", then signal-to-noise ratio such as "snr10".
Within each folder is an `R` script for running the simulations and saving the results as comma-separated values (.csv), and a second script for reading the .csv files and producing the summary values and/or the plots.

```
FN_n20snr10
FN_n20snr5
FN_n40snr10
FN_n40snr5
GMAbook_n100SNR10
GMAbook_n100SNR5
GMAbook_n200SNR10
GMAbook_n200SNR5
GMApaper_n100SNR10
LV_n100SNR10
LV_n100SNR5
LV_n200SNR10
LV_n200SNR5
SIR_n18SNR10
SIR_n18SNR5
SIR_n36SNR10
SIR_n36SNR5
```

## How to run

All code is run under the `R` programming language. We have tested it using version 3.5.2. The following packages are needed:
 - doParallel
 - doRNG
 - simode
 - ggplot2 (for plots)

The simulation code can take anywhere from a few minutes to several hours depending on the hardware setup and the experimental setup.

The simulations were run with 16 cores `registerDoParallel(cores=16)`. Change this to reflect the number of cores on your system.



