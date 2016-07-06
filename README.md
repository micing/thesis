This repository contains all code and data presented in the thesis: "P-hacking in academic research and its implications for statistical inference". Copyright (C) 2016 Michael Ingre.

The thesis can be downloaded here: https://dx.doi.org/10.6084/m9.figshare.3393664

The code is licenced under GPL v3.0. The data, figures etc. (and the thesis) are licenced under CC-BY 4.0. See LICENSE.txt for details.

The repository is structured after the thesis and the thesis is essential to appreciate the code and data herein.

The root directory consists of code related to the introduction, methods and discussion sections while the the incuded studies have their own subdirectory (study1-study5).

All code assumes that the working directory (WD) is the root directory of the repository.

Code to reproduce figures and tables have their own file, named approprietly; and since the results from intermiediary analyses have been saved to disk and included in the "WD/data" directory they should run without preparation on their own to produce expected the output in the "output"" directory. To reproduce the intermediate results you need to run the code in numbered (#) files starting with an underscore ("_#_*.R") in the correct order.

All sections of the thesis also have a "master file" named the same as the section (i.e. introduction.R, study1.R etc) and these files provides a good starting point when exploring code related to a specific finding in the thesis.

TL;DR: To best enjoy this work open up the linked thesis to find what you are interested in and source the corresponding file in this repository.

