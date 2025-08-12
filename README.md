# REvenT 

This R package is suited to create simple Event Trees to illustrate operations like marginalization and conditioning of events on a tree.
No optimization for speed or memory is implemented, furthermore objects should be accessible with  basic knowledge of R.

In order to install this package, type in the R console the following commands:

```
install.packages("devtools")  
```

if such a package is not already installed.

```
library(devtools)
install_github(repo="federico-m-stefanini/REvenT",
               build_vignettes= TRUE,
               force= TRUE)
               
help(package="REvenT") # to see vignettes in italian and english               
```