# detlimitr3

The **detlimitr3** (estimating *det*ection *limit*s using *R* with S*3*) package contains functions for use with calibration data in analytical chemistry. In this package, the detection limit can be calculated for either linear, quadratic or power regression.


## Installation

If required, the *detlimiter3* package can be downloaded using

``` R
#install.packages("devtools") 
devtools::install_github("peterjwatkins/detlimitr3", force=TRUE)
```

## Usage

Some example code:
```R 
library(detlimitr3)
data(mtbe)
m1 <- det.limit(mtbe$x, mtbe$y)  # linear regression as default
plot(m1, residplot = TRUE)       # review the residual plot
print(m1, dp = 2)                # print detection limits, with two decimal points
plot(m1)

data(chloromethane)
m2 <- det.limit(chloromethane$Concentration, chloromethane$Response,
             "Quadratic")
#' plot(m2, residplot = TRUE)
```

Further information and examples can be found in '/docs/detlimitr.Rmd'

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
[GPL-3](https://www.gnu.org/licenses/gpl-3.0.en.html)
