# OpenSolar: Promoting the Openness and Accessibility of Diverse Public Solar Datasets

Solar data is the foundation of data-driven research in solar power grid integration and power system operations. Compared to other fields in data science, the openness and accessibility of solar data fall behind, which prevents solar data science from catching up with the emerging trend of data science (e.g., deep learning). In this repository, OpenSolar, an R package, is developed to enhance the openness and accessibility of publicly available solar datasets. The OpenSolar package provides access to multiple formats of data with diverse measurements in 4 datasets, which are:
-(i) the National Renewable Energy Laboratory (NREL) [Solar Power Data for Integration Studies (SPDIS)](https://www.nrel.gov/grid/solar-power-data.html) dataset
-(ii) the NREL [Solar Radiation Research Laboratory (SRRL)](https://midcdmz.nrel.gov/apps/go2url.pl?site=BMS) dataset
-(iii) the [Sheffield Solar-Microgen](https://www.solar.sheffield.ac.uk/all-projects/microgen-database/) database
-(iv) the [Dataport](https://dataport.cloud) database. 

Different from other open solar datasets that only contain meteorological data, the 4 datasets in the OpenSolar package also consists of behind-the-meter data, sky images, and solar power data with satisfactory temporal and spatial resolution and coverage. The overview, quality control methods, and potential usage of the datasets, in conjunction with the sample code of implementing the OpenSolar functions, are described. The package is expected to assist in bridging the gaps among solar energy, power systems, and machine/deep learning research.			



## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. 

### Prerequisites

This is an R package, so you need to install [R](https://www.r-project.org/) on your computer first. In addition, [RStudio](https://www.rstudio.com/) is an integrated development environment (IDE) for R; it is highly recommended.

### Installing

Once R and RStudio are installed. Open R or RStudio and install the [devtools](https://cran.r-project.org/web/packages/devtools/index.html) package, which allows you to install R package from GitHub

```
install.packages("devtools")
```

Load the package that you just installed

```
library("devtools")
```

Now, you can install the SolMod package, using

```
install_github("dazhiyang/SolarData")
```

## Running the tests

This code segment gives an example on how to run transposition modeling (horizontal to tilt) using a variety of models. (This is not up to date, I will update this section, as well as the package reference manual once the paper is accepted for publication)

```
library("SolarData")

#get SURFRAD data from Goodwin_Creek_MS (gwn) station, for the first three days in 2004
SURFRAD.get(station = 'Goodwin_Creek_MS', year = '2004', day_of_year = c(1:3))

#get PSM data for two locations
PSM.get(lat = c(42.05, 44), lon = c(-124.02, -110), api_key <- 'FVltdchrxzBCHiSNF6M7R4ua6BFe4j81fbPp8dDP', attributes <- 'ghi,dhi,dni,clearsky_dhi,clearsky_dni,clearsky_ghi,solar_zenith_angle', name = 'John+Smith', affiliation = 'Some+Institute', year = '2016', leap_year = 'true', interval = '30', utc = 'false', reason_for_use = 'research', email = 'yangdazhi.nus@gmail.com', mailing_list = 'false')

#get SRTM, i.e., digital elevation model, data for two boxes with resolution 3 arcsec
SRTM.list(3, want.plot = TRUE) #check available files
files <- c("Eurasia/N00E072.hgt.zip", "Eurasia/N00E073.hgt.zip")
SRTM.get(resolution = 3, files = files)
```

## License

This package is under the GPL-2 license.
