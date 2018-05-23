# Pakistan Census Dataset

## Description of data 
This respository collects data related to Pakistan's national census. The most recent completed census, the first in nearly twenty years, was carried out by the Pakistan Bureau of Statistics (PBS) between March 15 2017 - May 25 2017. Although there have been disputes about the accuracy of these figures, the provisional results were approved by the federal-provincial Council on Common Interests on August 25, 2017. 

The PBS has released provisional census block-level population and household numbers and aggregate totals, dated January 3 2018, for the four provinces, Islamabad Capital Territory, and Federally Administered Tribal Areas. (Accessible as of mid-May 2018, or by Wayback Machine [here](https://web.archive.org/web/20180515014953/http://www.pbscensus.gov.pk/).)

Final results and detailed demographic data have not been released as of this writing.

## Data generation
The Pakistani army provided security for the enumeration exercise, which was marred by at least two major incidents of violence. Army personnel also conducted parallel tabulation exercises. Pakistani expatriates and Afghan refugees living in official refugee camps were not included in the count, and six months’ residency was required to be counted as a resident of a given area. One analysis of the census process can be found from a [UN Populations Fund observer report](http://www.statistics.gov.pk/assets/publications/Pakistan%20Paopulation%20and%20Housing%20Census-2017%20National%20Report.pdf).

## Description of code
While the author greatly appreciates the release by PBS of census block-level data, its use of the pdf format (split over 136 separate files) forestalled easy analysis. This repository offers R code used to download, scan, and restructure this data in a tidy format for use in analysis, and the output in .csv format, with the census block as the basic unit of observation.

## Data limitations and caveats
This dataset is being presented to encourage broader open data sharing among the community of analysts on Pakistan. The author cannot verify the accuracy of, or account for any discrepancies in, the underlying data.

Naming conventions for administrative subdivisions below the district level are not consistent across provinces or within provinces (depending on an area's administrative designation as a rural or urban area.)

Note that the total number of census blocks reported in the provisional blockwise data release (163,542) appears to differ from initial PBS planned census block allocations (162,851, excluding the territories of Gilgit-Baltistan and Azad Jammu Kashmir, data for which has not been released - per [the PBS national census district map](http://www.pbscensus.gov.pk/sites/default/files/gis_maps/cd_pakistan.jpg)). No data was reported for 235 census blocks (0.144% of total), and 1,628 census blocks (0.995%) reported a count of zero population.

As of this writing, census block-level data for previous Pakistani census exercises does not appear to be available. The PBS does provide [tehsil-level aggregate data from the preceding (1998) census](http://www.pbscensus.gov.pk/content/area-population-aadministrative-units), although it should be noted that administrative boundaries have changed during the intervening period.

## Expansion and updates
Corrections and contributions aimed at improving the accuracy and utility of the data here are appreciated. Suggestions on other datasets with which to combine this data for further analysis opportunities are also welcome. Please check the issues tracker for planned updates or functionality improvements.
