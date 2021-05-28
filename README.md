# Pakistan Census Dataset

## Description of data 
This respository collects data related to Pakistan's national census. The most recent completed census, the first in nearly twenty years, was carried out by the Pakistan Bureau of Statistics (PBS) between March 15 2017 - May 25 2017. Although there have been disputes about the accuracy of these figures, the provisional results were approved by the federal-provincial Council on Common Interests on August 25, 2017. 

The PBS initially released provisional census block-level population and household numbers and aggregate totals, dated January 3 2018, for the four provinces, Islamabad Capital Territory, and Federally Administered Tribal Areas. (Accessible as of mid-May 2018, or by Wayback Machine [here](https://web.archive.org/web/20180515014953/http://www.pbscensus.gov.pk/).) 

On April 12, 2021, the Council on Common Interests approved [final census results](https://www.pbs.gov.pk/content/final-results-census-2017), which were published by the PBS at the tehsil (or equivalent fourth-level administrative unit) and district level on May 19, 2021. 

The total final population figure (207,684,626 people) is a -0.44% difference from total provisional figures (207,776,954). Below the top-line change, all but four administrative units (out of 542) reported adjustments to population totals between the provisional and final results; see the file **[provisional_to_final_adm_lvl_4_comparison.csv](https://github.com/colincookman/pakistan_census/blob/master/provisional_to_final_adm_lvl_4_comparison.csv)** for author-calculated differences. (But note caveat about Karachi cantonment populations below.)

Detailed demographic data, and final census block-level figures, have not been released as of this writing.

## Data generation
The Pakistani army provided security for the enumeration exercise, which was marred by at least two major incidents of violence. Army personnel also conducted parallel tabulation exercises. Pakistani expatriates and Afghan refugees living in official refugee camps were not included in the count, and six months’ residency was required to be counted as a resident of a given area. One analysis of the census process can be found from a [UN Populations Fund observer report](http://www.statistics.gov.pk/assets/publications/Pakistan%20Paopulation%20and%20Housing%20Census-2017%20National%20Report.pdf).

At the time of the April 2021 CCI approval, Planning Minister Asad Umar [said that](https://tribune.com.pk/story/2294393/cci-approves-2017-census-results-decides-to-hold-fresh-nosecount-by-year-end) the government was also planning to conduct a new, off-schedule, census before the 2023 general elections.

## Description of code
While the author greatly appreciates the release by PBS of census block-level data, its use of the pdf format forestalled easy analysis. This repository offers R code used to download, scan, and restructure this data in a tidy format for use in analysis, and the output in .csv format, with the census block as the basic unit of observation for provisional results and the tehsil (or equivalent fourth-level administrative unit) the unit of observation for final results.

## Data limitations and caveats
This dataset is being presented to encourage broader open data sharing among the community of analysts on Pakistan. The author cannot verify the accuracy of, or account for any discrepancies in, the underlying data.

Naming conventions for administrative subdivisions below the district level are not consistent across provinces or within provinces (depending on an area's administrative designation as a rural or urban area.) Punjab and Khyber Paktunkhwa provinces include divisional administrative areas (administrative level two), which other provinces do not follow. Since the completion of the census, the former Federally Administered Tribal Areas (FATA) have been incorporated into Khyber Paktunkhwa province.

The total number of census blocks reported in the provisional blockwise data release (163,542) appears to differ from initial PBS planned census block allocations (162,851, excluding the territories of Gilgit-Baltistan and Azad Jammu Kashmir, data for which has not been released - per [the PBS national census district map](http://www.pbscensus.gov.pk/sites/default/files/gis_maps/cd_pakistan.jpg)). No data was reported for 235 census blocks (0.144% of total), and 1,628 census blocks (0.995%) reported a count of zero population.

Six military cantonment areas (Faisal Cantonment in the Karachi East District, Clifton and Karachi Cantonments in the Karachi South District, Manora Cantonment in the Karachi West District, and Korangi Creek and Malir Cantonments in the Malir District) reported block-level census results and were identified as distinct fourth-level administrative areas in the preliminary census results. These cantonments are not listed as distinct fourth-level administrative areas in the final results, however. It is unclear based on existing information whether the cantonment populations were inadvertently ommitted from the final published census totals, or whether their populations were instead attributed to other adjoining areas. This has most likely introduced discrepancies in the calculated population differences between provisional and final census figures for at least some of these other areas in Karachi (some of which show very high increases in population), but the decomposition of this effect from other adjustments as part of the finalization of the census results is not currently possible absent further clarifications from PBS.

As of this writing, census block-level data for previous Pakistani census exercises does not appear to be available. The PBS does provide [tehsil-level aggregate data from the preceding (1998) census](http://www.pbscensus.gov.pk/content/area-population-aadministrative-units), although it should be noted that administrative boundaries have changed during the intervening period.

## Expansion and updates
Corrections and contributions aimed at improving the accuracy and utility of the data here are appreciated. Suggestions on other datasets with which to combine this data for further analysis opportunities are also welcome. Please check the issues tracker for planned updates or functionality improvements.
