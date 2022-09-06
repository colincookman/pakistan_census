# Pakistan Census Dataset

## Description of data 
This repository collects data related to Pakistan's national census. The most recent completed census, the first in nearly twenty years, was carried out by the Pakistan Bureau of Statistics (PBS) between March 15 2017 - May 25 2017. 

The Pakistani army provided security for the enumeration exercise, which was marred by at least two major incidents of violence. Army personnel also conducted parallel tabulation exercises. Pakistani expatriates and Afghan refugees living in official refugee camps were not included in the count, and six months’ residency was required to be counted as a resident of a given area. One analysis of the census process can be found from a [UN Populations Fund observer report](http://www.statistics.gov.pk/assets/publications/Pakistan%20Paopulation%20and%20Housing%20Census-2017%20National%20Report.pdf). Although there have been disputes about the accuracy of these figures, the provisional results were approved by the federal-provincial Council on Common Interests on August 25, 2017 and were used as the basis for constituency boundaries in the 2018 Pakistani general elections.

The PBS initially released provisional census block-level population and household numbers and aggregate totals, dated January 3 2018, for the four provinces, Islamabad Capital Territory, and Federally Administered Tribal Areas. (Now only accessible through the Internet Archive Wayback Machine [here](https://web.archive.org/web/20180515014953/http://www.pbscensus.gov.pk/); also mirrored in this repository.) 

On April 12, 2021, the Council on Common Interests approved [final census results](https://www.pbs.gov.pk/content/final-results-census-2017), which were published by the PBS at the tehsil- (or equivalent administrative unit) and district-level on May 19, 2021.

On August 1, 2021, the PBS released a further set of [more detailed demographic data](https://www.pbs.gov.pk/content/district-wise-results-tables-census-2017) collected as part of the census, split across 40 standardized table formats for each of the 135 districts in the country. Although most tables did not report information below the tehsil- or equivalent-level administrative area, Tables 23-26 for each district reported summary population, literacy, education, religion, aggregate age profile, and housing facility statistics for the rural and urban administrative areas just above the lowest census block level.

While the author greatly appreciates the release by PBS of census block-level data, its use of the pdf format and choice of data table structures forestalled easy analysis. This repository offers R code used to download, scan, and restructure this data in a tidy format for use in analysis, and the output from this code in .csv format.

## Description of files

The output from these cleaning processes is collected in the following primary datasets:

* **[Pakistan_2017_Census_Provisional.csv](https://github.com/colincookman/pakistan_census/blob/master/Pakistan_2017_Census_Provisional.csv)**: nationwide provisional population and household counts reported at the census-block level as reported in January 2018.
* **[Pakistan_2017_Census_Final.csv](https://github.com/colincookman/pakistan_census/blob/master/Pakistan_2017_Census_Final.csv)**: nationwide final population and household counts, with gender breakdown, at the tehsil level as reported in April 2021.
* **[By district](https://github.com/colincookman/pakistan_census/tree/master/processed_forms), and in [national aggregate](https://github.com/colincookman/pakistan_census/tree/master/processed_forms/CONSOLIDATED%20NATIONAL%20TABLES)**, the following 40 detailed demographic tables as released by PBS in August 2021:
    + **Table 01:** *Area, Population by sex, sex ratio, population density, urban proportion, household size and annual growth rate*
    + **Table 02:** *Urban localities by population size and their population by sex, annual growth rate and household size*
    + **Table 03:** *Number of rural localities by population size and their population by sex*
    + **Table 04:** *Population by single year age, sex and rural/urban*
    + **Table 05:** *Population by selected age group, sex and rural/urban*
    + **Table 06:** *Population (15 years and above) by age group, sex, marital status and rural/urban*
    + **Table 07:** *Population (15 years and above) by relationship to the head of household, sex, marital status and rural/urban*
    + **Table 08:** *Population by sex, age group, relationship to the head of household and rural/urban*
    + **Table 09:** *Population by sex, religion and rural/urban*
    + **Table 10:** *Population by nationality, age group, sex and rural/urban*
    + **Table 11:** *Population by mother tongue, sex and rural/urban*
    + **Table 12:** *Population (10 years and above) by literacy, sex, age group and rural/urban*
    + **Table 13:** *Population (10 years and above) by literacy, sex and rural/urban*
    + **Table 14:** *Literate population (10 years and above) by level of educational attainment, sex, age group and rural/urban*
    + **Table 15:** *Population (5 years and above) by level of educational attainment, sex, age group and rural/urban*
    + **Table 16:** *Population (10 years and above) by usual activity, sex, age group and rural/urban*
    + **Table 17:** *Disabled Population by Sex, Age Group and Rural/ Urban*
    + **Table 18:** *Disabled Population (05 Years and Above) by Sex, Age Group, Educational Attainment and Rural/Urban*
    + **Table 19:** *Disabled Population (10 Years and Above) by Sex, Age Group, Economic Activity and Rural/Urban*
    + **Table 20:** *Number of persons living abroad by sex and rural/urban*
    + **Table 21:** *Pakistani citizen (18 years and above) by holding computerized national identity card age group, sex, and rural/urban*
    + **Table 22:** *Homeless population by age group, sex, marital status, religion, literacy, activity and rural/urban*
    + **Table 23:** *Selected population statistics of rural localities*
    + **Table 24:** *Selected housing characteristics of rural localities*
    + **Table 25:** *Selected population statistics of urban localities*
    + **Table 26:** *Selected housing characteristics of urban localities*
    + **Table 27:** *Type of housing units, population by sex and rural/urban*
    + **Table 28:** *Household by number of persons and rural/urban*
    + **Table 29:** *Housing units by household size, number of rooms and rural/urban*
    + **Table 30:** *Housing units by number of rooms, tenure and rural/urban*
    + **Table 31:** *Owned housing units by sex of ownership and rural/urban*
    + **Table 32:** *Owned housing units by period of construction, number of rooms and rural/urban*
    + **Table 33:** *Housing units by tenure, material used in outer walls, roofs and rural/urban*
    + **Table 34:** *Owned housing units by period of construction, material used in outer walls, roofs and rural/urban*
    + **Table 35:** *Housing units by ownership, source of drinking water, lighting, cooking fuel used and rural/urban*
    + **Table 36:** *Owned housing units by period of construction, source of drinking water, lighting,cooking fuel used and rural/urban*
    + **Table 37:** *Housing units by tenure, kitchen, bathroom, latrine facilities and rural/urban*
    + **Table 38:** *Owned housing units by period of construction, housing facilities and rural/urban*
    + **Table 39:** *Owned housing units by period of construction, material used in outer walls, material used in roofs and rural/urban*
    + **Table 40:** *Number of households by source of information and rural/urban*

Some cleaned table output files have been reorganized to long or wide format to facilitate easier analysis (and Table 22 has been split into four distinct subcomponents); some of these files may require further cleaning and restructuring before final use. Users are encouraged to refer to the original PBS table files, also mirrored in this repository, for additional information, data verification, and for an understanding of the original data structure. Note that the cleaned table output files retain aggregate summary row information which may be used to verify and validate PBS-reported sums, but should be removed prior to conducting analysis to avoid data doubling.

Additionally, the repository includes a **[keyfile](https://github.com/colincookman/pakistan_census/blob/master/keyfiles/administrative_area_keyfile.csv)** with unique numeric codes assigned to each administrative area above the census block level, in order to facilitate future matching of non-standardized English language name transliterations. A **[separate keyfile](https://github.com/colincookman/pakistan_census/blob/master/keyfiles/census_block_administrative_area_keyfile.csv)** links these adminstrative areas to the census blocks as reported in the initial provisional results. Due to PBS reporting gaps, these keyfiles are most likely not exhaustive of all administrative areas (see notes below).

Some sample population charts are included in the repository's **[analysis](https://github.com/colincookman/pakistan_census/tree/master/analysis)** subfolder. For the PBS' own summary reports on census findings, see its [National Census Report and associated provincial reports](https://www.pbs.gov.pk/content/descriptive-and-broad-analysis-census-2017).

## Data validation, limitations, and caveats
This dataset is being presented to encourage broader open data sharing among the community of analysts working on issues related to Pakistan. The author cannot verify the accuracy of, or account for any discrepancies in, the underlying data.

The total number of census blocks reported in the provisional blockwise data release (163,542) appears to differ from initial PBS planned census block allocations (162,851, excluding the territories of Gilgit-Baltistan and Azad Jammu Kashmir, data for which has not been released - per [the PBS national census district map](http://www.pbscensus.gov.pk/sites/default/files/gis_maps/cd_pakistan.jpg)). No provisional data was reported for 235 census blocks (0.144% of total), and 1,628 census blocks (0.995%) reported a provisional count of zero population. Some additional gaps can be observed in census block sequences reported in the provisional data, indicating that this list may not be exhaustive.

Of the final detailed demographic data tables released in August 2021, Table 22 was missing for the Gujranwala district; Tables 2 and 23-26 were missing for the Kohistan district (which featured multiple data errors across other tables); and Table 11 was missing from the Okara district. See additional data cleaning notes below.

Out of 55,669 unique administrative areas identified immediately above the census block level, 126 that reported provisional results data in January 2018 did not report final results data in Tables 23 or 25 in August 2021 (excluding Kohistan district, for which no information was released below the tehsil level). These areas (again excluding Kohistan) had previously reported a provisional total of 564,526 individuals. Additionally, 114 administrative areas (primarily in North Waziristan, the Murree tehsil in Rawalpindi, and the cantonment areas of Karachi) reported final detailed population data in Tables 23 and 25 in August 2021 but had not previously been reported in the provisional block-level count; these areas reported a final total of 553,629 individuals.

The total final population figure (207,684,626 people) reported by PBS in April 2021 is a -0.044% (-92,328) difference from total provisional figures released in January 2018 (207,776,954). However, at least part of this discrepanacy appears to be due to the ommission of the aforementioned administrative areas (particularly Karachi cantonment areas) from the 'final' tehsil-level count. The detailed population totals reported in Tables 23 and 25 in August 2021 show a provisional total of 207,794,116 people when matched to administrative areas, and a final total of 206,917,069; with the addition of a final total of 784,711 people reported at the aggregate level for the Kohistan district (unchanged from provisional reporting, and not available in detail below the tehsil level) this makes a final total of 207,701,780 people, an overall net change of -0.044% (-92,336) when compared to January 2018 provisional totals or +0.008% (+17,154) when compared to the April 2021 'final' totals.

Of the 53,964 unique administrative areas that reported *both* provisional and final counts, 35,289 areas (65.4% of total reporting in both periods) saw at least some net change between the January 2018 and August 2021 reporting periods (minimum observed net change for an administrative area: -8607; max: +10237; mean: -1.5; median: 0), for a total net change of -81,439 between provisional and final results not otherwise potentially explained by data gaps in either reporting period. 

See the file **[provisional_to_final_adm_lvl_4_comparison.csv](https://github.com/colincookman/pakistan_census/blob/master/validity_checks/provisional_to_final_adm_lvl_4_comparison.csv)** for author-calculated differences at the tehsil level (using January 2018 and April 2021 figures), or **[provisional_final_census_comparison.csv](https://github.com/colincookman/pakistan_census/blob/master/validity_checks/provisional_final_census_comparison.csv)** for differences at the lowest administrative area level above census blocks (using January 2018 and August 2021 figures).

## Additional data cleaning notes
Naming conventions for administrative subdivisions below the district level are not consistent across provinces or within provinces (depending on an area's administrative designation as a rural or urban area). Sindh uses the *taluka* as a tehsil-equivalent administrative area; some provinces also use 'sub-divisions' or 'sub-tehsils'. The datasets and keyfiles in this repository (derived from the January 2018 blockwise reporting and Tables 23-26 in the August 2021 detailed census tables) principally follow a structure of four standard administrative levels between the top-level district and lowest census block level. (An exception is the April 2021 final results file, which includes divisional labels between the provincial and district level in Punjab and Khyber Paktunkhwa provinces, which other provinces do not currently follow).

Since the completion of the census, the former Federally Administered Tribal Areas (FATA) have been incorporated into Khyber Paktunkhwa province, but are reported separately in all PBS 2017 census reports. The Lehri district in Balochistan was merged with the Sibi district in January 2018; Lehri was reported as separate area in the provisional census data, but merged with Sibi in the final census data. Several other changes to administrative boundaries at the tehsil and lower levels have been made across the provinces in the years since the census was conducted, and the names and areas identified in this dataset and its associated keyfiles will most likely not correspond directly to other data sources.

Some additional errata from the final detailed demographic tables:

* Batagram district Table 12 ommited tehsil-level information and only reported a district-level aggregate;
* Dera Ghazi Khan district Table 4 ommitted some values in the 'All Ages' row for the De-Excluded DG Khan Tehsil;
* Kohistan district Tables 13-14, 27-28, and 40 ommited tehsil-level information and only reported a district-level aggregate;
* Hafizabad district Table 23 mis-copied identical aggregate summary values for the first reported page;
* Upper Dir district Table 11 omits columns for Saraiki, Hindko, and Brahvi speakers, although these appear to be included in total columns.

This list is non-exhaustive; while the author has endeavored to document all changes made through the data cleaning process in the accompanying code, additional errors may exist and any corrections are welcomed and encouraged.

## Expansion and updates
As of this writing, census block-level data for previous Pakistani census exercises does not appear to be publicly available. The PBS does provide some [tehsil-level aggregate data from the preceding 1998 census](http://www.pbscensus.gov.pk/content/area-population-aadministrative-units), and reports 1998 population totals at the tehsil level in Table 1 of the detailed district tables released in August 2021. Administrative boundaries have changed during the intervening period, however, and no information from the 1998 census has been released for lower administrative levels; the author cannot verify or validate the PBS' reallocation of 1998 population units to match 2017 tehsil-level boundaries. 

At the time of the April 2021 CCI approval, Planning Minister Asad Umar [said that](https://tribune.com.pk/story/2294393/cci-approves-2017-census-results-decides-to-hold-fresh-nosecount-by-year-end) the government was also planning to conduct a new, off-schedule, census before the 2023 general election. As of August 2022, planning for this new census [appears to be behind schedule](https://tribune.com.pk/story/2371154/7th-digital-census-delayed-by-four-months).

Corrections and contributions aimed at improving the accuracy and utility of the data here are appreciated. Suggestions on other datasets with which to combine this data for further analysis opportunities are also welcome. Please check the issues tracker for planned updates or functionality improvements.
