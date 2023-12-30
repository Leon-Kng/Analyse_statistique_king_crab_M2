# Projet_modeles_stats_M2
# README

**King Crab Data**

> This is a description of the Kodiak Island king crab survey data, distributed for the Data Analysis Exposition being sponsored by the Statistical Graphics and Statistical Computing Sections at the 1990 American Statistical Society Joint Statistical meetings in Anaheim. Here is a quick inventory of the files and their sizes:
> 

File      Lines  Cols  Bytes  Contents

---

README      458    -   22672  text: (this file)
survey     3450   14  244950  the basic survey data
kodiak     2687    2   48264  map coordinates for the Kodiak Island shoreline
dstns      1845    5   40590  distributions of crab by size, year, category
fleet        23    6     946  commercial fleet/catch/price data, by year
catch        96    4    2112  commercial catch data, by year, district
eggs         14    2     140  ave number of eggs per female, by year
salinity     60    3     718  ocean salinity, by month
celsius      68    3     741  ocean temperature, by month, in degrees celsius
fullness   1170    7   37440  distributions of females by clutch fullness

---

total    9920       401584

> **Data sources:**  All the data sets except salinity and temperature were provided by the Alaska Department of Fish and Game, 211 Mission Road, Kodiak, AK 99615.  The salinity and temperature data came from Tom Royer, Institute of Marine Sciences, University of Alaska, Fairbanks, AK 99775-1080
> 

### Contents of file "survey"

The main data set consists of king crab pot survey data for the years 1973
through 1986.  The surveys were conducted in the waters around Kodiak Island,
Alaska, using pots similar to the pots used by the commercial fishing fleet.
(A crab pot is a trap that resembles a wooden crate.)  A fixed sampling grid
was used to place strings of pots (stations) consisting usually of 10 pots in
open ocean, or of 2-5 pots in bays.  The pots were left in the water for
periods of 16-24 hours, removed, and the crab counts recorded.  The survey was
conducted each summer, 2-4 weeks prior to start of the commercial fishing
season.  The crab counts are classified by size (roughly representing age) and
sex into six categories.

The basic survey data is a file "survey", containing a 3,450 by 14 matrix
with these columns:

```
  1. Year (last two digits)
  2. Fishing district (one of four)
  3. Station identifier (alphabetic)
  4. The number of pots fished
5-6. Latitude and longitude of the location halfway between
         the first and last pot of the station
  7. Number of pre-recruit-4 crab
  8. Number of pre-recruit-3 crab
  9. Number of pre-recruit-2 crab
 10. Number of pre-recruit-1 crab
 11. Number of recruit males
 12. Number of post-recruit males
 13. Number of juvenile females
 14. Number of adult females
```

A **recruit** is a male king crab that has reached a certain specified minimum size that makes him eligible to be caught.  (The precise definition is somewhat technical, involving "carapace length" and new-shell vs old-shell, but the details are not important here.)

A **pre-recruit-1** is a smaller male, roughly a year too young to be legally caught.  (The perational definition is again in terms of size.)

A **pre-recruit-2** is a still-smaller male crab roughly 2 years away from reaching legal size.  And so on.

A **post-recruit** is, roughly, a male crab that was probably already legal a year earlier.

A **"legal" king crab** is any male crab that is either a recruit or post-recruit.  Since the commercial fleet is only permitted to catch legal crab, it is the total of recruits and post-recruits each year that is of primary commercial interest.

Note that for the 1973 through 1979 surveys, "adult female" was defined to be any female with eggs.  However, starting in 1980, the definition of adult female was expanded to include large eggless females that exceed a certain specified size.

> In addition to the basic survey data, a number of other data sets have been
provided which are thought to have varying degrees of relevance to the primary
analysis and to the graphical presentation of results.
> 

### Contents of file "kodiak"

→ Geographical coordinates of the shoreline of the 17 islands that form the Kodiak Island group.  The two columns are

```
  1.  latitude
  2.  longitude
```

measured in degrees and fractions of a degree.  Each of the 17 groups of coordinates is terminated by a pair of "NA"s, and the end of each group loops back to the beginning.  For drawing maps, bear in mind that longitude is measured East to West, which is right to left.  This suggests plotting negative longitude instead of longitude.  Also, to draw maps that "look right"
to an Alaskan, you must take into account that in this part of the world the aspect ratio of one degree latitude (y-axis) to one degree longitude (x-axis) is 1:1.8 (in terms of actual ground distance).

### Contents of file "dstns"

For each of the years in the survey (1973 to 1986), a frequency distribution of the crab by size (in 1 mm increments) that were surveyed.  Separate distributions are given for juvenile females, adult females, and all males.
The five columns are:

```
  1. year
  2. length in mm
  3. count of juvenile females
  4. count of adult females
  5. count of all males

```

### Contents of file "fleet"

Some statistics on the fishing fleet and commercial catch, for each year
between 1960 and 1982.  The six columns are:

```
  1. year
  2. number of vessels registered for fishing
  3. number of crab caught
  4. total weight in kilograms of crab caught
  5. total number of pot-lifts.
  6. wholesale price of king crab in dollars per pound

```

### Contents of file "catch"

Commercial catch data for 1960-1982, broken out by district.  The four columns are:

```
  1. year
  2. district number (1, 2, 3 or 4)
  3. total catch as a count
  4. total catch in kilograms

```

### Contents of file "eggs"

For each of the 14 years in the survey (1973-86), an estimate of the number of eggs per female.  Columns are:

```
  1. year
  2. estimated eggs per adult female
```

### Contents of file "salinity"

For a selection of months in the period 1970 to 1983, a measurement of the
ocean salinity at a depth of 100 meters off the Alaskan coast, given in parts
per thousand.  Columns are:

```
  1. year
  2. month
  3. salinity

```

### Contents of file "celsius"

For a selection of months in the period 1970 to 1983, a measurement of the ocean temperature at a depth of 100 meters off the Alaskan coast, given in degrees Celsius.  Columns are:

```
  1. year
  2. month
  3. temperature
```

### Contents of file "fullness"

For each year in the survey, a frequency distribution of all females cross-classified by size (in 1 mm increments) and percent clutch fullness (5 categories).  Clutch fullness is, roughly, the realized egg-bearing potential of a female crab.  The seven columns are:

```
  1. year
  2. size, in mm
  3. count of females with 0% fullness
  4. count of females with 1-29% fullness
  5. count of females with 30-59% fullness
  6. count of females with 60-89% fullness
  7. count of females with 90-100% fullness

```

### General Discussion

> The underlying purpose of the survey has been to gather biological and abundance information to be used in the management of king crab (opening and closing of fisheries, setting of quotas, etc.)
> 

The following is a set general questions and issues to stimulate the analysis.  The list is certainly not exhaustive:

- Can trends in crab abundance be characterized?
- What can be said about sampling error in this kind of survey?
- To the extent that abundance has declined, what factors are linked
to the decline? Is there evidence that overfishing is a major
cause? Do environmental factors contribute? Are we seeing
natural population cycles?
- Characterize the dynamics of the crab population
- Characterize the changing spatial distribution of the crab
population
- By comparing abundances of recruits, prerecruits and postrecruits
from year to year, can cohorts be identified and tracked?
- Can anything be said about the "social" interaction of crab
with regard to sexes and age groups?
- Although the primary interest is in recruits, clearly having
a plentiful supply of fecund females to produce new generations
of crab is essential. Is the fertility of females in jeopardy,
as well as their abundance? Can the supply of recruits in
a given year be linked to the abundance and fertility of
females in earlier years?

### Questions and answers

> We append to this list some questions for discussion contributed by one of the king crab researchers in Alaska.  While it is not clear which, if any, of these specific questions could be addressed with this data, the questions DO give a flavor of the kinds of issues that are of concern, and the language that is spoken in this industry:
> 
- Do the size classifications used provide a realistic view of the growth of male crab as portrayed by the change in modes over time?
- If not, what is an improved alogorithm for defining crab age classes? (Keep in mind that crab do not grow continuously, but incrementally, and that crab age cannot be measured, other than by inference by looking at length frequency information.)
- Does the data indicate that M (natural mortality) varies by size? by shell age? among years?
- What change would M have to have before it would be detectable?
- Are F (fishing mortality) and M related? i.e. does fishing mortality affect, in some indirect way (such as through ghost fishing by lost pots, illegal catch, mortality from handling
and sorting crabs) the assumed F values from the catch data?
- What affect would increasing or decreasing F have on long term average yield and its variance using a 50 year time horizon for rojections? What affect would varying size limits have on the
above?
- Length frequency data can be obtained from survey data, much less costly than Catch Per Unit Effort data. What information is obtained about crab population dynamics from the CPUE data that cannot be obtained from the length frequency data when coupled with catch data? without catch data (i.e. when the fishery is closed)?
- Do crab grow at different rates between years? Does the frequency of skip molting change, based on age of crab, as compared to length? Can these questions be addressed by this
data set?

> In the course of a preliminary analysis of this data, a person knowing little about crab (apart from how they taste!) posed several questions.  Alan Johnson, of the Alaska department of Fish and Game, addressed each briefly.  This dialogue may provide some additional insights:
> 

**Q:** Although only recruits and postrecruits may be legally fished, some number of females and smaller males are probably harvested, either by error or deliberate violation.  How extensive it this?

**A:** This is tightly controlled, and not regarded as a problem

**Q:** How reliable are the figures on commercial catch?  E.g., how carefully do vessels measure their catch?

**A:** They use a "stick" of the correct size and measure every crab. Of course, if the vessel processes crab onboard instead of landing the crab live, they could process illegal crab.  This
   issue has been studied for the Bering sea fishery by comparing size distributions.  Onboard observers are now required, but historically, in the Kodiak fishery, it is believed that onboard
   processing did not occur.

**Q:** How conscientiously do they report?

**A:** Counts of crab are guesstimates, pounds are accurate; that's how they get paid. Location of catch could be squirrely because the skippers are protective of where they fish.

**Q:** How many unregistered vessels (possibly of other nationalities, or noncommercial) operate illegally?

**A:** None.  This is very tightly controlled.

**Q:** I was told that a possibly significant number of crab (of all categories) may be injured or destroyed when the pots are lowered and land on them.   Once an area is found where there happens to be a local concentration of crab, many vessels converge there and many pots are dropped.  These pots are LARGE and HEAVY, and the ocean floor is literally blanketed with crab.  Also, a certain number of pots must have been lost over time, but such "ghost pots" may continue to destroy crab.  How serious a factor is this? 

**A:** No one knows for sure, but the problem has been addressed and is thought, at this time, not to be a serious problem.

 **Q:** Does the area of the survey pretty well cover the area that is commercially fished?  A superficial study of this data shows that the crab populations tend to be concentrated in smallish neighborhoods, and wander around from year to year.  Do they also wander significantly from day to day or from week to week?  Do they wander on and off the survey grid, so that we might actually miss them in a given year's survey?  If they do move from week to week, and if it takes several weeks to conduct the survey (how long does it take?) is it possible that in a given survey we might actually miss an elusive concentration of crab, even if they are on the rid?  Or could a given survey accidentally track a concentration of crab, erroneously observing the same crab community at several locations?

**A:** These issues have been well covered, with the exception that minor areas have never been surveyed.  The survey areas changed over the years in response to the industry saying "There's crab out there".  The catch quotas were based on the survey results, so the fishermen wanted the survey to hit the concentrations of crab.
   There is quite a bit of crab movement.  (The fishermen have to follow the crab to be really successful). Simultaneous movement of the crabs and the survey is a potential problem.  The rate of movement of the survey is probably higher than that of the crab, day by day.  In the survey, up to three strings per day are fished. (There are 39 pots on a vessel.)  The survey moves faster in the bay locations.

**Q:** Getting a handle on sampling error in this survey from the data at hand is a staggering task!

**A:** Sampling error should probably be handled at the count-per-pot  level of resolution, to get within-station and between-station variance estimates. [In fact, crab counts within pots are   available, but we chose to provide only station totals in the present data set, in order to keep it down to a manageable size. The raw data files from this survey occupy about 80M byte.]

**Q:** Even apart from the issues of crab migration on, off, and across the grid, and differences in numbers of pots lowered and lengths of times they are in the water, there is a serious question about what motivates crab to crawl into a pot, and how the presence of crab in a pot may encourage or discourage other crabs from entering the same pot.  In other words, the crab count from a pot is at best only related to the actual crab concentration at that station through some unknown (hopefully monotonic) probabilistic function. Or does someone somewhere have knowledge of crab behavior at this level?  When I asked people questions of this kind at the joint meeting of the Alaska Chapters of the ASA and the American Fisheries Society in Anchorage, I got the impression that crab social behavior is not well understood.

**A:** True.  If we redo this set of data at the pot resolution, this could be addressed, but not now.  The social behavior is indeed not well understood.

**Q: I** have the impression that until the mid '70s there was very little appreciation of the need to manage the crab population, especially to protect the abundance and fertility of females, and that there was very little control of commercial crab fishing.

**A:** The first surveys were in 1971 and '72 and I think the quotas were started about that time.  (The '71 and '72 surveys are not even comparable to this data set)

**Q:** To what extent has the decline in commercial catch been the result of

1. increased controls on fishing during this period, and 2. declining profitability due to declining crab population?

**A:** A declining population causes an INCREASE in profitability.  Price went up!

**Q:** Is crab survey data available for 1987 and 1988?

A: No pot survey was conducted in those years.  A trawl survey was conducted instead, so the '87 and '88 data are not comparable.
