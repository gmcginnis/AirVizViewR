
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AirVizViewR

<!-- badges: start -->
<!-- badges: end -->

### ☛ Visit the dashboard [here](https://gmcginnis.shinyapps.io/airvizviewr/)

☛ Full link: <https://gmcginnis.shinyapps.io/AirVizViewR/>

☛ Alternative link, hosted on the Reed College server:
<https://shiny.reed.edu/s/users/gmcginnis/AirVizViewR/>

### Overview

The goal of AirVizViewR is to provide a user-friendly web interface for
interacting with the [AirVizR
package](https://github.com/gmcginnis/AirVizR).

The AirVizR package and corresponding AirVizViewR web interface were
written by [Gillian McGinnis](https://github.com/gmcginnis) during the
summer of 2021, and is actively being updated.

All sections of the tool are annotated for ease of use.

#### Project information (“<i>Welcome</i>” tab)

This tab provides an overview of the dashboard, as well as authorship
information, acknowledgments, a disclaimer, and links to related
resources.

To begin using the tool, visit the “<i>Inputs</i>” tab.

#### Accessing data (“<i>Inputs</i>” tab)

The dashboard requires location and temporal inputs in order to load the
spatio-temporal data set of interest. The larger the set, the longer it
will take to be loaded. Data limits have been placed in order to prevent
server overload.

A <i>boundary box</i> using W,S,E,N numeric-form coordinates is
required. For example, <code>-122.65, 45.46, -122.60, 45.50</code>
corresponds to a portion of the Portland OR metro area.

A <i>date range</i> is also required. It is recommended to keep this
date range under 1 month, as the data is downloaded with maximum
granularity (which reports every few seconds).

Monitors can be selected to be only <i>indoor</i> or only
<i>outdoor</i>. The default is both.

##### Optional inputs

An optional input is a <i>label filter</i>, which allows for monitors in
a specified area to be selected only if they match a corresponding
string. For example, <code>NCA</code> would filter only for monitors who
have the term “NCA” at any point in their name.

Furthermore, if one is selecting an area along state boundaries but
would only like to download the data from one state, a <i>state code</i>
filter can be applied.

After following the instructions for inputs (and adding any optional
inputs as desired) and pressing the emphasized button, a map will first
be generated in order to verify that the monitors of interest are
correct. If successful, the next step is to have the tool load the time
series data of interest. Once the data has loaded, a summary table will
appear, and the true values can be graphically visualized in the
“<i>Visualize</i>” tab.

#### Creating data visualizations (“<i>Visualize</i>” tab)

Behind-the-scenes, R will be constructing data visualizations based on
the provided data inputs and using functions from the [AirVizR
package](https://github.com/gmcginnis/AirVizR). Four different
visualization options are available, each with their own customization.

Selecting the <i>data</i> to be plotted will change the granularity of
the values (with the exception of the hourly single heatmap, which will
only use hourly data). Generally, the higher the granularity (i.e., the
closer to “full”), the longer the data will take to visualize.

##### Variable information

The <i>variable of interest</i> allows for the selection of the values
to graph.

PM<sub>2.5</sub> is Particulate Matter with a diameter &lt;2.5 μm.

For PurpleAir values, the different <code>CF</code> codes are the
correspond internal average particle density values using one of two
mass concentration conversion options: <code>CF=1</code> for indoor PM,
and <code>CF=ATM</code> for outdoor PM. Both are available for
visualization on the dashboard, regardless of reported monitor location.
More information can be found on the [PurpleAir
FAQ](https://www2.purpleair.com/community/faq#hc-what-is-the-difference-between-cf-1-and-cf-atm).

Correction factors are incorporated from the [Environmental Protection
Agency (EPA)](https://www.epa.gov/) and [Lane Regional Air Protection
Agency (LRAPA)](https://www.lrapa.org/) via calculations using the
reported PM<sub>2.5</sub>, temperature, and humidity values. Both are
available as variables of interest for visualizations. The EPA factor
was based on US-wide PurpleAir data and revised in late 2020 to
incorporate adjustments for high-PM events (such as forest fires). The
LRAPA correction was established in the Pacific Northwest in order to
accommodate the unique airshed (the PM of the region is proportionally
comprised of more woodsmoke than other areas of the country). It should
be noted, however, that the LRAPA correction factor cannot be applied to
values &gt;65 μg<sup>3</sup>.

In addition to PM, other reported values are available for
visualization, using different color palettes in order to provide
greater distinction. Reported temperature (in both ℃ and ℉) is
available, as well as humidity (as a percentage).

##### Map

The <b>map</b> visualizes spatio-temporal atmospheric data using points
on a map, colored by a specified variable. It is generated via the
<a href="https://github.com/gmcginnis/AirVizR/blob/main/R/map_stad.R"><code>map\_stad()</code></a>
function from [AirVizR](https://github.com/gmcginnis/AirVizR). Below is
an example:

<img src="README_files/figure-gfm/example_map_stad-1.png" width="672" />

Change the background image using the <i>map type</i> input. If the
streets appear to be fuzzy, increase the <i>map zoom value</i>. Adjust
the sizes of points using the <i>point size</i>.

##### Heatmap

The <b>multiple heatmap</b> visualizes temporal atmospheric data for
multiple monitors using a heatmap. It is generated via the
<a href="https://github.com/gmcginnis/AirVizR/blob/main/R/heatmap_cross.R"><code>heatmap\_cross()</code></a>
function from [AirVizR](https://github.com/gmcginnis/AirVizR). Below is
an example:

![](README_files/figure-gfm/example_heatmap_cross-1.png)<!-- -->

Optionally <i>drop incomplete sets</i> (i.e., monitors with missing or
otherwise removed data).

##### Hourly heatmap

The <b>hourly heatmap</b> visualizes hourly atmospheric data for a
single monitor (selected via the <i>site of interest</i> input) using a
heatmap. It is generated via the
<a href="https://github.com/gmcginnis/AirVizR/blob/main/R/heatmap_single.R"><code>heatmap\_single()</code></a>
function from [AirVizR](https://github.com/gmcginnis/AirVizR). Below is
an example:

![](README_files/figure-gfm/example_heatmap_single-1.png)<!-- -->

As with the regular heatmap, one may <i>drop incomplete sets</i> (i.e.,
monitors with missing or otherwise removed data). As mentioned before,
select the monitor of interest using the <i>site of interest</i>
setting.

##### Classic time series

The <b>time series</b> visualizes temporal atmospheric data for multiple
monitors, with optional minimum and maximum labels and a moving average.
It is generated via the
<a href="https://github.com/gmcginnis/AirVizR/blob/main/R/ts_line.R"><code>ts\_line()</code></a>
function from [AirVizR](https://github.com/gmcginnis/AirVizR). Below is
an example:

<img src="README_files/figure-gfm/example_ts_line-1.png" width="672" />

Select or remove monitors using the <i>sites of interest</i> setting; it
should be noted that this does not remove the monitors from the shadowed
time series lines (i.e., those in gray), but instead selects for those
that will be spotlighted and labeled from the set. Aesthetic
customization allow for the addition of <i>data points</i> as dots,
<i>extrema</i> labels, and an addition of an <i>averaged</i> trend. The
colors for each extrema and/or the average trend can also be customized.

##### Further customization

Each of the four visualization options have their own custom settings,
as described below each example figure above.

A notable customization that is available for all four options is the
“color cap”. If a certain value or values are “washing out” the color
palette on a visualization (as with the “multiple heatmap” above), a
color cap can be applied which will color all values at or above a
specified value to a different, discrete color.

Below is an example with the cap value set to <code>50</code>, and the
color for said cap set to <code>green</code>:

![](README_files/figure-gfm/example_cap-1.png)<!-- -->

### Acknowledgments

-   <b>[Neighbors for Clean Air
    (NCA)](https://neighborsforcleanair.org/)
-   [Reed College](https://reed.edu/)
-   [Meyer Memorial Trust](https://mmt.org/)
-   [OSU Pacific Northwest Center for Translational Environmental Health
    Research Pilot Project Program](https://ehsc.oregonstate.edu/pilot)

#### Special thanks:

-   Dr. Juliane Fry, Reed College
-   Mary Peveto, NCA
-   Micah Bishop, NCA</b>
-   Dr. Christine Kendrick, City of Portland
-   Lance Giles, Lane Regional Air Protection Agency (LRAPA)
-   ACSI Air Toxins Subcommittee, Multnomah County
-   John Wasiutynski, Multnomah County
-   Knowledge Murphy, Multnomah County
