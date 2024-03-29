% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JuniperDatasetProvider.R
\name{JuniperDatasetProvider}
\alias{JuniperDatasetProvider}
\title{SPIM private data}
\description{
SPIM private data

SPIM private data
}
\section{Super classes}{
\code{\link[ukcovidtools:PassthroughFilesystemCache]{ukcovidtools::PassthroughFilesystemCache}} -> \code{\link[ukcovidtools:DataProvider]{ukcovidtools::DataProvider}} -> \code{\link[ukcovidtools:CovidTimeseriesProvider]{ukcovidtools::CovidTimeseriesProvider}} -> \code{JuniperDatasetProvider}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{JuniperDatasetProvider$new()}}
\item \href{#method-getBristolData}{\code{JuniperDatasetProvider$getBristolData()}}
\item \href{#method-bristolSurvivalSubset}{\code{JuniperDatasetProvider$bristolSurvivalSubset()}}
\item \href{#method-clone}{\code{JuniperDatasetProvider$clone()}}
}
}
\if{html}{
\out{<details ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="PassthroughFilesystemCache" data-id="getDaily">}\href{../../ukcovidtools/html/PassthroughFilesystemCache.html#method-getDaily}{\code{ukcovidtools::PassthroughFilesystemCache$getDaily()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="PassthroughFilesystemCache" data-id="getHashCached">}\href{../../ukcovidtools/html/PassthroughFilesystemCache.html#method-getHashCached}{\code{ukcovidtools::PassthroughFilesystemCache$getHashCached()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="PassthroughFilesystemCache" data-id="getSaved">}\href{../../ukcovidtools/html/PassthroughFilesystemCache.html#method-getSaved}{\code{ukcovidtools::PassthroughFilesystemCache$getSaved()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="PassthroughFilesystemCache" data-id="unloadCache">}\href{../../ukcovidtools/html/PassthroughFilesystemCache.html#method-unloadCache}{\code{ukcovidtools::PassthroughFilesystemCache$unloadCache()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="DataProvider" data-id="ageCatToFactor">}\href{../../ukcovidtools/html/DataProvider.html#method-ageCatToFactor}{\code{ukcovidtools::DataProvider$ageCatToFactor()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="DataProvider" data-id="ageToAgeCat">}\href{../../ukcovidtools/html/DataProvider.html#method-ageToAgeCat}{\code{ukcovidtools::DataProvider$ageToAgeCat()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="DataProvider" data-id="breakFromCats">}\href{../../ukcovidtools/html/DataProvider.html#method-breakFromCats}{\code{ukcovidtools::DataProvider$breakFromCats()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="DataProvider" data-id="cutByAge">}\href{../../ukcovidtools/html/DataProvider.html#method-cutByAge}{\code{ukcovidtools::DataProvider$cutByAge()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="DataProvider" data-id="download">}\href{../../ukcovidtools/html/DataProvider.html#method-download}{\code{ukcovidtools::DataProvider$download()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="DataProvider" data-id="downloadAndUntar">}\href{../../ukcovidtools/html/DataProvider.html#method-downloadAndUntar}{\code{ukcovidtools::DataProvider$downloadAndUntar()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="DataProvider" data-id="downloadAndUnzip">}\href{../../ukcovidtools/html/DataProvider.html#method-downloadAndUnzip}{\code{ukcovidtools::DataProvider$downloadAndUnzip()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="DataProvider" data-id="downloadDaily">}\href{../../ukcovidtools/html/DataProvider.html#method-downloadDaily}{\code{ukcovidtools::DataProvider$downloadDaily()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="DataProvider" data-id="labelsFromBreaks">}\href{../../ukcovidtools/html/DataProvider.html#method-labelsFromBreaks}{\code{ukcovidtools::DataProvider$labelsFromBreaks()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="DataProvider" data-id="normaliseAgeCat">}\href{../../ukcovidtools/html/DataProvider.html#method-normaliseAgeCat}{\code{ukcovidtools::DataProvider$normaliseAgeCat()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="DataProvider" data-id="normaliseGender">}\href{../../ukcovidtools/html/DataProvider.html#method-normaliseGender}{\code{ukcovidtools::DataProvider$normaliseGender()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="CovidTimeseriesProvider" data-id="complete">}\href{../../ukcovidtools/html/CovidTimeseriesProvider.html#method-complete}{\code{ukcovidtools::CovidTimeseriesProvider$complete()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="CovidTimeseriesProvider" data-id="completeAllRegions">}\href{../../ukcovidtools/html/CovidTimeseriesProvider.html#method-completeAllRegions}{\code{ukcovidtools::CovidTimeseriesProvider$completeAllRegions()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="CovidTimeseriesProvider" data-id="describe">}\href{../../ukcovidtools/html/CovidTimeseriesProvider.html#method-describe}{\code{ukcovidtools::CovidTimeseriesProvider$describe()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="CovidTimeseriesProvider" data-id="describeErrors">}\href{../../ukcovidtools/html/CovidTimeseriesProvider.html#method-describeErrors}{\code{ukcovidtools::CovidTimeseriesProvider$describeErrors()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="CovidTimeseriesProvider" data-id="fillAbsent">}\href{../../ukcovidtools/html/CovidTimeseriesProvider.html#method-fillAbsent}{\code{ukcovidtools::CovidTimeseriesProvider$fillAbsent()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="CovidTimeseriesProvider" data-id="fillAbsentAllRegions">}\href{../../ukcovidtools/html/CovidTimeseriesProvider.html#method-fillAbsentAllRegions}{\code{ukcovidtools::CovidTimeseriesProvider$fillAbsentAllRegions()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="CovidTimeseriesProvider" data-id="fillAbsentByRegion">}\href{../../ukcovidtools/html/CovidTimeseriesProvider.html#method-fillAbsentByRegion}{\code{ukcovidtools::CovidTimeseriesProvider$fillAbsentByRegion()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="CovidTimeseriesProvider" data-id="fixDatesAndNames">}\href{../../ukcovidtools/html/CovidTimeseriesProvider.html#method-fixDatesAndNames}{\code{ukcovidtools::CovidTimeseriesProvider$fixDatesAndNames()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="CovidTimeseriesProvider" data-id="setTruncation">}\href{../../ukcovidtools/html/CovidTimeseriesProvider.html#method-setTruncation}{\code{ukcovidtools::CovidTimeseriesProvider$setTruncation()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="CovidTimeseriesProvider" data-id="timeseriesQA">}\href{../../ukcovidtools/html/CovidTimeseriesProvider.html#method-timeseriesQA}{\code{ukcovidtools::CovidTimeseriesProvider$timeseriesQA()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="ukcovidtools" data-topic="CovidTimeseriesProvider" data-id="trimNAs">}\href{../../ukcovidtools/html/CovidTimeseriesProvider.html#method-trimNAs}{\code{ukcovidtools::CovidTimeseriesProvider$trimNAs()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{JuniperDatasetProvider$new(providerController, fileProvider, ...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getBristolData"></a>}}
\subsection{Method \code{getBristolData()}}{
Load Bristol data
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{JuniperDatasetProvider$getBristolData(path)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{path}}{- path to the bristol data file}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
raw Bristol data set
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-bristolSurvivalSubset"></a>}}
\subsection{Method \code{bristolSurvivalSubset()}}{
Load Bristol survival subset
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{JuniperDatasetProvider$bristolSurvivalSubset(bristolDf)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{path}}{- path to the ff100 file}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Bristol survival subset
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{JuniperDatasetProvider$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
