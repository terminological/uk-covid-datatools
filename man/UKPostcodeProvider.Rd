% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UKPostcodeProvider.R
\name{UKPostcodeProvider}
\alias{UKPostcodeProvider}
\title{Get a provider of UK postcodes}
\description{
Get a provider of UK postcodes

Get a provider of UK postcodes
}
\section{Super classes}{
\code{\link[ukcovidtools:PassthroughFilesystemCache]{ukcovidtools::PassthroughFilesystemCache}} -> \code{\link[ukcovidtools:DataProvider]{ukcovidtools::DataProvider}} -> \code{UKPostcodeProvider}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{UKPostcodeProvider$new()}}
\item \href{#method-getFullONS}{\code{UKPostcodeProvider$getFullONS()}}
\item \href{#method-getOutcodeCentroids}{\code{UKPostcodeProvider$getOutcodeCentroids()}}
\item \href{#method-lookupFeatures}{\code{UKPostcodeProvider$lookupFeatures()}}
\item \href{#method-lookupWeightedFeatureByOutcode}{\code{UKPostcodeProvider$lookupWeightedFeatureByOutcode()}}
\item \href{#method-lookupLocation}{\code{UKPostcodeProvider$lookupLocation()}}
\item \href{#method-clone}{\code{UKPostcodeProvider$clone()}}
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
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UKPostcodeProvider$new(providerController, ...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getFullONS"></a>}}
\subsection{Method \code{getFullONS()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UKPostcodeProvider$getFullONS(...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getOutcodeCentroids"></a>}}
\subsection{Method \code{getOutcodeCentroids()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UKPostcodeProvider$getOutcodeCentroids(...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-lookupFeatures"></a>}}
\subsection{Method \code{lookupFeatures()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UKPostcodeProvider$lookupFeatures(df, postcodeVar = "pcd", onspdVars)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-lookupWeightedFeatureByOutcode"></a>}}
\subsection{Method \code{lookupWeightedFeatureByOutcode()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UKPostcodeProvider$lookupWeightedFeatureByOutcode(
  df,
  outcodeVar = "outcode",
  onspdVar
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-lookupLocation"></a>}}
\subsection{Method \code{lookupLocation()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UKPostcodeProvider$lookupLocation(df, postcodeVar = "pcd")}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UKPostcodeProvider$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
