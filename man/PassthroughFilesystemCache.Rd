% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PassthroughFilesystemCache.R
\name{PassthroughFilesystemCache}
\alias{PassthroughFilesystemCache}
\title{Caching and dataset management}
\description{
Caching and dataset management

Caching and dataset management
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{PassthroughFilesystemCache$new()}}
\item \href{#method-getDaily}{\code{PassthroughFilesystemCache$getDaily()}}
\item \href{#method-getHashCached}{\code{PassthroughFilesystemCache$getHashCached()}}
\item \href{#method-unloadCache}{\code{PassthroughFilesystemCache$unloadCache()}}
\item \href{#method-getSaved}{\code{PassthroughFilesystemCache$getSaved()}}
\item \href{#method-clone}{\code{PassthroughFilesystemCache$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PassthroughFilesystemCache$new(
  wd = tempdir(),
  nocache = FALSE,
  debug = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getDaily"></a>}}
\subsection{Method \code{getDaily()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PassthroughFilesystemCache$getDaily(id, orElse, ...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getHashCached"></a>}}
\subsection{Method \code{getHashCached()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PassthroughFilesystemCache$getHashCached(
  object,
  operation,
  params = NULL,
  orElse,
  ...
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-unloadCache"></a>}}
\subsection{Method \code{unloadCache()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PassthroughFilesystemCache$unloadCache()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getSaved"></a>}}
\subsection{Method \code{getSaved()}}{
a pass through 2 level cache (memory / saved file / orElse function)
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PassthroughFilesystemCache$getSaved(
  id,
  orElse,
  ...,
  params = NULL,
  nocache = NULL,
  dir = NULL
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PassthroughFilesystemCache$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
