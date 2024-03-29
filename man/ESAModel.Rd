% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ESAModel.R
\name{ESAModel}
\alias{ESAModel}
\title{Title: ESAModel
Author: Edmund Haacke
Date created: 22.09.2021
Date modified: 22.09.2021
Changelog:
- 22.09.21: file created.
Description:}
\description{
Title: ESAModel
Author: Edmund Haacke
Date created: 22.09.2021
Date modified: 22.09.2021
Changelog:
- 22.09.21: file created.
Description:

Title: ESAModel
Author: Edmund Haacke
Date created: 22.09.2021
Date modified: 22.09.2021
Changelog:
- 22.09.21: file created.
Description:
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{ESAModel$new()}}
\item \href{#method-runScenario}{\code{ESAModel$runScenario()}}
\item \href{#method-results.slot.avg}{\code{ESAModel$results.slot.avg()}}
\item \href{#method-getResults}{\code{ESAModel$getResults()}}
\item \href{#method-getCoefficientPlots}{\code{ESAModel$getCoefficientPlots()}}
\item \href{#method-getBedOccupancyPlots}{\code{ESAModel$getBedOccupancyPlots()}}
\item \href{#method-getRegressionDatasets}{\code{ESAModel$getRegressionDatasets()}}
\item \href{#method-getSampleAvgs}{\code{ESAModel$getSampleAvgs()}}
\item \href{#method-getRegressionTable}{\code{ESAModel$getRegressionTable()}}
\item \href{#method-getAMEs}{\code{ESAModel$getAMEs()}}
\item \href{#method-clone}{\code{ESAModel$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ESAModel$new(
  ESAEDAggregated,
  model.type = c("poisfe", "olsfe"),
  bedOccupancy = NULL,
  slotVars = NULL,
  nonSlotVars = NULL,
  fixedEffects = NULL,
  printSummary = FALSE,
  withAME = FALSE
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-runScenario"></a>}}
\if{latex}{\out{\hypertarget{method-runScenario}{}}}
\subsection{Method \code{runScenario()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ESAModel$runScenario(scenario)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-results.slot.avg"></a>}}
\if{latex}{\out{\hypertarget{method-results.slot.avg}{}}}
\subsection{Method \code{results.slot.avg()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ESAModel$results.slot.avg(sig = 0.1, min.slot.sig = 3)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getResults"></a>}}
\if{latex}{\out{\hypertarget{method-getResults}{}}}
\subsection{Method \code{getResults()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ESAModel$getResults(
  sig = 0.1,
  min.slot.sig = 3,
  useSampleAvgs = TRUE,
  from = "max",
  to = "mean"
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getCoefficientPlots"></a>}}
\if{latex}{\out{\hypertarget{method-getCoefficientPlots}{}}}
\subsection{Method \code{getCoefficientPlots()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ESAModel$getCoefficientPlots(exclusions = NULL)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getBedOccupancyPlots"></a>}}
\if{latex}{\out{\hypertarget{method-getBedOccupancyPlots}{}}}
\subsection{Method \code{getBedOccupancyPlots()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ESAModel$getBedOccupancyPlots(
  sig = 0.1,
  min.slot.sig = 3,
  useSampleAvgs = TRUE,
  groupSize = 5,
  reverse = FALSE
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getRegressionDatasets"></a>}}
\if{latex}{\out{\hypertarget{method-getRegressionDatasets}{}}}
\subsection{Method \code{getRegressionDatasets()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ESAModel$getRegressionDatasets()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getSampleAvgs"></a>}}
\if{latex}{\out{\hypertarget{method-getSampleAvgs}{}}}
\subsection{Method \code{getSampleAvgs()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ESAModel$getSampleAvgs(bo = TRUE)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getRegressionTable"></a>}}
\if{latex}{\out{\hypertarget{method-getRegressionTable}{}}}
\subsection{Method \code{getRegressionTable()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ESAModel$getRegressionTable(exclusions = NULL)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getAMEs"></a>}}
\if{latex}{\out{\hypertarget{method-getAMEs}{}}}
\subsection{Method \code{getAMEs()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ESAModel$getAMEs()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ESAModel$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
