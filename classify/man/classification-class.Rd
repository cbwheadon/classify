\name{classification-class}
\Rdversion{1.1}
\docType{class}
\alias{classification-class}
\alias{classification}
\alias{across-reps}
\title{Class \code{"classification"}}
\description{
S4 object with classification statistics.
}
\section{Slots}{
  \describe{
    \item{\code{acc}:}{Classification accuracy summary statistic across replications.}
    \item{\code{fp}:}{False positive summary statistic across replications.}
    \item{\code{fn}:}{False negative summary statistic across replications.}
    \item{\code{cand.acc}:}{Candidate level accuracy across replications.}
    \item{\code{cand.fn}:}{Candidate level false negative across replications.}
    \item{\code{cand.fp}:}{Candidate level false positive across replications.}
    \item{\code{consist}:}{Classification consistency summary statistic across replications.}
    \item{\code{cand.consist}:}{Candidate level classification consistency across replications.}
    \item{\code{Xij}:}{Summary matrix of classification probability into all grade combinations.}
    \item{\code{kappa}:}{Kappa value for classification accuracy across replications.}
    \item{\code{item.scores}:}{Raw item scores used.}
	\item{\code{bnds}:}{Grade bounaries used.}
    \item{\code{tru.grades}:}{Candidate grades.}
	\item{\code{tru.scores}:}{Candidate true scores.}
    \item{\code{acc.by.grade}:}{Accuracy by grade.}
	\item{\code{fp.by.grade}:}{False positive by grade.}
	\item{\code{fn.by.grade}:}{False negative by grade.}
	\item{\code{consist.by.grade}:}{Consistency by grade.}
	\item{\code{labels}:}{Grade labels.}
    \item{\code{m.acc}:}{Mean of Classification accuracy summary statistic across replications.}
    \item{\code{m.consist}:}{Mean of Candidate level classification consistency across replications.}
    \item{\code{m.kappa}:}{Mean of Kappa value for classification accuracy across replications.}
    \item{\code{m.fp}:}{Mean of Candidate level false positive across replications.}
    \item{\code{m.fn}:}{Mean of Candidate level false negative across replications.}
    \item{\code{sd.acc}:}{SD of Classification accuracy summary statistic across replications.}
    \item{\code{sd.consist}:}{SD of Candidate level classification consistency across replications.}
    \item{\code{sd.kappa}:}{SD of Kappa value for classification accuracy across replications.}
    \item{\code{sd.fp}:}{SD of Candidate level false positive across replications.}
    \item{\code{sd.fn}:}{SD of Candidate level false negative across replications.}
	\item{\code{m.acc.by.grade}:}{Mean accuracy by grade.}
	\item{\code{m.fp.by.grade}:}{Mean false positive by grade.}
	\item{\code{m.fn.by.grade}:}{Mean false negative by grade.}
	\item{\code{m.consist.by.grade}:}{Mean consistency by grade.}
	\item{\code{max}:}{Maximum test score.}
  }
}
\section{Methods}{
  \describe{
    \item{plot}{Plots.}
    \item{across.reps}{Summarise stats across replications.}
    \item{summary}{Summary statistics.}
  }
}
