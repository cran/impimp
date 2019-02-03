\name{NEWS}
\title{News for Package \pkg{impimp}}

\section{Changes in impimp version 0.3.0 (2018-11-26)}{
  \subsection{New features}{
    \itemize{
      \item New argument \code{matchvars} in \code{impimp()}
      allowing customization of matching variables (Paul Fink).
      \item Adding of NEWS file
    }
  }
  \subsection{Internal changes}{
    \itemize{
      \item Refactoring functions enabling translation from
      \code{sprintf(gettext())} to \code{gettextf()} (Paul Fink).
    }
  }
}

\section{Changes in impimp version 0.3.1 (2019-02-03)}{
  \subsection{New features}{
    \itemize{
      \item \code{impimp()} returns names of variables which are
      imputed as an attribute (Paul Fink).
      \item \code{impimp_event()} has a new argument
      \code{isEventList} allowing to supply events now also
      programmatically as list input (Paul Fink).
    }
  }
  \subsection{Changes}{
    \itemize{
      \item \code{rbindimpimp} stores variables containing
      imputed values as \code{character} (Paul Fink).
      \item \code{rbindimpimp} returns the names of the variables
      which contain imputed values as an attribute (Paul Fink).
    }
  }
  \subsection{Bug Fix}{
    \itemize{
      \item The internal levels guessing algorithm used by
      \code{impimp} is able to cope with factor variables (Paul Fink).
    }
  }
}
