% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_sim_data.R
\name{generate_sim_data}
\alias{generate_sim_data}
\title{Generate simulated binary decision data using DDM}
\usage{
generate_sim_data(
  N = 30,
  n_each = 100,
  n_xvar = 2,
  beta_0 = 0,
  beta_c1 = 0,
  beta_c2 = 0,
  beta_v_0 = 0,
  beta_v_x1 = 0,
  beta_v_x2 = 0,
  sigma_y = 1,
  sigma_v = 0,
  y_family = "gaussian"
)
}
\arguments{
\item{N}{Number of subjects.}

\item{n_each}{Number of trials per subject}

\item{n_xvar}{Number of trial-level variables influencing drift rate}

\item{beta_0}{Intercept}

\item{beta_c1}{Slope of c1}

\item{beta_c2}{Slope of c2}

\item{beta_v_0}{Slope of v_0}

\item{beta_v_x1}{Slope of v_x1}

\item{beta_v_x2}{Slope of v_x2}

\item{sigma_y}{Standard deviation of error term of y, Only used when
\code{y_family} is "gaussian"}

\item{sigma_v}{Contaminant level for drift rate v.}

\item{y_family}{Family of distribution of y. Can be either "gaussian",
"bernoulli" or "poisson"}
}
\value{
A named list with four elements. \code{data1_true} and
\code{data2_true} are true values of DDM parameters of each subject and
trial. \code{data1} and \code{data2} removed those hidden variables.
}
\description{
This function generates a simulated dataset under different configurations
It can be used to test the performance and functionality of \pkg{RegDDM}.
The outcome variable is \code{y}, which is influenced by different variables.
}
\examples{
\donttest{
sim_data = generate_sim_data()
sim_data$data1
sim_data$data2
}

}
