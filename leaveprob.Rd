\name{leaveprob}
\alias{leaveprob}
\title{Probability of MUM leaving via resignation versus termination}
\usage{
leaveprob(disciplined, strategic, tolerant, persuasive, reserved, composed, high_stress)
}
\arguments{
\item{leaveprob}{This argument is used to obtain the likelihood results. Method to use: type in exactly as in the example, or copy and paste example, and substitute the values for the values of the person of interest. After entering new values, hit enter to get percentage in terms of resignation.
                }
\item{disciplined}{Corresponds to the Assessment trait: disorganized, procrastinator v. disciplined}
\item{strategic}{Corresponds to the Assessment trait: short-term decisions v. strategic}
\item{tolerant}{Corresponds to the Assessment trait: over-confident, opinionated v. tolerant}
\item{persuasive}{Corresponds to the Assessment trait: uninfluencing v. persuasive}
\item{reserved}{Corresponds to the Assessment trait: chatty, inattentive v. reserved, thoughtful}
\item{composed}{Corresponds to the Assessment trait: emotional v. composed, objective}
\item{high_stress}{Corresponds to the Assessment trait: low v. high stress tolerance}

}
\description{
Calculates the probability of a MUM being more likely to resign or be terminated based on a set of traits from the Assessment that were determined to be most significantly correlated with performance based on a study previously conducted.The traits used in this calculation are 'disorganized, procrastinator v. disciplined', 'short-term decisions v. strategic', 'over-confident, opinionated v. tolerant', 'uninfluencing v. persuasive', 'chatty, inattentive v. reserved, thoughtful', 'emotional v. composed, objective', and 'low v. high stress tolerance'. Probability refers to how likely the person is to resign if they decide to leave.
}
\examples{
leaveprob(disciplined = 10, strategic = 1, tolerant = 10,
            persuasive = 1, reserved = 10, composed = 10, high_stress = 10)
}
