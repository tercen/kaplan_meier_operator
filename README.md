# Kaplan-Meier operator

##### Description

The `Kaplan-Meier operator` is an operator to fit a Kaplan-Meier curve to the data.

##### Usage

Input projection|.
---|---
`x-axis`        | numeric, time 
`y-axis`        | numeric, survival status 
`colors`        | factors, covariates

Output relations|.
---|---
`results table` | Model results
`plot`        | Graph of the Kaplan-Meier curve

##### Details

The computation is based on the `survival` R package.

