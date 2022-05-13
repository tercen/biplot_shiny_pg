# Biplot shiny operator

##### Description

Shiny application for creating a biplot from 2 layers of data.

A biplot is typically used to show both the scores and loadings of a Principal Component Analysis in a single plot but can also be used with other matrix factorization methods. 

##### Usage
- Use in Tercen is somewhat involved as two layers of data must be added to the cross tab view. See: https://tercen.com/Rik/w/02c1a6dfed523f30e97c03be927d6979
- Set the layer selector to `All` before switching to the `Operator View`

Input projection Layer 1|.
---|---
`row`           | rows contain the observations (i.e. PCA scores)
`column`        | Factor(s) creating the columns: factors that identify the observations
`colors`        | Factor(s) or variable(s) added as colors are used to colour the points for the score plots
`labels`        | A single factor that identifies the observations

Input projection Layer 2|.
---|---
`column`        | columns contain the variables (i.e. PCA loadings)
`row`           | Factor(s) creating the rows: factors that identify the variables
`labels`        | A single factor that identifies the variables

Output relations|.
---|---
`Operator view`        | Interactive biplot application, Set the layer selector to `All` before switching to the `Operator View`

