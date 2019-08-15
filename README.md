# Scientifica Shiny App
A Shiny app for the Scientifica fair at UZH / ETH

The main goal of this application is to show visitors the importance of multiple testing with multiple samples. 

## Procedure
Visitors are asked test blue ducks, if the distribution of an illness is the same as in the one of yellow ducks. Therefore they take blue ducks out of a pool and check their buttom if they're ill or not. 

Every duck is recorded by pressing the corresponding button and a p-value is beeing calculated and shown on a graph. If people don't want to check every duck, they are given the possibility to add five random ducks from the remaining sample.

There are also two buttons to generate a completely random sample and one sample with a "critical" condition where the p-value falls below 5%. 

## Simulation
On the second tab you can choose between two options. One is simulating new data where you can simulate 1000 samples on various sizes and one where you just use pre-simulated dataseds. The sidepanel shows you the percentages of the probability of having a p-value below 5% anywhere in the process or at the end after the complete sample.

On the right side are nine graphes where two of them are specificly choosen to be a "critical" condition, with a p-value below 5%. 

## Link to Shiny App
If you want to see the app in action you can follow this link:  https://noboss.shinyapps.io/combined/
