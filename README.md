# ggViz

> Grammar of Graphics in Shiny

`ggViz` allows to 

- Build `ggplot` graphs from predefined datasets or input `.csv` or excel files.
- Determine whether the variables in your dataframe are continuous or categorical.
- Input the parameters for the graphs in a "layered way", according to the principles of "Grammar of Graphics" followed by `ggplot`
- See, copy and edit in real time the `ggplot` code that generates the graph.
- Download your graphs.

## Access deployed version

[Here](https://cvmartin.shinyapps.io/ggviz/) you can find the version deployed in shinyapps.io

## Use it locally

You can install the package directly from github:

```
remotes::install_github("Vandebron/ggViz@main")
```
And run it with 
```
ggViz::run_app()
```