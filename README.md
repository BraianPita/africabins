statebins is an alternative to choropleth maps for US States

The following functions are implemented:

-   `statebins` - creates "statebin" charts in the style of <http://bit.ly/statebins> - This version uses discrete `RColorBrewer` scales, binned by the "breaks" parameter.
-   `statebins_continuous` - creates "statebin" charts in the style of <http://bit.ly/statebins> - This version uses a continuous scale based on `RColorBrewer` scales (passing in a 6 element `RColorBrewer` palette to `scale_fill_gradientn`).

### News

-   Version `1.0.0` released

### Installation

``` {.r}
devtools::install_github("hrbrmstr/statebins")
```

### Usage

``` {.r}
library(statebins)

# current verison
packageVersion("statebins")
```

    ## [1] '1.0'

``` {.r}
# the original wapo data

dat <- read.csv("http://www.washingtonpost.com/wp-srv/special/business/states-most-threatened-by-trade/states.csv?cache=1", stringsAsFactors=FALSE)

gg <- statebins(dat, "state", "avgshare94_00", breaks=4, 
                labels=c("0-1", "1-2", "2-3", "3-4"),
                legend_title="State Groups", font_size=3, 
                brewer_pal="Blues", text_color="black", 
                plot_title="1994-2000", title_position="bottom")

gg
```

![plot of chunk unnamed-chunk-3](./_README_files/figure-markdown_github/unnamed-chunk-31.png)

``` {.r}
# continuous scale, legend on top

gg2 <- statebins_continuous(dat, "state", "avgshare01_07",
                            legend_title="State Data", legend_position="top",
                            brewer_pal="OrRd", text_color="black", font_size=3, 
                            plot_title="2001-2007", title_position="bottom")

gg2
```

![plot of chunk unnamed-chunk-3](./_README_files/figure-markdown_github/unnamed-chunk-32.png)

``` {.r}
# continuous scale, no legend

gg3 <- statebins_continuous(dat, "state", "avgshare08_12",
                            legend_title="States", legend_position="none",
                            brewer_pal="Purples", text_color="black", font_size=3, 
                            plot_title="2008-2012", title_position="bottom")

gg3
```

![plot of chunk unnamed-chunk-3](./_README_files/figure-markdown_github/unnamed-chunk-33.png)

``` {.r}
# or, more like the one in the WaPo article; i might be picking the wrong columns here. it's just for an example

sb <- function(col, title) {
  statebins(dat, "state",col, brewer_pal="Blues", text_color="black", legend_position="none", font_size=3, plot_title=title, breaks=4, labels=1:4)
}

image(1:4, 1, as.matrix(1:4), col = brewer.pal(4, name="Blues"), xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
```

![plot of chunk unnamed-chunk-3](./_README_files/figure-markdown_github/unnamed-chunk-34.png)

``` {.r}
grid.arrange(sb("avgshare94_00", "1994-2000"), sb("avgshare01_07", "2001-2007"), 
             sb("avgshare08_12", "2008-2012"), ncol=2)
```

![plot of chunk unnamed-chunk-3](./_README_files/figure-markdown_github/unnamed-chunk-35.png)

### Test Results

``` {.r}
library(statebins)
library(testthat)

date()
```

    ## [1] "Tue Aug 26 13:18:47 2014"

``` {.r}
test_dir("tests/")
```

    ## basic functionality :
