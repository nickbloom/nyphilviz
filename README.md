# NY Philharmonic Visualizations

### How to Use

1. Download/obtain complete XML file from [here](https://github.com/nyphilarchive/PerformanceHistory/tree/master/Programs).
2. Run it through `reformat_nyphil.py`, which depends on Python 3 and BeautifulSoup 4.
3. Then you can run the cleaned stuff through the R code, which should produce valid JSON for the D3 visualizations.


### Vizs

- `philviz.R` and the code in the `compnetwork` folder produces [this](http://nickbloom.net/d3/composers/) visualization.
- `composer_sunburst.R` and the code in the `composersunburst` folder produces [this](http://nickbloom.net/d3/compins/) visualization.