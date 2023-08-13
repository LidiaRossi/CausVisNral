# CausVisNral

This repository stores the code to reproduce the experiments presented in *"Model free estimation of causal effects of different stimuli on neuron activities".*

The code is divided in three main files:

1.  `download_data.R` : It downloads the data using the [AllenSDK](https://allensdk.readthedocs.io/en/latest/visual_behavior_optical_physiology.html), the official Python based toolkit provided by the Allen Institute. All the downloaded `.nwb` files are stored in a folder named `boc`. While all operations are carried out in `R` via the `reticulate` package, the script still assumes the sdk to have been already installed locally via `pip` .\
    It creates a folder called `data` and stores there the rearranged dataset files.

2.  `exploratory.R` : It reads the file in the `data` folder and creates the plots reported in the manuscript. Both plots and related data are stored in the `output` folder.

3.  `tests.R:` It reads the data in the `output/data/` sub-folder and carries out the causal tests as described in the manuscript.
