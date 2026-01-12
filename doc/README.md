# Building the JULES User Guide

> [!NOTE]
> For previous releases of the user guide, visit
> [jules-lsm.github.io](https://jules-lsm.github.io/).

This README describes how to build the JULES User Guide.

For first time users, please create a virtual environment to build the docs.

From the `jules/doc` folder of the repository run:

```sh
# Create a virtual conda environment:
conda env create -f environment.yml

# Activate the environment:
conda activate jules-user-guide

# Build the documentation:
make clean html
```

View the documentation in a browser (e.g., Firefox):

```sh
firefox build/html/index.html
```
