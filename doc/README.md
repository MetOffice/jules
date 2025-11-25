# Building the JULES User Guide

This README describes how to build the JULES User Guide.

For first time users, please create a virtual environment to build the docs.

From the `jules/doc` folder of the repository run:

```sh
conda env create -f environment.yml
```

Activate the environment:

```sh
conda activate jules-user-guide
```

Build the documentation:

```sh
make clean html
```

View the documentation:

```sh
firefox build/html/index.html
```
