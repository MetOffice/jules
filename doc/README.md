# Building the JULES User Guide

> [!NOTE]
> For previous releases of the user guide, visit
> [jules-lsm.github.io](https://jules-lsm.github.io/).

This README describes how to build the JULES User Guide.

For first time users, please create a virtual environment to build the docs.

From the `jules/doc` folder of the repository run:

```bash
# Create a virtual environment and install dependencies (in ./doc):
cd doc
python3.12 -m venv .venv
.venv/bin/pip install .

# Activate the environment:
source .venv/bin/activate

# Build HTML documentation (in ./build/html/):
make clean html
```

At the Met Office you can also run the following to deploy the
html documents directly into `~/public_html/jules/<branch>/`:

```bash
make clean deploy
```

To generate PDF documentation, ensure you have a LaTeX distribution
installed and run the following command. The pdf will be generated as
`./build/latex/JULES_User_Guide.pdf`:

```bash
make latexpdf
```
