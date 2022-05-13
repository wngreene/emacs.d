#!/usr/bin/env bash

# Install depdencies needed for my emacs config.
sudo apt-get install clangd

pip3 install --user python-lsp-server
pip3 install --user pylint
pip3 install --user cpplint
