#!/bin/bash

grep target $* | sed 's/">.*$//' | sed 's/^.*"/ls /' | bash
