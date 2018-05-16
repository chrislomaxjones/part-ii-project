#!/bin/sh

detex docs/diss/include/introduction.tex | tr -cd '0-9A-Za-z \n' | wc -w
detex docs/diss/include/preparation.tex | tr -cd '0-9A-Za-z \n' | wc -w
detex docs/diss/include/implementation.tex | tr -cd '0-9A-Za-z \n' | wc -w
detex docs/diss/include/evaluation.tex | tr -cd '0-9A-Za-z \n' | wc -w
detex docs/diss/include/conclusion.tex | tr -cd '0-9A-Za-z \n' | wc -w
