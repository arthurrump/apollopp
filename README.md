# Apollo++ Prototypes

This repository contains prototypes for parts of Apollo++, a tool for automated assessment of programming projects. The prototype works for projects in Java, as well as Processing projects through a pre-processing step. These parts are divided across four folders:

- [*extractor*](extractor/README.md): extracts graph representations from code. Currently, it creates a typegraph (with classes, methods, fields and their relations) for each project. This part is written in [Rascal](https://www.rascal-mpl.org).
- [*graphmatcher*](graphmatcher/README.md): matches typegraphs created by the extractors with defined code patterns. The tool includes a batch functionality to run the patterns on a set of target projects and a watch-mode to run patterns on a set of targets each time the patternfile changes. The latter is useful to quickly see some results and get suggestions for extensions while writing the pattern definitions. This part is written in F#.
- [*pre_process*](pre_process/README.md) contains a few scripts used to anonymize student projects used for evaluation, prepare the dataset for analysis and to convert Processing projects to Java code. These scripts are written in Python.
- [*suggest_llm*](suggest_llm/README.md): commandline tool based on [TypeChat](https://microsoft.github.io/TypeChat/) to get suggestions from ChatGPT for patterns based on asssessment criteria.
