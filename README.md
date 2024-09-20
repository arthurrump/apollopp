# Apollo++ Prototypes

This repository contains prototypes for parts of Apollo++, a tool for automated assessment of programming projects. The prototype works for projects in Java, as well as Processing projects through a pre-processing step. These parts are divided across four folders:

- [*extractor*](extractor/README.md): extracts graph representations from code. Currently, it creates a typegraph (with classes, methods, fields and their relations) for each project. This part is written in [Rascal](https://www.rascal-mpl.org).
- [*graphmatcher*](graphmatcher/README.md): matches typegraphs created by the extractors with defined code patterns. The tool includes a batch functionality to run the patterns on a set of target projects and a watch-mode to run patterns on a set of targets each time the patternfile changes. The latter is useful to quickly see some results and get suggestions for extensions while writing the pattern definitions. This part is written in F#.
- [*pre_process*](pre_process/README.md) contains a few scripts used to anonymize student projects used for evaluation, prepare the dataset for analysis and to convert Processing projects to Java code. These scripts are written in Python.
- [*suggest_llm*](suggest_llm/README.md): commandline tool based on [TypeChat](https://microsoft.github.io/TypeChat/) to get suggestions from ChatGPT for patterns based on asssessment criteria.

The folder *[criteria](criteria/README.md)* contains the patterns for some of the assessment criteria for the courses we considered in the research.

## Running the pipeline

The main assessment pipeline (so not including the configuration phase), consists of the pre-processing scripts if required, the extractor and finally the graph matcher. Each part can run stand-alone and details on running them is available in their respective READMEs. To make things easier, there is a `run.py` script which runs the entire pipeline:

```
> python run.py --help
usage: run.py [-h] -l {java,processing} -m {single,s,batch,b} -c CRITERIA -t TARGET [-o OUTPUT]

options:
  -h, --help            show this help message and exit
  -l {java,processing}, --language {java,processing}
                        Programming language used in the project(s)
  -m {single,s,batch,b}, --mode {single,s,batch,b}
                        Whether to run on a single (s) project or in batch (b) mode on multiple projects
  -c CRITERIA, --criteria CRITERIA
                        Path to a directory containing criteria rules
  -t TARGET, --target TARGET
                        Target directory containing the project or a set of projects
  -o OUTPUT, --output OUTPUT
                        Directory to write output to, rather than stdout
```

Example to run the pipeline on a set of Processing projects, applying the AiC criteria from this repository:

```
> python run.py -l processing -m batch -c criteria/AiC -t ~/path/to/projects/ -o ~/path/to/output/
```

This script expects some tools to be available on the PATH:

- *processing-java*, part of [Processing](https://processing.org/) (only when using `-l processing`)
- [*rascal*](https://www.rascal-mpl.org/), which requires the ["standalone .jar file"](https://www.rascal-mpl.org/docs/GettingStarted/DownloadAndInstallation/) to be executable under the alias *rascal*
- [*dotnet*](https://dotnet.microsoft.com/), specifically the .NET 8 SDK

Also: when using the Processing, the extractor needs to know where to find the Processing core library and user-installed libraries. These locations are configured using the environment variables described in the [README for the extractor](extractor/README.md).

To make fulfilling these requirements easier, a [Nix](https://nixos.org/) environment is available that configures everything correctly on Linux. It can be started using `nix develop` in the root of this repository. This requires Nix to be installed with flake support, which is easiest to do with the [Determinate Nix Installer](https://github.com/DeterminateSystems/nix-installer).
