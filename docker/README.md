# Docker Builds for LinguaQuanta

This directory containerizes the distribution of LinguaQuanta. The container
containers all Haskell dependencies, the
[Quipper](https://www.mathstat.dal.ca/~selinger/quipper/) ecosystem, and the
LinguaQuanta toolset. Example programs are included to test each tool.

## Build Instructions

First install [Docker](https://hub.docker.com/). Afterwards, prepare the
LinguaQuanta Docker image by running the following commands.

```
cd /path/to/linguaquanta/docker
docker build --tag linguaquanta -f linguaquanta.Dockerfile .
```
