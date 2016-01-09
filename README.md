# nac (Network Anomaly Classifier)


## Install OPAM

* Install opam (https://opam.ocaml.org/doc/Install.html)


## Setup OPAM and OCaml

* Initalize OPAM in any directory of your choice (usually your home directory):
```unix
opam init
```

* List the available compiler versions:
```unix
opam switch
```
* Install OCaml:
```unix
opam switch x.y.z
```
Current latest one:
```unix
opam switch 4.02.3
```

* Add our custom opam repository:
```unix
opam remote add por-dev git@github.com:johanmazel/por-dev.git
```
or
```unix
opam remote add por-dev https://github.com/johanmazel/por-dev.git
```
* Update package list:
```unix
opam update
```

* Install our libraries:
```unix
opam install ocaml-nac_lib
```

* Additional information (https://opam.ocaml.org/doc/Usage.html)

## Tool compilation

```unix
make
```

## Usage example

ctx mode (classify anomalies annotated in an xml file and located in a network trace):
* anomaly_taxonomy: a description of network anomalies (e.g. http://www.fukuda-lab.org/mawilab/classification/taxonomy_v1.23.zip to uncompress)
* trace.dump: PCAP traffic trace containing anomalies network traffic
* admd.xml: a XML file that describes occurring anomalies (http://admd.sourceforge.net/)

```unix
./nac.native -ctx taxonomy_v1.23/anomaly_taxonomy trace.dump admd.xml
```


