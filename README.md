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

Note:
You will probably need to install dependancies such as the GNU Scientific Library.

On debian:
```unix
sudo apt-get install libgsl-dev
```


* Additional information (https://opam.ocaml.org/doc/Usage.html)

## Tool compilation

```unix
make
```

## Usages

### Trace and XML

ctx mode: classify anomalies annotated in an xml file and located in a network trace using:

* anomaly_taxonomy: a description of network anomalies (e.g. http://www.fukuda-lab.org/mawilab/classification/taxonomy_v1.23.zip to uncompress)
* trace.dump: PCAP traffic trace containing anomalies network traffic
* admd.xml: a XML file that describes occurring anomalies (http://admd.sourceforge.net/)

```unix
./nac.native -ctx taxonomy_v1.23/anomaly_taxonomy trace.dump admd.xml
```

### Trace and MAWILab XML

ctmx mode: classify anomalies annotated in a "anomalous/suspicious" xml file and a "notice" XML file, and, located in a network trace using:

* anomaly_taxonomy
* trace.dump
* anomalous_suspicious_admd.xml
* notice_admd.xml

```unix
./nac.native -ctmx taxonomy_v1.23/anomaly_taxonomy trace.dump anomalous_suspicious_admd.xml notice_admd.xml
```


### Trace

ct mode: classify anomalies in a network trace using:

* anomaly_taxonomy
* trace.dump

```unix
./nac.native -ctmx taxonomy_v1.23/anomaly_taxonomy trace.dump
```


