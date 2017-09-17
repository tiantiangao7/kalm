# Knowledge Acquisition Logic Machine (KALM)
Authors: Tiantian Gao, Paul Fodor, Michael Kifer

# Introduction
KALM is a high accuracy knowledge acquisition system from Controlled Natural Language (CNL) sentences aimed to be used for decision support. KALM performs deep semantic analysis of CNL sentences by doing standardization of the logical parses obtained from CNLs and taking advantage of the FrameNet semantic relation knowledge base and the BabelNet lexical ontology and semantic network in order to ensure that different sentences that express the same meaning are mapped into the same logical representation.

# Requirements
1. Java 1.8
2. XSB Prolog (http://xsb.sourceforge.net/)
3. XSB Prolog version of ACE Parsing Engine (APE) under LGPL licence (included in this repository)
4. APE Clex under GPL licence (http://attempto.ifi.uzh.ch/site/downloads/files/)
5. BabelNet 3.7.1 Java API. (http://babelnet.org/download)
6. BabelNet 3.7 indices (http://babelnet.org/download)

# Installation
1. BabelNet 3.7 indices. User must request access from http://babelnet.org/download.
2. XSB Prolog.

# Code
* `src/` Java source code for UI, semantic score computation, meta data deserialization for frame property/semantic link override/semantic score parameters/candidate parse results.
* `scripts/` XSB Prolog code for APE engine + candidate frame parses
* `bin/` BabelNet bin files (please download from BabelNet website specified in requirement section)
* `config/` BabelNet config files (please download from BabelNet website specified in requirement section)
* `lib/` BabelNet lib files (please download from BabelNet website specified in requirement section)
* `resources/jlt` BabelNet resources files (please download from BabelNet website specified in requirement section)
* `resources/wnplusplus` BabelNet resources files (please download from BabelNet website specified in requirement section)

# Run
1. GUI
* `./start.sh`
2. Bach Mode (Read sentences from file and serialize the frame extraction results into file)
* `./runbatch.sh`
