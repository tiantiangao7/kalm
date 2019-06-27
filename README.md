# Knowledge Authoring Logic Machine (KALM)
Authors: Tiantian Gao, Paul Fodor, Michael Kifer

# Introduction
The KALM system allows knowledge authoring with the aim of endowing domain experts with tools that would allow them to translate their knowledge into logic by means of a Controlled Natural Language (CNL). We also develop the query service to support question answering based on the authored knowledge. The features of KALM are five-fold:
* A formal, FrameNet-inspired ontology FrameOnt that formalizes FrameNet frames and integrates linguistic resources from BabelNet to represent the meaning of English sentences.
* An incrementally-learned semantic parser that disambiguates CNL sentences by mapping semantically equivalent sentences into the same FrameOnt frames and assigns them unique logical representation (ULR). 
* Explainability: the approach makes it possible to explain both why particular meanings are assigned and also why mistakes were made.
* A hybrid CNL-based language for authoring queries.
* Both knowledge authoring and question answering parts achieve superior accuracy.

KALM-QA (https://github.com/tiantiangao7/kalm-qa) is the question answering part of KALM.

# Academic papers (high-level description of the system)
1. Knowledge Authoring for Rule-Based Reasoning. Tiantian Gao, Paul Fodor, Michael Kifer. ODBASE, OTM Conferences 2018: 461-480.  https://github.com/tiantiangao7/kalm/blob/master/docs/KALM_ODBASE18.pdf
2. High Accuracy Question Answering via Hybrid Controlled Natural Language. Tiantian Gao, Paul Fodor, Michael Kifer. Web Intelligence (WI), 2018. https://github.com/tiantiangao7/kalm/blob/master/docs/QA_WI18.pdf

# Dependencies
1. Java JRE 1.8 (https://www.java.com/en/download/)
2. XSB Prolog Version 3.7 (http://xsb.sourceforge.net/)
3. XSB Prolog version of Attempto Parsing Engine (APE) under LGPL licence (included in this repository)
4. APE Clex under GPL licence (http://attempto.ifi.uzh.ch/site/downloads/files/) (included in this repository)
5. BabelNet 3.7.1 Java API. (http://babelnet.org/download)
6. BabelNet 3.7 indices (http://babelnet.org/download)


# Installation and Configuration
1. Download BabelNet 3.7 indices. User must request access from http://babelnet.org/download. The license of BabelNet 3.7 indices applies.
2. Download BabelNet 3.7.1 API. Include all the jar files in `lib/`. Include `config/` directory under the root directory. (Further configurations, please follow BabelNet 3.7.1 API doc).
3. Install XSB Prolog Version 3.7 (http://xsb.sourceforge.net/). The license of XSB Prolog is GNU Library or Lesser General Public License version 2.0 (LGPLv2).
4. Add the installation path to XSB Prolog to runxsb.sh.
5. Add the path to BabelNet 3.7 indices to `config/babelnet.var.properties` (Note that this file is not included in this repository. It comes with BabelNet 3.7 API).

# Code
* `src/` Java source code for UI, semantic score computation, meta data deserialization for frame property/semantic link override/semantic score parameters/candidate parse results.
* `scripts/` XSB Prolog code for APE engine + candidate frame parses.
* `config/` BabelNet config files (please download from BabelNet website specified in requirement section).
* `lib/` BabelNet lib files (please download from BabelNet website specified in requirement section).
* `resources/jlt/` BabelNet resources files (please download from BabelNet website specified in requirement section). User has to create the directory by hand.
* `resources/wnplusplus/` BabelNet resources files (please download from BabelNet website specified in requirement section). User has to create the directory by hand.
* `resources/batch/batch.txt` A collection of CNL sentences used for batch processing. The default file is the test suite for the system.
* `resources/scores/score.txt` The file containing the frame extraction results. For each sentences, it shows all of the extracted candidate parses with semantic scores, the disambiguated filler-word BabelNet synsets, and semantic paths connecting the filler word to the synset. User has to create the directory by hand. 
* `resources/frame_property/frame_property.txt` The file containing the frame descriptions.
* `resources/semantic_score_meta/` Files containing the weight bias scores, edge penalty scores, and overriden semantic links.
* `testsuite/` Test suite for frame extraction.
* `runxsb.sh` The shell script for running XSB Prolog. User must specify the installation path to XSB Prolog.
* `runbatch.sh` The shell script to process CNL sentences in batch mode.
* `start.sh` The shell script to start the KALM system, the GUI.

# How to add a new frame?
1. open `resources/frame_property/frame_property.txt`
2. Add a new frame at the end of the line. e.g., 
   `fp('Growing_food',[property('Grower',['bn:00046516n']),property('Food',['bn:00035650n','bn:00035649n'])]).` 

# How to compose an annotated sentence and learn a new lvp?
```
cd scripts/prolog/ape
xsb
?-['mk.pl'].
?-annotate_sentence('Mary has a job at IBM in London.',1/4,'Being_employed',[pair('Employee',1/1,required),pair('Position',1/4,required),pair('Employer',1/6,optional),pair('Place',1/8,optional)],[],LVP).
```

# How to run the GUI
1. Command line: `./start.sh`
2. Start from Eclipse (recommended).

# How to run the batch mode for testing?
1. Command line: `./runbatch.sh`
In batch mode, the program read sentences from file and then serialize the frame extraction results.

# License
The license of the KALM code is BSD 3-Clause License.
