# Knowledge Acquisition Logic Machine (KALM)
Authors: Tiantian Gao, Paul Fodor, Michael Kifer

# Introduction
KALM is a high accuracy knowledge acquisition system from Controlled Natural Language (CNL) sentences aimed to be used for decision support. KALM performs deep semantic analysis of CNL sentences by doing standardization of the logical parses obtained from CNLs and taking advantage of the FrameNet semantic relation knowledge base and the BabelNet lexical ontology and semantic network in order to ensure that different sentences that express the same meaning are mapped into the same logical representation.

# Requirements
1. Java 1.8
2. XSB Prolog (http://xsb.sourceforge.net/)
3. XSB Prolog version of ACE Parsing Engine (APE) (included in the source code)
4. APE Clex under GPL licence (http://attempto.ifi.uzh.ch/site/downloads/files/)
5. BabelNet Java API. (http://babelnet.org/download)
6. BabelNet knowledge base (http://babelnet.org/download)
