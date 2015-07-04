# Goal #
This project aims at developing a utility to translate PL/I structures to COBOL structures.

Many mainframe integration tools have parsers for COBOL data structures. There is less support for PL/I, another important mainframe language.

By converting PL/I structures to COBOL, it becomes possible to integrate PL/I programs using tools which only support COBOL.

# Get started #

To get started, [download](http://code.google.com/p/legstar-pli2cob/downloads/list) and unzip the distribution file. The readme.txt file contains instructions on how to run the sample provided.

Checkout the [wiki](http://code.google.com/p/legstar-pli2cob/w/list) and [maven site](http://www.legsem.com/legstar/pli2cob/) for additional documentation.

# How it works #

The PL/I structure parser implementation is based on [ANTLR](http://www.antlr.org/), a great lexer/parser.

The product is written in java. You can:

  * call the [API](http://code.google.com/p/legstar-pli2cob/source/browse/trunk/src/main/java/com/legstar/pli2cob/PLIStructureToCobol.java) directly from your code

  * use run.sh/run.bat command files to call a standalone executable jar

  * use a build.xml [Apache ANT](http://ant.apache.org/) script.

# Status #

| Oct 19, 2009 | New release 0.0.5 Delivers a standalone executable jar as an alternative to ant.|
|:-------------|:--------------------------------------------------------------------------------|

List of [Fixed issues](http://code.google.com/p/legstar-pli2cob/issues/list?can=1&q=status:Fixed) so far.

This is work in progress and all contributions are very welcome. Please join us on the [Google group](http://groups.google.com/group/legstar-user).

