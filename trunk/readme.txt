Overview:
=========
  The pli2cob ant task converts a set of PLI structure definitions to
  COBOL data descriptions.

Prerequisites:
=============
  You need ant version 1.6.5 or higher and JRE 1.5 or higher.

Running the sample:
===================

  1. Go to the folder where you unzipped the distribution file. It should
     contain a build.xml file with a reference to a sample PLI source found
     under the pli sub folder.
  2. Type ant.
  3. Check the cobol sub folder, a cobol source file should have been created.

Troubleshooting:
===============
   The conf subfolder contains a log4j configuration file. You can set the
   debug level to get more information on errors.
   
   You can also run ant with the -v option.
   
   Report issues at: http://code.google.com/p/legstar-pli2cob/issues/list
   
Using your own PLI source:
=========================
  
  Only data descriptions are supported. Sources should not contain other PLI
  statements.
  Check http://code.google.com/p/legstar-pli2cob/w/list for the data descriptions
  supported.
