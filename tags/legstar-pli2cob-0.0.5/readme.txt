Overview:
=========
  The pli2cob utility translates a set of PL/I structure definitions to
  COBOL data descriptions.
  
  The utility is available as a standalone executable jar or as an ANT task.

Prerequisites:
=============
  You need Java JRE 1.5 or higher.
  
  If you want to use the ANT task, you need ANT version 1.6.5.

Running the sample using standalone executable jar:
==================================================

  1. Go to the folder where you unzipped the distribution file. It should
     contain run.sh (Linux) and run.bat (Windows) command files.
     The pl1 sub folder is used as input. It contains a single PL/I file.
  2. Type ./run.sh (Linux) or run (Windows).
  3. Check the cobol sub folder, a cobol source file should have been created.

Troubleshooting standalone executable jar:
=========================================
   The run.sh and run.bat files have a commented out option that turns
   debugging on.

   Uncomment and run again to get more details on what went wrong.

   Checkout the group discussion list.
   
   If you can't find a solution, please file a report at:
   http://code.google.com/p/legstar-pli2cob/issues/list
   
Running the sample using ANT:
=============================

  1. Go to the folder where you unzipped the distribution file. It should
     contain a build.xml ANT script.
  2. Type ant.
  3. Check the cobol sub folder, a cobol source file should have been created.

Troubleshooting ANT:
===================

   The conf sub folder contains a log4j configuration file. Set the debug
   level to get more information on errors.
   
   You can also run ant with the -v option.
   
   Checkout the group discussion list.
   
   If you can't find a solution, please file a report at:
   http://code.google.com/p/legstar-pli2cob/issues/list
   
Additional information:
=======================
  
  Check the wiki pages: http://code.google.com/p/legstar-pli2cob/w/list.
  
  Javadoc: http://www.legsem.com/legstar/pli2cob/apidocs/index.html 
  
  Join the discussion group: http://groups.google.com/group/legstar-user.
