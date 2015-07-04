# Problems running ANT #

The build.xml script provided with the legstar-pli2cob distribution is quite simple and yet you might (indeed you will probably) run into one of several issues related with  class loading.

Let's refer to the location where you unzipped the legstar-pli2cob distribution as HOME\_PLI2COB.

Most of these problems are the consequence of conflicts between classes from HOME\_PLI2COB/lib and classes in ANT\_HOME/lib or ${user.home}/.ant/lib.

Furthermore, you might get different behaviors when running the ANT script from the command line or using an Eclipse launcher.

ANT does not load all libraries using the same classloader. In my case, when I run ANT from the command line:

  * libraries from ANT\_HOME/lib are loaded by ANT using an instance of java.net.URLClassLoader

  * libraries defined to taskdef in build.xml (i.e. the HOME\_PLI2COB/lib libraries) are loaded with an instance of org.apache.tools.ant.loader.AntClassLoader2

These 2 class loaders are not parent of one another (although they both have the same parent hierarchy).

The same class, loaded by 2 class loaders which are not parent to one another, are considered 2 different classes by the JVM. As a consequence, any class that is both in ANT\_HOME/lib and HOME\_PLI2COB/lib might cause trouble.

The following are typical errors you might get.

## CharScanner; panic: ClassNotFoundException: org.antlr.stringtemplate.language.ChunkToken ##

### Reason ###

This exception happens when a library in ANT\_HOME/lib contains ANTLR libraries (typically antlr-2.7.7.jar or any other jar containing these classes).

In this case, a class cast within StringTemplate libraries (loaded from HOME\_PLI2COB/lib) fails because antlr.CharScanner was initially loaded from ANT\_HOME/lib.

### Workaround ###

There are 2 things you can do:

  * Run ANT with the -lib command line option: ant -lib lib

  * Copy stringtemplate-3.2.jar from HOME\_PLI2COB/lib to ANT\_HOME/lib

The objective is to get ANTLR and StringTemplate to be loaded by the same class loader.

## java.lang.ClassCastException: org.antlr.stringtemplate.language.StringTemplateAST cannot be cast to org.antlr.stringtemplate.language.StringTemplateAST ##

I got this one on a fedora core 6 linux distribution and I am still unsure why it happens.

You can tell from the message that StringTemplateAST was loaded by 2 different class loaders and therefore we have 2 different classes from a cast standpoint.

My guess is that some StringTemplate classes are in some jars in ANT\_HOME/lib.

## WARN No appenders could be found for logger (com.legstar.pli2cob.task.PLIStructureToCobolTask). ##

### Reason ###

This one is easy, there is a log4j jar in ANT\_HOME/lib. When it gets loaded, the class loader has no access to HOME\_PLI2COB/conf where the log4j.properties file reside.

### Workaround ###

Run ant with -lib option: ant -lib conf. If you already use the -lib option to overcome one of the previous issues then copy log4j.properties to lib.

## Running ant from Eclipse ##

In my experience, none of the problems mentioned here occur when you run the build.xml from Eclipse.

Eclipse has its own ant libraries it gets from a plugin. Typically the ant libraries in this plugin are the bare minimum needed to run ant.

Another reason why the previous issues do not happen with Eclipse is that there is only one class loader used for both the ant libraries and the libraries defined to taskdef.

# Conclusion #

Whenever you can, just use Eclipse to run the ant script.

If you can't and start hitting the kind of issues mentioned here, look for potential conflicts at the antlr/stringtemplate/log4j level.

Also note that the legstar-pli2cob delivery now contains a standalone executable jar that you can use as an alternative to the ant script.
