scala-loom
==========

A digital loom written in Scala

This program takes a script file that describes the set-up of a traditional pedalled loom, the threading and weaving
and produces a picture that represents the cloth produced in this way. The picture can be copied to clipboard or stored
in PNG format.

loom/Pattern.scala - classes to describe and manipulate patterns. Patterns are basically lists of positive integers
but they can be added (concatenated), multiplied and reversed.  These patterns are used to define the loom setup,
the threading, the pedalling and the colors.

loom/PatternParser.scala - an interpreter for scripts that define patterns, using parser-combinations.

loom/Loom.scala - the actual loom: produces a weaving pattern

loom/Cloth.scala - translates a weaving pattern into a picture, using colors

main.scala - the Swing GUI

An executable jar file and a usr manual have been provided with release v1.0.

