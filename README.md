# University Projects
Old university projects (2003-2008, Urbino and Bologna universities, computer science courses) in C, C++, .NET, Ocaml, TCL, Winsock, WxWidgets and others.

Code and reports are written in Italian.

### Command line CSV-based calendar - C++ language
Written in C++ using all the OOP capabilities.
C++  Command line software to manage (list, edit, delete, show today's and weekly deadlines, contacts management CSV) events stored in a CSV-file.
Based on a personal idea.

### C to JVM Compiler - Ocaml language + parse tools
Parser, AST and Compiler from a modified C language (also supporting bidimensional arrays) into Java JVM.
Written using the functional language [OCaml](http://en.wikipedia.org/wiki/OCaml).

### Gomory's cut implementation with HTML/CSS output - C language

Implementation of [Gomory's cut algorithm](http://en.wikipedia.org/wiki/Cutting-plane_method#Gomory.27s_cut)
to solve the [Cutting stock problem](http://en.wikipedia.org/wiki/Cutting_stock_problem)

### HTTP Server with advanced settings - C language, winsock library

Basic mono-thread HTTP Server written in C language using C winsocket libraries, with also module to interpret additional languages, ASCII/bin regocgnition, URL spaces support, MIME recognition, with fallback on unknown type, 404 on existing links,FORM post support, error log, speed settings, interlacing GIF support


### Math expression parser and type checker - C language

Parser and type checker of mathematical expression like `(((((+12)*ab)*c)/d)+4)` composed by integers, floats, constants, arithmetic operators (2 unary and 5 binary), parentheses (infinite nesting levels).

* The given expression is validated and also rewritten inserting parentheses reflecting the operators' priority. 
* Calculation of global type of the expression (e.g. considering C-language casting). Variables A-L* are int, M-Z are floats.
* Error validation
* Optimized, minimum amount of memory used, no a single extra byte of memory is being used

### OpenGL vs .NET speed comparison - .NET with tao framework

C# project written to compare speed of Microsoft Graphics vs OpenGL (without VBO) in rendering thousands of polygons, written in Italian. Employed to evaluate decision for .NET-based GIS software.

### Winsock SMTP mail sender - C language

Mail sender written with winsock, C language, to use SMTP protocol and send email. 


### Tic Tac Toe with computer player - 4 levels - AI - C++ with WxWidgets UI libs

C++ implementation of Tic-tac-toe game played by the computer at 4 levels of difficulty

An introduction of mine to WxWiget (in Italian) at this [link, page 6-7](http://www.slideshare.net/elviscio/6-32982223)


### TSP Hungarian algorithm implementation - C language

Implementation in C language of Branch-and-Bound & Branch-and-Cut Hungarian method
to solve the [Travelling salesman problem](http://en.wikipedia.org/wiki/Travelling_salesman_problem).

### Labyrinth path calculator - C language

Simple C algorithm to generate a random matrix NxM with binaries values (labyrinth where "true" values are "walkable"), calculate the tree of all the possible paths and print them out

### Network triangulation for node localisation - TCL language

University project consisting in localising a node in a network based on triangulation, written in TCL language.
