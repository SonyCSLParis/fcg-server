# fcg-interactive
FCG Interactive Web Interface, Web Server, and API. Version 2.0, replaces v1.0.

## Introduction

This project consisted in a partial redesign and reimplementation of the FCG Interactive web service and API (https://www.fcg-net.org/fcg-interactive). The main goals of this redesign and reimplementation were to ensure that there is an easy and intuitive way to update the grammars that are included.

The solution presented in this document consists of two systems, called :fcg-server and :fcg-interactive. :fcg-server is a general framework for implementing web services around FCG-powered systems. :fcg-interactive builds on the :fcg-server system and implements functionality that powers the FCG Interactive front-end. The front-end of FCG interactive was redesigned while keeping the look-and-feel of the previous implementation.

The code is structured in three folders. The fcg-server and fcg-interactive folders contain the source code of the :fcg-server and :fcg-interactive systems mentioned above. The www folder contains the code for the front-end of FCG interactive.

## Installation and Loading

In order to launch the FCG interactive web application, the following steps need to be undertaken.

1.	The folders fcg-server and fcg-interactive should be moved to Babel’s systems/folder. Note: if you have a Babel installation (https://gitlab.ai.vub.ac.be/ehai/babel/) from later than March 2022, fcg-server and fcg-interactive are already part of your installation and you can skip this step.
2.	The path in the ﬁle babel-core/systems/fcg-interactive/run.sh should be adapted according to the place of your Babel installation.
3.	If necessary, adapt the path to your ccl implementation or (preferably) make a symlink so that it is accessible via the ccl command.
4.	Open a terminal, navigate to the fcg-interactive folder, enter bash run.sh and wait for about 10 seconds until the web service is active.
5.	Open index.html in the www folder, choose a grammar, and have fun.

The code was designed for compatibility with the version of babel-core (https: //gitlab.ai.vub.ac.be/ehai/babel-core – commit af7338892d4e8b22f89c19bf447a272243d6b40e and has been tested using Clozure Com-mon Lisp version 1.12.

## Adding a Grammar to FCG Interactive

In order to integrate a new grammar into FCG Interactive, take the following steps:

1.	Open the ﬁle systems/fcg-interactive/grammars.lisp and add a call (e.g. a quickload call) that loads the new grammar. Make sure the diﬀerent grammars are accessible through separate global variables.
2.	In the ﬁle systems/fcg-interactive/examples.lisp , add example utterances and meaning representations to the alists for *example-utterances* and *example-meanings*. This list should be of the form (package::*grammar* . (”u-1” ”u-2” ”u-3”)). These examples will show up in the front-end.
3.	In the ﬁle www/index.html, add an option to the grammarComboBox for your new grammar (e.g. <option value=”dutch”¿Dutch¡/option>)
4.	In the ﬁle www/fcg interactive.js, add a case to the grammarChosen() func-tion (e.g. case ”dutch”: chosenGrammar = ”*dutch-vp-grammar*”; package = ”dutch-vp”; break;).
5.	If you now open the www/index.html ﬁle in a web browser, you will be able to use your new grammar right away.

## Hosting

In order to host FCG interactive, we recommend to conﬁgure an Apache server (or similar) to access the web service through a reverse proxy pointing to the localhost. The FCG server and related services were not designed to be exposed to the web directly. Also, you might want to restrict CORS headers, etc., in the fcg-server/fcg-server.lisp ﬁle (and follow other good practices in web deployment).

## FCG Server System

The FCG server system (folder fcg-server) can also be used independently from FCG interactive to build web services around FCG-powered systems. Out-of-the-box, it comes with two endpoints: comprehend-utterance and produce-utterance. These endpoints accept POST requests with json-encoded data (utterance/meaning,grammar, package and timeout). Additional routes can be added following the examples in routes.lisp. A manual ﬁle is included as demo.lisp.
