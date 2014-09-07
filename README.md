
# erlyberly

erlyberly is a clone of the console program [entop](https://github.com/mazenharake/entop), rewritten in java/javafx.  

![you cannot see the beautiful screen shot](doc/erlyberly.png)

### Current State

erlyberly currently does not contain all functionality that entop has so isn't a proper replacement.  It is the basis of a development console which I hope will eventually be a UI over the erlang dbg module to run and view traces on a running erlang node.

### Compiling

You will need JDK 8 and Maven to compile erlyberly and JDK/Java 8 to run it.

erlyberly loads an erlang module to the remote node and then uses RPC to call functions on it to retrieve data.  For convenience I have bundled the beam code in the source as well as the original erlang source.  If you want to recompile the beam code for yourself run the following command from the project directory:

    erlc -o src/main/resources/erlyberly/beam/ src/main/resources/erlyberly/beam/erlyberly.erl

To build the jar:

    mvn install