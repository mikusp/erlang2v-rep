erlang2v-rep
============

Erlang interface to V-Rep.

How to build
============

Copy "erlang" directory to V-Rep_Dir/programming/ and run make.

How to use
==========

First of all, enable remote API in V-Rep by uncommenting relevant lines in
remoteApiConnections.txt file in V-Rep directory.

Example config should look like:

    portIndex1_port = 19999
    portindex1_debug = true
    portindex1_syncSimTrigger = false

Then run V-Rep, run erl and type

    > c(simx).
    {ok,simx}

If everything went right, it will return {ok,simx} and you can try to connect
to V-Rep instance using simx:start/5 function. For example:

    > simx:start("127.0.0.1", 19999, 1, 1, 5000, 5).
    0

This function returns a clientID used in many other functions as a parameter.

Documentation
=============

You'll have to consult source code and V-Rep remote API documentation for now. A couple of rules I tried to stick to
while writing this API:

* operation mode is an atom with name corresponding to C constant, but stripped
from namespaces (e.g. atom oneshot represents simx_opmode_oneshot),
* identifiers of boolean, integer, float parameters and button, model properties follow the same rule as
operation mode,
* if these properties can be ORed, you can specify a single property or a list of them,
* if a function takes a fixed-length array of values in C, in Erlang it takes a tuple,
* if a function takes a variable length array of values in C, in Erlang it takes a list,
* all functions will throw badarg exception if a type of parameter is different from expected,
* if a function returns a remote API error code and something else, it will return a tuple of
error code and a proper return value (or values),
* adding arbitrary numeric values to operation mode is not supported.

Parameter naming convention
===========================

* NamexxT - a tuple of xx arity,
* NameL - a list.
