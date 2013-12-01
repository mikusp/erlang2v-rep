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

Documentation can be generated using

    > edoc:files(["erlang/simx.erl"],[]).

Consult V-Rep remote API documentation for explanation of what each function does and values of
certain parameters (e.g. ParameterID in simx:getObjectFloatParameter/4).

