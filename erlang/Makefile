DEFINES = -DNON_MATLAB_PARSING -DMAX_EXT_API_CONNECTIONS=255
INCLUDES = -I/usr/lib/erlang/erts-5.10.3/include -I../remoteApi -I../include
CFLAGS = -fPIC -shared -Wall -Wextra -Wno-unused-parameter

all: lib

lib: simx.beam simxerl.so

simxerl.so: simxerl.c ../remoteApi/extApi.c ../remoteApi/extApiPlatform.c
	gcc $(CFLAGS) $(INCLUDES) $(DEFINES) -o $@ $^

simx.beam: simx.erl
	erlc $^

.PHONY: clean

clean:
	rm *.o
	rm *.beam
	rm *.so
