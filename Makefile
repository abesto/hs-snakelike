
HC = ghc
SDL_OPTIONS = `sdl-config --cflags --libs | sed -e 's/-Wl,\(\w*\)/-optl\1/' | tr ',' '-'`

EXECUTABLE = snake
SRCS = Model.hs Display.hs SdlMain.hs
OBJS = $(SRCS:.hs=.o)

all: ${EXECUTABLE}

${EXECUTABLE}: ${OBJS} c_main.o
		${HC} --make -no-hs-main c_main.o $(OBJS:.o=) -o ${EXECUTABLE}

c_main.o: c_main.c
		${HC} -c -optc-O c_main.c ${SDL_OPTIONS}

%.o: %.hs
		${HC} -c -O $<

.PHONY: clean

clean:
		rm *.o *.hi ${EXECUTABLE} SdlMain_stub.h 

