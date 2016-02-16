.c.o:
	$(CC) $(CFLAGS) -c $<

OBJECTS = $(SOURCES_CPP:.cpp=.o) $(SOURCES_C:.c=.o) $(SOURCES_S:.S=.o)

all:$(TARGETS) $(TARGET)

clean:
	rm *.o $(TARGETS) $(TARGET) $(CLEAN_OBJECTS)
