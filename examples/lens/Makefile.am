## Non GNU complient  project
AUTOMAKE_OPTIONS = foreign

bin_PROGRAMS = webcam
webcam_SOURCES = webcam.c
webcam_LDADD = $(OPENCV_LIBS) -L$(ERLANG_LIB_DIR_erl_interface)/lib -lei -lerl_interface

INCLUDES = -I$(ERLANG_LIB_DIR_erl_interface)/include

AM_CPPFLAGS = $(OPENCV_CFLAGS) $(ERLANG_CLFLAGS) #-ansi -pedantic -Wall -Wextra -Wformat=2 -Wfloat-equal -Wundef -Wshadow -Wcast-qual -Wwrite-strings -Wbad-function-cast -Wmissing-declarations 
