## Non GNU complient  project
AUTOMAKE_OPTIONS = foreign

bin_PROGRAMS = webcam_lense visualize create

webcam_lense_SOURCES = webcam_lense.c
webcam_lense_LDADD = $(OPENCV_LIBS) -L$(ERLANG_LIB_DIR_erl_interface)/lib -lei -lerl_interface

visualize_SOURCES = visualize.c
visualize_LDADD = $(OPENCV_LIBS) -L$(ERLANG_LIB_DIR_erl_interface)/lib -lei -lerl_interface

create_SOURCES = create.c
create_LDADD = $(OPENCV_LIBS)

INCLUDES = -I$(ERLANG_LIB_DIR_erl_interface)/include

NORMAL_FLAGS = -Wall -Wextra 
SUPER_FLAGS = -Wformat=2 -Wfloat-equal -Wundef -Wshadow -Wcast-qual -Wwrite-strings -Wbad-function-cast -Wmissing-declarations 
UNANGRY_FLAGS = -pendatic  $(NORMAL_FLAGS) $(SUPER_FLAGS)
ALL_FLAGS = -ansi -pendatic  $(NORMAL_FLAGS) $(SUPER_FLAGS)

AM_CPPFLAGS = $(OPENCV_CFLAGS) $(ERLANG_CLFLAGS) $(UNANGRY_FLAGS) 
