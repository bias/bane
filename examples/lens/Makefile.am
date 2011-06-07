## Non GNU complient  project
AUTOMAKE_OPTIONS = foreign

#INCLUDES = -I../../common

bin_PROGRAMS = webcam
webcam_SOURCES = webcam.c
webcam_LDADD = $(OPENCV_LIBS)

AM_CPPFLAGS = $(OPENCV_CFLAGS) #-ansi -pedantic -Wall -Wextra -Wformat=2 -Wfloat-equal -Wundef -Wshadow -Wcast-qual -Wwrite-strings -Wbad-function-cast -Wmissing-declarations 
#AM_CXXFLAGS = $(OPENCV_CFLAGS)
