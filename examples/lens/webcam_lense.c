#include <stdio.h>

/* opencv libs */
#include <cv.h>
#include <highgui.h>

/* c libs for erlang */
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h> 

/* erlang libs */
#include <erl_interface.h>
#include <ei.h>

int main( int argc, char **argv ) {

	/* ***** *** * OPENCV DEFINES * *** ***** */ 
	CvCapture *capture;
	IplImage  *img;
	int key = 0;

	/* ***** *** * ERLANG DEFINES * *** ***** */ 
	char *self_addr;
	char *self_hostname;
	char *self_nodename;
	char *cookie;
	char *self_fullname;
	char *conn_addr;
	char *conn_nodename;

	struct in_addr self_iaddr;	/* 32-bit IP number of host */
	struct in_addr conn_iaddr;	/* 32-bit IP number of host */ 

	int fd;	                                 /* file descriptor of Erlang node */
	int loop = 1;	                         /* Loop flag */
	int got;                                 /* Result of receive */
	unsigned char buf[BUFSIZ];               /* Buffer for incoming message */
	ErlMessage emsg;                         /* Incoming message */
	ETERM *pid, *msg;
	int family;

	ETERM **families;

	/* ***** *** * GENERAL DEFINES * *** ***** */
	int x,y;

	#define SELF_ADDR argv[1]
	#define SELF_HOSTNAME argv[2]
	#define SELF_NODENAME argv[3]
	#define COOKIE argv[4]
	#define CONN_ADDR argv[5]
	#define CONN_NODENAME argv[6]
	
	/* FIXME this should really use getopt */
	if ( argc != 7 ) {
		fprintf(stderr, "Usage %s: self_addr self_hostname self_nodename cookie conn_addr conn_nodename", argv[0]);
		return 1;
	}

	/* ***** *** * OPENCV SETUP * *** ***** */ 

	/* initialize camera */
	capture = cvCaptureFromCAM( 0 );

	/* always check */
	if ( !capture ) {
		fprintf(stderr, "Cannot initialize webcam!\n" );
		return 1;
	}

	/* create a window for the video */
	cvNamedWindow( "result", CV_WINDOW_AUTOSIZE );


	/* check image for 3 channels */
	if( !(img = cvQueryFrame( capture )) || img->nChannels != 3) {
		fprintf(stderr, "Expected 3 channel camera");
		return 1;
	}

	/* ***** *** * ERLANG SETUP * *** ***** */ 

	/* Configure cnode (self) */
	strcpy(self_addr = malloc(strlen(SELF_ADDR)+1), SELF_ADDR);
	strcpy(self_hostname = malloc(strlen(SELF_HOSTNAME)+1), SELF_HOSTNAME);
	strcpy(self_nodename = malloc(strlen(SELF_NODENAME)+1), SELF_NODENAME);
	strcpy(cookie = malloc(strlen(COOKIE)+1), COOKIE);

	self_fullname = malloc(strlen(self_nodename)+strlen(self_addr)+2);
	strcat(strcat(strcpy(self_fullname, self_nodename), "@"), self_addr);

	/* Configure erlang connection */
	strcpy(conn_addr = malloc(strlen(CONN_ADDR)+1), CONN_ADDR);
	strcpy(conn_nodename = malloc(strlen(CONN_NODENAME)+1), CONN_NODENAME);

	/* initialize erl_interface (once only) */
	erl_init(NULL, 0);  

	/* initialize the connection mechanism */
	self_iaddr.s_addr = inet_addr( self_addr );
	conn_iaddr.s_addr = inet_addr( conn_addr );
	if (erl_connect_xinit(self_hostname, self_nodename, self_fullname, &self_iaddr, cookie, 0) == -1)
		erl_err_quit("erl_connect_init");

	/* connect to a running Erlang node */
	if ((fd = erl_xconnect( &conn_iaddr, conn_nodename )) < 0)
		erl_err_quit("erl_connect");

	/* ***** *** * GRAB FAMILY LISTS * *** ***** */
	/* TODO construct message */
		
	/* XXX send message to get process data */
	msg = erl_format((char*)"{p_query, {any, ~s}}]", self_fullname);
	erl_reg_send(fd, "father", msg);

	/* XXX receive messages to get process data */
	/* XXX populate 6 by n array of eterm pointers */
	/*
	while (loop) {
		got = erl_receive_msg(fd, buf, BUFSIZ, &emsg);
		if (got == ERL_TICK) {
		} else if (got == ERL_ERROR) {
			loop = 0;
		} else {
			if (emsg.type == ERL_REG_SEND) {
				// message will be 2 tuple {F, Pid}
				family = erl_element(1, emsg.msg);
				pid = erl_element(2, emsg.msg);
				// erl_free_term(family);
			}
			erl_free_term(emsg.msg);
		}
	}
	*/

	/* ***** *** *	LOOP	* *** ***** */ 
	loop = 1;
	while ( loop && key != 'q' ) {
		/* get an image */
		if( !(img = cvQueryFrame(capture)) ) 
			break;
		/* display current image */
		cvShowImage("result", img);

		/* TODO send image to erlang */
		/* TODO send red pixels to corr pids */
		/*
		for(x = 0; x < img->width; x++) {
			for(y = 0; y < img->width; y++) {
				((uchar*)(img->imageData + img->widthStep*y))[x*3]; 
				((uchar*)(img->imageData + img->widthStep*y))[x*3+1];
				((uchar*)(img->imageData + img->widthStep*y))[x*3+2];
			}
		}
		*/
		/* XXX int erl_send(fd, to, msg) */
		/* XXX 
		args = erl_format((char*)"[data_man, {~f, ~f, ~f, ~f}]", ch.at(0), ch.at(1), ch.at(2), ch.at(3));
		erl_rpc_to(fd, (char*)"gen_event", (char*)"notify", args);
		*/

		/* exit if user presses 'q' */
		key = cvWaitKey( 1 );
	}

	/* free memory */
	cvDestroyWindow( "result" );
	cvReleaseCapture( &capture );

	return 0;
}
