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


#define PI 3.1415926535

typedef struct Hex {
	double x;
	double y;
	double ang;
	double step;
} Hex;

int spiral_hex_map(CvPoint *[], int, Hex *, int count);
uchar pixel_map(IplImage *, CvPoint *, int);

int main(int argc, char **argv) {

	/* ***** *** * OPENCV DEFINES * *** ***** */ 
	CvCapture *capture;
	IplImage  *img;
	int key = 0;
	Hex hex;
	CvPoint center;


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
	ETERM *erequest, *earg, *ehead, *egraph_order, *epid, *efam, *egen;
	ETERM **families[6], *mother, *photons[256]; 
	int graph_order = 0;


	/* ***** *** * GENERAL DEFINES * *** ***** */
	int i,j;

	#define SELF_ADDR argv[1]
	#define SELF_HOSTNAME argv[2]
	#define SELF_NODENAME argv[3]
	#define COOKIE argv[4]
	#define CONN_ADDR argv[5]
	#define CONN_NODENAME argv[6]
	
	/* FIXME this should really use getopt */
	if ( argc != 7 && argc != 8 ) {
		fprintf(stderr, "Usage %s: self_addr self_hostname self_nodename cookie conn_addr conn_nodename [avi input]", argv[0]);
		return 1;
	}

	/* ***** *** * OPENCV SETUP * *** ***** */ 

	fprintf(stderr, "argc = %i\n", argc);

	/* initialize camera */
	if ( argc == 7 ) {
		fprintf(stderr, "using camera\n");
		capture = cvCaptureFromCAM( 0 );
	}
	else {
		fprintf(stderr, "using file %s\n", argv[7]);
		capture = cvCaptureFromAVI( argv[7] );
	}

	/* always check */
	if ( !capture ) {
		fprintf(stderr, "Cannot initialize input!\n" );
		return 1;
	}

	/* create a window for the video */
	cvNamedWindow( argv[0], CV_WINDOW_AUTOSIZE );


	/* check image for 3 channels, and get dimensions */
	if( !(img = cvQueryFrame( capture )) ) {
		fprintf(stderr, "Expected 3 channel camera");
		return 1;
	}
	else if ( img->nChannels != 3 ) {
		fprintf(stderr, "Expected 3 channel camera");
		return 1;
	} 
	else {
		fprintf(stdout, "image h,w: %i, %i\n", img->height, img->width);
		center.x = round(img->width/2);
		center.y = round(img->height/2);
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

	for (i=0; i<256; i++)
		photons[i] = erl_format((char*)"{{pho,~i}, 0}", (uchar)i);

	/* ***** *** * GRAB FAMILY LISTS * *** ***** */
		
	erequest= erl_format((char*)"{p_q, {any, ~a}}", self_fullname);
	erl_reg_send(fd, "father", erequest);
	erl_free_term(erequest);

	while (loop) {
		got = erl_receive_msg(fd, buf, BUFSIZ, &emsg);
		if (got == ERL_TICK) { /* nothing */ } 
		else if (got == ERL_ERROR) { return 1; } 
		else {
			if (emsg.type == ERL_REG_SEND) {
				ehead = erl_element(1, emsg.msg);
				if ( strcmp(ERL_ATOM_PTR(ehead), "p_r_start") == 0 ) {
					/* {p_r_start, Graph_order} */ 
					egraph_order = erl_element(2, emsg.msg);
					graph_order = ERL_INT_VALUE(egraph_order);
					for (i=0; i<6; i++)
						families[i] = malloc( (graph_order-1)/6 * sizeof(ETERM*) );
					erl_free_term(egraph_order);
				}
				if ( strcmp(ERL_ATOM_PTR(ehead), "p_r") == 0 ) {
					/* {p_r, {Pid, Fam, Gen}} */
					earg = erl_element(2, emsg.msg);
					efam = erl_element(2, earg);
					egen = erl_element(3, earg);
					if ( ERL_INT_VALUE(efam) == 0 )
						mother = erl_element(1, earg); 
					else
						families[ERL_INT_VALUE(efam)-1][ERL_INT_VALUE(egen)-1] = erl_element(1, earg);
				}
				if ( strcmp(ERL_ATOM_PTR(ehead), "p_r_stop") == 0 ) { loop = 0; }
			}
			erl_free_term(emsg.msg);
			erl_free_term(ehead);
			erl_free_term(earg);
			erl_free_term(efam);
			erl_free_term(egen);
		}
	}


	/* XXX create map from 6 by n array to pixels (square and hexagon map) */
	int family_order = (graph_order-1)/6;
	CvPoint **map[6];
	for (i=0; i<6; i++) {
		map[i] = malloc( family_order * sizeof(CvPoint*) );
		for (j=0; j<family_order; j++)
			map[i][j] = malloc( sizeof(struct CvPoint) ); 
	}

	hex.step = 1;
	for (i=0; i<6; i++) {
		hex.ang = i*PI/3;
		hex.x = center.x + cos(hex.ang)*hex.step;
		map[i][0]->x = (int) (hex.x+0.5);
		hex.y = center.y + sin(hex.ang)*hex.step;
		map[i][0]->y = (int) (hex.y+0.5);
		hex.ang += PI/3;
		spiral_hex_map(map[i], family_order-1, &hex, 1);
	}


	/* ***** *** *	LOOP	* *** ***** */ 

	fprintf(stderr, "ready\n");
	loop = 1;

	while ( loop  && key != 'q' ) {

		/* TODO calculate framerate */
		
		/* get an image */
		if( !(img = cvQueryFrame(capture)) ) 
			break;

		/* display current image */
		cvShowImage(argv[0], img);
		
		/* center of retina */
		erl_send(fd, mother, photons[pixel_map(img, &center, 1)]);
		for (i=0; i<6; i++) 
			for (j=0; j<family_order; j++)
				erl_send(fd, families[i][j], photons[pixel_map(img, map[i][j], 1)]);

		/* exit if user presses 'q' */
		key = cvWaitKey( 1 );
	}

	/* free memory */
	cvDestroyWindow( argv[0] );
	cvReleaseCapture( &capture );

	return 0;
}


int spiral_hex_map(CvPoint *map[], int size, Hex *hex, int count) {
	int i;
	for (i=0; i<count; i++) { 
		hex->x += cos(hex->ang)*hex->step;
		map[i]->x = (int) (hex->x+0.5);
		hex->y += sin(hex->ang)*hex->step;
		map[i]->y = (int) (hex->y+0.5);
	}
	hex->ang += PI/3;
	if ( size-count > 0 )
		spiral_hex_map(&map[count], size-count, hex, count+1);
	else
		return 0;	
}

/* FIXME XXX oh no!!! we are going to average the channels for now XXX ignoring int channel !!! */
uchar pixel_map(IplImage *img, CvPoint *p, int channel) {
	/* XXX widthStep : size of an aligned image row, in bytes */
	/* XXX nChannels : channel size to offset in bytes */
	return (uchar) ( ((uchar*)(img->imageData + p->y*img->widthStep))[p->x*img->nChannels] + 
		     ((uchar*)(img->imageData + p->y*img->widthStep))[p->x*img->nChannels+1] + 
			 ((uchar*)(img->imageData + p->y*img->widthStep))[p->x*img->nChannels+2] ) / 3;
}
