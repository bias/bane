#include <stdio.h>

/* opencv libs */
#include <cv.h>
#include <highgui.h>



int main(int argc, char **argv) {
	int i, key, right, left;
	int nFrames;

	if ( argc != 5 )
		return 1; // rate left right name
	else {
		nFrames = atoi(argv[1]);
		left = atoi(argv[2]);
		right = atoi(argv[3]);
	}

	IplImage  *img;
	CvVideoWriter *writer;

	img = cvCreateImage(cvSize(100, 100), IPL_DEPTH_8U, 3);
	writer = cvCreateVideoWriter(argv[4], CV_FOURCC('P','I','M','1'), 20 /*fps*/, cvGetSize(img), 1);

	cvSet(img, CV_RGB(left, left, left), 0);

	CvPoint  curve1[] = {50,0,  100,0,  100,100,  50,100};
	CvPoint* curveArr[1] = {curve1};
	int nCurvePts[1] = {4};
	int nCurves = 1;
	cvFillPoly(img, curveArr, nCurvePts, nCurves, cvScalar(right,right,right,0), 8, 0);

	for (i=0; i<nFrames; i++) {
		cvWriteFrame(writer, img);
	}

	cvReleaseVideoWriter( &writer );
	cvDestroyWindow( argv[0] );
}
