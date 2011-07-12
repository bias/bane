#include <stdio.h>

/* opencv libs */
#include <cv.h>
#include <highgui.h>



int main(int argc, char **argv) {
	int i, key;
	int nFrames;

	if ( argc != 3 )
		return 1;
	else {
		nFrames = atoi(argv[1]);
	}

	IplImage  *img;
	CvVideoWriter *writer;

	img = cvCreateImage(cvSize(100, 100), IPL_DEPTH_8U, 3);
	writer = cvCreateVideoWriter(argv[2], CV_FOURCC('P','I','M','1'), 25 /*fps*/, cvGetSize(img), 1);

	cvSet(img, CV_RGB(255, 255, 255), 0);

	CvPoint  curve1[] = {50,0,  100,0,  100,100,  50,100};
	CvPoint* curveArr[1] = {curve1};
	int nCurvePts[1] = {4};
	int nCurves = 1;
	cvFillPoly(img, curveArr, nCurvePts, nCurves, cvScalar(0,0,0,0), 8, 0);

	for (i=0; i<nFrames; i++) {
		cvShowImage(argv[0], img); 
		cvWriteFrame(writer, img);
		key = cvWaitKey(20);
	}

	cvReleaseVideoWriter( &writer );
	cvDestroyWindow( argv[0] );
}
