/*
 * Copyright (C) 2009-2011 Freescale Semiconductor, Inc.  All Rights Reserved.
 * 
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation 
 * files (the "Software"), to deal in the Software without 
 * restriction, including without limitation the rights to use, copy, 
 * modify, merge, publish, distribute, sublicense, and/or sell copies 
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS 
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN 
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN 
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
 * SOFTWARE.
 */
#include <xorg-server.h>

#include "xf86.h"
#include "xf86_OSproc.h"
#include "exa.h"
#include "imx.h"
#include "imx_accel.h"
#include "imx_exa.h"
#include "z160.h"
#include <errno.h>
#include <fcntl.h>

#include "compat-api.h"

/* Set if handles pixmap allocation and migration, i.e, EXA_HANDLES_PIXMAPS */
#define	IMX_EXA_ENABLE_HANDLES_PIXMAPS	\
		(1 && (IMX_EXA_VERSION_COMPILED >= IMX_EXA_VERSION(2,5,0)))

/* Name for semaphore to prevent another X driver to use z160 for same fb */
/* For diff processes to recognize same semaphore, must have / at name begin. */
#define	IMX_EXA_SEM_NAME		"/x-imx-z160"

/* Set minimum size (pixel area) for accelerating operations. */
#define	IMX_EXA_MIN_PIXEL_AREA_SOLID		150
#define	IMX_EXA_MIN_PIXEL_AREA_COPY		150
#define	IMX_EXA_MIN_PIXEL_AREA_COMPOSITE	150

/* This flag must be enabled to perform any debug logging */
#define IMX_EXA_DEBUG_MASTER		0

#define IMX_EXA_DEBUG_INSTRUMENT_SIZES	(0 && IMX_EXA_DEBUG_MASTER)
#define	IMX_EXA_DEBUG_PREPARE_SOLID	(0 && IMX_EXA_DEBUG_MASTER)
#define	IMX_EXA_DEBUG_SOLID		(0 && IMX_EXA_DEBUG_MASTER)
#define	IMX_EXA_DEBUG_PREPARE_COPY	(0 && IMX_EXA_DEBUG_MASTER)
#define	IMX_EXA_DEBUG_COPY		(0 && IMX_EXA_DEBUG_MASTER)
#define	IMX_EXA_DEBUG_CHECK_COMPOSITE	(0 && IMX_EXA_DEBUG_MASTER)


/* Z160 uses Z430's 32 pixel horizontal and vertical pixel alignment */
#define IMX_EXA_Z160_ALIGN(offset)	IMX_ALIGN((offset), 32)


#if IMX_EXA_DEBUG_MASTER
#include <stdio.h>
#include <stdlib.h>
#include <syslog.h>
#include <unistd.h>
#endif


#if IMX_EXA_ENABLE_HANDLES_PIXMAPS

/* This is pixmap private data for the EXA driver to use. */

struct _ImxExaPixmapRec;
typedef struct _ImxExaPixmapRec ImxExaPixmapRec;
typedef struct _ImxExaPixmapRec* ImxExaPixmapPtr;

struct _ImxExaPixmapRec {

	/* Properties for pixmap header passed in CreatePixmap2 or */
	/* in ModifyPixmapHeader callbacks. */
	int			width;
	int			height;
	int			depth;
	int			bitsPerPixel;

	/* Common properties for allocated pixmap regardless where stored. */
	int			pitchBytes;	/* bytes per row */
	void*			ptr;		/* ptr to system/virtual addr */
	Bool			canAccel;	/* true if in GPU memory */
	void*			*gpuAddr;	/* physical GPU addr if accel */

	/* Properties for pixmap allocated from GPU FB (offscreen) memory */
	int			widthAligned;	/* aligned to 32 pixel horz */
	int			heightAligned;	/* aligned to 32 pixel vert */
	ExaOffscreenArea	*area;		/* ptr to GPU FB memory alloc */

	/* Properties for pixmap allocated from system memory. */
	int			sysAllocSize;	/* size of sys memory alloc */
	void*			sysPtr;		/* ptr to sys memory alloc */

	/* Tracks pixmaps busy with GPU operation since last GPU sync. */
	Bool			gpuBusy;	/* pixmap busy in GPU op? */
	ImxExaPixmapPtr		pNextBusyPixmap;/* next pixmap in link list */

};


static ImxExaPixmapPtr
imxExaZ160GetPixmapDriverPrivate(PixmapPtr pPixmap)
{
	if (NULL == pPixmap) {
		return NULL;
	}

	return (ImxExaPixmapPtr)(exaGetPixmapDriverPrivate(pPixmap));
}

#endif


/* This is private data for the EXA driver to use */

typedef struct {

	/* This must be the first entry in this data structure */
	/* because code such as in imx_exa_offscreen.c will */
	/* reference this structure just using ImxPtr. */
	ImxExaRec			imxExaRec;

	int				fdMutex;

	CloseScreenProcPtr		saveCloseScreenPtr;
	int				scrnIndex;

	int				maxWidth;
	int				maxHeight;

	void*				gpuContext;
	Bool				gpuIdle;

	/* Parameters passed into PrepareSolid */
	int				solidALU;
	Pixel				solidPlaneMask;
	Pixel				solidColor;

	/* Parameters passed into PrepareCopy */
	int				copyALU;
	Pixel				copyPlaneMask;
	int				copyDirX;
	int				copyDirY;

	/* Pixmap and Z160-derived parameters passed into Prepare{Solid,Copy,Composite} */
	PixmapPtr			pPixmapDst;
	PixmapPtr			pPixmapSrc;
	Z160Buffer			z160BufferDst;	
	Z160Buffer			z160BufferSrc;	
	unsigned long			z160Color;

	/* Flag set if GPU has been setup for solid, copy, or */
	/* composite operation. */
	Bool				gpuOpSetup;

#if IMX_EXA_ENABLE_HANDLES_PIXMAPS

	/* Pointer to pixmaps busy with GPU operation since last sync. */
	ImxExaPixmapPtr			pBusyPixmapLinkList;

#endif

#if IMX_EXA_DEBUG_INSTRUMENT_SIZES
	unsigned long			numSolidFillRect100;
	unsigned long			numSolidFillRect1000;
	unsigned long			numSolidFillRect10000;
	unsigned long			numSolidFillRect100000;
	unsigned long			numSolidFillRectLarge;
	unsigned long			numScreenCopyRect100;
	unsigned long			numScreenCopyRect1000;
	unsigned long			numScreenCopyRect10000;
	unsigned long			numScreenCopyRect100000;
	unsigned long			numScreenCopyRectLarge;
#endif

} ImxExaZ160Rec, *ImxExaZ160Ptr;

#define IMXEXAZ160PTR(imxPtr) ((ImxExaZ160Ptr)((imxPtr)->exaDriverPrivate))



/* Definitions for functions defined in imx_exa_offscreen.c */
extern Bool
imxExaOffscreenInit(ScreenPtr pScreen);

extern ExaOffscreenArea*
imxExaOffscreenAlloc(ScreenPtr pScreen, int size, int align, Bool locked,
			ExaOffscreenSaveProc save, pointer privData);

extern ExaOffscreenArea*
imxExaOffscreenFree(ScreenPtr pScreen, ExaOffscreenArea* area);

extern void
imxExaOffscreenFini(ScreenPtr pScreen);



static
PixmapPtr
imxExaZ160GetDrawablePixmap(DrawablePtr pDrawable)
{
	/* Make sure there is a drawable. */
	if (NULL == pDrawable) {
		return NULL;
	}

	/* Check for a backing pixmap. */
	if (DRAWABLE_WINDOW == pDrawable->type) {

		WindowPtr pWindow = (WindowPtr)pDrawable;
		return pDrawable->pScreen->GetWindowPixmap(pWindow);
	}

	/* Otherwise, it's a regular pixmap. */
	return (PixmapPtr)pDrawable;
}

static PixmapPtr
imxExaZ160GetPicturePixmap(PicturePtr pPicture)
{
	if (NULL != pPicture) {

		return imxExaZ160GetDrawablePixmap(pPicture->pDrawable);
	}

	return NULL;
}


#if IMX_DEBUG_MASTER

static unsigned long
imxExaZ160GetElapsedMicroseconds(struct timeval* pTimeStart,
					struct timeval* pTimeStop)
{
	/* If either time is missing, then return 0. */
	if ((NULL == pTimeStart) || (NULL == pTimeStop)) {
		return 0;
	}

	/* Start time after stop time (looking at seconds field only). */
	if (pTimeStart->tv_sec > pTimeStop->tv_sec) {

		return 0;

	/* Start time and stop time have same seconds field. */
	} else if (pTimeStart->tv_sec == pTimeStop->tv_sec) {

		/* Start time after stop time (looking at usec field only). */
		if (pTimeStart->tv_usec > pTimeStop->tv_usec) {

			return 0;

		} else {

			return pTimeStop->tv_usec - pTimeStart->tv_usec;
		}

	/* Start time is before stop time, but the seconds are different. */
	} else {

		unsigned long elapsedMicroseconds =
			(pTimeStop->tv_sec - pTimeStart->tv_sec) * 1000000U;

		elapsedMicroseconds += 
			(pTimeStop->tv_usec - pTimeStart->tv_usec);

		return elapsedMicroseconds;
	}
}
#endif

static
void*
imxExaZ160GetPixmapAddress(PixmapPtr pPixmap)
{
#if IMX_EXA_ENABLE_HANDLES_PIXMAPS
	/* Make sure pixmap is defined. */
	if (NULL == pPixmap) {
		return NULL;
	}

	/* Access driver private data structure associated with pixmap. */
	ImxExaPixmapPtr fPixmapPtr = imxExaZ160GetPixmapDriverPrivate(pPixmap);
	if (NULL == fPixmapPtr) {
		return NULL;
	}

	return fPixmapPtr->ptr;
#else
	/* Access screen associated with this pixmap. */
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pPixmap->drawable.pScreen);

	/* Access driver specific data */
	ImxPtr imxPtr = IMXPTR(pScrn);

	/* Compute the virtual address using relative offset. */
	return (unsigned char*)imxPtr->fbMemoryStart + exaGetPixmapOffset(pPixmap);
#endif
}

Bool
imxExaZ160GetPixmapProperties(
	PixmapPtr pPixmap,
	void** pPhysAddr,
	int* pPitch)
{
	/* Initialize values to be returned. */
	*pPhysAddr = NULL;
	*pPitch = 0;

	/* Is there a pixmap? */
	if (NULL == pPixmap) {
		return FALSE;
	}

#if IMX_EXA_ENABLE_HANDLES_PIXMAPS

	/* Access driver private data structure associated with pixmap. */
	ImxExaPixmapPtr fPixmapPtr = imxExaZ160GetPixmapDriverPrivate(pPixmap);
	if (NULL == fPixmapPtr) {
		return FALSE;
	}

	/* Make sure pixmap is in GPU memory. */
	if (!fPixmapPtr->canAccel) {
		return FALSE;
	}

	/* Get the physical address of pixmap and its pitch */
	*pPhysAddr = fPixmapPtr->gpuAddr;
	*pPitch = fPixmapPtr->pitchBytes;

#else

	/* Access screen associated with this pixmap. */
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pPixmap->drawable.pScreen);

	/* Make sure pixmap is in framebuffer */
	if (!exaDrawableIsOffscreen(&(pPixmap->drawable))) {
		return FALSE;
	}

	/* Get the physical address of pixmap and its pitch */
	*pPhysAddr = (void*)((unsigned char*)pScrn->memPhysBase + exaGetPixmapOffset(pPixmap));
	*pPitch = exaGetPixmapPitch(pPixmap);

#endif

	return TRUE;
}

static
Bool
imxExaZ160IsDrawablePixelOnly(DrawablePtr pDrawable)
{
	if ((1 == pDrawable->width) && (1 == pDrawable->height)) {

		return TRUE;

	} else {

		return FALSE;
	}
}

static
Bool
imxExaimxExaZ160CanAcceleratePixmapRectangles(PixmapPtr pPixmap)
{
	/* Do not check pixmap size because EXA may want to call to */
	/* accelerate rectangles within a pixmap which is larger */
	/* than that allowed by the z160 size limits, as long as */
	/* those rectangles are within the z160 size limits bounds. */

	/* Pixmap must be defined */
	if (NULL == pPixmap) {
		return FALSE;
	}

#if IMX_EXA_ENABLE_HANDLES_PIXMAPS

	/* Access driver private data structure associated with pixmap. */
	ImxExaPixmapPtr fPixmapPtr = imxExaZ160GetPixmapDriverPrivate(pPixmap);
	if (NULL == fPixmapPtr) {
		return FALSE;
	}

	/* Make sure pixmap is in GPU memory. */
	if (!fPixmapPtr->canAccel) {
		return FALSE;
	}

	/* Pixmap pitch must be within z160 limits and must be aligned. */
	const unsigned pitchBytes = fPixmapPtr->pitchBytes;
	if ((pitchBytes > Z160_MAX_PITCH_BYTES) ||
		(0 != (pitchBytes & (Z160_ALIGN_PITCH-1)))) {

		return FALSE;
	}

	/* Pixmap must be offset aligned. */
	const void* gpuAddr = fPixmapPtr->gpuAddr;
	if (0 != ((int)gpuAddr & (Z160_ALIGN_OFFSET-1))) {
		return FALSE;
	}

#else

	/* Pixmap must be in frame buffer memory */
	if (!exaDrawableIsOffscreen(&(pPixmap->drawable))) {
		return FALSE;
	}

	/* Pixmap pitch must be within z160 limits and must be aligned. */
	unsigned pitchBytes = exaGetPixmapPitch(pPixmap);
	if ((pitchBytes > Z160_MAX_PITCH_BYTES) ||
		(0 != (pitchBytes & (Z160_ALIGN_PITCH-1)))) {

		return FALSE;
	}

	/* Pixmap must be offset aligned. */
	if (0 != (exaGetPixmapOffset(pPixmap) & (Z160_ALIGN_OFFSET-1))) {
		return FALSE;
	}

#endif

	/* If we get here, then operations on this pixmap can be accelerated. */
	return TRUE;
}

static
Bool
imxExaZ160CanAcceleratePixmap(PixmapPtr pPixmap)
{
	/* Pixmap must be defined */
	if (NULL == pPixmap) {
		return FALSE;
	}

	/* Pixmap size must be within z160 limits */
	if ((pPixmap->drawable.width > Z160_MAX_WIDTH) ||
		(pPixmap->drawable.height > Z160_MAX_HEIGHT)) {

		return FALSE;
	}

	/* Check rest of pixmap properties for acceleration. */
	return imxExaimxExaZ160CanAcceleratePixmapRectangles(pPixmap);
}

static Bool 
Z160GetPixmapConfig(PixmapPtr pPixmap, Z160Buffer* pBuffer)
{
	/* Is there a pixmap? */
	if (NULL == pPixmap) {
		return FALSE;
	}

	/* Is there a buffer to store the results? */
	if (NULL == pBuffer) {
		return FALSE;
	}

	/* Get frame buffer properties about the pixmap. */
	if (!imxExaZ160GetPixmapProperties(pPixmap, &pBuffer->base, &pBuffer->pitch)) {
		return FALSE;
	}

	/* Get other properties from the pixmap */
	pBuffer->width = pPixmap->drawable.width;
	pBuffer->height = pPixmap->drawable.height;
	pBuffer->bpp = pPixmap->drawable.bitsPerPixel;

	return TRUE;
}

static Bool 
imxExaZ160GetPictureConfig(ScrnInfoPtr pScrn, PicturePtr pPicture,
				Z160Buffer* pBuffer)
{
	/* Is there a picture? */
	if (NULL == pPicture) {
		return FALSE;
	}

	/* Is there a buffer to store the results? */
	if (NULL == pBuffer) {
		return FALSE;
	}

	/* Access the pixmap associated with this picture. */
	PixmapPtr pPixmap = imxExaZ160GetDrawablePixmap(pPicture->pDrawable);
	if (NULL == pPixmap) {
		return FALSE;
	}

	/* Setup information from pixmap */
	if (!Z160GetPixmapConfig(pPixmap, pBuffer)) {
		return FALSE;
	}

	/* Setup based on the picture format. */
	switch (pPicture->format) {

		default:
			return FALSE;

		/* Alpha */

		case PICT_a8:
			pBuffer->format = Z160_FORMAT_A8;
			pBuffer->swapRB = FALSE;
			pBuffer->opaque = FALSE;
			break;

		/* 8-bit */

		case PICT_g8:
			pBuffer->format = Z160_FORMAT_8;
			pBuffer->swapRB = FALSE;
			pBuffer->opaque = FALSE;
			break;

		/* 16-bit */

		case PICT_r5g6b5:
			pBuffer->format = Z160_FORMAT_0565;
			pBuffer->swapRB = FALSE;
			pBuffer->opaque = FALSE;
			break;

		case PICT_b5g6r5:
			pBuffer->format = Z160_FORMAT_0565;
			pBuffer->swapRB = TRUE;
			pBuffer->opaque = FALSE;
			break;

		case PICT_a4r4g4b4:
			pBuffer->format = Z160_FORMAT_4444;
			pBuffer->swapRB = FALSE;
			pBuffer->opaque = FALSE;
			break;

		case PICT_x4r4g4b4:
			pBuffer->format = Z160_FORMAT_4444;
			pBuffer->swapRB = FALSE;
			pBuffer->opaque = TRUE;
			break;

		case PICT_a4b4g4r4:
			pBuffer->format = Z160_FORMAT_4444;
			pBuffer->swapRB = TRUE;
			pBuffer->opaque = FALSE;
			break;

		case PICT_x4b4g4r4:
			pBuffer->format = Z160_FORMAT_4444;
			pBuffer->swapRB = TRUE;
			pBuffer->opaque = TRUE;
			break;

		case PICT_a1r5g5b5:
			pBuffer->format = Z160_FORMAT_1555;
			pBuffer->swapRB = FALSE;
			pBuffer->opaque = FALSE;
			break;

		case PICT_x1r5g5b5:
			pBuffer->format = Z160_FORMAT_1555;
			pBuffer->swapRB = FALSE;
			pBuffer->opaque = TRUE;
			break;

		case PICT_a1b5g5r5:
			pBuffer->format = Z160_FORMAT_1555;
			pBuffer->swapRB = TRUE;
			pBuffer->opaque = FALSE;
			break;

		case PICT_x1b5g5r5:
			pBuffer->format = Z160_FORMAT_1555;
			pBuffer->swapRB = TRUE;
			pBuffer->opaque = TRUE;
			break;

		/* 32-bit  - normal format is ARGB */

		case PICT_a8r8g8b8:
			pBuffer->format = Z160_FORMAT_8888;
			pBuffer->swapRB = FALSE;
			pBuffer->opaque = FALSE;
			break;

		case PICT_x8r8g8b8:
			pBuffer->format = Z160_FORMAT_8888;
			pBuffer->swapRB = FALSE;
			pBuffer->opaque = TRUE;
			break;

		case PICT_a8b8g8r8:
			pBuffer->format = Z160_FORMAT_8888;
			pBuffer->swapRB = TRUE;
			pBuffer->opaque = FALSE;
			break;

		case PICT_x8b8g8r8:
			pBuffer->format = Z160_FORMAT_8888;
			pBuffer->swapRB = TRUE;
			pBuffer->opaque = TRUE;
			break;
	}

	pBuffer->alpha4 = pPicture->componentAlpha ? TRUE : FALSE;

	return TRUE;
}

#if 0
static unsigned long
Z160ConvertScreenColor(ScrnInfoPtr pScrn, Pixel color)
{
	/* How many bits assigned to alpha channel. */
	unsigned bitsAlpha = pScrn->bitsPerPixel - pScrn->weight.red -
				pScrn->weight.green - pScrn->weight.blue;

	unsigned long gpuColor = 0x00000000;

	/* Assign the alpha channel. */
	if (0 == bitsAlpha) {
		gpuColor = 0xFF000000;
	} else {
		/* TODO */
	}

	/* Assign the red channel. */
	unsigned long red = (color & pScrn->mask.red) >> pScrn->offset.red;
	gpuColor = gpuColor | (red << (24 - pScrn->weight.red));

	/* Assign the green channel. */
	unsigned long green = (color & pScrn->mask.green) >> pScrn->offset.green;
	gpuColor = gpuColor | (green << (16 - pScrn->weight.green));

	/* Assign the blue channel. */
	unsigned long blue = (color & pScrn->mask.blue) >> pScrn->offset.blue;
	gpuColor = gpuColor | (blue << (8 - pScrn->weight.blue));

	return gpuColor;
}
#endif

static void
imxExaZ160ContextRelease(ImxExaZ160Ptr fPtr)
{
	/* Destroy the GPU context? */
	if ((NULL != fPtr) && (NULL != fPtr->gpuContext)) {

		z160_sync(fPtr->gpuContext);
		z160_disconnect(fPtr->gpuContext);
		fPtr->gpuContext = NULL;
	}
}

static void*
imxExaZ160ContextGet(ImxExaZ160Ptr fPtr)
{
	/* If no connection, attempt to establish it. */
	if (NULL == fPtr->gpuContext) {

		/* Get context to access the GPU. */
		fPtr->gpuContext = z160_connect();
		if (NULL == fPtr->gpuContext) {

			xf86DrvMsg(fPtr->scrnIndex, X_ERROR,
				"Unable to access Z160 GPU\n");
			imxExaZ160ContextRelease(fPtr);
			return NULL;
		}

		/* Other initialization. */
		fPtr->gpuIdle = FALSE;
		fPtr->gpuOpSetup = FALSE;
	}

	return fPtr->gpuContext;
}

static void
imxExaZ160WaitIdleGPU(ImxExaZ160Ptr fPtr)
{
	if (NULL == fPtr) {
		return;
	}

	/* If there is no GPU context, then no reason to sync. */
	/* Do not use imxExaZ160ContextGet because it will regain */
	/* access if we currently do not have it (because idle). */
	void* gpuContext = fPtr->gpuContext;
	if (NULL == gpuContext) {
		return;
	}
	
	/* Was there a GPU operation since the last sync? */
	if (!fPtr->gpuIdle) {

		/* Do the wait */
		z160_sync(gpuContext);

		/* Update state */
		fPtr->gpuIdle = TRUE;
	}
}

#if IMX_EXA_ENABLE_HANDLES_PIXMAPS

/* Functions for driver to handle pixmap allocation and tracking of those */
/* busy with GPU operations since last GPU sync. */

static void
imxExaZ160TrackBusyPixmap(ImxExaZ160Ptr fPtr, PixmapPtr pPixmap)
{
	/* Nothing to do if pointers are not valid. */
	if ((NULL == fPtr) || (NULL == pPixmap)) {
		return;
	}

	/* Access driver private data structure associated with pixmap. */
	ImxExaPixmapPtr fPixmapPtr = imxExaZ160GetPixmapDriverPrivate(pPixmap);
	if (NULL == fPixmapPtr) {
		return;
	}

	/* Nothing to do if pixmap is already busy with GPU operation. */
	if (fPixmapPtr->gpuBusy) {
		return;
	}

	/* Not busy, so mark as busy and put at head of list. */
	fPixmapPtr->gpuBusy = TRUE;
	fPixmapPtr->pNextBusyPixmap = fPtr->pBusyPixmapLinkList;
	fPtr->pBusyPixmapLinkList = fPixmapPtr;
}

static void
imxExaZ160UntrackBusyPrivatePixmap(ImxExaZ160Ptr fPtr,
					ImxExaPixmapPtr fPixmapPtr)
{
        /* Nothing to do if pointers are not valid. */
        if ((NULL == fPtr) || (NULL == fPixmapPtr)) {
                return;
        }

	/* Nothing to do if the pixmap is not marked as busy. */
	if (!fPixmapPtr->gpuBusy) {
		return;
	}

	/* Remove pixmap from front of linked list? */
	if (fPixmapPtr == fPtr->pBusyPixmapLinkList) {

		fPtr->pBusyPixmapLinkList = fPixmapPtr->pNextBusyPixmap;

	/* Otherwise, traverse link list looking for matching pixmap node. */
	} else {

		ImxExaPixmapPtr fNextPixmapPtr = fPtr->pBusyPixmapLinkList;
		while (NULL != fNextPixmapPtr) {

			/* Found match at next node? */
			if (fPixmapPtr == fNextPixmapPtr->pNextBusyPixmap) {

				/* Hook around matched node and stop search. */
				fNextPixmapPtr->pNextBusyPixmap =
					fPixmapPtr->pNextBusyPixmap;
				break;
			}

			fNextPixmapPtr = fNextPixmapPtr->pNextBusyPixmap;
		}
	}

	/* Mark the pixmap as no longer busy. */
	fPixmapPtr->gpuBusy = FALSE;
	fPixmapPtr->pNextBusyPixmap = NULL;
}

static void
imxExaZ160UntrackBusyPixmap(ImxExaZ160Ptr fPtr, PixmapPtr pPixmap)
{
        /* Nothing to do if pointers are not valid. */
        if ((NULL == fPtr) || (NULL == pPixmap)) {
                return;
        }

	/* Access driver private data structure associated with pixmap. */
	ImxExaPixmapPtr fPixmapPtr = imxExaZ160GetPixmapDriverPrivate(pPixmap);
	if (NULL == fPixmapPtr) {
		return;
	}

	imxExaZ160UntrackBusyPrivatePixmap(fPtr, fPixmapPtr);
}

static void
imxExaZ160UntrackAllBusyPixmaps(ImxExaZ160Ptr fPtr)
{
	/* Clear linked list of private pixmap data structures */
	/* associated with pixmaps busy with GPU operations. */
	ImxExaPixmapPtr fPixmapPtr = fPtr->pBusyPixmapLinkList;
	while (NULL != fPixmapPtr) {

		/* Note next pointer in list past current. */
		ImxExaPixmapPtr fNextPixmapPtr = fPixmapPtr->pNextBusyPixmap;

		/* Untrack the current pixmap in list */
		fPixmapPtr->gpuBusy = FALSE;
		fPixmapPtr->pNextBusyPixmap = NULL;

		fPixmapPtr = fNextPixmapPtr;
	}

	fPtr->pBusyPixmapLinkList = NULL;
}

static Bool
imxExaZ160IsBusyPixmap(PixmapPtr pPixmap)
{
	/* Pointer must be valid. */
	if (NULL == pPixmap) {
		return FALSE;
	}

	/* Access driver private data associated with pixmap. */
	ImxExaPixmapPtr fPixmapPtr = imxExaZ160GetPixmapDriverPrivate(pPixmap);
	if (NULL == fPixmapPtr) {
		return FALSE;
	}

	return fPixmapPtr->gpuBusy;
}

static void
imxExaZ160SyncIfBusyPixmap(PixmapPtr pPixmap)
{
	/* Access screen associated with this pixmap. */
	ScreenPtr pScreen = pPixmap->drawable.pScreen;
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);

	/* Access driver specific data for screen. */
	ImxPtr imxPtr = IMXPTR(pScrn);
	ImxExaZ160Ptr fPtr = IMXEXAZ160PTR(imxPtr);

	/* Must sync if pixmap is busy with GPU operation. */
	if (TRUE || imxExaZ160IsBusyPixmap(pPixmap)) {

		/* Do the sync */
		imxExaZ160WaitIdleGPU(fPtr);

		/* Then remove all the pixmaps marked as busy. */
		imxExaZ160UntrackAllBusyPixmaps(fPtr);
	}
}

static int
imxExaZ160ComputeSystemMemoryPitch(int width, int bitsPerPixel)
{
	return ((width * bitsPerPixel + FB_MASK) >> FB_SHIFT) * sizeof(FbBits);
}

static void*
imxExaZ160CreatePixmap2(ScreenPtr pScreen, int width, int height,
			int depth, int usage_hint, int bitsPerPixel,
			int *pPitch)
{
	/* Allocate the private data structure to be stored with pixmap. */
	ImxExaPixmapPtr fPixmapPtr =
		(ImxExaPixmapPtr)malloc(sizeof(ImxExaPixmapRec));

	if (NULL == fPixmapPtr) {
		return NULL;
	}

	/* Initialize pixmap properties passed in. */
	fPixmapPtr->width = width;
	fPixmapPtr->height = height;
	fPixmapPtr->depth = depth;
	fPixmapPtr->bitsPerPixel = bitsPerPixel;

	/* Initialize common properties. */
	fPixmapPtr->pitchBytes = 0;
	fPixmapPtr->ptr = NULL;
	fPixmapPtr->canAccel = FALSE;
	fPixmapPtr->gpuAddr = NULL;

	/* Initialize properties for GPU frame buffer allocated memory. */
	fPixmapPtr->widthAligned = 0;
	fPixmapPtr->heightAligned = 0;
	fPixmapPtr->area = NULL;

	/* Initialize properties for system allocated memory. */
	fPixmapPtr->sysAllocSize = 0;
	fPixmapPtr->sysPtr = NULL;

	/* Initialize properties for tracking pixmaps busy with GPU. */
	fPixmapPtr->gpuBusy = FALSE;
	fPixmapPtr->pNextBusyPixmap = NULL;

	/* Nothing more to do if the width or height have no dimensions. */
	if ((0 == width) || (0 == height)) {
		*pPitch = 0;
		return fPixmapPtr;
	}

	/* Access the driver specific data. */
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	ImxPtr imxPtr = IMXPTR(pScrn);
	ImxExaZ160Ptr fPtr = IMXEXAZ160PTR(imxPtr);
	
	/* What is the start of screen (and offscreen) memory. */
	CARD8* screenMemoryBegin =
		(CARD8*)(fPtr->imxExaRec.exaDriverPtr->memoryBase);

	/* First try to allocate pixmap memory from GPU memory but */
	/* can only when bits per pixel >= 8. */
	if (bitsPerPixel >= 8) {

		/* Z160 has 32 pixel width and height alignment. */
		const int gpuAlignedWidth = IMX_EXA_Z160_ALIGN(width);
		const int gpuAlignedHeight = IMX_EXA_Z160_ALIGN(height);

		/* Compute number of pitch bytes for GPU allocated memory. */
		const int gpuPitchBytes = gpuAlignedWidth * bitsPerPixel / 8;

		/* Compute how much memory to allocate for GPU memory. */
		const int gpuAllocSize = gpuAlignedHeight * gpuPitchBytes;

		/* Attemp to allocate from GPU (offscreen) FB memory pool. */
		ExaOffscreenArea* area =
			imxExaOffscreenAlloc(
				pScreen,		/* ScreenPtr */
				gpuAllocSize,		/* size */
				Z160_ALIGN_OFFSET,	/* align */
				TRUE,			/* locked? */
				NULL,			/* save */
				NULL);			/* privData */

		/* If memory allocated, then assign values to private */
		/* data structure and return. */
		if (NULL != area) {

			fPixmapPtr->widthAligned = gpuAlignedWidth;
			fPixmapPtr->heightAligned = gpuAlignedHeight;
			fPixmapPtr->area = area;

			fPixmapPtr->canAccel = TRUE;
			fPixmapPtr->pitchBytes = gpuPitchBytes;
			fPixmapPtr->ptr = screenMemoryBegin + area->offset;
			fPixmapPtr->gpuAddr =
				(void*)((unsigned char*)pScrn->memPhysBase +
					area->offset);

			*pPitch = gpuPitchBytes;
		}
	}

	/* If we could not allocate pixmap memory from GPU memory, */
	/* then must try allocating from system memory. */
	if (NULL == fPixmapPtr->ptr) {

		/* Compute number of pitch bytes for system allocated memory. */
		/* The number of pitch bytes is passed in as the mis-named */
		/* "devKind" parameter. */
		const int sysPitchBytes =
			imxExaZ160ComputeSystemMemoryPitch(width, bitsPerPixel);

		/* Compute how much memory to allocate for system memory. */
		const int sysAllocSize = height * sysPitchBytes;

		/* Attempt to allocate pixmap memory from system memory. */
		void* sysPtr = malloc(sysAllocSize);
		
		/* If memory allocated, then assign values to private */
		/* data structure and return. */
		if (NULL != sysPtr) {

			fPixmapPtr->sysAllocSize = sysAllocSize;
			fPixmapPtr->sysPtr = sysPtr;

			fPixmapPtr->pitchBytes = sysPitchBytes;
			fPixmapPtr->ptr = sysPtr;
			fPixmapPtr->canAccel = FALSE;

			*pPitch = sysPitchBytes;
		}
	}

	/* If we got here and still have no pixmap memory, then cleanup */
	/* and setup to return failure. */
	if (NULL == fPixmapPtr->ptr) {
		free(fPixmapPtr);
		fPixmapPtr = NULL;
	}

	return fPixmapPtr;
}

static void
imxExaZ160DestroyPixmap(ScreenPtr pScreen, void *driverPriv)
{
	/* Nothing to do if driver private pointer not defined. */
	if (NULL == driverPriv) {
		return;
	}

	/* Cast pointer to driver private data structure. */
	ImxExaPixmapPtr fPixmapPtr = (ImxExaPixmapPtr)driverPriv;

	/* Access the driver specific data. */
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	ImxPtr imxPtr = IMXPTR(pScrn);
	ImxExaZ160Ptr fPtr = IMXEXAZ160PTR(imxPtr);

	/* No longer need to track this pixmap as being in GPU operation. */
	if (NULL != fPtr) {
		imxExaZ160UntrackBusyPrivatePixmap(fPtr, fPixmapPtr);
	}

	/* Is pixmap allocated in offscreen frame buffer memory? */
	if (NULL != fPixmapPtr->area) {

		imxExaOffscreenFree(pScreen, fPixmapPtr->area);

	/* Is pixmap allocated in system memory? */
	} else if (NULL != fPixmapPtr->sysPtr) {

		free(fPixmapPtr->sysPtr);
	}

	/* Free the driver private data structure associated with pixmap. */
	free(fPixmapPtr);
}

static Bool
imxExaZ160ModifyPixmapHeader(PixmapPtr pPixmap, int width, int height,
		int depth, int bitsPerPixel, int devKind, pointer pPixData)
{
	/* Make sure the pixmap is defined. */
	if (NULL == pPixmap) {
		return FALSE;
	}

	/* Access driver private data structure associated with pixmap. */
	ImxExaPixmapPtr fPixmapPtr = imxExaZ160GetPixmapDriverPrivate(pPixmap);
	if (NULL == fPixmapPtr) {
		return FALSE;
	}

	/* Access screen associated with this pixmap */
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pPixmap->drawable.pScreen);

	/* Access driver specific data */
	ImxPtr imxPtr = IMXPTR(pScrn);
	ImxExaZ160Ptr fPtr = IMXEXAZ160PTR(imxPtr);

	/* What is the start of screen (and offscreen) memory and its size. */
	CARD8* screenMemoryBegin =
		(CARD8*)(fPtr->imxExaRec.exaDriverPtr->memoryBase);
	CARD8* screenMemoryEnd =
		screenMemoryBegin + fPtr->imxExaRec.exaDriverPtr->memorySize;

	/* Update the width if specified. */
	if (0 < width) {
		fPixmapPtr->width = width;
	}

	/* Update the height if specified. */
	if (0 < height) {
		fPixmapPtr->height = height;
	}

	/* Update the bits per pixel if specified */
	if (0 < bitsPerPixel) {
		fPixmapPtr->bitsPerPixel = bitsPerPixel;
	}

	/* Update the bits per pixel if specified */
	if (0 < depth) {
		fPixmapPtr->depth = depth;
	}

	/* Update the pointer to pixel data if specified. */
	if (0 != pPixData) {

		fPixmapPtr->ptr = pPixData;

		if ((screenMemoryBegin <= (CARD8*)(fPixmapPtr->ptr)) &&
			((CARD8*)(fPixmapPtr->ptr) < screenMemoryEnd)) {

			fPixmapPtr->canAccel = TRUE;

			/* Compute address relative to begin of FB memory. */
			const unsigned long offset =
				(CARD8*)(fPixmapPtr->ptr) - screenMemoryBegin;

			/* Store GPU address. */
			fPixmapPtr->gpuAddr =
				(void*)((unsigned char*)pScrn->memPhysBase +
					offset);

		} else {

			fPixmapPtr->canAccel = FALSE;
		}

		/* If the pixel buffer changed and the pitch was not */
		/* specified, then recompute the pitch. */
		if (0 >= devKind) {
			devKind =
				imxExaZ160ComputeSystemMemoryPitch(
					fPixmapPtr->width,
					fPixmapPtr->bitsPerPixel);
		}
	}

	/* Update the pitch bytes if specified, or if recomputed. */
	if (0 < devKind) {
		fPixmapPtr->pitchBytes = devKind;
	}

	/* Update the pixmap header with our info. */
	pPixmap->drawable.width = fPixmapPtr->width;
	pPixmap->drawable.height = fPixmapPtr->height;
	pPixmap->drawable.bitsPerPixel = fPixmapPtr->bitsPerPixel;
	pPixmap->drawable.depth = fPixmapPtr->depth;
	pPixmap->devPrivate.ptr = fPixmapPtr->ptr;
	pPixmap->devKind = fPixmapPtr->pitchBytes;

	return TRUE;
}

static Bool
imxExaZ160PixmapIsOffscreen(PixmapPtr pPixmap)
{
	/* Make sure pixmap is defined. */
	if (NULL == pPixmap) {
		return FALSE;
	}

	/* Access driver private data structure associated with pixmap. */
	ImxExaPixmapPtr fPixmapPtr = imxExaZ160GetPixmapDriverPrivate(pPixmap);
	if (NULL == fPixmapPtr) {
		return FALSE;
	}

	return fPixmapPtr->canAccel;
}

static void
imxExaZ160WaitMarker(ScreenPtr pScreen, int marker)
{
	/* Do nothing since sync is deferred until CPU needs access. */
}

static Bool
imxExaZ160PrepareAccess(PixmapPtr pPixmap, int index)
{
	/* Function called before CPU access to pixmap.  So, if we have */
	/* not yet waited for GPU to be idle, then do so now. */
	imxExaZ160SyncIfBusyPixmap(pPixmap);

	return TRUE;
}

#else

static void
imxExaZ160TrackBusyPixmap(ImxExaZ160Ptr fPtr, PixmapPtr pPixmap)
{
	/* Do nothing. */
}

static void
imxExaZ160WaitMarker(ScreenPtr pScreen, int marker)
{
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);

	/* Access driver specific data associated with the screen. */
	ImxPtr imxPtr = IMXPTR(pScrn);
	ImxExaZ160Ptr fPtr = IMXEXAZ160PTR(imxPtr);

	imxExaZ160WaitIdleGPU(fPtr);
}

static void
imxExaZ160SyncIfBusyPixmap(PixmapPtr pPixmap)
{
	/* Access screen associated with this pixmap. */
	ScreenPtr pScreen = pPixmap->drawable.pScreen;

	imxExaZ160WaitMarker(pScreen, 0);
}

static Bool
imxExaZ160PrepareAccess(PixmapPtr pPixmap, int index)
{
	/* Do nothing else. */

	return TRUE;
}

#endif

static void
imxExaZ160FinishAccess(PixmapPtr pPixmap, int index)
{
}

static Bool
imxExaZ160PrepareSolid(PixmapPtr pPixmap, int alu, Pixel planemask, Pixel fg)
{
	/* Make sure pixmap is defined. */
	if (NULL == pPixmap) {
		return FALSE;
	}

	/* Make sure operations can be accelerated on this pixmap. */
	if (!imxExaZ160CanAcceleratePixmap(pPixmap)) {
		return FALSE;
	}

	/* Determine number of pixels in target pixmap. */
	unsigned pixmapArea = pPixmap->drawable.width * pPixmap->drawable.height;

	/* Can't accelerate solid fill unless pixmap has minimum number of pixels. */
	if (pixmapArea < IMX_EXA_MIN_PIXEL_AREA_SOLID) {
		return FALSE;
	}

	/* Access screen associated with this pixmap */
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pPixmap->drawable.pScreen);

	/* Access driver specific data */
	ImxPtr imxPtr = IMXPTR(pScrn);
	ImxExaZ160Ptr fPtr = IMXEXAZ160PTR(imxPtr);

	/* Check the number of entities, and fail if it isn't one. */
	if (pScrn->numEntities != 1) {

		xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
			"imxExaZ160PrepareSolid called with number of screen entities (%d) not 1\n",
			pScrn->numEntities);
		return FALSE;
	}

	/* Make sure that the input planemask specifies a solid */
	if (!EXA_PM_IS_SOLID(&pPixmap->drawable, planemask)) {

#if DEBUG_PREPARE_SOLID
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
			"imxExaZ160PrepareSolid called with planemask=0x%08x which is not solid\n",
			(unsigned)planemask);
#endif
		return FALSE;
	}

	/* Make sure that only GXcopy is only raster op called for. */
	if (GXcopy != alu) {

#if DEBUG_PREPARE_SOLID
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
			"imxExaZ160PrepareSolid called with rop=0x%08x which is not GXcopy\n",
			(unsigned)alu);
#endif
		return FALSE;
	}

	/* Access GPU context */
	void* gpuContext = imxExaZ160ContextGet(fPtr);
	if (NULL == gpuContext) {
		return FALSE;
	}

	/* Only 8, 16, and 32-bit pixmaps are supported. */
	/* Associate a pixel format which is required for configuring */
	/* the Z160.  It does not matter what format is chosen as long as it */
	/* is one that matchs the bitsPerPixel.  The format of the input */
	/* foreground color matches the format of the target pixmap, but */
	/* we will shift the bits around to match the chosen format. */
	if (!Z160GetPixmapConfig(pPixmap, &fPtr->z160BufferDst)) {
		return FALSE;
	}
	switch (pPixmap->drawable.bitsPerPixel) {

	case 8:
		fPtr->z160BufferDst.format = Z160_FORMAT_8;	/* value goes in blue channel */
		fPtr->z160BufferDst.swapRB = FALSE;
		fPtr->z160Color = fg & 0x000000FF;
		break;

	case 16:
		fPtr->z160BufferDst.format = Z160_FORMAT_4444;	/* upper nibble */
		fPtr->z160BufferDst.swapRB = FALSE;
		fPtr->z160Color =	((fg & 0x0000F000) << 16) |
					((fg & 0x00000F00) << 12) |
					((fg & 0x000000F0) <<  8) |
					((fg & 0x0000000F) <<  4);
		break;

	case 32:
		fPtr->z160BufferDst.format = Z160_FORMAT_8888;	/* ARGB */
		fPtr->z160BufferDst.swapRB = FALSE;
		fPtr->z160Color = fg;
		break;

	default:
#if DEBUG_PREPARE_SOLID
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
			"imxExaZ160PrepareSolid called with unsupported pixmap bitsPerPixel=%d\n",
			pPixmap>bitsPerPixel);
#endif
		return FALSE;
	}

	/* GPU setup deferred */
	fPtr->gpuOpSetup = FALSE;

	/* Track this pixmap as being busy with GPU operation. */
	imxExaZ160TrackBusyPixmap(fPtr, pPixmap);

	/* Remember the parameters passed in */
	fPtr->pPixmapDst = pPixmap;
	fPtr->solidALU = alu;
	fPtr->solidPlaneMask = planemask;
	fPtr->solidColor = fg;

	return TRUE;
}

static void
imxExaZ160Solid(PixmapPtr pPixmap, int x1, int y1, int x2, int y2)
{
	/* Access screen associated with this pixmap */
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pPixmap->drawable.pScreen);

	/* Access driver specific data */
	ImxPtr imxPtr = IMXPTR(pScrn);
	ImxExaZ160Ptr fPtr = IMXEXAZ160PTR(imxPtr);

	/* Access the GPU */
	void* gpuContext = imxExaZ160ContextGet(fPtr);
	if (NULL == gpuContext) {
		return;
	}

	/* Nothing to unless rectangle has area. */
	if ((x1 >= x2) || (y1 >= y2)) {
		return;
	}

	/* Compute the width and height of the rectangle to fill. */
	int width = x2 - x1;
	int height = y2 - y1;

	/* Need to prepare the GPU for accelerated solid fill? */
	if (!fPtr->gpuOpSetup) {

		z160_setup_buffer_target(gpuContext, &fPtr->z160BufferDst);
		z160_setup_fill_solid(gpuContext, fPtr->z160Color);

		fPtr->gpuOpSetup = TRUE;
	}


	/* Perform GPU accelerated solid fill? */
	z160_fill_solid_rect(gpuContext, x1, y1, width, height);

#if IMX_EXA_DEBUG_SOLID 
	xf86DrvMsg(pScrn->scrnIndex, X_INFO,
		"imxExaZ160Solid called with rect=(%d-%d,%d-%d)\n",
		x1, x2, y1, y2);
#endif

#if IMX_EXA_DEBUG_INSTRUMENT_SIZES
	const unsigned long size = 
		(unsigned long)(z160DstRect.width) * z160DstRect.height;

	if (size < 100) {

		++(fPtr->numSolidFillRect100);

	} else if (size < 1000) {

		++(fPtr->numSolidFillRect1000);

	} else if (size < 10000) {

		++(fPtr->numSolidFillRect10000);

	} else if (size < 100000) {

		++(fPtr->numSolidFillRect100000);

	} else {

		++(fPtr->numSolidFillRectLarge);
	}
#endif
}

static void
imxExaZ160DoneSolid(PixmapPtr pPixmap)
{
	/* Access screen associated with this pixmap */
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pPixmap->drawable.pScreen);

	/* Access driver specific data */
	ImxPtr imxPtr = IMXPTR(pScrn);
	ImxExaZ160Ptr fPtr = IMXEXAZ160PTR(imxPtr);

	/* Access the GPU */
	void* gpuContext = imxExaZ160ContextGet(fPtr);
	if (NULL == gpuContext) {
		return;
	}

	/* Finalize any GPU operations if any where used */
	if (fPtr->gpuOpSetup) {

		/* Flush pending operations to the GPU. */
		z160_flush(gpuContext);

		/* Update state. */
		fPtr->gpuIdle = FALSE;
		fPtr->gpuOpSetup = FALSE;
	}
}

static Bool
imxExaZ160PrepareCopy(
	PixmapPtr pPixmapSrc,
	PixmapPtr pPixmapDst,
	int xdir,
	int ydir,
	int alu,
	Pixel planemask)
{
	/* Make sure source and target pixmaps are defined. */
	if ((NULL == pPixmapDst) || (NULL == pPixmapSrc)) {
		return FALSE;
	}

	/* Make sure operations can be accelerated on the source and target */
	/* pixmaps.  As long as rectangles are within z160 size bounds */
	/* EXA will accelerate copy even if pixmaps are bigger than */
	/* size bounds.  EXA only does this for copies. */
	if (!imxExaimxExaZ160CanAcceleratePixmapRectangles(pPixmapDst) ||
		!imxExaimxExaZ160CanAcceleratePixmapRectangles(pPixmapSrc)) {

		return FALSE;
	}

	/* Determine number of pixels in target and source pixmaps. */
	unsigned pixmapAreaDst =
		pPixmapDst->drawable.width * pPixmapDst->drawable.height;
	unsigned pixmapAreaSrc =
		pPixmapSrc->drawable.width * pPixmapSrc->drawable.height;

	/* Can't accelerate copy unless pixmaps have min number of pixels. */
	if ((pixmapAreaDst < IMX_EXA_MIN_PIXEL_AREA_COPY) || 
		(pixmapAreaSrc < IMX_EXA_MIN_PIXEL_AREA_COPY)) {

		return FALSE;
	}

	/* Access the screen associated with this pixmap. */
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pPixmapDst->drawable.pScreen);

	/* Access driver specific data */
	ImxPtr imxPtr = IMXPTR(pScrn);
	ImxExaZ160Ptr fPtr = IMXEXAZ160PTR(imxPtr);

	/* Access GPU context */
	void* gpuContext = imxExaZ160ContextGet(fPtr);
	if (NULL == gpuContext) {
		return FALSE;
	}

	/* Determine the bits-per-pixels for src and dst pixmaps. */
	int dstPixmapBitsPerPixel = pPixmapDst->drawable.bitsPerPixel;
	int srcPixmapBitsPerPixel = pPixmapSrc->drawable.bitsPerPixel;

	/* Cannot perform copy unless these src and dst pixmaps have the same bits-per-pixel. */
	if (dstPixmapBitsPerPixel != srcPixmapBitsPerPixel) {
		return FALSE;
	}

	/* Check the number of entities, and fail if it isn't one. */
	if (pScrn->numEntities != 1) {

		xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
			"imxExaZ160PrepareCopy called with number of screen entities (%d) not 1\n",
			pScrn->numEntities);
		return FALSE;
	}

	/* Make sure that the input planemask specifies a solid */
	if (!EXA_PM_IS_SOLID(&pPixmapDst->drawable, planemask)) {

#if IMX_EXA_DEBUG_COPY
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
			"imxExaZ160PrepareCopy called with planemask=0x%08x which is not solid\n",
			(unsigned)planemask);
#endif
		return FALSE;
	}

	/* Make sure that only GXcopy is only raster op called for. */
	if (GXcopy != alu) {

#if IMX_EXA_DEBUG_COPY
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
			"imxExaZ160PrepareCopy called with rop=0x%08x which is not GXcopy\n",
			(unsigned)alu);
#endif
		return FALSE;
	}

	/* Setup buffer parameters based on target pixmap. */
	if (!Z160GetPixmapConfig(pPixmapDst, &fPtr->z160BufferDst)) {
		return FALSE;
	}

	/* Setup buffer parameters based on source pixmap. */
	if (!Z160GetPixmapConfig(pPixmapSrc, &fPtr->z160BufferSrc)) {
		return FALSE;
	}

	/* Only 8, 16, and 32-bit pixmaps are supported. */
	/* Associate a pixel format which is required for configuring */
	/* the Z160.  It does not matter what format is chosen as long as it */
	/* is one that matchs the bitsPerPixel. */
	Z160_FORMAT z160Format;
	switch (dstPixmapBitsPerPixel) {

		case 8:
			z160Format = Z160_FORMAT_8;	/* value goes in alpha channel */
			break;

		case 16:
			z160Format = Z160_FORMAT_4444;	/* upper nibble */
			break;

		case 32:
			z160Format = Z160_FORMAT_8888;	/* ARGB */
			break;

		default:
#if IMX_EXA_DEBUG_COPY
			xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
				"imxExaZ160PrepareCopy unsupported pixmap bits per pixel %dy\n",
				dstPixmapBitsPerPixel);
#endif
			return FALSE;
	}
	fPtr->z160BufferDst.format = fPtr->z160BufferSrc.format = z160Format;
	fPtr->z160BufferDst.swapRB = fPtr->z160BufferSrc.swapRB = FALSE;

	/* GPU setup deferred */
	fPtr->gpuOpSetup = FALSE;

	/* Track pixmaps as being busy with GPU operation. */
	imxExaZ160TrackBusyPixmap(fPtr, pPixmapSrc);
	imxExaZ160TrackBusyPixmap(fPtr, pPixmapDst);

	/* Remember the parameters passed in */
	fPtr->pPixmapDst = pPixmapDst;
	fPtr->pPixmapSrc = pPixmapSrc;
	fPtr->copyALU = alu;
	fPtr->copyPlaneMask = planemask;
	fPtr->copyDirX = xdir;
	fPtr->copyDirY = ydir;

	return TRUE;
}

static void
imxExaZ160Copy(PixmapPtr pPixmapDst, int srcX, int srcY, int dstX, int dstY, int width, int height)
{
	/* Access screen associated with dst pixmap */
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pPixmapDst->drawable.pScreen);

	/* Access driver specific data */
	ImxPtr imxPtr = IMXPTR(pScrn);
	ImxExaZ160Ptr fPtr = IMXEXAZ160PTR(imxPtr);

	/* Access the GPU */
	void* gpuContext = imxExaZ160ContextGet(fPtr);
	if (NULL == gpuContext) {
		return;
	}

	/* Need to prepare the GPU for accelerated copy? */
	if (!fPtr->gpuOpSetup) {

		z160_setup_buffer_target(gpuContext, &fPtr->z160BufferDst);
		z160_setup_copy(gpuContext, &fPtr->z160BufferSrc,
					fPtr->copyDirX, fPtr->copyDirY);

		fPtr->gpuOpSetup = TRUE;
	}

	/* Perform GPU accelerated copy */
	z160_copy_rect(gpuContext, dstX, dstY, width, height, srcX, srcY);

#if IMX_EXA_DEBUG_INSTRUMENT_SIZES
	const unsigned long size = 
		(unsigned long)(z160DstRect.width) * z160DstRect.height;

	if (size < 100) {

		++(fPtr->numScreenCopyRect100);

	} else if (size < 1000) {

		++(fPtr->numScreenCopyRect1000);

	} else if (size < 10000) {

		++(fPtr->numScreenCopyRect10000);

	} else if (size < 100000) {

		++(fPtr->numScreenCopyRect100000);

	} else {

		++(fPtr->numScreenCopyRectLarge);
	}
#endif

#if IMX_EXA_DEBUG_COPY
	xf86DrvMsg(pScrn->scrnIndex, X_INFO,
		"imxExaZ160Copy called with src=(%d-%d,%d-%d) dst=(%d-%d,%d-%d)\n",
		srcX, srcX+width, srcY, srcY+height, dstX, dstX+width, dstY, dstY+height);
#endif
}

static void
imxExaZ160DoneCopy(PixmapPtr pPixmapDst)
{
	/* Access screen associated with this pixmap */
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pPixmapDst->drawable.pScreen);

	/* Access driver specific data */
	ImxPtr imxPtr = IMXPTR(pScrn);
	ImxExaZ160Ptr fPtr = IMXEXAZ160PTR(imxPtr);

	/* Access the GPU */
	void* gpuContext = imxExaZ160ContextGet(fPtr);
	if (NULL == gpuContext) {
		return;
	}

	/* Finalize any GPU operations if any where used */
	if (fPtr->gpuOpSetup) {

		/* Flush pending operations to the GPU. */
		z160_flush(gpuContext);

		/* Update state */
		fPtr->gpuIdle = FALSE;
		fPtr->gpuOpSetup = FALSE;
	}
}

static Z160_BLEND Z160SetupBlendOpTable[] = {
	Z160_BLEND_UNKNOWN,	/* 0 = PictOpMinimum, PictOpClear */
	Z160_BLEND_SRC,		/* 1 = PictOpSrc */
	Z160_BLEND_UNKNOWN,	/* 2 = PictOpDst */
	Z160_BLEND_OVER,	/* 3 = PictOpOver */
	Z160_BLEND_UNKNOWN,	/* 4 = PictOpOverReverse */
	Z160_BLEND_IN,		/* 5 = PictOpIn */
	Z160_BLEND_IN_REVERSE,	/* 6 = PictOpInReverse */
	Z160_BLEND_UNKNOWN,	/* 7 = PictOpOut */
	Z160_BLEND_OUT_REVERSE,	/* 8 = PictOpOutReverse */
	Z160_BLEND_UNKNOWN,	/* 9 = PictOpAtop */
	Z160_BLEND_UNKNOWN,	/* 10 = PictOpAtopReverse */
	Z160_BLEND_UNKNOWN,	/* 11 = PictOpXor */
	Z160_BLEND_ADD,		/* 12 = PictOpAdd */
	Z160_BLEND_UNKNOWN	/* 13 = PictOpSaturate, PictOpMaximum */
};
static int NumZ160SetupBlendOps = 
	sizeof(Z160SetupBlendOpTable) / sizeof(Z160SetupBlendOpTable[0]);

#if IMX_EXA_DEBUG_CHECK_COMPOSITE

static const char* 
Z160GetPictureTypeName(PicturePtr pPicture)
{
	switch(PICT_FORMAT_TYPE(pPicture->format)) {
		case PICT_TYPE_OTHER:	return "other";
		case PICT_TYPE_A:	return "alpha";
		case PICT_TYPE_ARGB:	return "argb";
		case PICT_TYPE_ABGR:	return "abgr";
		case PICT_TYPE_COLOR:	return "color";
		case PICT_TYPE_GRAY:	return "gray";
		case PICT_TYPE_BGRA:	return "bgra";
		default:		return "???";
	}
}

#endif

/*
 * From /usr/include/xorg/exa.h:
 *
 * Notes on interpreting Picture structures:
 * - The Picture structures will always have a valid pDrawable.
 * - The Picture structures will never have alphaMap set.
 * - The mask Picture (and therefore pMask) may be NULL, in which case the
 *   operation is simply src OP dst instead of src IN mask OP dst, and
 *   mask coordinates should be ignored.
 * - pMarkPicture may have componentAlpha set, which greatly changes
 *   the behavior of the Composite operation.  componentAlpha has no effect
 *   when set on pSrcPicture or pDstPicture.
 * - The source and mask Pictures may have a transformation set
 *   (Picture->transform != NULL), which means that the source coordinates
 *   should be transformed by that transformation, resulting in scaling,
 *   rotation, etc.  The PictureTransformPoint() call can transform
 *   coordinates for you.  Transforms have no effect on Pictures when used
 *   as a destination.
 * - The source and mask pictures may have a filter set.  PictFilterNearest
 *   and PictFilterBilinear are defined in the Render protocol, but others
 *   may be encountered, and must be handled correctly (usually by
 *   PrepareComposite failing, and falling back to software).  Filters have
 *   no effect on Pictures when used as a destination.
 * - The source and mask Pictures may have repeating set, which must be
 *   respected.  Many chipsets will be unable to support repeating on
 *   pixmaps that have a width or height that is not a power of two.
 */

static Bool
imxExaZ160CheckComposite(int op, PicturePtr pPictureSrc, PicturePtr pPictureMask, PicturePtr pPictureDst)
{
	/* Pictures for src and dst must be defined. */
	if ((NULL == pPictureSrc) || (NULL == pPictureDst)) {
		return FALSE;
	}

	/* Access the pixmap associated with each picture */
	PixmapPtr pPixmapDst = imxExaZ160GetPicturePixmap(pPictureDst);
	PixmapPtr pPixmapSrc = imxExaZ160GetPicturePixmap(pPictureSrc);
	PixmapPtr pPixmapMask = imxExaZ160GetPicturePixmap(pPictureMask);

	/* Cannot perform blend if there is no target pixmap. */
	if (NULL == pPixmapDst) {
		return FALSE;
	}

	/* Access screen associated with dst pixmap (same screen as for src pixmap). */
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pPixmapDst->drawable.pScreen);

	/* Check the number of entities, and fail if it isn't one. */
	if (pScrn->numEntities != 1) {

		xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
			"imxExaZ160CheckComposite called with number of screen entities (%d) not 1\n",
			pScrn->numEntities);
		return FALSE;
	}

	/* Cannot perform blend if there is no source pixmap. */
	if (NULL == pPixmapSrc) {
		return FALSE;
	}

	/* Cannot perform blend unless screens associated with src and dst pictures are same. */
	if (pPixmapSrc->drawable.pScreen->myNum !=
		pPixmapDst->drawable.pScreen->myNum) {

		return FALSE;
	}

	/* Make sure operations can be accelerated on the target pixmap. */
	if (!imxExaZ160CanAcceleratePixmap(pPixmapDst)) {
		return FALSE;
	}

	/* Make sure operations can be accelerated on the source pixmap. */
	if (!imxExaZ160CanAcceleratePixmap(pPixmapSrc)) {
		return FALSE;
	}

	/* Make sure operations can be accelerated on optional mask pixmap. */
	if ((NULL != pPixmapMask) &&
		!imxExaZ160CanAcceleratePixmap(pPixmapMask)) {

		return FALSE;
	}

	/* Can't accel composite unless target pixmap has min pixels. */
	unsigned pixmapAreaDst =
		pPixmapDst->drawable.width * pPixmapDst->drawable.height;
	if (pixmapAreaDst < IMX_EXA_MIN_PIXEL_AREA_COMPOSITE) {

		return FALSE;
	}

	/* Can't accel composite unless pixmap from non-repeat source picture */
	/* has a minimum number of pixels. */
	if (! pPictureSrc->repeat) {

		unsigned pixmapArea =
			pPixmapSrc->drawable.width *
				pPixmapSrc->drawable.height;

		if (pixmapArea < IMX_EXA_MIN_PIXEL_AREA_COMPOSITE) {
			return FALSE;
		}
	}

	/* Can't accel composite unless pixmap from defined non-repeat mask */
	/* picture has a minimum number of pixels. */
	if ((NULL != pPictureMask) && !pPictureMask->repeat) {

		unsigned pixmapArea =
			pPixmapMask->drawable.width *
				pPixmapMask->drawable.height;

		if (pixmapArea < IMX_EXA_MIN_PIXEL_AREA_COMPOSITE) {
			return FALSE;
		}
	}

	/* Reset this variable if cannot support composite. */
	Bool canComposite = TRUE;

	/* Check if blending operation is supported. */
	if ((0 > op) || (NumZ160SetupBlendOps <= op) ||
		(Z160_BLEND_UNKNOWN == Z160SetupBlendOpTable[op])) {

		canComposite = FALSE;
	}

	/* Determine Z160 config that matches format used in target picture. */
	Z160Buffer z160BufferDst;
	Bool z160BufferDstDefined =
		imxExaZ160GetPictureConfig(pScrn, pPictureDst, &z160BufferDst);
	if (!z160BufferDstDefined) {
		canComposite = FALSE;
	}

	/* Determine Z160 config that matches format used in source picture. */
	Z160Buffer z160BufferSrc;
	Bool z160BufferSrcDefined =
		imxExaZ160GetPictureConfig(pScrn, pPictureSrc, &z160BufferSrc);
	if (!z160BufferSrcDefined) {
		canComposite = FALSE;
	}

	/* Determine Z160 config that matches format used in mask picture. */
	Z160Buffer z160BufferMask;
	Bool z160BufferMaskDefined = FALSE;
	if (NULL != pPictureMask) {

		z160BufferMaskDefined =
			imxExaZ160GetPictureConfig(pScrn, pPictureMask, &z160BufferMask);
		if (!z160BufferMaskDefined) {
			canComposite = FALSE;
		}
	}
	
	/* Do not accelerate masks that do not have an alpha channel. */
	if (z160BufferMaskDefined) {
		if (0 == PICT_FORMAT_A(pPictureMask->format)) {
			canComposite = FALSE;
		}
	}

	/* Do not accelerate sources with a transform. */
	if (NULL != pPictureSrc->transform) {
		canComposite = FALSE;
	}

	/* Do not accelerate masks, if defined, that have a transform. */
	if ((NULL != pPictureMask) && (NULL != pPictureMask->transform)) {
		canComposite = FALSE;
	}

	/* Do not accelerate when mask, if defined, is repeating. */
	if ((NULL != pPictureMask) && pPictureMask->repeat) {
		canComposite = FALSE;
	}

#if IMX_EXA_DEBUG_CHECK_COMPOSITE

	/* Check whether to log parameter data when composite is rejected. */
	if (! canComposite) {

		/* Source OP Target */
		if (NULL == pPictureMask) {

			xf86DrvMsg(pScrn->scrnIndex, X_INFO,
				"imxExaZ160CheckComposite not support: SRC(%s%dx%d,%s%d%s:%d%d%d%d) op=%d DST(%d%s:%d%d%d%d)\n",
				pPictureSrc->repeat ? "R" : "",
				pPictureSrc->pDrawable->width,
				pPictureSrc->pDrawable->height,
				(NULL != pPictureSrc->transform) ? "T" : "",
				PICT_FORMAT_BPP(pPictureSrc->format),
				Z160GetPictureTypeName(pPictureSrc),
				PICT_FORMAT_A(pPictureSrc->format),
				PICT_FORMAT_R(pPictureSrc->format),
				PICT_FORMAT_G(pPictureSrc->format),
				PICT_FORMAT_B(pPictureSrc->format),
				op,
				PICT_FORMAT_BPP(pPictureDst->format),
				Z160GetPictureTypeName(pPictureDst),
				PICT_FORMAT_A(pPictureDst->format),
				PICT_FORMAT_R(pPictureDst->format),
				PICT_FORMAT_G(pPictureDst->format),
				PICT_FORMAT_B(pPictureDst->format));


		/* (Source IN Mask) OP Target */
		} else {

			xf86DrvMsg(pScrn->scrnIndex, X_INFO,
				"imxExaZ160CheckComposite not support: SRC(%s%dx%d,%s%d%s:%d%d%d%d) MASK(%s%dx%d,%s%d%s:%s%d%d%d%d) op=%d DST(%d%s:%d%d%d%d)\n",
				pPictureSrc->repeat ? "R" : "",
				pPictureSrc->pDrawable->width,
				pPictureSrc->pDrawable->height,
				(NULL != pPictureSrc->transform) ? "T" : "",
				PICT_FORMAT_BPP(pPictureSrc->format),
				Z160GetPictureTypeName(pPictureSrc),
				PICT_FORMAT_A(pPictureSrc->format),
				PICT_FORMAT_R(pPictureSrc->format),
				PICT_FORMAT_G(pPictureSrc->format),
				PICT_FORMAT_B(pPictureSrc->format),
				pPictureMask->repeat ? "R" : "",
				pPictureMask->pDrawable->width,
				pPictureMask->pDrawable->height,
				(NULL != pPictureMask->transform) ? "T" : "",
				PICT_FORMAT_BPP(pPictureMask->format),
				Z160GetPictureTypeName(pPictureMask),
				pPictureMask->componentAlpha ? "C" : "",
				PICT_FORMAT_A(pPictureMask->format),
				PICT_FORMAT_R(pPictureMask->format),
				PICT_FORMAT_G(pPictureMask->format),
				PICT_FORMAT_B(pPictureMask->format),
				op,
				PICT_FORMAT_BPP(pPictureDst->format),
				Z160GetPictureTypeName(pPictureDst),
				PICT_FORMAT_A(pPictureDst->format),
				PICT_FORMAT_R(pPictureDst->format),
				PICT_FORMAT_G(pPictureDst->format),
				PICT_FORMAT_B(pPictureDst->format));
		}

	}

#endif

	return canComposite;
}

static Bool
imxExaZ160PrepareComposite(
	int op,
	PicturePtr pPictureSrc,
	PicturePtr pPictureMask,
	PicturePtr pPictureDst,
	PixmapPtr pPixmapSrc,
	PixmapPtr pPixmapMask,
	PixmapPtr pPixmapDst)
{
	/* Access screen associated with dst pixmap. */
	/* Should be same screen as for src pixmap. */
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pPixmapDst->drawable.pScreen);

	/* NOTE - many preconditions already verified in CheckComposite. */

	/* Determine Z160 config that matches format used in target picture. */
	Z160Buffer z160BufferDst;
	if (!imxExaZ160GetPictureConfig(pScrn, pPictureDst, &z160BufferDst)) {
		return FALSE;
	}

	/* Determine Z160 config that matches format used in source picture. */
	Z160Buffer z160BufferSrc;
	if (!imxExaZ160GetPictureConfig(pScrn, pPictureSrc, &z160BufferSrc)) {
		return FALSE;
	}

	/* Access driver specific data */
	ImxPtr imxPtr = IMXPTR(pScrn);
	ImxExaZ160Ptr fPtr = IMXEXAZ160PTR(imxPtr);

	/* Access GPU context */
	void* gpuContext = imxExaZ160ContextGet(fPtr);
	if (NULL == gpuContext) {
		return FALSE;
	}

	/* Map the Xrender blend op into the Z160 blend op. */
	Z160_BLEND z160BlendOp = Z160SetupBlendOpTable[op];

	/* Setup the target buffer */
	z160_setup_buffer_target(gpuContext, &z160BufferDst);

	/* Mask blend? */
	fPtr->gpuOpSetup = FALSE;
	if (NULL != pPictureMask) {

		/* Determine Z160 config that matches format in mask picture */
		Z160Buffer z160BufferMask;
		if (!imxExaZ160GetPictureConfig(pScrn, pPictureMask, &z160BufferMask)) {
			return FALSE;
		}

		/* Blend repeating source using a mask */
		if (pPictureSrc->repeat) {

			/* Source is 1x1 (constant) repeat pattern? */
			if (imxExaZ160IsDrawablePixelOnly(pPictureSrc->pDrawable)) {

				z160_setup_blend_const_masked(
					gpuContext,
					z160BlendOp,
					&z160BufferSrc,
					&z160BufferMask);

				fPtr->gpuOpSetup = TRUE;

			/* Source is arbitrary sized repeat pattern? */
			} else {

				z160_setup_blend_pattern_masked(
					gpuContext,
					z160BlendOp,
					&z160BufferSrc,
					&z160BufferMask);

				fPtr->gpuOpSetup = TRUE;
			}

		/* Simple (source IN mask) blend */
		} else {

			z160_setup_blend_image_masked(
				gpuContext,
				z160BlendOp,
				&z160BufferSrc,
				&z160BufferMask);

			fPtr->gpuOpSetup = TRUE;
		}

	/* Source only (no mask) blend */
	} else {

		/* Repeating source (pattern)? */
		if (pPictureSrc->repeat) {

			/* Source is 1x1 (constant) repeat pattern? */
			if (imxExaZ160IsDrawablePixelOnly(pPictureSrc->pDrawable)) {

				z160_setup_blend_const(
					gpuContext,
					z160BlendOp,
					&z160BufferSrc);

				fPtr->gpuOpSetup = TRUE;

			/* Source is arbitrary sized repeat pattern? */
			} else {

				z160_setup_blend_pattern(
					gpuContext,
					z160BlendOp,
					&z160BufferSrc);

				fPtr->gpuOpSetup = TRUE;
			}

		/* Simple source blend */
		} else {

			z160_setup_blend_image(gpuContext, z160BlendOp, &z160BufferSrc);
			fPtr->gpuOpSetup = TRUE;
		}
	}

	/* Note if the composite operation is being accelerated. */
	if (fPtr->gpuOpSetup) {

		/* Track pixmaps as being busy with GPU operation. */
		imxExaZ160TrackBusyPixmap(fPtr, pPixmapSrc);
		imxExaZ160TrackBusyPixmap(fPtr, pPixmapDst);
		if (NULL != pPixmapMask) {
			imxExaZ160TrackBusyPixmap(fPtr, pPixmapMask);
		}

		return TRUE;
	}

#if IMX_EXA_DEBUG_CHECK_COMPOSITE

	/* Log info about which operations were not accelerated. */

	/* Source OP Target */
	if (NULL == pPictureMask) {

		xf86DrvMsg(pScrn->scrnIndex, X_INFO,
			"imxExaZ160PrepareComposite not support: SRC(%s%dx%d,%s%d%s:%d%d%d%d) op=%d DST(%d%s:%d%d%d%d)\n",
			pPictureSrc->repeat ? "R" : "",
			pPictureSrc->pDrawable->width,
			pPictureSrc->pDrawable->height,
			(NULL != pPictureSrc->transform) ? "T" : "",
			PICT_FORMAT_BPP(pPictureSrc->format),
			Z160GetPictureTypeName(pPictureSrc),
			PICT_FORMAT_A(pPictureSrc->format),
			PICT_FORMAT_R(pPictureSrc->format),
			PICT_FORMAT_G(pPictureSrc->format),
			PICT_FORMAT_B(pPictureSrc->format),
			op,
			PICT_FORMAT_BPP(pPictureDst->format),
			Z160GetPictureTypeName(pPictureDst),
			PICT_FORMAT_A(pPictureDst->format),
			PICT_FORMAT_R(pPictureDst->format),
			PICT_FORMAT_G(pPictureDst->format),
			PICT_FORMAT_B(pPictureDst->format));


	/* (Source IN Mask) OP Target */
	} else {

		xf86DrvMsg(pScrn->scrnIndex, X_INFO,
			"imxExaZ160PrepareComposite not support: SRC(%s%dx%d,%s%d%s:%d%d%d%d) MASK(%s%dx%d,%s%d%s:%s%d%d%d%d) op=%d DST(%d%s:%d%d%d%d)\n",
			pPictureSrc->repeat ? "R" : "",
			pPictureSrc->pDrawable->width,
			pPictureSrc->pDrawable->height,
			(NULL != pPictureSrc->transform) ? "T" : "",
			PICT_FORMAT_BPP(pPictureSrc->format),
			Z160GetPictureTypeName(pPictureSrc),
			PICT_FORMAT_A(pPictureSrc->format),
			PICT_FORMAT_R(pPictureSrc->format),
			PICT_FORMAT_G(pPictureSrc->format),
			PICT_FORMAT_B(pPictureSrc->format),
			pPictureMask->repeat ? "R" : "",
			pPictureMask->pDrawable->width,
			pPictureMask->pDrawable->height,
			(NULL != pPictureMask->transform) ? "T" : "",
			PICT_FORMAT_BPP(pPictureMask->format),
			Z160GetPictureTypeName(pPictureMask),
			pPictureMask->componentAlpha ? "C" : "",
			PICT_FORMAT_A(pPictureMask->format),
			PICT_FORMAT_R(pPictureMask->format),
			PICT_FORMAT_G(pPictureMask->format),
			PICT_FORMAT_B(pPictureMask->format),
			op,
			PICT_FORMAT_BPP(pPictureDst->format),
			Z160GetPictureTypeName(pPictureDst),
			PICT_FORMAT_A(pPictureDst->format),
			PICT_FORMAT_R(pPictureDst->format),
			PICT_FORMAT_G(pPictureDst->format),
			PICT_FORMAT_B(pPictureDst->format));
	}

#endif

	return FALSE;
}

static void
imxExaZ160Composite(
	PixmapPtr pPixmapDst,
	int srcX,
	int srcY,
	int maskX,
	int maskY,
	int dstX,
	int dstY,
	int width,
	int height)
{
	/* Access screen associated with dst pixmap */
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pPixmapDst->drawable.pScreen);

	/* Access driver specific data */
	ImxPtr imxPtr = IMXPTR(pScrn);
	ImxExaZ160Ptr fPtr = IMXEXAZ160PTR(imxPtr);

	/* Access the GPU */
	void* gpuContext = imxExaZ160ContextGet(fPtr);
	if (NULL == gpuContext) {
		return;
	}

	/* Perform rectangle render based on setup in PrepareComposite */
	switch (z160_get_setup(gpuContext)) {

		case Z160_SETUP_BLEND_IMAGE:
			z160_blend_image_rect(
				gpuContext,
				dstX, dstY,
				width, height,
				srcX, srcY);
			break;

		case Z160_SETUP_BLEND_IMAGE_MASKED:
			z160_blend_image_masked_rect(
				gpuContext,
				dstX, dstY,
				width, height,
				srcX, srcY,
				maskX, maskY);
			break;

		case Z160_SETUP_BLEND_CONST:
			z160_blend_const_rect(
				gpuContext,
				dstX, dstY,
				width, height);
			break;

		case Z160_SETUP_BLEND_CONST_MASKED:
			z160_blend_const_masked_rect(
				gpuContext,
				dstX, dstY,
				width, height,
				maskX, maskY);
			break;

		case Z160_SETUP_BLEND_PATTERN:
			z160_blend_pattern_rect(
				gpuContext,
				dstX, dstY,
				width, height,
				srcX, srcY);
			break;

		case Z160_SETUP_BLEND_PATTERN_MASKED:
			z160_blend_pattern_masked_rect(
				gpuContext,
				dstX, dstY,
				width, height,
				srcX, srcY,
				maskX, maskY);
			break;

		default:
			return;
	}

#if 0
	xf86DrvMsg(pScrn->scrnIndex, X_INFO,
		"imxExaZ160Composite called with src=(%d-%d,%d-%d) dst=(%d-%d,%d-%d)\n",
		srcX, srcX+width, srcY, srcY+height, dstX, dstX+width, dstY, dstY+height);
#endif
}

static void
imxExaZ160DoneComposite(PixmapPtr pPixmapDst)
{
	/* Access screen associated with this pixmap */
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pPixmapDst->drawable.pScreen);

	/* Access driver specific data */
	ImxPtr imxPtr = IMXPTR(pScrn);
	ImxExaZ160Ptr fPtr = IMXEXAZ160PTR(imxPtr);

	/* Flush pending operations to the GPU. */
	/* Access the GPU */
	void* gpuContext = imxExaZ160ContextGet(fPtr);
	if (NULL == gpuContext) {
		return;
	}

	z160_flush(gpuContext);

	/* Update state. */
	fPtr->gpuIdle = FALSE;
	fPtr->gpuOpSetup = FALSE;
}

static Bool
imxExaZ160UploadToScreen(
	PixmapPtr pPixmapDst,
	int dstX,
	int dstY,
	int width,
	int height,
	char* pBufferSrc,
	int pitchSrc)
{
	/* Cannot support target pixmaps unless 8, 16, or 32 bits per pixel */
	imx_copy_sw_no_overlap_func copy_func = NULL;
	switch(pPixmapDst->drawable.bitsPerPixel) {
		case 8:
			copy_func = imx_copy_sw_no_overlap_8;
			break;
		case 16:
			copy_func = imx_copy_sw_no_overlap_16;
			break;
		case 32:
			copy_func = imx_copy_sw_no_overlap_32;
			break;
		default:
			return FALSE;
	}

	/* Access screen associated with this pixmap */
	ScreenPtr pScreen = pPixmapDst->drawable.pScreen;
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);

	/* Access driver specific data */
	ImxPtr imxPtr = IMXPTR(pScrn);
	ImxExaZ160Ptr fPtr = IMXEXAZ160PTR(imxPtr);

	/* Wait for the GPU to become idle if busy with pixmap. */
	imxExaZ160SyncIfBusyPixmap(pPixmapDst);

	/* Compute number of bytes per pixel to transfer. */
	int bytesPerPixel = pPixmapDst->drawable.bitsPerPixel / 8;

	/* Access the pitch for the target pixmap. */
	int pitchDst = exaGetPixmapPitch(pPixmapDst);

	/* Access the starting address for the pixmap. */
	unsigned char* pBufferDst =
		(unsigned char*)imxExaZ160GetPixmapAddress(pPixmapDst);

	/* Advance to the starting pixel. */
	pBufferDst += (dstY * pitchDst + dstX * bytesPerPixel);

	/* Do the copy */
	(*copy_func)(pBufferDst, pBufferSrc,
			width, height,
			pitchDst, pitchSrc);

	return TRUE;
}

static Bool
imxExaZ160DownloadFromScreen(
	PixmapPtr pPixmapSrc,
	int srcX,
	int srcY,
	int width,
	int height,
	char* pBufferDst,
	int pitchDst)
{
	/* Cannot support target pixmaps unless 8, 16, or 32 bits per pixel */
	imx_copy_sw_no_overlap_func copy_func = NULL;
	switch(pPixmapSrc->drawable.bitsPerPixel) {
		case 8:
			copy_func = imx_copy_sw_no_overlap_8;
			break;
		case 16:
			copy_func = imx_copy_sw_no_overlap_16;
			break;
		case 32:
			copy_func = imx_copy_sw_no_overlap_32;
			break;
		default:
			return FALSE;
	}

	/* Access screen associated with this pixmap */
	ScreenPtr pScreen = pPixmapSrc->drawable.pScreen;
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);

	/* Access driver specific data */
	ImxPtr imxPtr = IMXPTR(pScrn);
	ImxExaZ160Ptr fPtr = IMXEXAZ160PTR(imxPtr);

	/* Wait for the GPU to become idle if busy with pixmap. */
	imxExaZ160SyncIfBusyPixmap(pPixmapSrc);

	/* Compute number of bytes per pixel to transfer. */
	int bytesPerPixel = pPixmapSrc->drawable.bitsPerPixel / 8;

	/* Access the pitch for the source pixmap. */
	int pitchSrc = exaGetPixmapPitch(pPixmapSrc);

	/* Access the starting address for the pixmap. */
	unsigned char* pBufferSrc =
		(unsigned char*)imxExaZ160GetPixmapAddress(pPixmapSrc);

	/* Advance to the starting pixel. */
	pBufferSrc += (srcY * pitchSrc + srcX * bytesPerPixel);

	/* Do the copy */
	(*copy_func)(pBufferDst, pBufferSrc,
			width, height,
			pitchDst, pitchSrc);

	return TRUE;
}

Bool
imxExaZ160CloseScreen(CLOSE_SCREEN_ARGS_DECL)
{
	CLOSE_SCREEN_DECL_ScrnInfoPtr;
	ImxPtr imxPtr = IMXPTR(pScrn);

	ImxExaZ160Ptr fPtr = IMXEXAZ160PTR(imxPtr);
	if (NULL != fPtr) {

#if IMX_EXA_DEBUG_INSTRUMENT_SIZES

		syslog(LOG_INFO | LOG_USER,
			"Z160 Xorg driver: %d = num solid fill rects < 100 pixels\n",
			fPtr->numSolidFillRect100);

		syslog(LOG_INFO | LOG_USER,
			"Z160 Xorg driver: %d = num solid fill rects 101 - 1000 pixels\n",
			fPtr->numSolidFillRect1000);

		syslog(LOG_INFO | LOG_USER,
			"Z160 Xorg driver: %d = num solid fill rects 1001 - 10000 pixels\n",
			fPtr->numSolidFillRect10000);

		syslog(LOG_INFO | LOG_USER,
			"Z160 Xorg driver: %d = num solid fill rects 10001 - 100000 pixels\n",
			fPtr->numSolidFillRect100000);

		syslog(LOG_INFO | LOG_USER,
			"Z160 Xorg driver: %d = num solid fill rects > 100000 pixels\n",
			fPtr->numSolidFillRectLarge);

		syslog(LOG_INFO | LOG_USER,
			"Z160 Xorg driver: %d = num screen copy rects < 100 pixels\n",
			fPtr->numScreenCopyRect100);

		syslog(LOG_INFO | LOG_USER,
			"Z160 Xorg driver: %d = num screen copy rects 101 - 1000 pixels\n",
			fPtr->numScreenCopyRect1000);

		syslog(LOG_INFO | LOG_USER,
			"Z160 Xorg driver: %d = num screen copy rects 1001 - 10000 pixels\n",
			fPtr->numScreenCopyRect10000);

		syslog(LOG_INFO | LOG_USER,
			"Z160 Xorg driver: %d = num screen copy rects 10001 - 100000 pixels\n",
			fPtr->numScreenCopyRect100000);

		syslog(LOG_INFO | LOG_USER,
			"Z160 Xorg driver: %d = num screen copy rects > 100000 pixels\n",
			fPtr->numScreenCopyRectLarge);
#endif

		/* EXA cleanup */
		if (NULL != fPtr->imxExaRec.exaDriverPtr) {

#if IMX_EXA_ENABLE_HANDLES_PIXMAPS
			/* Driver allocation of pixmaps will use the built-in */
			/* EXA offscreen memory manager. */
			imxExaOffscreenFini(pScreen);
#endif

			exaDriverFini(pScreen);
			free(fPtr->imxExaRec.exaDriverPtr);
			fPtr->imxExaRec.exaDriverPtr = NULL;
		}

		/* Shutdown the Z160 hardware access. */
		imxExaZ160ContextRelease(fPtr);

		/* Closing the file for the mutex releases lock. */
		/* But try to be explicit. */
		if (-1 != fPtr->fdMutex) {

			lockf(fPtr->fdMutex, F_ULOCK, 0);
			close(fPtr->fdMutex);
			fPtr->fdMutex = -1;
		}

		/* Always remove the mutex file. */
		unlink(IMX_EXA_SEM_NAME);

		/* Restore the saved CloseScreen function */
		pScreen->CloseScreen = fPtr->saveCloseScreenPtr;

		/* Destroy EXA private data structure. */
		free(imxPtr->exaDriverPrivate);
		imxPtr->exaDriverPrivate = NULL;
	}

	/* Restore the saved CloseScreen function to call it. */
	/* Install our CloseScreen function so that it gets called. */
	if (NULL != pScreen->CloseScreen) {

		return (*pScreen->CloseScreen)(CLOSE_SCREEN_ARGS);
	}

	return TRUE;
}

Bool
imxExaZ160Setup(int scrnIndex, ScreenPtr pScreen)
{
	/* Access the screen info and then private data structures. */
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	ImxPtr imxPtr = IMXPTR(pScrn);

	/* Private data structure must not already be in use. */
	if (NULL != imxPtr->exaDriverPrivate) {
		return FALSE;
	}
	
	/* Allocate memory for EXA Z160 private data */
	imxPtr->exaDriverPrivate = calloc(sizeof(ImxExaZ160Rec), 1);
	if (NULL == imxPtr->exaDriverPrivate) {
		return FALSE;
	}
	ImxExaZ160Ptr fPtr = IMXEXAZ160PTR(imxPtr);

	/* Initialize EXA Z160 private data. */
	fPtr->fdMutex = -1;
	fPtr->saveCloseScreenPtr = NULL;
	fPtr->gpuContext = NULL;
	fPtr->gpuIdle = FALSE;
	fPtr->gpuOpSetup = FALSE;

#if IMX_EXA_DEBUG_INSTRUMENT_SIZES
	fPtr->numSolidFillRect100 = 0;
	fPtr->numSolidFillRect1000 = 0;
	fPtr->numSolidFillRect10000 = 0;
	fPtr->numSolidFillRect100000 = 0;
	fPtr->numSolidFillRectLarge = 0;

	fPtr->numScreenCopyRect100 = 0;
	fPtr->numScreenCopyRect1000 = 0;
	fPtr->numScreenCopyRect10000 = 0;
	fPtr->numScreenCopyRect100000 = 0;
	fPtr->numScreenCopyRectLarge = 0;
#endif

	/* Remember the index associated with this screen. */
	fPtr->scrnIndex = pScrn->scrnIndex;

	/* First try to open and create the mutex. */
	fPtr->fdMutex = open(IMX_EXA_SEM_NAME, O_CREAT | O_EXCL | O_RDWR, 0600);
	if ((-1 == fPtr->fdMutex) && (EEXIST == errno)) {

		/* Get here if another process has it open exclusively. */
		fPtr->fdMutex = open(IMX_EXA_SEM_NAME, O_CREAT | O_RDWR, 0600);
		if (-1 == fPtr->fdMutex) {

			xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
				"Unable to open mutex lock file '%s'.\n",
				IMX_EXA_SEM_NAME);
			return FALSE;
		}
	}

	/* Try to lock the file.  Does not block. */
	if (0 != lockf(fPtr->fdMutex, F_TLOCK, 0)) {

		xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
			"Exclusive access for Z160 interface failed.\n");
		close(fPtr->fdMutex);
		return FALSE;
	}

	/* Initialize the Z160 hardware */
	if (!imxExaZ160ContextGet(fPtr)) {

		xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
			"Initialize Z160 interfaces failed.\n");
		imxExaZ160CloseScreen(CLOSE_SCREEN_ARGS);
		return FALSE;
	}

	/* Initialize EXA. */
	ExaDriverPtr exaDriverPtr = exaDriverAlloc();
	if (NULL == exaDriverPtr) {

		xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
			"Allocate EXA driver structure.\n");
		imxExaZ160CloseScreen(CLOSE_SCREEN_ARGS);
		return FALSE;
	}

	/* Alignment of pixmap pitch is 32 pixels for z430 */
	/* (4 pixels for z160), but times 4 bytes max per pixel. */
	const unsigned long pixmapPitchAlign = 32 * 4;

	exaDriverPtr->exa_major = EXA_VERSION_MAJOR;
	exaDriverPtr->exa_minor = EXA_VERSION_MINOR;
	exaDriverPtr->flags = EXA_OFFSCREEN_PIXMAPS;
	exaDriverPtr->memoryBase = imxPtr->fbMemoryStart;
	exaDriverPtr->memorySize = imxPtr->fbMemorySize;
	exaDriverPtr->offScreenBase = imxPtr->fbMemoryScreenReserve;
	exaDriverPtr->pixmapOffsetAlign = Z160_ALIGN_OFFSET;
	exaDriverPtr->pixmapPitchAlign = pixmapPitchAlign;
	exaDriverPtr->maxPitchBytes = Z160_MAX_PITCH_BYTES;
	exaDriverPtr->maxX = Z160_MAX_WIDTH - 1;
	exaDriverPtr->maxY = Z160_MAX_HEIGHT - 1;

	/* Required */
	exaDriverPtr->WaitMarker = imxExaZ160WaitMarker;

	/* Solid fill - required */
	exaDriverPtr->PrepareSolid = imxExaZ160PrepareSolid;
	exaDriverPtr->Solid = imxExaZ160Solid;
	exaDriverPtr->DoneSolid = imxExaZ160DoneSolid;

	/* Copy - required */
	exaDriverPtr->PrepareCopy = imxExaZ160PrepareCopy;
	exaDriverPtr->Copy = imxExaZ160Copy;
	exaDriverPtr->DoneCopy = imxExaZ160DoneCopy;

	/* Composite */
	exaDriverPtr->CheckComposite = imxExaZ160CheckComposite;
	exaDriverPtr->PrepareComposite = imxExaZ160PrepareComposite;
	exaDriverPtr->Composite = imxExaZ160Composite;
	exaDriverPtr->DoneComposite = imxExaZ160DoneComposite;

	/* Screen upload/download */
	exaDriverPtr->UploadToScreen = imxExaZ160UploadToScreen;
	exaDriverPtr->DownloadFromScreen = imxExaZ160DownloadFromScreen;

	/* Access prepare/finish */
#if (IMX_EXA_VERSION_COMPILED >= IMX_EXA_VERSION(2,5,0))
	exaDriverPtr->flags |= EXA_SUPPORTS_PREPARE_AUX;
#endif
	exaDriverPtr->PrepareAccess = imxExaZ160PrepareAccess;
	exaDriverPtr->FinishAccess = imxExaZ160FinishAccess;

#if IMX_EXA_ENABLE_HANDLES_PIXMAPS

	/* For driver pixmap allocation. */
	exaDriverPtr->flags |= EXA_HANDLES_PIXMAPS;
	exaDriverPtr->CreatePixmap2 = imxExaZ160CreatePixmap2;
	exaDriverPtr->DestroyPixmap = imxExaZ160DestroyPixmap;
	exaDriverPtr->ModifyPixmapHeader = imxExaZ160ModifyPixmapHeader;
	exaDriverPtr->PixmapIsOffscreen = imxExaZ160PixmapIsOffscreen;

#endif

	/* EXA initialization. */
	if (!exaDriverInit(pScreen, exaDriverPtr)) {

		xf86DrvMsg(pScrn->scrnIndex, X_ERROR, "EXA initialization failed.\n");
		imxExaZ160CloseScreen(CLOSE_SCREEN_ARGS);
		return FALSE;
	}
	fPtr->imxExaRec.exaDriverPtr = exaDriverPtr;

	/* Install our CloseScreen function so that it gets called. */
	fPtr->saveCloseScreenPtr = pScreen->CloseScreen;
	pScreen->CloseScreen = imxExaZ160CloseScreen;

#if IMX_EXA_ENABLE_HANDLES_PIXMAPS

	xf86DrvMsg(pScrn->scrnIndex, X_INFO,
		"Driver handles allocation of pixmaps\n");

	const unsigned long numAvailPixmapBytes =
		exaDriverPtr->memorySize - exaDriverPtr->offScreenBase;
	xf86DrvMsg(pScrn->scrnIndex, X_INFO,
		"Offscreen pixmap area of %luK bytes\n",
		numAvailPixmapBytes / 1024);

	/* Driver allocation of pixmaps will use the built-in */
	/* EXA offscreen memory manager. */
	imxExaOffscreenInit(pScreen);
#endif

	return TRUE;
}

