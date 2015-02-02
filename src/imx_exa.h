/*
 * Copyright (C) 2011 Freescale Semiconductor, Inc.  All Rights Reserved.
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

#ifndef __IMX_EXA_H__
#define __IMX_EXA_H__

#include "xf86.h"
#include "exa.h"


/* Macro converts the EXA_VERSION_* definitions for major, minor, and */
/* release into a single integer value that can be compared. */
#define	IMX_EXA_VERSION(maj,min,rel)	(((maj)*0x010000)|((min)*0x0100)|(rel))

#define	IMX_EXA_VERSION_COMPILED	IMX_EXA_VERSION( \
						EXA_VERSION_MAJOR, \
						EXA_VERSION_MINOR, \
						EXA_VERSION_RELEASE)


/* -------------------------------------------------------------------- */
/* our private data, and two functions to allocate/free this            */

typedef struct {

	/* This must be pointer allocated by calling exaDriverAlloc */
	ExaDriverPtr			exaDriverPtr;

	/* For use by functions in imx_exa_offscreen.c */
	ExaOffscreenArea*		offScreenAreas;
	unsigned			offScreenCounter;
	unsigned			numOffscreenAvailable;

} ImxExaRec, *ImxExaPtr;

#define IMXEXAPTR(imxPtr) ((ImxExaPtr)((imxPtr)->exaDriverPrivate))

#endif
