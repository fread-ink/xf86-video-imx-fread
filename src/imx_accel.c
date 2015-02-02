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
#include <xorg-server.h>

#include <stdint.h>
#include "xf86.h"
#include "imx_accel.h"

extern void* neon_memcpy(void* dest, const void* source, unsigned int numBytes);
extern void* neon_memmove(void* dest, const void* source, unsigned int numBytes);

void
imx_copy_sw_no_overlap_8(
	unsigned char* __restrict__ pBufferDst,
	unsigned char* __restrict__ pBufferSrc,
	int width,
	int height,
	int pitchDst,
	int pitchSrc)
{
	/* 1 pixel only */
	if ((1 == width) && (1 == height)) {

		*pBufferDst = *pBufferSrc;

	/* Width matches pitch for src/dst, then copy entire block */
	} else if ((pitchDst == width) && (pitchSrc == width)) {

		neon_memcpy(pBufferDst, pBufferSrc, width * height);

	/* 1 pixel per row */
	} else if (1 == width) {

		while (height-- > 0) {

			*pBufferDst = *pBufferSrc;
			pBufferDst += pitchDst;
			pBufferSrc += pitchSrc;
		}

	/* Use neon_memcpy when width big enough */
	} else if (128 <= width) {

		while (height-- > 0) {

			neon_memcpy(pBufferDst, pBufferSrc, width);
			pBufferDst += pitchDst;
			pBufferSrc += pitchSrc;
		}

	/* Fallback to use memcpy. */
	} else {

		while (height-- > 0) {

			memcpy(pBufferDst, pBufferSrc, width);
			pBufferDst += pitchDst;
			pBufferSrc += pitchSrc;
		}
	}
}

void
imx_copy_sw_no_overlap_16(
	unsigned char* __restrict__ pBufferDst,
	unsigned char* __restrict__ pBufferSrc,
	int width,
	int height,
	int pitchDst,
	int pitchSrc)
{
	/* 1 pixel only */
	if ((1 == width) && (1 == height)) {

		*(uint16_t*)pBufferDst = *(uint16_t*)pBufferSrc;
		return;
	}

	/* How many bytes to copy per row of rectangle? */
	int rowCopyBytes = width << 1;

	/* Width matches pitch for src/dst, then copy entire block */
	if ((pitchDst == rowCopyBytes) && (pitchSrc == rowCopyBytes)) {

		neon_memcpy(pBufferDst, pBufferSrc, rowCopyBytes * height);

	/* 1 pixel per row */
	} else if (1 == width) {

		while (height-- > 0) {

			*(uint16_t*)pBufferDst = *(uint16_t*)pBufferSrc;
			pBufferDst += pitchDst;
			pBufferSrc += pitchSrc;
		}

	/* Use neon_memcpy when width big enough */
	} else if (64 <= width) {

		while (height-- > 0) {

			neon_memcpy(pBufferDst, pBufferSrc, rowCopyBytes);
			pBufferDst += pitchDst;
			pBufferSrc += pitchSrc;
		}

	/* Fallback to use memcpy. */
	} else {

		while (height-- > 0) {

			memcpy(pBufferDst, pBufferSrc, rowCopyBytes);
			pBufferDst += pitchDst;
			pBufferSrc += pitchSrc;
		}
	}
}

void
imx_copy_sw_no_overlap_32(
	unsigned char* __restrict__ pBufferDst,
	unsigned char* __restrict__ pBufferSrc,
	int width,
	int height,
	int pitchDst,
	int pitchSrc)
{
	/* 1 pixel only */
	if ((1 == width) && (1 == height)) {

		*(uint32_t*)pBufferDst = *(uint32_t*)pBufferSrc;
		return;
	}
	/* How many bytes to copy per row of rectangle? */
	int rowCopyBytes = width << 2;

	/* Width matches pitch for src/dst, then copy entire block */
	if ((pitchDst == rowCopyBytes) && (pitchSrc == rowCopyBytes)) {

		neon_memcpy(pBufferDst, pBufferSrc, rowCopyBytes * height);

	/* 1 pixel per row */
	} else if (1 == width) {

		while (height-- > 0) {

			*(uint32_t*)pBufferDst = *(uint32_t*)pBufferSrc;
			pBufferDst += pitchDst;
			pBufferSrc += pitchSrc;
		}

	/* Use neon_memcpy when width big enough */
	} else if (32 <= width) {

		while (height-- > 0) {

			neon_memcpy(pBufferDst, pBufferSrc, rowCopyBytes);
			pBufferDst += pitchDst;
			pBufferSrc += pitchSrc;
		}

	/* Fallback to use memcpy. */
	} else {

		while (height-- > 0) {

			memcpy(pBufferDst, pBufferSrc, rowCopyBytes);
			pBufferDst += pitchDst;
			pBufferSrc += pitchSrc;
		}
	}
}
