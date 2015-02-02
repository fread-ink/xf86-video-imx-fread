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

#ifndef __IMX_ACCEL_H__
#define __IMX_ACCEL_H__

typedef void (*imx_copy_sw_no_overlap_func)(
	unsigned char* __restrict__ pBufferDst,
	unsigned char* __restrict__ pBufferSrc,
	int width,
	int height,
	int pitchDst,
	int pitchSrc);

void imx_copy_sw_no_overlap_8(
	unsigned char* __restrict__ pBufferDst,
	unsigned char* __restrict__ pBufferSrc,
	int width,
	int height,
	int pitchDst,
	int pitchSrc);

void imx_copy_sw_no_overlap_16(
	unsigned char* __restrict__ pBufferDst,
	unsigned char* __restrict__ pBufferSrc,
	int width,
	int height,
	int pitchDst,
	int pitchSrc);

void imx_copy_sw_no_overlap_32(
	unsigned char* __restrict__ pBufferDst,
	unsigned char* __restrict__ pBufferSrc,
	int width,
	int height,
	int pitchDst,
	int pitchSrc);

#endif
