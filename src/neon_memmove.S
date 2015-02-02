/* neon-memmove.S
 Copyright (c) 2009-2010, Code Aurora Forum. All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:
     * Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.
     * Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.
     * Neither the name of Code Aurora nor the names of its contributors may
       be used to endorse or promote products derived from this software
       without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 POSSIBILITY OF SUCH DAMAGE.
  ***************************************************************************/

/***************************************************************************
 *  Neon memmove: Attempts to do a memmove with Neon registers if possible,
 *     Inputs:
 *        dest: The destination buffer
 *        src: The source buffer
 *        n: The size of the buffer to transfer
 *     Outputs:
 *
 ***************************************************************************/

/*
 * General note:
 * The original code that was compiled for rvct used PUSH/POP and VPUSH/VPOP
 * However, it looks like the 2006 CodeSourcery Assembler has issues generating
 * the correct object code for VPOP, resulting in horrific stack crashes.
 * As a result, I've temporarily move PUSH->STMDB, POP->LDMIA, VPUSH->VSTMDB,
 * and VPOP->VLDMIA.  We can revert this back once we update our toolchain.
 *
 * Also, VSHL swaps the source register and the shift-amount register
 * around in 2006-q3.  I've coded this incorrectly so it turns out correct
 * in the object code, but we'll need to undo that later...
 */
	.code 32
	.align 4
	.globl neon_memmove
	.func

neon_memmove:
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	stmdb		sp!, {r0}
#else
	push		{r0}
#endif

	/*
	 * The requirements for memmove state that the function should
	 * operate as if data were being copied from the source to a
	 * buffer, then to the destination.  This is to allow a user
	 * to copy data from a source and target that overlap.
	 *
	 * We can't just do byte copies front-to-back automatically, since
	 * there's a good chance we may have an overlap (why else would someone
	 * intentionally use memmove then?).
	 *
	 * We'll break this into two parts.  Front-to-back, or back-to-front
	 * copies.
	 */
neon_memmove_cmf:
	cmp		r0, r1
	blt		neon_front_to_back_copy
	bgt		neon_back_to_front_copy
	b		neon_memmove_done

	/* #############################################################
	 * Front to Back copy
	 */
neon_front_to_back_copy:
	/*
	 * For small copies, just do a quick memcpy.  We can do this for
	 * front-to-back copies, aligned or unaligned, since we're only
	 * doing 1 byte at a time...
	 */
	cmp		r2, #4
	bgt		neon_f2b_gt4
	cmp		r2, #0
neon_f2b_smallcopy_loop:
	beq		neon_memmove_done
	ldrb		r12, [r1], #1
	subs		r2, r2, #1
	strb		r12, [r0], #1
	b		neon_f2b_smallcopy_loop
neon_f2b_gt4:
	/* Preload what we can...*/
	pld		[r0,#0]
	pld		[r1,#0]
	/* The window size is in r3. */
	sub		r3, r1, r0
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	stmdb		sp!, {r4-r6}
#else
	push		{r4-r6}
#endif

neon_f2b_check_align:
	/* Check alignment. */
	ands		r12, r0, #0x3
	beq		neon_f2b_source_align_check
	cmp		r12, #2
	ldrb		r4, [r1], #1
	ldrleb		r5, [r1], #1
	ldrltb		r6, [r1], #1
	rsb		r12, r12, #4
	sub		r2, r2, r12
	strb		r4, [r0], #1
	strleb		r5, [r0], #1
	strltb		r6, [r0], #1

neon_f2b_source_align_check:
	ands		r12, r1, #0x3
	bne		neon_f2b_nonaligned

neon_f2b_try_16_align:
	/* If we're >64, attempt to align on 16-bytes.  Smaller amounts
	 * don't seem to be worth handling. */
	cmp		r2, #64
	blt		neon_f2b_align_route
	/* This is where we try 16-byte alignment. */
	ands		r12, r0, #0xf
	beq		neon_f2b_align_route
	rsb		r12, r12, #16
neon_f2b_16_start:
	sub		r2, r2, r12
	lsrs		r5, r12, #2
neon_f2b_align_16_4:
	ldr		r4, [r1], #4
	subs		r5, r5, #1
	str		r4, [r0], #4
	bne		neon_f2b_align_16_4
neon_f2b_align_route:
	/* #############################################################
	 * Front to Back copy - aligned
	 */
	/*
	 * Note that we can't just route based on the size in r2.  If that's
	 * larger than the overlap window in r3, we could potentially
	 * (and likely!) destroy data we're copying.
	 */
	cmp		r2, r3
	movle		r12, r2
	movgt		r12, r3
	cmp		r12, #256
	bge		neon_f2b_copy_128_a
	cmp		r12, #64
	bge		neon_f2b_copy_32_a
	cmp		r12, #16
	bge		neon_f2b_copy_16_a
	cmp		r12, #8
	bge		neon_f2b_copy_8_a
	cmp		r12, #4
	bge		neon_f2b_copy_4_a
	b		neon_f2b_copy_1_a
neon_f2b_copy_128_a:
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	vstmdb		sp!, {q4-q7}
#else
	vpush		{q4-q7}
#endif
	mov		r12, r2, lsr #7
neon_f2b_copy_128_a_loop:
	vld1.32		{q0,q1}, [r1]!
	vld1.32		{q2,q3}, [r1]!
	vld1.32		{q4,q5}, [r1]!
	vld1.32		{q6,q7}, [r1]!
	pld		[r1, #0]
	pld		[r1, #128]
	vst1.32		{q0,q1}, [r0]!
	vst1.32		{q2,q3}, [r0]!
	vst1.32		{q4,q5}, [r0]!
	vst1.32		{q6,q7}, [r0]!
	subs		r12, r12, #1
	pld		[r0, #0]
	pld		[r0, #128]
	bne		neon_f2b_copy_128_a_loop
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	vldmia		sp!, {q4-q7}
#else
	vpop		{q4-q7}
#endif
	ands		r2, r2, #0x7f
	beq		neon_f2b_finish
	cmp		r2, #32
	bge		neon_f2b_copy_32_a
	b		neon_f2b_copy_finish_a
neon_f2b_copy_32_a:
	mov		r12, r2, lsr #5
neon_f2b_copy_32_a_loop:
	vld1.32		{q0,q1}, [r1]!
	subs		r12, r12, #1
	pld		[r1, #0]
	vst1.32		{q0,q1}, [r0]!
	bne		neon_f2b_copy_32_a_loop
	ands		r2, r2, #0x1f
	beq		neon_f2b_finish
neon_f2b_copy_finish_a:
neon_f2b_copy_16_a:
	movs		r12, r2, lsr #4
	beq		neon_f2b_copy_8_a
neon_f2b_copy_16_a_loop:
	vld1.32		{q0}, [r1]!
	subs		r12, r12, #1
	vst1.32		{q0}, [r0]!
	bne		neon_f2b_copy_16_a_loop
	ands		r2, r2, #0xf
	beq		neon_f2b_finish
neon_f2b_copy_8_a:
	cmp		r2, #8
	blt		neon_f2b_copy_4_a
	ldm		r1!, {r4-r5}
	subs		r2, r2, #8
	stm		r0!, {r4-r5}
neon_f2b_copy_4_a:
	cmp		r2, #4
	blt		neon_f2b_copy_1_a
	ldr		r4, [r1], #4
	subs		r2, r2, #4
	str		r4, [r0], #4
neon_f2b_copy_1_a:
	cmp		r2, #0
	beq		neon_f2b_finish
neon_f2b_copy_1_a_loop:
	ldrb		r12, [r1], #1
	subs		r2, r2, #1
	strb		r12, [r0], #1
	bne		neon_f2b_copy_1_a_loop
	b		neon_f2b_finish

	/* #############################################################
	 * Front to Back copy - unaligned
	 */
neon_f2b_nonaligned:
	/*
	 * For sizes < 8, does it really make sense to do the whole shift
	 * party?  Note that we DON'T want to call neon_f2b_copy_1_u,
	 * since we'll end up trying to pop r8-r11, and we DON'T want
	 * to do that...
	 */
	cmp		r2, #8
	ble		neon_f2b_copy_1_a

#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	stmdb		sp!, {r7-r9}
#else
	push		{r7-r9}
#endif
	cmp		r12, #2
	ldrb		r4, [r1], #1
	ldrleb		r5, [r1], #1
	ldrltb		r6, [r1], #1
	rsb		r8, r12, #4
	sub		r2, r2, r8
	lsl		r8, r8, #3
	orrle		r4, r4, r5, lsl #8
	orrlt		r4, r4, r6, lsl #16
	rsb		r9, r8, #32
	/*
	 * r4  = overflow bits
	 * r8 = # of bits we copied into the r4 register to align source.
	 * r9 = 32 - r8
	 * r12 = Index counter for each size, so we determine how many times
	 *       the given size will go into r2, then count down that # of
	 *       times in r12.
	 */
	cmp		r2, #64
	blt		neon_f2b_unaligned_route
	ands		r12, r0, #0xf
	beq		neon_f2b_unaligned_route
	cmp		r3, #4
	blt		neon_f2b_unaligned_route
	rsb		r12, r12, #16
neon_f2b_16_start_u:
	sub		r2, r2, r12
	lsrs		r6, r12, #2
neon_f2b_align_16_4_u:
	ldr		r5, [r1], #4
	subs		r6, r6, #1
	orr		r4, r4, r5, lsl r8
	str		r4, [r0], #4
	mov		r4, r5, lsr r9
	bne		neon_f2b_align_16_4_u
neon_f2b_unaligned_route:
	cmp		r2, r3
	movle		r12, r2
	movgt		r12, r3
	cmp		r12, #256
	bge		neon_f2b_copy_64_u
	cmp		r12, #64
	bge		neon_f2b_copy_32_u
	cmp		r12, #16
	bge		neon_f2b_copy_16_u
	cmp		r12, #8
	bge		neon_f2b_copy_8_u
	cmp		r12, #4
	bge		neon_f2b_copy_4_u
	b		neon_f2b_last_bits_u
neon_f2b_copy_64_u:
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	vstmdb		sp!, {q4}
	vstmdb		sp!, {q5-q8}
#else
	vpush		{q4}
	vpush		{q5-q8}
#endif
	vdup.u32	q8, r8
	mov		r12, r2, lsr #6
	and		r2, r2, #0x3f
neon_f2b_copy_64_u_loop:
	vld1.32		{q4, q5}, [r1]!
	vld1.32		{q6, q7}, [r1]!
	lsls		r5, r8, #28
	bcc		neon_f2b_copy_64_u_b8
	bpl		neon_f2b_copy_64_u_b16
	vshr.u64	q0, q4, #40
	vshr.u64	q1, q5, #40
	vshr.u64	q2, q6, #40
	vshr.u64	q3, q7, #40
	b		neon_f2b_copy_64_unify
neon_f2b_copy_64_u_b8:
	vshr.u64	q0, q4, #56
	vshr.u64	q1, q5, #56
	vshr.u64	q2, q6, #56
	vshr.u64	q3, q7, #56
	b		neon_f2b_copy_64_unify
neon_f2b_copy_64_u_b16:
	vshr.u64	q0, q4, #48
	vshr.u64	q1, q5, #48
	vshr.u64	q2, q6, #48
	vshr.u64	q3, q7, #48
neon_f2b_copy_64_unify:
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	vshl.u64	q4, q8, q4
	vshl.u64	q5, q8, q5
	vshl.u64	q6, q8, q6
	vshl.u64	q7, q8, q7
#else
	vshl.u64	q4, q4, q8
	vshl.u64	q5, q5, q8
	vshl.u64	q6, q6, q8
	vshl.u64	q7, q7, q8
#endif
	vmov		r5, s14
	vorr		d9, d9, d0
	vmov		s14, r4
	vorr		d10, d10, d1
	vorr		d11, d11, d2
	vorr		d12, d12, d3
	vorr		d13, d13, d4
	vorr		d14, d14, d5
	vorr		d15, d15, d6
	vorr		d8, d8, d7
	subs		r12, r12, #1
	pld		[r1, #0]
	pld		[r1, #128]
	mov		r4, r5
	vst1.32		{q4, q5}, [r0]!
	vst1.32		{q6, q7}, [r0]!
	pld		[r0, #0]
	pld		[r0, #128]
	bne		neon_f2b_copy_64_u_loop
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	vldmia		sp!, {q5-q8}
	vldmia		sp!, {q4}
#else
	vpop		{q5-q8}
	vpop		{q4}
#endif
	cmp		r2, #32
	bge		neon_f2b_copy_32_u
	b		neon_f2b_copy_finish_u
neon_f2b_copy_32_u:
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	vstmdb		sp!, {q4}
#else
	vpush		{q4}
#endif
	vdup.u32	q4, r8
	mov		r12, r2, lsr #5
	and		r2, r2, #0x1f
neon_f2b_copy_32_u_loop:
	vld1.32		{q0, q1}, [r1]!
	lsls		r5, r8, #28
	bcc		neon_f2b_copy_32_u_b8
	bpl		neon_f2b_copy_32_u_b16
	vshr.u64	q2, q0, #40
	vshr.u64	q3, q1, #40
	b		neon_f2b_copy_32_unify
neon_f2b_copy_32_u_b8:
	vshr.u64	q2, q0, #56
	vshr.u64	q3, q1, #56
	b		neon_f2b_copy_32_unify
neon_f2b_copy_32_u_b16:
	vshr.u64	q2, q0, #48
	vshr.u64	q3, q1, #48
neon_f2b_copy_32_unify:
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	vshl.u64	q0, q4, q0
	vshl.u64	q1, q4, q1
#else
	vshl.u64	q0, q0, q4
	vshl.u64	q1, q1, q4
#endif
	vmov		r5, s14
	vorr		d1, d1, d4
	vmov		s14, r4
	vorr		d2, d2, d5
	vorr		d3, d3, d6
	vorr		d0, d0, d7
	subs		r12, r12, #1
	pld		[r1, #0]
	mov		r4, r5
	vst1.32		{q0, q1}, [r0]!
	bne		neon_f2b_copy_32_u_loop
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	vldmia		sp!, {q4}
#else
	vpop		{q4}
#endif
neon_f2b_copy_finish_u:
neon_f2b_copy_16_u:
	movs		r12, r2, lsr #4
	beq		neon_f2b_copy_8_u
	vdup.u32	q2, r8
	and		r2, r2, #0xf
neon_f2b_copy_16_u_loop:
	vld1.32		{q0}, [r1]!
	lsls		r5, r8, #28
	bcc		neon_f2b_copy_16_u_b8
	bpl		neon_f2b_copy_16_u_b16
	vshr.u64	q1, q0, #40
	b		neon_f2b_copy_16_unify
neon_f2b_copy_16_u_b8:
	vshr.u64	q1, q0, #56
	b		neon_f2b_copy_16_unify
neon_f2b_copy_16_u_b16:
	vshr.u64	q1, q0, #48
neon_f2b_copy_16_unify:
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	vshl.u64	q0, q2, q0
#else
	vshl.u64	q0, q0, q2
#endif
	vmov		r5, s6
	vorr		d1, d1, d2
	vmov		s6, r4
	vorr		d0, d0, d3
	subs		r12, r12, #1
	mov		r4, r5
	vst1.32		{q0}, [r0]!
	bne		neon_f2b_copy_16_u_loop
neon_f2b_copy_8_u:
	cmp		r2, #8
	blt		neon_f2b_copy_4_u
	ldm		r1!, {r6-r7}
	subs		r2, r2, #8
	orr		r4, r4, r6, lsl r8
	mov		r5, r6, lsr r9
	orr		r5, r5, r7, lsl r8
	stm		r0!, {r4-r5}
	mov		r4, r7, lsr r9
neon_f2b_copy_4_u:
	cmp		r2, #4
	blt		neon_f2b_last_bits_u
	ldr		r5, [r1], #4
	subs		r2, r2, #4
	orr		r4, r4, r5, lsl r8
	str		r4, [r0], #4
	mov		r4, r5, lsr r9
neon_f2b_last_bits_u:
	lsr		r8, r8, #0x3
neon_f2b_last_bits_u_loop:
	strb		r4, [r0], #1
	subs		r8, r8, #1
	lsr		r4, r4, #8
	bne		neon_f2b_last_bits_u_loop
neon_f2b_copy_1_u:
	cmp		r2, #0
	beq		neon_f2b_finish_u
neon_f2b_copy_1_u_loop:
	ldrb		r12, [r1], #1
	subs		r2, r2, #1
	strb		r12, [r0], #1
	bne		neon_f2b_copy_1_u_loop
neon_f2b_finish_u:
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	ldmia		sp!, {r7-r9}
#else
	pop		{r7-r9}
#endif
	/* #############################################################
	 * Front to Back copy - finish
	 */
neon_f2b_finish:
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	ldmia		sp!, {r4-r6}
#else
	pop		{r4-r6}
#endif
	b		neon_memmove_done

	/* #############################################################
	 * Back to Front copy
	 */
neon_back_to_front_copy:
	/*
	 * Here, we'll want to shift to the end of the buffers.  This
	 * actually points us one past where we need to go, but since
	 * we'll pre-decrement throughout, this will be fine.
	 */
	add		r0, r0, r2
	add		r1, r1, r2
	cmp		r2, #4
	bgt		neon_b2f_gt4
	cmp		r2, #0
neon_b2f_smallcopy_loop:
	beq		neon_memmove_done
	ldrb		r12, [r1, #-1]!
	subs		r2, r2, #1
	strb		r12, [r0, #-1]!
	b		neon_b2f_smallcopy_loop
neon_b2f_gt4:
	pld		[r0, #0]
	pld		[r1, #0]
	/*
	 * The minimum of the overlap window size and the copy size
	 * is in r3.
	 */
	sub		r3, r0, r1
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	stmdb		sp!, {r4-r5}
#else
	push		{r4-r5}
#endif

	/*
	 * Check alignment.  Since we'll pre-decrement as we step thru, we'll
	 * need to make sure we're on word-alignment.
	 */
neon_b2f_check_align:
	ands		r12, r0, #0x3
	beq		neon_b2f_source_align_check
	sub		r2, r2, r12
neon_b2f_shift_align:
	ldrb		r4, [r1, #-1]!
	subs		r12, r12, #1
	strb		r4, [r0, #-1]!
	bne		neon_b2f_shift_align
neon_b2f_source_align_check:
	ands		r4, r1, #0x3
	bne		neon_b2f_nonaligned

neon_b2f_try_16_align:
	/* If we're >64, attempt to align on 16-bytes.  Smaller amounts
	 * don't seem to be worth handling. */
	cmp		r2, #64
	blt		neon_b2f_align_route
	ands		r12, r0, #0xf
	beq		neon_b2f_align_route
	/* In this case, r12 has the number of bytes to roll backward. */
neon_b2f_16_start:
	sub		r2, r2, r12
	lsrs		r5, r12, #2
neon_b2f_align_16_4:
	ldr		r4, [r1, #-4]!
	subs		r5, r5, #1
	str		r4, [r0, #-4]!
	bne		neon_b2f_align_16_4
neon_b2f_align_route:
	/*
	 * #############################################################
	 * Back to Front copy - aligned
	 */
	cmp		r2, r3
	movle		r12, r2
	movgt		r12, r3
	cmp		r12, #256
	bge		neon_b2f_copy_128_a
	cmp		r12, #64
	bge		neon_b2f_copy_32_a
	cmp		r12, #8
	bge		neon_b2f_copy_8_a
	cmp		r12, #4
	bge		neon_b2f_copy_4_a
	b		neon_b2f_copy_1_a
neon_b2f_copy_128_a:
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	vstmdb		sp!, {q4-q7}
#else
	vpush		{q4-q7}
#endif
	movs		r12, r2, lsr #7
	/*
	 * This irks me.  There MUST be a better way to read these in and
	 * scan the register backward instead of making it go forward.  Then
	 * we need to do two subtractions...
	 */
neon_b2f_copy_128_a_loop:
	sub		r1, r1, #128
	sub		r0, r0, #128
	vld1.32		{q0, q1}, [r1]!
	vld1.32		{q2, q3}, [r1]!
	vld1.32		{q4, q5}, [r1]!
	vld1.32		{q6, q7}, [r1]!
	pld		[r1, #-128]
	pld		[r1, #-256]
	vst1.32		{q0, q1}, [r0]!
	vst1.32		{q2, q3}, [r0]!
	vst1.32		{q4, q5}, [r0]!
	vst1.32		{q6, q7}, [r0]!
	subs		r12, r12, #1
	pld		[r0, #-128]
	pld		[r0, #-256]
	sub		r1, r1, #128
	sub		r0, r0, #128
	bne		neon_b2f_copy_128_a_loop
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	vldmia		sp!, {q4-q7}
#else
	vpop		{q4-q7}
#endif
	ands		r2, r2, #0x7f
	beq		neon_b2f_finish
	cmp		r2, #32
	bge		neon_b2f_copy_32_a
	b		neon_b2f_copy_finish_a
neon_b2f_copy_32_a:
	mov		r12, r2, lsr #5
neon_b2f_copy_32_a_loop:
	sub		r1, r1, #32
	sub		r0, r0, #32
	vld1.32		{q0,q1}, [r1]
	subs		r12, r12, #1
	vst1.32		{q0,q1}, [r0]
	pld		[r1, #0]
	bne		neon_b2f_copy_32_a_loop
	ands		r2, r2, #0x1f
	beq		neon_b2f_finish
neon_b2f_copy_finish_a:
neon_b2f_copy_8_a:
	movs		r12, r2, lsr #0x3
	beq		neon_b2f_copy_4_a
neon_b2f_copy_8_a_loop:
	ldmdb		r1!, {r4-r5}
	subs		r12, r12, #1
	stmdb		r0!, {r4-r5}
	bne		neon_b2f_copy_8_a_loop
	and		r2, r2, #0x7
neon_b2f_copy_4_a:
	movs		r12, r2, lsr #0x2
	beq		neon_b2f_copy_1_a
	and		r2, r2, #0x3
neon_b2f_copy_4_a_loop:
	ldr		r4, [r1, #-4]!
	subs		r12, r12, #1
	str		r4, [r0, #-4]!
	bne		neon_b2f_copy_4_a_loop
neon_b2f_copy_1_a:
	cmp		r2, #0
	beq		neon_b2f_finish
neon_b2f_copy_1_a_loop:
	ldrb		r12, [r1, #-1]!
	subs		r2, r2, #1
	strb		r12, [r0, #-1]!
	bne		neon_b2f_copy_1_a_loop

	/* #############################################################
	 * Back to Front copy - unaligned
	 */
neon_b2f_nonaligned:
	/*
	 * For sizes < 8, does it really make sense to do the whole shift
	 * party?
	 */
	cmp		r2, #8
	ble		neon_b2f_copy_1_a
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	stmdb		sp!, {r6-r11}
#else
	push		{r6-r11}
#endif
	/*
	 * r3 = max window size
	 * r4 = overflow bytes
	 * r5 = bytes we're reading into
	 * r6 = # bytes we're off.
	 * r10 = copy of r6
	 */
	and		r6, r1, #0x3
	eor		r4, r4, r4
	mov		r10, r6
neon_b2f_realign:
	ldrb		r5, [r1, #-1]!
	subs		r6, r6, #1
	orr		r4, r5, r4, lsl #8
	bne		neon_b2f_realign
	/*
	 * r10 = # of bits we copied into the r4 register to align source.
	 * r11 = 32 - r10
	 * r12 = Index counter for each size, so we determine how many times
	 *       the given size will go into r2, then count down that # of
	 *       times in r12.
	 */
	sub		r2, r2, r10
	lsl		r10, r10, #0x3
	rsb		r11, r10, #32

	cmp		r2, r3
	movle		r12, r2
	movgt		r12, r3
	cmp		r12, #256
	bge		neon_b2f_copy_64_u
	cmp		r12, #64
	bge		neon_b2f_copy_32_u
	cmp		r12, #8
	bge		neon_b2f_copy_8_u
	cmp		r12, #4
	bge		neon_b2f_copy_4_u
	b		neon_b2f_last_bits_u
neon_b2f_copy_64_u:
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	vstmdb		sp!, {q4,q5}
	vstmdb		sp!, {q6-q8}
#else
	vpush		{q4,q5}
	vpush		{q6-q8}
#endif
	add		r7, r11, #32
	movs		r12, r2, lsr #6
	vdup.u32	q8, r7
neon_b2f_copy_64_u_loop:
	sub		r1, r1, #64
	sub		r0, r0, #64
	vld1.32		{q0, q1}, [r1]!
	vld1.32		{q2, q3}, [r1]
	sub		r1, r1, #32
	vmov		q4, q0
	vmov		q5, q1
	vmov		q6, q2
	vmov		q7, q3
	vmov		r5, s0
	mov		r4, r4, lsl r11
	lsls		r6, r10, #28
	bcc		neon_b2f_copy_64_u_b8
	bpl		neon_b2f_copy_64_u_b16
	vshr.u64	q0, q0, #24
	vshr.u64	q1, q1, #24
	vshr.u64	q2, q2, #24
	vshr.u64	q3, q3, #24
	b		neon_b2f_copy_64_unify
neon_b2f_copy_64_u_b8:
	vshr.u64	q0, q0, #8
	vshr.u64	q1, q1, #8
	vshr.u64	q2, q2, #8
	vshr.u64	q3, q3, #8
	b		neon_b2f_copy_64_unify
neon_b2f_copy_64_u_b16:
	vshr.u64	q0, q0, #16
	vshr.u64	q1, q1, #16
	vshr.u64	q2, q2, #16
	vshr.u64	q3, q3, #16
neon_b2f_copy_64_unify:
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	vshl.u64	q4, q8, q4
	vshl.u64	q5, q8, q5
	vshl.u64	q6, q8, q6
	vshl.u64	q7, q8, q7
#else
	vshl.u64	q4, q4, q8
	vshl.u64	q5, q5, q8
	vshl.u64	q6, q6, q8
	vshl.u64	q7, q7, q8
#endif
	vmov		s17, r4
	vorr		d7, d7, d8
	vorr		d6, d6, d15
	vorr		d5, d5, d14
	vorr		d4, d4, d13
	vorr		d3, d3, d12
	vorr		d2, d2, d11
	vorr		d1, d1, d10
	vorr		d0, d0, d9
	mov		r4, r5, lsl r11
	subs		r12, r12, #1
	lsr		r4, r4, r11
	vst1.32		{q0, q1}, [r0]!
	vst1.32		{q2, q3}, [r0]
	pld		[r1, #0]
	sub		r0, r0, #32
	bne		neon_b2f_copy_64_u_loop
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	vldmia		sp!, {q6-q8}
	vldmia		sp!, {q4,q5}
#else
	vpop		{q6-q8}
	vpop		{q4,q5}
#endif
	ands		r2, r2, #0x3f
	cmp		r2, #32
	bge		neon_b2f_copy_32_u
	b		neon_b2f_copy_finish_u
neon_b2f_copy_32_u:
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	vstmdb		sp!, {q4}
#else
	vpush		{q4}
#endif
	add		r7, r11, #32
	movs		r12, r2, lsr #5
	vdup.u32	q4, r7
	and		r2, r2, #0x1f
neon_b2f_copy_32_u_loop:
	sub		r1, r1, #32
	sub		r0, r0, #32
	vld1.32		{q0, q1}, [r1]
	vmov		q2, q0
	vmov		q3, q1
	vmov		r5, s0
	mov		r4, r4, lsl r11
	lsls		r6, r10, #28
	bcc		neon_b2f_copy_32_u_b8
	bpl		neon_b2f_copy_32_u_b16
	vshr.u64	q0, q0, #24
	vshr.u64	q1, q1, #24
	b		neon_b2f_copy_32_unify
neon_b2f_copy_32_u_b8:
	vshr.u64	q0, q0, #8
	vshr.u64	q1, q1, #8
	b		neon_b2f_copy_32_unify
neon_b2f_copy_32_u_b16:
	vshr.u64	q0, q0, #16
	vshr.u64	q1, q1, #16
neon_b2f_copy_32_unify:
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	vshl.u64	q2, q4, q2
	vshl.u64	q3, q4, q3
#else
	vshl.u64	q2, q2, q4
	vshl.u64	q3, q3, q4
#endif
	vmov		s9, r4
	vorr		d3, d3, d4
	vorr		d2, d2, d7
	vorr		d1, d1, d6
	vorr		d0, d0, d5
	mov		r4, r5, lsl r11
	subs		r12, r12, #1
	lsr		r4, r4, r11
	vst1.32		{q0, q1}, [r0]
	pld		[r1, #0]
	bne		neon_b2f_copy_32_u_loop
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	vldmia		sp!, {q4}
#else
	vpop		{q4}
#endif
neon_b2f_copy_finish_u:
neon_b2f_copy_8_u:
	movs		r12, r2, lsr #0x3
	beq		neon_b2f_copy_4_u
	mov		r5, r4, lsl r11
neon_b2f_copy_8_u_loop:
	ldmdb		r1!, {r6-r7}
	subs		r12, r12, #1
	orr		r5, r5, r7, lsr r10
	mov		r4, r7, lsl r11
	orr		r4, r4, r6, lsr r10
	stmdb		r0!, {r4-r5}
	mov		r4, r6, lsl r11
	lsr		r4, r4, r11
	mov		r5, r4, lsl r11
	bne		neon_b2f_copy_8_u_loop
	ands		r2, r2, #0x7
neon_b2f_copy_4_u:
	movs		r12, r2, lsr #0x2
	beq		neon_b2f_last_bits_u
	mov		r5, r4, lsl r11
neon_b2f_copy_4_u_loop:
	ldr		r6, [r1, #-4]!
	subs		r12, r12, #1
	orr		r5, r5, r6, lsr r10
	str		r5, [r0, #-4]!
	mov		r4, r6, lsl r11
	lsr		r4, r4, r11
	mov		r5, r4, lsl r11
	bne		neon_b2f_copy_4_u_loop
	and		r2, r2, #0x3
neon_b2f_last_bits_u:
neon_b2f_last_bits_u_loop:
	subs		r10, r10, #8
	mov		r5, r4, lsr r10
	strb		r5, [r0, #-1]!
	bne		neon_b2f_last_bits_u_loop
neon_b2f_copy_1_u:
	cmp		r2, #0
	beq		neon_b2f_finish_u
neon_b2f_copy_1_u_loop:
	ldrb		r12, [r1, #-1]!
	subs		r2, r2, #1
	strb		r12, [r0, #-1]!
	bne		neon_b2f_copy_1_u_loop
neon_b2f_finish_u:
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	ldmia		sp!, {r6-r11}
#else
	pop		{r6-r11}
#endif

neon_b2f_finish:
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	ldmia		sp!, {r4-r5}
#else
	pop		{r4-r5}
#endif

neon_memmove_done:
#if defined __GNUC__ && (4 == __GNUC__ && 1 == __GNUC_MINOR__ && 1 == __GNUC_PATCHLEVEL__)
	ldmia		sp!, {r0}
#else
	pop		{r0}
#endif
	bx		lr

	.endfunc
	.end
