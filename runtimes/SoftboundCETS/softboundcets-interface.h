//=== softboundcets-interface.h - header for public interface functions--*- C -*===//
// Copyright (c) 2015 Santosh Nagarakatte. All rights reserved.

// Developed by: Santosh Nagarakatte, Rutgers University
//               http://www.cs.rutgers.edu/~santosh.nagarakatte/softbound/

// Extended by: Andrew Anderson, Trinity College Dublin
//              https://scss.tcd.ie/~andersan

// The  SoftBoundCETS project had contributions from:
// Santosh Nagarakatte, Rutgers University,
// Milo M K Martin, University of Pennsylvania,
// Steve Zdancewic, University of Pennsylvania,
// Jianzhou Zhao, University of Pennsylvania


// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal with the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

//   1. Redistributions of source code must retain the above copyright notice,
//      this list of conditions and the following disclaimers.

//   2. Redistributions in binary form must reproduce the above copyright
//      notice, this list of conditions and the following disclaimers in the
//      documentation and/or other materials provided with the distribution.

//   3. Neither the names of its developers nor the names of its
//      contributors may be used to endorse or promote products
//      derived from this software without specific prior written
//      permission.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
// CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
// WITH THE SOFTWARE.
//===---------------------------------------------------------------------===//

#ifndef SOFTBOUNDCETS_INTERFACE_H
#define SOFTBOUNDCETS_INTERFACE_H

#include "softboundcets-internal.h"

__WEAK__ void
__softboundcets_spatial_load_dereference_check(void *base,
                                               void *bound,
                                               void *ptr,
                                               size_t size_of_type);

__WEAK__ void
__softboundcets_spatial_store_dereference_check(void *base,
                                                void *bound,
                                                void *ptr,
                                                size_t size_of_type);

__WEAK__ void
__softboundcets_temporal_load_dereference_check(void* pointer_lock,
                                                size_t key,
                                                void* base,
                                                void* bound);

__WEAK__ void
__softboundcets_temporal_store_dereference_check(void* pointer_lock,
                                                 size_t key,
                                                 void* base,
                                                 void* bound);

__WEAK__ void*
__softboundcets_get_global_lock();

__WEAK__ void
__softboundcets_introspect_metadata(void* ptr,
                                    void* base,
                                    void* bound,
                                    int arg_no);

__WEAK__ void
__softboundcets_copy_metadata(void* dest,
                              void* from,
                              size_t size);

/* Shadow stack routines */

__WEAK__ void
__softboundcets_allocate_shadow_stack_space(int num_pointer_args);

__WEAK__ void
__softboundcets_deallocate_shadow_stack_space();

__WEAK__ void
__softboundcets_store_base_shadow_stack(void* base,
                                        int arg_no);

__WEAK__ void
__softboundcets_store_bound_shadow_stack(void* bound,
                                         int arg_no);

__WEAK__ void*
__softboundcets_load_base_shadow_stack(int arg_no);

__WEAK__ void*
__softboundcets_load_bound_shadow_stack(int arg_no);

__WEAK__ size_t
__softboundcets_load_key_shadow_stack(int arg_no);

__WEAK__ void*
__softboundcets_load_lock_shadow_stack(int arg_no);

__WEAK__ void
__softboundcets_store_key_shadow_stack(size_t key,
                                       int arg_no);

__WEAK__ void
__softboundcets_store_lock_shadow_stack(void* lock,
                                        int arg_no);

__WEAK__ void
__softboundcets_stack_memory_allocation(void** ptr_lock,
                                        size_t* ptr_key);

__WEAK__ void
__softboundcets_stack_memory_deallocation(size_t ptr_key);

/* Metadata routines */

__WEAK__ void*
__softboundcets_metadata_map(void* addr_of_ptr);

__WEAK__ void
__softboundcets_metadata_load(void* addr_of_ptr,
                              void** base,
                              void** bound,
                              size_t* key,
                              void** lock);

__WEAK__ void
__softboundcets_metadata_store(void* addr_of_ptr,
                               void* base,
                               void* bound,
                               size_t key,
                               void* lock);

__WEAK__ void*
__softboundcets_metadata_load_base(void* address);

__WEAK__ void*
__softboundcets_metadata_load_bound(void* address);

__WEAK__ size_t
__softboundcets_metadata_load_key(void* address);

__WEAK__ void*
__softboundcets_metadata_load_lock(void* address);

__WEAK__ void
__softboundcets_metadata_load_vector(void* addr_of_ptr,
                                     void** base,
                                     void** bound,
                                     size_t* key,
                                     void** lock,
                                     int index);

__WEAK__ void
__softboundcets_metadata_store_vector(void* addr_of_ptr,
                                      void* base,
                                      void* bound,
                                      size_t key,
                                      void* lock,
                                      int index);

/* Miscellaneous routines */

__WEAK__ void
__softboundcets_spatial_call_dereference_check(void* base,
                                               void* bound,
                                               void* ptr);

__WEAK__ void
__softboundcets_memcopy_check(void* dest, void* src, size_t size,
                              void* dest_base, void* dest_bound,
                              void* src_base, void* src_bound,
                              size_t dest_key, void* dest_lock,
                              size_t src_key, void* src_lock);

__WEAK__ void
__softboundcets_memset_check(void* dest, size_t size,
                             void* dest_base, void* dest_bound,
                             size_t dest_key, void* dest_lock);

#endif // SOFTBOUNDCETS_INTERFACE_H
