//=== softboundcets-interface.c - implementation of public interface functions--*- C -*===//
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

#include "softboundcets-internal.h"
#include "softboundcets-interface.h"

__WEAK_INLINE__ void
__softboundcets_spatial_load_dereference_check(void *base,
                                               void *bound,
                                               void *ptr,
                                               size_t size_of_type) {
  if ((ptr < base) || ((void*)((char*) ptr + size_of_type) > bound)) {
    __softboundcets_printf("[spatial_load_dereference_check] base=%p, bound=%p, ptr=%p\n",
                           base, bound, ptr);
    if (ptr < base) {
      __softboundcets_abort_reason("read through pointer out of bounds below object base address");
    }
    if ((void*)((char*)ptr + size_of_type) > bound) {
      __softboundcets_abort_reason("read through pointer out of bounds above object bound address");
    }
  }

#if defined(SOFTBOUNDCETS_DEBUG)
  __softboundcets_printf("[spatial_load_dereference_check] base=%p, bound=%p, ptr=%p\n",
                         base, bound, ptr);
#endif
  return;
}

__WEAK_INLINE__ void
__softboundcets_spatial_store_dereference_check(void *base,
                                                void *bound,
                                                void *ptr,
                                                size_t size_of_type) {
  if ((ptr < base) || ((void*)((char*)ptr + size_of_type) > bound)) {
    __softboundcets_printf("[spatial_store_dereference_check] base=%p, bound=%p, ptr=%p, size_of_type=%zx, ptr+size=%p\n",
                           base, bound, ptr, size_of_type, (char*)ptr+size_of_type);
    if (ptr < base) {
      __softboundcets_abort_reason("write through pointer out of bounds below object base address");
    }
    if ((void*)((char*)ptr + size_of_type) > bound) {
      __softboundcets_abort_reason("write through pointer out of bounds above object bound address");
    }
  }

#if defined(SOFTBOUNDCETS_DEBUG)
  __softboundcets_printf("[spatial_store_dereference_check] base=%p, bound=%p, ptr=%p, size_of_type=%zx, ptr+size=%p\n",
                         base, bound, ptr, size_of_type, (char*)ptr+size_of_type);
#endif
  return;
}

__WEAK_INLINE__ void
__softboundcets_temporal_load_dereference_check(void* pointer_lock,
                                                size_t key) {
  if (pointer_lock != NULL) {
    size_t temp = *((size_t*)pointer_lock);

    if(temp != key) {
      __softboundcets_printf("[temporal_load_dereference_check] Key mismatch key=%zx, *lock=%zx, next_ptr=%zx\n",
                             key, temp, __softboundcets_lock_next_location);
      __softboundcets_abort_reason("read through pointer after free or return from function");
    }
  } else {
    __softboundcets_printf("[temporal_load_dereference_check] Invalid pointer, key=%zx, lock=%zx\n",
                             key, pointer_lock);
    __softboundcets_abort_reason("null pointer dereference");
  }

#if defined(SOFTBOUNDCETS_DEBUG)
__softboundcets_printf("[temporal_load_dereference_check] key=%zx, lock=%p, next_ptr=%zx\n",
                             key, pointer_lock, __softboundcets_lock_next_location);
#endif
  return;
}

__WEAK_INLINE__ void
__softboundcets_temporal_store_dereference_check(void* pointer_lock,
                                                 size_t key) {
  if (pointer_lock != NULL) {
    size_t temp = *((size_t*)pointer_lock);

    if(temp != key) {
      __softboundcets_printf("[temporal_store_dereference_check] Key mismatch, key=%zx, *lock=%zx\n",
                             key, temp);
      __softboundcets_abort_reason("write through pointer after free or return from function");
    }
  } else {
    __softboundcets_printf("[temporal_store_dereference_check] Invalid pointer, key=%zx, lock=%zx\n",
                             key, pointer_lock);
    __softboundcets_abort_reason("null pointer dereference");
  }

#if defined(SOFTBOUNDCETS_DEBUG)
__softboundcets_printf("[temporal_store_dereference_check] key=%zx, lock=%p\n",
                             key, pointer_lock);
#endif
  return;
}

__WEAK_INLINE__ void
__softboundcets_destroy_stack_key(size_t key){
  __softboundcets_stack_key_table_ptr--;

  if (__softboundcets_stack_key_table_ptr == NULL) {
    __softboundcets_printf("[destroy_stack_key] stack key table pointer is null\n");
    __softboundcets_abort();
  }

  size_t *lock = (size_t *) __softboundcets_stack_key_table_ptr;

  if (*lock != key) {
    __softboundcets_printf("[destroy_stack_key] destroying stack key %zx in function with stack key %zx\n", key, *lock);
    __softboundcets_abort_reason("control flow anomaly");
  }

  *(lock) = 0;

#if defined(SOFTBOUNDCETS_DEBUG)
  __softboundcets_printf("[destroy_stack_key] key=%zx\n", key);
#endif
  return;
}

__WEAK_INLINE__ void
__softboundcets_heap_allocation(void* ptr, void** ptr_lock, size_t* ptr_key){
  size_t temp_id;

#if defined(SOFTBOUNDCETS_BENCHMARKING_MODE)
  *((size_t**) ptr_lock) = __softboundcets_global_lock;
  temp_id = **((size_t**) ptr_lock);
  *((size_t*) ptr_key) = temp_id;
#else
  temp_id = __softboundcets_key_id_counter++;
  *((size_t**) ptr_lock) = (size_t*)__softboundcets_allocate_lock_location();
  *((size_t*) ptr_key) = temp_id;
  **((size_t**) ptr_lock) = temp_id;
#endif

  __softboundcets_allocation_secondary_trie_allocate(ptr);

#if defined(SOFTBOUNDCETS_DEBUG)
    __softboundcets_printf("[heap_allocation] ptr = %p, lock = %p, key = %zx\n",
                           ptr, *ptr_lock, temp_id);
#endif
}

__WEAK_INLINE__ void
__softboundcets_heap_deallocation(void* ptr, void* ptr_lock, size_t key) {

  if (ptr_lock != NULL && ptr != NULL) {
#if defined(SOFTBOUNDCETS_DEBUG)
    __softboundcets_printf("[heap_deallocation] ptr = %p, lock = %p, key=%zx\n",
                           ptr, ptr_lock, *((size_t*) ptr_lock));
#endif
    size_t temp = *((size_t*)ptr_lock);

    if(temp != key) {
      __softboundcets_printf("[heap_deallocation] Key mismatch key=%zx, *lock=%zx, next_ptr=%zx\n",
                             key, temp, __softboundcets_lock_next_location);
      __softboundcets_abort_reason("double free");
    }

#if defined(SOFTBOUNDCETS_BENCHMARKING_MODE)
#else
    if (ptr_lock == (void*)__softboundcets_global_lock) {
      __softboundcets_abort_reason("deallocating global variable");
    }

    *((size_t*)ptr_lock) = 0;
    *((void**) ptr_lock) = __softboundcets_lock_next_location;
    __softboundcets_lock_next_location = ptr_lock;
#endif
    return;
  } else {
#if defined(SOFTBOUNDCETS_DEBUG)
    __softboundcets_printf("[heap_deallocation] ptr = %p, lock = %p\n",
                           ptr, ptr_lock);
#endif
    return;
  }
}

__WEAK_INLINE__ void
__softboundcets_memcopy_check(void* dest, void* src, size_t size,
                              void* dest_base, void* dest_bound,
                              void* src_base, void* src_bound,
                              size_t dest_key, void* dest_lock,
                              size_t src_key, void* src_lock) {
  if(size >= LONG_MAX)
    __softboundcets_abort();

  if(dest < dest_base || (char*) dest > ((char*) dest_bound - size) || (size > (size_t) dest_bound))
    __softboundcets_abort();

  if(src < src_base || (char*) src > ((char*) src_bound - size) || (size > (size_t) dest_bound))
    __softboundcets_abort();

  if(dest_key != *((size_t*)(dest_lock))){
    __softboundcets_abort();
  }

  if(src_key != *((size_t*)(src_lock))){
    __softboundcets_abort();
  }
  return;
}

__WEAK_INLINE__ void
__softboundcets_memset_check(void* dest, size_t size,
                             void* dest_base, void* dest_bound,
                             size_t dest_key, void* dest_lock){

  if(size >= LONG_MAX)
    __softboundcets_abort();

  if(dest < dest_base || (char*) dest > ((char*) dest_bound - size) || (size > (size_t) dest_bound))
    __softboundcets_abort();

  if(dest_key != *((size_t*)(dest_lock))){
    __softboundcets_abort();
  }
  return;
}
