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

__WEAK__ void
__softboundcets_spatial_load_dereference_check(void *base,
                                               void *bound,
                                               void *ptr,
                                               size_t size_of_type) {
  if (base != NULL && bound != NULL) {
    if ((ptr < base) || ((void*)((char*) ptr + size_of_type) > bound)) {
      __softboundcets_printf("[spatial_load_dereference_check] base=%zx, bound=%zx, ptr=%zx\n",
                             base, bound, ptr);
      __softboundcets_abort();
    }
  }
  return;
}

__WEAK__ void
__softboundcets_spatial_store_dereference_check(void *base,
                                                void *bound,
                                                void *ptr,
                                                size_t size_of_type) {
  if (base != NULL && bound != NULL) {
    if ((ptr < base) || ((void*)((char*)ptr + size_of_type) > bound)) {
      __softboundcets_printf("[spatial_store_dereference_check] base=%p, bound=%p, ptr=%p, size_of_type=%zx, ptr+size=%p\n",
                             base, bound, ptr, size_of_type, (char*)ptr+size_of_type);
      __softboundcets_abort();
    }
  }
  return;
}

__WEAK__ void
__softboundcets_temporal_load_dereference_check(void* pointer_lock,
                                                size_t key) {
  if (pointer_lock != NULL) {
    size_t temp = *((size_t*)pointer_lock);

    if(temp != key) {
      __softboundcets_printf("[temporal_load_dereference_check] Key mismatch key = %zx, *lock=%zx, next_ptr =%zx\n",
                             key, temp, __softboundcets_lock_next_location );
      __softboundcets_abort();
    }
  }
  return;
}

__WEAK__ void
__softboundcets_temporal_store_dereference_check(void* pointer_lock,
                                                 size_t key) {
  if (pointer_lock != NULL) {
    size_t temp = *((size_t*)pointer_lock);

    if(temp != key) {
      __softboundcets_printf("[temporal_store_dereference_check] Key mismatch, key = %zx, *lock=%zx\n",
                             key, temp );
      __softboundcets_abort();
    }
  }
  return;
}

__WEAK__ void*
__softboundcets_get_global_lock(){
  return __softboundcets_global_lock;
}

/* Shadow stack routines */

/*
 * Layout of the shadow stack
 *   1) size of the previous stack frame
 *   2) size of the current stack frame
 *   3) base/bound/key/lock of each argument
 *
 *   Allocation: read the current stack frames size, increment the
 *   shadow_stack_ptr by current_size + 2, store the previous size into
 *   the new prev value, calcuate the allocation size and store in the
 *   new current stack size field; Deallocation: read the previous size,
 *   and decrement the shadow_stack_ptr
 */

__WEAK__ void
__softboundcets_allocate_shadow_stack_space(int num_pointer_args) {

  size_t* prev_stack_size_ptr = __softboundcets_shadow_stack_ptr + 1;
  size_t prev_stack_size = *((size_t*)prev_stack_size_ptr);

  __softboundcets_shadow_stack_ptr = __softboundcets_shadow_stack_ptr + prev_stack_size + 2;

  *((size_t*) __softboundcets_shadow_stack_ptr) = prev_stack_size;
  size_t* current_stack_size_ptr = __softboundcets_shadow_stack_ptr + 1;
  ssize_t size = num_pointer_args * __SOFTBOUNDCETS_METADATA_NUM_FIELDS;
  *((size_t*) current_stack_size_ptr) = size;
  return;
}

__WEAK__ void
__softboundcets_deallocate_shadow_stack_space() {
  size_t* reserved_space_ptr = __softboundcets_shadow_stack_ptr;
  size_t read_value = *((size_t*) reserved_space_ptr);
  assert((read_value >=0 && read_value <= __SOFTBOUNDCETS_SHADOW_STACK_ENTRIES));
  __softboundcets_shadow_stack_ptr =  __softboundcets_shadow_stack_ptr - read_value - 2;
  return;
}

__WEAK__ void
__softboundcets_store_base_shadow_stack(void* base,
                                                           int arg_no) {
  assert(arg_no >= 0);
  size_t count = 2 +  arg_no * __SOFTBOUNDCETS_METADATA_NUM_FIELDS + __BASE_INDEX;
  void** base_ptr = (void**)(__softboundcets_shadow_stack_ptr + count);
  *(base_ptr) = base;
  return;
}

__WEAK__ void
__softboundcets_store_bound_shadow_stack(void* bound,
                                                            int arg_no) {
  assert(arg_no >= 0);
  size_t count = 2 +  arg_no * __SOFTBOUNDCETS_METADATA_NUM_FIELDS + __BOUND_INDEX;
  void** bound_ptr = (void**)(__softboundcets_shadow_stack_ptr + count);
  *(bound_ptr) = bound;
  return;
}

__WEAK__ void*
__softboundcets_load_base_shadow_stack(int arg_no) {
  assert (arg_no >= 0 );
  size_t count = 2 +  arg_no * __SOFTBOUNDCETS_METADATA_NUM_FIELDS + __BASE_INDEX;
  size_t* base_ptr = (__softboundcets_shadow_stack_ptr + count);
  void* base = *((void**)base_ptr);
  return base;
}

__WEAK__ void*
__softboundcets_load_bound_shadow_stack(int arg_no) {
  assert (arg_no >= 0 );
  size_t count = 2 + arg_no * __SOFTBOUNDCETS_METADATA_NUM_FIELDS  + __BOUND_INDEX;
  size_t* bound_ptr = (__softboundcets_shadow_stack_ptr + count);
  void* bound = *((void**)bound_ptr);
  return bound;
}

__WEAK__ size_t
__softboundcets_load_key_shadow_stack(int arg_no) {
  assert (arg_no >= 0 );
  size_t count = 2 + arg_no * __SOFTBOUNDCETS_METADATA_NUM_FIELDS  + __KEY_INDEX;
  size_t* key_ptr = (__softboundcets_shadow_stack_ptr + count);
  size_t key = *key_ptr;
  return key;
}

__WEAK__ void*
__softboundcets_load_lock_shadow_stack(int arg_no) {
  assert (arg_no >= 0 );
  size_t count = 2 + arg_no * __SOFTBOUNDCETS_METADATA_NUM_FIELDS + __LOCK_INDEX;
  size_t* lock_ptr = (__softboundcets_shadow_stack_ptr + count);
  void* lock = *((void**)lock_ptr);
  return lock;
}

__WEAK__ void
__softboundcets_store_key_shadow_stack(size_t key,
                                                          int arg_no) {
  assert(arg_no >= 0);
  size_t count = 2 +  arg_no * __SOFTBOUNDCETS_METADATA_NUM_FIELDS + __KEY_INDEX;
  size_t* key_ptr = (__softboundcets_shadow_stack_ptr + count);
  *(key_ptr) = key;
  return;
}

__WEAK__ void
__softboundcets_store_lock_shadow_stack(void* lock,
                                                           int arg_no) {
  assert(arg_no >= 0);
  size_t count = 2 +  arg_no * __SOFTBOUNDCETS_METADATA_NUM_FIELDS + __LOCK_INDEX;
  void** lock_ptr = (void**)(__softboundcets_shadow_stack_ptr + count);
  *(lock_ptr) = lock;
  return;
}

__WEAK__ void
__softboundcets_create_stack_key(void** ptr_lock, size_t* ptr_key) {
  size_t temp_id = __softboundcets_key_id_counter++;
  *((size_t**) ptr_lock) = (size_t*)__softboundcets_stack_temporal_space_begin++;
  *((size_t*)ptr_key) = temp_id;
  **((size_t**)ptr_lock) = temp_id;
  return;
}

__WEAK__ void
__softboundcets_destroy_stack_key(size_t ptr_key){
  __softboundcets_stack_temporal_space_begin--;
  *(__softboundcets_stack_temporal_space_begin) = 0;
  return;
}

/* Metadata routines */

__WEAK__ void
__softboundcets_metadata_load(void* addr_of_ptr,
                              void** base,
                              void** bound,
                              size_t* key,
                              void** lock) {

  if(addr_of_ptr == NULL) {
    *((void**) base) = NULL;
    *((void**) bound) = NULL;
    *((size_t*) key) = 0;
    *((void**) lock) = NULL;
#if defined(SOFTBOUNDCETS_DEBUG)
    __softboundcets_printf("[metadata_load] ptr_addr=%p, base=%p, bound=%p, key=%zx, lock=%p\n",
                            addr_of_ptr, *((void**) base), *((void**) bound),
                            *((size_t*) key), *((void**) lock));
#endif
    return;
  }

  size_t ptr = (size_t) addr_of_ptr;
  __softboundcets_trie_entry_t* trie_secondary_table;

  size_t primary_index = ( ptr >> 25);
  trie_secondary_table = __softboundcets_trie_primary_table[primary_index];

  if(!__SOFTBOUNDCETS_PREALLOCATE_TRIE) {
    if(trie_secondary_table == NULL) {
      *((void**) base) = NULL;
      *((void**) bound) = NULL;
      *((size_t*) key ) = 0;
      *((void**) lock) = NULL;
      return;
    }
  }

  size_t secondary_index = ((ptr >> 3) & 0x3fffff);
  __softboundcets_trie_entry_t* entry_ptr = &trie_secondary_table[secondary_index];

  assert(entry_ptr && "[metadata_load] lookup failed for input pointer");

  *((void**) base) = entry_ptr->base;
  *((void**) bound) = entry_ptr->bound;
  *((size_t*) key) = entry_ptr->key;
  *((void**) lock) = (void*) entry_ptr->lock;

#if defined(SOFTBOUNDCETS_DEBUG)
  __softboundcets_printf("[metadata_load] ptr_addr=%p, ptr=%p, base=%p, bound=%p, key=%zx, lock=%p\n",
                          addr_of_ptr, *((void **)addr_of_ptr), entry_ptr->base, entry_ptr->bound,
                          entry_ptr->key, entry_ptr->lock);
#endif
  return;
}

__WEAK__ void
__softboundcets_metadata_store(void* addr_of_ptr,
                               void* base,
                               void* bound,
                               size_t key,
                               void* lock) {

  if(addr_of_ptr == NULL) {
#if defined(SOFTBOUNDCETS_DEBUG)
    __softboundcets_printf("[metadata_store] ptr_addr=%p, base=%p, bound=%p, key=%zx, lock=%p\n",
                            addr_of_ptr, *((void**) base), *((void**) bound),
                            *((size_t*) key), *((void**) lock));
#endif
    return;
  }

  size_t ptr = (size_t) addr_of_ptr;
  size_t primary_index;
  __softboundcets_trie_entry_t* trie_secondary_table;

  primary_index = (ptr >> 25);
  trie_secondary_table = __softboundcets_trie_primary_table[primary_index];

  if(!__SOFTBOUNDCETS_PREALLOCATE_TRIE) {
    if(trie_secondary_table == NULL){
      trie_secondary_table =  __softboundcets_trie_allocate();
      __softboundcets_trie_primary_table[primary_index] = trie_secondary_table;
    }
    assert(trie_secondary_table != NULL);
  }

  size_t secondary_index = ((ptr >> 3) & 0x3fffff);
  __softboundcets_trie_entry_t* entry_ptr =&trie_secondary_table[secondary_index];

  entry_ptr->base = base;
  entry_ptr->bound = bound;
  entry_ptr->key = key;
  entry_ptr->lock = lock;

#if defined(SOFTBOUNDCETS_DEBUG)
  __softboundcets_printf("[metadata_store] ptr_addr=%p, ptr=%p, base=%p, bound=%p, key=%zx, lock=%p, *lock=%zx\n",
                          addr_of_ptr, *((void **)addr_of_ptr), entry_ptr->base, entry_ptr->bound,
                          entry_ptr->key, entry_ptr->lock, *((size_t*) (entry_ptr->lock)));
#endif

  return;
}

/* Miscellaneous routines */

__WEAK__ void
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

__WEAK__ void
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
