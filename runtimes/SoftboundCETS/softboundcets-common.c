//=== softboundcets-common.c - implementation of common public interface functions--*- C -*===//
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

#include <stdint.h>
#include "softboundcets-internal.h"
#include "softboundcets-interface.h"

__WEAK_INLINE__ void*
__softboundcets_get_global_lock(){
#if defined(SOFTBOUNDCETS_DEBUG)
  __softboundcets_printf("[get_global_lock] lock=%p, *lock=%zx\n",
                          __softboundcets_global_lock, *((size_t*) (__softboundcets_global_lock)));
#endif
  return __softboundcets_global_lock;
}

__WEAK_INLINE__ void
__softboundcets_allocate_shadow_stack_space(int num_pointer_args) {

  size_t* prev_stack_size_ptr = __softboundcets_shadow_stack_ptr + 1;
  size_t prev_stack_size = *((size_t*)prev_stack_size_ptr);

  __softboundcets_shadow_stack_ptr = __softboundcets_shadow_stack_ptr + prev_stack_size + 2;

  *((size_t*) __softboundcets_shadow_stack_ptr) = prev_stack_size;
  size_t* current_stack_size_ptr = __softboundcets_shadow_stack_ptr + 1;
  ssize_t size = num_pointer_args * __SOFTBOUNDCETS_METADATA_NUM_FIELDS;
  *((size_t*) current_stack_size_ptr) = size;

#if defined(SOFTBOUNDCETS_DEBUG)
  __softboundcets_printf("[allocate_shadow_stack_space] shadow_sp=%p, metadata_entries=%zx\n",
                          __softboundcets_shadow_stack_ptr, num_pointer_args);
#endif
  return;
}

__WEAK_INLINE__ void
__softboundcets_deallocate_shadow_stack_space() {
  size_t* reserved_space_ptr = __softboundcets_shadow_stack_ptr;
  size_t read_value = *((size_t*) reserved_space_ptr);
#if defined(SOFTBOUNDCETS_DEBUG)
  assert((read_value >=0 && read_value <= __SOFTBOUNDCETS_SHADOW_STACK_ENTRIES));
#endif
  __softboundcets_shadow_stack_ptr =  __softboundcets_shadow_stack_ptr - read_value - 2;

#if defined(SOFTBOUNDCETS_DEBUG)
  __softboundcets_printf("[deallocate_shadow_stack_space] shadow_sp=%p\n",
                          __softboundcets_shadow_stack_ptr);
#endif
  return;
}

__WEAK_INLINE__ void
__softboundcets_store_base_shadow_stack(void* base,
                                                           int arg_no) {
#if defined(SOFTBOUNDCETS_DEBUG)
  assert(arg_no >= 0);
#endif
  size_t count = 2 +  arg_no * __SOFTBOUNDCETS_METADATA_NUM_FIELDS + __BASE_INDEX;
  void** base_ptr = (void**)(__softboundcets_shadow_stack_ptr + count);
  *(base_ptr) = base;

#if defined(SOFTBOUNDCETS_DEBUG)
  __softboundcets_printf("[store_base_shadow_stack] base=%p, entry=%zx\n",
                          base, arg_no);
#endif
  return;
}

__WEAK_INLINE__ void
__softboundcets_store_bound_shadow_stack(void* bound,
                                                            int arg_no) {
#if defined(SOFTBOUNDCETS_DEBUG)
  assert(arg_no >= 0);
#endif
  size_t count = 2 +  arg_no * __SOFTBOUNDCETS_METADATA_NUM_FIELDS + __BOUND_INDEX;
  void** bound_ptr = (void**)(__softboundcets_shadow_stack_ptr + count);
  *(bound_ptr) = bound;

#if defined(SOFTBOUNDCETS_DEBUG)
  __softboundcets_printf("[store_bound_shadow_stack] bound=%p, entry=%zx\n",
                          bound, arg_no);
#endif
  return;
}

__WEAK_INLINE__ void
__softboundcets_store_key_shadow_stack(size_t key,
                                                          int arg_no) {
#if defined(SOFTBOUNDCETS_DEBUG)
  assert(arg_no >= 0);
#endif
  size_t count = 2 +  arg_no * __SOFTBOUNDCETS_METADATA_NUM_FIELDS + __KEY_INDEX;
  size_t* key_ptr = (__softboundcets_shadow_stack_ptr + count);
  *(key_ptr) = key;

#if defined(SOFTBOUNDCETS_DEBUG)
  __softboundcets_printf("[store_key_shadow_stack] key=%zx, entry=%zx\n",
                          key, arg_no);
#endif
  return;
}

__WEAK_INLINE__ void
__softboundcets_store_lock_shadow_stack(void* lock,
                                                           int arg_no) {
#if defined(SOFTBOUNDCETS_DEBUG)
  assert(arg_no >= 0);
#endif
  size_t count = 2 +  arg_no * __SOFTBOUNDCETS_METADATA_NUM_FIELDS + __LOCK_INDEX;
  void** lock_ptr = (void**)(__softboundcets_shadow_stack_ptr + count);
  *(lock_ptr) = lock;

#if defined(SOFTBOUNDCETS_DEBUG)
  __softboundcets_printf("[store_lock_shadow_stack] lock=%p, entry=%zx\n",
                          lock, arg_no);
#endif
  return;
}

__WEAK_INLINE__ void*
__softboundcets_load_base_shadow_stack(int arg_no) {
#if defined(SOFTBOUNDCETS_DEBUG)
  assert (arg_no >= 0 );
#endif
  size_t count = 2 +  arg_no * __SOFTBOUNDCETS_METADATA_NUM_FIELDS + __BASE_INDEX;
  size_t* base_ptr = (__softboundcets_shadow_stack_ptr + count);
  void* base = *((void**)base_ptr);

#if defined(SOFTBOUNDCETS_DEBUG)
  __softboundcets_printf("[load_base_shadow_stack] base=%p, location=%zx\n",
                          base, arg_no);
#endif
  return base;
}

__WEAK_INLINE__ void*
__softboundcets_load_bound_shadow_stack(int arg_no) {
#if defined(SOFTBOUNDCETS_DEBUG)
  assert (arg_no >= 0 );
#endif
  size_t count = 2 + arg_no * __SOFTBOUNDCETS_METADATA_NUM_FIELDS  + __BOUND_INDEX;
  size_t* bound_ptr = (__softboundcets_shadow_stack_ptr + count);
  void* bound = *((void**)bound_ptr);

#if defined(SOFTBOUNDCETS_DEBUG)
  __softboundcets_printf("[load_bound_shadow_stack] bound=%p, location=%zx\n",
                          bound, arg_no);
#endif
  return bound;
}

__WEAK_INLINE__ size_t
__softboundcets_load_key_shadow_stack(int arg_no) {
#if defined(SOFTBOUNDCETS_DEBUG)
  assert (arg_no >= 0 );
#endif
  size_t count = 2 + arg_no * __SOFTBOUNDCETS_METADATA_NUM_FIELDS  + __KEY_INDEX;
  size_t* key_ptr = (__softboundcets_shadow_stack_ptr + count);
  size_t key = *key_ptr;

#if defined(SOFTBOUNDCETS_DEBUG)
  __softboundcets_printf("[load_key_shadow_stack] key=%zx, entry=%zx\n",
                          key, arg_no);
#endif
  return key;
}

__WEAK_INLINE__ void*
__softboundcets_load_lock_shadow_stack(int arg_no) {
#if defined(SOFTBOUNDCETS_DEBUG)
  assert (arg_no >= 0 );
#endif
  size_t count = 2 + arg_no * __SOFTBOUNDCETS_METADATA_NUM_FIELDS + __LOCK_INDEX;
  size_t* lock_ptr = (__softboundcets_shadow_stack_ptr + count);
  void* lock = *((void**)lock_ptr);

#if defined(SOFTBOUNDCETS_DEBUG)
  __softboundcets_printf("[load_lock_shadow_stack] lock=%p, entry=%zx\n",
                          lock, arg_no);
#endif
  return lock;
}

__WEAK_INLINE__ void
__softboundcets_create_stack_key(void** ptr_lock, size_t* ptr_key) {
#if defined(SOFTBOUNDCETS_DEBUG)
  assert(ptr_lock && "[create_stack_key] passed a null pointer to parent lock location");
  assert(ptr_key && "[create_stack_key] passed a null pointer to key location");
#endif

  if (__softboundcets_stack_key_table_ptr == NULL) {
    __softboundcets_printf("[create_stack_key] stack key table pointer is null\n");
    __softboundcets_abort();
  }

  size_t temp_id = __softboundcets_key_id_counter++;
  size_t *lock_location = (size_t*)__softboundcets_stack_key_table_ptr++;

  *(lock_location) = temp_id;
  *(ptr_key) = temp_id;
  *((size_t**) ptr_lock) = lock_location;

#if defined(SOFTBOUNDCETS_DEBUG)
  __softboundcets_printf("[create_stack_key] key=%zx, lock=%p\n",
                          *ptr_key, *ptr_lock);
#endif
  return;
}

__WEAK_INLINE__ void
__softboundcets_metadata_check(void** base,
                               void** bound,
                               size_t* key,
                               void** lock) {

  if ((base == NULL)  ||
      (bound == NULL) ||
      (key == NULL)   ||
      (lock == NULL)) {
    __softboundcets_printf("[metadata_check] corrupt metadata detected: basePtr=%p, boundPtr=%p, keyPtr=%p, lockPtr=%p\n",
                             base, bound, key, lock);
    __softboundcets_abort();

    if ((*lock) == NULL) {
#if defined(SOFTBOUNDCETS_DEBUG)
      __softboundcets_printf("[metadata_check] invalid metadata detected: basePtr=%p, boundPtr=%p, keyPtr=%p, lockPtr=%p, lock=%p\n",
                             base, bound, key, lock, (*lock));
#endif
    }
  }
  return;
}

__WEAK_INLINE__ void
__softboundcets_metadata_load(void* addr_of_ptr,
                              void** base,
                              void** bound,
                              size_t* key,
                              void** lock) {

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

#if defined(SOFTBOUNDCETS_DEBUG)
  assert(entry_ptr && "[metadata_load] lookup failed for input pointer");
#endif

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

__WEAK_INLINE__ void
__softboundcets_metadata_store(void* addr_of_ptr,
                               void* base,
                               void* bound,
                               size_t key,
                               void* lock) {

#if defined(SOFTBOUNDCETS_DEBUG)
  assert(addr_of_ptr && "[metadata_store] storing metadata for a null pointer");
#endif

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
#if defined(SOFTBOUNDCETS_DEBUG)
    assert(trie_secondary_table != NULL);
#endif
  }

  size_t secondary_index = ((ptr >> 3) & 0x3fffff);
  __softboundcets_trie_entry_t* entry_ptr =&trie_secondary_table[secondary_index];

#if defined(SOFTBOUNDCETS_DEBUG)
  assert(entry_ptr && "[metadata_store] lookup failed for input pointer");
#endif

  entry_ptr->base = base;
  entry_ptr->bound = bound;
  entry_ptr->key = key;
  entry_ptr->lock = lock;

#if defined(SOFTBOUNDCETS_DEBUG)
  __softboundcets_printf("[metadata_store] ptr_addr=%p, base=%p, bound=%p, key=%zx, lock=%p\n",
                          addr_of_ptr, entry_ptr->base, entry_ptr->bound,
                          entry_ptr->key, entry_ptr->lock);
#endif

  return;
}
