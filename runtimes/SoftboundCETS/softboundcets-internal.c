//=== softboundcets.c - Creates the main function for SoftBound+CETS Runtime --*- C -*===//
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

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#if defined(__linux__)
#include <malloc.h>
#endif
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include <sys/mman.h>
#if !defined(__FreeBSD__)
#include <execinfo.h>
#endif

#include "softboundcets-internal.h"
#include "softboundcets-interface.h"

__SOFTBOUNDCETS_NORETURN void
__softboundcets_abort() {
  fprintf(stderr, "\nSoftboundcets: Memory safety violation detected\n\nBacktrace:\n");
  // Based on code from the backtrace man page
  size_t size;
  void *array[SOFTBOUNDCETS_BACKTRACE_DEPTH];
#if !defined (__FreeBSD__)
  size = backtrace(array, SOFTBOUNDCETS_BACKTRACE_DEPTH);
  backtrace_symbols_fd(array, size, fileno(stderr));
#endif
  fprintf(stderr, "\n\n");
  abort();
}

__SOFTBOUNDCETS_NORETURN void
__softboundcets_abort_reason(const char *reason) {
  fprintf(stderr, "\nSoftboundcets: Memory safety violation detected (");
  fprintf(stderr, "%s)\n\n", reason);
  fprintf(stderr, "Backtrace:\n");
  // Based on code from the backtrace man page
  size_t size;
  void *array[SOFTBOUNDCETS_BACKTRACE_DEPTH];
#if !defined (__FreeBSD__)
  size = backtrace(array, SOFTBOUNDCETS_BACKTRACE_DEPTH);
  backtrace_symbols_fd(array, size, fileno(stderr));
#endif
  fprintf(stderr, "\n\n");
  abort();
}

void
__softboundcets_printf(const char* str, ...) {
  va_list args;

  va_start(args, str);
  vfprintf(stderr, str, args);
  va_end(args);
}

__NO_INLINE void
__softboundcets_stub(void) {
  return;
}

__softboundcets_trie_entry_t** __softboundcets_trie_primary_table;
size_t* __softboundcets_shadow_stack_ptr = NULL;
size_t* __softboundcets_heap_key_table_ptr = 0;
size_t* __softboundcets_stack_key_table_ptr = NULL;
size_t* __softboundcets_global_lock = 0;
size_t* __softboundcets_lock_next_location = NULL;
size_t* __softboundcets_lock_new_location = NULL;
size_t __softboundcets_key_id_counter = 2; /* 0 = unused, 1 = globals*/
void* __softboundcets_malloc_start_address = NULL;

__WEAK__ inline __softboundcets_trie_entry_t*
__softboundcets_trie_allocate() {
  __softboundcets_trie_entry_t* secondary_entry;
  size_t length = (__SOFTBOUNDCETS_TRIE_SECONDARY_TABLE_ENTRIES) * sizeof(__softboundcets_trie_entry_t);
  secondary_entry = __softboundcets_unchecked_mmap(0, length, PROT_READ| PROT_WRITE,
                SOFTBOUNDCETS_MMAP_FLAGS, -1, 0);
  return secondary_entry;
}

__WEAK__ inline void*
__softboundcets_allocate_lock_location() {
  void* temp = NULL;
  if(__softboundcets_lock_next_location == NULL) {
#if defined(SOFTBOUNDCETS_DEBUG)
      __softboundcets_printf("[allocate_lock_location] new_lock_location=%p\n", __softboundcets_lock_new_location);
      if(__softboundcets_lock_new_location > __softboundcets_heap_key_table_ptr + __SOFTBOUNDCETS_N_TEMPORAL_ENTRIES) {
        __softboundcets_printf("[allocate_lock_location] out of temporal free entries \n");
        __softboundcets_abort();
      }
#endif
    return __softboundcets_lock_new_location++;
  } else {
    temp = __softboundcets_lock_next_location;
#if defined(SOFTBOUNDCETS_DEBUG)
      __softboundcets_printf("[allocate_lock_location] next_lock_location=%p\n", temp);
#endif
    __softboundcets_lock_next_location = *((void**)__softboundcets_lock_next_location);
    return temp;
  }
}

__WEAK__ inline void
__softboundcets_allocation_secondary_trie_allocate_range(void* initial_ptr,
                                                         size_t size) {

  if(!__SOFTBOUNDCETS_PREALLOCATE_TRIE)
    return;

  void* addr_of_ptr = initial_ptr;
  size_t start_addr_of_ptr = (size_t) addr_of_ptr;
  size_t start_primary_index = start_addr_of_ptr >> 25;

  size_t end_addr_of_ptr = (size_t)((char*) initial_ptr + size);
  size_t end_primary_index = end_addr_of_ptr >> 25;

  for(; start_primary_index <= end_primary_index; start_primary_index++){

    __softboundcets_trie_entry_t*
      trie_secondary_table = __softboundcets_trie_primary_table[start_primary_index];
    if(trie_secondary_table == NULL) {
      trie_secondary_table =  __softboundcets_trie_allocate();
      __softboundcets_trie_primary_table[start_primary_index] = trie_secondary_table;
    }
  }
}

__WEAK__ inline void
__softboundcets_allocation_secondary_trie_allocate(void* addr_of_ptr) {

  if(!__SOFTBOUNDCETS_PREALLOCATE_TRIE)
    return;

  size_t ptr = (size_t) addr_of_ptr;
  size_t primary_index = ( ptr >> 25);
  //  size_t secondary_index = ((ptr >> 3) & 0x3fffff);

  __softboundcets_trie_entry_t*
    trie_secondary_table = __softboundcets_trie_primary_table[primary_index];

  if(trie_secondary_table == NULL) {
    trie_secondary_table =  __softboundcets_trie_allocate();
    __softboundcets_trie_primary_table[primary_index] = trie_secondary_table;
  }

  __softboundcets_trie_entry_t*
    trie_secondary_table_second_entry = __softboundcets_trie_primary_table[primary_index +1];

  if(trie_secondary_table_second_entry == NULL) {
    __softboundcets_trie_primary_table[primary_index+1] = __softboundcets_trie_allocate();
  }

  if(primary_index != 0 && (__softboundcets_trie_primary_table[primary_index -1] == NULL)){
    __softboundcets_trie_primary_table[primary_index-1] = __softboundcets_trie_allocate();
  }

  return;
}

__WEAK__ inline void __softboundcets_dummy(){
  printf("calling abort");
}

__WEAK__
void __softboundcets_metadata_copy(void* dest, void* from, size_t size) {

  size_t dest_ptr = (size_t) dest;
  size_t dest_ptr_end = dest_ptr + size;

  size_t from_ptr = (size_t) from;
  size_t from_ptr_end = from_ptr + size;


  if(from_ptr % 8 != 0){ // bail if the from pointer is 8-byte misaligned
    return;
  }

  __softboundcets_trie_entry_t* trie_secondary_table_dest_begin;
  __softboundcets_trie_entry_t* trie_secondary_table_from_begin;

  size_t dest_primary_index_begin = (dest_ptr >> 25);
  size_t dest_primary_index_end = (dest_ptr_end >> 25);

  size_t from_primary_index_begin = (from_ptr >> 25);
  size_t from_primary_index_end =  (from_ptr_end >> 25);


  if((from_primary_index_begin != from_primary_index_end) ||
     (dest_primary_index_begin != dest_primary_index_end)){

    size_t from_sizet = from_ptr;
    size_t dest_sizet = dest_ptr;

    size_t trie_size = size;
    size_t index = 0;

    for(index = 0; index < trie_size; index = index + 8){

      size_t temp_from_pindex = (from_sizet + index) >> 25;
      size_t temp_to_pindex = (dest_sizet + index) >> 25;

      size_t dest_secondary_index = (((dest_sizet + index) >> 3) & 0x3fffff);
      size_t from_secondary_index = (((from_sizet + index) >> 3) & 0x3fffff);

      __softboundcets_trie_entry_t* temp_from_strie = __softboundcets_trie_primary_table[temp_from_pindex];

      if(temp_from_strie == NULL){
        temp_from_strie = __softboundcets_trie_allocate();
        __softboundcets_trie_primary_table[temp_from_pindex] = temp_from_strie;
      }

      __softboundcets_trie_entry_t* temp_to_strie = __softboundcets_trie_primary_table[temp_to_pindex];

      if(temp_to_strie == NULL){
        temp_to_strie = __softboundcets_trie_allocate();
        __softboundcets_trie_primary_table[temp_to_pindex] = temp_to_strie;
      }

      void* dest_entry_ptr = &temp_to_strie[dest_secondary_index];
      void* from_entry_ptr = &temp_from_strie[from_secondary_index];

      memcpy(dest_entry_ptr, from_entry_ptr, 32);
    }

    return;
  }

  trie_secondary_table_dest_begin = __softboundcets_trie_primary_table[dest_primary_index_begin];
  trie_secondary_table_from_begin = __softboundcets_trie_primary_table[from_primary_index_begin];

  if(trie_secondary_table_from_begin == NULL) {
    // bail if we're trying to copy metadata from a pointer we don't have a record for
    return;
  }

  if(trie_secondary_table_dest_begin == NULL) {
    trie_secondary_table_dest_begin = __softboundcets_trie_allocate();
    __softboundcets_trie_primary_table[dest_primary_index_begin] = trie_secondary_table_dest_begin;
  }

  size_t dest_secondary_index = ((dest_ptr>> 3) & 0x3fffff);
  size_t from_secondary_index = ((from_ptr>> 3) & 0x3fffff);

  assert(dest_secondary_index < __SOFTBOUNDCETS_TRIE_SECONDARY_TABLE_ENTRIES);
  assert(from_secondary_index < __SOFTBOUNDCETS_TRIE_SECONDARY_TABLE_ENTRIES);

  void* dest_entry_ptr = &trie_secondary_table_dest_begin[dest_secondary_index];
  void* from_entry_ptr = &trie_secondary_table_from_begin[from_secondary_index];

  memcpy(dest_entry_ptr, from_entry_ptr, 32* (size>> 3));

  return;
}

__WEAK__ inline void
__softboundcets_shrink_bounds(void* new_base, void* new_bound,
                              void* old_base, void* old_bound,
                              void** base_alloca, void** bound_alloca) {
  *(base_alloca) = new_base < old_base ? old_base: new_base;
  *(bound_alloca) = new_bound > old_bound? old_bound : new_bound;
}

__WEAK__ inline void
__softboundcets_read_shadow_stack_metadata_store(char** endptr, int arg_num) {
    void* nptr_base = __softboundcets_load_base_shadow_stack(arg_num);
    void* nptr_bound = __softboundcets_load_bound_shadow_stack(arg_num);
    size_t nptr_key = __softboundcets_load_key_shadow_stack(arg_num);
    void* nptr_lock = __softboundcets_load_lock_shadow_stack(arg_num);
    __softboundcets_metadata_store(endptr, nptr_base, nptr_bound, nptr_key,
                                   nptr_lock);
}

__WEAK__ inline void
__softboundcets_propagate_metadata_shadow_stack_from(int from_argnum,
                                                     int to_argnum) {
  void* base = __softboundcets_load_base_shadow_stack(from_argnum);
  void* bound = __softboundcets_load_bound_shadow_stack(from_argnum);
  size_t key = __softboundcets_load_key_shadow_stack(from_argnum);
  void* lock = __softboundcets_load_lock_shadow_stack(from_argnum);
  __softboundcets_store_base_shadow_stack(base, to_argnum);
  __softboundcets_store_bound_shadow_stack(bound, to_argnum);
  __softboundcets_store_key_shadow_stack(key, to_argnum);
  __softboundcets_store_lock_shadow_stack(lock, to_argnum);
}

__WEAK__ inline void __softboundcets_store_null_return_metadata() {
  __softboundcets_store_base_shadow_stack(NULL, 0);
  __softboundcets_store_bound_shadow_stack(NULL, 0);
  __softboundcets_store_key_shadow_stack(0, 0);
  __softboundcets_store_lock_shadow_stack(NULL, 0);
}

__WEAK__ inline void
__softboundcets_store_return_metadata(void* base, void* bound, size_t key,
                                      void* lock) {
  __softboundcets_store_base_shadow_stack(base, 0);
  __softboundcets_store_bound_shadow_stack(bound, 0);
  __softboundcets_store_key_shadow_stack(key, 0);
  __softboundcets_store_lock_shadow_stack(lock, 0);
}

__WEAK__ inline void
__softboundcets_store_dontcare_return_metadata() {
  __softboundcets_store_base_shadow_stack(NULL, 0);
  __softboundcets_store_bound_shadow_stack((void*)PTRDIFF_MAX, 0);
  __softboundcets_store_key_shadow_stack(1, 0);
  __softboundcets_store_lock_shadow_stack(__softboundcets_global_lock, 0);
}

__WEAK__ inline void
__softboundcets_store_dontcare_base_return_metadata(void* base) {
  __softboundcets_store_base_shadow_stack(base, 0);
  __softboundcets_store_bound_shadow_stack((void*)PTRDIFF_MAX, 0);
  __softboundcets_store_key_shadow_stack(1, 0);
  __softboundcets_store_lock_shadow_stack(__softboundcets_global_lock, 0);
}

inline void* __softboundcets_unchecked_calloc(size_t nmemb, size_t size){

  return calloc(nmemb, size);
}

inline void* __softboundcets_unchecked_malloc(size_t size){

  return malloc(size);
}

inline void __softboundcets_unchecked_free(void* ptr){

  free(ptr);
}

inline void * __softboundcets_unchecked_mmap(void* addr,
                                 size_t length, int prot,
                                 int flags, int fd,
                                 off_t offset){
  return mmap(addr, length, prot, flags, fd, offset);
}
