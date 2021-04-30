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

static int softboundcets_initialized = 0;

__SOFTBOUNDCETS_NORETURN void __softboundcets_abort()
{
  fprintf(stderr, "\nSoftboundcets: Memory safety violation detected\n\nBacktrace:\n");

  // Based on code from the backtrace man page
  size_t size;
  void *array[100];

#if !defined (__FreeBSD__)
  size = backtrace(array, 100);
  backtrace_symbols_fd(array, size, fileno(stderr));
#endif

  fprintf(stderr, "\n\n");

  abort();
}

void __softboundcets_printf(const char* str, ...)
{
  va_list args;

  va_start(args, str);
  vfprintf(stderr, str, args);
  va_end(args);
}

__NO_INLINE void __softboundcets_stub(void) {
  return;
}

void __softboundcets_init(void)
{
  if (softboundcets_initialized != 0) {
    return;  // already initialized, do nothing
  }

  softboundcets_initialized = 1;

#if defined(SOFTBOUNDCETS_DEBUG)
    __softboundcets_printf("Initializing softboundcets metadata space\n");
#endif

  assert(sizeof(__softboundcets_trie_entry_t) >= 16);

  /* Allocating the temporal shadow space */

  size_t temporal_table_length = (__SOFTBOUNDCETS_N_TEMPORAL_ENTRIES)* sizeof(void*);

  __softboundcets_lock_new_location = mmap(0, temporal_table_length,
                                           PROT_READ| PROT_WRITE,
                                           SOFTBOUNDCETS_MMAP_FLAGS, -1, 0);

  assert(__softboundcets_lock_new_location != (void*) -1);
  __softboundcets_temporal_space_begin = (size_t *)__softboundcets_lock_new_location;


  size_t stack_temporal_table_length = (__SOFTBOUNDCETS_N_STACK_TEMPORAL_ENTRIES) * sizeof(void*);
  __softboundcets_stack_temporal_space_begin = mmap(0, stack_temporal_table_length,
                                                    PROT_READ| PROT_WRITE,
                                                    SOFTBOUNDCETS_MMAP_FLAGS, -1, 0);
  assert(__softboundcets_stack_temporal_space_begin != (void*) -1);


  size_t global_lock_size = (__SOFTBOUNDCETS_N_GLOBAL_LOCK_SIZE) * sizeof(void*);
  __softboundcets_global_lock = mmap(0, global_lock_size,
                                     PROT_READ|PROT_WRITE,
                                     SOFTBOUNDCETS_MMAP_FLAGS, -1, 0);
  assert(__softboundcets_global_lock != (void*) -1);
  //  __softboundcets_global_lock =  __softboundcets_lock_new_location++;
  *((size_t*)__softboundcets_global_lock) = 1;



  size_t shadow_stack_size = __SOFTBOUNDCETS_SHADOW_STACK_ENTRIES * sizeof(size_t);
  __softboundcets_shadow_stack_ptr = mmap(0, shadow_stack_size,
                                          PROT_READ|PROT_WRITE,
                                          SOFTBOUNDCETS_MMAP_FLAGS, -1, 0);
  assert(__softboundcets_shadow_stack_ptr != (void*)-1);

  *((size_t*)__softboundcets_shadow_stack_ptr) = 0; /* prev stack size */
  size_t * current_size_shadow_stack_ptr =  __softboundcets_shadow_stack_ptr +1 ;
  *(current_size_shadow_stack_ptr) = 0;

  size_t length_trie = (__SOFTBOUNDCETS_TRIE_PRIMARY_TABLE_ENTRIES) * sizeof(__softboundcets_trie_entry_t*);

  __softboundcets_trie_primary_table = mmap(0, length_trie,
              PROT_READ| PROT_WRITE,
              SOFTBOUNDCETS_MMAP_FLAGS, -1, 0);
  assert(__softboundcets_trie_primary_table != (void *)-1);

  int* temp = malloc(1);
  __softboundcets_allocation_secondary_trie_allocate_range(0, (size_t)temp);

}

__softboundcets_trie_entry_t** __softboundcets_trie_primary_table;
size_t* __softboundcets_shadow_stack_ptr = NULL;
size_t* __softboundcets_temporal_space_begin = 0;
size_t* __softboundcets_stack_temporal_space_begin = NULL;
size_t* __softboundcets_global_lock = 0;
size_t* __softboundcets_lock_next_location = NULL;
size_t* __softboundcets_lock_new_location = NULL;
size_t __softboundcets_key_id_counter = 2; /* 0 = unused, 1 = globals*/
void* __softboundcets_malloc_start_address = NULL;

__WEAK__ __softboundcets_trie_entry_t* __softboundcets_trie_allocate(){

  __softboundcets_trie_entry_t* secondary_entry;
  size_t length = (__SOFTBOUNDCETS_TRIE_SECONDARY_TABLE_ENTRIES) * sizeof(__softboundcets_trie_entry_t);
  secondary_entry = __softboundcets_unchecked_mmap(0, length, PROT_READ| PROT_WRITE,
                SOFTBOUNDCETS_MMAP_FLAGS, -1, 0);
  return secondary_entry;
}

__WEAK__ void*
__softboundcets_allocate_lock_location() {

  void* temp = NULL;
  if(__softboundcets_lock_next_location == NULL) {
#if defined(SOFTBOUNDCETS_DEBUG)
      __softboundcets_printf("[allocate_lock_location] new_lock_location=%p\n",
                             __softboundcets_lock_new_location);

      if(__softboundcets_lock_new_location  >
         __softboundcets_temporal_space_begin + __SOFTBOUNDCETS_N_TEMPORAL_ENTRIES){
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

__WEAK__ void
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

__WEAK__ void
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

__WEAK__ void
__softboundcets_heap_allocation(void* ptr, void** ptr_lock, size_t* ptr_key){

  size_t temp_id = __softboundcets_key_id_counter++;

  *((size_t**) ptr_lock) = (size_t*)__softboundcets_allocate_lock_location();
  *((size_t*) ptr_key) = temp_id;
  **((size_t**) ptr_lock) = temp_id;

  __softboundcets_allocation_secondary_trie_allocate(ptr);

#if defined(SOFTBOUNDCETS_DEBUG)
    __softboundcets_printf("[heap_allocation] ptr = %p, lock = %p, key = %zx\n",
                           ptr, *ptr_lock, temp_id);
#endif
}

__WEAK__ void
__softboundcets_heap_deallocation(void* ptr, void* ptr_lock, size_t ptr_key) {

  if (ptr_lock != NULL) {
#if defined(SOFTBOUNDCETS_DEBUG)
    __softboundcets_printf("[heap_deallocation] ptr = %p, lock = %p, key=%zx\n",
                           ptr, ptr_lock, *((size_t*) ptr_lock));
#endif
    *((size_t*)ptr_lock) = 0;
    *((void**) ptr_lock) = __softboundcets_lock_next_location;
    __softboundcets_lock_next_location = ptr_lock;
    return;
  } else {
#if defined(SOFTBOUNDCETS_DEBUG)
    __softboundcets_printf("[heap_deallocation] ptr = %p, lock = %p\n",
                           ptr, ptr_lock);
#endif
    return;
  }
}

__WEAK__ void __softboundcets_dummy(){
  printf("calling abort");
}

__WEAK__ void
__softboundcets_shrink_bounds(void* new_base, void* new_bound,
                              void* old_base, void* old_bound,
                              void** base_alloca, void** bound_alloca) {
  *(base_alloca) = new_base < old_base ? old_base: new_base;
  *(bound_alloca) = new_bound > old_bound? old_bound : new_bound;
}

__WEAK__ void
__softboundcets_read_shadow_stack_metadata_store(char** endptr, int arg_num) {
    void* nptr_base = __softboundcets_load_base_shadow_stack(arg_num);
    void* nptr_bound = __softboundcets_load_bound_shadow_stack(arg_num);
    size_t nptr_key = __softboundcets_load_key_shadow_stack(arg_num);
    void* nptr_lock = __softboundcets_load_lock_shadow_stack(arg_num);
    __softboundcets_metadata_store(endptr, nptr_base, nptr_bound, nptr_key,
                                   nptr_lock);
}

__WEAK__ void
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

__WEAK__ void __softboundcets_store_null_return_metadata() {
  __softboundcets_store_base_shadow_stack(NULL, 0);
  __softboundcets_store_bound_shadow_stack(NULL, 0);
  __softboundcets_store_key_shadow_stack(0, 0);
  __softboundcets_store_lock_shadow_stack(NULL, 0);
}

__WEAK__ void
__softboundcets_store_return_metadata(void* base, void* bound, size_t key,
                                      void* lock) {
  __softboundcets_store_base_shadow_stack(base, 0);
  __softboundcets_store_bound_shadow_stack(bound, 0);
  __softboundcets_store_key_shadow_stack(key, 0);
  __softboundcets_store_lock_shadow_stack(lock, 0);
}

__WEAK__ void
__softboundcets_store_dontcare_return_metadata() {
  __softboundcets_store_base_shadow_stack(NULL, 0);
  __softboundcets_store_bound_shadow_stack((void*)UINT_MAX, 0);
  __softboundcets_store_key_shadow_stack(1, 0);
  __softboundcets_store_lock_shadow_stack(__softboundcets_global_lock, 0);
}

void* __softboundcets_unchecked_calloc(size_t nmemb, size_t size){

  return calloc(nmemb, size);
}

void* __softboundcets_unchecked_malloc(size_t size){

  return malloc(size);
}

void __softboundcets_unchecked_free(void* ptr){

  free(ptr);
}

void * __softboundcets_unchecked_mmap(void* addr,
                                 size_t length, int prot,
                                 int flags, int fd,
                                 off_t offset){
  return mmap(addr, length, prot, flags, fd, offset);
}

static __attribute__ ((__constructor__)) void __softboundcets_global_init() {
  __softboundcets_init();
  __softboundcets_stub();
}

static void softboundcets_init_ctype(){
#if defined(__linux__)

  char* ptr;
  char* base_ptr;

/*
 * In order to check that accesses to the __ctype_b array are in bounds, we must
 * manually set up a base and bound for the array. This lives in a separate
 * secondary trie level "bucket" created specifically for this purpose.
 *
 * The description of __ctype_b_loc() in the Linux Standard Base Core Spec is as
 * follows:
 *
 * The __ctype_b_loc() function shall return a pointer into an array of
 * characters in the current locale that contains characteristics for each
 * character in the current character set. The array shall contain a total of 384
 * characters, and can be indexed with any signed or unsigned char (i.e. with an
 * index value between -128 and 255). If the application is multithreaded, the
 * array shall be local to the current thread.
 *
 * Since this array is a global, it uses the __softboundcets_global_lock
 * However, the array cannot be deallocated, so this is mostly just a formality.
 */

  ptr = (void*) __ctype_b_loc();

  base_ptr = (void*) (*(__ctype_b_loc()));

  __softboundcets_allocation_secondary_trie_allocate(base_ptr);

  __softboundcets_metadata_store(ptr, ((char*) base_ptr - 128),
                                      ((char*) base_ptr + 255 + 1), 1,
                                      __softboundcets_global_lock);

#endif // defined(__linux__)
}

extern int softboundcets_main(int argc, char **argv);

int main(int argc, char **argv) {

  if(!softboundcets_initialized){
    __softboundcets_global_init();
  }

  char** new_argv = argv;
  size_t argv_key;
  void* argv_loc;

  int* temp = (int*)malloc(1);
  __softboundcets_malloc_start_address = temp;
  __softboundcets_allocation_secondary_trie_allocate_range(0, (size_t)temp);
  __softboundcets_create_stack_key(&argv_loc, &argv_key);

#if defined(__linux__)
  mallopt(M_MMAP_MAX, 0);
#endif

  for(int i = 0; i < argc; i++) {
    __softboundcets_metadata_store(&new_argv[i],
                                   new_argv[i],
                                   new_argv[i] + strlen(new_argv[i]) + 1,
                                   argv_key, argv_loc);
  }

  softboundcets_init_ctype();

  // Santosh: Real Nasty Hack because C programmers assume argv[argc] is NULL.

  char* temp_ptr = ((char*) &new_argv[argc]) + sizeof(char*);

  __softboundcets_allocate_shadow_stack_space(2);
  __softboundcets_store_base_shadow_stack(&new_argv[0], 1);
  __softboundcets_store_bound_shadow_stack(temp_ptr, 1);
  __softboundcets_store_key_shadow_stack(argv_key, 1);
  __softboundcets_store_lock_shadow_stack(argv_loc, 1);
  int return_value = softboundcets_main(argc, new_argv);
  __softboundcets_deallocate_shadow_stack_space();
  __softboundcets_destroy_stack_key(argv_key);
  return return_value;
}
