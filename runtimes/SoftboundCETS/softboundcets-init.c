//=== softboundcets-init.c- SoftBound runtime init --*- C -*===//
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

#if defined(__linux__)
#include <malloc.h>
#endif
#include <ctype.h>
#include "softboundcets-internal.h"
#include "softboundcets-interface.h"

static int softboundcets_initialized = 0;

__NO_INLINE__ void
__softboundcets_init(void) {
  if (softboundcets_initialized != 0) {
    return;  // already initialized, do nothing
  }

  softboundcets_initialized = 1;

#if defined(SOFTBOUNDCETS_DEBUG)
    __softboundcets_printf("Initializing softboundcets runtime metadata storage\n");
#endif

  assert(sizeof(__softboundcets_trie_entry_t) >= 16);

  /* Allocating the temporal shadow space */

  size_t temporal_table_length = (__SOFTBOUNDCETS_N_TEMPORAL_ENTRIES)* sizeof(void*);

  __softboundcets_lock_new_location = mmap(0, temporal_table_length,
                                           PROT_READ| PROT_WRITE,
                                           SOFTBOUNDCETS_MMAP_FLAGS, -1, 0);

  assert(__softboundcets_lock_new_location != (void*) -1);
  __softboundcets_heap_key_table_ptr = (size_t *)__softboundcets_lock_new_location;

  size_t stack_temporal_table_length = (__SOFTBOUNDCETS_N_STACK_TEMPORAL_ENTRIES) * sizeof(void*);
  __softboundcets_stack_key_table_ptr = mmap(0, stack_temporal_table_length,
                                                    PROT_READ| PROT_WRITE,
                                                    SOFTBOUNDCETS_MMAP_FLAGS, -1, 0);
  assert(__softboundcets_stack_key_table_ptr != (void*) -1);

  size_t global_lock_size = (__SOFTBOUNDCETS_N_GLOBAL_LOCK_SIZE) * sizeof(void*);
  __softboundcets_global_lock = mmap(0, global_lock_size,
                                     PROT_READ|PROT_WRITE,
                                     SOFTBOUNDCETS_MMAP_FLAGS, -1, 0);
  assert(__softboundcets_global_lock != (void*) -1);
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
  __softboundcets_malloc_start_address = temp;
  __softboundcets_allocation_secondary_trie_allocate_range(0, (size_t)temp);

#if defined(__linux__)
  mallopt(M_MMAP_MAX, 0);
#endif
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

  size_t argv_key;
  void* argv_lock;
  __softboundcets_create_stack_key(&argv_lock, &argv_key);

  char** new_argv = argv;

  for(int i = 0; i < argc; i++) {
    __softboundcets_metadata_store(&new_argv[i],
                                   new_argv[i],
                                   new_argv[i] + strlen(new_argv[i]) + 1,
                                   argv_key, argv_lock);
  }

  softboundcets_init_ctype();

  __softboundcets_allocate_shadow_stack_space(2);
  __softboundcets_store_base_shadow_stack(&new_argv[0], 1);
  __softboundcets_store_bound_shadow_stack((&new_argv[argc-1])+2, 1);
  __softboundcets_store_key_shadow_stack(argv_key, 1);
  __softboundcets_store_lock_shadow_stack(argv_lock, 1);
  int return_value = softboundcets_main(argc, new_argv);
  __softboundcets_deallocate_shadow_stack_space();
  __softboundcets_destroy_stack_key(argv_key);
  return return_value;
}
