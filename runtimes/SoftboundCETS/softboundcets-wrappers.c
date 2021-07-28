//=== softboundcets-wrappers.c- SoftBound wrappers for libraries --*- C -*===//
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

#include <arpa/inet.h>

#if defined(__linux__)
#include <errno.h>
#include <sys/wait.h>
#include <wait.h>
#include <obstack.h>
#include <libintl.h>
#endif

#include <sys/mman.h>
#include <sys/times.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <fnmatch.h>
#include <wchar.h>

#include <netinet/in.h>

#include <assert.h>
#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <grp.h>
#include <getopt.h>
#include <glob.h>
#include <limits.h>
#include <math.h>
#include <netdb.h>
#include <pwd.h>
#include <syslog.h>
#include <setjmp.h>
#include <string.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include <ttyent.h>
#include <time.h>
#include <unistd.h>
#include <langinfo.h>
#include <regex.h>

#ifdef HAVE_ICONV_H
#include <iconv.h>
#endif

#include <utime.h>
#include <math.h>
#include <locale.h>

#include <fcntl.h>
#include <wctype.h>

typedef size_t key_type;
typedef void* lock_type;

#include "softboundcets-internal.h"
#include "softboundcets-interface.h"

typedef void(*sighandler_t)(int);
typedef void(*void_func_ptr)(void);

extern size_t* __softboundcets_global_lock;

extern void __softboundcets_process_memory_total();

__WEAK__ FILE*
softboundcets_tmpfile(void) {
  void* ret_ptr = tmpfile();
  void* ret_ptr_bound = (char*) ret_ptr + sizeof(FILE);
  __softboundcets_store_return_metadata(ret_ptr, ret_ptr_bound,
                                        1, __softboundcets_global_lock);
  return ret_ptr;
}

__WEAK__ DIR*
softboundcets_fdopendir(int fd) {
  void* ret_ptr = (void*) fdopendir(fd);
  // FIXME: overflow allowed because bound is unknown
  __softboundcets_store_dontcare_base_return_metadata(ret_ptr);
  return (DIR*)ret_ptr;
}

__WEAK__  char*
softboundcets_mkdtemp(char *template) {
  char* ret_ptr = mkdtemp(template);
  __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
  return ret_ptr;
}

__WEAK__ struct lconv*
softboundcets_localeconv(void) {
  struct lconv* temp = localeconv();
  // FIXME: overflow allowed because bound is unknown
  __softboundcets_store_dontcare_base_return_metadata(temp);
  return temp;
}

__WEAK__ struct tm*
softboundcets_gmtime(const time_t *timep) {
  struct tm * temp = gmtime(timep);
  // FIXME: overflow allowed because bound is unknown
  __softboundcets_store_dontcare_base_return_metadata(temp);
  return temp;
}

__WEAK__ void*
softboundcets_bsearch(const void *key, const void *base,
                      size_t nmemb, size_t size,
                      int (*compar)(const void *, const void *)) {
  void* ret_ptr = bsearch(key, base, nmemb, size, compar);
  __softboundcets_propagate_metadata_shadow_stack_from(2, 0);
  return ret_ptr;
}

__WEAK__ struct group*
softboundcets_getgrnam(const char *name) {
  void* ret_ptr = getgrnam(name);
  // FIXME: overflow allowed because bound is unknown
  __softboundcets_store_dontcare_base_return_metadata(ret_ptr);
  return ret_ptr;
}

__WEAK__ struct passwd*
softboundcets_getpwnam(const char *name) {
  void* ret_ptr = getpwnam(name);
  // FIXME: overflow allowed because bound is unknown
  __softboundcets_store_dontcare_base_return_metadata(ret_ptr);
  return ret_ptr;
}

__WEAK__ struct passwd*
softboundcets_getpwuid(uid_t uid) {
  void* ret_ptr= getpwuid(uid);
  // FIXME: overflow allowed because bound is unknown
  __softboundcets_store_dontcare_base_return_metadata(ret_ptr);
  return ret_ptr;
}

__WEAK__ struct group*
softboundcets_getgrgid(gid_t gid) {
  void* ret_ptr = getgrgid(gid);
  // FIXME: overflow allowed because bound is unknown
  __softboundcets_store_dontcare_base_return_metadata(ret_ptr);
  return ret_ptr;
}

__WEAK__ FILE*
softboundcets_fopen(const char* path, const char* mode) {
  void* ret_ptr = (void*) fopen(path, mode);
  void* ret_ptr_bound = (char*) ret_ptr + sizeof(FILE);
  __softboundcets_store_return_metadata(ret_ptr, ret_ptr_bound,
                                        1, (void*) __softboundcets_global_lock);
  return (FILE*)ret_ptr;
}

__WEAK__ FILE*
softboundcets_fdopen(int fildes, const char* mode) {
  void* ret_ptr = (void*) fdopen(fildes, mode);
  void* ret_ptr_bound = (char*) ret_ptr + sizeof(FILE);

  __softboundcets_store_return_metadata(ret_ptr, ret_ptr_bound,
                                        1, (void*)__softboundcets_global_lock);
  return (FILE*)ret_ptr;
}

__WEAK__ FILE*
softboundcets_popen(const char* command, const char* type) {
  void* ret_ptr = (void*) popen(command, type);
  void* ret_ptr_bound = (char*)ret_ptr + sizeof(FILE);
  __softboundcets_store_return_metadata(ret_ptr, ret_ptr_bound,
                                        1, (void*) __softboundcets_global_lock);
  return (FILE*)ret_ptr;
}

__WEAK__ struct dirent*
softboundcets_readdir(DIR* dir) {
  void* ret_ptr = (void*) readdir(dir);
  void* ret_ptr_bound = (char*)ret_ptr + sizeof(struct dirent);
  __softboundcets_store_return_metadata(ret_ptr, ret_ptr_bound,
                                        1, (void*) __softboundcets_global_lock);
  return (struct dirent*)ret_ptr;
}

__WEAK__ DIR*
softboundcets_opendir(const char* name) {
  void* ret_ptr = opendir(name);
  // FIXME: overflow allowed because bound is unknown
  __softboundcets_store_dontcare_base_return_metadata(ret_ptr);
  return (DIR*)ret_ptr;
}

__WEAK__ char*
softboundcets_getcwd(char* buf, size_t size) {
  if(buf == NULL) {
    printf("[getcwd] null buffer pointer passed (requesting memory from system) -- unhandled\n");
    __softboundcets_abort();
  }

  char* base = (char *)__softboundcets_load_base_shadow_stack(1);
  char* bound = (char *) __softboundcets_load_bound_shadow_stack(1);
  if (buf < base || buf + size > bound){
    __softboundcets_printf("[getcwd] overflow in buf in getcwd\n");
    __softboundcets_abort();
  }

  char* ret_ptr = getcwd(buf, size);
  __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
  return ret_ptr;
}

__WEAK__ char*
softboundcets_strpbrk(const char* s, const char* accept) {
  char* ret_ptr = strpbrk(s, accept);
  if(ret_ptr != NULL) {
    __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
  } else {
    __softboundcets_store_null_return_metadata();
  }
  return ret_ptr;
}

__WEAK__ char*
softboundcets_fgets(char* s, int size, FILE* stream) {
  char* ret_ptr = fgets(s, size, stream);
  __softboundcets_propagate_metadata_shadow_stack_from(1,0);
  return ret_ptr;
}

#ifdef _GNU_SOURCE
__WEAK__ void*
softboundcets_mempcpy(void * dest, const void * src, size_t n) {
  // FIXME: need to copy the metadata
  void* ret_ptr = mempcpy(dest, src, n);
  __softboundcets_propagate_metadata_shadow_stack_from(1,0);
  return ret_ptr;
}

__WEAK__ void*
softboundcets_memrchr(const void * s, int c, size_t n) {
  void* ret_ptr = memrchr(s, c, n);
  if(ret_ptr != NULL) {
    __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
  } else {
    __softboundcets_store_null_return_metadata();
  }
  return ret_ptr;
}
#endif

__WEAK__ void*
softboundcets_memchr(const void * s, int c, size_t n) {
  void* ret_ptr = memchr(s, c, n);
  if(ret_ptr != NULL) {
    __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
  } else {
    __softboundcets_store_null_return_metadata();
  }
  return ret_ptr;
}

__WEAK__ char*
softboundcets_rindex(char* s, int c) {
  char* ret_ptr = rindex(s,c);
  __softboundcets_propagate_metadata_shadow_stack_from(1,0);
  return ret_ptr;
}

__WEAK__ ssize_t
softboundcets___getdelim(char **lineptr, size_t *n, int delim, FILE *stream) {
  int metadata_prop = 1;
  if(*lineptr == NULL){
    metadata_prop = 0;
  }
  ssize_t ret_val = getdelim(lineptr, n, delim, stream);
  if(metadata_prop){
    __softboundcets_read_shadow_stack_metadata_store(lineptr, 1);
  } else {
    __softboundcets_store_return_metadata(*lineptr,
                                          (*lineptr) + strlen(*lineptr),
                                          1, __softboundcets_global_lock);
  }
  return ret_val;
}

__WEAK__ unsigned long int
softboundcets_strtoul(const char* nptr, char ** endptr, int base) {
  unsigned long temp = strtoul(nptr, endptr, base);
  if(endptr != NULL){
    __softboundcets_read_shadow_stack_metadata_store(endptr, 1);
  }
  return temp;
}

__WEAK__ double
softboundcets_strtod(const char* nptr, char** endptr) {
  double temp = strtod(nptr, endptr);
  if(endptr != NULL) {
    __softboundcets_read_shadow_stack_metadata_store(endptr, 1);
  }
  return temp;
}

__WEAK__ long
softboundcets_strtol(const char* nptr, char **endptr, int base) {
  long temp = strtol(nptr, endptr, base);
  if(endptr != NULL) {
    __softboundcets_read_shadow_stack_metadata_store(endptr, 1);
  }
  return temp;
}

#ifdef _GNU_SOURCE
__WEAK__ char*
softboundcets_strchrnul(const char* s, int c) {
  char* ret_ptr = strchrnul(s, c);
   __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
   return ret_ptr;
}
#endif

__WEAK__ char*
softboundcets_strchr(const char* s, int c) {
  char* ret_ptr = strchr(s, c);
   __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
   return ret_ptr;
}

__WEAK__ char*
softboundcets_strrchr(const char* s, int c) {
  char* ret_ptr = strrchr(s, c);
  __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
  return ret_ptr;
}

__WEAK__ char*
softboundcets_stpcpy(char* dest, char* src) {
  void* ret_ptr = stpcpy(dest, src);
  __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
  return ret_ptr;
}

__WEAK__ char*
softboundcets_strcpy(char* dest, char* src) {
  char* dest_base = __softboundcets_load_base_shadow_stack(1);
  char* dest_bound = __softboundcets_load_bound_shadow_stack(1);
  char* src_base = __softboundcets_load_base_shadow_stack(2);
  char* src_bound = __softboundcets_load_bound_shadow_stack(2);

  /* There will be an out-of-bound read before we trigger an error as
     we currently use strlen. Can either (dest + size) or (src + size)
     overflow?
  */

  size_t size = strlen(src);
  if(dest < dest_base || (dest > dest_bound - size -1) || (size > (size_t) dest_bound)){
    printf("[strcpy] overflow in strcpy with dest\n");
    __softboundcets_abort();
  }
  if(src < src_base || (src > src_bound -size -1 ) || (size > (size_t) src_bound)){
    printf("[strcpy] overflow in strcpy with src\n");
    __softboundcets_abort();
  }

  void * ret_ptr = strcpy(dest, src);
  __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
  return ret_ptr;
}

__WEAK__ char*
softboundcets_strtok(char* str, const char* delim) {
  char* ret_ptr = strtok(str, delim);
  __softboundcets_store_dontcare_return_metadata();
  return ret_ptr;
}

__WEAK__ char*
softboundcets_strndup(const char* s, size_t n) {
  char* ret_ptr = strndup(s, n);
  key_type ptr_key;
  lock_type ptr_lock;
  if(ret_ptr == NULL) {
    __softboundcets_store_null_return_metadata();
  } else {
    __softboundcets_heap_allocation(ret_ptr, &ptr_lock, &ptr_key);
    __softboundcets_store_return_metadata(ret_ptr,
                                          (void*)
                                          ((char*)ret_ptr + strlen(ret_ptr) + 1),
                                          ptr_key, ptr_lock);
  }
  return ret_ptr;
}

__WEAK__ char*
softboundcets_strdup(const char* s) {
  void* ret_ptr = strdup(s);
  key_type ptr_key;
  lock_type ptr_lock;
  if(ret_ptr == NULL) {
    __softboundcets_store_null_return_metadata();
  } else {
    __softboundcets_heap_allocation(ret_ptr, &ptr_lock, &ptr_key);
    __softboundcets_store_return_metadata(ret_ptr,
                                          (void*)
                                          ((char*)ret_ptr + strlen(ret_ptr) + 1),
                                          ptr_key, ptr_lock);
  }
  return ret_ptr;
}

__WEAK__ char*
softboundcets_strcat (char* dest, const char* src) {
  char* ret_ptr = strcat(dest, src);
  __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
  return ret_ptr;
}

__WEAK__ char*
softboundcets_strncat (char* dest,const char* src, size_t n) {
  char* ret_ptr = strncat(dest, src, n);
  __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
  return ret_ptr;
}

__WEAK__ char*
softboundcets_strncpy(char* dest, char* src, size_t n) {
  char* dest_base = __softboundcets_load_base_shadow_stack(1);
  char* dest_bound = __softboundcets_load_bound_shadow_stack(1);
  char* src_base = __softboundcets_load_base_shadow_stack(2);
  char* src_bound = __softboundcets_load_bound_shadow_stack(2);

  /* Can either (dest + n) or (src + n) overflow? */
  if(dest < dest_base || dest + n > dest_bound){
    printf("[strncpy] overflow in strncpy with dest\n");
    __softboundcets_abort();
  }
  if(src < src_base || src + n > src_bound){
    printf("[strncpy] overflow in strncpy with src, src=%px, src_base=%px, src_bound=%px\n", (void*)src, (void*)src_base, (void*)src_bound);
    __softboundcets_abort();
  }
  char* ret_ptr = strncpy(dest, src, n);
  __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
  return ret_ptr;
}

__WEAK__ char*
softboundcets_strstr(const char* haystack, const char* needle) {
  char* ret_ptr = strstr(haystack, needle);
  if(ret_ptr != NULL) {
    __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
  }
  else {
    __softboundcets_store_null_return_metadata();
  }
  return ret_ptr;
}

__WEAK__ sighandler_t
softboundcets_signal(int signum, sighandler_t handler) {
  sighandler_t ptr = signal(signum, handler);
  __softboundcets_store_return_metadata((void*)ptr, (void*) ptr,
                                        1, __softboundcets_global_lock);
  return ptr;
}

__WEAK__ void*
softboundcets_realloc(void* ptr, size_t size) {
   void* ret_ptr = realloc(ptr, size);
   __softboundcets_allocation_secondary_trie_allocate(ret_ptr);
   size_t ptr_key = 1;
   void* ptr_lock = __softboundcets_global_lock;
   ptr_key = __softboundcets_load_key_shadow_stack(1);
   ptr_lock = __softboundcets_load_lock_shadow_stack(1);
   __softboundcets_store_return_metadata(ret_ptr,
                                         (char*)(ret_ptr) + size,
                                         ptr_key, ptr_lock);
   if(ret_ptr != ptr){
     __softboundcets_metadata_copy(ret_ptr, ptr, size);
   }
   return ret_ptr;
}

__WEAK__ void*
softboundcets_calloc(size_t nmemb, size_t size) {
 key_type ptr_key = 1 ;
 lock_type  ptr_lock = NULL;

 void* ret_ptr = calloc(nmemb, size);
 if(ret_ptr != NULL) {
   __softboundcets_heap_allocation(ret_ptr, &ptr_lock, &ptr_key);
   __softboundcets_store_return_metadata(ret_ptr,
                                         ((char*)(ret_ptr) + (nmemb * size)),
                                         ptr_key, ptr_lock);
 } else {
   __softboundcets_store_null_return_metadata();
 }
 return ret_ptr;
}

__WEAK__ void*
softboundcets_mmap(void* addr, size_t length,
                   int prot, int flags, int fd,
                   off_t offset) {
  key_type ptr_key=1;
  lock_type ptr_lock=__softboundcets_global_lock;
  char* ret_ptr = mmap(addr, length, prot, flags, fd, offset);
  if(ret_ptr == (void*) -1){
    __softboundcets_store_null_return_metadata();
  } else {
    char* ret_bound = ret_ptr + length;
    __softboundcets_store_return_metadata(ret_ptr, ret_bound,
                                          ptr_key, ptr_lock);
  }
  return ret_ptr;
}

__WEAK__ void*
softboundcets_malloc(size_t size) {
  key_type ptr_key=1;
  lock_type ptr_lock=NULL;

  char* ret_ptr = (char*)malloc(size);
  if(ret_ptr == NULL){
    __softboundcets_store_null_return_metadata();
  } else {
    __softboundcets_heap_allocation(ret_ptr, &ptr_lock, &ptr_key);

    char* ret_bound = ret_ptr + size;
    __softboundcets_store_return_metadata(ret_ptr, ret_bound,
                                          ptr_key, ptr_lock);
  }
  return ret_ptr;
}

__WEAK__ struct tm*
softboundcets_localtime(const time_t* timep) {
  struct tm * ret_ptr = localtime(timep);
  __softboundcets_store_return_metadata(ret_ptr,
                                        (char*)ret_ptr + sizeof(struct tm),
                                        1, __softboundcets_global_lock);
  return ret_ptr;
}

__WEAK__ void softboundcets_free(void* ptr) {
  // FIXME: more checks are technically required to verify it is a malloced address
  if(ptr != NULL){
    void* ptr_lock = __softboundcets_load_lock_shadow_stack(1);
    size_t ptr_key = __softboundcets_load_key_shadow_stack(1);
    __softboundcets_heap_deallocation(ptr, ptr_lock, ptr_key);
  }
  free(ptr);
}

__WEAK__ char*
softboundcets_ctime(const time_t* timep) {
  char* ret_ptr = ctime(timep);
  if(ret_ptr == NULL){
    __softboundcets_store_null_return_metadata();
  }
  else {
    __softboundcets_store_return_metadata(ret_ptr, ret_ptr + strlen(ret_ptr) + 1,
                                          1, __softboundcets_global_lock);
  }
  return ret_ptr;
}

__WEAK__ char*
softboundcets_getenv(const char* name) {
  char* ret_ptr = getenv(name);
  if(ret_ptr != NULL){
    __softboundcets_store_return_metadata(ret_ptr,
                                          ret_ptr + strlen(ret_ptr) + 1,
                                          1, __softboundcets_global_lock);
  }
  else {
    __softboundcets_store_null_return_metadata();
  }
  return ret_ptr;
}

#ifdef _GNU_SOURCE
__WEAK__ char*
softboundcets_strerror_r(int errnum, char* buf, size_t buf_len) {
  void* ret_ptr = strerror_r(errnum, buf, buf_len);
  __softboundcets_store_return_metadata(ret_ptr,
                                        (void*)
                                        ((char*)ret_ptr + strlen(ret_ptr) +1),
                                        1, __softboundcets_global_lock);
  return ret_ptr;
}
#endif

__WEAK__ char*
softboundcets_strerror(int errnum) {

  void* ret_ptr = strerror(errnum);
  __softboundcets_store_return_metadata(ret_ptr,
                                        (void*)
                                        ((char*)ret_ptr + strlen(ret_ptr) +1),
                                        1, __softboundcets_global_lock);
  return ret_ptr;
}

#if defined (__linux__)

__WEAK__ char*
softboundcets_setlocale(int category, const char* locale) {
  void* ret_ptr = setlocale(category, locale);
  __softboundcets_store_return_metadata(ret_ptr,
                                        (void*)
                                        ((char*) ret_ptr+ strlen(ret_ptr)),
                                        1, __softboundcets_global_lock);
  return ret_ptr;
}

__WEAK__ char*
softboundcets_textdomain(const char* domainname) {
  void* ret_ptr = textdomain(domainname);
  __softboundcets_store_return_metadata(ret_ptr,
                                        (void *)
                                        ((char*) ret_ptr + strlen(ret_ptr)),
                                        1, __softboundcets_global_lock);
  return ret_ptr;
}

__WEAK__ char*
softboundcets_bindtextdomain(const char* domainname, const char* dirname) {
  void* ret_ptr = bindtextdomain(domainname, dirname);
  __softboundcets_store_return_metadata(ret_ptr,
                                        (void *)
                                        ((char*) ret_ptr + strlen(ret_ptr)),
                                        1, __softboundcets_global_lock);
  return ret_ptr;
}

__WEAK__ char*
softboundcets_gettext(const char * msgid) {
  void* ret_ptr = gettext(msgid);
  __softboundcets_store_return_metadata(ret_ptr,
                                        (void*)
                                        ((char*) ret_ptr + strlen(ret_ptr)),
                                        1, __softboundcets_global_lock);
  return ret_ptr;
}

__WEAK__ char*
softboundcets_dcngettext (const char * domainname,
                          const char * msgid, const char * msgid_plural,
                          unsigned long int n, int category) {
  void* ret_ptr = dcngettext(domainname, msgid, msgid_plural, n, category);
  __softboundcets_store_return_metadata(ret_ptr,
                                        (void*)
                                        ((char*) ret_ptr + strlen(ret_ptr)),
                                        1, __softboundcets_global_lock);
  return ret_ptr;
}

__WEAK__ struct hostent*
softboundcets_gethostbyname(const char *name){
  struct hostent * ret_ptr = gethostbyname(name);
  void* ret_bound = ret_ptr + sizeof(struct hostent);
  __softboundcets_store_return_metadata(ret_ptr,
                                        ret_bound,
                                        1, __softboundcets_global_lock);
  // FIXME: struct hostent contains pointers which need to be tracked also.
  // We should be calling __softboundcets_metadata_store on those here.
  return ret_ptr;
}

__WEAK__ char*
softboundcets_dcgettext (const char * domainname,
                         const char * msgid,
                         int category) {
  void* ret_ptr = dcgettext(domainname, msgid, category);
  __softboundcets_store_return_metadata(ret_ptr,
                                        (void*)
                                        ((char*) ret_ptr + strlen(ret_ptr)),
                                        1, __softboundcets_global_lock);
  return ret_ptr;
}

__WEAK__ int*
softboundcets___errno_location() {
  void* ret_ptr = (int *)__errno_location();
  __softboundcets_store_return_metadata(ret_ptr,
                                        (void*)((char*)ret_ptr + sizeof(int*)),
                                        1, __softboundcets_global_lock);
  return ret_ptr;
}

__WEAK__ unsigned short const**
softboundcets___ctype_b_loc(void) {
  unsigned short const** ret_ptr =__ctype_b_loc();
  // FIXME: overflow allowed because bound is unknown
  __softboundcets_store_dontcare_base_return_metadata(ret_ptr);
  return ret_ptr;
}

__WEAK__ int const**
softboundcets___ctype_toupper_loc(void) {
  int const ** ret_ptr  =  __ctype_toupper_loc();
  // FIXME: overflow allowed because bound is unknown
  __softboundcets_store_dontcare_base_return_metadata(ret_ptr);
  return ret_ptr;
}

__WEAK__ int const**
softboundcets___ctype_tolower_loc(void) {
  int const ** ret_ptr  =  __ctype_tolower_loc();
  // FIXME: overflow allowed because bound is unknown
  __softboundcets_store_dontcare_base_return_metadata(ret_ptr);
  return ret_ptr;
}

__WEAK__ char*
softboundcets_nl_langinfo(nl_item item){
  char* ret_ptr = nl_langinfo(item);
  // FIXME: overflow allowed because bound is unknown
  __softboundcets_store_dontcare_base_return_metadata(ret_ptr);
  return ret_ptr;
}

#endif
