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

////////////////File Library Wrappers here //////////////////////

__WEAK__ FILE* softboundcets_tmpfile(void) {

  void* ret_ptr = tmpfile();
  void* ret_ptr_bound = (char*) ret_ptr + sizeof(FILE);
  __softboundcets_store_return_metadata(ret_ptr, ret_ptr_bound,
                                        1, __softboundcets_global_lock);
  return ret_ptr;
}

__WEAK__ DIR* softboundcets_fdopendir(int fd){

  void* ret_ptr = (void*) fdopendir(fd);
  void* ret_ptr_bound = (char *) ret_ptr + 1024 * 1024;
    __softboundcets_store_return_metadata(ret_ptr, ret_ptr_bound,
                                        1, (void*) __softboundcets_global_lock);
  return (DIR*)ret_ptr;

}

__WEAK__  char * softboundcets_mkdtemp(char *template){

  char* ret_ptr = mkdtemp(template);
  __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
  return ret_ptr;
}

__WEAK__ struct lconv *softboundcets_localeconv(void){
  struct lconv* temp = localeconv();

  __softboundcets_store_return_metadata(temp, temp + 1024,
                                        1, (void*) __softboundcets_global_lock);

  return temp;
}

__WEAK__ struct tm *softboundcets_gmtime(const time_t *timep){

  struct tm * temp = gmtime(timep);

  __softboundcets_store_return_metadata(temp, temp + 1024,
                                        1, (void*) __softboundcets_global_lock);

  return temp;
}

__WEAK__ void *
softboundcets_bsearch(const void *key, const void *base,
                      size_t nmemb, size_t size,
                      int (*compar)(const void *, const void *)){

  void* ret_ptr = bsearch(key, base, nmemb, size, compar);

  __softboundcets_propagate_metadata_shadow_stack_from(2, 0);
    return ret_ptr;

}

__WEAK__
struct group *softboundcets_getgrnam(const char *name){
  void* ret_ptr = getgrnam(name);
  __softboundcets_store_return_metadata(ret_ptr, (char*) ret_ptr + 1024 * 1024,
                                        1, (void*) __softboundcets_global_lock);

  return ret_ptr;
}

__WEAK__
struct passwd * softboundcets_getpwnam(const char *name){
  void* ret_ptr = getpwnam(name);
  __softboundcets_store_return_metadata(ret_ptr, (char*) ret_ptr + 1024 * 1024,
                                        1, (void*) __softboundcets_global_lock);

  return ret_ptr;
}

__WEAK__ struct passwd *softboundcets_getpwuid(uid_t uid){
  void* ret_ptr= getpwuid(uid);

  __softboundcets_store_return_metadata(ret_ptr, (char*) ret_ptr + 1024 * 1024,
                                        1, (void*) __softboundcets_global_lock);

  return ret_ptr;
}

__WEAK__ struct group *softboundcets_getgrgid(gid_t gid){

  void* ret_ptr = getgrgid(gid);
  __softboundcets_store_return_metadata(ret_ptr, (char*) ret_ptr + 1024 * 1024,
                                        1, (void*) __softboundcets_global_lock);

  return ret_ptr;

}

__WEAK__ FILE* softboundcets_fopen(const char* path, const char* mode){

  void* ret_ptr = (void*) fopen(path, mode);
  void* ret_ptr_bound = (char*) ret_ptr + sizeof(FILE);

  __softboundcets_store_return_metadata(ret_ptr, ret_ptr_bound,
                                        1, (void*) __softboundcets_global_lock);
  return (FILE*)ret_ptr;
}

__WEAK__ FILE* softboundcets_fdopen(int fildes, const char* mode){

  void* ret_ptr = (void*) fdopen(fildes, mode);
  void* ret_ptr_bound = (char*) ret_ptr + sizeof(FILE);

  __softboundcets_store_return_metadata(ret_ptr, ret_ptr_bound,
                                        1, (void*)__softboundcets_global_lock);
  return (FILE*)ret_ptr;
}

__WEAK__ FILE* softboundcets_popen(const char* command, const char* type){

  void* ret_ptr = (void*) popen(command, type);
  void* ret_ptr_bound = (char*)ret_ptr + sizeof(FILE);

  __softboundcets_store_return_metadata(ret_ptr, ret_ptr_bound,
                                        1, (void*) __softboundcets_global_lock);
  return (FILE*)ret_ptr;

}

__WEAK__ struct dirent*  softboundcets_readdir(DIR* dir){

  void* ret_ptr = (void*) readdir(dir);
  void* ret_ptr_bound = (char*)ret_ptr + sizeof(struct dirent);

  __softboundcets_store_return_metadata(ret_ptr, ret_ptr_bound,
                                        1, (void*) __softboundcets_global_lock);

  return (struct dirent*)ret_ptr;

}

__WEAK__ DIR* softboundcets_opendir(const char* name){

  void* ret_ptr = opendir(name);

  /* FIXME:, don't know the sizeof(DIR) */
  void* ret_ptr_bound = (char*) ret_ptr + 1024* 1024;

  __softboundcets_store_return_metadata(ret_ptr, ret_ptr_bound,
                                        1,  (void*)__softboundcets_global_lock);

  return (DIR*)ret_ptr;
}

////////////////////unistd.h wrappers ////////////////////////////////

__WEAK__ char* softboundcets_getcwd(char* buf, size_t size){

  if(buf == NULL) {
    printf("This case not handled, requesting memory from system\n");
    __softboundcets_abort();
  }

  char* base = (char *)__softboundcets_load_base_shadow_stack(1);
  char* bound = (char *) __softboundcets_load_bound_shadow_stack(1);

  if (buf < base || buf + size > bound){
    __softboundcets_printf("[getcwd], overflow in buf in getcwd\n");
    __softboundcets_abort();
  }

  char* ret_ptr = getcwd(buf, size);

  __softboundcets_propagate_metadata_shadow_stack_from(1, 0);

  return ret_ptr;
}

///////////////////String related wrappers ////////////////////////////

__WEAK__ char* softboundcets_strpbrk(const char* s, const char* accept){

  char* ret_ptr = strpbrk(s, accept);
  if(ret_ptr != NULL) {

    __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
  }
  else {

    __softboundcets_store_null_return_metadata();
  }

  return ret_ptr;
}

__WEAK__ char* softboundcets_gets(char* s){

  printf("[SBCETS] gets used and should not be used\n");
  __softboundcets_abort();
  return NULL;

}

__WEAK__ char* softboundcets_fgets(char* s, int size, FILE* stream){

  char* ret_ptr = fgets(s, size, stream);
  __softboundcets_propagate_metadata_shadow_stack_from(1,0);

  return ret_ptr;
}

#ifdef _GNU_SOURCE

__WEAK__ void* softboundcets_mempcpy(void * dest, const void * src, size_t n){

  // FIXME: need to copy the metadata
  void* ret_ptr = mempcpy(dest, src, n);
  __softboundcets_propagate_metadata_shadow_stack_from(1,0);
  return ret_ptr;
}

__WEAK__ void* softboundcets_memrchr(const void * s, int c, size_t n){
  void* ret_ptr = memrchr(s, c, n);
  if(ret_ptr != NULL) {
    __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
  }
  else{
    __softboundcets_store_null_return_metadata();
  }
  return ret_ptr;
}
#endif

__WEAK__ void* softboundcets_memchr(const void * s, int c, size_t n){
  void* ret_ptr = memchr(s, c, n);
  if(ret_ptr != NULL) {
    __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
  }
  else{
    __softboundcets_store_null_return_metadata();
  }
  return ret_ptr;
}

__WEAK__ char* softboundcets_rindex(char* s, int c){

  char* ret_ptr = rindex(s,c);
  __softboundcets_propagate_metadata_shadow_stack_from(1,0);
  return ret_ptr;
}

__WEAK__ ssize_t
softboundcets___getdelim(char **lineptr, size_t *n, int delim, FILE *stream){

  int metadata_prop = 1;
  if(*lineptr == NULL){
    metadata_prop = 0;
  }

  ssize_t ret_val = getdelim(lineptr, n, delim, stream);

  if(metadata_prop){
    __softboundcets_read_shadow_stack_metadata_store(lineptr, 1);
  }
  else{
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

__WEAK__ double softboundcets_strtod(const char* nptr, char** endptr){

  double temp = strtod(nptr, endptr);

  if(endptr != NULL) {
    __softboundcets_read_shadow_stack_metadata_store(endptr, 1);
  }
  return temp;
 }

__WEAK__ long
softboundcets_strtol(const char* nptr, char **endptr, int base){

   long temp = strtol(nptr, endptr, base);
   if(endptr != NULL) {
     //    __softboundcets_printf("*endptr=%p\n", *endptr);
     __softboundcets_read_shadow_stack_metadata_store(endptr, 1);
  }
  return temp;
}

#ifdef _GNU_SOURCE

__WEAK__ char* softboundcets_strchrnul(const char* s, int c){

  char* ret_ptr = strchrnul(s, c);
   __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
   return ret_ptr;
}
#endif

__WEAK__ char* softboundcets_strchr(const char* s, int c){

  char* ret_ptr = strchr(s, c);
   __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
   return ret_ptr;
}

__WEAK__ char* softboundcets_strrchr(const char* s, int c){

  char* ret_ptr = strrchr(s, c);
  __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
  return ret_ptr;
}

__WEAK__ char* softboundcets_stpcpy(char* dest, char* src){

  void* ret_ptr = stpcpy(dest, src);
  __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
  return ret_ptr;
}

__WEAK__ char* softboundcets_strcpy(char* dest, char* src){

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


__WEAK__ void softboundcets_abort() {
  abort();
}

 __WEAK__ int softboundcets_rand() {
   return rand();
}

 /////////////////* TODO *///////////////////////////

__WEAK__ int softboundcets_atoi(const char* ptr){

if(ptr == NULL) {
  __softboundcets_abort();
}
return atoi(ptr);
}

__WEAK__ void softboundcets_puts(char* ptr){
 puts(ptr);
}

__WEAK__ void softboundcets_exit(int status) {

  exit(status);
}

__WEAK__ char*  softboundcets_strtok(char* str, const char* delim){

  char* ret_ptr = strtok(str, delim);
  __softboundcets_store_return_metadata((void*)0, (void*)(281474976710656),
                                        1, __softboundcets_global_lock);
  return ret_ptr;
}

__WEAK__ void __softboundcets_strdup_handler(void* ret_ptr){
  key_type ptr_key;
  lock_type ptr_lock;

  if(ret_ptr == NULL) {
    __softboundcets_store_null_return_metadata();
  }
  else {
    //    printf("strndup malloced pointer %p\n", ret_ptr);
    __softboundcets_memory_allocation(ret_ptr, &ptr_lock, &ptr_key);
    __softboundcets_store_return_metadata(ret_ptr,
                                          (void*)
                                          ((char*)ret_ptr + strlen(ret_ptr) + 1),
                                          ptr_key, ptr_lock);
  }
}

//strdup, allocates memory from the system using malloc, thus can be freed
__WEAK__ char* softboundcets_strndup(const char* s, size_t n){

  /* IMP: strndup just copies the string s */
  char* ret_ptr = strndup(s, n);
  __softboundcets_strdup_handler(ret_ptr);
  return ret_ptr;
 }

//strdup, allocates memory from the system using malloc, thus can be freed
__WEAK__ char* softboundcets_strdup(const char* s){

  /* IMP: strdup just copies the string s */
  void* ret_ptr = strdup(s);

  __softboundcets_strdup_handler(ret_ptr);
  return ret_ptr;
}

__WEAK__ char* softboundcets___strdup(const char* s){

  void* ret_ptr = strdup(s);
  __softboundcets_strdup_handler(ret_ptr);
  return ret_ptr;
}


 __WEAK__ char* softboundcets_strcat (char* dest, const char* src){

  char* ret_ptr = strcat(dest, src);
  __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
  return ret_ptr;
}

__WEAK__ char*
softboundcets_strncat (char* dest,const char* src, size_t n){

  char* ret_ptr = strncat(dest, src, n);
  __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
  return ret_ptr;
}

__WEAK__ char*
softboundcets_strncpy(char* dest, char* src, size_t n){

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
    printf("[strncpy] overflow in strncpy with src, src=%px, src_base=%px, src_bound=%px\n", src, src_base, src_bound);
    __softboundcets_abort();
  }

  char* ret_ptr = strncpy(dest, src, n);
  __softboundcets_propagate_metadata_shadow_stack_from(1, 0);
  return ret_ptr;
}

__WEAK__ char*
softboundcets_strstr(const char* haystack, const char* needle){

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
softboundcets_signal(int signum, sighandler_t handler){

  sighandler_t ptr = signal(signum, handler);
  __softboundcets_store_return_metadata((void*)ptr, (void*) ptr,
                                        1, __softboundcets_global_lock);
  return ptr;
}

__WEAK__ clock_t softboundcets_clock(void){
  return clock();
}


__WEAK__ long softboundcets_atol(const char* nptr){
  return atol(nptr);
}

__WEAK__ void* softboundcets_realloc(void* ptr, size_t size){

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
     __softboundcets_check_remove_from_free_map(ptr_key, ptr);
     __softboundcets_add_to_free_map(ptr_key, ret_ptr);
     __softboundcets_copy_metadata(ret_ptr, ptr, size);
   }

   return ret_ptr;
}

__WEAK__ void* softboundcets_calloc(size_t nmemb, size_t size) {

 key_type ptr_key = 1 ;
 lock_type  ptr_lock = NULL;

 void* ret_ptr = calloc(nmemb, size);
 if(ret_ptr != NULL) {

   __softboundcets_memory_allocation(ret_ptr, &ptr_lock, &ptr_key);

   __softboundcets_store_return_metadata(ret_ptr,
                                         ((char*)(ret_ptr) + (nmemb * size)),
                                         ptr_key, ptr_lock);

   if(__SOFTBOUNDCETS_FREE_MAP) {
     //       __softboundcets_add_to_free_map(ptr_key, ret_ptr);
   }
 }
 else{
   __softboundcets_store_null_return_metadata();
 }
 return ret_ptr;
}

__WEAK__ void* softboundcets_mmap(void* addr, size_t length,
                                       int prot, int flags, int fd,
                                       off_t offset){

  key_type ptr_key=1;
  lock_type ptr_lock=__softboundcets_global_lock;
  char* ret_ptr = mmap(addr, length, prot, flags, fd, offset);
  if(ret_ptr == (void*) -1){
    __softboundcets_store_null_return_metadata();
  }
  else{

    char* ret_bound = ret_ptr + length;
    __softboundcets_store_return_metadata(ret_ptr, ret_bound,
                                          ptr_key, ptr_lock);

  }
  return ret_ptr;
}

__WEAK__ void* softboundcets_malloc(size_t size) {

  key_type ptr_key=1;
  lock_type ptr_lock=NULL;

  char* ret_ptr = (char*)malloc(size);
  if(ret_ptr == NULL){
    __softboundcets_store_null_return_metadata();
  }
  else{

    __softboundcets_memory_allocation(ret_ptr, &ptr_lock, &ptr_key);

    char* ret_bound = ret_ptr + size;
    __softboundcets_store_return_metadata(ret_ptr, ret_bound,
                                          ptr_key, ptr_lock);

    if(__SOFTBOUNDCETS_FREE_MAP) {
       //      __softboundcets_add_to_free_map(ptr_key, ret_ptr);
    }
  }
  return ret_ptr;
}


__WEAK__ int softboundcets_putchar(int c) {

  return putchar(c);
}


 __WEAK__ clock_t softboundcets_times(struct tms* buf){
  return times(buf);
}

__WEAK__ size_t
softboundcets_strftime(char* s, size_t max,
                       const char* format, const struct tm *tm){

  return strftime(s, max, format, tm);
}


__WEAK__ time_t softboundcets_mktime(struct tm *tm){
  return mktime(tm);
}

__WEAK__ long softboundcets_pathconf(char *path, int name){
  return pathconf(path, name);
}


__WEAK__ struct tm* softboundcets_localtime(const time_t* timep){

  struct tm * ret_ptr = localtime(timep);
  __softboundcets_store_return_metadata(ret_ptr,
                                        (char*)ret_ptr + sizeof(struct tm),
                                        1, __softboundcets_global_lock);
  return ret_ptr;
}

 __WEAK__ time_t softboundcets_time(time_t* t){

   return time(t);
 }

__WEAK__ double softboundcets_drand48(){

  return drand48();
}

__WEAK__ void softboundcets_free(void* ptr){
  /* more checks required to check if it is a malloced address */
  if(ptr != NULL){
    void* ptr_lock = __softboundcets_load_lock_shadow_stack(1);
    size_t ptr_key = __softboundcets_load_key_shadow_stack(1);
    __softboundcets_memory_deallocation(ptr_lock, ptr_key);

    if(__SOFTBOUNDCETS_FREE_MAP){
      __softboundcets_check_remove_from_free_map(ptr_key, ptr);
    }
  }
   free(ptr);
}


__WEAK__ long int softboundcets_lrand48(){
  return lrand48();
}


/* ////////////////////Time Related Library Wrappers///////////////////////// */


__WEAK__ char* softboundcets_ctime( const time_t* timep){

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



__WEAK__ double softboundcets_difftime(time_t time1, time_t time0) {
  return difftime(time1, time0);
}

__WEAK__ int softboundcets_toupper(int c) {

  return toupper(c);
}

__WEAK__ int softboundcets_tolower(int c){

  return tolower(c);
}

__WEAK__ void softboundcets_setbuf(FILE* stream, char* buf){
  setbuf(stream, buf);
}

__WEAK__ char* softboundcets_getenv(const char* name){

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

__WEAK__ int softboundcets_atexit(void_func_ptr function){
  return atexit(function);

}

#ifdef _GNU_SOURCE
__WEAK__ char* softboundcets_strerror_r(int errnum, char* buf,
                                             size_t buf_len) {

  void* ret_ptr = strerror_r(errnum, buf, buf_len);
  __softboundcets_store_return_metadata(ret_ptr,
                                        (void*)
                                        ((char*)ret_ptr + strlen(ret_ptr) +1),
                                        1, __softboundcets_global_lock);
  return ret_ptr;
}
#endif

__WEAK__ char* softboundcets_strerror(int errnum) {

  void* ret_ptr = strerror(errnum);
  __softboundcets_store_return_metadata(ret_ptr,
                                        (void*)
                                        ((char*)ret_ptr + strlen(ret_ptr) +1),
                                        1, __softboundcets_global_lock);
  return ret_ptr;
}


__WEAK__ int softboundcets_unlink(const char* pathname){
  return unlink(pathname);
}


__WEAK__ int softboundcets_close(int fd) {

  return close(fd);
}


__WEAK__ int softboundcets_open(const char *pathname, int flags){
  return open(pathname, flags);

}

__WEAK__ ssize_t softboundcets_read(int fd, void* buf, size_t count){

  return read(fd, buf, count);
}

__WEAK__ ssize_t softboundcets_write(int fd, void* buf, size_t count){
  return write(fd, buf, count);
}


__WEAK__ off_t softboundcets_lseek(int fildes, off_t offset, int whence) {

  return lseek(fildes, offset, whence);
}


__WEAK__ int
softboundcets_gettimeofday(struct timeval* tv, struct timezone* tz){
  return gettimeofday(tv, tz);
}


__WEAK__ int
softboundcets_select(int nfds, fd_set* readfds, fd_set* writefds,
                     fd_set* exceptfds, struct timeval* timeout){
  return select(nfds, readfds, writefds, exceptfds, timeout);
}

#if defined (__linux__)

__WEAK__ char*
softboundcets_setlocale(int category, const char* locale){

  void* ret_ptr = setlocale(category, locale);
  __softboundcets_store_return_metadata(ret_ptr,
                                        (void*)
                                        ((char*) ret_ptr+ strlen(ret_ptr)),
                                        1, __softboundcets_global_lock);
  return ret_ptr;

}

__WEAK__ char*
softboundcets_textdomain(const char* domainname){

  void* ret_ptr = textdomain(domainname);
  __softboundcets_store_return_metadata(ret_ptr,
                                        (void *)
                                        ((char*) ret_ptr + strlen(ret_ptr)),
                                        1, __softboundcets_global_lock);

  return ret_ptr;

}


__WEAK__ char*
softboundcets_bindtextdomain(const char* domainname, const char* dirname){

  void* ret_ptr = bindtextdomain(domainname, dirname);
  __softboundcets_store_return_metadata(ret_ptr,
                                        (void *)
                                        ((char*) ret_ptr + strlen(ret_ptr)),
                                        1, __softboundcets_global_lock);

  return ret_ptr;

}

__WEAK__ char *
softboundcets_gettext(const char * msgid){

  void* ret_ptr = gettext(msgid);
  __softboundcets_store_return_metadata(ret_ptr,
                                        (void*)
                                        ((char*) ret_ptr + strlen(ret_ptr)),
                                        1, __softboundcets_global_lock);

  return ret_ptr;

}


__WEAK__ char *
softboundcets_dcngettext (const char * domainname,
                          const char * msgid, const char * msgid_plural,
                          unsigned long int n, int category){

  void* ret_ptr = dcngettext(domainname, msgid, msgid_plural, n, category);
  __softboundcets_store_return_metadata(ret_ptr,
                                        (void*)
                                        ((char*) ret_ptr + strlen(ret_ptr)),
                                        1, __softboundcets_global_lock);

  return ret_ptr;

}


/* IMP: struct hostent may have pointers in the structure being returned,
   we need to store the metadata for all those pointers */
__WEAK__
struct hostent * softboundcets_gethostbyname(const char *name){

  struct hostent * ret_ptr = gethostbyname(name);

  void* ret_bound = ret_ptr + sizeof(struct hostent);
  __softboundcets_store_return_metadata(ret_ptr,
                                        ret_bound,
                                        1, __softboundcets_global_lock);

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

#endif

#if defined(__linux__)
__WEAK__ int* softboundcets___errno_location() {
  void* ret_ptr = (int *)__errno_location();
  //  printf("ERRNO: ptr is %lx", ptrs->ptr);
  __softboundcets_store_return_metadata(ret_ptr,
                                        (void*)((char*)ret_ptr + sizeof(int*)),
                                        1, __softboundcets_global_lock);

  return ret_ptr;
}

__WEAK__ unsigned short const**
softboundcets___ctype_b_loc(void) {

  unsigned short const** ret_ptr =__ctype_b_loc();
  __softboundcets_store_return_metadata((void*) ret_ptr,
                                        (void*)
                                        ((char*) ret_ptr + 1024*1024),
                                        1, __softboundcets_global_lock);
  return ret_ptr;
}

__WEAK__ int const**  softboundcets___ctype_toupper_loc(void) {

  int const ** ret_ptr  =  __ctype_toupper_loc();
  __softboundcets_store_return_metadata((void*) ret_ptr,
                                        (void*)
                                        ((char*)ret_ptr + 1024*1024),
                                        1, __softboundcets_global_lock);
  return ret_ptr;

}


__WEAK__ int const**  softboundcets___ctype_tolower_loc(void) {

  int const ** ret_ptr  =  __ctype_tolower_loc();
  __softboundcets_store_return_metadata((void*) ret_ptr,
                                        (void*) ((char*)ret_ptr + 1024*1024),
                                        1, __softboundcets_global_lock);
  return ret_ptr;

}
#endif

/* This is a custom implementation of qsort */

static int
compare_elements_helper(void* base, size_t element_size,
                        int idx1, int idx2,
                        int (*comparer)(const void*, const void*)){

  char* base_bytes = base;
  return comparer(&base_bytes[idx1 * element_size],
                  &base_bytes[idx2*element_size]);
}

#define element_less_than(i,j) (compare_elements_helper(base, element_size, (i), (j), comparer) < 0)

static void
exchange_elements_helper(void* base, size_t element_size,
                         int idx1, int idx2){

  char* base_bytes = base;
  size_t i;

  for (i=0; i < element_size; i++){
    char temp = base_bytes[idx1* element_size + i];
    base_bytes[idx1 * element_size + i] = base_bytes[idx2 * element_size + i];
    base_bytes[idx2 * element_size + i] = temp;
  }

  for(i=0; i < element_size; i+= 8){
    void* base_idx1;
    void* bound_idx1;

    void* base_idx2;
    void* bound_idx2;

    size_t key_idx1=1;
    size_t key_idx2=1;

    void* lock_idx1=NULL;
    void* lock_idx2=NULL;

    char* addr_idx1 = &base_bytes[idx1 * element_size + i];
    char* addr_idx2 = &base_bytes[idx2 * element_size + i];

    //    printf("addr_idx1= %p, addr_idx2=%p\n", addr_idx1, addr_idx2);

    __softboundcets_metadata_load(addr_idx1, &base_idx1, &bound_idx1,
                                  &key_idx1, &lock_idx1);
    __softboundcets_metadata_load(addr_idx2, &base_idx2, &bound_idx2,
                                  &key_idx2, &lock_idx2);

    __softboundcets_metadata_store(addr_idx1, base_idx2, bound_idx2,
                                   key_idx2, lock_idx2);
    __softboundcets_metadata_store(addr_idx2, base_idx1, bound_idx1,
                                   key_idx1, lock_idx1);
  }

}

#define exchange_elements(i,j) (exchange_elements_helper(base, element_size, (i), (j)))

#define MIN_QSORT_LIST_SIZE 32

__WEAK__
void my_qsort(void* base, size_t num_elements,
              size_t element_size,
              int (*comparer)(const void*, const void*)){

  size_t i;

  for(i = 0; i < num_elements; i++){
    int j;
    for (j = i - 1; j >= 0; j--){
      if(element_less_than(j, j + 1)) break;
      exchange_elements(j, j + 1);
    }
  }
  /* may be implement qsort here */

}


__WEAK__ void
softboundcets_qsort(void* base, size_t nmemb, size_t size,
                    int (*compar)(const void*, const void*)){

  my_qsort(base, nmemb, size, compar);
}

#if defined(__linux__)

__WEAK__
void softboundcets__obstack_newchunk(struct obstack *obj, int b){

  _obstack_newchunk(obj, b);
}

__WEAK__
int softboundcets__obstack_begin(struct obstack * obj, int a, int b,
                                 void *(foo) (long), void (bar) (void *)){
  return _obstack_begin(obj, a, b, foo, bar);
}

__WEAK__
void softboundcets_obstack_free(struct obstack *obj, void *object){
  obstack_free(obj, object);
}


__WEAK__
char * softboundcets_nl_langinfo(nl_item item){

  char* ret_ptr = nl_langinfo(item);

  __softboundcets_store_return_metadata(ret_ptr,
                                        ret_ptr + 1024 * 1024,
                                        1, __softboundcets_global_lock);

  return ret_ptr;
}

__WEAK__
int softboundcets_clock_gettime(clockid_t clk_id, struct timespec *tp){
  return clock_gettime(clk_id, tp);
}

#endif
