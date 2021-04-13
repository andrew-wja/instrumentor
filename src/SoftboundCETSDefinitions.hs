module SoftboundCETSDefinitions where

import LLVM.AST
import LLVM.AST.Type
import LLVM.IRBuilder.Module
import Data.Set hiding (map)

{-
  We create a new module and place the declaration of the instrumentation
  interface API in it. This lets us just link this instrumentation module with
  the original module we were asked to transform and avoid all the nonsense of
  injecting function declarations one-by-one with getOrInsertFunction()
-}

sbcetsModule :: ModuleBuilder ()
sbcetsModule = do
  _ <- extern (mkName "__softboundcets_spatial_load_dereference_check")
         [ptr i8, ptr i8, ptr i8, i64] void

  _ <- extern (mkName "__softboundcets_spatial_store_dereference_check")
         [ptr i8, ptr i8, ptr i8, i64] void

  _ <- extern (mkName "__softboundcets_temporal_load_dereference_check")
         [ptr i8, i64, ptr i8, ptr i8] void

  _ <- extern (mkName "__softboundcets_temporal_store_dereference_check")
         [ptr i8, i64, ptr i8, ptr i8] void

  _ <- extern (mkName "__softboundcets_get_global_lock") [] (ptr i8)

  _ <- extern (mkName "__softboundcets_introspect_metadata")
         [ptr i8, ptr i8, ptr i8, i64] void

  _ <- extern (mkName "__softboundcets_copy_metadata")
         [ptr i8, ptr i8, i64] void

  _ <- extern (mkName "__softboundcets_allocate_shadow_stack_space")
         [i32] void

  _ <- extern (mkName "__softboundcets_deallocate_shadow_stack_space") [] void

  _ <- extern (mkName "__softboundcets_store_base_shadow_stack")
         [ptr i8, i32] void

  _ <- extern (mkName "__softboundcets_store_bound_shadow_stack")
         [ptr i8, i32] void

  _ <- extern (mkName "__softboundcets_load_base_shadow_stack")
         [i32] (ptr i8)

  _ <- extern (mkName "__softboundcets_load_bound_shadow_stack")
         [i32] (ptr i8)

  _ <- extern (mkName "__softboundcets_load_key_shadow_stack")
         [i32] i64

  _ <- extern (mkName "__softboundcets_load_lock_shadow_stack")
         [i32] (ptr i8)

  _ <- extern (mkName "__softboundcets_store_key_shadow_stack")
         [i64, i32] void

  _ <- extern (mkName "__softboundcets_store_lock_shadow_stack")
         [ptr i8, i32] void

  _ <- extern (mkName "__softboundcets_stack_memory_allocation")
         [ptr i8, i64] void

  _ <- extern (mkName "__softboundcets_stack_memory_deallocation")
         [i64] void

  _ <- extern (mkName "__softboundcets_metadata_map")
         [ptr i8] (ptr i8)

  _ <- extern (mkName "__softboundcets_metadata_load_base")
         [ptr i8] (ptr i8)

  _ <- extern (mkName "__softboundcets_metadata_load_bound")
         [ptr i8] (ptr i8)

  _ <- extern (mkName "__softboundcets_metadata_load_key")
         [ptr i8] i64

  _ <- extern (mkName "__softboundcets_metadata_load_lock")
         [ptr i8] (ptr i8)

  _ <- extern (mkName "__softboundcets_metadata_load_vector")
         [ptr i8, ptr i8, ptr i8, i64, ptr i8, i32] void

  _ <- extern (mkName "__softboundcets_metadata_store_vector")
         [ptr i8, ptr i8, ptr i8, i64, ptr i8, i32] void

  _ <- extern (mkName "__softboundcets_metadata_load")
         [ptr i8, (ptr $ ptr i8), (ptr $ ptr i8), ptr i64, (ptr $ ptr i8)] void

  _ <- extern (mkName "__softboundcets_metadata_store")
         [ptr i8, ptr i8, ptr i8, i64, ptr i8] void

  _ <- extern (mkName "__softboundcets_spatial_call_dereference_check")
         [ptr i8, ptr i8, ptr i8] void

  _ <- extern (mkName "__softboundcets_memcopy_check")
         [ptr i8, ptr i8, i64, ptr i8, ptr i8, ptr i8, ptr i8, i64,
          ptr i8, i64, ptr i8] void

  _ <- extern (mkName "__softboundcets_memset_check")
         [ptr i8, i64, ptr i8, ptr i8, i64, ptr i8] void

  return ()

ignoredFunctions :: Set Name
ignoredFunctions = fromList $ map mkName [
  "asprintf", "compare_pic_by_pic_num_desc", "dup2", "dup", "error", "execlp",
  "execl", "execv", "_exit", "fcntl", "fflush_unlocked", "flockfile", "fork",
  "__fpending", "fprintf", "fscanf", "full_write", "funlockfile",
  "fwrite_unlocked", "__hashProbeAddrOfPtr", "ioctl", "_IO_getc", "_IO_putc",
  "longjmp", "__memcopyCheck_i64", "__memcopyCheck", "__option_is_short",
  "__overflow", "pipe", "printf", "puts", "safe_calloc", "safe_free",
  "safe_malloc", "safe_mmap", "safe_read", "scanf", "select", "_setjmp",
  "setuid", "__shrinkBounds", "snprintf", "__softboundcets_abort",
  "__softboundcets_add_to_free_map", "__softboundcets_allocate_lock_location",
  "__softboundcets_allocate_shadow_stack_space",
  "__softboundcets_allocation_secondary_trie_allocate_range",
  "__softboundcets_allocation_secondary_trie_allocate",
  "__SOFTBOUNDCETS_ASSERT_fail", "SOFTBOUNDCETS_ASSERT",
  "__softboundcets_check_remove_from_free_map",
  "__softboundcets_copy_metadata",
  "__softboundcets_deallocate_shadow_stack_space", "__softboundcets_dummy",
  "__softboundcets_get_global_lock", "__softboundcets_global_init",
  "__softboundcets_init", "__softboundcets_intermediate",
  "__softboundcets_introspect_metadata",
  "__softboundcets_load_base_shadow_stack",
  "__softboundcets_load_bound_shadow_stack",
  "__softboundcets_load_key_shadow_stack",
  "__softboundcets_load_lock_shadow_stack", "__softboundcets_memcopy_check",
  "__softboundcets_memory_allocation", "__softboundcets_memory_deallocation",
  "__softboundcets_metadata_load", "__softboundcets_metadata_load_vector",
  "__softboundcets_metadata_store", "__softboundcets_metadata_store_vector",
  "__softboundcets_printf", "__softboundcets_print_metadata",
  "__softboundcets_spatial_call_dereference_check",
  "__softboundcets_spatial_load_dereference_check",
  "__softboundcets_spatial_store_dereference_check",
  "__softboundcets_stack_memory_allocation",
  "__softboundcets_stack_memory_deallocation",
  "__softboundcets_store_base_shadow_stack",
  "__softboundcets_store_bound_shadow_stack",
  "__softboundcets_store_key_shadow_stack",
  "__softboundcets_store_lock_shadow_stack", "__softboundcets_stub",
  "__softboundcets_temporal_load_dereference_check",
  "__softboundcets_temporal_store_dereference_check",
  "__softboundcets_trie_allocate", "sprintf", "sscanf", "__strcspn_c2",
  "__stroul_internal", "__strspn_c2", "__strtod_internal", "__strtol_internal",
  "__strtoul_internal", "__uflow", "vasprintf", "vfprintf", "vsnprintf",
  "vsprintf", "waitpid", "wprintf" ]

wrappedFunctions :: Set Name
wrappedFunctions = fromList $ map mkName [
{-
  "abort", "abs", "acos", "atan2", "atexit", "atof", "atoi", "atol",
  "ceilf", "ceil", "chdir", "chown", "chroot", "clock", "closedir", "close",
  "cosf", "cosl", "cos", "ctime", "__ctype_b_loc", "__ctype_tolower_loc",
  "__ctype_toupper_loc", "difftime", "drand48", "__errno_location", "exit",
  "exp2", "expf", "exp", "fabsf", "fabs", "fclose", "fdopen", "feof",
  "ferror", "fflush", "fgetc", "fgets", "fileno", "floorf", "floor",
  "fopen", "fputc", "fputs", "fread", "fseek", "fstat", "ftell",
  "ftruncate", "fwrite", "getcwd", "getenv", "getrlimit", "gets",
  "gettimeofday", "getuid", "isatty", "ldexp", "localtime", "log10", "log",
  "lrand48", "lseek", "memchr", "memcmp", "mkdir", "mkstemp", "opendir",
  "open", "pclose", "perror", "popen", "pow", "putchar", "qsort", "rand",
  "readdir", "read", "remove", "rename", "rewind", "rindex", "rmdir",
  "select", "setbuf", "setreuid", "setrlimit", "signal", "sinf", "sinl",
  "sin", "sleep", "sqrtf", "sqrt", "srand48", "srand", "stat", "strcasecmp",
  "strcat", "strchr", "strcmp", "strcpy", "strcspn", "strdup", "strerror",
  "strftime", "strlen", "strncasecmp", "strncat", "strncmp", "strncpy",
  "strpbrk", "strrchr", "strspn", "strstr", "strtod", "strtok", "strtol",
  "strtoul", "system", "tanf", "tanl", "tan", "times", "time", "tmpfile",
  "tolower", "toupper", "umask", "unlink", "write",
-}
  "calloc", "free", "main", "malloc", "mmap", "realloc" ]
